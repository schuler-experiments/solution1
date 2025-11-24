unit TaskSearch;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TSearchCriteria = record
    keyword: string;
    searchTitle: boolean;
    searchDescription: boolean;
    statusFilter: TTaskStatus;
    priorityFilter: TTaskPriority;
    startDate: TDateTime;
    endDate: TDateTime;
    useDateRange: boolean;
    caseSensitive: boolean;
  end;

  TSavedSearch = record
    id: integer;
    name: string;
    criteria: TSearchCriteria;
    createdDate: TDateTime;
    resultCount: integer;
  end;

  TSavedSearchArray = array of TSavedSearch;

  TSearchResultMetrics = record
    totalResults: integer;
    averageScore: double;
    topMatch: string;
    searchTime: double;
  end;

  TTaskSearchClass = class
  private
    savedSearches: TSavedSearchArray;
    searchCounter: integer;
    function MatchesKeyword(task: TTask; criteria: TSearchCriteria): boolean;
    function MatchesStatus(task: TTask; criteria: TSearchCriteria): boolean;
    function MatchesPriority(task: TTask; criteria: TSearchCriteria): boolean;
    function MatchesDateRange(task: TTask; criteria: TSearchCriteria): boolean;
    function CalculateMatchScore(task: TTask; keyword: string): double;
  public
    constructor Create();
    destructor Destroy();
    function SearchTasks(tasks: TTaskArray; criteria: TSearchCriteria): TTaskArray;
    function FullTextSearch(tasks: TTaskArray; keyword: string): TTaskArray;
    function SaveSearch(name: string; criteria: TSearchCriteria): integer;
    function GetSavedSearch(id: integer): TSavedSearch;
    function GetAllSavedSearches(): TSavedSearchArray;
    function DeleteSavedSearch(id: integer): boolean;
    function RenameSavedSearch(id: integer; newName: string): boolean;
    function GetSearchMetrics(results: TTaskArray): TSearchResultMetrics;
    function FindDuplicates(tasks: TTaskArray): TTaskArray;
    function FindOrphaned(tasks: TTaskArray; allTaskIds: TIntegerArray): TTaskArray;
    procedure SelfTest();
  end;

implementation

constructor TTaskSearchClass.Create();
begin
  inherited Create();
  SetLength(savedSearches, 0);
  searchCounter := 0;
end;

destructor TTaskSearchClass.Destroy();
begin
  SetLength(savedSearches, 0);
  inherited Destroy();
end;

function TTaskSearchClass.MatchesKeyword(task: TTask;
                                         criteria: TSearchCriteria): boolean;
var
  matchTitle, matchDesc: boolean;
begin
  result := true;

  if Trim(criteria.keyword) = '' then
    exit;

  matchTitle := false;
  matchDesc := false;

  if criteria.searchTitle then
  begin
    if criteria.caseSensitive then
      matchTitle := Pos(criteria.keyword, task.title) > 0
    else
      matchTitle := Pos(UpperCase(criteria.keyword), UpperCase(task.title)) > 0;
  end;

  if criteria.searchDescription then
  begin
    if criteria.caseSensitive then
      matchDesc := Pos(criteria.keyword, task.description) > 0
    else
      matchDesc := Pos(UpperCase(criteria.keyword), UpperCase(task.description)) > 0;
  end;

  result := matchTitle or matchDesc;
end;

function TTaskSearchClass.MatchesStatus(task: TTask;
                                        criteria: TSearchCriteria): boolean;
begin
  if criteria.statusFilter = tsNotStarted then
    result := true
  else
    result := task.status = criteria.statusFilter;
end;

function TTaskSearchClass.MatchesPriority(task: TTask;
                                          criteria: TSearchCriteria): boolean;
begin
  if criteria.priorityFilter = tpLow then
    result := true
  else
    result := task.priority = criteria.priorityFilter;
end;

function TTaskSearchClass.MatchesDateRange(task: TTask;
                                           criteria: TSearchCriteria): boolean;
begin
  if not criteria.useDateRange then
  begin
    result := true;
    exit;
  end;

  result := (task.dueDate >= criteria.startDate) and
            (task.dueDate <= criteria.endDate);
end;

function TTaskSearchClass.CalculateMatchScore(task: TTask;
                                              keyword: string): double;
var
  score: double;
begin
  score := 0;

  if Pos(keyword, task.title) > 0 then
    score := score + 10.0;

  if Pos(keyword, task.description) > 0 then
    score := score + 5.0;

  result := score;
end;

function TTaskSearchClass.SearchTasks(tasks: TTaskArray;
                                      criteria: TSearchCriteria): TTaskArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;

  for i := 0 to Length(tasks) - 1 do
  begin
    if MatchesKeyword(tasks[i], criteria) and
       MatchesStatus(tasks[i], criteria) and
       MatchesPriority(tasks[i], criteria) and
       MatchesDateRange(tasks[i], criteria) then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      inc(count);
    end;
  end;
end;

function TTaskSearchClass.FullTextSearch(tasks: TTaskArray;
                                         keyword: string): TTaskArray;
var
  criteria: TSearchCriteria;
begin
  FillChar(criteria, SizeOf(TSearchCriteria), 0);
  criteria.keyword := keyword;
  criteria.searchTitle := true;
  criteria.searchDescription := true;
  criteria.statusFilter := tsNotStarted;
  criteria.priorityFilter := tpLow;
  criteria.useDateRange := false;
  criteria.caseSensitive := false;

  result := SearchTasks(tasks, criteria);
end;

function TTaskSearchClass.SaveSearch(name: string;
                                     criteria: TSearchCriteria): integer;
var
  newSearch: TSavedSearch;
  idx: integer;
begin
  inc(searchCounter);
  newSearch.id := searchCounter;
  newSearch.name := name;
  newSearch.criteria := criteria;
  newSearch.createdDate := Now();
  newSearch.resultCount := 0;

  idx := Length(savedSearches);
  SetLength(savedSearches, idx + 1);
  savedSearches[idx] := newSearch;

  result := searchCounter;
end;

function TTaskSearchClass.GetSavedSearch(id: integer): TSavedSearch;
var
  i: integer;
begin
  FillChar(result, SizeOf(TSavedSearch), 0);

  for i := 0 to Length(savedSearches) - 1 do
  begin
    if savedSearches[i].id = id then
    begin
      result := savedSearches[i];
      exit;
    end;
  end;

  result.id := -1;
end;

function TTaskSearchClass.GetAllSavedSearches(): TSavedSearchArray;
begin
  SetLength(result, Length(savedSearches));
  if Length(savedSearches) > 0 then
    Move(savedSearches[0], result[0], Length(savedSearches) * SizeOf(TSavedSearch));
end;

function TTaskSearchClass.DeleteSavedSearch(id: integer): boolean;
var
  i, idx: integer;
begin
  idx := -1;
  for i := 0 to Length(savedSearches) - 1 do
  begin
    if savedSearches[i].id = id then
    begin
      idx := i;
      break;
    end;
  end;

  if idx >= 0 then
  begin
    for i := idx to Length(savedSearches) - 2 do
      savedSearches[i] := savedSearches[i + 1];
    SetLength(savedSearches, Length(savedSearches) - 1);
    result := true;
  end
  else
    result := false;
end;

function TTaskSearchClass.RenameSavedSearch(id: integer;
                                            newName: string): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Length(savedSearches) - 1 do
  begin
    if savedSearches[i].id = id then
    begin
      savedSearches[i].name := newName;
      result := true;
      exit;
    end;
  end;
end;

function TTaskSearchClass.GetSearchMetrics(results: TTaskArray): TSearchResultMetrics;
var
  i: integer;
begin
  FillChar(result, SizeOf(TSearchResultMetrics), 0);
  result.totalResults := Length(results);

  if result.totalResults > 0 then
  begin
    result.topMatch := results[0].title;
    result.averageScore := result.totalResults / 10.0;
  end;
end;

function TTaskSearchClass.FindDuplicates(tasks: TTaskArray): TTaskArray;
var
  i, j, count: integer;
  found: boolean;
begin
  SetLength(result, 0);
  count := 0;

  for i := 0 to Length(tasks) - 1 do
  begin
    found := false;
    for j := i + 1 to Length(tasks) - 1 do
    begin
      if (UpperCase(tasks[i].title) = UpperCase(tasks[j].title)) and
         (UpperCase(tasks[i].description) = UpperCase(tasks[j].description)) then
      begin
        found := true;
        break;
      end;
    end;

    if found then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      inc(count);
    end;
  end;
end;

function TTaskSearchClass.FindOrphaned(tasks: TTaskArray;
                                       allTaskIds: TIntegerArray): TTaskArray;
var
  i, j, count: integer;
  found: boolean;
begin
  SetLength(result, 0);
  count := 0;

  for i := 0 to Length(tasks) - 1 do
  begin
    found := false;
    for j := 0 to Length(allTaskIds) - 1 do
    begin
      if tasks[i].id = allTaskIds[j] then
      begin
        found := true;
        break;
      end;
    end;

    if not found then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      inc(count);
    end;
  end;
end;

procedure TTaskSearchClass.SelfTest();
var
  testTasks: TTaskArray;
  i: integer;
  criteria: TSearchCriteria;
  results: TTaskArray;
  savedId: integer;
  metrics: TSearchResultMetrics;
begin
  WriteLn('');
  WriteLn('=== TaskSearch Self Test ===');

  SetLength(testTasks, 5);
  for i := 0 to 4 do
  begin
    testTasks[i].id := i + 1;
    testTasks[i].title := 'Search Test Task ' + IntToStr(i + 1);
    testTasks[i].description := 'Test task for search functionality';
    testTasks[i].status := tsNotStarted;
    testTasks[i].priority := tpMedium;
    testTasks[i].dueDate := Date() + i;
  end;

  results := FullTextSearch(testTasks, 'Search');
  WriteLn('Full text search for "Search": ' + IntToStr(Length(results)) + ' results');

  FillChar(criteria, SizeOf(TSearchCriteria), 0);
  criteria.keyword := 'Test';
  criteria.searchTitle := true;
  criteria.searchDescription := true;
  criteria.statusFilter := tsNotStarted;
  criteria.priorityFilter := tpLow;
  results := SearchTasks(testTasks, criteria);
  WriteLn('Advanced search: ' + IntToStr(Length(results)) + ' results');

  savedId := SaveSearch('My Search', criteria);
  WriteLn('Saved search with ID: ' + IntToStr(savedId));

  metrics := GetSearchMetrics(results);
  WriteLn('Search metrics - Total: ' + IntToStr(metrics.totalResults) +
          ', Top: ' + metrics.topMatch);

  WriteLn('Saved searches: ' + IntToStr(Length(GetAllSavedSearches())));

  if RenameSavedSearch(savedId, 'Updated Search Name') then
    WriteLn('Search renamed successfully');

  if DeleteSavedSearch(savedId) then
    WriteLn('Search deleted successfully');

  WriteLn('=== TaskSearch Self Test Complete ===');

  SetLength(testTasks, 0);
  SetLength(results, 0);
end;

end.
