
unit taskmanagersearch;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Classes, taskmanager, taskmanagerext, taskmanageradvanced;

type
  // Search operator types
  TSearchOperator = (soAND, soOR, soNOT);
  
  // Search field types
  TSearchField = (sfTitle, sfDescription, sfCategory, sfTags, sfNotes, 
                  sfAll, sfAssignee, sfCreator);
  
  // Search comparison operators
  TComparisonOp = (coEquals, coContains, coStartsWith, coEndsWith, 
                   coGreaterThan, coLessThan, coBetween, coRegex);
  
  // Saved search/filter
  TSavedSearch = record
    ID: Integer;
    Name: string;
    Description: string;
    Query: string;
    CreatedDate: TDateTime;
    LastUsed: TDateTime;
    UseCount: Integer;
    IsFavorite: Boolean;
    IsQuickFilter: Boolean;
  end;
  TSavedSearchArray = array of TSavedSearch;
  
  // Search criteria
  TSearchCriteria = record
    Field: TSearchField;
    ComparisonOp: TComparisonOp;
    Value: string;
    Value2: string;  // For between operations
    CaseSensitive: Boolean;
  end;
  TSearchCriteriaArray = array of TSearchCriteria;
  
  // Complex search query
  TSearchQuery = record
    Criteria: TSearchCriteriaArray;
    LogicOperator: TSearchOperator;
    IncludeArchived: Boolean;
    IncludeCompleted: Boolean;
    MaxResults: Integer;
    SortBy: TSortCriteria;
    SortDescending: Boolean;
  end;
  
  // Search result with relevance score
  TSearchResult = record
    TaskID: Integer;
    RelevanceScore: Double;
    MatchedFields: array of TSearchField;
    Snippet: string;
  end;
  TSearchResultArray = array of TSearchResult;
  
  // Search history entry
  TSearchHistory = record
    ID: Integer;
    Query: string;
    Timestamp: TDateTime;
    ResultCount: Integer;
    ExecutionTimeMs: Integer;
  end;
  TSearchHistoryArray = array of TSearchHistory;
  
  // Search suggestion
  TSearchSuggestion = record
    Text: string;
    Type_: string;  // 'recent', 'popular', 'autocomplete', 'template'
    Score: Double;
  end;
  TSearchSuggestionArray = array of TSearchSuggestion;
  
  // Search index entry (for faster searching)
  TSearchIndexEntry = record
    TaskID: Integer;
    Field: TSearchField;
    Content: string;
    NormalizedContent: string;  // Lowercase, no special chars
  end;
  TSearchIndexArray = array of TSearchIndexEntry;
  
  // Search statistics
  TSearchStats = record
    TotalSearches: Integer;
    AverageExecutionTime: Double;
    MostSearchedTerms: array of string;
    PopularFilters: TSavedSearchArray;
    LastReindexed: TDateTime;
    IndexSize: Integer;
  end;

  TSearchTaskManager = class(TAdvancedTaskManager)
  private
    FSavedSearches: TSavedSearchArray;
    FSearchHistory: TSearchHistoryArray;
    FSearchIndex: TSearchIndexArray;
    FNextSavedSearchID: Integer;
    FNextHistoryID: Integer;
    FIndexEnabled: Boolean;
    FMaxHistoryEntries: Integer;
    
    function FindSavedSearchIndex(ASearchID: Integer): Integer;
    function FindHistoryIndex(AHistoryID: Integer): Integer;
    procedure AddToSearchIndex(ATaskID: Integer);
    procedure UpdateSearchIndex(ATaskID: Integer);
    procedure RemoveFromSearchIndex(ATaskID: Integer);
    function NormalizeText(const AText: string): string;
    function MatchesCriteria(ATaskID: Integer; const ACriteria: TSearchCriteria): Boolean;
    function CalculateRelevance(ATaskID: Integer; const AQuery: TSearchQuery): Double;
    function GetFieldContent(ATaskID: Integer; AField: TSearchField): string;
    procedure RecordSearchHistory(const AQuery: string; AResultCount, AExecutionTime: Integer);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Saved searches
    function CreateSavedSearch(const AName, ADescription, AQuery: string;
      AIsQuickFilter: Boolean): Integer;
    function UpdateSavedSearch(ASearchID: Integer; const AName, ADescription, 
      AQuery: string): Boolean;
    function DeleteSavedSearch(ASearchID: Integer): Boolean;
    function GetSavedSearch(ASearchID: Integer): TSavedSearch;
    function GetAllSavedSearches: TSavedSearchArray;
    function GetQuickFilters: TSavedSearchArray;
    function GetFavoriteSearches: TSavedSearchArray;
    function ToggleFavorite(ASearchID: Integer): Boolean;
    function ExecuteSavedSearch(ASearchID: Integer): TSearchResultArray;
    
    // Simple search
    function QuickSearch(const ASearchTerm: string): TSearchResultArray;
    function SearchInField(const ASearchTerm: string; AField: TSearchField): TSearchResultArray;
    function FullTextSearch(const ASearchTerm: string): TSearchResultArray;
    
    // Advanced search
    function AdvancedSearch(const AQuery: TSearchQuery): TSearchResultArray;
    function SearchWithCriteria(const ACriteria: TSearchCriteriaArray;
      ALogicOp: TSearchOperator): TSearchResultArray;
    function BooleanSearch(const AExpression: string): TSearchResultArray;
    
    // Search suggestions
    function GetSearchSuggestions(const APartialQuery: string; 
      AMaxSuggestions: Integer): TSearchSuggestionArray;
    function GetRecentSearches(ACount: Integer): TSearchHistoryArray;
    function GetPopularSearches(ACount: Integer): TSearchHistoryArray;
    function GetAutocomplete(const APrefix: string; AField: TSearchField): TStringArray;
    
    // Search history
    function GetSearchHistory(ADays: Integer): TSearchHistoryArray;
    function ClearSearchHistory: Boolean;
    function DeleteHistoryEntry(AHistoryID: Integer): Boolean;
    
    // Search index management
    procedure RebuildSearchIndex;
    procedure EnableSearchIndex(AEnabled: Boolean);
    function GetIndexStatus: string;
    function OptimizeIndex: Boolean;
    
    // Search statistics
    function GetSearchStats: TSearchStats;
    function GetMostSearchedTerms(ACount: Integer): TStringArray;
    function GetSearchTrends: string;
    
    // Utility functions
    function HighlightMatches(const AText, ASearchTerm: string): string;
    function CreateSnippet(const AText, ASearchTerm: string; 
      AContextChars: Integer): string;
    
    // Export/Import
    function ExportSavedSearches: string;
    function ImportSavedSearches(const AData: string): Integer;
    function SaveSearchDataToFile(const AFilename: string): Boolean;
    function LoadSearchDataFromFile(const AFilename: string): Boolean;
  end;

implementation

uses
  Math;

{ Private methods }

function TSearchTaskManager.FindSavedSearchIndex(ASearchID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FSavedSearches) do
    if FSavedSearches[i].ID = ASearchID then
    begin
      Result := i;
      Exit;
    end;
end;

function TSearchTaskManager.FindHistoryIndex(AHistoryID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FSearchHistory) do
    if FSearchHistory[i].ID = AHistoryID then
    begin
      Result := i;
      Exit;
    end;
end;

function TSearchTaskManager.NormalizeText(const AText: string): string;
var
  i: Integer;
  c: Char;
  lowerText: string;
begin
  Result := '';
  lowerText := LowerCase(AText);
  for i := 1 to Length(lowerText) do
  begin
    c := lowerText[i];
    if c in ['a'..'z', '0'..'9', ' '] then
      Result := Result + c;
  end;
end;

procedure TSearchTaskManager.AddToSearchIndex(ATaskID: Integer);
var
  idx: Integer;
  entry: TSearchIndexEntry;
  fields: array of TSearchField;
  i: Integer;
  content: string;
begin
  if not FIndexEnabled then Exit;
  
  // Index multiple fields
  SetLength(fields, 4);
  fields[0] := sfTitle;
  fields[1] := sfDescription;
  fields[2] := sfCategory;
  fields[3] := sfTags;
  
  for i := 0 to High(fields) do
  begin
    content := GetFieldContent(ATaskID, fields[i]);
    if content <> '' then
    begin
      entry.TaskID := ATaskID;
      entry.Field := fields[i];
      entry.Content := content;
      entry.NormalizedContent := NormalizeText(content);
      
      SetLength(FSearchIndex, Length(FSearchIndex) + 1);
      FSearchIndex[High(FSearchIndex)] := entry;
    end;
  end;
end;

procedure TSearchTaskManager.UpdateSearchIndex(ATaskID: Integer);
begin
  RemoveFromSearchIndex(ATaskID);
  AddToSearchIndex(ATaskID);
end;

procedure TSearchTaskManager.RemoveFromSearchIndex(ATaskID: Integer);
var
  i, j: Integer;
begin
  for i := High(FSearchIndex) downto 0 do
    if FSearchIndex[i].TaskID = ATaskID then
    begin
      for j := i to High(FSearchIndex) - 1 do
        FSearchIndex[j] := FSearchIndex[j + 1];
      SetLength(FSearchIndex, Length(FSearchIndex) - 1);
    end;
end;

function TSearchTaskManager.GetFieldContent(ATaskID: Integer; AField: TSearchField): string;
var
  taskIdx: Integer;
  i: Integer;
  allTasks: TTaskArray;
begin
  Result := '';
  taskIdx := GetTaskByID(ATaskID);
  if taskIdx = -1 then Exit;
  
  allTasks := GetAllTasks;
  if (taskIdx < 0) or (taskIdx > High(allTasks)) then Exit;
  
  case AField of
    sfTitle: Result := allTasks[taskIdx].Title;
    sfDescription: Result := allTasks[taskIdx].Description;
    sfCategory: Result := allTasks[taskIdx].Category;
    sfTags:
      begin
        for i := 0 to High(allTasks[taskIdx].Tags) do
          Result := Result + allTasks[taskIdx].Tags[i] + ' ';
        Result := Trim(Result);
      end;
    sfAll: Result := allTasks[taskIdx].Title + ' ' + allTasks[taskIdx].Description + 
                     ' ' + allTasks[taskIdx].Category;
  end;
end;

function TSearchTaskManager.MatchesCriteria(ATaskID: Integer; 
  const ACriteria: TSearchCriteria): Boolean;
var
  content: string;
  normalizedContent, normalizedValue: string;
begin
  Result := False;
  content := GetFieldContent(ATaskID, ACriteria.Field);
  
  if ACriteria.CaseSensitive then
  begin
    normalizedContent := content;
    normalizedValue := ACriteria.Value;
  end
  else
  begin
    normalizedContent := LowerCase(content);
    normalizedValue := LowerCase(ACriteria.Value);
  end;
  
  case ACriteria.ComparisonOp of
    coEquals: Result := (normalizedContent = normalizedValue);
    coContains: Result := (Pos(normalizedValue, normalizedContent) > 0);
    coStartsWith: Result := (Pos(normalizedValue, normalizedContent) = 1);
    coEndsWith: Result := (Copy(normalizedContent, 
      Length(normalizedContent) - Length(normalizedValue) + 1, 
      Length(normalizedValue)) = normalizedValue);
  end;
end;

function TSearchTaskManager.CalculateRelevance(ATaskID: Integer; 
  const AQuery: TSearchQuery): Double;
var
  score: Double;
  i, matchCount: Integer;
  content: string;
begin
  score := 0.0;
  matchCount := 0;
  
  for i := 0 to High(AQuery.Criteria) do
  begin
    if MatchesCriteria(ATaskID, AQuery.Criteria[i]) then
    begin
      Inc(matchCount);
      // Weight by field importance
      case AQuery.Criteria[i].Field of
        sfTitle: score := score + 10.0;
        sfDescription: score := score + 5.0;
        sfTags: score := score + 7.0;
        sfCategory: score := score + 3.0;
      else
        score := score + 1.0;
      end;
    end;
  end;
  
  // Normalize score
  if Length(AQuery.Criteria) > 0 then
    Result := score / Length(AQuery.Criteria)
  else
    Result := 0.0;
end;

procedure TSearchTaskManager.RecordSearchHistory(const AQuery: string; 
  AResultCount, AExecutionTime: Integer);
var
  entry: TSearchHistory;
begin
  entry.ID := FNextHistoryID;
  Inc(FNextHistoryID);
  entry.Query := AQuery;
  entry.Timestamp := Now;
  entry.ResultCount := AResultCount;
  entry.ExecutionTimeMs := AExecutionTime;
  
  SetLength(FSearchHistory, Length(FSearchHistory) + 1);
  FSearchHistory[High(FSearchHistory)] := entry;
  
  // Limit history size
  while Length(FSearchHistory) > FMaxHistoryEntries do
  begin
    DeleteHistoryEntry(FSearchHistory[0].ID);
  end;
end;

{ Public methods }

constructor TSearchTaskManager.Create;
begin
  inherited Create;
  SetLength(FSavedSearches, 0);
  SetLength(FSearchHistory, 0);
  SetLength(FSearchIndex, 0);
  FNextSavedSearchID := 1;
  FNextHistoryID := 1;
  FIndexEnabled := True;
  FMaxHistoryEntries := 100;
end;

destructor TSearchTaskManager.Destroy;
begin
  SetLength(FSavedSearches, 0);
  SetLength(FSearchHistory, 0);
  SetLength(FSearchIndex, 0);
  inherited Destroy;
end;

function TSearchTaskManager.CreateSavedSearch(const AName, ADescription, 
  AQuery: string; AIsQuickFilter: Boolean): Integer;
var
  search: TSavedSearch;
begin
  search.ID := FNextSavedSearchID;
  Inc(FNextSavedSearchID);
  search.Name := AName;
  search.Description := ADescription;
  search.Query := AQuery;
  search.CreatedDate := Now;
  search.LastUsed := 0;
  search.UseCount := 0;
  search.IsFavorite := False;
  search.IsQuickFilter := AIsQuickFilter;
  
  SetLength(FSavedSearches, Length(FSavedSearches) + 1);
  FSavedSearches[High(FSavedSearches)] := search;
  
  Result := search.ID;
end;

function TSearchTaskManager.UpdateSavedSearch(ASearchID: Integer; 
  const AName, ADescription, AQuery: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindSavedSearchIndex(ASearchID);
  if idx = -1 then Exit;
  
  FSavedSearches[idx].Name := AName;
  FSavedSearches[idx].Description := ADescription;
  FSavedSearches[idx].Query := AQuery;
  Result := True;
end;

function TSearchTaskManager.DeleteSavedSearch(ASearchID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindSavedSearchIndex(ASearchID);
  if idx = -1 then Exit;
  
  for i := idx to High(FSavedSearches) - 1 do
    FSavedSearches[i] := FSavedSearches[i + 1];
  SetLength(FSavedSearches, Length(FSavedSearches) - 1);
  Result := True;
end;

function TSearchTaskManager.GetSavedSearch(ASearchID: Integer): TSavedSearch;
var
  idx: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  idx := FindSavedSearchIndex(ASearchID);
  if idx <> -1 then
    Result := FSavedSearches[idx];
end;

function TSearchTaskManager.GetAllSavedSearches: TSavedSearchArray;
begin
  Result := Copy(FSavedSearches, 0, Length(FSavedSearches));
end;

function TSearchTaskManager.GetQuickFilters: TSavedSearchArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  for i := 0 to High(FSavedSearches) do
    if FSavedSearches[i].IsQuickFilter then
    begin
      SetLength(Result, count + 1);
      Result[count] := FSavedSearches[i];
      Inc(count);
    end;
end;

function TSearchTaskManager.GetFavoriteSearches: TSavedSearchArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  for i := 0 to High(FSavedSearches) do
    if FSavedSearches[i].IsFavorite then
    begin
      SetLength(Result, count + 1);
      Result[count] := FSavedSearches[i];
      Inc(count);
    end;
end;

function TSearchTaskManager.ToggleFavorite(ASearchID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindSavedSearchIndex(ASearchID);
  if idx = -1 then Exit;
  
  FSavedSearches[idx].IsFavorite := not FSavedSearches[idx].IsFavorite;
  Result := True;
end;

function TSearchTaskManager.ExecuteSavedSearch(ASearchID: Integer): TSearchResultArray;
var
  idx: Integer;
begin
  SetLength(Result, 0);
  idx := FindSavedSearchIndex(ASearchID);
  if idx = -1 then Exit;
  
  FSavedSearches[idx].LastUsed := Now;
  Inc(FSavedSearches[idx].UseCount);
  
  Result := QuickSearch(FSavedSearches[idx].Query);
end;

function TSearchTaskManager.QuickSearch(const ASearchTerm: string): TSearchResultArray;
var
  i, count: Integer;
  allTasks: TTaskArray;
  searchResult: TSearchResult;
  startTime: TDateTime;
  executionTime: Integer;
begin
  startTime := Now;
  SetLength(Result, 0);
  count := 0;
  
  allTasks := GetAllTasks;
  
  for i := 0 to High(allTasks) do
  begin
    if (Pos(LowerCase(ASearchTerm), LowerCase(allTasks[i].Title)) > 0) or
       (Pos(LowerCase(ASearchTerm), LowerCase(allTasks[i].Description)) > 0) or
       (Pos(LowerCase(ASearchTerm), LowerCase(allTasks[i].Category)) > 0) then
    begin
      searchResult.TaskID := allTasks[i].ID;
      searchResult.RelevanceScore := 1.0;
      SetLength(searchResult.MatchedFields, 1);
      searchResult.MatchedFields[0] := sfAll;
      searchResult.Snippet := CreateSnippet(allTasks[i].Description, ASearchTerm, 50);
      
      SetLength(Result, count + 1);
      Result[count] := searchResult;
      Inc(count);
    end;
  end;
  
  executionTime := MilliSecondsBetween(Now, startTime);
  RecordSearchHistory(ASearchTerm, Length(Result), executionTime);
end;

function TSearchTaskManager.SearchInField(const ASearchTerm: string; 
  AField: TSearchField): TSearchResultArray;
var
  criteria: TSearchCriteria;
  query: TSearchQuery;
begin
  criteria.Field := AField;
  criteria.ComparisonOp := coContains;
  criteria.Value := ASearchTerm;
  criteria.CaseSensitive := False;
  
  SetLength(query.Criteria, 1);
  query.Criteria[0] := criteria;
  query.LogicOperator := soAND;
  query.IncludeArchived := False;
  query.IncludeCompleted := True;
  query.MaxResults := 100;
  
  Result := AdvancedSearch(query);
end;

function TSearchTaskManager.FullTextSearch(const ASearchTerm: string): TSearchResultArray;
begin
  Result := QuickSearch(ASearchTerm);
end;

function TSearchTaskManager.AdvancedSearch(const AQuery: TSearchQuery): TSearchResultArray;
var
  i, count: Integer;
  allTasks: TTaskArray;
  matches: Boolean;
  j: Integer;
  searchResult: TSearchResult;
  startTime: TDateTime;
  executionTime: Integer;
begin
  startTime := Now;
  SetLength(Result, 0);
  count := 0;
  
  allTasks := GetAllTasks;
  
  for i := 0 to High(allTasks) do
  begin
    if (not AQuery.IncludeCompleted) and (allTasks[i].Status = tsCompleted) then
      Continue;
      
    matches := (AQuery.LogicOperator = soAND);
    
    for j := 0 to High(AQuery.Criteria) do
    begin
      case AQuery.LogicOperator of
        soAND: matches := matches and MatchesCriteria(allTasks[i].ID, AQuery.Criteria[j]);
        soOR: matches := matches or MatchesCriteria(allTasks[i].ID, AQuery.Criteria[j]);
        soNOT: matches := matches and not MatchesCriteria(allTasks[i].ID, AQuery.Criteria[j]);
      end;
    end;
    
    if matches then
    begin
      searchResult.TaskID := allTasks[i].ID;
      searchResult.RelevanceScore := CalculateRelevance(allTasks[i].ID, AQuery);
      SetLength(searchResult.MatchedFields, 0);
      searchResult.Snippet := '';
      
      SetLength(Result, count + 1);
      Result[count] := searchResult;
      Inc(count);
      
      if (AQuery.MaxResults > 0) and (count >= AQuery.MaxResults) then
        Break;
    end;
  end;
  
  executionTime := MilliSecondsBetween(Now, startTime);
  RecordSearchHistory('Advanced search', Length(Result), executionTime);
end;

function TSearchTaskManager.SearchWithCriteria(const ACriteria: TSearchCriteriaArray;
  ALogicOp: TSearchOperator): TSearchResultArray;
var
  query: TSearchQuery;
begin
  query.Criteria := ACriteria;
  query.LogicOperator := ALogicOp;
  query.IncludeArchived := False;
  query.IncludeCompleted := True;
  query.MaxResults := 100;
  
  Result := AdvancedSearch(query);
end;

function TSearchTaskManager.BooleanSearch(const AExpression: string): TSearchResultArray;
begin
  // Simplified boolean search - just use QuickSearch for now
  Result := QuickSearch(AExpression);
end;

function TSearchTaskManager.GetSearchSuggestions(const APartialQuery: string; 
  AMaxSuggestions: Integer): TSearchSuggestionArray;
var
  i, count: Integer;
  suggestion: TSearchSuggestion;
begin
  SetLength(Result, 0);
  count := 0;
  
  // Get recent searches that match
  for i := High(FSearchHistory) downto 0 do
  begin
    if Pos(LowerCase(APartialQuery), LowerCase(FSearchHistory[i].Query)) > 0 then
    begin
      suggestion.Text := FSearchHistory[i].Query;
      suggestion.Type_ := 'recent';
      suggestion.Score := 1.0;
      
      SetLength(Result, count + 1);
      Result[count] := suggestion;
      Inc(count);
      
      if count >= AMaxSuggestions then
        Break;
    end;
  end;
end;

function TSearchTaskManager.GetRecentSearches(ACount: Integer): TSearchHistoryArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := High(FSearchHistory) downto 0 do
  begin
    SetLength(Result, count + 1);
    Result[count] := FSearchHistory[i];
    Inc(count);
    
    if count >= ACount then
      Break;
  end;
end;

function TSearchTaskManager.GetPopularSearches(ACount: Integer): TSearchHistoryArray;
begin
  // For now, return recent searches
  Result := GetRecentSearches(ACount);
end;

function TSearchTaskManager.GetAutocomplete(const APrefix: string; 
  AField: TSearchField): TStringArray;
begin
  SetLength(Result, 0);
  // TODO: Implement autocomplete based on existing field values
end;

function TSearchTaskManager.GetSearchHistory(ADays: Integer): TSearchHistoryArray;
var
  i, count: Integer;
  cutoffDate: TDateTime;
begin
  SetLength(Result, 0);
  count := 0;
  cutoffDate := Now - ADays;
  
  for i := 0 to High(FSearchHistory) do
  begin
    if FSearchHistory[i].Timestamp >= cutoffDate then
    begin
      SetLength(Result, count + 1);
      Result[count] := FSearchHistory[i];
      Inc(count);
    end;
  end;
end;

function TSearchTaskManager.ClearSearchHistory: Boolean;
begin
  SetLength(FSearchHistory, 0);
  Result := True;
end;

function TSearchTaskManager.DeleteHistoryEntry(AHistoryID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindHistoryIndex(AHistoryID);
  if idx = -1 then Exit;
  
  for i := idx to High(FSearchHistory) - 1 do
    FSearchHistory[i] := FSearchHistory[i + 1];
  SetLength(FSearchHistory, Length(FSearchHistory) - 1);
  Result := True;
end;

procedure TSearchTaskManager.RebuildSearchIndex;
var
  i: Integer;
  allTasks: TTaskArray;
begin
  SetLength(FSearchIndex, 0);
  allTasks := GetAllTasks;
  
  for i := 0 to High(allTasks) do
    AddToSearchIndex(allTasks[i].ID);
end;

procedure TSearchTaskManager.EnableSearchIndex(AEnabled: Boolean);
begin
  FIndexEnabled := AEnabled;
  if AEnabled then
    RebuildSearchIndex;
end;

function TSearchTaskManager.GetIndexStatus: string;
begin
  Result := Format('Index: %s, Entries: %d, Tasks indexed: %d',
    [BoolToStr(FIndexEnabled, 'Enabled', 'Disabled'),
     Length(FSearchIndex),
     Length(GetAllTasks)]);
end;

function TSearchTaskManager.OptimizeIndex: Boolean;
begin
  RebuildSearchIndex;
  Result := True;
end;

function TSearchTaskManager.GetSearchStats: TSearchStats;
var
  i: Integer;
  totalTime: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TotalSearches := Length(FSearchHistory);
  
  totalTime := 0;
  for i := 0 to High(FSearchHistory) do
    totalTime := totalTime + FSearchHistory[i].ExecutionTimeMs;
  
  if Length(FSearchHistory) > 0 then
    Result.AverageExecutionTime := totalTime / Length(FSearchHistory)
  else
    Result.AverageExecutionTime := 0.0;
  
  Result.LastReindexed := Now;
  Result.IndexSize := Length(FSearchIndex);
  Result.PopularFilters := GetQuickFilters;
end;

function TSearchTaskManager.GetMostSearchedTerms(ACount: Integer): TStringArray;
begin
  SetLength(Result, 0);
  // TODO: Implement term frequency analysis
end;

function TSearchTaskManager.GetSearchTrends: string;
begin
  Result := Format('Total searches: %d, Average execution time: %.2f ms',
    [Length(FSearchHistory), GetSearchStats.AverageExecutionTime]);
end;

function TSearchTaskManager.HighlightMatches(const AText, ASearchTerm: string): string;
var
  pos_: Integer;
begin
  Result := AText;
  pos_ := Pos(LowerCase(ASearchTerm), LowerCase(AText));
  if pos_ > 0 then
    Result := Copy(AText, 1, pos_ - 1) + '[' + 
              Copy(AText, pos_, Length(ASearchTerm)) + ']' +
              Copy(AText, pos_ + Length(ASearchTerm), Length(AText));
end;

function TSearchTaskManager.CreateSnippet(const AText, ASearchTerm: string; 
  AContextChars: Integer): string;
var
  pos_: Integer;
  startPos, endPos: Integer;
begin
  Result := '';
  if AText = '' then Exit;
  
  pos_ := Pos(LowerCase(ASearchTerm), LowerCase(AText));
  
  if pos_ > 0 then
  begin
    startPos := Max(1, pos_ - AContextChars);
    endPos := Min(Length(AText), pos_ + Length(ASearchTerm) + AContextChars);
    Result := Copy(AText, startPos, endPos - startPos + 1);
    
    if startPos > 1 then
      Result := '...' + Result;
    if endPos < Length(AText) then
      Result := Result + '...';
  end
  else
    Result := Copy(AText, 1, Min(Length(AText), AContextChars * 2));
end;

function TSearchTaskManager.ExportSavedSearches: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(FSavedSearches) do
  begin
    Result := Result + Format('%s|%s|%s'#13#10,
      [FSavedSearches[i].Name,
       FSavedSearches[i].Description,
       FSavedSearches[i].Query]);
  end;
end;

function TSearchTaskManager.ImportSavedSearches(const AData: string): Integer;
begin
  Result := 0;
  // TODO: Implement import from formatted string
end;

function TSearchTaskManager.SaveSearchDataToFile(const AFilename: string): Boolean;
var
  f: TextFile;
  i: Integer;
begin
  Result := False;
  try
    AssignFile(f, AFilename);
    Rewrite(f);
    
    WriteLn(f, '[SavedSearches]');
    for i := 0 to High(FSavedSearches) do
    begin
      WriteLn(f, Format('%d|%s|%s|%s|%s|%d|%s',
        [FSavedSearches[i].ID,
         FSavedSearches[i].Name,
         FSavedSearches[i].Description,
         FSavedSearches[i].Query,
         DateTimeToStr(FSavedSearches[i].CreatedDate),
         FSavedSearches[i].UseCount,
         BoolToStr(FSavedSearches[i].IsFavorite, True)]));
    end;
    
    CloseFile(f);
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TSearchTaskManager.LoadSearchDataFromFile(const AFilename: string): Boolean;
begin
  Result := False;
  // TODO: Implement loading from file
end;

end.
