
unit TaskManager;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math, TaskTypes;

type
  // Main task manager class
  TTaskManagerCore = class
  private
    FTasks: TTaskArray;
    FNextID: integer;
    FModified: boolean;
    
    function GetTaskCount: integer;
    function FindTaskIndex(aID: integer): integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Task CRUD operations
    function AddTask(const aTitle, aDescription: string; 
                     aPriority: TTaskPriority; 
                     aDueDate: TDateTime): integer; overload;
    function AddTask(const aTitle, aDescription, aCategory: string;
                     aPriority: TTaskPriority;
                     aDueDate: TDateTime;
                     aEstimatedHours: double): integer; overload;
    function DeleteTask(aID: integer): boolean;
    function UpdateTask(aID: integer; const aTitle, aDescription: string;
                        aPriority: TTaskPriority; aStatus: TTaskStatus;
                        aDueDate: TDateTime): boolean;
    function GetTask(aID: integer; out aTask: TTask): boolean;
    
    // Status management
    function SetTaskStatus(aID: integer; aStatus: TTaskStatus): boolean;
    function CompleteTask(aID: integer): boolean;
    function CancelTask(aID: integer): boolean;
    
    // NEW: Category management
    function SetTaskCategory(aID: integer; const aCategory: string): boolean;
    function GetTasksByCategory(const aCategory: string): TTaskArray;
    
    // NEW: Dependency management
    function AddTaskDependency(aTaskID, aDependsOnID: integer): boolean;
    function RemoveTaskDependency(aTaskID, aDependsOnID: integer): boolean;
    function GetTaskDependencies(aTaskID: integer): TTaskArray;
    function CanStartTask(aTaskID: integer): boolean;
    
    // NEW: Time tracking
    function SetEstimatedHours(aTaskID: integer; aHours: double): boolean;
    function SetActualHours(aTaskID: integer; aHours: double): boolean;
    function AddActualHours(aTaskID: integer; aHours: double): boolean;
    
    // Query operations
    function GetAllTasks: TTaskArray;
    function GetActiveTasks: TTaskArray;
    function SearchTasks(const aCriteria: TSearchCriteria): TTaskArray;
    function GetTasksByStatus(aStatus: TTaskStatus): TTaskArray;
    function GetTasksByPriority(aPriority: TTaskPriority): TTaskArray;
    function GetOverdueTasks: TTaskArray;
    
    // Statistics
    function GetStatistics: TTaskStatistics;
    function GetCategoryStatistics: TCategoryStatisticsArray;
    
    // Utility
    procedure ClearAll;
    function GetTasksArray: TTaskArray;
    procedure SetTasksArray(const aTasks: TTaskArray; aNextID: integer);
    
    property TaskCount: integer read GetTaskCount;
    property Modified: boolean read FModified write FModified;
    property NextID: integer read FNextID;
  end;

implementation

{ TTaskManagerCore }

constructor TTaskManagerCore.Create;
begin
  inherited Create;
  SetLength(FTasks, 0);
  FNextID := 1;
  FModified := false;
end;

destructor TTaskManagerCore.Destroy;
begin
  SetLength(FTasks, 0);
  inherited Destroy;
end;

function TTaskManagerCore.GetTaskCount: integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to High(FTasks) do
    if FTasks[i].IsActive then
      Inc(count);
  Result := count;
end;

function TTaskManagerCore.FindTaskIndex(aID: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to High(FTasks) do
  begin
    if (FTasks[i].ID = aID) and FTasks[i].IsActive then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTaskManagerCore.AddTask(const aTitle, aDescription: string;
  aPriority: TTaskPriority; aDueDate: TDateTime): integer;
begin
  Result := AddTask(aTitle, aDescription, '', aPriority, aDueDate, 0);
end;

function TTaskManagerCore.AddTask(const aTitle, aDescription, aCategory: string;
  aPriority: TTaskPriority; aDueDate: TDateTime; aEstimatedHours: double): integer;
var
  idx: integer;
  newTask: TTask;
begin
  newTask.ID := FNextID;
  newTask.Title := aTitle;
  newTask.Description := aDescription;
  newTask.Priority := aPriority;
  newTask.Status := tsNew;
  newTask.CreatedDate := Now;
  newTask.DueDate := aDueDate;
  newTask.CompletedDate := 0;
  newTask.Tags := '';
  newTask.IsActive := true;
  newTask.Category := aCategory;
  newTask.DependsOnIDs := '';
  newTask.EstimatedHours := aEstimatedHours;
  newTask.ActualHours := 0;
  newTask.LastModifiedDate := Now;
  
  idx := Length(FTasks);
  SetLength(FTasks, idx + 1);
  FTasks[idx] := newTask;
  
  Inc(FNextID);
  FModified := true;
  Result := newTask.ID;
end;

function TTaskManagerCore.DeleteTask(aID: integer): boolean;
var
  idx: integer;
begin
  idx := FindTaskIndex(aID);
  Result := idx >= 0;
  if Result then
  begin
    FTasks[idx].IsActive := false;
    FTasks[idx].LastModifiedDate := Now;
    FModified := true;
  end;
end;

function TTaskManagerCore.UpdateTask(aID: integer; const aTitle, aDescription: string;
  aPriority: TTaskPriority; aStatus: TTaskStatus; aDueDate: TDateTime): boolean;
var
  idx: integer;
begin
  idx := FindTaskIndex(aID);
  Result := idx >= 0;
  if Result then
  begin
    FTasks[idx].Title := aTitle;
    FTasks[idx].Description := aDescription;
    FTasks[idx].Priority := aPriority;
    FTasks[idx].Status := aStatus;
    FTasks[idx].DueDate := aDueDate;
    FTasks[idx].LastModifiedDate := Now;
    
    if (aStatus = tsCompleted) and (FTasks[idx].CompletedDate = 0) then
      FTasks[idx].CompletedDate := Now;
      
    FModified := true;
  end;
end;

function TTaskManagerCore.GetTask(aID: integer; out aTask: TTask): boolean;
var
  idx: integer;
begin
  idx := FindTaskIndex(aID);
  Result := idx >= 0;
  if Result then
    aTask := FTasks[idx];
end;

function TTaskManagerCore.SetTaskStatus(aID: integer; aStatus: TTaskStatus): boolean;
var
  idx: integer;
begin
  idx := FindTaskIndex(aID);
  Result := idx >= 0;
  if Result then
  begin
    FTasks[idx].Status := aStatus;
    FTasks[idx].LastModifiedDate := Now;
    if (aStatus = tsCompleted) and (FTasks[idx].CompletedDate = 0) then
      FTasks[idx].CompletedDate := Now;
    FModified := true;
  end;
end;

function TTaskManagerCore.CompleteTask(aID: integer): boolean;
begin
  Result := SetTaskStatus(aID, tsCompleted);
end;

function TTaskManagerCore.CancelTask(aID: integer): boolean;
begin
  Result := SetTaskStatus(aID, tsCancelled);
end;

function TTaskManagerCore.SetTaskCategory(aID: integer; const aCategory: string): boolean;
var
  idx: integer;
begin
  idx := FindTaskIndex(aID);
  Result := idx >= 0;
  if Result then
  begin
    FTasks[idx].Category := aCategory;
    FTasks[idx].LastModifiedDate := Now;
    FModified := true;
  end;
end;

function TTaskManagerCore.GetTasksByCategory(const aCategory: string): TTaskArray;
var
  i, idx: integer;
begin
  SetLength(Result, 0);
  idx := 0;
  
  for i := 0 to High(FTasks) do
  begin
    if FTasks[i].IsActive and (LowerCase(FTasks[i].Category) = LowerCase(aCategory)) then
    begin
      SetLength(Result, idx + 1);
      Result[idx] := FTasks[i];
      Inc(idx);
    end;
  end;
end;

function TTaskManagerCore.AddTaskDependency(aTaskID, aDependsOnID: integer): boolean;
var
  idx: integer;
begin
  idx := FindTaskIndex(aTaskID);
  Result := idx >= 0;
  if Result then
  begin
    Result := AddDependency(FTasks[idx], aDependsOnID);
    if Result then
    begin
      FTasks[idx].LastModifiedDate := Now;
      FModified := true;
    end;
  end;
end;

function TTaskManagerCore.RemoveTaskDependency(aTaskID, aDependsOnID: integer): boolean;
var
  idx: integer;
begin
  idx := FindTaskIndex(aTaskID);
  Result := idx >= 0;
  if Result then
  begin
    Result := RemoveDependency(FTasks[idx], aDependsOnID);
    if Result then
    begin
      FTasks[idx].LastModifiedDate := Now;
      FModified := true;
    end;
  end;
end;

function TTaskManagerCore.GetTaskDependencies(aTaskID: integer): TTaskArray;
var
  idx, i, j: integer;
  task: TTask;
  depIDs: array of integer;
begin
  SetLength(Result, 0);
  
  idx := FindTaskIndex(aTaskID);
  if idx < 0 then
    Exit;
  
  task := FTasks[idx];
  depIDs := GetDependencyIDs(task);
  
  for i := 0 to High(depIDs) do
  begin
    for j := 0 to High(FTasks) do
    begin
      if (FTasks[j].ID = depIDs[i]) and FTasks[j].IsActive then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FTasks[j];
        Break;
      end;
    end;
  end;
end;

function TTaskManagerCore.CanStartTask(aTaskID: integer): boolean;
var
  deps: TTaskArray;
  i: integer;
begin
  Result := true;
  deps := GetTaskDependencies(aTaskID);
  
  for i := 0 to High(deps) do
  begin
    if deps[i].Status <> tsCompleted then
    begin
      Result := false;
      Exit;
    end;
  end;
end;

function TTaskManagerCore.SetEstimatedHours(aTaskID: integer; aHours: double): boolean;
var
  idx: integer;
begin
  idx := FindTaskIndex(aTaskID);
  Result := idx >= 0;
  if Result then
  begin
    FTasks[idx].EstimatedHours := aHours;
    FTasks[idx].LastModifiedDate := Now;
    FModified := true;
  end;
end;

function TTaskManagerCore.SetActualHours(aTaskID: integer; aHours: double): boolean;
var
  idx: integer;
begin
  idx := FindTaskIndex(aTaskID);
  Result := idx >= 0;
  if Result then
  begin
    FTasks[idx].ActualHours := aHours;
    FTasks[idx].LastModifiedDate := Now;
    FModified := true;
  end;
end;

function TTaskManagerCore.AddActualHours(aTaskID: integer; aHours: double): boolean;
var
  idx: integer;
begin
  idx := FindTaskIndex(aTaskID);
  Result := idx >= 0;
  if Result then
  begin
    FTasks[idx].ActualHours := FTasks[idx].ActualHours + aHours;
    FTasks[idx].LastModifiedDate := Now;
    FModified := true;
  end;
end;

function TTaskManagerCore.GetAllTasks: TTaskArray;
var
  i, idx: integer;
begin
  SetLength(Result, 0);
  idx := 0;
  for i := 0 to High(FTasks) do
  begin
    if FTasks[i].IsActive then
    begin
      SetLength(Result, idx + 1);
      Result[idx] := FTasks[i];
      Inc(idx);
    end;
  end;
end;

function TTaskManagerCore.GetActiveTasks: TTaskArray;
var
  i, idx: integer;
begin
  SetLength(Result, 0);
  idx := 0;
  for i := 0 to High(FTasks) do
  begin
    if FTasks[i].IsActive and (FTasks[i].Status <> tsCompleted) and 
       (FTasks[i].Status <> tsCancelled) then
    begin
      SetLength(Result, idx + 1);
      Result[idx] := FTasks[i];
      Inc(idx);
    end;
  end;
end;

function TTaskManagerCore.SearchTasks(const aCriteria: TSearchCriteria): TTaskArray;
var
  i, idx: integer;
  match: boolean;
  titleLower, descLower, searchTitleLower, searchDescLower, categoryLower, searchCategoryLower: string;
begin
  SetLength(Result, 0);
  idx := 0;
  
  searchTitleLower := LowerCase(aCriteria.SearchTitle);
  searchDescLower := LowerCase(aCriteria.SearchDescription);
  searchCategoryLower := LowerCase(aCriteria.SearchCategory);
  
  for i := 0 to High(FTasks) do
  begin
    if not FTasks[i].IsActive then
      Continue;
      
    match := true;
    
    if searchTitleLower <> '' then
    begin
      titleLower := LowerCase(FTasks[i].Title);
      if Pos(searchTitleLower, titleLower) = 0 then
        match := false;
    end;
    
    if match and (searchDescLower <> '') then
    begin
      descLower := LowerCase(FTasks[i].Description);
      if Pos(searchDescLower, descLower) = 0 then
        match := false;
    end;
    
    if match and (searchCategoryLower <> '') then
    begin
      categoryLower := LowerCase(FTasks[i].Category);
      if Pos(searchCategoryLower, categoryLower) = 0 then
        match := false;
    end;
    
    if match and aCriteria.UseStatusFilter then
      if FTasks[i].Status <> aCriteria.FilterStatus then
        match := false;
    
    if match and aCriteria.UsePriorityFilter then
      if FTasks[i].Priority <> aCriteria.FilterPriority then
        match := false;
    
    if match then
    begin
      SetLength(Result, idx + 1);
      Result[idx] := FTasks[i];
      Inc(idx);
    end;
  end;
end;

function TTaskManagerCore.GetTasksByStatus(aStatus: TTaskStatus): TTaskArray;
var
  criteria: TSearchCriteria;
begin
  FillChar(criteria, SizeOf(criteria), 0);
  criteria.FilterStatus := aStatus;
  criteria.UseStatusFilter := true;
  Result := SearchTasks(criteria);
end;

function TTaskManagerCore.GetTasksByPriority(aPriority: TTaskPriority): TTaskArray;
var
  criteria: TSearchCriteria;
begin
  FillChar(criteria, SizeOf(criteria), 0);
  criteria.FilterPriority := aPriority;
  criteria.UsePriorityFilter := true;
  Result := SearchTasks(criteria);
end;

function TTaskManagerCore.GetOverdueTasks: TTaskArray;
var
  i, idx: integer;
  now: TDateTime;
begin
  SetLength(Result, 0);
  idx := 0;
  now := Now;
  
  for i := 0 to High(FTasks) do
  begin
    if FTasks[i].IsActive and (FTasks[i].Status <> tsCompleted) and
       (FTasks[i].Status <> tsCancelled) and (FTasks[i].DueDate > 0) and
       (FTasks[i].DueDate < now) then
    begin
      SetLength(Result, idx + 1);
      Result[idx] := FTasks[i];
      Inc(idx);
    end;
  end;
end;

function TTaskManagerCore.GetStatistics: TTaskStatistics;
var
  i: integer;
  stats: TTaskStatistics;
  now: TDateTime;
begin
  FillChar(stats, SizeOf(stats), 0);
  now := Now;
  
  for i := 0 to High(FTasks) do
  begin
    if FTasks[i].IsActive then
    begin
      Inc(stats.TotalTasks);
      stats.TotalEstimatedHours := stats.TotalEstimatedHours + FTasks[i].EstimatedHours;
      stats.TotalActualHours := stats.TotalActualHours + FTasks[i].ActualHours;
      
      if HasDependencies(FTasks[i]) then
        Inc(stats.TasksWithDependencies);
      
      case FTasks[i].Status of
        tsCompleted: Inc(stats.CompletedTasks);
        tsCancelled: Inc(stats.CancelledTasks);
      else
        Inc(stats.ActiveTasks);
      end;
      
      if (FTasks[i].Priority = tpHigh) or (FTasks[i].Priority = tpCritical) then
        Inc(stats.HighPriorityTasks);
        
      if (FTasks[i].Status <> tsCompleted) and (FTasks[i].Status <> tsCancelled) and
         (FTasks[i].DueDate > 0) and (FTasks[i].DueDate < now) then
        Inc(stats.OverdueTasks);
    end;
  end;
  
  Result := stats;
end;

function TTaskManagerCore.GetCategoryStatistics: TCategoryStatisticsArray;
var
  categories: array of string;
  i, j, catIdx: integer;
  cat: string;
  found: boolean;
  stats: TCategoryStatisticsArray;
begin
  categories := GetUniqueCategories(FTasks);
  SetLength(stats, Length(categories));
  
  for i := 0 to High(categories) do
  begin
    stats[i].CategoryName := categories[i];
    stats[i].TotalTasks := 0;
    stats[i].CompletedTasks := 0;
    stats[i].ActiveTasks := 0;
    stats[i].EstimatedHours := 0;
    stats[i].ActualHours := 0;
  end;
  
  for i := 0 to High(FTasks) do
  begin
    if not FTasks[i].IsActive then
      Continue;
      
    cat := Trim(FTasks[i].Category);
    if cat = '' then
      Continue;
      
    catIdx := -1;
    for j := 0 to High(categories) do
    begin
      if categories[j] = cat then
      begin
        catIdx := j;
        Break;
      end;
    end;
    
    if catIdx >= 0 then
    begin
      Inc(stats[catIdx].TotalTasks);
      stats[catIdx].EstimatedHours := stats[catIdx].EstimatedHours + FTasks[i].EstimatedHours;
      stats[catIdx].ActualHours := stats[catIdx].ActualHours + FTasks[i].ActualHours;
      
      if FTasks[i].Status = tsCompleted then
        Inc(stats[catIdx].CompletedTasks)
      else if FTasks[i].Status <> tsCancelled then
        Inc(stats[catIdx].ActiveTasks);
    end;
  end;
  
  Result := stats;
end;

procedure TTaskManagerCore.ClearAll;
begin
  SetLength(FTasks, 0);
  FNextID := 1;
  FModified := true;
end;

function TTaskManagerCore.GetTasksArray: TTaskArray;
begin
  Result := FTasks;
end;

procedure TTaskManagerCore.SetTasksArray(const aTasks: TTaskArray; aNextID: integer);
begin
  FTasks := aTasks;
  FNextID := aNextID;
  FModified := false;
end;

end.
