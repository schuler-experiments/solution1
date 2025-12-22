
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
                     aDueDate: TDateTime): integer;
    function DeleteTask(aID: integer): boolean;
    function UpdateTask(aID: integer; const aTitle, aDescription: string;
                        aPriority: TTaskPriority; aStatus: TTaskStatus;
                        aDueDate: TDateTime): boolean;
    function GetTask(aID: integer; out aTask: TTask): boolean;
    
    // Status management
    function SetTaskStatus(aID: integer; aStatus: TTaskStatus): boolean;
    function CompleteTask(aID: integer): boolean;
    function CancelTask(aID: integer): boolean;
    
    // Query operations
    function GetAllTasks: TTaskArray;
    function GetActiveTasks: TTaskArray;
    function SearchTasks(const aCriteria: TSearchCriteria): TTaskArray;
    function GetTasksByStatus(aStatus: TTaskStatus): TTaskArray;
    function GetTasksByPriority(aPriority: TTaskPriority): TTaskArray;
    function GetOverdueTasks: TTaskArray;
    
    // Statistics
    function GetStatistics: TTaskStatistics;
    
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
  titleLower, descLower, searchTitleLower, searchDescLower: string;
begin
  SetLength(Result, 0);
  idx := 0;
  
  searchTitleLower := LowerCase(aCriteria.SearchTitle);
  searchDescLower := LowerCase(aCriteria.SearchDescription);
  
  for i := 0 to High(FTasks) do
  begin
    if not FTasks[i].IsActive then
      Continue;
      
    match := true;
    
    // Check title
    if searchTitleLower <> '' then
    begin
      titleLower := LowerCase(FTasks[i].Title);
      if Pos(searchTitleLower, titleLower) = 0 then
        match := false;
    end;
    
    // Check description
    if match and (searchDescLower <> '') then
    begin
      descLower := LowerCase(FTasks[i].Description);
      if Pos(searchDescLower, descLower) = 0 then
        match := false;
    end;
    
    // Check status filter
    if match and aCriteria.UseStatusFilter then
      if FTasks[i].Status <> aCriteria.FilterStatus then
        match := false;
    
    // Check priority filter
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
      
      case FTasks[i].Status of
        tsCompleted: Inc(stats.CompletedTasks);
        tsCancelled: Inc(stats.CancelledTasks);
      else
        Inc(stats.ActiveTasks);
      end;
      
      if FTasks[i].Priority = tpHigh then
        Inc(stats.HighPriorityTasks);
        
      if (FTasks[i].Status <> tsCompleted) and (FTasks[i].Status <> tsCancelled) and
         (FTasks[i].DueDate > 0) and (FTasks[i].DueDate < now) then
        Inc(stats.OverdueTasks);
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
