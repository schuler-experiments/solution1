
unit taskmanager;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, DateUtils, Math;

type
  // Task status enumeration
  TTaskStatus = (tsNotStarted, tsInProgress, tsCompleted, tsCancelled, tsOnHold);
  
  // Task priority enumeration
  TTaskPriority = (tpLow, tpMedium, tpHigh, tpCritical);
  
  // Task record structure
  TTask = record
    ID: Integer;
    Title: string;
    Description: string;
    Status: TTaskStatus;
    Priority: TTaskPriority;
    CreatedDate: TDateTime;
    DueDate: TDateTime;
    CompletedDate: TDateTime;
    Tags: array of string;
  end;
  
  // Dynamic array of tasks
  TTaskArray = array of TTask;
  
  // Task Manager class
  TTaskManager = class
  private
    FTasks: TTaskArray;
    FNextID: Integer;
    function GetTaskCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Core operations
    function AddTask(const ATitle, ADescription: string; 
                     APriority: TTaskPriority; ADueDate: TDateTime): Integer;
    function DeleteTask(ATaskID: Integer): Boolean;
    function UpdateTaskTitle(ATaskID: Integer; const ANewTitle: string): Boolean;
    function UpdateTaskDescription(ATaskID: Integer; const ANewDesc: string): Boolean;
    function UpdateTaskStatus(ATaskID: Integer; ANewStatus: TTaskStatus): Boolean;
    function UpdateTaskPriority(ATaskID: Integer; ANewPriority: TTaskPriority): Boolean;
    function UpdateTaskDueDate(ATaskID: Integer; ANewDueDate: TDateTime): Boolean;
    
    // Search and filter
    function GetTaskByID(ATaskID: Integer): Integer; // Returns index, -1 if not found
    function GetAllTasks: TTaskArray;
    function FilterByStatus(AStatus: TTaskStatus): TTaskArray;
    function FilterByPriority(APriority: TTaskPriority): TTaskArray;
    function FilterByDateRange(AStartDate, AEndDate: TDateTime): TTaskArray;
    function SearchByTitle(const ASearchTerm: string): TTaskArray;
    
    // Tag management
    function AddTagToTask(ATaskID: Integer; const ATag: string): Boolean;
    function RemoveTagFromTask(ATaskID: Integer; const ATag: string): Boolean;
    function FilterByTag(const ATag: string): TTaskArray;
    
    // Statistics
    function GetCompletedCount: Integer;
    function GetPendingCount: Integer;
    function GetOverdueCount: Integer;
    function GetCompletionRate: Double;
    
    // Utility functions
    procedure ClearAllTasks;
    function TaskStatusToString(AStatus: TTaskStatus): string;
    function TaskPriorityToString(APriority: TTaskPriority): string;
    function TaskToString(const ATask: TTask): string;
    
    property TaskCount: Integer read GetTaskCount;
  end;

implementation

{ TTaskManager }

constructor TTaskManager.Create;
begin
  inherited Create;
  SetLength(FTasks, 0);
  FNextID := 1;
end;

destructor TTaskManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FTasks) - 1 do
    SetLength(FTasks[i].Tags, 0);
  SetLength(FTasks, 0);
  inherited Destroy;
end;

function TTaskManager.GetTaskCount: Integer;
begin
  Result := Length(FTasks);
end;

function TTaskManager.AddTask(const ATitle, ADescription: string;
  APriority: TTaskPriority; ADueDate: TDateTime): Integer;
var
  NewTask: TTask;
  Idx: Integer;
begin
  NewTask.ID := FNextID;
  NewTask.Title := ATitle;
  NewTask.Description := ADescription;
  NewTask.Status := tsNotStarted;
  NewTask.Priority := APriority;
  NewTask.CreatedDate := Now;
  NewTask.DueDate := ADueDate;
  NewTask.CompletedDate := 0;
  SetLength(NewTask.Tags, 0);
  
  Idx := Length(FTasks);
  SetLength(FTasks, Idx + 1);
  FTasks[Idx] := NewTask;
  
  Result := FNextID;
  Inc(FNextID);
end;

function TTaskManager.DeleteTask(ATaskID: Integer): Boolean;
var
  Idx, i: Integer;
begin
  Idx := GetTaskByID(ATaskID);
  Result := Idx >= 0;
  
  if Result then
  begin
    SetLength(FTasks[Idx].Tags, 0);
    for i := Idx to Length(FTasks) - 2 do
      FTasks[i] := FTasks[i + 1];
    SetLength(FTasks, Length(FTasks) - 1);
  end;
end;

function TTaskManager.UpdateTaskTitle(ATaskID: Integer; const ANewTitle: string): Boolean;
var
  Idx: Integer;
begin
  Idx := GetTaskByID(ATaskID);
  Result := Idx >= 0;
  if Result then
    FTasks[Idx].Title := ANewTitle;
end;

function TTaskManager.UpdateTaskDescription(ATaskID: Integer; const ANewDesc: string): Boolean;
var
  Idx: Integer;
begin
  Idx := GetTaskByID(ATaskID);
  Result := Idx >= 0;
  if Result then
    FTasks[Idx].Description := ANewDesc;
end;

function TTaskManager.UpdateTaskStatus(ATaskID: Integer; ANewStatus: TTaskStatus): Boolean;
var
  Idx: Integer;
begin
  Idx := GetTaskByID(ATaskID);
  Result := Idx >= 0;
  if Result then
  begin
    FTasks[Idx].Status := ANewStatus;
    if ANewStatus = tsCompleted then
      FTasks[Idx].CompletedDate := Now;
  end;
end;

function TTaskManager.UpdateTaskPriority(ATaskID: Integer; ANewPriority: TTaskPriority): Boolean;
var
  Idx: Integer;
begin
  Idx := GetTaskByID(ATaskID);
  Result := Idx >= 0;
  if Result then
    FTasks[Idx].Priority := ANewPriority;
end;

function TTaskManager.UpdateTaskDueDate(ATaskID: Integer; ANewDueDate: TDateTime): Boolean;
var
  Idx: Integer;
begin
  Idx := GetTaskByID(ATaskID);
  Result := Idx >= 0;
  if Result then
    FTasks[Idx].DueDate := ANewDueDate;
end;

function TTaskManager.GetTaskByID(ATaskID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FTasks) - 1 do
  begin
    if FTasks[i].ID = ATaskID then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTaskManager.GetAllTasks: TTaskArray;
begin
  Result := Copy(FTasks, 0, Length(FTasks));
end;

function TTaskManager.FilterByStatus(AStatus: TTaskStatus): TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FTasks) - 1 do
  begin
    if FTasks[i].Status = AStatus then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByPriority(APriority: TTaskPriority): TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FTasks) - 1 do
  begin
    if FTasks[i].Priority = APriority then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByDateRange(AStartDate, AEndDate: TDateTime): TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FTasks) - 1 do
  begin
    if (FTasks[i].DueDate >= AStartDate) and (FTasks[i].DueDate <= AEndDate) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.SearchByTitle(const ASearchTerm: string): TTaskArray;
var
  i, Count: Integer;
  LowerSearch, LowerTitle: string;
begin
  SetLength(Result, 0);
  Count := 0;
  LowerSearch := LowerCase(ASearchTerm);
  
  for i := 0 to Length(FTasks) - 1 do
  begin
    LowerTitle := LowerCase(FTasks[i].Title);
    if Pos(LowerSearch, LowerTitle) > 0 then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.AddTagToTask(ATaskID: Integer; const ATag: string): Boolean;
var
  Idx, TagIdx: Integer;
begin
  Idx := GetTaskByID(ATaskID);
  Result := Idx >= 0;
  
  if Result then
  begin
    TagIdx := Length(FTasks[Idx].Tags);
    SetLength(FTasks[Idx].Tags, TagIdx + 1);
    FTasks[Idx].Tags[TagIdx] := ATag;
  end;
end;

function TTaskManager.RemoveTagFromTask(ATaskID: Integer; const ATag: string): Boolean;
var
  Idx, i, j: Integer;
begin
  Idx := GetTaskByID(ATaskID);
  Result := False;
  
  if Idx >= 0 then
  begin
    for i := 0 to Length(FTasks[Idx].Tags) - 1 do
    begin
      if FTasks[Idx].Tags[i] = ATag then
      begin
        for j := i to Length(FTasks[Idx].Tags) - 2 do
          FTasks[Idx].Tags[j] := FTasks[Idx].Tags[j + 1];
        SetLength(FTasks[Idx].Tags, Length(FTasks[Idx].Tags) - 1);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TTaskManager.FilterByTag(const ATag: string): TTaskArray;
var
  i, j, Count: Integer;
  HasTag: Boolean;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FTasks) - 1 do
  begin
    HasTag := False;
    for j := 0 to Length(FTasks[i].Tags) - 1 do
    begin
      if FTasks[i].Tags[j] = ATag then
      begin
        HasTag := True;
        Break;
      end;
    end;
    
    if HasTag then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.GetCompletedCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FTasks) - 1 do
  begin
    if FTasks[i].Status = tsCompleted then
      Inc(Result);
  end;
end;

function TTaskManager.GetPendingCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FTasks) - 1 do
  begin
    if (FTasks[i].Status <> tsCompleted) and (FTasks[i].Status <> tsCancelled) then
      Inc(Result);
  end;
end;

function TTaskManager.GetOverdueCount: Integer;
var
  i: Integer;
  CurrentDate: TDateTime;
begin
  Result := 0;
  CurrentDate := Now;
  
  for i := 0 to Length(FTasks) - 1 do
  begin
    if (FTasks[i].Status <> tsCompleted) and 
       (FTasks[i].Status <> tsCancelled) and 
       (FTasks[i].DueDate > 0) and 
       (FTasks[i].DueDate < CurrentDate) then
      Inc(Result);
  end;
end;

function TTaskManager.GetCompletionRate: Double;
var
  Total: Integer;
begin
  Total := Length(FTasks);
  if Total = 0 then
    Result := 0.0
  else
    Result := (GetCompletedCount / Total) * 100.0;
end;

procedure TTaskManager.ClearAllTasks;
var
  i: Integer;
begin
  for i := 0 to Length(FTasks) - 1 do
    SetLength(FTasks[i].Tags, 0);
  SetLength(FTasks, 0);
  FNextID := 1;
end;

function TTaskManager.TaskStatusToString(AStatus: TTaskStatus): string;
begin
  case AStatus of
    tsNotStarted: Result := 'Not Started';
    tsInProgress: Result := 'In Progress';
    tsCompleted: Result := 'Completed';
    tsCancelled: Result := 'Cancelled';
    tsOnHold: Result := 'On Hold';
  else
    Result := 'Unknown';
  end;
end;

function TTaskManager.TaskPriorityToString(APriority: TTaskPriority): string;
begin
  case APriority of
    tpLow: Result := 'Low';
    tpMedium: Result := 'Medium';
    tpHigh: Result := 'High';
    tpCritical: Result := 'Critical';
  else
    Result := 'Unknown';
  end;
end;

function TTaskManager.TaskToString(const ATask: TTask): string;
var
  i: Integer;
  TagsStr: string;
begin
  Result := Format('ID: %d | Title: %s | Status: %s | Priority: %s', 
    [ATask.ID, ATask.Title, TaskStatusToString(ATask.Status), 
     TaskPriorityToString(ATask.Priority)]);
  
  if ATask.Description <> '' then
    Result := Result + Format(' | Description: %s', [ATask.Description]);
  
  if ATask.DueDate > 0 then
    Result := Result + Format(' | Due: %s', [DateTimeToStr(ATask.DueDate)]);
  
  if Length(ATask.Tags) > 0 then
  begin
    TagsStr := '';
    for i := 0 to Length(ATask.Tags) - 1 do
    begin
      if i > 0 then
        TagsStr := TagsStr + ', ';
      TagsStr := TagsStr + ATask.Tags[i];
    end;
    Result := Result + Format(' | Tags: [%s]', [TagsStr]);
  end;
end;

end.
