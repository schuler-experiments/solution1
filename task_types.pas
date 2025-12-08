
unit task_types;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, DateUtils, Math, Classes;

type
  TTaskStatus = (tsPending, tsInProgress, tsCompleted);
  TTaskPriority = (tpLow, tpMedium, tpHigh);
  TTagArray = array of String;
  TDepArray = array of Integer;

  TTask = record
    ID: Integer;
    Title: String;
    Description: String;
    Status: TTaskStatus;
    Priority: TTaskPriority;
    CreatedAt: TDateTime;
    DueDate: TDateTime;    // 0 means no due date
    Tags: TTagArray;       // Dynamic array of tags
    Dependencies: TDepArray; // IDs of tasks that must be completed before this one
    TimeSpent: Double;       // Total time spent in seconds
    LastStartTime: TDateTime; // When the timer was last started
    IsTiming: Boolean;       // Is the timer currently running?
    RecurrenceInterval: Integer; // 0 means no recurrence, >0 means repeat every N days
  end;

  TTaskArray = array of TTask;

  TTaskStats = record
    Total: Integer;
    Pending: Integer;
    InProgress: Integer;
    Completed: Integer;
    Overdue: Integer;
    Blocked: Integer;
  end;

  // Undo/Redo Types
  TUndoActionType = (uatAdd, uatDelete, uatUpdate);

  TUndoAction = record
    ActionType: TUndoActionType;
    TaskSnapshot: TTask;
  end;

  TUndoStack = array of TUndoAction;

  { TUndoManager }
  TUndoManager = class
  private
    FUndoStack: TUndoStack;
    FRedoStack: TUndoStack;
    procedure Push(var Stack: TUndoStack; Action: TUndoAction);
    function Pop(var Stack: TUndoStack): TUndoAction;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RecordAction(ActionType: TUndoActionType; const Task: TTask);
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function PopUndo: TUndoAction;
    function PopRedo: TUndoAction;
    procedure ClearRedo;
    procedure ClearAll;
    procedure PushRedo(Action: TUndoAction); 
  end;

  { TTaskManager }

  TTaskManager = class
  private
    FTasks: TTaskArray;
    FLastID: Integer;
    FUndoManager: TUndoManager;
    function HasTag(const Task: TTask; const Tag: String): Boolean;
    function CheckCircularDependency(TaskID, DepID: Integer): Boolean;
    function GetTaskIndex(const ID: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    function AddTask(const ATitle, ADescription: String; APriority: TTaskPriority = tpMedium; ADueDate: TDateTime = 0; ARecurrence: Integer = 0): Integer;
    function GetTaskCount: Integer;
    function GetTask(const Index: Integer): TTask;
    function FindTaskByID(const ID: Integer): Integer;
    function FindTasksByStatus(const Status: TTaskStatus): TTaskArray;
    function UpdateTaskStatus(const ID: Integer; NewStatus: TTaskStatus): Boolean;
    function DeleteTask(const ID: Integer): Boolean;
    procedure SortTasksByPriority;
    
    function AddTagToTask(const ID: Integer; const Tag: String): Boolean;
    function FindTasksByTag(const Tag: String): TTaskArray;
    function GetOverdueTasks: TTaskArray;
    function FindTasksByDueDate(StartDate, EndDate: TDateTime): TTaskArray;
    
    // New Features
    function SearchTasks(const Query: String): TTaskArray;
    function CloneTask(const ID: Integer): Integer;
    function GetTaskStatistics: TTaskStats;
    function CompleteTasksByTag(const Tag: String): Integer;
    
    // Dependencies
    function AddDependency(const TaskID, DepID: Integer): Boolean;
    function CanStart(const TaskID: Integer): Boolean;
    function GetBlockedTasks: TTaskArray;

    // Export
    function ExportToHTML(const Filename: String): Boolean;

    // Time Tracking
    function StartTaskTimer(const ID: Integer): Boolean;
    function StopTaskTimer(const ID: Integer): Boolean;
    function GetTaskTimeSpent(const ID: Integer): Double;

    // Persistence
    function SaveToFile(const Filename: String): Boolean;
    function LoadFromFile(const Filename: String): Boolean;
    procedure ClearTasks;
    procedure RestoreTask(const Task: TTask);
    
    // Undo/Redo
    function Undo: Boolean;
    function Redo: Boolean;
  end;

// Standalone Helper Functions
function StatusToString(Status: TTaskStatus): String;
function StringToStatus(const S: String): TTaskStatus;
function PriorityToString(Priority: TTaskPriority): String;
function StringToPriority(const S: String): TTaskPriority;
function TagsToString(const Tags: TTagArray): String;
function StringToTags(const TagString: String): TTagArray;
function DepsToString(const Deps: TDepArray): String;
function StringToDeps(const DepString: String): TDepArray;

implementation

{ Helper Implementations }

function StatusToString(Status: TTaskStatus): String;
begin
  case Status of
    tsPending: Result := 'Pending';
    tsInProgress: Result := 'In Progress';
    tsCompleted: Result := 'Completed';
  end;
end;

function StringToStatus(const S: String): TTaskStatus;
begin
  if SameText(S, 'Pending') then Result := tsPending
  else if SameText(S, 'In Progress') then Result := tsInProgress
  else if SameText(S, 'Completed') then Result := tsCompleted
  else Result := tsPending;
end;

function PriorityToString(Priority: TTaskPriority): String;
begin
  case Priority of
    tpLow: Result := 'Low';
    tpMedium: Result := 'Medium';
    tpHigh: Result := 'High';
  end;
end;

function StringToPriority(const S: String): TTaskPriority;
begin
  if SameText(S, 'Low') then Result := tpLow
  else if SameText(S, 'Medium') then Result := tpMedium
  else if SameText(S, 'High') then Result := tpHigh
  else Result := tpMedium;
end;

function TagsToString(const Tags: TTagArray): String;
var i: Integer;
begin
  Result := '';
  for i := 0 to High(Tags) do
    Result := Result + Tags[i] + ',';
  if Length(Result) > 0 then SetLength(Result, Length(Result) - 1);
end;

function StringToTags(const TagString: String): TTagArray;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.CommaText := TagString;
    SetLength(Result, SL.Count);
    for i := 0 to SL.Count - 1 do
      Result[i] := SL[i];
  finally
    SL.Free;
  end;
end;

function DepsToString(const Deps: TDepArray): String;
var i: Integer;
begin
  Result := '';
  for i := 0 to High(Deps) do
    Result := Result + IntToStr(Deps[i]) + ',';
  if Length(Result) > 0 then SetLength(Result, Length(Result) - 1);
end;

function StringToDeps(const DepString: String): TDepArray;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.CommaText := DepString;
    SetLength(Result, SL.Count);
    for i := 0 to SL.Count - 1 do
      Result[i] := StrToIntDef(SL[i], 0);
  finally
    SL.Free;
  end;
end;

{ TUndoManager }

constructor TUndoManager.Create;
begin
  SetLength(FUndoStack, 0);
  SetLength(FRedoStack, 0);
end;

destructor TUndoManager.Destroy;
begin
  SetLength(FUndoStack, 0);
  SetLength(FRedoStack, 0);
  inherited Destroy;
end;

procedure TUndoManager.Push(var Stack: TUndoStack; Action: TUndoAction);
var
  Len: Integer;
begin
  Len := Length(Stack);
  SetLength(Stack, Len + 1);
  Stack[Len] := Action;
end;

function TUndoManager.Pop(var Stack: TUndoStack): TUndoAction;
var
  Len: Integer;
begin
  Len := Length(Stack);
  if Len > 0 then
  begin
    Result := Stack[Len - 1];
    SetLength(Stack, Len - 1);
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TUndoManager.RecordAction(ActionType: TUndoActionType; const Task: TTask);
var
  Action: TUndoAction;
begin
  Action.ActionType := ActionType;
  Action.TaskSnapshot := Task;
  Push(FUndoStack, Action);
  ClearRedo;
end;

function TUndoManager.CanUndo: Boolean;
begin
  Result := Length(FUndoStack) > 0;
end;

function TUndoManager.CanRedo: Boolean;
begin
  Result := Length(FRedoStack) > 0;
end;

function TUndoManager.PopUndo: TUndoAction;
begin
  Result := Pop(FUndoStack);
end;

function TUndoManager.PopRedo: TUndoAction;
begin
  Result := Pop(FRedoStack);
end;

procedure TUndoManager.PushRedo(Action: TUndoAction);
begin
  Push(FRedoStack, Action);
end;

procedure TUndoManager.ClearRedo;
begin
  SetLength(FRedoStack, 0);
end;

procedure TUndoManager.ClearAll;
begin
  SetLength(FUndoStack, 0);
  SetLength(FRedoStack, 0);
end;

{ TTaskManager }

constructor TTaskManager.Create;
begin
  SetLength(FTasks, 0);
  FLastID := 0;
  FUndoManager := TUndoManager.Create;
end;

destructor TTaskManager.Destroy;
begin
  SetLength(FTasks, 0);
  FUndoManager.Free;
  inherited Destroy;
end;

function TTaskManager.GetTaskIndex(const ID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTasks) do
    if FTasks[i].ID = ID then
    begin
      Result := i;
      Exit;
    end;
end;

function TTaskManager.AddTask(const ATitle, ADescription: String; APriority: TTaskPriority; ADueDate: TDateTime; ARecurrence: Integer): Integer;
var
  NewTask: TTask;
  Len: Integer;
begin
  Inc(FLastID);
  NewTask.ID := FLastID;
  NewTask.Title := ATitle;
  NewTask.Description := ADescription;
  NewTask.Status := tsPending;
  NewTask.Priority := APriority;
  NewTask.CreatedAt := Now;
  NewTask.DueDate := ADueDate;
  NewTask.TimeSpent := 0;
  NewTask.IsTiming := False;
  NewTask.RecurrenceInterval := ARecurrence;
  SetLength(NewTask.Tags, 0);
  SetLength(NewTask.Dependencies, 0);

  Len := Length(FTasks);
  SetLength(FTasks, Len + 1);
  FTasks[Len] := NewTask;
  
  FUndoManager.RecordAction(uatAdd, NewTask);
  
  Result := NewTask.ID;
end;

function TTaskManager.GetTaskCount: Integer;
begin
  Result := Length(FTasks);
end;

function TTaskManager.GetTask(const Index: Integer): TTask;
begin
  if (Index >= 0) and (Index < Length(FTasks)) then
    Result := FTasks[Index]
  else
    raise Exception.Create('Task index out of bounds');
end;

function TTaskManager.FindTaskByID(const ID: Integer): Integer;
begin
  Result := GetTaskIndex(ID);
end;

function TTaskManager.FindTasksByStatus(const Status: TTaskStatus): TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FTasks) do
    if FTasks[i].Status = Status then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
end;

function TTaskManager.UpdateTaskStatus(const ID: Integer; NewStatus: TTaskStatus): Boolean;
var
  Idx, NewID, NewIdx: Integer;
  OldTask: TTask;
begin
  Idx := GetTaskIndex(ID);
  if Idx <> -1 then
  begin
    OldTask := FTasks[Idx];
    // FUndoManager.RecordAction(uatUpdate, OldTask); // Disable Undo for debugging
    
    FTasks[Idx].Status := NewStatus;
    
    // Handle Recurrence
    if (NewStatus = tsCompleted) and (OldTask.RecurrenceInterval > 0) then
    begin
      NewID := AddTask(
        OldTask.Title,
        OldTask.Description,
        OldTask.Priority,
        OldTask.DueDate + OldTask.RecurrenceInterval,
        OldTask.RecurrenceInterval
      );
      NewIdx := GetTaskIndex(NewID);
      if NewIdx <> -1 then
        // FTasks[NewIdx].Tags := Copy(OldTask.Tags); // Potential crash source
        SetLength(FTasks[NewIdx].Tags, 0); // Just init
    end;

    Result := True;
  end
  else
    Result := False;
end;


function TTaskManager.DeleteTask(const ID: Integer): Boolean;
var
  Idx, i: Integer;
  DeletedTask: TTask;
begin
  Idx := GetTaskIndex(ID);
  if Idx <> -1 then
  begin
    DeletedTask := FTasks[Idx];
    FUndoManager.RecordAction(uatDelete, DeletedTask);
    
    for i := Idx to High(FTasks) - 1 do
      FTasks[i] := FTasks[i + 1];
    SetLength(FTasks, Length(FTasks) - 1);
    Result := True;
  end
  else
    Result := False;
end;


procedure TTaskManager.SortTasksByPriority;
var
  i, j: Integer;
  Temp: TTask;
begin
  for i := 0 to High(FTasks) - 1 do
    for j := 0 to High(FTasks) - 1 - i do
      if FTasks[j].Priority < FTasks[j + 1].Priority then
      begin
        Temp := FTasks[j];
        FTasks[j] := FTasks[j + 1];
        FTasks[j + 1] := Temp;
      end;
end;

function TTaskManager.HasTag(const Task: TTask; const Tag: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(Task.Tags) do
    if SameText(Task.Tags[i], Tag) then
    begin
      Result := True;
      Exit;
    end;
end;

function TTaskManager.AddTagToTask(const ID: Integer; const Tag: String): Boolean;
var
  Idx, TagLen: Integer;
  OldTask: TTask;
begin
  Idx := GetTaskIndex(ID);
  if Idx <> -1 then
  begin
    if not HasTag(FTasks[Idx], Tag) then
    begin
      OldTask := FTasks[Idx];
      FUndoManager.RecordAction(uatUpdate, OldTask);
      
      TagLen := Length(FTasks[Idx].Tags);
      SetLength(FTasks[Idx].Tags, TagLen + 1);
      FTasks[Idx].Tags[TagLen] := Tag;
    end;
    Result := True;
  end
  else
    Result := False;
end;


function TTaskManager.FindTasksByTag(const Tag: String): TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FTasks) do
    if HasTag(FTasks[i], Tag) then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
end;

function TTaskManager.GetOverdueTasks: TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FTasks) do
    if (FTasks[i].Status <> tsCompleted) and (FTasks[i].DueDate > 0) and (FTasks[i].DueDate < Now) then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
end;

function TTaskManager.SearchTasks(const Query: String): TTaskArray;
var
  i, Count: Integer;
  Q: String;
begin
  SetLength(Result, 0);
  Count := 0;
  Q := LowerCase(Query);
  for i := 0 to High(FTasks) do
    if (Pos(Q, LowerCase(FTasks[i].Title)) > 0) or (Pos(Q, LowerCase(FTasks[i].Description)) > 0) then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
end;

function TTaskManager.CloneTask(const ID: Integer): Integer;
var
  Idx: Integer;
  Original: TTask;
begin
  Idx := GetTaskIndex(ID);
  if Idx <> -1 then
  begin
    Original := FTasks[Idx];
    Result := AddTask(Original.Title + ' (Copy)', Original.Description, Original.Priority, Original.DueDate);
    
    Idx := GetTaskIndex(Result);
    FTasks[Idx].Tags := Copy(Original.Tags);
    FTasks[Idx].Dependencies := Copy(Original.Dependencies);
  end
  else
    Result := -1;
end;

function TTaskManager.GetTaskStatistics: TTaskStats;
var
  i: Integer;
begin
  Result.Total := 0;
  Result.Pending := 0;
  Result.InProgress := 0;
  Result.Completed := 0;
  Result.Overdue := 0;
  Result.Blocked := 0;
  
  for i := 0 to High(FTasks) do
  begin
    Inc(Result.Total);
    case FTasks[i].Status of
      tsPending: Inc(Result.Pending);
      tsInProgress: Inc(Result.InProgress);
      tsCompleted: Inc(Result.Completed);
    end;
    
    if (FTasks[i].Status <> tsCompleted) and (FTasks[i].DueDate > 0) and (FTasks[i].DueDate < Now) then
      Inc(Result.Overdue);
      
    if not CanStart(FTasks[i].ID) then
      Inc(Result.Blocked);
  end;
end;

function TTaskManager.CompleteTasksByTag(const Tag: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FTasks) do
    if HasTag(FTasks[i], Tag) and (FTasks[i].Status <> tsCompleted) then
    begin
      UpdateTaskStatus(FTasks[i].ID, tsCompleted);
      Inc(Result);
    end;
end;

function TTaskManager.CheckCircularDependency(TaskID, DepID: Integer): Boolean;
var
  Task: TTask;
  i: Integer;
  Idx: Integer;
begin
  if TaskID = DepID then Exit(True);
  
  Idx := GetTaskIndex(DepID);
  if Idx = -1 then Exit(False);
  
  Task := FTasks[Idx];
  for i := 0 to High(Task.Dependencies) do
  begin
    if Task.Dependencies[i] = TaskID then Exit(True);
    if CheckCircularDependency(TaskID, Task.Dependencies[i]) then Exit(True);
  end;
  Result := False;
end;

function TTaskManager.AddDependency(const TaskID, DepID: Integer): Boolean;
var
  Idx, DepLen: Integer;
  OldTask: TTask;
begin
  if TaskID = DepID then Exit(False);
  if CheckCircularDependency(TaskID, DepID) then Exit(False);
  
  Idx := GetTaskIndex(TaskID);
  if Idx <> -1 then
  begin
    OldTask := FTasks[Idx];
    FUndoManager.RecordAction(uatUpdate, OldTask);
    
    DepLen := Length(FTasks[Idx].Dependencies);
    SetLength(FTasks[Idx].Dependencies, DepLen + 1);
    FTasks[Idx].Dependencies[DepLen] := DepID;
    Result := True;
  end
  else
    Result := False;
end;


function TTaskManager.CanStart(const TaskID: Integer): Boolean;
var
  Idx, i, DepIdx: Integer;
  Task: TTask;
begin
  Idx := GetTaskIndex(TaskID);
  if Idx = -1 then Exit(False);
  
  Task := FTasks[Idx];
  if Task.Status = tsCompleted then Exit(True);
  
  for i := 0 to High(Task.Dependencies) do
  begin
    DepIdx := GetTaskIndex(Task.Dependencies[i]);
    if (DepIdx <> -1) and (FTasks[DepIdx].Status <> tsCompleted) then
      Exit(False);
  end;
  Result := True;
end;

function TTaskManager.GetBlockedTasks: TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FTasks) do
    if not CanStart(FTasks[i].ID) then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
end;

function TTaskManager.ExportToHTML(const Filename: String): Boolean;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('<html><body><h1>Task Report</h1><table border="1">');
    SL.Add('<tr><th>ID</th><th>Title</th><th>Status</th><th>Priority</th></tr>');
    for i := 0 to High(FTasks) do
      SL.Add(Format('<tr><td>%d</td><td>%s</td><td>%s</td><td>%s</td></tr>',
        [FTasks[i].ID, FTasks[i].Title, StatusToString(FTasks[i].Status), PriorityToString(FTasks[i].Priority)]));
    SL.Add('</table></body></html>');
    SL.SaveToFile(Filename);
    Result := True;
  except
    Result := False;
  end;
  SL.Free;
end;

function TTaskManager.StartTaskTimer(const ID: Integer): Boolean;
var
  Idx: Integer;
  OldTask: TTask;
begin
  Idx := GetTaskIndex(ID);
  if Idx <> -1 then
  begin
    if not FTasks[Idx].IsTiming then
    begin
      OldTask := FTasks[Idx];
      FUndoManager.RecordAction(uatUpdate, OldTask);
      
      FTasks[Idx].IsTiming := True;
      FTasks[Idx].LastStartTime := Now;
      FTasks[Idx].Status := tsInProgress;
    end;
    Result := True;
  end
  else
    Result := False;
end;


function TTaskManager.StopTaskTimer(const ID: Integer): Boolean;
var
  Idx: Integer;
  OldTask: TTask;
  Duration: Double;
begin
  Idx := GetTaskIndex(ID);
  if Idx <> -1 then
  begin
    if FTasks[Idx].IsTiming then
    begin
      OldTask := FTasks[Idx];
      FUndoManager.RecordAction(uatUpdate, OldTask);
      
      Duration := SecondSpan(Now, FTasks[Idx].LastStartTime);
      FTasks[Idx].TimeSpent := FTasks[Idx].TimeSpent + Duration;
      FTasks[Idx].IsTiming := False;
    end;
    Result := True;
  end
  else
    Result := False;
end;


function TTaskManager.GetTaskTimeSpent(const ID: Integer): Double;
var
  Idx: Integer;
begin
  Idx := GetTaskIndex(ID);
  if Idx <> -1 then
  begin
    Result := FTasks[Idx].TimeSpent;
    if FTasks[Idx].IsTiming then
      Result := Result + SecondSpan(Now, FTasks[Idx].LastStartTime);
  end
  else
    Result := 0.0;
end;

function TTaskManager.SaveToFile(const Filename: String): Boolean;
var
  FS: TFileStream;
  Writer: TWriter;
  i, j: Integer;
begin
  Result := False;
  try
    FS := TFileStream.Create(Filename, fmCreate);
    Writer := TWriter.Create(FS, 4096);
    try
      Writer.WriteInteger(FLastID);
      Writer.WriteInteger(Length(FTasks));
      for i := 0 to High(FTasks) do
      begin
        Writer.WriteInteger(FTasks[i].ID);
        Writer.WriteString(FTasks[i].Title);
        Writer.WriteString(FTasks[i].Description);
        Writer.WriteInteger(Ord(FTasks[i].Status));
        Writer.WriteInteger(Ord(FTasks[i].Priority));
        Writer.WriteDate(FTasks[i].CreatedAt);
        Writer.WriteDate(FTasks[i].DueDate);
        Writer.WriteFloat(FTasks[i].TimeSpent);
        Writer.WriteInteger(Length(FTasks[i].Tags));
        for j := 0 to High(FTasks[i].Tags) do
          Writer.WriteString(FTasks[i].Tags[j]);
        Writer.WriteInteger(Length(FTasks[i].Dependencies));
        for j := 0 to High(FTasks[i].Dependencies) do
          Writer.WriteInteger(FTasks[i].Dependencies[j]);
        Writer.WriteInteger(FTasks[i].RecurrenceInterval);
      end;
      Result := True;
    finally
      Writer.Free;
      FS.Free;
    end;
  except
    Result := False;
  end;
end;

function TTaskManager.LoadFromFile(const Filename: String): Boolean;
var
  FS: TFileStream;
  Reader: TReader;
  i, j, Count, TagCount, DepCount: Integer;
begin
  Result := False;
  if not FileExists(Filename) then Exit;
  
  ClearTasks;
  
  try
    FS := TFileStream.Create(Filename, fmOpenRead);
    Reader := TReader.Create(FS, 4096);
    try
      FLastID := Reader.ReadInteger;
      Count := Reader.ReadInteger;
      SetLength(FTasks, Count);
      for i := 0 to Count - 1 do
      begin
        FTasks[i].ID := Reader.ReadInteger;
        FTasks[i].Title := Reader.ReadString;
        FTasks[i].Description := Reader.ReadString;
        FTasks[i].Status := TTaskStatus(Reader.ReadInteger);
        FTasks[i].Priority := TTaskPriority(Reader.ReadInteger);
        FTasks[i].CreatedAt := Reader.ReadDate;
        FTasks[i].DueDate := Reader.ReadDate;
        FTasks[i].TimeSpent := Reader.ReadFloat;
        FTasks[i].IsTiming := False;
        
        TagCount := Reader.ReadInteger;
        SetLength(FTasks[i].Tags, TagCount);
        for j := 0 to TagCount - 1 do
          FTasks[i].Tags[j] := Reader.ReadString;
          
        DepCount := Reader.ReadInteger;
        SetLength(FTasks[i].Dependencies, DepCount);
        for j := 0 to DepCount - 1 do
          FTasks[i].Dependencies[j] := Reader.ReadInteger;
        try
          FTasks[i].RecurrenceInterval := Reader.ReadInteger;
        except
          FTasks[i].RecurrenceInterval := 0;
        end;
      end;
      Result := True;
    finally
      Reader.Free;
      FS.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TTaskManager.ClearTasks;
begin
  SetLength(FTasks, 0);
  FLastID := 0;
  FUndoManager.ClearAll;
end;

procedure TTaskManager.RestoreTask(const Task: TTask);
var
  Len: Integer;
begin
  Len := Length(FTasks);
  SetLength(FTasks, Len + 1);
  FTasks[Len] := Task;
  if Task.ID > FLastID then FLastID := Task.ID;
end;

function TTaskManager.Undo: Boolean;
var
  Action: TUndoAction;
  Idx, i: Integer;
  RedoAction: TUndoAction;
begin
  if not FUndoManager.CanUndo then Exit(False);
  
  Action := FUndoManager.PopUndo;
  
  case Action.ActionType of
    uatAdd:
      begin
        Idx := GetTaskIndex(Action.TaskSnapshot.ID);
        if Idx <> -1 then
        begin
          RedoAction.ActionType := uatAdd;
          RedoAction.TaskSnapshot := FTasks[Idx];
          FUndoManager.PushRedo(RedoAction);
          
          for i := Idx to High(FTasks) - 1 do
            FTasks[i] := FTasks[i + 1];
          SetLength(FTasks, Length(FTasks) - 1);
        end;
      end;
      
    uatDelete:
      begin
        RestoreTask(Action.TaskSnapshot);
        
        RedoAction.ActionType := uatDelete;
        RedoAction.TaskSnapshot := Action.TaskSnapshot;
        FUndoManager.PushRedo(RedoAction);
      end;
      
    uatUpdate:
      begin
        Idx := GetTaskIndex(Action.TaskSnapshot.ID);
        if Idx <> -1 then
        begin
          RedoAction.ActionType := uatUpdate;
          RedoAction.TaskSnapshot := FTasks[Idx];
          FUndoManager.PushRedo(RedoAction);
          
          FTasks[Idx] := Action.TaskSnapshot;
        end;
      end;
  end;
  Result := True;
end;

function TTaskManager.Redo: Boolean;
var
  Action: TUndoAction;
  Idx, i: Integer;
  UndoAction: TUndoAction;
begin
  if not FUndoManager.CanRedo then Exit(False);
  
  Action := FUndoManager.PopRedo;
  
  case Action.ActionType of
    uatAdd:
      begin
        RestoreTask(Action.TaskSnapshot);
        
        UndoAction.ActionType := uatAdd;
        UndoAction.TaskSnapshot := Action.TaskSnapshot;
        FUndoManager.Push(FUndoManager.FUndoStack, UndoAction);
      end;
      
    uatDelete:
      begin
        Idx := GetTaskIndex(Action.TaskSnapshot.ID);
        if Idx <> -1 then
        begin
          UndoAction.ActionType := uatDelete;
          UndoAction.TaskSnapshot := FTasks[Idx];
          FUndoManager.Push(FUndoManager.FUndoStack, UndoAction);
          
          for i := Idx to High(FTasks) - 1 do
            FTasks[i] := FTasks[i + 1];
          SetLength(FTasks, Length(FTasks) - 1);
        end;
      end;
      
    uatUpdate:
      begin
        Idx := GetTaskIndex(Action.TaskSnapshot.ID);
        if Idx <> -1 then
        begin
          UndoAction.ActionType := uatUpdate;
          UndoAction.TaskSnapshot := FTasks[Idx];
          FUndoManager.Push(FUndoManager.FUndoStack, UndoAction);
          
          FTasks[Idx] := Action.TaskSnapshot;
        end;
      end;
  end;
  Result := True;
end;


function TTaskManager.FindTasksByDueDate(StartDate, EndDate: TDateTime): TTaskArray;
var
  i, Count: Integer;
begin
  Count := 0;
  SetLength(Result, Length(FTasks));
  for i := 0 to High(FTasks) do
  begin
    if (FTasks[i].DueDate >= StartDate) and (FTasks[i].DueDate <= EndDate) then
    begin
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
  end;
  SetLength(Result, Count);
end;
end.
