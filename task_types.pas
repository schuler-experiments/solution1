
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
  end;

  TTaskArray = array of TTask;

  TTaskStats = record
    Total: Integer;
    Pending: Integer;
    InProgress: Integer;
    Completed: Integer;
    Overdue: Integer;
    Blocked: Integer; // New stat
  end;

  { TTaskManager }

  TTaskManager = class
  private
    FTasks: TTaskArray;
    FLastID: Integer;
    function HasTag(const Task: TTask; const Tag: String): Boolean;
    function TagsToString(const Tags: TTagArray): String;
    function StringToTags(const TagString: String): TTagArray;
    function DepsToString(const Deps: TDepArray): String;
    function StringToDeps(const DepString: String): TDepArray;
    function StatusToString(Status: TTaskStatus): String;
    function StringToStatus(const S: String): TTaskStatus;
    function PriorityToString(Priority: TTaskPriority): String;
    function StringToPriority(const S: String): TTaskPriority;
    function CheckCircularDependency(TaskID, DepID: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    function AddTask(const ATitle, ADescription: String; APriority: TTaskPriority = tpMedium; ADueDate: TDateTime = 0): Integer;
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

    // Persistence
    function SaveToFile(const Filename: String): Boolean;
    function LoadFromFile(const Filename: String): Boolean;
    procedure ClearTasks;
  end;

implementation

{ TTaskManager }

constructor TTaskManager.Create;
begin
  inherited Create;
  SetLength(FTasks, 0);
  FLastID := 0;
end;

destructor TTaskManager.Destroy;
begin
  SetLength(FTasks, 0);
  inherited Destroy;
end;

procedure TTaskManager.ClearTasks;
begin
  SetLength(FTasks, 0);
  FLastID := 0;
end;

function TTaskManager.AddTask(const ATitle, ADescription: String; APriority: TTaskPriority = tpMedium; ADueDate: TDateTime = 0): Integer;
var
  NewIndex: Integer;
begin
  Inc(FLastID);
  NewIndex := Length(FTasks);
  SetLength(FTasks, NewIndex + 1);
  
  FTasks[NewIndex].ID := FLastID;
  FTasks[NewIndex].Title := ATitle;
  FTasks[NewIndex].Description := ADescription;
  FTasks[NewIndex].Status := tsPending;
  FTasks[NewIndex].Priority := APriority;
  FTasks[NewIndex].CreatedAt := Now;
  FTasks[NewIndex].DueDate := ADueDate;
  SetLength(FTasks[NewIndex].Tags, 0);
  SetLength(FTasks[NewIndex].Dependencies, 0);
  
  Result := FLastID;
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
    raise Exception.Create('Index out of bounds');
end;

function TTaskManager.FindTaskByID(const ID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTasks) do
  begin
    if FTasks[i].ID = ID then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTaskManager.FindTasksByStatus(const Status: TTaskStatus): TTaskArray;
var
  i, Count: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FTasks) do
  begin
    if FTasks[i].Status = Status then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
  end;
end;

function TTaskManager.UpdateTaskStatus(const ID: Integer; NewStatus: TTaskStatus): Boolean;
var
  Index: Integer;
begin
  Index := FindTaskByID(ID);
  if Index <> -1 then
  begin
    // Check dependencies if trying to start or complete
    if (NewStatus <> tsPending) and not CanStart(ID) then
    begin
      // Allow moving to pending, but prevent progress if blocked? 
      // For now, we allow status change but CanStart returns false.
      // Ideally, we might want to block 'InProgress' if CanStart is false.
      // But let's keep it simple: CanStart is a query, UpdateTaskStatus just updates.
    end;
    
    FTasks[Index].Status := NewStatus;
    Result := True;
  end
  else
    Result := False;
end;

function TTaskManager.DeleteTask(const ID: Integer): Boolean;
var
  Index, i, j, k: Integer;
begin
  Index := FindTaskByID(ID);
  if Index <> -1 then
  begin
    // Remove this ID from other tasks' dependencies
    for i := 0 to High(FTasks) do
    begin
      for j := 0 to High(FTasks[i].Dependencies) do
      begin
        if FTasks[i].Dependencies[j] = ID then
        begin
          // Remove dependency
          for k := j to High(FTasks[i].Dependencies) - 1 do
            FTasks[i].Dependencies[k] := FTasks[i].Dependencies[k + 1];
          SetLength(FTasks[i].Dependencies, Length(FTasks[i].Dependencies) - 1);
          Break; // Assuming unique dependencies
        end;
      end;
    end;

    for i := Index to High(FTasks) - 1 do
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
  if Length(FTasks) < 2 then Exit;
  
  for i := 0 to High(FTasks) - 1 do
    for j := 0 to High(FTasks) - i - 1 do
    begin
      if FTasks[j].Priority < FTasks[j + 1].Priority then
      begin
        Temp := FTasks[j];
        FTasks[j] := FTasks[j + 1];
        FTasks[j + 1] := Temp;
      end;
    end;
end;

function TTaskManager.AddTagToTask(const ID: Integer; const Tag: String): Boolean;
var
  Index, TagIndex: Integer;
begin
  Index := FindTaskByID(ID);
  if Index <> -1 then
  begin
    if HasTag(FTasks[Index], Tag) then
    begin
      Result := True;
      Exit;
    end;

    TagIndex := Length(FTasks[Index].Tags);
    SetLength(FTasks[Index].Tags, TagIndex + 1);
    FTasks[Index].Tags[TagIndex] := Tag;
    Result := True;
  end
  else
    Result := False;
end;

function TTaskManager.HasTag(const Task: TTask; const Tag: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(Task.Tags) do
    if CompareText(Task.Tags[i], Tag) = 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function TTaskManager.FindTasksByTag(const Tag: String): TTaskArray;
var
  i, Count: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FTasks) do
  begin
    if HasTag(FTasks[i], Tag) then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
  end;
end;

function TTaskManager.GetOverdueTasks: TTaskArray;
var
  i, Count: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FTasks) do
  begin
    if (FTasks[i].DueDate <> 0) and (FTasks[i].DueDate < Now) and (FTasks[i].Status <> tsCompleted) then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
  end;
end;

function TTaskManager.SearchTasks(const Query: String): TTaskArray;
var
  i, Count: Integer;
  LowerCaseQuery: String;
begin
  Result := nil;
  SetLength(Result, 0);
  Count := 0;
  LowerCaseQuery := LowerCase(Query);
  
  for i := 0 to High(FTasks) do
  begin
    if (Pos(LowerCaseQuery, LowerCase(FTasks[i].Title)) > 0) or 
       (Pos(LowerCaseQuery, LowerCase(FTasks[i].Description)) > 0) then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
  end;
end;

function TTaskManager.CloneTask(const ID: Integer): Integer;
var
  Index, NewIndex, i: Integer;
begin
  Index := FindTaskByID(ID);
  if Index = -1 then Exit(-1);
  
  Inc(FLastID);
  NewIndex := Length(FTasks);
  SetLength(FTasks, NewIndex + 1);
  
  FTasks[NewIndex] := FTasks[Index];
  FTasks[NewIndex].ID := FLastID;
  FTasks[NewIndex].Title := FTasks[Index].Title + ' (Copy)';
  FTasks[NewIndex].CreatedAt := Now;
  
  SetLength(FTasks[NewIndex].Tags, Length(FTasks[Index].Tags));
  for i := 0 to High(FTasks[Index].Tags) do
    FTasks[NewIndex].Tags[i] := FTasks[Index].Tags[i];

  SetLength(FTasks[NewIndex].Dependencies, Length(FTasks[Index].Dependencies));
  for i := 0 to High(FTasks[Index].Dependencies) do
    FTasks[NewIndex].Dependencies[i] := FTasks[Index].Dependencies[i];
    
  Result := FLastID;
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
    
    if (FTasks[i].DueDate <> 0) and (FTasks[i].DueDate < Now) and (FTasks[i].Status <> tsCompleted) then
      Inc(Result.Overdue);
      
    if (FTasks[i].Status <> tsCompleted) and not CanStart(FTasks[i].ID) then
      Inc(Result.Blocked);
  end;
end;

function TTaskManager.CompleteTasksByTag(const Tag: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FTasks) do
  begin
    if (FTasks[i].Status <> tsCompleted) and HasTag(FTasks[i], Tag) then
    begin
      FTasks[i].Status := tsCompleted;
      Inc(Result);
    end;
  end;
end;

// Dependencies Implementation

function TTaskManager.CheckCircularDependency(TaskID, DepID: Integer): Boolean;
var
  DepIndex, i: Integer;
  DepTask: TTask;
begin
  // Simple DFS or recursive check could be expensive.
  // For now, check if DepID depends on TaskID (direct or 1-level indirect)
  // Real implementation should be more robust.
  if TaskID = DepID then Exit(True);
  
  DepIndex := FindTaskByID(DepID);
  if DepIndex = -1 then Exit(False);
  
  DepTask := FTasks[DepIndex];
  for i := 0 to High(DepTask.Dependencies) do
  begin
    if DepTask.Dependencies[i] = TaskID then Exit(True);
    // Recursive check could go here
    if CheckCircularDependency(TaskID, DepTask.Dependencies[i]) then Exit(True);
  end;
  Result := False;
end;

function TTaskManager.AddDependency(const TaskID, DepID: Integer): Boolean;
var
  Index, DepIndex, i: Integer;
begin
  if TaskID = DepID then Exit(False);
  
  Index := FindTaskByID(TaskID);
  DepIndex := FindTaskByID(DepID);
  
  if (Index = -1) or (DepIndex = -1) then Exit(False);
  
  // Check if already exists
  for i := 0 to High(FTasks[Index].Dependencies) do
    if FTasks[Index].Dependencies[i] = DepID then Exit(True);
    
  // Check circular
  if CheckCircularDependency(TaskID, DepID) then Exit(False);
  
  SetLength(FTasks[Index].Dependencies, Length(FTasks[Index].Dependencies) + 1);
  FTasks[Index].Dependencies[High(FTasks[Index].Dependencies)] := DepID;
  Result := True;
end;

function TTaskManager.CanStart(const TaskID: Integer): Boolean;
var
  Index, i, DepIndex: Integer;
begin
  Index := FindTaskByID(TaskID);
  if Index = -1 then Exit(False);
  
  Result := True;
  for i := 0 to High(FTasks[Index].Dependencies) do
  begin
    DepIndex := FindTaskByID(FTasks[Index].Dependencies[i]);
    if (DepIndex <> -1) and (FTasks[DepIndex].Status <> tsCompleted) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function TTaskManager.GetBlockedTasks: TTaskArray;
var
  i, Count: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FTasks) do
  begin
    if (FTasks[i].Status <> tsCompleted) and not CanStart(FTasks[i].ID) then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
  end;
end;

// Export

function TTaskManager.ExportToHTML(const Filename: String): Boolean;
var
  List: TStringList;
  i, j: Integer;
  RowClass, DepStr: String;
begin
  List := TStringList.Create;
  try
    List.Add('<html><head><style>');
    List.Add('body { font-family: sans-serif; }');
    List.Add('table { border-collapse: collapse; width: 100%; }');
    List.Add('th, td { border: 1px solid #ddd; padding: 8px; }');
    List.Add('th { background-color: #f2f2f2; }');
    List.Add('.completed { background-color: #e6ffe6; text-decoration: line-through; }');
    List.Add('.overdue { background-color: #ffe6e6; }');
    List.Add('.blocked { background-color: #fff2e6; }');
    List.Add('</style></head><body>');
    List.Add('<h1>Task List</h1>');
    List.Add('<table>');
    List.Add('<tr><th>ID</th><th>Title</th><th>Status</th><th>Priority</th><th>Due Date</th><th>Tags</th><th>Deps</th></tr>');
    
    for i := 0 to High(FTasks) do
    begin
      RowClass := '';
      if FTasks[i].Status = tsCompleted then RowClass := 'completed'
      else if (FTasks[i].DueDate <> 0) and (FTasks[i].DueDate < Now) then RowClass := 'overdue'
      else if not CanStart(FTasks[i].ID) then RowClass := 'blocked';
      
      DepStr := '';
      for j := 0 to High(FTasks[i].Dependencies) do
      begin
        if j > 0 then DepStr := DepStr + ', ';
        DepStr := DepStr + IntToStr(FTasks[i].Dependencies[j]);
      end;
      
      List.Add(Format('<tr class="%s">', [RowClass]));
      List.Add(Format('<td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>', [
        FTasks[i].ID,
        FTasks[i].Title,
        StatusToString(FTasks[i].Status),
        PriorityToString(FTasks[i].Priority),
        DateToStr(FTasks[i].DueDate),
        TagsToString(FTasks[i].Tags),
        DepStr
      ]));
      List.Add('</tr>');
    end;
    
    List.Add('</table></body></html>');
    List.SaveToFile(Filename);
    Result := True;
  except
    Result := False;
  end;
  List.Free;
end;

// Persistence Helpers

function TTaskManager.TagsToString(const Tags: TTagArray): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(Tags) do
  begin
    if i > 0 then Result := Result + ',';
    Result := Result + Tags[i];
  end;
end;

function TTaskManager.StringToTags(const TagString: String): TTagArray;
var
  List: TStringList;
  i: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  if TagString = '' then Exit;
  
  List := TStringList.Create;
  try
    List.Delimiter := ',';
    List.StrictDelimiter := True;
    List.DelimitedText := TagString;
    SetLength(Result, List.Count);
    for i := 0 to List.Count - 1 do
      Result[i] := List[i];
  finally
    List.Free;
  end;
end;

function TTaskManager.DepsToString(const Deps: TDepArray): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(Deps) do
  begin
    if i > 0 then Result := Result + ',';
    Result := Result + IntToStr(Deps[i]);
  end;
end;

function TTaskManager.StringToDeps(const DepString: String): TDepArray;
var
  List: TStringList;
  i: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  if DepString = '' then Exit;
  
  List := TStringList.Create;
  try
    List.Delimiter := ',';
    List.StrictDelimiter := True;
    List.DelimitedText := DepString;
    SetLength(Result, List.Count);
    for i := 0 to List.Count - 1 do
      Result[i] := StrToIntDef(List[i], 0);
  finally
    List.Free;
  end;
end;

function TTaskManager.StatusToString(Status: TTaskStatus): String;
begin
  WriteStr(Result, Status);
end;

function TTaskManager.StringToStatus(const S: String): TTaskStatus;
begin
  ReadStr(S, Result);
end;

function TTaskManager.PriorityToString(Priority: TTaskPriority): String;
begin
  WriteStr(Result, Priority);
end;

function TTaskManager.StringToPriority(const S: String): TTaskPriority;
begin
  ReadStr(S, Result);
end;

function TTaskManager.SaveToFile(const Filename: String): Boolean;
var
  List: TStringList;
  i: Integer;
  Line: String;
begin
  List := TStringList.Create;
  try
    for i := 0 to High(FTasks) do
    begin
      // Format: ID|Title|Description|Status|Priority|CreatedAt|DueDate|Tags|Dependencies
      Line := Format('%d|%s|%s|%s|%s|%f|%f|%s|%s', [
        FTasks[i].ID,
        FTasks[i].Title,
        FTasks[i].Description,
        StatusToString(FTasks[i].Status),
        PriorityToString(FTasks[i].Priority),
        FTasks[i].CreatedAt,
        FTasks[i].DueDate,
        TagsToString(FTasks[i].Tags),
        DepsToString(FTasks[i].Dependencies)
      ]);
      List.Add(Line);
    end;
    List.SaveToFile(Filename);
    Result := True;
  except
    Result := False;
  end;
  List.Free;
end;

function TTaskManager.LoadFromFile(const Filename: String): Boolean;
var
  List: TStringList;
  Parts: TStringList;
  i: Integer;
  Line: String;
  NewTask: TTask;
begin
  if not FileExists(Filename) then Exit(False);
  
  ClearTasks;
  List := TStringList.Create;
  Parts := TStringList.Create;
  Parts.Delimiter := '|';
  Parts.StrictDelimiter := True;
  
  try
    List.LoadFromFile(Filename);
    for i := 0 to List.Count - 1 do
    begin
      Line := List[i];
      Parts.DelimitedText := Line;
      if Parts.Count >= 8 then
      begin
        NewTask.ID := StrToIntDef(Parts[0], 0);
        NewTask.Title := Parts[1];
        NewTask.Description := Parts[2];
        NewTask.Status := StringToStatus(Parts[3]);
        NewTask.Priority := StringToPriority(Parts[4]);
        NewTask.CreatedAt := StrToFloatDef(Parts[5], 0);
        NewTask.DueDate := StrToFloatDef(Parts[6], 0);
        NewTask.Tags := StringToTags(Parts[7]);
        
        if Parts.Count >= 9 then
          NewTask.Dependencies := StringToDeps(Parts[8])
        else
          SetLength(NewTask.Dependencies, 0);
        
        // Add to array
        SetLength(FTasks, Length(FTasks) + 1);
        FTasks[High(FTasks)] := NewTask;
        
        // Update LastID
        if NewTask.ID > FLastID then FLastID := NewTask.ID;
      end;
    end;
    Result := True;
  except
    Result := False;
  end;
  
  List.Free;
  Parts.Free;
end;

end.
