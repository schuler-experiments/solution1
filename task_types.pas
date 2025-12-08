
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

  TTask = record
    ID: Integer;
    Title: String;
    Description: String;
    Status: TTaskStatus;
    Priority: TTaskPriority;
    CreatedAt: TDateTime;
    DueDate: TDateTime;    // 0 means no due date
    Tags: TTagArray;       // Dynamic array of tags
  end;

  TTaskArray = array of TTask;

  { TTaskManager }

  TTaskManager = class
  private
    FTasks: TTaskArray;
    FLastID: Integer;
    function HasTag(const Task: TTask; const Tag: String): Boolean;
    function TagsToString(const Tags: TTagArray): String;
    function StringToTags(const TagString: String): TTagArray;
    function StatusToString(Status: TTaskStatus): String;
    function StringToStatus(const S: String): TTaskStatus;
    function PriorityToString(Priority: TTaskPriority): String;
    function StringToPriority(const S: String): TTaskPriority;
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
    
    // Persistence
    function SaveToFile(const Filename: String): Boolean;
    function LoadFromFile(const Filename: String): Boolean;
    procedure ClearTasks; // Helper for testing load
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
    FTasks[Index].Status := NewStatus;
    Result := True;
  end
  else
    Result := False;
end;

function TTaskManager.DeleteTask(const ID: Integer): Boolean;
var
  Index, i: Integer;
begin
  Index := FindTaskByID(ID);
  if Index <> -1 then
  begin
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

// Helper methods for persistence

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
      // Format: ID|Title|Description|Status|Priority|CreatedAt|DueDate|Tags
      Line := Format('%d|%s|%s|%s|%s|%f|%f|%s', [
        FTasks[i].ID,
        FTasks[i].Title,
        FTasks[i].Description,
        StatusToString(FTasks[i].Status),
        PriorityToString(FTasks[i].Priority),
        FTasks[i].CreatedAt,
        FTasks[i].DueDate,
        TagsToString(FTasks[i].Tags)
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
        
        // Add to array
        SetLength(FTasks, Length(FTasks) + 1);
        FTasks[High(FTasks)] := NewTask;
        
        // Update LastID to ensure uniqueness for new tasks
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
