
unit task_types;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, DateUtils, Math;

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
    DueDate: TDateTime;    // New: 0 means no due date
    Tags: TTagArray;       // New: Dynamic array of tags
  end;

  TTaskArray = array of TTask;

  { TTaskManager }

  TTaskManager = class
  private
    FTasks: TTaskArray;
    FLastID: Integer;
    function HasTag(const Task: TTask; const Tag: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    // Updated AddTask with DueDate
    function AddTask(const ATitle, ADescription: String; APriority: TTaskPriority = tpMedium; ADueDate: TDateTime = 0): Integer;
    function GetTaskCount: Integer;
    function GetTask(const Index: Integer): TTask;
    function FindTaskByID(const ID: Integer): Integer;
    function FindTasksByStatus(const Status: TTaskStatus): TTaskArray;
    function UpdateTaskStatus(const ID: Integer; NewStatus: TTaskStatus): Boolean;
    function DeleteTask(const ID: Integer): Boolean;
    procedure SortTasksByPriority;
    
    // New Methods
    function AddTagToTask(const ID: Integer; const Tag: String): Boolean;
    function FindTasksByTag(const Tag: String): TTaskArray;
    function GetOverdueTasks: TTaskArray;
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
  SetLength(FTasks[NewIndex].Tags, 0); // Initialize tags
  
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
  Result := nil; // Explicit initialization to silence warning
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

// New Methods Implementation

function TTaskManager.AddTagToTask(const ID: Integer; const Tag: String): Boolean;
var
  Index, TagIndex: Integer;
begin
  Index := FindTaskByID(ID);
  if Index <> -1 then
  begin
    // Check if tag already exists
    if HasTag(FTasks[Index], Tag) then
    begin
      Result := True; // Already exists, consider success
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
    if CompareText(Task.Tags[i], Tag) = 0 then // Case insensitive
    begin
      Result := True;
      Exit;
    end;
end;

function TTaskManager.FindTasksByTag(const Tag: String): TTaskArray;
var
  i, Count: Integer;
begin
  Result := nil; // Explicit initialization
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
  Result := nil; // Explicit initialization
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FTasks) do
  begin
    // Check if DueDate is set (not 0) and is before Now, and task is not completed
    if (FTasks[i].DueDate <> 0) and (FTasks[i].DueDate < Now) and (FTasks[i].Status <> tsCompleted) then
    begin
      Inc(Count);
      SetLength(Result, Count);
      Result[Count - 1] := FTasks[i];
    end;
  end;
end;

end.
