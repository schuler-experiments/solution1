
unit task_types;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, DateUtils, Math;

type
  TTaskStatus = (tsPending, tsInProgress, tsCompleted);

  TTask = record
    ID: Integer;
    Title: String;
    Description: String;
    Status: TTaskStatus;
    CreatedAt: TDateTime;
  end;

  TTaskArray = array of TTask;

  { TTaskManager }

  TTaskManager = class
  private
    FTasks: TTaskArray;
    FLastID: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddTask(const ATitle, ADescription: String): Integer;
    function GetTaskCount: Integer;
    function GetTask(const Index: Integer): TTask;
    function FindTaskByID(const ID: Integer): Integer;
    function FindTasksByStatus(const Status: TTaskStatus): TTaskArray;
    
    // New methods for Cycle 3
    function UpdateTaskStatus(const ID: Integer; NewStatus: TTaskStatus): Boolean;
    function DeleteTask(const ID: Integer): Boolean;
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

function TTaskManager.AddTask(const ATitle, ADescription: String): Integer;
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
  FTasks[NewIndex].CreatedAt := Now;
  
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
  SetLength(Result, 0); // Initialize to silence warning
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
    // Shift elements down
    for i := Index to High(FTasks) - 1 do
      FTasks[i] := FTasks[i + 1];
    
    // Reduce size
    SetLength(FTasks, Length(FTasks) - 1);
    Result := True;
  end
  else
    Result := False;
end;

end.
