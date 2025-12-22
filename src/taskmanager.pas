
{
  TaskManager - A Pascal Unit for Task Management

  This unit provides functionality for managing tasks in a hierarchical structure.
  It includes classes for tasks, task lists, and task managers, along with
  methods for creating, manipulating, and executing tasks.

  Features:
  - Hierarchical task management
  - Task execution with optional parameters
  - Task filtering and searching
  - Task scheduling (conceptual)
  - Resource allocation (conceptual)

  Usage:
  Include this unit in your project and use the TaskManager class
  to manage your tasks efficiently.

  Author: Beyond AI
  Version: 1.0
}

unit taskmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type
  { Forward declarations }
  TTask = class;
  TTaskList = class;
TTaskManager = class;

  { TTask class }
  TTask = class
  private
    FName: string;
    FDescription: string;
    FPriority: integer;
    FCompleted: boolean;
    FParent: TTask;
    FSubtasks: TTaskList;
    FExecuteProc: procedure of object;
  public
    constructor Create(const AName: string; const ADescription: string; APriority: integer = 0);
    destructor Destroy; override;
    procedure Execute;
    procedure AddSubtask(ATask: TTask);
    procedure RemoveSubtask(ATask: TTask);
    procedure SetExecuteProc(AProc: procedure of object);
    function HasSubtasks: boolean;
    function GetSubtasks: TTaskList;
    property Name: string read FName;
    property Description: string read FDescription;
    property Priority: integer read FPriority write FPriority;
    property Completed: boolean read FCompleted write FCompleted;
    property Parent: TTask read FParent;
    property Subtasks: TTaskList read GetSubtasks;
  end;

  { TTaskList class }
  TTaskList = class
  private
    FTasks: TList;
    FOwner: TTask;
  public
    constructor Create(AOwner: TTask = nil);
    destructor Destroy; override;
    procedure Add(ATask: TTask);
    procedure Remove(ATask: TTask);
    procedure Clear;
    function Count: integer;
    function GetTask(Index: integer): TTask;
    function GetTaskByName(const AName: string): TTask;
    function GetTaskByPriority(APriority: integer): TTask;
    procedure ExecuteAll;
    procedure ExecuteCompletedTasks;
    procedure ExecuteIncompleteTasks;
    procedure ExecuteHighPriorityTasks;
    procedure ExecuteLowPriorityTasks;
    property Tasks[Index: integer]: TTask read GetTask; default;
  end;

  { TTaskManager class }
  TTaskManager = class
  private
    FRootTasks: TTaskList;
    FCurrentTask: TTask;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTask(ATask: TTask);
    procedure RemoveTask(ATask: TTask);
    procedure Clear;
    function Count: integer;
    function GetTask(Index: integer): TTask;
    function GetTaskByName(const AName: string): TTask;
    function GetTaskByPriority(APriority: integer): TTask;
    procedure ExecuteAllTasks;
    procedure ExecuteTaskByName(const AName: string);
    procedure ExecuteTaskByPriority(APriority: integer);
    procedure ExecuteHighPriorityTasks;
    procedure ExecuteLowPriorityTasks;
    procedure ExecuteCompletedTasks;
    procedure ExecuteIncompleteTasks;
    function FindTaskByName(const AName: string): TTask;
    function FindTaskByDescription(const ADescription: string): TTask;
    function GetCompletedTasks: TTaskList;
    function GetIncompleteTasks: TTaskList;
    function GetHighPriorityTasks: TTaskList;
    function GetLowPriorityTasks: TTaskList;
    function GetTasksByPriority(APriority: integer): TTaskList;
    function GetLeafTasks: TTaskList;
    function GetRootTasks: TTaskList;
    procedure FilterTasksByPriority(ATaskList: TTaskList; APriority: integer);
    procedure FilterTasksByCompletion(ATaskList: TTaskList; ACompleted: boolean);
    procedure SortTasksByPriority(ATaskList: TTaskList);
    procedure SortTasksByName(ATaskList: TTaskList);
    function SearchTasks(const AKeyword: string): TTaskList;
    procedure ExecuteTaskRecursively(ATask: TTask);
    procedure ExecuteTaskList(ATaskList: TTaskList);
    procedure ExecuteTasksByFilter(ATaskList: TTaskList; AFilterProc: function(ATask: TTask): boolean);
    property CurrentTask: TTask read FCurrentTask write FCurrentTask;
    property RootTasks: TTaskList read GetRootTasks;
  end;

procedure self_test;

implementation

{ TTask implementation }

constructor TTask.Create(const AName: string; const ADescription: string; APriority: integer);
begin
  FName := AName;
  FDescription := ADescription;
  FPriority := APriority;
  FCompleted := False;
  FParent := nil;
  FSubtasks := TTaskList.Create(Self);
  FExecuteProc := nil;
end;

destructor TTask.Destroy;
begin
  if Assigned(FSubtasks) then
    FSubtasks.Free;
  inherited Destroy;
end;

procedure TTask.Execute;
begin
  if Assigned(FExecuteProc) then
    FExecuteProc
  else
    Writeln('Executing task: ', FName);
end;

procedure TTask.AddSubtask(ATask: TTask);
begin
  if Assigned(ATask) then
  begin
    ATask.FParent := Self;
    FSubtasks.Add(ATask);
  end;
end;

procedure TTask.RemoveSubtask(ATask: TTask);
begin
  if Assigned(ATask) then
  begin
    ATask.FParent := nil;
    FSubtasks.Remove(ATask);
  end;
end;

procedure TTask.SetExecuteProc(AProc: procedure of object);
begin
  FExecuteProc := AProc;
end;

function TTask.HasSubtasks: boolean;
begin
  Result := (Assigned(FSubtasks)) and (FSubtasks.Count > 0);
end;

function TTask.GetSubtasks: TTaskList;
begin
  Result := FSubtasks;
end;

{ TTaskList implementation }

constructor TTaskList.Create(AOwner: TTask);
begin
  FTasks := TList.Create;
  FOwner := AOwner;
end;

destructor TTaskList.Destroy;
begin
  Clear;
  FTasks.Free;
  inherited Destroy;
end;

procedure TTaskList.Add(ATask: TTask);
begin
  if Assigned(ATask) then
    FTasks.Add(ATask);
end;

procedure TTaskList.Remove(ATask: TTask);
begin
  if Assigned(ATask) then
    FTasks.Remove(ATask);
end;

procedure TTaskList.Clear;
var
  I: Integer;
begin
  for I := 0 to FTasks.Count - 1 do
    TTask(FTasks[I]).Free;
  FTasks.Clear;
end;

function TTaskList.Count: integer;
begin
  Result := FTasks.Count;
end;

function TTaskList.GetTask(Index: integer): TTask;
begin
  if (Index >= 0) and (Index < FTasks.Count) then
    Result := TTask(FTasks[Index])
  else
    Result := nil;
end;

function TTaskList.GetTaskByName(const AName: string): TTask;
var
  I: Integer;
begin
  for I := 0 to FTasks.Count - 1 do
  begin
    if SameText(TTask(FTasks[I]).Name, AName) then
    begin
      Result := TTask(FTasks[I]);
      Exit;
    end;
  end;
  Result := nil;
end;

function TTaskList.GetTaskByPriority(APriority: integer): TTask;
var
  I: Integer;
begin
  for I := 0 to FTasks.Count - 1 do
  begin
    if TTask(FTasks[I]).Priority = APriority then
    begin
      Result := TTask(FTasks[I]);
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TTaskList.ExecuteAll;
var
  I: Integer;
begin
  for I := 0 to FTasks.Count - 1 do
    TTask(FTasks[I]).Execute;
end;

procedure TTaskList.ExecuteCompletedTasks;
var
  I: Integer;
begin
  for I := 0 to FTasks.Count - 1 do
    if TTask(FTasks[I]).Completed then
      TTask(FTasks[I]).Execute;
end;

procedure TTaskList.ExecuteIncompleteTasks;
var
  I: Integer;
begin
  for I := 0 to FTasks.Count - 1 do
    if not TTask(FTasks[I]).Completed then
      TTask(FTasks[I]).Execute;
end;

procedure TTaskList.ExecuteHighPriorityTasks;
var
  I: Integer;
begin
  for I := 0 to FTasks.Count - 1 do
    if TTask(FTasks[I]).Priority > 0 then
      TTask(FTasks[I]).Execute;
end;

procedure TTaskList.ExecuteLowPriorityTasks;
var
  I: Integer;
begin
  for I := 0 to FTasks.Count - 1 do
    if TTask(FTasks[I]).Priority <= 0 then
      TTask(FTasks[I]).Execute;
end;

{ TTaskManager implementation }

constructor TTaskManager.Create;
begin
  FRootTasks := TTaskList.Create;
  FCurrentTask := nil;
end;

destructor TTaskManager.Destroy;
begin
  FRootTasks.Free;
  inherited Destroy;
end;

procedure TTaskManager.AddTask(ATask: TTask);
begin
  if Assigned(ATask) then
    FRootTasks.Add(ATask);
end;

procedure TTaskManager.RemoveTask(ATask: TTask);
begin
  if Assigned(ATask) then
    FRootTasks.Remove(ATask);
end;

procedure TTaskManager.Clear;
begin
  FRootTasks.Clear;
end;

function TTaskManager.Count: integer;
begin
  Result := FRootTasks.Count;
end;

function TTaskManager.GetTask(Index: integer): TTask;
begin
  Result := FRootTasks.GetTask(Index);
end;

function TTaskManager.GetTaskByName(const AName: string): TTask;
begin
  Result := FRootTasks.GetTaskByName(AName);
end;

function TTaskManager.GetTaskByPriority(APriority: integer): TTask;
begin
  Result := FRootTasks.GetTaskByPriority(APriority);
end;

procedure TTaskManager.ExecuteAllTasks;
begin
  FRootTasks.ExecuteAll;
end;

procedure TTaskManager.ExecuteTaskByName(const AName: string);
var
  Task: TTask;
begin
  Task := GetTaskByName(AName);
  if Assigned(Task) then
    Task.Execute;
end;

procedure TTaskManager.ExecuteTaskByPriority(APriority: integer);
var
  Task: TTask;
begin
  Task := GetTaskByPriority(APriority);
  if Assigned(Task) then
    Task.Execute;
end;

procedure TTaskManager.ExecuteHighPriorityTasks;
begin
  FRootTasks.ExecuteHighPriorityTasks;
end;

procedure TTaskManager.ExecuteLowPriorityTasks;
begin
  FRootTasks.ExecuteLowPriorityTasks;
end;

procedure TTaskManager.ExecuteCompletedTasks;
begin
  FRootTasks.ExecuteCompletedTasks;
end;

procedure TTaskManager.ExecuteIncompleteTasks;
begin
  FRootTasks.ExecuteIncompleteTasks;
end;

function TTaskManager.FindTaskByName(const AName: string): TTask;
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if SameText(Task.Name, AName) then
    begin
      Result := Task;
      Exit;
    end;
    if Task.HasSubtasks then
    begin
      Result := FindTaskByNameInList(Task.Subtasks, AName);
      if Assigned(Result) then
        Exit;
    end;
  end;
  Result := nil;
end;

function TTaskManager.FindTaskByNameInList(ATaskList: TTaskList; const AName: string): TTask;
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if SameText(Task.Name, AName) then
    begin
      Result := Task;
      Exit;
    end;
    if Task.HasSubtasks then
    begin
      Result := FindTaskByNameInList(Task.Subtasks, AName);
      if Assigned(Result) then
        Exit;
    end;
  end;
  Result := nil;
end;

function TTaskManager.FindTaskByDescription(const ADescription: string): TTask;
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if SameText(Task.Description, ADescription) then
    begin
      Result := Task;
      Exit;
    end;
    if Task.HasSubtasks then
    begin
      Result := FindTaskByDescriptionInList(Task.Subtasks, ADescription);
      if Assigned(Result) then
        Exit;
    end;
  end;
  Result := nil;
end;

function TTaskManager.FindTaskByDescriptionInList(ATaskList: TTaskList; const ADescription: string): TTask;
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if SameText(Task.Description, ADescription) then
    begin
      Result := Task;
      Exit;
    end;
    if Task.HasSubtasks then
    begin
      Result := FindTaskByDescriptionInList(Task.Subtasks, ADescription);
      if Assigned(Result) then
        Exit;
    end;
  end;
  Result := nil;
end;

function TTaskManager.GetCompletedTasks: TTaskList;
var
  I: Integer;
  Task: TTask;
  CompletedList: TTaskList;
begin
  CompletedList := TTaskList.Create;
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if Task.Completed then
      CompletedList.Add(Task);
    if Task.HasSubtasks then
      AddCompletedTasksToList(Task.Subtasks, CompletedList);
  end;
  Result := CompletedList;
end;

function TTaskManager.GetIncompleteTasks: TTaskList;
var
  I: Integer;
  Task: TTask;
  IncompleteList: TTaskList;
begin
  IncompleteList := TTaskList.Create;
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if not Task.Completed then
      IncompleteList.Add(Task);
    if Task.HasSubtasks then
      AddIncompleteTasksToList(Task.Subtasks, IncompleteList);
  end;
  Result := IncompleteList;
end;

function TTaskManager.GetHighPriorityTasks: TTaskList;
var
  I: Integer;
  Task: TTask;
  HighPriorityList: TTaskList;
begin
  HighPriorityList := TTaskList.Create;
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if Task.Priority > 0 then
      HighPriorityList.Add(Task);
    if Task.HasSubtasks then
      AddHighPriorityTasksToList(Task.Subtasks, HighPriorityList);
  end;
  Result := HighPriorityList;
end;

function TTaskManager.GetLowPriorityTasks: TTaskList;
var
  I: Integer;
  Task: TTask;
  LowPriorityList: TTaskList;
begin
  LowPriorityList := TTaskList.Create;
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if Task.Priority <= 0 then
      LowPriorityList.Add(Task);
    if Task.HasSubtasks then
      AddLowPriorityTasksToList(Task.Subtasks, LowPriorityList);
  end;
  Result := LowPriorityList;
end;

function TTaskManager.GetTasksByPriority(APriority: integer): TTaskList;
var
  I: Integer;
  Task: TTask;
  PriorityList: TTaskList;
begin
  PriorityList := TTaskList.Create;
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if Task.Priority = APriority then
      PriorityList.Add(Task);
    if Task.HasSubtasks then
      AddTasksByPriorityToList(Task.Subtasks, APriority, PriorityList);
  end;
  Result := PriorityList;
end;

function TTaskManager.GetLeafTasks: TTaskList;
var
  I: Integer;
  Task: TTask;
  LeafList: TTaskList;
begin
  LeafList := TTaskList.Create;
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if not Task.HasSubtasks then
      LeafList.Add(Task);
    if Task.HasSubtasks then
      AddLeafTasksToList(Task.Subtasks, LeafList);
  end;
  Result := LeafList;
end;

function TTaskManager.GetRootTasks: TTaskList;
begin
  Result := FRootTasks;
end;

procedure TTaskManager.FilterTasksByPriority(ATaskList: TTaskList; APriority: integer);
var
  I: Integer;
  Task: TTask;
begin
  ATaskList.Clear;
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if Task.Priority = APriority then
      ATaskList.Add(Task);
    if Task.HasSubtasks then
      AddTasksByPriorityToList(Task.Subtasks, APriority, ATaskList);
  end;
end;

procedure TTaskManager.FilterTasksByCompletion(ATaskList: TTaskList; ACompleted: boolean);
var
  I: Integer;
  Task: TTask;
begin
  ATaskList.Clear;
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if Task.Completed = ACompleted then
      ATaskList.Add(Task);
    if Task.HasSubtasks then
      AddTasksByCompletionToList(Task.Subtasks, ACompleted, ATaskList);
  end;
end;

procedure TTaskManager.SortTasksByPriority(ATaskList: TTaskList);
var
  I, J: Integer;
  TempTask: TTask;
begin
  for I := 0 to ATaskList.Count - 2 do
    for J := I + 1 to ATaskList.Count - 1 do
      if TTask(ATaskList.FTasks[I]).Priority < TTask(ATaskList.FTasks[J]).Priority then
      begin
        TempTask := TTask(ATaskList.FTasks[I]);
        ATaskList.FTasks[I] := ATaskList.FTasks[J];
        ATaskList.FTasks[J] := TempTask;
      end;
end;

procedure TTaskManager.SortTasksByName(ATaskList: TTaskList);
var
  I, J: Integer;
  TempTask: TTask;
begin
  for I := 0 to ATaskList.Count - 2 do
    for J := I + 1 to ATaskList.Count - 1 do
      if CompareText(TTask(ATaskList.FTasks[I]).Name, TTask(ATaskList.FTasks[J]).Name) > 0 then
      begin
        TempTask := TTask(ATaskList.FTasks[I]);
        ATaskList.FTasks[I] := ATaskList.FTasks[J];
        ATaskList.FTasks[J] := TempTask;
      end;
end;

function TTaskManager.SearchTasks(const AKeyword: string): TTaskList;
var
  I: Integer;
  Task: TTask;
  SearchList: TTaskList;
begin
  SearchList := TTaskList.Create;
  for I := 0 to FRootTasks.Count - 1 do
  begin
    Task := FRootTasks.GetTask(I);
    if (Pos(UpperCase(AKeyword), UpperCase(Task.Name)) > 0) or
       (Pos(UpperCase(AKeyword), UpperCase(Task.Description)) > 0) then
      SearchList.Add(Task);
    if Task.HasSubtasks then
      AddTasksByKeywordToList(Task.Subtasks, AKeyword, SearchList);
  end;
  Result := SearchList;
end;

procedure TTaskManager.ExecuteTaskRecursively(ATask: TTask);
begin
  if Assigned(ATask) then
  begin
    ATask.Execute;
    if ATask.HasSubtasks then
      ATask.Subtasks.ExecuteAll;
  end;
end;

procedure TTaskManager.ExecuteTaskList(ATaskList: TTaskList);
var
  I: Integer;
begin
  for I := 0 to ATaskList.Count - 1 do
    ATaskList.GetTask(I).Execute;
end;

procedure TTaskManager.ExecuteTasksByFilter(ATaskList: TTaskList; AFilterProc: function(ATask: TTask): boolean);
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if AFilterProc(Task) then
      Task.Execute;
  end;
end;

procedure TTaskManager.AddCompletedTasksToList(ATaskList: TTaskList; CompletedList: TTaskList);
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if Task.Completed then
      CompletedList.Add(Task);
    if Task.HasSubtasks then
      AddCompletedTasksToList(Task.Subtasks, CompletedList);
  end;
end;

procedure TTaskManager.AddIncompleteTasksToList(ATaskList: TTaskList; IncompleteList: TTaskList);
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if not Task.Completed then
      IncompleteList.Add(Task);
    if Task.HasSubtasks then
      AddIncompleteTasksToList(Task.Subtasks, IncompleteList);
  end;
end;

procedure TTaskManager.AddHighPriorityTasksToList(ATaskList: TTaskList; HighPriorityList: TTaskList);
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if Task.Priority > 0 then
      HighPriorityList.Add(Task);
    if Task.HasSubtasks then
      AddHighPriorityTasksToList(Task.Subtasks, HighPriorityList);
  end;
end;

procedure TTaskManager.AddLowPriorityTasksToList(ATaskList: TTaskList; LowPriorityList: TTaskList);
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if Task.Priority <= 0 then
      LowPriorityList.Add(Task);
    if Task.HasSubtasks then
      AddLowPriorityTasksToList(Task.Subtasks, LowPriorityList);
  end;
end;

procedure TTaskManager.AddTasksByPriorityToList(ATaskList: TTaskList; APriority: integer; PriorityList: TTaskList);
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if Task.Priority = APriority then
      PriorityList.Add(Task);
    if Task.HasSubtasks then
      AddTasksByPriorityToList(Task.Subtasks, APriority, PriorityList);
  end;
end;

procedure TTaskManager.AddLeafTasksToList(ATaskList: TTaskList; LeafList: TTaskList);
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if not Task.HasSubtasks then
      LeafList.Add(Task);
    if Task.HasSubtasks then
      AddLeafTasksToList(Task.Subtasks, LeafList);
  end;
end;

procedure TTaskManager.AddTasksByCompletionToList(ATaskList: TTaskList; ACompleted: boolean; CompletionList: TTaskList);
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if Task.Completed = ACompleted then
      CompletionList.Add(Task);
    if Task.HasSubtasks then
      AddTasksByCompletionToList(Task.Subtasks, ACompleted, CompletionList);
  end;
end;

procedure TTaskManager.AddTasksByKeywordToList(ATaskList: TTaskList; const AKeyword: string; KeywordList: TTaskList);
var
  I: Integer;
  Task: TTask;
begin
  for I := 0 to ATaskList.Count - 1 do
  begin
    Task := ATaskList.GetTask(I);
    if (Pos(UpperCase(AKeyword), UpperCase(Task.Name)) > 0) or
       (Pos(UpperCase(AKeyword), UpperCase(Task.Description)) > 0) then
      KeywordList.Add(Task);
    if Task.HasSubtasks then
      AddTasksByKeywordToList(Task.Subtasks, AKeyword, KeywordList);
  end;
end;

procedure self_test;
var
  Manager: TTaskManager;
  Task1, Task2, Task3, SubTask1: TTask;
  TaskList: TTaskList;
  FoundTask: TTask;
  i: Integer;

  function TestFilterProc(ATask: TTask): boolean;
  begin
    Result := ATask.Priority > 0;
  end;

begin
  Writeln('Starting TaskManager self-test...');
  
  // Create task manager
  Manager := TTaskManager.Create;
  
  try
    // Test 1: Basic task creation and management
    Writeln('Test 1: Basic task creation and management');
    Task1 := TTask.Create('Task 1', 'First test task', 5);
    Task2 := TTask.Create('Task 2', 'Second test task', 3);
    Task3 := TTask.Create('Task 3', 'Third test task', 1);
    
    Manager.AddTask(Task1);
    Manager.AddTask(Task2);
    Manager.AddTask(Task3);
    
    if Manager.Count = 3 then
      Writeln('  ✓ Task count correct')
    else
      Writeln('  ✗ Task count incorrect');
    
    // Test 2: Task retrieval
    Writeln('Test 2: Task retrieval');
    FoundTask := Manager.GetTaskByName('Task 2');
    if Assigned(FoundTask) and (FoundTask.Name = 'Task 2') then
      Writeln('  ✓ Task retrieval by name works')
    else
      Writeln('  ✗ Task retrieval by name failed');
    
    FoundTask := Manager.GetTaskByPriority(3);
    if Assigned(FoundTask) and (FoundTask.Priority = 3) then
      Writeln('  ✓ Task retrieval by priority works')
    else
      Writeln('  ✗ Task retrieval by priority failed');
    
    // Test 3: Task hierarchy
    Writeln('Test 3: Task hierarchy');
    SubTask1 := TTask.Create('SubTask 1', 'Sub task of Task 1', 4);
    Task1.AddSubtask(SubTask1);
    
    if Task1.HasSubtasks then
      Writeln('  ✓ Task hierarchy creation works')
    else
      Writeln('  ✗ Task hierarchy creation failed');
    
    // Test 4: Task execution
    Writeln('Test 4: Task execution');
    Writeln('  Executing all tasks:');
    Manager.ExecuteAllTasks;
    
    // Test 5: Task filtering
    Writeln('Test 5: Task filtering');
    TaskList := Manager.GetHighPriorityTasks;
    if TaskList.Count > 0 then
      Writeln('  ✓ High priority task filtering works')
    else
      Writeln('  ✗ High priority task filtering failed');
    
    TaskList := Manager.GetLowPriorityTasks;
    if TaskList.Count > 0 then
      Writeln('  ✓ Low priority task filtering works')
    else
      Writeln('  ✗ Low priority task filtering failed');
    
    // Test 6: Task searching
    Writeln('Test 6: Task searching');
    TaskList := Manager.SearchTasks('Task');
    if TaskList.Count = 3 then
      Writeln('  ✓ Task searching works')
    else
      Writeln('  ✗ Task searching failed');
    
    // Test 7: Task sorting
    Writeln('Test 7: Task sorting');
    Manager.SortTasksByPriority(Manager.RootTasks);
    if Manager.RootTasks.GetTask(0).Priority >= Manager.RootTasks.GetTask(1).Priority then
      Writeln('  ✓ Task sorting by priority works')
    else
      Writeln('  ✗ Task sorting by priority failed');
    
    Manager.SortTasksByName(Manager.RootTasks);
    if CompareText(Manager.RootTasks.GetTask(0).Name, Manager.RootTasks.GetTask(1).Name) <= 0 then
      Writeln('  ✓ Task sorting by name works')
    else
      Writeln('  ✗ Task sorting by name failed');
    
    // Test 8: Task execution with filter
    Writeln('Test 8: Task execution with filter');
    Writeln('  Executing tasks with custom filter (priority > 0):');
    Manager.ExecuteTasksByFilter(Manager.RootTasks, @TestFilterProc);
    
    // Test 9: Task completion tracking
    Writeln('Test 9: Task completion tracking');
    Task1.Completed := True;
    TaskList := Manager.GetCompletedTasks;
    if TaskList.Count = 1 then
      Writeln('  ✓ Task completion tracking works')
    else
      Writeln('  ✗ Task completion tracking failed');
    
    TaskList := Manager.GetIncompleteTasks;
    if TaskList.Count = 2 then
      Writeln('  ✓ Task incomplete tracking works')
    else
      Writeln('  ✗ Task incomplete tracking failed');
    
    // Test 10: Task removal
    Writeln('Test 10: Task removal');
    Manager.RemoveTask(Task2);
    if Manager.Count = 2 then
      Writeln('  ✓ Task removal works')
    else
      Writeln('  ✗ Task removal failed');
    
    Writeln('All tests completed!');
    
  finally
    Manager.Free;
  end;
end;

end.
