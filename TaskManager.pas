
unit TaskManager;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, TaskTypes;

type
  TTaskManagerClass = class
  private
    tasks: TTaskArray;
    nextId: integer;
    function FindTaskIndex(id: integer): integer;
  public
    constructor Create();
    destructor Destroy(); override;
    
    { Core CRUD operations }
    function AddTask(title, description: string; priority: TTaskPriority; 
                     dueDate: TDateTime): integer;
    function GetTask(id: integer): TTask;
    procedure UpdateTask(id: integer; newTask: TTask);
    procedure DeleteTask(id: integer);
    
    { Query operations }
    function GetTaskCount(): integer;
    function GetAllTasks(): TTaskArray;
    function GetTasksByStatus(status: TTaskStatus): TTaskArray;
    function GetTasksByPriority(priority: TTaskPriority): TTaskArray;
    function GetOverdueTasks(): TTaskArray;
    
    { Status operations }
    procedure SetTaskStatus(id: integer; newStatus: TTaskStatus);
    procedure CompleteTask(id: integer);
    
    { Sorting operations }
    procedure SortByPriority();
    procedure SortByDueDate();
    procedure SortByTitle();
    
    { Task duplication and templating }
    function DuplicateTask(id: integer): integer;
    function DuplicateTaskWithNewDate(id: integer; newDueDate: TDateTime): integer;
    function GetTaskAsTemplate(id: integer): TTask;
    function CreateFromTemplate(template: TTask; title: string): integer;
    
    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskManagerClass.Create();
begin
  inherited Create();
  SetLength(tasks, 0);
  nextId := 1;
end;

destructor TTaskManagerClass.Destroy();
begin
  SetLength(tasks, 0);
  inherited Destroy();
end;

function TTaskManagerClass.FindTaskIndex(id: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to length(tasks) - 1 do
  begin
    if tasks[i].id = id then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTaskManagerClass.AddTask(title, description: string; priority: TTaskPriority;
                                    dueDate: TDateTime): integer;
var
  idx: integer;
  newTask: TTask;
begin
  newTask := CreateTask(nextId, title, description, priority, dueDate);
  
  idx := length(tasks);
  SetLength(tasks, idx + 1);
  tasks[idx] := newTask;
  
  Result := nextId;
  inc(nextId);
end;

function TTaskManagerClass.GetTask(id: integer): TTask;
var
  idx: integer;
begin
  idx := FindTaskIndex(id);
  if idx >= 0 then
    Result := tasks[idx]
  else
  begin
    Result.id := -1;
    Result.title := '';
  end;
end;

procedure TTaskManagerClass.UpdateTask(id: integer; newTask: TTask);
var
  idx: integer;
begin
  idx := FindTaskIndex(id);
  if idx >= 0 then
  begin
    newTask.id := id;
    tasks[idx] := newTask;
  end;
end;

procedure TTaskManagerClass.DeleteTask(id: integer);
var
  idx, i: integer;
begin
  idx := FindTaskIndex(id);
  if idx < 0 then
    Exit;
  
  for i := idx to length(tasks) - 2 do
    tasks[i] := tasks[i + 1];
  SetLength(tasks, length(tasks) - 1);
end;

function TTaskManagerClass.GetTaskCount(): integer;
begin
  Result := length(tasks);
end;

function TTaskManagerClass.GetAllTasks(): TTaskArray;
begin
  Result := tasks;
end;

function TTaskManagerClass.GetTasksByStatus(status: TTaskStatus): TTaskArray;
var
  i, count: integer;
  arr: TTaskArray;
begin
  count := 0;
  SetLength(arr, 0);
  for i := 0 to length(tasks) - 1 do
  begin
    if tasks[i].status = status then
    begin
      SetLength(arr, count + 1);
      arr[count] := tasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

function TTaskManagerClass.GetTasksByPriority(priority: TTaskPriority): TTaskArray;
var
  i, count: integer;
  arr: TTaskArray;
begin
  count := 0;
  SetLength(arr, 0);
  for i := 0 to length(tasks) - 1 do
  begin
    if tasks[i].priority = priority then
    begin
      SetLength(arr, count + 1);
      arr[count] := tasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

function TTaskManagerClass.GetOverdueTasks(): TTaskArray;
var
  i, count: integer;
  arr: TTaskArray;
  currentTime: TDateTime;
begin
  currentTime := Now;
  count := 0;
  SetLength(arr, 0);
  for i := 0 to length(tasks) - 1 do
  begin
    if (tasks[i].dueDate < currentTime) and (tasks[i].status <> tsCompleted) then
    begin
      SetLength(arr, count + 1);
      arr[count] := tasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

procedure TTaskManagerClass.SetTaskStatus(id: integer; newStatus: TTaskStatus);
var
  idx: integer;
begin
  idx := FindTaskIndex(id);
  if idx >= 0 then
    tasks[idx].status := newStatus;
end;

procedure TTaskManagerClass.CompleteTask(id: integer);
var
  idx: integer;
begin
  idx := FindTaskIndex(id);
  if idx >= 0 then
  begin
    tasks[idx].status := tsCompleted;
    tasks[idx].completedDate := Now;
  end;
end;

procedure TTaskManagerClass.SortByPriority();
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to length(tasks) - 2 do
    for j := i + 1 to length(tasks) - 1 do
    begin
      if ord(tasks[j].priority) > ord(tasks[i].priority) then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
end;

procedure TTaskManagerClass.SortByDueDate();
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to length(tasks) - 2 do
    for j := i + 1 to length(tasks) - 1 do
    begin
      if tasks[j].dueDate < tasks[i].dueDate then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
end;

procedure TTaskManagerClass.SortByTitle();
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to length(tasks) - 2 do
    for j := i + 1 to length(tasks) - 1 do
    begin
      if tasks[j].title < tasks[i].title then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
end;

function TTaskManagerClass.DuplicateTask(id: integer): integer;
var
  idx: integer;
  sourceTask: TTask;
begin
  idx := FindTaskIndex(id);
  if idx < 0 then
  begin
    Result := -1;
    Exit;
  end;
  
  sourceTask := tasks[idx];
  sourceTask.status := tsNotStarted;
  sourceTask.completedDate := 0;
  sourceTask.createdDate := Now;
  
  Result := AddTask(sourceTask.title, sourceTask.description, 
                    sourceTask.priority, sourceTask.dueDate);
end;

function TTaskManagerClass.DuplicateTaskWithNewDate(id: integer; newDueDate: TDateTime): integer;
var
  idx: integer;
  sourceTask: TTask;
begin
  idx := FindTaskIndex(id);
  if idx < 0 then
  begin
    Result := -1;
    Exit;
  end;
  
  sourceTask := tasks[idx];
  sourceTask.status := tsNotStarted;
  sourceTask.completedDate := 0;
  sourceTask.createdDate := Now;
  
  Result := AddTask(sourceTask.title, sourceTask.description, 
                    sourceTask.priority, newDueDate);
end;

function TTaskManagerClass.GetTaskAsTemplate(id: integer): TTask;
var
  idx: integer;
begin
  idx := FindTaskIndex(id);
  if idx >= 0 then
    Result := tasks[idx]
  else
  begin
    Result.id := -1;
    Result.title := '';
  end;
end;

function TTaskManagerClass.CreateFromTemplate(template: TTask; title: string): integer;
begin
  Result := AddTask(title, template.description, template.priority, template.dueDate);
end;

procedure TTaskManagerClass.SelfTest();
var
  id1, id2, dup1, dup2: integer;
  task: TTask;
  allTasks: TTaskArray;
begin
  WriteLn('=== Task Manager Self Test ===');
  
  WriteLn('Adding tasks...');
  id1 := AddTask('Design Database Schema', 'Create database design document', tpHigh, Now + 5);
  id2 := AddTask('Code API Endpoints', 'Implement REST API', tpHigh, Now + 10);
  AddTask('Write Unit Tests', 'Create comprehensive unit tests', tpMedium, Now + 15);
  AddTask('Fix Documentation', 'Update API documentation', tpLow, Now + 20);
  WriteLn('Total tasks added: ', GetTaskCount());
  
  WriteLn('Testing task duplication...');
  dup1 := DuplicateTask(id1);
  WriteLn('Duplicated task 1 as task ', dup1);
  
  WriteLn('Testing duplication with new date...');
  dup2 := DuplicateTaskWithNewDate(id2, Now + 30);
  WriteLn('Duplicated task 2 with new date as task ', dup2);
  
  WriteLn('Total tasks after duplication: ', GetTaskCount());
  
  WriteLn('Testing task templates...');
  task := GetTaskAsTemplate(id1);
  WriteLn('Task template retrieved, title: ', task.title);
  
  WriteLn('Creating task from template...');
  AddTask(task.title + ' - Copy', task.description, task.priority, Now + 25);
  WriteLn('Total tasks: ', GetTaskCount());
  
  WriteLn('Updating task statuses...');
  SetTaskStatus(id1, tsInProgress);
  WriteLn('Task 1 status: ', StatusToString(GetTask(id1).status));
  
  WriteLn('Getting high priority tasks...');
  allTasks := GetTasksByPriority(tpHigh);
  WriteLn('High priority tasks: ', length(allTasks));
  
  WriteLn('=== Self Test Complete ===');
  WriteLn('');
end;

end.
