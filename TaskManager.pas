
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
  result := -1;
  for i := 0 to Length(tasks) - 1 do
  begin
    if tasks[i].id = id then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskManagerClass.AddTask(title, description: string; 
                                   priority: TTaskPriority; 
                                   dueDate: TDateTime): integer;
var
  newTask: TTask;
  len: integer;
begin
  newTask := CreateTask(nextId, title, description, priority, dueDate);
  len := Length(tasks);
  SetLength(tasks, len + 1);
  tasks[len] := newTask;
  result := nextId;
  Inc(nextId);
end;

function TTaskManagerClass.GetTask(id: integer): TTask;
var
  idx: integer;
begin
  idx := FindTaskIndex(id);
  if idx >= 0 then
    result := tasks[idx]
  else
  begin
    result.id := 0;
    result.title := '';
    result.description := '';
  end;
end;

procedure TTaskManagerClass.UpdateTask(id: integer; newTask: TTask);
var
  idx: integer;
begin
  idx := FindTaskIndex(id);
  if idx >= 0 then
    tasks[idx] := newTask;
end;

procedure TTaskManagerClass.DeleteTask(id: integer);
var
  idx, i: integer;
begin
  idx := FindTaskIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(tasks) - 2 do
      tasks[i] := tasks[i + 1];
    SetLength(tasks, Length(tasks) - 1);
  end;
end;

function TTaskManagerClass.GetTaskCount(): integer;
begin
  result := Length(tasks);
end;

function TTaskManagerClass.GetAllTasks(): TTaskArray;
begin
  result := Copy(tasks, 0, Length(tasks));
end;

function TTaskManagerClass.GetTasksByStatus(status: TTaskStatus): TTaskArray;
var
  i, count: integer;
begin
  count := 0;
  SetLength(result, 0);
  for i := 0 to Length(tasks) - 1 do
  begin
    if tasks[i].status = status then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      Inc(count);
    end;
  end;
end;

function TTaskManagerClass.GetTasksByPriority(priority: TTaskPriority): TTaskArray;
var
  i, count: integer;
begin
  count := 0;
  SetLength(result, 0);
  for i := 0 to Length(tasks) - 1 do
  begin
    if tasks[i].priority = priority then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      Inc(count);
    end;
  end;
end;

function TTaskManagerClass.GetOverdueTasks(): TTaskArray;
var
  i, count: integer;
  currentTime: TDateTime;
begin
  count := 0;
  SetLength(result, 0);
  currentTime := Now();
  for i := 0 to Length(tasks) - 1 do
  begin
    if (tasks[i].status <> tsCompleted) and (tasks[i].dueDate > 0) and 
       (tasks[i].dueDate < currentTime) then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      Inc(count);
    end;
  end;
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
    tasks[idx].completedDate := Now();
  end;
end;

procedure TTaskManagerClass.SortByPriority();
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to Length(tasks) - 2 do
  begin
    for j := i + 1 to Length(tasks) - 1 do
    begin
      if tasks[j].priority > tasks[i].priority then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
  end;
end;

procedure TTaskManagerClass.SortByDueDate();
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to Length(tasks) - 2 do
  begin
    for j := i + 1 to Length(tasks) - 1 do
    begin
      if (tasks[j].dueDate > 0) and (tasks[j].dueDate < tasks[i].dueDate) then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
  end;
end;

procedure TTaskManagerClass.SortByTitle();
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to Length(tasks) - 2 do
  begin
    for j := i + 1 to Length(tasks) - 1 do
    begin
      if tasks[j].title < tasks[i].title then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
  end;
end;

procedure TTaskManagerClass.SelfTest();
var
  mgr: TTaskManagerClass;
  id1, id2, id3, id4: integer;
  task: TTask;
  allTasks: TTaskArray;
  highPriorityTasks: TTaskArray;
  completedTasks: TTaskArray;
  i: integer;
  tomorrow: TDateTime;
  yesterday: TDateTime;
begin
  WriteLn('=== Task Manager Self Test ===');
  
  mgr := TTaskManagerClass.Create();
  
  try
    { Create some test tasks }
    tomorrow := IncDay(Now(), 1);
    yesterday := IncDay(Now(), -1);
    
    WriteLn('Adding tasks...');
    id1 := mgr.AddTask('Design Database Schema', 
                       'Create ERD and normalize tables', tpHigh, tomorrow);
    id2 := mgr.AddTask('Code API Endpoints', 
                       'Implement REST API with authentication', tpHigh, IncDay(tomorrow, 1));
    id3 := mgr.AddTask('Write Unit Tests', 
                       'Achieve 80% code coverage', tpMedium, IncDay(tomorrow, 2));
    id4 := mgr.AddTask('Fix Documentation', 
                       'Update API docs', tpLow, IncDay(tomorrow, 3));
    
    WriteLn(Format('Total tasks added: %d', [mgr.GetTaskCount()]));
    
    { Test status updates }
    WriteLn('Updating task statuses...');
    mgr.SetTaskStatus(id1, tsInProgress);
    mgr.CompleteTask(id4);
    
    task := mgr.GetTask(id1);
    WriteLn(Format('Task %d status: %s', [id1, StatusToString(task.status)]));
    
    { Test filtering }
    WriteLn('Getting high priority tasks...');
    highPriorityTasks := mgr.GetTasksByPriority(tpHigh);
    WriteLn(Format('High priority tasks: %d', [Length(highPriorityTasks)]));
    
    WriteLn('Getting completed tasks...');
    completedTasks := mgr.GetTasksByStatus(tsCompleted);
    WriteLn(Format('Completed tasks: %d', [Length(completedTasks)]));
    
    { Test sorting }
    WriteLn('Sorting by priority...');
    mgr.SortByPriority();
    allTasks := mgr.GetAllTasks();
    WriteLn('Task order after sorting by priority:');
    for i := 0 to Length(allTasks) - 1 do
      WriteLn(Format('  %d. %s (Priority: %s)', 
                     [allTasks[i].id, allTasks[i].title, 
                      PriorityToString(allTasks[i].priority)]));
    
    { Test deletion }
    WriteLn('Deleting task...');
    mgr.DeleteTask(id3);
    WriteLn(Format('Total tasks after deletion: %d', [mgr.GetTaskCount()]));
    
    WriteLn('=== Self Test Complete ===');
    
  finally
    mgr.Free();
  end;
end;

end.
