
unit TaskManager;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes;

type
  TTaskManager = class
  private
    tasks: TTaskArray;
    nextTaskId: integer;
    lastError: string;

    function findTaskIndex(id: integer): integer;

  public
    constructor Create;
    destructor Destroy; override;

    // Core CRUD operations
    function addTask(const name, description, category: string;
                     priority: TTaskPriority; dueDate: TDateTime): integer;
    function getTask(id: integer; var task: TTask): boolean;
    function updateTask(var task: TTask): boolean;
    function deleteTask(id: integer): boolean;
    function getTaskCount: integer;

    // Task retrieval
    function getAllTasks: TTaskArray;
    function getTasksByStatus(status: TTaskStatus): TTaskArray;
    function getTasksByPriority(priority: TTaskPriority): TTaskArray;
    function getTasksByCategory(const category: string): TTaskArray;

    // Task operations
    function setTaskStatus(id: integer; newStatus: TTaskStatus): boolean;
    function setTaskPriority(id: integer; newPriority: TTaskPriority): boolean;
    function completeTask(id: integer): boolean;

    // Statistics
    function getStatistics: TTaskStats;
    function getOverdueTaskCount: integer;

    // Error handling
    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskManager.Create;
begin
  inherited Create;
  setLength(tasks, 0);
  nextTaskId := 1;
  lastError := '';
end;

destructor TTaskManager.Destroy;
begin
  setLength(tasks, 0);
  inherited Destroy;
end;

function TTaskManager.findTaskIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to length(tasks) - 1 do
    if tasks[i].id = id then
      begin
        result := i;
        exit;
      end;
end;

function TTaskManager.addTask(const name, description, category: string;
                               priority: TTaskPriority; dueDate: TDateTime): integer;
var
  newTask: TTask;
begin
  clearError;

  if length(name) = 0 then
    begin
      lastError := 'Task name cannot be empty';
      result := -1;
      exit;
    end;

  newTask.id := nextTaskId;
  inc(nextTaskId);
  newTask.name := name;
  newTask.description := description;
  newTask.category := category;
  newTask.status := tsNew;
  newTask.priority := priority;
  newTask.dueDate := dueDate;
  newTask.createdDate := now;
  newTask.completedDate := 0;

  setLength(tasks, length(tasks) + 1);
  tasks[length(tasks) - 1] := newTask;

  result := newTask.id;
end;

function TTaskManager.getTask(id: integer; var task: TTask): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      task := tasks[idx];
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
    end;
end;

function TTaskManager.updateTask(var task: TTask): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(task.id);
  if idx >= 0 then
    begin
      tasks[idx] := task;
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(task.id);
      result := false;
    end;
end;

function TTaskManager.deleteTask(id: integer): boolean;
var
  idx, i: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      for i := idx to length(tasks) - 2 do
        tasks[i] := tasks[i + 1];
      setLength(tasks, length(tasks) - 1);
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
    end;
end;

function TTaskManager.getTaskCount: integer;
begin
  result := length(tasks);
end;

function TTaskManager.getAllTasks: TTaskArray;
begin
  result := copy(tasks, 0, length(tasks));
end;

function TTaskManager.getTasksByStatus(status: TTaskStatus): TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to length(tasks) - 1 do
    if tasks[i].status = status then
      begin
        setLength(result, count + 1);
        result[count] := tasks[i];
        inc(count);
      end;
end;

function TTaskManager.getTasksByPriority(priority: TTaskPriority): TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to length(tasks) - 1 do
    if tasks[i].priority = priority then
      begin
        setLength(result, count + 1);
        result[count] := tasks[i];
        inc(count);
      end;
end;

function TTaskManager.getTasksByCategory(const category: string): TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to length(tasks) - 1 do
    if tasks[i].category = category then
      begin
        setLength(result, count + 1);
        result[count] := tasks[i];
        inc(count);
      end;
end;

function TTaskManager.setTaskStatus(id: integer; newStatus: TTaskStatus): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      tasks[idx].status := newStatus;
      if newStatus = tsCompleted then
        tasks[idx].completedDate := now;
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
    end;
end;

function TTaskManager.setTaskPriority(id: integer; newPriority: TTaskPriority): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      tasks[idx].priority := newPriority;
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
    end;
end;

function TTaskManager.completeTask(id: integer): boolean;
begin
  result := setTaskStatus(id, tsCompleted);
end;

function TTaskManager.getStatistics: TTaskStats;
var
  i: integer;
begin
  fillChar(result, sizeof(result), 0);
  result.totalTasks := length(tasks);

  for i := 0 to length(tasks) - 1 do
    begin
      case tasks[i].status of
        tsNew: inc(result.newTasks);
        tsInProgress: inc(result.inProgressTasks);
        tsBlocked: inc(result.blockedTasks);
        tsCompleted: inc(result.completedTasks);
        tsCancelled: inc(result.cancelledTasks);
      end;

      case tasks[i].priority of
        tpHigh: inc(result.highPriorityCount);
        tpCritical: inc(result.criticalPriorityCount);
        else;
      end;
    end;
end;

function TTaskManager.getOverdueTaskCount: integer;
var
  i, count: integer;
  currentTime: TDateTime;
begin
  count := 0;
  currentTime := now;
  for i := 0 to length(tasks) - 1 do
    if (tasks[i].dueDate < currentTime) and
       (tasks[i].status <> tsCompleted) and
       (tasks[i].status <> tsCancelled) then
      inc(count);
  result := count;
end;

function TTaskManager.getLastError: string;
begin
  result := lastError;
end;

procedure TTaskManager.clearError;
begin
  lastError := '';
end;

end.
