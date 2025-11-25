
unit TaskManagerSubtasks;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager,
  TaskManagerExtended,
  TaskManagerAdvanced;

type
  TTaskManagerSubtasks = class(TTaskManagerAdvanced)
  public
    function addSubtask(taskId: integer; const title: string; estimatedHours: double): integer;
    function removeSubtask(taskId: integer; subtaskIndex: integer): boolean;
    function updateSubtask(taskId: integer; subtaskIndex: integer; var subtask: TSubtask): boolean;
    function getSubtasks(taskId: integer): TSubtaskArray;
    function getSubtaskCount(taskId: integer): integer;
    function completeSubtask(taskId: integer; subtaskIndex: integer): boolean;
    function assignSubtaskTo(taskId: integer; subtaskIndex: integer; const assignee: string): boolean;
    function getSubtasksForAssignee(const assignee: string): TSubtaskArray;
    function getSubtaskCompletionPercentage(taskId: integer): double;
    function areAllSubtasksComplete(taskId: integer): boolean;
    function getSubtasksProgress: TTaskStats;
  end;

implementation

function TTaskManagerSubtasks.addSubtask(taskId: integer; const title: string; estimatedHours: double): integer;
var
  idx: integer;
  task: TTask;
  newSubtask: TSubtask;
begin
  result := -1;
  if not getTask(taskId, task) then
    exit;

  idx := length(task.subtasks);
  if idx >= maxSubtasksPerTask then
  begin
    lastError := 'Maximum subtasks per task exceeded';
    exit;
  end;

  setlength(task.subtasks, idx + 1);
  newSubtask.id := taskId * 1000 + idx + 1;
  newSubtask.taskId := taskId;
  newSubtask.title := title;
  newSubtask.status := tsNew;
  newSubtask.createdDate := now;
  newSubtask.completedDate := 0;
  newSubtask.assignedTo := '';
  newSubtask.estimatedHours := estimatedHours;
  newSubtask.actualHours := 0;

  task.subtasks[idx] := newSubtask;
  if updateTask(task) then
    result := newSubtask.id
  else
    setlength(task.subtasks, idx);
end;

function TTaskManagerSubtasks.removeSubtask(taskId: integer; subtaskIndex: integer): boolean;
var
  task: TTask;
  i: integer;
begin
  result := false;
  if not getTask(taskId, task) then
    exit;

  if (subtaskIndex < 0) or (subtaskIndex >= length(task.subtasks)) then
  begin
    lastError := 'Invalid subtask index';
    exit;
  end;

  for i := subtaskIndex to length(task.subtasks) - 2 do
    task.subtasks[i] := task.subtasks[i + 1];

  setlength(task.subtasks, length(task.subtasks) - 1);
  result := updateTask(task);
end;

function TTaskManagerSubtasks.updateSubtask(taskId: integer; subtaskIndex: integer; var subtask: TSubtask): boolean;
var
  task: TTask;
begin
  result := false;
  if not getTask(taskId, task) then
    exit;

  if (subtaskIndex < 0) or (subtaskIndex >= length(task.subtasks)) then
  begin
    lastError := 'Invalid subtask index';
    exit;
  end;

  task.subtasks[subtaskIndex] := subtask;
  result := updateTask(task);
end;

function TTaskManagerSubtasks.getSubtasks(taskId: integer): TSubtaskArray;
var
  task: TTask;
begin
  setlength(result, 0);
  if getTask(taskId, task) then
    result := task.subtasks;
end;

function TTaskManagerSubtasks.getSubtaskCount(taskId: integer): integer;
var
  task: TTask;
begin
  result := 0;
  if getTask(taskId, task) then
    result := length(task.subtasks);
end;

function TTaskManagerSubtasks.completeSubtask(taskId: integer; subtaskIndex: integer): boolean;
var
  task: TTask;
begin
  result := false;
  if not getTask(taskId, task) then
    exit;

  if (subtaskIndex < 0) or (subtaskIndex >= length(task.subtasks)) then
  begin
    lastError := 'Invalid subtask index';
    exit;
  end;

  task.subtasks[subtaskIndex].status := tsCompleted;
  task.subtasks[subtaskIndex].completedDate := now;
  result := updateTask(task);
end;

function TTaskManagerSubtasks.assignSubtaskTo(taskId: integer; subtaskIndex: integer; const assignee: string): boolean;
var
  task: TTask;
begin
  result := false;
  if not getTask(taskId, task) then
    exit;

  if (subtaskIndex < 0) or (subtaskIndex >= length(task.subtasks)) then
  begin
    lastError := 'Invalid subtask index';
    exit;
  end;

  task.subtasks[subtaskIndex].assignedTo := assignee;
  result := updateTask(task);
end;

function TTaskManagerSubtasks.getSubtasksForAssignee(const assignee: string): TSubtaskArray;
var
  allTasks: TTaskArray;
  i, j: integer;
  count: integer;
begin
  setlength(result, 0);
  allTasks := getAllTasks;
  count := 0;

  for i := 0 to length(allTasks) - 1 do
  begin
    for j := 0 to length(allTasks[i].subtasks) - 1 do
    begin
      if allTasks[i].subtasks[j].assignedTo = assignee then
      begin
        setlength(result, count + 1);
        result[count] := allTasks[i].subtasks[j];
        inc(count);
      end;
    end;
  end;
end;

function TTaskManagerSubtasks.getSubtaskCompletionPercentage(taskId: integer): double;
var
  task: TTask;
  completed: integer;
  total: integer;
  i: integer;
begin
  result := 0.0;
  if not getTask(taskId, task) then
    exit;

  total := length(task.subtasks);
  if total = 0 then
  begin
    result := 100.0;
    exit;
  end;

  completed := 0;
  for i := 0 to total - 1 do
  begin
    if task.subtasks[i].status = tsCompleted then
      inc(completed);
  end;

  result := (completed / total) * 100.0;
end;

function TTaskManagerSubtasks.areAllSubtasksComplete(taskId: integer): boolean;
var
  task: TTask;
  i: integer;
begin
  result := true;
  if not getTask(taskId, task) then
    exit;

  if length(task.subtasks) = 0 then
    exit;

  for i := 0 to length(task.subtasks) - 1 do
  begin
    if task.subtasks[i].status <> tsCompleted then
    begin
      result := false;
      exit;
    end;
  end;
end;

function TTaskManagerSubtasks.getSubtasksProgress: TTaskStats;
var
  allTasks: TTaskArray;
  i, j: integer;
  stats: TTaskStats;
begin
  stats := getStatistics;
  allTasks := getAllTasks;

  for i := 0 to length(allTasks) - 1 do
  begin
    for j := 0 to length(allTasks[i].subtasks) - 1 do
    begin
      case allTasks[i].subtasks[j].status of
        tsNew: stats.newTasks := stats.newTasks + 1;
        tsInProgress: stats.inProgressTasks := stats.inProgressTasks + 1;
        tsCompleted: stats.completedTasks := stats.completedTasks + 1;
        tsCancelled: stats.cancelledTasks := stats.cancelledTasks + 1;
      end;
    end;
  end;

  result := stats;
end;

end.
