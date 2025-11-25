
unit TaskManagerEnhanced;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager,
  TaskManagerExtended,
  TaskManagerAdvanced,
  TaskManagerSubtasks,
  TaskManagerHistory;

type
  TTaskManagerEnhanced = class(TTaskManagerHistory)
  public
    function escalatePriorityIfDue(taskId: integer): boolean;
    function escalatePrioritiesDueInDays(days: integer): integer;
    function getTasksByEscalationRisk: TTaskArray;
    function batchUpdatePriority(const taskIds: array of integer; newPriority: TTaskPriority): integer;
    function batchUpdateStatus(const taskIds: array of integer; newStatus: TTaskStatus): integer;
    function batchAssignTasks(const taskIds: array of integer; const assignee: string): integer;
    function batchDeleteTasks(const taskIds: array of integer): integer;
    function getTasksNearDeadline(daysUntilDue: integer): TTaskArray;
    function getProductivityMetrics: TTaskStats;
  end;

implementation

function TTaskManagerEnhanced.escalatePriorityIfDue(taskId: integer): boolean;
var
  task: TTask;
  daysUntilDue: double;
begin
  result := false;
  if not getTask(taskId, task) then
    exit;

  if task.status = tsCompleted then
    exit;

  daysUntilDue := task.dueDate - now;

  if (daysUntilDue <= 1) and (task.priority < tpCritical) then
  begin
    task.priority := tpCritical;
    recordTaskChange(taskId, 'priority', 'previous', 'critical', 'system');
    result := updateTask(task);
  end
  else if (daysUntilDue <= 3) and (task.priority < tpHigh) then
  begin
    task.priority := tpHigh;
    recordTaskChange(taskId, 'priority', 'previous', 'high', 'system');
    result := updateTask(task);
  end
  else if (daysUntilDue <= 7) and (task.priority < tpMedium) then
  begin
    task.priority := tpMedium;
    recordTaskChange(taskId, 'priority', 'previous', 'medium', 'system');
    result := updateTask(task);
  end;
end;

function TTaskManagerEnhanced.escalatePrioritiesDueInDays(days: integer): integer;
var
  allTasks: TTaskArray;
  i: integer;
  updated: integer;
begin
  result := 0;
  allTasks := getAllTasks;

  for i := 0 to length(allTasks) - 1 do
  begin
    if (allTasks[i].dueDate - now) <= days then
    begin
      if escalatePriorityIfDue(allTasks[i].id) then
        inc(result);
    end;
  end;
end;

function TTaskManagerEnhanced.getTasksByEscalationRisk: TTaskArray;
var
  allTasks: TTaskArray;
  i: integer;
  count: integer;
  daysUntilDue: double;
begin
  setlength(result, 0);
  allTasks := getAllTasks;
  count := 0;

  for i := 0 to length(allTasks) - 1 do
  begin
    if allTasks[i].status <> tsCompleted then
    begin
      daysUntilDue := allTasks[i].dueDate - now;
      if daysUntilDue <= 7 then
      begin
        setlength(result, count + 1);
        result[count] := allTasks[i];
        inc(count);
      end;
    end;
  end;
end;

function TTaskManagerEnhanced.batchUpdatePriority(const taskIds: array of integer; newPriority: TTaskPriority): integer;
var
  i: integer;
  task: TTask;
begin
  result := 0;
  for i := 0 to length(taskIds) - 1 do
  begin
    if getTask(taskIds[i], task) then
    begin
      if setTaskPriority(taskIds[i], newPriority) then
      begin
        recordTaskChange(taskIds[i], 'priority', '', '', 'batch_operation');
        inc(result);
      end;
    end;
  end;
end;

function TTaskManagerEnhanced.batchUpdateStatus(const taskIds: array of integer; newStatus: TTaskStatus): integer;
var
  i: integer;
  task: TTask;
begin
  result := 0;
  for i := 0 to length(taskIds) - 1 do
  begin
    if getTask(taskIds[i], task) then
    begin
      if setTaskStatus(taskIds[i], newStatus) then
      begin
        recordTaskChange(taskIds[i], 'status', '', '', 'batch_operation');
        inc(result);
      end;
    end;
  end;
end;

function TTaskManagerEnhanced.batchAssignTasks(const taskIds: array of integer; const assignee: string): integer;
var
  i: integer;
  task: TTask;
begin
  result := 0;
  for i := 0 to length(taskIds) - 1 do
  begin
    if getTask(taskIds[i], task) then
    begin
      if assignTaskTo(taskIds[i], assignee) then
      begin
        recordTaskChange(taskIds[i], 'assignedTo', task.assignedTo, assignee, 'batch_operation');
        inc(result);
      end;
    end;
  end;
end;

function TTaskManagerEnhanced.batchDeleteTasks(const taskIds: array of integer): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to length(taskIds) - 1 do
  begin
    if deleteTask(taskIds[i]) then
      inc(result);
  end;
end;

function TTaskManagerEnhanced.getTasksNearDeadline(daysUntilDue: integer): TTaskArray;
var
  allTasks: TTaskArray;
  i: integer;
  count: integer;
  daysDiff: double;
begin
  setlength(result, 0);
  allTasks := getAllTasks;
  count := 0;

  for i := 0 to length(allTasks) - 1 do
  begin
    if allTasks[i].status <> tsCompleted then
    begin
      daysDiff := allTasks[i].dueDate - now;
      if (daysDiff > 0) and (daysDiff <= daysUntilDue) then
      begin
        setlength(result, count + 1);
        result[count] := allTasks[i];
        inc(count);
      end;
    end;
  end;
end;

function TTaskManagerEnhanced.getProductivityMetrics: TTaskStats;
var
  stats: TTaskStats;
  allTasks: TTaskArray;
  i: integer;
  totalTime: double;
  completedCount: integer;
begin
  stats := getStatistics;
  allTasks := getAllTasks;
  totalTime := 0;
  completedCount := 0;

  for i := 0 to length(allTasks) - 1 do
  begin
    if allTasks[i].status = tsCompleted then
    begin
      inc(completedCount);
      if allTasks[i].actualHours > 0 then
        totalTime := totalTime + allTasks[i].actualHours;
    end;
  end;

  if completedCount > 0 then
    stats.averageCompletionTime := totalTime / completedCount;

  result := stats;
end;

end.
