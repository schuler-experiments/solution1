
unit TaskManagerAdvanced;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManagerExtended;

type
  { Extends TTaskManagerExtended with advanced features }
  TTaskManagerAdvanced = class(TTaskManagerExtended)
  public
    { Recurrence management }
    function setTaskRecurrence(taskId: integer; pattern: TRecurrencePattern;
                               endDate: TDateTime): boolean;
    function getTaskRecurrence(taskId: integer; var pattern: TRecurrencePattern;
                                var endDate: TDateTime): boolean;
    function getRecurringTasks: TTaskArray;
    function generateNextRecurrence(taskId: integer): integer;

    { Story points / Agile estimation }
    function setTaskStoryPoints(taskId: integer; points: integer): boolean;
    function getTaskStoryPoints(taskId: integer): integer;
    function getTotalStoryPoints: integer;
    function getCompletedStoryPoints: integer;
    function getVelocity(const sprintDays: integer): double;

    { Task watchers }
    function addTaskWatcher(taskId: integer; const watcher: string): boolean;
    function removeTaskWatcher(taskId: integer; const watcher: string): boolean;
    function hasTaskWatcher(taskId: integer; const watcher: string): boolean;
    function getTaskWatchers(taskId: integer): TWatcherArray;
    function getTasksWatchedBy(const watcher: string): TTaskArray;

    { Task references/links }
    function addTaskReference(taskId: integer; referenceTaskId: integer;
                              const relationshipType: string): boolean;
    function removeTaskReference(taskId: integer; refIndex: integer): boolean;
    function getTaskReferences(taskId: integer): TReferenceArray;
    function getTasksReferencedBy(taskId: integer): TTaskArray;

    { Task blocking }
    function blockTask(taskId: integer; const reason: string): boolean;
    function unblockTask(taskId: integer): boolean;
    function isTaskBlocked(taskId: integer): boolean;
    function getBlockedTasks: TTaskArray;

    { Advanced statistics }
    function getSprintBurndown(const sprintName: string): TTaskArray;
    function getTeamWorkload(const assignee: string): double;
    function getCriticalPath: TTaskArray;
  end;

implementation

function TTaskManagerAdvanced.setTaskRecurrence(taskId: integer;
                                                 pattern: TRecurrencePattern;
                                                 endDate: TDateTime): boolean;
var
  idx: integer;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    tasks[idx].recurrencePattern := pattern;
    tasks[idx].recurrenceEndDate := endDate;
    result := true;
  end;
end;

function TTaskManagerAdvanced.getTaskRecurrence(taskId: integer;
                                                 var pattern: TRecurrencePattern;
                                                 var endDate: TDateTime): boolean;
var
  idx: integer;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    pattern := tasks[idx].recurrencePattern;
    endDate := tasks[idx].recurrenceEndDate;
    result := true;
  end;
end;

function TTaskManagerAdvanced.getRecurringTasks: TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to high(tasks) do
  begin
    if tasks[i].recurrencePattern <> rpNone then
    begin
      setLength(result, count + 1);
      result[count] := tasks[i];
      inc(count);
    end;
  end;
end;

function TTaskManagerAdvanced.generateNextRecurrence(taskId: integer): integer;
var
  idx: integer;
  newTask: TTask;
  daysToAdd: integer;
begin
  result := -1;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    newTask := tasks[idx];
    daysToAdd := 1;

    case tasks[idx].recurrencePattern of
      rpDaily: daysToAdd := 1;
      rpWeekly: daysToAdd := 7;
      rpBiWeekly: daysToAdd := 14;
      rpMonthly: daysToAdd := 30;
      rpQuarterly: daysToAdd := 90;
      rpYearly: daysToAdd := 365;
      else
        exit;
    end;

    newTask.dueDate := tasks[idx].dueDate + daysToAdd;
    if newTask.dueDate <= tasks[idx].recurrenceEndDate then
    begin
      newTask.status := tsNew;
      newTask.completedDate := 0;
      newTask.actualHours := 0;
      newTask.id := nextTaskId;
      inc(nextTaskId);
      setLength(tasks, length(tasks) + 1);
      tasks[high(tasks)] := newTask;
      result := newTask.id;
    end;
  end;
end;

function TTaskManagerAdvanced.setTaskStoryPoints(taskId: integer;
                                                  points: integer): boolean;
var
  idx: integer;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    tasks[idx].storyPoints := points;
    result := true;
  end;
end;

function TTaskManagerAdvanced.getTaskStoryPoints(taskId: integer): integer;
var
  idx: integer;
begin
  result := 0;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
    result := tasks[idx].storyPoints;
end;

function TTaskManagerAdvanced.getTotalStoryPoints: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to high(tasks) do
    result := result + tasks[i].storyPoints;
end;

function TTaskManagerAdvanced.getCompletedStoryPoints: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to high(tasks) do
  begin
    if tasks[i].status = tsCompleted then
      result := result + tasks[i].storyPoints;
  end;
end;

function TTaskManagerAdvanced.getVelocity(const sprintDays: integer): double;
var
  completedPoints: integer;
begin
  completedPoints := getCompletedStoryPoints;
  if sprintDays > 0 then
    result := completedPoints / sprintDays
  else
    result := 0;
end;

function TTaskManagerAdvanced.addTaskWatcher(taskId: integer;
                                              const watcher: string): boolean;
var
  idx, count: integer;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    if length(tasks[idx].watchers) < maxWatchersPerTask then
    begin
      count := length(tasks[idx].watchers);
      setLength(tasks[idx].watchers, count + 1);
      tasks[idx].watchers[count] := watcher;
      result := true;
    end;
  end;
end;

function TTaskManagerAdvanced.removeTaskWatcher(taskId: integer;
                                                 const watcher: string): boolean;
var
  idx, i, j: integer;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    for i := 0 to high(tasks[idx].watchers) do
    begin
      if tasks[idx].watchers[i] = watcher then
      begin
        for j := i to high(tasks[idx].watchers) - 1 do
          tasks[idx].watchers[j] := tasks[idx].watchers[j + 1];
        setLength(tasks[idx].watchers, length(tasks[idx].watchers) - 1);
        result := true;
        exit;
      end;
    end;
  end;
end;

function TTaskManagerAdvanced.hasTaskWatcher(taskId: integer;
                                              const watcher: string): boolean;
var
  idx, i: integer;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    for i := 0 to high(tasks[idx].watchers) do
    begin
      if tasks[idx].watchers[i] = watcher then
      begin
        result := true;
        exit;
      end;
    end;
  end;
end;

function TTaskManagerAdvanced.getTaskWatchers(taskId: integer): TWatcherArray;
var
  idx: integer;
begin
  setLength(result, 0);
  idx := findTaskIndex(taskId);
  if idx >= 0 then
    result := copy(tasks[idx].watchers, 0, length(tasks[idx].watchers));
end;

function TTaskManagerAdvanced.getTasksWatchedBy(const watcher: string): TTaskArray;
var
  i, j, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to high(tasks) do
  begin
    for j := 0 to high(tasks[i].watchers) do
    begin
      if tasks[i].watchers[j] = watcher then
      begin
        setLength(result, count + 1);
        result[count] := tasks[i];
        inc(count);
        break;
      end;
    end;
  end;
end;

function TTaskManagerAdvanced.addTaskReference(taskId: integer;
                                                referenceTaskId: integer;
                                                const relationshipType: string): boolean;
var
  idx, count: integer;
  newRef: TTaskReference;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    if length(tasks[idx].references) < maxReferencesPerTask then
    begin
      newRef.referenceId := referenceTaskId;
      newRef.relationshipType := relationshipType;
      count := length(tasks[idx].references);
      setLength(tasks[idx].references, count + 1);
      tasks[idx].references[count] := newRef;
      result := true;
    end;
  end;
end;

function TTaskManagerAdvanced.removeTaskReference(taskId: integer;
                                                   refIndex: integer): boolean;
var
  idx, i: integer;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    if (refIndex >= 0) and (refIndex < length(tasks[idx].references)) then
    begin
      for i := refIndex to high(tasks[idx].references) - 1 do
        tasks[idx].references[i] := tasks[idx].references[i + 1];
      setLength(tasks[idx].references, length(tasks[idx].references) - 1);
      result := true;
    end;
  end;
end;

function TTaskManagerAdvanced.getTaskReferences(taskId: integer): TReferenceArray;
var
  idx: integer;
begin
  setLength(result, 0);
  idx := findTaskIndex(taskId);
  if idx >= 0 then
    result := copy(tasks[idx].references, 0, length(tasks[idx].references));
end;

function TTaskManagerAdvanced.getTasksReferencedBy(taskId: integer): TTaskArray;
var
  refs: TReferenceArray;
  i, count: integer;
  refIdx: integer;
begin
  setLength(result, 0);
  refs := getTaskReferences(taskId);
  count := 0;

  for i := 0 to high(refs) do
  begin
    refIdx := findTaskIndex(refs[i].referenceId);
    if refIdx >= 0 then
    begin
      setLength(result, count + 1);
      result[count] := tasks[refIdx];
      inc(count);
    end;
  end;
end;

function TTaskManagerAdvanced.blockTask(taskId: integer;
                                         const reason: string): boolean;
var
  idx: integer;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    tasks[idx].isBlocked := true;
    tasks[idx].blockReason := reason;
    if tasks[idx].status <> tsCompleted then
      tasks[idx].status := tsBlocked;
    result := true;
  end;
end;

function TTaskManagerAdvanced.unblockTask(taskId: integer): boolean;
var
  idx: integer;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
  begin
    tasks[idx].isBlocked := false;
    tasks[idx].blockReason := '';
    if tasks[idx].status = tsBlocked then
      tasks[idx].status := tsNew;
    result := true;
  end;
end;

function TTaskManagerAdvanced.isTaskBlocked(taskId: integer): boolean;
var
  idx: integer;
begin
  result := false;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
    result := tasks[idx].isBlocked;
end;

function TTaskManagerAdvanced.getBlockedTasks: TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to high(tasks) do
  begin
    if tasks[i].isBlocked then
    begin
      setLength(result, count + 1);
      result[count] := tasks[i];
      inc(count);
    end;
  end;
end;

function TTaskManagerAdvanced.getSprintBurndown(const sprintName: string): TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to high(tasks) do
  begin
    if tasks[i].category = sprintName then
    begin
      setLength(result, count + 1);
      result[count] := tasks[i];
      inc(count);
    end;
  end;
end;

function TTaskManagerAdvanced.getTeamWorkload(const assignee: string): double;
var
  taskList: TTaskArray;
  i: integer;
begin
  result := 0;
  taskList := getTasksAssignedTo(assignee);
  for i := 0 to high(taskList) do
  begin
    if taskList[i].status = tsInProgress then
      result := result + taskList[i].estimatedHours;
  end;
end;

function TTaskManagerAdvanced.getCriticalPath: TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to high(tasks) do
  begin
    if (tasks[i].priority = tpCritical) and (tasks[i].status <> tsCompleted) then
    begin
      setLength(result, count + 1);
      result[count] := tasks[i];
      inc(count);
    end;
  end;
end;

end.
