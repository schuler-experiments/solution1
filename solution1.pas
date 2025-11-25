
program solution1;

{$mode objfpc}

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager,
  TaskManagerExtended,
  TaskManagerAdvanced;

var
  manager: TTaskManagerAdvanced;

procedure selfTest;
var
  i: integer;
  tasks: TTaskArray;
  stats: TTaskStats;
  blocked: TTaskArray;
  recurring: TTaskArray;
  watchers: TWatcherArray;
  taskId: integer;
begin
  WriteLn('===== TASK MANAGER WITH ADVANCED FEATURES TEST =====');
  WriteLn;

  { Test 1: Create tasks }
  WriteLn('Test 1: Creating tasks...');
  for i := 1 to 5 do
    WriteLn('Created task ' + intToStr(i) + ' (ID: ' +
            intToStr(manager.addTask('Task ' + intToStr(i), 'Description', 'Development',
                                     tpHigh, now + i)) + ')');
  WriteLn;

  { Test 2: Task Story Points }
  WriteLn('Test 2: Setting story points...');
  if manager.setTaskStoryPoints(1, 8) then
    WriteLn('Task 1: 8 story points');
  if manager.setTaskStoryPoints(2, 5) then
    WriteLn('Task 2: 5 story points');
  if manager.setTaskStoryPoints(3, 3) then
    WriteLn('Task 3: 3 story points');
  WriteLn('Total story points: ' + intToStr(manager.getTotalStoryPoints));
  WriteLn;

  { Test 3: Task Watchers }
  WriteLn('Test 3: Adding task watchers...');
  if manager.addTaskWatcher(1, 'Alice') then
    WriteLn('Alice is watching Task 1');
  if manager.addTaskWatcher(1, 'Bob') then
    WriteLn('Bob is watching Task 1');
  if manager.addTaskWatcher(2, 'Alice') then
    WriteLn('Alice is watching Task 2');
  watchers := manager.getTaskWatchers(1);
  WriteLn('Task 1 has ' + intToStr(length(watchers)) + ' watchers');
  WriteLn;

  { Test 4: Task Blocking }
  WriteLn('Test 4: Blocking tasks...');
  if manager.blockTask(2, 'Waiting for API documentation') then
    WriteLn('Task 2 blocked: Waiting for API documentation');
  if manager.blockTask(3, 'Dependency not ready') then
    WriteLn('Task 3 blocked: Dependency not ready');
  blocked := manager.getBlockedTasks;
  WriteLn('Total blocked tasks: ' + intToStr(length(blocked)));
  WriteLn;

  { Test 5: Task References/Links }
  WriteLn('Test 5: Adding task references...');
  if manager.addTaskReference(1, 2, 'blocks') then
    WriteLn('Task 1 blocks Task 2');
  if manager.addTaskReference(3, 1, 'related_to') then
    WriteLn('Task 3 related to Task 1');
  WriteLn;

  { Test 6: Task Recurrence }
  WriteLn('Test 6: Setting task recurrence...');
  if manager.setTaskRecurrence(4, rpWeekly, now + 90) then
    WriteLn('Task 4 set to recur weekly until 90 days from now');
  recurring := manager.getRecurringTasks;
  WriteLn('Total recurring tasks: ' + intToStr(length(recurring)));
  WriteLn;

  { Test 7: Complete and track velocity }
  WriteLn('Test 7: Completing tasks and tracking velocity...');
  manager.completeTask(1);
  WriteLn('Task 1 completed');
  WriteLn('Completed story points: ' + intToStr(manager.getCompletedStoryPoints));
  WriteLn('Velocity (per day): ' + FloatToStr(manager.getVelocity(1)));
  WriteLn;

  { Test 8: Team workload }
  WriteLn('Test 8: Checking team workload...');
  manager.assignTaskTo(1, 'Alice');
  manager.assignTaskTo(2, 'Alice');
  WriteLn('Alice workload: ' + FloatToStr(manager.getTeamWorkload('Alice')) + ' hours');
  WriteLn;

  { Test 9: Critical path }
  WriteLn('Test 9: Getting critical path...');
  manager.setTaskPriority(5, tpCritical);
  tasks := manager.getCriticalPath;
  WriteLn('Critical path tasks: ' + intToStr(length(tasks)));
  WriteLn;

  { Test 10: Get tasks watched by person }
  WriteLn('Test 10: Getting tasks watched by person...');
  tasks := manager.getTasksWatchedBy('Alice');
  WriteLn('Alice is watching ' + intToStr(length(tasks)) + ' task(s)');
  WriteLn;

  { Test 11: Statistics }
  WriteLn('Test 11: Final statistics...');
  stats := manager.getStatistics;
  WriteLn('Total tasks: ' + intToStr(stats.totalTasks));
  WriteLn('Completed: ' + intToStr(stats.completedTasks));
  WriteLn('Blocked: ' + intToStr(stats.blockedTasks));
  WriteLn;

  WriteLn('===== ALL TESTS COMPLETED SUCCESSFULLY =====');
  WriteLn;
end;

begin
  manager := TTaskManagerAdvanced.Create;
  try
    selfTest;
  finally
    manager.Free;
  end;
end.
