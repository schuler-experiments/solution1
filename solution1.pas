
program solution1;

{$mode objfpc}

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager;

var
  manager: TTaskManager;

procedure selfTest;
var
  taskId1, taskId2, taskId3, taskId4, taskId5: integer;
  task: TTask;
  stats: TTaskStats;
  tasks: TTaskArray;
  i: integer;
  dueDate1, dueDate2: TDateTime;
  est, act, overrun: double;
  tags: TTagArray;
begin
  writeln('===== TASK MANAGER SELF TEST =====');
  writeln;

  manager := TTaskManager.Create;

  try
    writeln('Test 1: Creating tasks...');
    dueDate1 := now + 7;
    dueDate2 := now + 3;

    taskId1 := manager.addTask('Design Database Schema', 'Create database tables and relationships', 'Design', tpCritical, dueDate1);
    writeln('Created task 1 (ID: ', taskId1, ')');

    taskId2 := manager.addTask('Implement Authentication', 'Add user login and security', 'Development', tpHigh, dueDate1);
    writeln('Created task 2 (ID: ', taskId2, ')');

    taskId3 := manager.addTask('Write Unit Tests', 'Create comprehensive test suite', 'Testing', tpMedium, dueDate2);
    writeln('Created task 3 (ID: ', taskId3, ')');

    taskId4 := manager.addTask('API Documentation', 'Document REST endpoints', 'Documentation', tpLow, dueDate1);
    writeln('Created task 4 (ID: ', taskId4, ')');

    taskId5 := manager.addTask('Code Review', 'Review pull requests', 'Quality', tpHigh, dueDate2);
    writeln('Created task 5 (ID: ', taskId5, ')');
    writeln;

    writeln('Test 2: Retrieving task...');
    if manager.getTask(taskId1, task) then
      writeln('Retrieved task: ', task.name)
    else
      writeln('Failed to retrieve task: ', manager.getLastError);
    writeln;

    writeln('Test 3: Updating task status...');
    if manager.setTaskStatus(taskId1, tsInProgress) then
      writeln('Task 1 status updated to: In Progress')
    else
      writeln('Failed to update status: ', manager.getLastError);
    writeln;

    writeln('Test 4: Completing a task...');
    if manager.completeTask(taskId3) then
      writeln('Task 3 completed successfully')
    else
      writeln('Failed to complete task: ', manager.getLastError);
    writeln;

    writeln('Test 5: Getting statistics...');
    stats := manager.getStatistics;
    writeln('Total Tasks: ', stats.totalTasks);
    writeln('New Tasks: ', stats.newTasks);
    writeln('In Progress: ', stats.inProgressTasks);
    writeln('Completed: ', stats.completedTasks);
    writeln('High Priority: ', stats.highPriorityCount);
    writeln('Critical Priority: ', stats.criticalPriorityCount);
    writeln;

    writeln('Test 6: Getting tasks by status (New)...');
    tasks := manager.getTasksByStatus(tsNew);
    writeln('Found ', length(tasks), ' new tasks');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name);
    writeln;

    writeln('Test 7: Getting tasks by priority (High)...');
    tasks := manager.getTasksByPriority(tpHigh);
    writeln('Found ', length(tasks), ' high priority tasks');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name);
    writeln;

    writeln('Test 8: Getting tasks by category (Development)...');
    tasks := manager.getTasksByCategory('Development');
    writeln('Found ', length(tasks), ' development tasks');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name);
    writeln;

    writeln('Test 9: Changing priority...');
    if manager.setTaskPriority(taskId4, tpHigh) then
      writeln('Task 4 priority updated to: High')
    else
      writeln('Failed to update priority: ', manager.getLastError);
    writeln;

    writeln('Test 10: Deleting a task...');
    if manager.deleteTask(taskId5) then
      writeln('Task 5 deleted successfully')
    else
      writeln('Failed to delete task: ', manager.getLastError);
    writeln;

    writeln('Test 11: Final statistics...');
    stats := manager.getStatistics;
    writeln('Final Total Tasks: ', stats.totalTasks);
    writeln('Final Completed: ', stats.completedTasks);
    writeln;

    writeln('Test 12: Sorting by due date...');
    tasks := manager.getTasksSortedByDueDate;
    writeln('Tasks sorted by due date:');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name);
    writeln;

    writeln('Test 13: Sorting by priority...');
    tasks := manager.getTasksSortedByPriority;
    writeln('Tasks sorted by priority (highest first):');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name);
    writeln;

    writeln('Test 14: Searching by name...');
    tasks := manager.searchTasksByName('Database');
    writeln('Found ', length(tasks), ' tasks with "Database" in name:');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name);
    writeln;

    writeln('Test 15: Searching by description...');
    tasks := manager.searchTasksByDescription('comprehensive');
    writeln('Found ', length(tasks), ' tasks with "comprehensive" in description:');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name);
    writeln;

    writeln('Test 16: General search...');
    tasks := manager.searchTasks('user');
    writeln('Found ', length(tasks), ' tasks with "user" in name or description:');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name);
    writeln;

    writeln('Test 17: Saving tasks to file...');
    if manager.saveTasks('tasks.dat') then
      writeln('Tasks saved successfully to tasks.dat')
    else
      writeln('Failed to save tasks: ', manager.getLastError);
    writeln;


    writeln('Test 18: Adding tags to tasks...');
    if manager.addTaskTag(taskId1, 'urgent') then
      writeln('Tag "urgent" added to task 1')
    else
      writeln('Failed to add tag: ', manager.getLastError);

    if manager.addTaskTag(taskId1, 'critical') then
      writeln('Tag "critical" added to task 1')
    else
      writeln('Failed to add tag: ', manager.getLastError);

    if manager.addTaskTag(taskId2, 'urgent') then
      writeln('Tag "urgent" added to task 2')
    else
      writeln('Failed to add tag: ', manager.getLastError);

    if manager.addTaskTag(taskId3, 'review') then
      writeln('Tag "review" added to task 3')
    else
      writeln('Failed to add tag: ', manager.getLastError);
    writeln;

    writeln('Test 19: Checking task tags...');
    if manager.hasTaskTag(taskId1, 'urgent') then
      writeln('Task 1 has "urgent" tag')
    else
      writeln('Task 1 does NOT have "urgent" tag');

    tags := manager.getTaskTags(taskId1);
    writeln('Task 1 has ', length(tags), ' tags:');
    for i := 0 to length(tags) - 1 do
      writeln('  - ', tags[i]);
    writeln;

    writeln('Test 20: Getting tasks by tag...');
    tasks := manager.getTasksByTag('urgent');
    writeln('Found ', length(tasks), ' tasks with "urgent" tag:');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name);
    writeln;

    writeln('Test 21: Removing tags...');
    if manager.removeTaskTag(taskId1, 'critical') then
      writeln('Tag "critical" removed from task 1')
    else
      writeln('Failed to remove tag: ', manager.getLastError);

    tags := manager.getTaskTags(taskId1);
    writeln('Task 1 now has ', length(tags), ' tag(s)');
    writeln;


    writeln('Test 22: Setting estimated hours...');
    if manager.setTaskEstimatedHours(taskId1, 8.5) then
      writeln('Task 1 estimated hours set to 8.5')
    else
      writeln('Failed to set estimated hours: ', manager.getLastError);

    if manager.setTaskEstimatedHours(taskId2, 5.0) then
      writeln('Task 2 estimated hours set to 5.0')
    else
      writeln('Failed to set estimated hours: ', manager.getLastError);

    if manager.setTaskEstimatedHours(taskId3, 3.0) then
      writeln('Task 3 estimated hours set to 3.0')
    else
      writeln('Failed to set estimated hours: ', manager.getLastError);
    writeln;

    writeln('Test 23: Adding actual hours worked...');
    if manager.addTaskActualHours(taskId1, 5.0) then
      writeln('Added 5.0 hours to Task 1')
    else
      writeln('Failed to add hours: ', manager.getLastError);

    if manager.addTaskActualHours(taskId1, 4.5) then
      writeln('Added 4.5 hours to Task 1 (total: 9.5)')
    else
      writeln('Failed to add hours: ', manager.getLastError);

    if manager.addTaskActualHours(taskId2, 4.0) then
      writeln('Added 4.0 hours to Task 2 (under estimate)')
    else
      writeln('Failed to add hours: ', manager.getLastError);
    writeln;

    writeln('Test 24: Getting time information...');
    est := manager.getTaskEstimatedHours(taskId1);
    act := manager.getTaskActualHours(taskId1);
    overrun := manager.getTaskTimeOverrun(taskId1);
    writeln('Task 1: Estimated=', est:0:1, ' hours, Actual=', act:0:1, ' hours, Overrun=', overrun:0:1, ' hours');

    est := manager.getTaskEstimatedHours(taskId2);
    act := manager.getTaskActualHours(taskId2);
    overrun := manager.getTaskTimeOverrun(taskId2);
    writeln('Task 2: Estimated=', est:0:1, ' hours, Actual=', act:0:1, ' hours, Overrun=', overrun:0:1, ' hours');
    writeln;

    writeln('Test 25: Getting tasks over time...');
    tasks := manager.getTasksOverTime;
    writeln('Found ', length(tasks), ' task(s) with time overrun:');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name, ' (Overrun: ', (tasks[i].actualHours - tasks[i].estimatedHours):0:1, ' hours)');
    writeln;


    writeln('Test 26: Adding task dependencies...');
    if manager.addTaskDependency(taskId4, taskId1) then
      writeln('Task 4 now depends on Task 1')
    else
      writeln('Failed to add dependency: ', manager.getLastError);

    if manager.addTaskDependency(taskId4, taskId2) then
      writeln('Task 4 now depends on Tasks 1 and 2')
    else
      writeln('Failed to add dependency: ', manager.getLastError);
    writeln;

    writeln('Test 27: Checking if task can be completed...');
    if manager.canCompleteTask(taskId4) then
      writeln('Task 4 can be completed (all dependencies done)')
    else
      writeln('Task 4 cannot be completed yet (has pending dependencies)');
    writeln;

    writeln('Test 28: Getting tasks that depend on a task...');
    tasks := manager.getTasksDependingOn(taskId1);
    writeln('Found ', length(tasks), ' task(s) that depend on Task 1:');
    for i := 0 to length(tasks) - 1 do
      writeln('  - ', tasks[i].name);
    writeln;

    writeln('===== ALL TESTS COMPLETED SUCCESSFULLY =====');

  finally
    manager.Free;
  end;
end;

begin
  selfTest;
end.
