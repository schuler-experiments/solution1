
program TaskManagerMain;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, Math,
  TaskTypes, TaskManager, TaskStorage;

var
  Manager: TTaskManagerCore;
  Storage: TTaskStorage;

procedure PrintTask(const aTask: TTask);
begin
  WriteLn('  ID: ', aTask.ID);
  WriteLn('  Title: ', aTask.Title);
  WriteLn('  Description: ', aTask.Description);
  WriteLn('  Priority: ', PriorityToString(aTask.Priority));
  WriteLn('  Status: ', StatusToString(aTask.Status));
  WriteLn('  Created: ', DateTimeToStr(aTask.CreatedDate));
  if aTask.DueDate > 0 then
    WriteLn('  Due: ', DateTimeToStr(aTask.DueDate));
  if aTask.CompletedDate > 0 then
    WriteLn('  Completed: ', DateTimeToStr(aTask.CompletedDate));
  WriteLn;
end;

procedure PrintTaskList(const aTasks: TTaskArray; const aTitle: string);
var
  i: integer;
begin
  WriteLn('=== ', aTitle, ' ===');
  WriteLn('Total: ', Length(aTasks), ' task(s)');
  WriteLn;
  
  for i := 0 to High(aTasks) do
    PrintTask(aTasks[i]);
end;

procedure PrintStatistics(const aStats: TTaskStatistics);
begin
  WriteLn('=== Task Statistics ===');
  WriteLn('Total Tasks: ', aStats.TotalTasks);
  WriteLn('Active Tasks: ', aStats.ActiveTasks);
  WriteLn('Completed Tasks: ', aStats.CompletedTasks);
  WriteLn('Cancelled Tasks: ', aStats.CancelledTasks);
  WriteLn('High Priority Tasks: ', aStats.HighPriorityTasks);
  WriteLn('Overdue Tasks: ', aStats.OverdueTasks);
  WriteLn;
end;

procedure SelfTest;
var
  taskID1, taskID2, taskID3, taskID4: integer;
  tasks: TTaskArray;
  task: TTask;
  stats: TTaskStatistics;
  criteria: TSearchCriteria;
  tomorrow, nextWeek, yesterday: TDateTime;
begin
  WriteLn('========================================');
  WriteLn('Task Manager Self-Test');
  WriteLn('Version: ', VERSION);
  WriteLn('========================================');
  WriteLn;
  
  // Initialize dates for testing
  tomorrow := IncDay(Now, 1);
  nextWeek := IncDay(Now, 7);
  yesterday := IncDay(Now, -1);
  
  // Test 1: Create tasks
  WriteLn('--- Test 1: Creating Tasks ---');
  taskID1 := Manager.AddTask('Complete project documentation', 
                             'Write comprehensive docs for the task manager',
                             tpHigh, tomorrow);
  WriteLn('Created task #', taskID1);
  
  taskID2 := Manager.AddTask('Review code', 
                             'Review Pascal code for best practices',
                             tpMedium, nextWeek);
  WriteLn('Created task #', taskID2);
  
  taskID3 := Manager.AddTask('Fix bugs', 
                             'Address reported issues in tracker',
                             tpCritical, yesterday);
  WriteLn('Created task #', taskID3);
  
  taskID4 := Manager.AddTask('Write unit tests', 
                             'Create comprehensive test suite',
                             tpLow, nextWeek);
  WriteLn('Created task #', taskID4);
  
  WriteLn('Total tasks created: ', Manager.TaskCount);
  WriteLn;
  
  // Test 2: List all tasks
  WriteLn('--- Test 2: Listing All Tasks ---');
  tasks := Manager.GetAllTasks;
  PrintTaskList(tasks, 'All Tasks');
  
  // Test 3: Update task status
  WriteLn('--- Test 3: Updating Task Status ---');
  if Manager.SetTaskStatus(taskID2, tsInProgress) then
    WriteLn('Task #', taskID2, ' status updated to In Progress');
  
  if Manager.CompleteTask(taskID4) then
    WriteLn('Task #', taskID4, ' marked as completed');
  WriteLn;
  
  // Test 4: Get active tasks
  WriteLn('--- Test 4: Getting Active Tasks ---');
  tasks := Manager.GetActiveTasks;
  PrintTaskList(tasks, 'Active Tasks');
  
  // Test 5: Search by priority
  WriteLn('--- Test 5: Searching High Priority Tasks ---');
  tasks := Manager.GetTasksByPriority(tpHigh);
  PrintTaskList(tasks, 'High Priority Tasks');
  
  // Test 6: Get overdue tasks
  WriteLn('--- Test 6: Finding Overdue Tasks ---');
  tasks := Manager.GetOverdueTasks;
  PrintTaskList(tasks, 'Overdue Tasks');
  
  // Test 7: Search with criteria
  WriteLn('--- Test 7: Searching with Custom Criteria ---');
  FillChar(criteria, SizeOf(criteria), 0);
  criteria.SearchTitle := 'code';
  tasks := Manager.SearchTasks(criteria);
  PrintTaskList(tasks, 'Tasks containing "code" in title');
  
  // Test 8: Get statistics
  WriteLn('--- Test 8: Task Statistics ---');
  stats := Manager.GetStatistics;
  PrintStatistics(stats);
  
  // Test 9: Save tasks to file
  WriteLn('--- Test 9: Saving Tasks to File ---');
  if Storage.SaveTasks(Manager.GetTasksArray, Manager.NextID) then
    WriteLn('Tasks saved successfully to ', Storage.FileName)
  else
    WriteLn('Failed to save tasks');
  WriteLn;
  
  // Test 10: Export to CSV
  WriteLn('--- Test 10: Exporting to CSV ---');
  if Storage.ExportToCSV(Manager.GetAllTasks, 'solution1/tasks_export.csv') then
    WriteLn('Tasks exported to tasks_export.csv')
  else
    WriteLn('Failed to export to CSV');
  WriteLn;
  
  // Test 11: Export to text
  WriteLn('--- Test 11: Exporting to Text ---');
  if Storage.ExportToText(Manager.GetAllTasks, 'solution1/tasks_export.txt') then
    WriteLn('Tasks exported to tasks_export.txt')
  else
    WriteLn('Failed to export to text');
  WriteLn;
  
  // Test 12: Clear and reload
  WriteLn('--- Test 12: Clearing and Reloading ---');
  Manager.ClearAll;
  WriteLn('Tasks cleared. Count: ', Manager.TaskCount);
  
  if Storage.LoadTasks(tasks, taskID1) then
  begin
    Manager.SetTasksArray(tasks, taskID1);
    WriteLn('Tasks reloaded successfully. Count: ', Manager.TaskCount);
  end
  else
    WriteLn('Failed to reload tasks');
  WriteLn;
  
  // Test 13: Verify loaded data
  WriteLn('--- Test 13: Verifying Loaded Data ---');
  if Manager.GetTask(taskID2, task) then
  begin
    WriteLn('Successfully retrieved task #', taskID2, ' after reload:');
    PrintTask(task);
  end
  else
    WriteLn('Failed to retrieve task after reload');
  
  // Test 14: Delete task
  WriteLn('--- Test 14: Deleting Task ---');
  if Manager.DeleteTask(taskID1) then
    WriteLn('Task #', taskID1, ' deleted successfully');
  WriteLn('Active tasks after deletion: ', Manager.TaskCount);
  WriteLn;
  
  // Final statistics
  WriteLn('--- Final Statistics ---');
  stats := Manager.GetStatistics;
  PrintStatistics(stats);
  
  WriteLn('========================================');
  WriteLn('Self-Test Completed Successfully!');
  WriteLn('========================================');
end;

begin
  try
    // Initialize manager and storage
    Manager := TTaskManagerCore.Create;
    Storage := TTaskStorage.Create('solution1/' + DATA_FILE);
    
    try
      // Run self-test
      SelfTest;
    finally
      Manager.Free;
      Storage.Free;
    end;
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
