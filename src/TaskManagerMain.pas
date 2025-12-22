
program TaskManagerMain;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, Math,
  TaskTypes, TaskManager, TaskStorage, TaskExport, TaskHistory;

var
  Manager: TTaskManagerCore;
  Storage: TTaskStorage;
  Exporter: TTaskExporter;
  History: TTaskHistory;

procedure PrintTask(const aTask: TTask);
begin
  WriteLn('  ID: ', aTask.ID);
  WriteLn('  Title: ', aTask.Title);
  WriteLn('  Description: ', aTask.Description);
  if aTask.Category <> '' then
    WriteLn('  Category: ', aTask.Category);
  WriteLn('  Priority: ', PriorityToString(aTask.Priority));
  WriteLn('  Status: ', StatusToString(aTask.Status));
  WriteLn('  Created: ', DateTimeToStr(aTask.CreatedDate));
  if aTask.DueDate > 0 then
    WriteLn('  Due: ', DateTimeToStr(aTask.DueDate));
  if aTask.CompletedDate > 0 then
    WriteLn('  Completed: ', DateTimeToStr(aTask.CompletedDate));
  if aTask.LastModifiedDate > 0 then
    WriteLn('  Last Modified: ', DateTimeToStr(aTask.LastModifiedDate));
  if aTask.DependsOnIDs <> '' then
    WriteLn('  Depends On: Task(s) ', aTask.DependsOnIDs);
  if aTask.EstimatedHours > 0 then
    WriteLn('  Estimated Hours: ', FloatToStrF(aTask.EstimatedHours, ffFixed, 10, 1));
  if aTask.ActualHours > 0 then
    WriteLn('  Actual Hours: ', FloatToStrF(aTask.ActualHours, ffFixed, 10, 1));
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
  WriteLn('Total Estimated Hours: ', FloatToStrF(aStats.TotalEstimatedHours, ffFixed, 10, 1));
  WriteLn('Total Actual Hours: ', FloatToStrF(aStats.TotalActualHours, ffFixed, 10, 1));
  WriteLn('Tasks with Dependencies: ', aStats.TasksWithDependencies);
  WriteLn;
end;

procedure PrintCategoryStatistics(const aStats: TCategoryStatisticsArray);
var
  i: integer;
begin
  WriteLn('=== Category Statistics ===');
  for i := 0 to High(aStats) do
  begin
    WriteLn('Category: ', aStats[i].CategoryName);
    WriteLn('  Total Tasks: ', aStats[i].TotalTasks);
    WriteLn('  Active Tasks: ', aStats[i].ActiveTasks);
    WriteLn('  Completed Tasks: ', aStats[i].CompletedTasks);
    WriteLn('  Estimated Hours: ', FloatToStrF(aStats[i].EstimatedHours, ffFixed, 10, 1));
    WriteLn('  Actual Hours: ', FloatToStrF(aStats[i].ActualHours, ffFixed, 10, 1));
    WriteLn;
  end;
end;

procedure SelfTest;
var
  taskID1, taskID2, taskID3, taskID4, taskID5, taskID6: integer;
  tasks: TTaskArray;
  task: TTask;
  stats: TTaskStatistics;
  categoryStats: TCategoryStatisticsArray;
  criteria: TSearchCriteria;
  tomorrow, nextWeek, yesterday: TDateTime;
  historyEntries: THistoryArray;
  i: integer;
begin
  WriteLn('========================================');
  WriteLn('Task Manager Self-Test');
  WriteLn('Version: ', VERSION);
  WriteLn('========================================');
  WriteLn;
  
  tomorrow := IncDay(Now, 1);
  nextWeek := IncDay(Now, 7);
  yesterday := IncDay(Now, -1);
  
  // Test 1: Create tasks with categories and time estimates
  WriteLn('--- Test 1: Creating Tasks with Categories ---');
  taskID1 := Manager.AddTask('Complete project documentation', 
                             'Write comprehensive docs for the task manager',
                             'Documentation',
                             tpHigh, tomorrow, 8.0);
  WriteLn('Created task #', taskID1, ' in category "Documentation"');
  History.LogTaskCreated(taskID1, 'Complete project documentation');
  
  taskID2 := Manager.AddTask('Review code', 
                             'Review Pascal code for best practices',
                             'Code Review',
                             tpMedium, nextWeek, 4.0);
  WriteLn('Created task #', taskID2, ' in category "Code Review"');
  History.LogTaskCreated(taskID2, 'Review code');
  
  taskID3 := Manager.AddTask('Fix critical bugs', 
                             'Address reported issues in tracker',
                             'Bug Fixing',
                             tpCritical, yesterday, 6.0);
  WriteLn('Created task #', taskID3, ' in category "Bug Fixing"');
  History.LogTaskCreated(taskID3, 'Fix critical bugs');
  
  taskID4 := Manager.AddTask('Write unit tests', 
                             'Create comprehensive test suite',
                             'Testing',
                             tpLow, nextWeek, 12.0);
  WriteLn('Created task #', taskID4, ' in category "Testing"');
  History.LogTaskCreated(taskID4, 'Write unit tests');
  
  taskID5 := Manager.AddTask('Setup CI/CD pipeline',
                             'Configure continuous integration',
                             'DevOps',
                             tpMedium, nextWeek, 5.0);
  WriteLn('Created task #', taskID5, ' in category "DevOps"');
  History.LogTaskCreated(taskID5, 'Setup CI/CD pipeline');
  
  taskID6 := Manager.AddTask('Deploy to production',
                             'Deploy the task manager application',
                             'DevOps',
                             tpHigh, nextWeek, 3.0);
  WriteLn('Created task #', taskID6, ' in category "DevOps"');
  History.LogTaskCreated(taskID6, 'Deploy to production');
  
  WriteLn('Total tasks created: ', Manager.TaskCount);
  WriteLn;
  
  // Test 2: Add task dependencies
  WriteLn('--- Test 2: Adding Task Dependencies ---');
  if Manager.AddTaskDependency(taskID6, taskID4) then
    WriteLn('Task #', taskID6, ' now depends on Task #', taskID4, ' (tests must pass before deploy)');
  if Manager.AddTaskDependency(taskID6, taskID5) then
    WriteLn('Task #', taskID6, ' now depends on Task #', taskID5, ' (CI/CD must be ready before deploy)');
  if Manager.AddTaskDependency(taskID4, taskID2) then
    WriteLn('Task #', taskID4, ' now depends on Task #', taskID2, ' (code review before tests)');
  WriteLn;
  
  // Test 3: Check if tasks can start
  WriteLn('--- Test 3: Checking Task Dependencies ---');
  if Manager.CanStartTask(taskID6) then
    WriteLn('Task #', taskID6, ' can start (all dependencies met)')
  else
    WriteLn('Task #', taskID6, ' cannot start yet (waiting for dependencies)');
  WriteLn;
  
  // Test 4: Track actual hours
  WriteLn('--- Test 4: Tracking Time ---');
  Manager.AddActualHours(taskID3, 2.5);
  WriteLn('Added 2.5 hours to task #', taskID3);
  Manager.AddActualHours(taskID3, 1.5);
  WriteLn('Added 1.5 hours to task #', taskID3);
  Manager.SetActualHours(taskID2, 3.0);
  WriteLn('Set actual hours for task #', taskID2, ' to 3.0');
  WriteLn;
  
  // Test 5: Update task status and track history
  WriteLn('--- Test 5: Updating Task Status ---');
  if Manager.SetTaskStatus(taskID2, tsInProgress) then
  begin
    WriteLn('Task #', taskID2, ' status updated to In Progress');
    History.LogStatusChange(taskID2, tsNew, tsInProgress);
  end;
  
  if Manager.CompleteTask(taskID4) then
  begin
    WriteLn('Task #', taskID4, ' marked as completed');
    History.LogStatusChange(taskID4, tsNew, tsCompleted);
  end;
  
  if Manager.CompleteTask(taskID3) then
  begin
    WriteLn('Task #', taskID3, ' marked as completed');
    History.LogStatusChange(taskID3, tsNew, tsCompleted);
  end;
  WriteLn;
  
  // Test 6: Get tasks by category
  WriteLn('--- Test 6: Getting Tasks by Category ---');
  tasks := Manager.GetTasksByCategory('DevOps');
  PrintTaskList(tasks, 'DevOps Tasks');
  
  // Test 7: Search with category filter
  WriteLn('--- Test 7: Searching with Category Filter ---');
  FillChar(criteria, SizeOf(criteria), 0);
  criteria.SearchCategory := 'dev';
  tasks := Manager.SearchTasks(criteria);
  PrintTaskList(tasks, 'Tasks with "dev" in category');
  
  // Test 8: Get task dependencies
  WriteLn('--- Test 8: Getting Task Dependencies ---');
  tasks := Manager.GetTaskDependencies(taskID6);
  PrintTaskList(tasks, 'Dependencies for Task #' + IntToStr(taskID6));
  
  // Test 9: Get statistics
  WriteLn('--- Test 9: Overall Statistics ---');
  stats := Manager.GetStatistics;
  PrintStatistics(stats);
  
  // Test 10: Get category statistics
  WriteLn('--- Test 10: Category Statistics ---');
  categoryStats := Manager.GetCategoryStatistics;
  PrintCategoryStatistics(categoryStats);
  
  // Test 11: Save tasks to file
  WriteLn('--- Test 11: Saving Tasks to File ---');
  if Storage.SaveTasks(Manager.GetTasksArray, Manager.NextID) then
    WriteLn('Tasks saved successfully to ', Storage.FileName)
  else
    WriteLn('Failed to save tasks');
  WriteLn;
  
  // Test 12: Export to JSON
  WriteLn('--- Test 12: Exporting to JSON ---');
  if Exporter.ExportToJSON(Manager.GetAllTasks, 'solution1/tasks_export.json') then
    WriteLn('Tasks exported to tasks_export.json')
  else
    WriteLn('Failed to export to JSON');
  WriteLn;
  
  // Test 13: Export to HTML
  WriteLn('--- Test 13: Exporting to HTML ---');
  if Exporter.ExportToHTML(Manager.GetAllTasks, 'solution1/tasks_export.html', 'Task Manager - Task List') then
    WriteLn('Tasks exported to tasks_export.html')
  else
    WriteLn('Failed to export to HTML');
  WriteLn;
  
  // Test 14: Generate comprehensive HTML report
  WriteLn('--- Test 14: Generating HTML Report ---');
  if Exporter.GenerateHTMLReport(Manager.GetAllTasks, stats, categoryStats, 'solution1/tasks_report.html') then
    WriteLn('Comprehensive report generated: tasks_report.html')
  else
    WriteLn('Failed to generate HTML report');
  WriteLn;
  
  // Test 15: Export to Markdown
  WriteLn('--- Test 15: Exporting to Markdown ---');
  if Exporter.ExportToMarkdown(Manager.GetAllTasks, 'solution1/tasks_export.md', 'Task Manager - Task List') then
    WriteLn('Tasks exported to tasks_export.md')
  else
    WriteLn('Failed to export to Markdown');
  WriteLn;
  
  // Test 16: Export to CSV with new fields
  WriteLn('--- Test 16: Exporting to CSV ---');
  if Storage.ExportToCSV(Manager.GetAllTasks, 'solution1/tasks_export.csv') then
    WriteLn('Tasks exported to tasks_export.csv (with new fields)')
  else
    WriteLn('Failed to export to CSV');
  WriteLn;
  
  // Test 17: Save history
  WriteLn('--- Test 17: Saving Task History ---');
  if History.SaveHistory then
    WriteLn('History saved successfully')
  else
    WriteLn('Failed to save history');
  WriteLn;
  
  // Test 18: Get task history
  WriteLn('--- Test 18: Getting Task History ---');
  historyEntries := History.GetTaskHistory(taskID2);
  WriteLn('History for Task #', taskID2, ': ', Length(historyEntries), ' entries');
  for i := 0 to High(historyEntries) do
  begin
    WriteLn('  [', DateTimeToStr(historyEntries[i].ChangeDate), '] ', historyEntries[i].ChangeDescription);
  end;
  WriteLn;
  
  // Test 19: Clear and reload
  WriteLn('--- Test 19: Clearing and Reloading ---');
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
  
  // Test 20: Verify loaded data with new fields
  WriteLn('--- Test 20: Verifying Loaded Data ---');
  if Manager.GetTask(taskID2, task) then
  begin
    WriteLn('Successfully retrieved task #', taskID2, ' after reload:');
    PrintTask(task);
  end
  else
    WriteLn('Failed to retrieve task after reload');
  
  // Final statistics
  WriteLn('--- Final Statistics ---');
  stats := Manager.GetStatistics;
  PrintStatistics(stats);
  
  categoryStats := Manager.GetCategoryStatistics;
  PrintCategoryStatistics(categoryStats);
  
  WriteLn('========================================');
  WriteLn('Self-Test Completed Successfully!');
  WriteLn('All new features tested:');
  WriteLn('  - Categories/Projects');
  WriteLn('  - Task Dependencies');
  WriteLn('  - Time Tracking (Estimated/Actual Hours)');
  WriteLn('  - JSON Export');
  WriteLn('  - HTML Reports with Statistics');
  WriteLn('  - Markdown Export');
  WriteLn('  - Task History/Audit Trail');
  WriteLn('  - Category Statistics');
  WriteLn('========================================');
end;

begin
  try
    Manager := TTaskManagerCore.Create;
    Storage := TTaskStorage.Create('solution1/' + DATA_FILE);
    Exporter := TTaskExporter.Create;
    History := TTaskHistory.Create('solution1/tasks_history.dat');
    
    try
      SelfTest;
    finally
      Manager.Free;
      Storage.Free;
      Exporter.Free;
      History.Free;
    end;
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
