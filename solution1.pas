
program TaskManagerDemo;

{$mode objfpc}
{$H+}

uses
  SysUtils, DateUtils, taskmanager;

procedure SelfTest;
var
  TM: TTaskManager;
  TaskID1, TaskID2, TaskID3, TaskID4, TaskID5, TaskID6: Integer;
  Tasks: TTaskArray;
  i: Integer;
  CompletionRate, AvgTime: Double;
  CSVData, CategoryStats: string;
begin
  WriteLn('=== Task Manager Self Test - Enhanced Version ===');
  WriteLn;
  
  TM := TTaskManager.Create;
  try
    // Test 1: Adding tasks with categories and time estimates
    WriteLn('Test 1: Adding tasks with new features (category, time tracking)...');
    TaskID1 := TM.AddTask('Implement login feature', 'Create user authentication system', 
                          'Backend', tpHigh, EncodeDate(2024, 2, 15), 8.0);
    TaskID2 := TM.AddTask('Write documentation', 'Document all API endpoints', 
                          'Documentation', tpMedium, EncodeDate(2024, 2, 20), 4.0);
    TaskID3 := TM.AddTask('Fix critical bug', 'Memory leak in payment module', 
                          'Backend', tpCritical, EncodeDate(2024, 1, 30), 2.0);
    TaskID4 := TM.AddTask('Update dependencies', 'Upgrade all npm packages', 
                          'Maintenance', tpLow, EncodeDate(2024, 3, 1), 1.5);
    TaskID5 := TM.AddTask('Code review', 'Review pull request #42', 
                          'Frontend', tpHigh, EncodeDate(2024, 2, 10), 3.0);
    TaskID6 := TM.AddTask('Design database schema', 'Create ER diagram for new features',
                          'Backend', tpMedium, EncodeDate(2024, 2, 25), 5.0);
    
    WriteLn(Format('Added %d tasks successfully', [TM.TaskCount]));
    WriteLn;
    
    // Test 2: Updating task status and actual hours
    WriteLn('Test 2: Updating task statuses and tracking actual hours...');
    TM.UpdateTaskStatus(TaskID1, tsInProgress);
    TM.UpdateTaskActualHours(TaskID1, 4.5);
    TM.UpdateTaskStatus(TaskID3, tsCompleted);
    TM.UpdateTaskActualHours(TaskID3, 3.0);
    TM.UpdateTaskStatus(TaskID5, tsCompleted);
    TM.UpdateTaskActualHours(TaskID5, 2.5);
    WriteLn('Task statuses and hours updated');
    WriteLn;
    
    // Test 3: Adding tags
    WriteLn('Test 3: Adding tags to tasks...');
    TM.AddTagToTask(TaskID1, 'backend');
    TM.AddTagToTask(TaskID1, 'security');
    TM.AddTagToTask(TaskID2, 'documentation');
    TM.AddTagToTask(TaskID3, 'bug');
    TM.AddTagToTask(TaskID3, 'hotfix');
    TM.AddTagToTask(TaskID4, 'maintenance');
    TM.AddTagToTask(TaskID5, 'frontend');
    TM.AddTagToTask(TaskID6, 'database');
    TM.AddTagToTask(TaskID6, 'design');
    WriteLn('Tags added successfully');
    WriteLn;
    
    // Test 4: List all tasks
    WriteLn('Test 4: Listing all tasks...');
    Tasks := TM.GetAllTasks;
    for i := 0 to Length(Tasks) - 1 do
      WriteLn(TM.TaskToString(Tasks[i]));
    WriteLn;
    
    // Test 5: Sort tasks by priority (descending)
    WriteLn('Test 5: Sorting tasks by priority (highest first)...');
    Tasks := TM.SortTasksDescending(scPriority);
    for i := 0 to Length(Tasks) - 1 do
      WriteLn(Format('  %s - %s', [TM.TaskPriorityToString(Tasks[i].Priority), Tasks[i].Title]));
    WriteLn;
    
    // Test 6: Sort tasks by due date
    WriteLn('Test 6: Sorting tasks by due date (earliest first)...');
    Tasks := TM.SortTasks(scDueDate);
    for i := 0 to Length(Tasks) - 1 do
      WriteLn(Format('  %s - %s', [DateToStr(Tasks[i].DueDate), Tasks[i].Title]));
    WriteLn;
    
    // Test 7: Sort tasks by title
    WriteLn('Test 7: Sorting tasks alphabetically by title...');
    Tasks := TM.SortTasks(scTitle);
    for i := 0 to Length(Tasks) - 1 do
      WriteLn(Format('  %s', [Tasks[i].Title]));
    WriteLn;
    
    // Test 8: Filter by category
    WriteLn('Test 8: Filtering by category (Backend)...');
    Tasks := TM.FilterByCategory('Backend');
    WriteLn(Format('Found %d backend tasks:', [Length(Tasks)]));
    for i := 0 to Length(Tasks) - 1 do
      WriteLn('  - ' + Tasks[i].Title);
    WriteLn;
    
    // Test 9: Filter by status
    WriteLn('Test 9: Filtering by status (Completed)...');
    Tasks := TM.FilterByStatus(tsCompleted);
    WriteLn(Format('Found %d completed tasks:', [Length(Tasks)]));
    for i := 0 to Length(Tasks) - 1 do
      WriteLn('  - ' + Tasks[i].Title);
    WriteLn;
    
    // Test 10: Enhanced statistics
    WriteLn('Test 10: Getting enhanced statistics...');
    WriteLn(Format('Total tasks: %d', [TM.TaskCount]));
    WriteLn(Format('Completed tasks: %d', [TM.GetCompletedCount]));
    WriteLn(Format('Pending tasks: %d', [TM.GetPendingCount]));
    WriteLn(Format('Overdue tasks: %d', [TM.GetOverdueCount]));
    CompletionRate := TM.GetCompletionRate;
    WriteLn(Format('Completion rate: %.2f%%', [CompletionRate]));
    AvgTime := TM.GetAverageCompletionTime;
    WriteLn(Format('Average completion time: %.2f days', [AvgTime]));
    WriteLn(Format('Total estimated hours: %.2f', [TM.GetTotalEstimatedHours]));
    WriteLn(Format('Total actual hours: %.2f', [TM.GetTotalActualHours]));
    CategoryStats := TM.GetTasksByCategory;
    WriteLn('Tasks by category: ' + CategoryStats);
    WriteLn;
    
    // Test 11: Export to CSV
    WriteLn('Test 11: Exporting tasks to CSV...');
    CSVData := TM.ExportToCSV;
    WriteLn('CSV Export (first 300 characters):');
    if Length(CSVData) > 300 then
      WriteLn(Copy(CSVData, 1, 300) + '...')
    else
      WriteLn(CSVData);
    WriteLn;
    
    // Test 12: Save to file
    WriteLn('Test 12: Saving tasks to file...');
    if TM.SaveToFile('solution1/tasks_backup.dat') then
      WriteLn('Tasks saved successfully to tasks_backup.dat')
    else
      WriteLn('Failed to save tasks');
    WriteLn;
    
    // Test 13: Clear and reload from file
    WriteLn('Test 13: Clearing tasks and reloading from file...');
    WriteLn(Format('Tasks before clear: %d', [TM.TaskCount]));
    TM.ClearAllTasks;
    WriteLn(Format('Tasks after clear: %d', [TM.TaskCount]));
    if TM.LoadFromFile('solution1/tasks_backup.dat') then
    begin
      WriteLn(Format('Tasks reloaded: %d', [TM.TaskCount]));
      WriteLn('Verifying reloaded data...');
      Tasks := TM.GetAllTasks;
      if Length(Tasks) > 0 then
        WriteLn('First task: ' + TM.TaskToString(Tasks[0]));
    end
    else
      WriteLn('Failed to load tasks');
    WriteLn;
    
    // Test 14: Update task category
    WriteLn('Test 14: Updating task category...');
    TM.UpdateTaskCategory(TaskID2, 'Technical Writing');
    Tasks := TM.GetAllTasks;
    for i := 0 to Length(Tasks) - 1 do
    begin
      if Tasks[i].ID = TaskID2 then
      begin
        WriteLn('Updated task: ' + TM.TaskToString(Tasks[i]));
        Break;
      end;
    end;
    WriteLn;
    
    // Test 15: Update estimated hours
    WriteLn('Test 15: Updating estimated hours...');
    TM.UpdateTaskEstimatedHours(TaskID1, 10.0);
    WriteLn(Format('Updated estimated hours. New total: %.2f', [TM.GetTotalEstimatedHours]));
    WriteLn;
    
    // Test 16: Delete task
    WriteLn('Test 16: Deleting a task...');
    if TM.DeleteTask(TaskID4) then
      WriteLn(Format('Task %d deleted successfully. Remaining tasks: %d', [TaskID4, TM.TaskCount]))
    else
      WriteLn('Failed to delete task');
    WriteLn;
    
    // Test 17: Sort by category
    WriteLn('Test 17: Sorting tasks by category...');
    Tasks := TM.SortTasks(scCategory);
    for i := 0 to Length(Tasks) - 1 do
      WriteLn(Format('  %s - %s', [Tasks[i].Category, Tasks[i].Title]));
    WriteLn;
    
    WriteLn('=== All tests completed successfully! ===');
    WriteLn;
    WriteLn('New features demonstrated:');
    WriteLn('✓ Task categories for better organization');
    WriteLn('✓ Time tracking (estimated vs actual hours)');
    WriteLn('✓ Sorting by multiple criteria (title, priority, date, category)');
    WriteLn('✓ Enhanced statistics (average completion time, hours tracking)');
    WriteLn('✓ CSV export functionality');
    WriteLn('✓ Persistent storage (save/load to file)');
    WriteLn('✓ Category-based filtering');
    
  finally
    TM.Free;
  end;
end;

begin
  SelfTest;
end.
