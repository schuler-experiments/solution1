
program TaskManagerDemo;

{$mode objfpc}
{$H+}

uses
  SysUtils, DateUtils, taskmanager;

procedure SelfTest;
var
  TM: TTaskManager;
  TaskID1, TaskID2, TaskID3, TaskID4, TaskID5: Integer;
  Tasks: TTaskArray;
  i: Integer;
  CompletionRate: Double;
begin
  WriteLn('=== Task Manager Self Test ===');
  WriteLn;
  
  TM := TTaskManager.Create;
  try
    // Test 1: Adding tasks
    WriteLn('Test 1: Adding tasks...');
    TaskID1 := TM.AddTask('Implement login feature', 'Create user authentication system', 
                          tpHigh, EncodeDate(2024, 2, 15));
    TaskID2 := TM.AddTask('Write documentation', 'Document all API endpoints', 
                          tpMedium, EncodeDate(2024, 2, 20));
    TaskID3 := TM.AddTask('Fix critical bug', 'Memory leak in payment module', 
                          tpCritical, EncodeDate(2024, 1, 30));
    TaskID4 := TM.AddTask('Update dependencies', 'Upgrade all npm packages', 
                          tpLow, EncodeDate(2024, 3, 1));
    TaskID5 := TM.AddTask('Code review', 'Review pull request #42', 
                          tpHigh, EncodeDate(2024, 2, 10));
    
    WriteLn(Format('Added %d tasks successfully', [TM.TaskCount]));
    WriteLn;
    
    // Test 2: Updating task status
    WriteLn('Test 2: Updating task statuses...');
    TM.UpdateTaskStatus(TaskID1, tsInProgress);
    TM.UpdateTaskStatus(TaskID3, tsCompleted);
    TM.UpdateTaskStatus(TaskID5, tsCompleted);
    WriteLn('Task statuses updated');
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
    WriteLn('Tags added successfully');
    WriteLn;
    
    // Test 4: List all tasks
    WriteLn('Test 4: Listing all tasks...');
    Tasks := TM.GetAllTasks;
    for i := 0 to Length(Tasks) - 1 do
      WriteLn(TM.TaskToString(Tasks[i]));
    WriteLn;
    
    // Test 5: Filter by status
    WriteLn('Test 5: Filtering by status (Completed)...');
    Tasks := TM.FilterByStatus(tsCompleted);
    WriteLn(Format('Found %d completed tasks:', [Length(Tasks)]));
    for i := 0 to Length(Tasks) - 1 do
      WriteLn('  - ' + Tasks[i].Title);
    WriteLn;
    
    // Test 6: Filter by priority
    WriteLn('Test 6: Filtering by priority (High)...');
    Tasks := TM.FilterByPriority(tpHigh);
    WriteLn(Format('Found %d high priority tasks:', [Length(Tasks)]));
    for i := 0 to Length(Tasks) - 1 do
      WriteLn('  - ' + Tasks[i].Title);
    WriteLn;
    
    // Test 7: Search by title
    WriteLn('Test 7: Searching by title (keyword: "bug")...');
    Tasks := TM.SearchByTitle('bug');
    WriteLn(Format('Found %d tasks matching "bug":', [Length(Tasks)]));
    for i := 0 to Length(Tasks) - 1 do
      WriteLn('  - ' + Tasks[i].Title);
    WriteLn;
    
    // Test 8: Filter by tag
    WriteLn('Test 8: Filtering by tag ("backend")...');
    Tasks := TM.FilterByTag('backend');
    WriteLn(Format('Found %d tasks with tag "backend":', [Length(Tasks)]));
    for i := 0 to Length(Tasks) - 1 do
      WriteLn('  - ' + Tasks[i].Title);
    WriteLn;
    
    // Test 9: Statistics
    WriteLn('Test 9: Getting statistics...');
    WriteLn(Format('Total tasks: %d', [TM.TaskCount]));
    WriteLn(Format('Completed tasks: %d', [TM.GetCompletedCount]));
    WriteLn(Format('Pending tasks: %d', [TM.GetPendingCount]));
    WriteLn(Format('Overdue tasks: %d', [TM.GetOverdueCount]));
    CompletionRate := TM.GetCompletionRate;
    WriteLn(Format('Completion rate: %.2f%%', [CompletionRate]));
    WriteLn;
    
    // Test 10: Update task
    WriteLn('Test 10: Updating task title...');
    TM.UpdateTaskTitle(TaskID2, 'Write comprehensive documentation');
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
    
    // Test 11: Delete task
    WriteLn('Test 11: Deleting a task...');
    if TM.DeleteTask(TaskID4) then
      WriteLn(Format('Task %d deleted successfully. Remaining tasks: %d', [TaskID4, TM.TaskCount]))
    else
      WriteLn('Failed to delete task');
    WriteLn;
    
    // Test 12: Date range filter
    WriteLn('Test 12: Filtering by date range...');
    Tasks := TM.FilterByDateRange(EncodeDate(2024, 2, 1), EncodeDate(2024, 2, 28));
    WriteLn(Format('Found %d tasks due in February 2024:', [Length(Tasks)]));
    for i := 0 to Length(Tasks) - 1 do
      WriteLn('  - ' + Tasks[i].Title);
    WriteLn;
    
    WriteLn('=== All tests completed successfully! ===');
    
  finally
    TM.Free;
  end;
end;

begin
  SelfTest;
end.
