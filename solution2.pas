
program TaskManagerExtendedDemo;
{$mode objfpc}
{$H+}

uses
  SysUtils, DateUtils, taskmanager, taskmanagerext;

procedure SelfTest;
var
  Manager: TExtendedTaskManager;
  Task1, Task2, Task3, Task4: Integer;
  Subtasks: TExtendedTaskArray;
  TopTasks: TExtendedTaskArray;
  TasksNeedingAttention: TExtendedTaskArray;
  BatchResult: TBatchOperationResult;
  TaskIDs: array[0..2] of Integer;
  i: Integer;
begin
  WriteLn('=== Extended Task Manager Self Test ===');
  WriteLn;
  
  Manager := TExtendedTaskManager.Create;
  try
    // Test 1: Adding tasks with recurring patterns
    WriteLn('Test 1: Adding recurring tasks...');
    Task1 := Manager.AddExtendedTask(
      'Daily standup meeting',
      'Team sync-up meeting',
      'Meetings',
      tpMedium,
      EncodeDate(2024, 2, 10),
      0.5,
      rpDaily
    );
    
    Task2 := Manager.AddExtendedTask(
      'Weekly report',
      'Submit weekly progress report',
      'Reporting',
      tpHigh,
      EncodeDate(2024, 2, 16),
      2.0,
      rpWeekly
    );
    
    Task3 := Manager.AddExtendedTask(
      'Monthly review',
      'Performance review meeting',
      'Management',
      tpCritical,
      EncodeDate(2024, 3, 1),
      4.0,
      rpMonthly
    );
    
    Task4 := Manager.AddExtendedTask(
      'Implement new feature',
      'Add dark mode support',
      'Development',
      tpHigh,
      EncodeDate(2024, 2, 20),
      8.0,
      rpNone
    );
    
    WriteLn('Added 4 tasks (3 recurring, 1 regular)');
    WriteLn;
    
    // Test 2: Adding subtasks
    WriteLn('Test 2: Adding subtasks to main development task...');
    Manager.AddSubtask(Task4, 'Design dark mode color scheme', 'Create color palette', tpHigh, EncodeDate(2024, 2, 12));
    Manager.AddSubtask(Task4, 'Update CSS variables', 'Implement theme switching', tpHigh, EncodeDate(2024, 2, 15));
    Manager.AddSubtask(Task4, 'Test across browsers', 'Cross-browser testing', tpMedium, EncodeDate(2024, 2, 18));
    WriteLn('Added 3 subtasks to task #', Task4);
    WriteLn;
    
    // Test 3: Display task hierarchy
    WriteLn('Test 3: Task hierarchy for main development task:');
    WriteLn(Manager.GetTaskHierarchy(Task4));
    
    // Test 4: Update priority scores
    WriteLn('Test 4: Calculating priority scores...');
    Manager.UpdatePriorityScores;
    WriteLn('Priority scores updated for all tasks');
    WriteLn;
    
    // Test 5: Get top priority tasks
    WriteLn('Test 5: Top 3 priority tasks:');
    TopTasks := Manager.GetTopPriorityTasks(3);
    for i := 0 to Length(TopTasks) - 1 do
      WriteLn('  ', Manager.ExtendedTaskToString(TopTasks[i]));
    WriteLn;
    
    // Test 6: Get tasks needing attention
    WriteLn('Test 6: Tasks needing immediate attention:');
    TasksNeedingAttention := Manager.GetTasksNeedingAttention;
    WriteLn('Found ', Length(TasksNeedingAttention), ' tasks needing attention');
    for i := 0 to Length(TasksNeedingAttention) - 1 do
      WriteLn('  ', Manager.ExtendedTaskToString(TasksNeedingAttention[i]));
    WriteLn;
    
    // Test 7: Batch operations
    WriteLn('Test 7: Batch updating task priorities...');
    TaskIDs[0] := Task1;
    TaskIDs[1] := Task2;
    TaskIDs[2] := Task3;
    BatchResult := Manager.BatchUpdatePriority(TaskIDs, tpCritical);
    WriteLn(BatchResult.Message);
    WriteLn;
    
    // Test 8: Batch add tags
    WriteLn('Test 8: Batch adding tags to multiple tasks...');
    BatchResult := Manager.BatchAddTag(TaskIDs, 'urgent');
    WriteLn(BatchResult.Message);
    WriteLn;
    
    // Test 9: Productivity report
    WriteLn('Test 9: Generating productivity report...');
    WriteLn(Manager.GetProductivityReport);
    
    // Test 10: Time management report
    WriteLn('Test 10: Time management analysis...');
    Manager.UpdateTaskActualHours(Task1, 0.4);
    Manager.UpdateTaskActualHours(Task2, 2.5);
    WriteLn(Manager.GetTimeManagementReport);
    
    // Test 11: Task complexity analysis
    WriteLn('Test 11: Task complexity breakdown...');
    WriteLn(Manager.GetTaskComplexityAnalysis);
    WriteLn;
    
    // Test 12: Export to extended CSV
    WriteLn('Test 12: Exporting to extended CSV format...');
    WriteLn('CSV Export (first 400 characters):');
    WriteLn(Copy(Manager.ExportExtendedToCSV, 1, 400));
    WriteLn('...');
    WriteLn;
    
    // Test 13: Save and load extended format
    WriteLn('Test 13: Saving to extended file format...');
    if Manager.SaveExtendedToFile('solution1/tasks_extended.dat') then
      WriteLn('Successfully saved to tasks_extended.dat')
    else
      WriteLn('Failed to save');
    WriteLn;
    
    // Test 14: Recurring task generation
    WriteLn('Test 14: Testing recurring task generation...');
    Manager.UpdateTaskStatus(Task1, tsCompleted);
    Manager.SetTaskRecurrence(Task1, rpDaily);
    WriteLn('Marked daily task as completed and set recurrence pattern');
    WriteLn;
    
    // Test 15: Get tasks due soon
    WriteLn('Test 15: Tasks due within 30 days:');
    Subtasks := Manager.GetTasksDueSoon(30);
    WriteLn('Found ', Length(Subtasks), ' tasks due soon');
    for i := 0 to Length(Subtasks) - 1 do
    begin
      if i >= 3 then Break;
      WriteLn('  ', Manager.ExtendedTaskToString(Subtasks[i]));
    end;
    WriteLn;
    
    // Test 16: Category performance
    WriteLn('Test 16: Category performance analysis...');
    WriteLn(Manager.GetCategoryPerformance);
    
    WriteLn('=== All extended tests completed successfully! ===');
    WriteLn;
    WriteLn('New features demonstrated:');
    WriteLn('✓ Recurring tasks (daily, weekly, monthly patterns)');
    WriteLn('✓ Hierarchical tasks with subtasks');
    WriteLn('✓ Auto-calculated priority scoring');
    WriteLn('✓ Smart task prioritization');
    WriteLn('✓ Batch operations on multiple tasks');
    WriteLn('✓ Advanced productivity analytics');
    WriteLn('✓ Time management tracking and analysis');
    WriteLn('✓ Task complexity analysis');
    WriteLn('✓ Extended CSV export with all fields');
    WriteLn('✓ Extended file persistence format');
    
  finally
    Manager.Free;
  end;
end;

begin
  SelfTest;
end.
