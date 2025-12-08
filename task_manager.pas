
program task_manager;

{$mode objfpc}
{$H+}

uses
  SysUtils, 
  DateUtils, 
  Math,
  task_types;

procedure SelfTest;
var
  Manager: TTaskManager;
  Task: TTask;
  OverdueTasks, FoundTasks: TTaskArray;
  Yesterday: TDateTime;
  SaveFile, HTMLFile: String;
  CloneID, CompletedCount, TaskA, TaskB, TaskC: Integer;
  Stats: TTaskStats;
begin
  WriteLn('--------------------------------------------------');
  WriteLn('Starting Task Manager Self Test (Persistence + New Features)');
  WriteLn('--------------------------------------------------');

  SaveFile := 'tasks_test.db';
  HTMLFile := 'tasks_report.html';
  if FileExists(SaveFile) then DeleteFile(SaveFile);
  if FileExists(HTMLFile) then DeleteFile(HTMLFile);

  Manager := TTaskManager.Create;
  try
    Yesterday := Now - 1;

    // Setup: Add tasks
    Manager.AddTask('Low Priority Task', 'Do later', tpLow);       // ID 1
    Manager.AddTask('High Priority Task', 'Do NOW!', tpHigh);      // ID 2
    Manager.AddTask('Overdue Task', 'Should have been done', tpMedium, Yesterday); // ID 3
    Manager.AddTagToTask(1, 'work');
    Manager.AddTagToTask(1, 'later');
    Manager.AddTagToTask(2, 'urgent');
    Manager.AddTagToTask(2, 'work');
    
    WriteLn('[INFO] Setup complete (3 tasks added)');

    // --- Test New Features ---

    // 1. Test Search
    WriteLn('[TEST] Searching for "NOW"...');
    FoundTasks := Manager.SearchTasks('NOW');
    if (Length(FoundTasks) = 1) and (FoundTasks[0].ID = 2) then
      WriteLn('[PASS] Search successful')
    else
      WriteLn('[FAIL] Search failed');

    // 2. Test Clone
    WriteLn('[TEST] Cloning Task 1...');
    CloneID := Manager.CloneTask(1);
    if CloneID <> -1 then
    begin
      Task := Manager.GetTask(Manager.FindTaskByID(CloneID));
      if (Pos('(Copy)', Task.Title) > 0) and (Length(Task.Tags) = 2) then
        WriteLn('[PASS] Cloning successful (Title and Tags verified)')
      else
        WriteLn('[FAIL] Cloning verification failed');
    end
    else
      WriteLn('[FAIL] Cloning failed to return valid ID');

    // 3. Test Statistics
    WriteLn('[TEST] Checking Statistics...');
    Stats := Manager.GetTaskStatistics;
    // We have 4 tasks now (3 original + 1 clone). All pending. 1 Overdue.
    if (Stats.Total = 4) and (Stats.Pending = 4) and (Stats.Overdue = 1) then
      WriteLn('[PASS] Statistics verified')
    else
      WriteLn('[FAIL] Statistics mismatch: Total=', Stats.Total, ' Pending=', Stats.Pending, ' Overdue=', Stats.Overdue);

    // 4. Test Bulk Complete
    WriteLn('[TEST] Bulk completing tasks with tag "work"...');
    // Task 1 (work), Task 2 (work), Clone of Task 1 (work). Total 3.
    CompletedCount := Manager.CompleteTasksByTag('work');
    if CompletedCount = 3 then
      WriteLn('[PASS] Bulk complete successful (Updated 3 tasks)')
    else
      WriteLn('[FAIL] Bulk complete failed (Updated ', CompletedCount, ')');

    Stats := Manager.GetTaskStatistics;
    if Stats.Completed = 3 then
      WriteLn('[PASS] Statistics updated after bulk complete')
    else
      WriteLn('[FAIL] Statistics mismatch after complete: Completed=', Stats.Completed);

    // 5. Test Dependencies
    WriteLn('[TEST] Testing Dependencies...');
    TaskA := Manager.AddTask('Task A', 'Base task');
    TaskB := Manager.AddTask('Task B', 'Depends on A');
    TaskC := Manager.AddTask('Task C', 'Depends on B');
    
    // Add dependencies: B -> A, C -> B
    if Manager.AddDependency(TaskB, TaskA) and Manager.AddDependency(TaskC, TaskB) then
      WriteLn('[PASS] Dependencies added')
    else
      WriteLn('[FAIL] Failed to add dependencies');
      
    // Check circular
    if not Manager.AddDependency(TaskA, TaskC) then
      WriteLn('[PASS] Circular dependency correctly rejected')
    else
      WriteLn('[FAIL] Circular dependency allowed');
      
    // Check CanStart
    if Manager.CanStart(TaskA) then // No deps
      WriteLn('[PASS] Task A can start (no deps)')
    else
      WriteLn('[FAIL] Task A should be able to start');
      
    if not Manager.CanStart(TaskB) then // A is pending
      WriteLn('[PASS] Task B cannot start (A pending)')
    else
      WriteLn('[FAIL] Task B should not start');
      
    // Complete A
    Manager.UpdateTaskStatus(TaskA, tsCompleted);
    if Manager.CanStart(TaskB) then
      WriteLn('[PASS] Task B can start after A completed')
    else
      WriteLn('[FAIL] Task B should start now');
      
    // Blocked Stats
    Stats := Manager.GetTaskStatistics;
    // C is blocked by B. B is not blocked (A is done).
    if Stats.Blocked = 1 then
      WriteLn('[PASS] Blocked stats verified (1 blocked task)')
    else
      WriteLn('[FAIL] Blocked stats mismatch: ', Stats.Blocked);

    // 6. Test HTML Export
    WriteLn('[TEST] Exporting to HTML...');
    if Manager.ExportToHTML(HTMLFile) and FileExists(HTMLFile) then
      WriteLn('[PASS] HTML report generated')
    else
      WriteLn('[FAIL] HTML report generation failed');

    // 7. Test Time Tracking
    WriteLn('[TEST] Testing Time Tracking...');
    // Use Task C (ID: TaskC) which is pending/blocked but let's just use it.
    // Start Timer
    if Manager.StartTaskTimer(TaskC) then
      WriteLn('[PASS] Timer started for Task C')
    else
      WriteLn('[FAIL] Failed to start timer');
      
    Task := Manager.GetTask(Manager.FindTaskByID(TaskC));
    if (Task.IsTiming) and (Task.Status = tsInProgress) then
      WriteLn('[PASS] Task status updated to InProgress and IsTiming is True')
    else
      WriteLn('[FAIL] Task status/timing mismatch');
      
    // Simulate time passing (we can't easily sleep in self-test without delay, 
    // but we can check if GetTaskTimeSpent returns > 0 after a tiny delay or just check logic)
    // Since we can't wait seconds, we will just stop it and check logic.
    // Actually, let's force a small sleep if possible, or just trust the logic.
    // Pascal's Sleep is in SysUtils.
    Sleep(1100); // Wait 1.1 seconds
    
    if Manager.GetTaskTimeSpent(TaskC) > 1.0 then
      WriteLn('[PASS] GetTaskTimeSpent returns > 1.0s while running')
    else
      WriteLn('[FAIL] GetTaskTimeSpent failed (returned ', Manager.GetTaskTimeSpent(TaskC):0:2, ')');
      
    // Stop Timer
    if Manager.StopTaskTimer(TaskC) then
      WriteLn('[PASS] Timer stopped')
    else
      WriteLn('[FAIL] Failed to stop timer');
      
    Task := Manager.GetTask(Manager.FindTaskByID(TaskC));
    if (not Task.IsTiming) and (Task.TimeSpent >= 1.0) then
      WriteLn('[PASS] Timer stopped correctly, TimeSpent accumulated')
    else
      WriteLn('[FAIL] Timer stop verification failed');

    // --- Test Persistence (Existing Tests) ---

    // Test Save
    WriteLn('[TEST] Saving tasks to file...');
    if Manager.SaveToFile(SaveFile) then
      WriteLn('[PASS] Tasks saved to ', SaveFile)
    else
      WriteLn('[FAIL] Failed to save tasks');

    // Clear and Load
    WriteLn('[TEST] Clearing and reloading tasks...');
    Manager.ClearTasks;
    if Manager.GetTaskCount = 0 then
      WriteLn('[PASS] Tasks cleared (Count: 0)')
    else
      WriteLn('[FAIL] Tasks not cleared');

    if Manager.LoadFromFile(SaveFile) then
      WriteLn('[PASS] Tasks loaded from file')
    else
      WriteLn('[FAIL] Failed to load tasks');

    // Verify Loaded Data
    // We had 4 original + 3 new (A,B,C) = 7 tasks.
    if Manager.GetTaskCount = 7 then
      WriteLn('[PASS] Loaded 7 tasks')
    else
      WriteLn('[FAIL] Loaded ', Manager.GetTaskCount, ' tasks (expected 7)');

    // Verify Task 1 Tags
    Task := Manager.GetTask(0); // ID 1
    if (Task.Title = 'Low Priority Task') and (Length(Task.Tags) = 2) then
      WriteLn('[PASS] Task 1 verified (Title and Tags)')
    else
      WriteLn('[FAIL] Task 1 verification failed');
      
    // Verify Dependency Persistence
    // Task C (last one) should depend on Task B (second to last)
    // IDs are preserved.
    Task := Manager.GetTask(6); // Task C
    if (Length(Task.Dependencies) = 1) then
      WriteLn('[PASS] Dependency persistence verified')
    else
      WriteLn('[FAIL] Dependency persistence failed');

    // Verify Task 3 Overdue
    OverdueTasks := Manager.GetOverdueTasks;
    // Task 3 was pending and overdue. But wait, we might have completed it?
    // Task 3 tags: none. So it wasn't completed by 'work' tag.
    // So it should still be overdue.
    if (Length(OverdueTasks) > 0) then
      WriteLn('[PASS] Overdue task verified after load')
    else
      WriteLn('[FAIL] Overdue task verification failed');

  finally
    Manager.Free;
    if FileExists(SaveFile) then DeleteFile(SaveFile);
    // if FileExists(HTMLFile) then DeleteFile(HTMLFile); // Keep HTML for inspection if needed
  end;
  
  WriteLn('--------------------------------------------------');
  WriteLn('Self Test Completed');
  WriteLn('--------------------------------------------------');
end;

begin
  try
    SelfTest;
  except
    on E: Exception do
      WriteLn('Fatal Error during execution: ', E.Message);
  end;
end.
