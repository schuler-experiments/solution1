
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
  SaveFile: String;
  CloneID, CompletedCount: Integer;
  Stats: TTaskStats;
begin
  WriteLn('--------------------------------------------------');
  WriteLn('Starting Task Manager Self Test (Persistence + New Features)');
  WriteLn('--------------------------------------------------');

  SaveFile := 'tasks_test.db';
  if FileExists(SaveFile) then DeleteFile(SaveFile);

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
    // We had 4 tasks before save.
    if Manager.GetTaskCount = 4 then
      WriteLn('[PASS] Loaded 4 tasks')
    else
      WriteLn('[FAIL] Loaded ', Manager.GetTaskCount, ' tasks (expected 4)');

    // Verify Task 1 Tags
    Task := Manager.GetTask(0); // ID 1
    if (Task.Title = 'Low Priority Task') and (Length(Task.Tags) = 2) then
      WriteLn('[PASS] Task 1 verified (Title and Tags)')
    else
      WriteLn('[FAIL] Task 1 verification failed');

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
