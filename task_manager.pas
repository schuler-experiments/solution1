
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
  OverdueTasks: TTaskArray;
  Yesterday: TDateTime;
  SaveFile: String;
begin
  WriteLn('--------------------------------------------------');
  WriteLn('Starting Task Manager Self Test (Persistence)');
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
    
    WriteLn('[INFO] Setup complete (3 tasks added)');

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
    if Manager.GetTaskCount = 3 then
      WriteLn('[PASS] Loaded 3 tasks')
    else
      WriteLn('[FAIL] Loaded ', Manager.GetTaskCount, ' tasks (expected 3)');

    // Verify Task 1 Tags
    Task := Manager.GetTask(0); // ID 1
    if (Task.Title = 'Low Priority Task') and (Length(Task.Tags) = 2) then
      WriteLn('[PASS] Task 1 verified (Title and Tags)')
    else
      WriteLn('[FAIL] Task 1 verification failed');

    // Verify Task 3 Overdue
    OverdueTasks := Manager.GetOverdueTasks;
    if (Length(OverdueTasks) > 0) and (OverdueTasks[0].ID = 3) then
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
