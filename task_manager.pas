
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
  PendingTasks: TTaskArray;
  Success: Boolean;
begin
  WriteLn('--------------------------------------------------');
  WriteLn('Starting Task Manager Self Test');
  WriteLn('--------------------------------------------------');

  Manager := TTaskManager.Create;
  try
    // Cycle 1 & 2 Setup
    Manager.AddTask('Buy Milk', 'Milk'); // ID 1
    Manager.AddTask('Walk Dog', 'Dog');  // ID 2
    WriteLn('[INFO] Setup complete (2 tasks added)');

    // Cycle 3 Tests: Update and Delete

    // Test 11: Update Task Status
    Success := Manager.UpdateTaskStatus(1, tsCompleted);
    if Success then
    begin
      Task := Manager.GetTask(Manager.FindTaskByID(1));
      if Task.Status = tsCompleted then
        WriteLn('[PASS] Task ID 1 updated to Completed')
      else
        WriteLn('[FAIL] Task ID 1 status is ', Task.Status);
    end
    else
      WriteLn('[FAIL] Failed to update Task ID 1');

    // Test 12: Verify Status Filter after Update
    PendingTasks := Manager.FindTasksByStatus(tsPending);
    if Length(PendingTasks) = 1 then
      WriteLn('[PASS] Found 1 Pending task after update')
    else
      WriteLn('[FAIL] Found ', Length(PendingTasks), ' Pending tasks, expected 1');

    // Test 13: Delete Task
    Success := Manager.DeleteTask(1); // Delete 'Buy Milk'
    if Success then
    begin
      if Manager.FindTaskByID(1) = -1 then
        WriteLn('[PASS] Task ID 1 deleted and not found')
      else
        WriteLn('[FAIL] Task ID 1 still exists after deletion');
    end
    else
      WriteLn('[FAIL] Failed to delete Task ID 1');

    // Test 14: Verify Count after Deletion
    if Manager.GetTaskCount = 1 then
      WriteLn('[PASS] Task count is 1 after deletion')
    else
      WriteLn('[FAIL] Task count is ', Manager.GetTaskCount);
      
    // Test 15: Delete Non-existing Task
    Success := Manager.DeleteTask(999);
    if not Success then
      WriteLn('[PASS] Correctly failed to delete non-existing task')
    else
      WriteLn('[FAIL] Reported success deleting non-existing task');

  finally
    Manager.Free;
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
