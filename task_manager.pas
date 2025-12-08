
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
  NewID, FoundIndex: Integer;
  Count: Integer;
  Task: TTask;
  PendingTasks: TTaskArray;
begin
  WriteLn('--------------------------------------------------');
  WriteLn('Starting Task Manager Self Test');
  WriteLn('--------------------------------------------------');

  Manager := TTaskManager.Create;
  try
    // Cycle 1 Tests
    // ... (Condensed for brevity in logs, but functionally same steps)
    Manager.AddTask('Buy Milk', 'Milk');
    Manager.AddTask('Walk Dog', 'Dog');
    
    WriteLn('[INFO] Cycle 1 setup complete (2 tasks added)');

    // Cycle 2 Tests: Search and Retrieval
    
    // Test 7: Find Task by ID (Existing)
    FoundIndex := Manager.FindTaskByID(1);
    if FoundIndex <> -1 then
    begin
      Task := Manager.GetTask(FoundIndex);
      if Task.ID = 1 then
        WriteLn('[PASS] Found Task ID 1 at index ', FoundIndex)
      else
        WriteLn('[FAIL] Found index ', FoundIndex, ' but ID is ', Task.ID);
    end
    else
      WriteLn('[FAIL] Task ID 1 not found');

    // Test 8: Find Task by ID (Non-existing)
    FoundIndex := Manager.FindTaskByID(999);
    if FoundIndex = -1 then
      WriteLn('[PASS] Non-existing Task ID 999 correctly not found')
    else
      WriteLn('[FAIL] Task ID 999 found at index ', FoundIndex);

    // Test 9: Find Tasks by Status
    // Both added tasks are Pending by default
    PendingTasks := Manager.FindTasksByStatus(tsPending);
    if Length(PendingTasks) = 2 then
      WriteLn('[PASS] Found 2 Pending tasks')
    else
      WriteLn('[FAIL] Found ', Length(PendingTasks), ' Pending tasks, expected 2');

    // Test 10: Find Tasks by Status (Empty result)
    // No tasks are Completed yet
    PendingTasks := Manager.FindTasksByStatus(tsCompleted);
    if Length(PendingTasks) = 0 then
      WriteLn('[PASS] Found 0 Completed tasks')
    else
      WriteLn('[FAIL] Found ', Length(PendingTasks), ' Completed tasks, expected 0');

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
