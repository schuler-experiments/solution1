
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
begin
  WriteLn('--------------------------------------------------');
  WriteLn('Starting Task Manager Self Test');
  WriteLn('--------------------------------------------------');

  Manager := TTaskManager.Create;
  try
    // Cycle 4 Setup: Add tasks with priorities
    Manager.AddTask('Low Priority Task', 'Do later', tpLow);       // ID 1
    Manager.AddTask('High Priority Task', 'Do NOW!', tpHigh);      // ID 2
    Manager.AddTask('Medium Priority Task', 'Do soon', tpMedium);  // ID 3
    
    WriteLn('[INFO] Setup complete (3 tasks added with mixed priorities)');

    // Test 16: Verify Initial Order (Insertion Order)
    Task := Manager.GetTask(0);
    if Task.Priority = tpLow then
      WriteLn('[PASS] Initial first task is Low Priority (Insertion Order)')
    else
      WriteLn('[FAIL] Initial first task priority is ', Task.Priority);

    // Test 17: Sort by Priority
    Manager.SortTasksByPriority;
    WriteLn('[INFO] Sorted tasks by priority');

    // Test 18: Verify New Order (High -> Medium -> Low)
    
    // First task should be High
    Task := Manager.GetTask(0);
    if Task.Priority = tpHigh then
      WriteLn('[PASS] First task is now High Priority')
    else
      WriteLn('[FAIL] First task priority is ', Task.Priority);

    // Second task should be Medium
    Task := Manager.GetTask(1);
    if Task.Priority = tpMedium then
      WriteLn('[PASS] Second task is now Medium Priority')
    else
      WriteLn('[FAIL] Second task priority is ', Task.Priority);

    // Third task should be Low
    Task := Manager.GetTask(2);
    if Task.Priority = tpLow then
      WriteLn('[PASS] Third task is now Low Priority')
    else
      WriteLn('[FAIL] Third task priority is ', Task.Priority);

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
