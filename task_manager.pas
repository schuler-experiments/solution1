
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
  TaggedTasks, OverdueTasks: TTaskArray;
  Yesterday: TDateTime;
begin
  WriteLn('--------------------------------------------------');
  WriteLn('Starting Task Manager Self Test (Extended)');
  WriteLn('--------------------------------------------------');

  Manager := TTaskManager.Create;
  try
    Yesterday := Now - 1;

    // Setup: Add tasks with priorities and due dates
    Manager.AddTask('Low Priority Task', 'Do later', tpLow);       // ID 1
    Manager.AddTask('High Priority Task', 'Do NOW!', tpHigh);      // ID 2
    Manager.AddTask('Overdue Task', 'Should have been done', tpMedium, Yesterday); // ID 3
    
    WriteLn('[INFO] Setup complete (3 tasks added)');

    // Test Tags
    WriteLn('[TEST] Adding tags...');
    if Manager.AddTagToTask(1, 'work') then WriteLn('[PASS] Added tag "work" to Task 1') else WriteLn('[FAIL] Failed to add tag');
    if Manager.AddTagToTask(1, 'later') then WriteLn('[PASS] Added tag "later" to Task 1') else WriteLn('[FAIL] Failed to add tag');
    if Manager.AddTagToTask(3, 'work') then WriteLn('[PASS] Added tag "work" to Task 3') else WriteLn('[FAIL] Failed to add tag');

    // Test Find by Tag
    WriteLn('[TEST] Finding tasks by tag "work"...');
    TaggedTasks := Manager.FindTasksByTag('work');
    if Length(TaggedTasks) = 2 then
      WriteLn('[PASS] Found 2 tasks with tag "work"')
    else
      WriteLn('[FAIL] Found ', Length(TaggedTasks), ' tasks with tag "work" (expected 2)');

    // Test Overdue
    WriteLn('[TEST] Checking overdue tasks...');
    OverdueTasks := Manager.GetOverdueTasks;
    if Length(OverdueTasks) = 1 then
    begin
      if OverdueTasks[0].ID = 3 then
        WriteLn('[PASS] Correctly identified overdue task (ID 3)')
      else
        WriteLn('[FAIL] Wrong overdue task identified (ID ', OverdueTasks[0].ID, ')');
    end
    else
      WriteLn('[FAIL] Found ', Length(OverdueTasks), ' overdue tasks (expected 1)');

    // Test Sort (Regression Test)
    Manager.SortTasksByPriority;
    WriteLn('[INFO] Sorted tasks by priority');
    Task := Manager.GetTask(0);
    if Task.Priority = tpHigh then
      WriteLn('[PASS] First task is High Priority')
    else
      WriteLn('[FAIL] First task priority is ', Task.Priority);

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
