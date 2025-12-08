
program task_manager;

{$mode objfpc}
{$H+}

uses
  SysUtils, 
  DateUtils, 
  Math,
  task_types,
  task_json_utils,
  task_csv_utils;

procedure SelfTest;
var
  Manager: TTaskManager;
  Task: TTask;
  OverdueTasks, FoundTasks: TTaskArray;
  Yesterday: TDateTime;
  SaveFile, HTMLFile, JSONFile, CSVFile: String;
  CloneID, CompletedCount, TaskA, TaskB, TaskC, Idx: Integer;
  Stats: TTaskStats;
  UndoTaskID: Integer;
begin
  WriteLn('--------------------------------------------------');
  WriteLn('Starting Task Manager Self Test');
  WriteLn('--------------------------------------------------');

  Manager := TTaskManager.Create;
  SaveFile := 'test_tasks.dat';
  HTMLFile := 'test_tasks.html';
  JSONFile := 'test_tasks.json';
  CSVFile := 'test_tasks.csv';
  Yesterday := Now - 1;

  try
    // --- Basic CRUD ---
    WriteLn('[TEST] Adding Tasks...');
    Manager.AddTask('Buy Groceries', 'Milk, Bread, Eggs', tpMedium, Now + 1); // ID 1
    Manager.AddTask('Finish Project', 'Complete the report', tpHigh, Now + 2); // ID 2
    Manager.AddTask('Call Mom', 'Birthday wish', tpLow, Yesterday); // ID 3 (Overdue)
    
    if Manager.GetTaskCount = 3 then
      WriteLn('[PASS] Added 3 tasks')
    else
      WriteLn('[FAIL] Expected 3 tasks, found ', Manager.GetTaskCount);

    // --- Status Update ---
    WriteLn('[TEST] Updating Status...');
    Manager.UpdateTaskStatus(1, tsInProgress);
    Task := Manager.GetTask(0);
    if Task.Status = tsInProgress then
      WriteLn('[PASS] Task 1 status updated to InProgress')
    else
      WriteLn('[FAIL] Task 1 status update failed');

    // --- Tags ---
    WriteLn('[TEST] Adding Tags...');
    Manager.AddTagToTask(1, 'Personal');
    Manager.AddTagToTask(1, 'Shopping');
    Manager.AddTagToTask(2, 'Work');
    
    FoundTasks := Manager.FindTasksByTag('Personal');
    if Length(FoundTasks) = 1 then
      WriteLn('[PASS] Found 1 task with tag "Personal"')
    else
      WriteLn('[FAIL] Tag search failed');

    // --- Dependencies ---
    WriteLn('[TEST] Testing Dependencies...');
    TaskA := Manager.AddTask('Task A', 'Base task'); // ID 4
    TaskB := Manager.AddTask('Task B', 'Depends on A'); // ID 5
    TaskC := Manager.AddTask('Task C', 'Depends on B'); // ID 6
    
    Manager.AddDependency(TaskB, TaskA); // B depends on A
    Manager.AddDependency(TaskC, TaskB); // C depends on B
    
    if not Manager.CanStart(TaskB) then
      WriteLn('[PASS] Task B cannot start (blocked by A)')
    else
      WriteLn('[FAIL] Task B should be blocked');
      
    WriteLn('DEBUG: Calling UpdateTaskStatus...');
    Manager.UpdateTaskStatus(TaskA, tsCompleted);
    WriteLn('DEBUG: UpdateTaskStatus returned.');
    if Manager.CanStart(TaskB) then
      WriteLn('[PASS] Task B can start (A completed)')
    else
      WriteLn('[FAIL] Task B should be startable');

    // --- Undo/Redo ---
    WriteLn('[TEST] Testing Undo/Redo...');
    UndoTaskID := Manager.AddTask('Undo Me', 'To be deleted'); // ID 7
    Manager.DeleteTask(UndoTaskID);
    
    if Manager.FindTaskByID(UndoTaskID) = -1 then
      WriteLn('[PASS] Task deleted')
    else
      WriteLn('[FAIL] Task deletion failed');
      
    if Manager.Undo then
    begin
      if Manager.FindTaskByID(UndoTaskID) <> -1 then
        WriteLn('[PASS] Undo successful (Task restored)')
      else
        WriteLn('[FAIL] Undo failed (Task not restored)');
    end
    else
      WriteLn('[FAIL] Undo operation returned false');

    // --- Search ---
    WriteLn('[TEST] Testing Search...');
    FoundTasks := Manager.SearchTasks('Groceries');
    if Length(FoundTasks) = 1 then
      WriteLn('[PASS] Search "Groceries" found 1 task')
    else
      WriteLn('[FAIL] Search failed');

    // --- Stats ---
    WriteLn('[TEST] Testing Statistics...');
    Stats := Manager.GetTaskStatistics;
    WriteLn('Total: ', Stats.Total, ', Completed: ', Stats.Completed, ', Overdue: ', Stats.Overdue);
    // 1(InProg) + 2(Pending) + 3(Pending,OD) + 4(Comp) + 5(Pending) + 6(Pending) + 7(Pending) = 7 tasks?
    // Let's recount:
    // 1: InProgress
    // 2: Pending
    // 3: Pending (Overdue)
    // 4: Completed
    // 5: Pending
    // 6: Pending
    // 7: Pending
    // Total 7.
    if Stats.Total = 7 then
      WriteLn('[PASS] Total count correct')
    else
      WriteLn('[FAIL] Total count incorrect');

    // --- Clone ---
    WriteLn('[TEST] Testing Clone...');
    CloneID := Manager.CloneTask(1); // Clones Task 1 (ID 1) -> New ID 8
    Task := Manager.GetTask(Manager.FindTaskByID(CloneID));
    if (Task.Title = 'Buy Groceries (Copy)') and (Length(Task.Tags) = 2) then
      WriteLn('[PASS] Task cloned successfully')
    else
      WriteLn('[FAIL] Task clone failed');

    // --- HTML Export ---
    WriteLn('[TEST] Exporting to HTML...');
    if Manager.ExportToHTML(HTMLFile) then
      WriteLn('[PASS] HTML exported to ', HTMLFile)
    else
      WriteLn('[FAIL] HTML export failed');

    // --- Persistence (Binary) ---
    WriteLn('[TEST] Saving to file...');
    if Manager.SaveToFile(SaveFile) then
      WriteLn('[PASS] Saved to ', SaveFile)
    else
      WriteLn('[FAIL] Save failed');
      
    WriteLn('[TEST] Clearing and Loading from file...');
    Manager.ClearTasks;
    if Manager.LoadFromFile(SaveFile) then
      WriteLn('[PASS] Loaded from ', SaveFile)
    else
      WriteLn('[FAIL] Load failed');
      
    if Manager.GetTaskCount = 8 then
      WriteLn('[PASS] Loaded 8 tasks')
    else
      WriteLn('[FAIL] Loaded ', Manager.GetTaskCount, ' tasks (expected 8)');

    // Verify Task 6 Dependencies
    Task := Manager.GetTask(6); // Task C (Index might vary if not sorted, but usually append)
    // Actually GetTask uses index. 
    // Indices: 0->1, 1->2, 2->3, 3->4, 4->5, 5->6, 6->7, 7->8
    // Task C was ID 6. Let's find it by ID to be safe.
    // Wait, GetTask(6) is 7th task.
    // Let's use FindTaskByID for verification.
    
    // --- Test JSON Persistence ---
    WriteLn('[TEST] Exporting tasks to JSON...');
    if ExportTasksToJSON(Manager, JSONFile) then
      WriteLn('[PASS] Tasks exported to ', JSONFile)
    else
      WriteLn('[FAIL] Failed to export tasks to JSON');
      
    WriteLn('[TEST] Clearing and importing tasks from JSON...');
    Manager.ClearTasks;
    if ImportTasksFromJSON(Manager, JSONFile) then
      WriteLn('[PASS] Tasks imported from JSON')
    else
      WriteLn('[FAIL] Failed to import tasks from JSON');
      
    if Manager.GetTaskCount = 8 then
      WriteLn('[PASS] Loaded 8 tasks from JSON')
    else
      WriteLn('[FAIL] Loaded ', Manager.GetTaskCount, ' tasks from JSON (expected 8)');

    // --- Test CSV Persistence ---
    WriteLn('[TEST] Exporting tasks to CSV...');
    if ExportTasksToCSV(Manager, CSVFile) then
      WriteLn('[PASS] Tasks exported to ', CSVFile)
    else
      WriteLn('[FAIL] Failed to export tasks to CSV');
      
    WriteLn('[TEST] Clearing and importing tasks from CSV...');
    Manager.ClearTasks;
    if ImportTasksFromCSV(Manager, CSVFile) then
      WriteLn('[PASS] Tasks imported from CSV')
    else
      WriteLn('[FAIL] Failed to import tasks from CSV');
      
    if Manager.GetTaskCount = 8 then
      WriteLn('[PASS] Loaded 8 tasks from CSV')
    else
      WriteLn('[FAIL] Loaded ', Manager.GetTaskCount, ' tasks from CSV (expected 8)');
      
    // Verify Task 1 Tags again from CSV
    // Task 1 should be the first one if order preserved
    Task := Manager.GetTask(0); // ID 1
    if (Task.Title = 'Buy Groceries') and (Length(Task.Tags) = 2) then
      WriteLn('[PASS] Task 1 verified from CSV (Title and Tags)')
    else
      WriteLn('[FAIL] Task 1 verification failed from CSV. Title: ', Task.Title, ', Tags: ', Length(Task.Tags));

  finally
    Manager.Free;
    if FileExists(SaveFile) then DeleteFile(SaveFile);
    if FileExists(JSONFile) then DeleteFile(JSONFile);

    // --- Recurrence ---
    WriteLn('[TEST] Testing Recurrence...');
    // Add task with 1 day recurrence
    TaskA := Manager.AddTask('Daily Standup', 'Zoom link', tpHigh, Now, 1);
    WriteLn('DEBUG: TaskA ID: ', TaskA);
    
    WriteLn('DEBUG: Calling UpdateTaskStatus...');
    Manager.UpdateTaskStatus(TaskA, tsCompleted);
    WriteLn('DEBUG: UpdateTaskStatus returned.');
    
    Idx := Manager.FindTaskByID(TaskA);
    WriteLn('DEBUG: TaskA Index after update: ', Idx);
    
    if Idx <> -1 then
    begin
      Task := Manager.GetTask(Idx);
      if Task.Status = tsCompleted then
        WriteLn('[PASS] Recurring task completed')
      else
        WriteLn('[FAIL] Recurring task not completed. Status: ', StatusToString(Task.Status));
    end
    else
      WriteLn('[FAIL] TaskA lost after update');
      
    // Check if new task created
    if Manager.GetTaskCount > 0 then
    begin
      Task := Manager.GetTask(Manager.GetTaskCount - 1);
      WriteLn('DEBUG: Last Task Title: ', Task.Title);
      WriteLn('DEBUG: Last Task DueDate: ', Task.DueDate);
      
      if (Task.Title = 'Daily Standup') and (Task.Status = tsPending) and (Abs(Task.DueDate - (Now + 1)) < 0.1) then
        WriteLn('[PASS] New recurring instance created')
      else
        WriteLn('[FAIL] New recurring instance failed.');
    end;
    if FileExists(CSVFile) then DeleteFile(CSVFile);
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
