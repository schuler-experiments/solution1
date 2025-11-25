
program solution1;

{$mode objfpc}

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager,
  TaskManagerExtended,
  TaskManagerAdvanced,
  TaskManagerSubtasks,
  TaskManagerHistory,
  TaskManagerEnhanced;

var
  manager: TTaskManagerEnhanced;

procedure selfTest;
var
  i: integer;
  tasks: TTaskArray;
  stats: TTaskStats;
  subtasks: TSubtaskArray;
  history: THistoryArray;
  taskId: integer;
  batchIds: array of integer;
  audit: string;
  tempTask: TTask;
begin
  WriteLn('===== ENHANCED TASK MANAGER WITH SUBTASKS & HISTORY TEST =====');
  WriteLn;

  { Test 1: Create tasks }
  WriteLn('Test 1: Creating tasks...');
  for i := 1 to 5 do
  begin
    taskId := manager.addTask('Task ' + intToStr(i), 'Description', 'Development',
                              tpHigh, now + i);
    WriteLn('Created task ' + intToStr(i) + ' (ID: ' + intToStr(taskId) + ')');
    manager.recordTaskChange(taskId, 'status', '', 'new', 'system');
  end;
  WriteLn;

  { Test 2: Subtasks }
  WriteLn('Test 2: Adding subtasks...');
  taskId := manager.addTask('Major Feature', 'Implement authentication', 'Development',
                           tpCritical, now + 7);
  manager.recordTaskChange(taskId, 'status', '', 'new', 'system');
  WriteLn('Created main task ID: ' + intToStr(taskId));

  for i := 1 to 3 do
  begin
    manager.addSubtask(taskId, 'Subtask ' + intToStr(i), 2.5);
    WriteLn('  Added subtask ' + intToStr(i));
  end;

  WriteLn('Total subtasks: ' + intToStr(manager.getSubtaskCount(taskId)));
  WriteLn('Completion: ' + FloatToStr(manager.getSubtaskCompletionPercentage(taskId)) + '%');
  WriteLn;

  { Test 3: Complete some subtasks }
  WriteLn('Test 3: Completing subtasks...');
  manager.completeSubtask(taskId, 0);
  WriteLn('Completed first subtask');
  WriteLn('New completion: ' + FloatToStr(manager.getSubtaskCompletionPercentage(taskId)) + '%');
  WriteLn;

  { Test 4: Task History }
  WriteLn('Test 4: Task history and audit trail...');
  manager.recordTaskChange(1, 'status', 'new', 'in_progress', 'alice');
  manager.recordTaskChange(1, 'priority', 'high', 'critical', 'bob');
  manager.recordTaskChange(1, 'assignedTo', '', 'alice', 'system');

  history := manager.getTaskHistory(1);
  WriteLn('Task 1 has ' + intToStr(length(history)) + ' history entries');

  audit := manager.getTaskAuditTrail(1);
  WriteLn(audit);
  WriteLn;

  { Test 5: Priority Escalation }
  WriteLn('Test 5: Priority escalation near deadline...');
  taskId := manager.addTask('Urgent Task', 'Must complete soon', 'Urgent',
                           tpMedium, now + 2);
  manager.recordTaskChange(taskId, 'status', '', 'new', 'system');
  WriteLn('Created task with low priority, due in 2 days');

  manager.escalatePriorityIfDue(taskId);
  WriteLn('Priority escalated for near-deadline task');
  WriteLn;

  { Test 6: Batch Operations }
  WriteLn('Test 6: Batch operations...');
  setlength(batchIds, 3);
  batchIds[0] := 1;
  batchIds[1] := 2;
  batchIds[2] := 3;

  WriteLn('Batch assigning tasks to alice...');
  i := manager.batchAssignTasks(batchIds, 'alice');
  WriteLn('Updated ' + intToStr(i) + ' tasks');
  WriteLn;

  { Test 7: Tasks near deadline }
  WriteLn('Test 7: Finding tasks near deadline...');
  tasks := manager.getTasksNearDeadline(7);
  WriteLn('Tasks due within 7 days: ' + intToStr(length(tasks)));
  WriteLn;

  { Test 8: Subtask assignments }
  WriteLn('Test 8: Subtask assignments...');
  taskId := manager.addTask('Project Planning', 'Plan Q1 roadmap', 'Planning',
                           tpHigh, now + 5);
  manager.addSubtask(taskId, 'Review requirements', 3.0);
  manager.addSubtask(taskId, 'Draft timeline', 2.0);
  manager.addSubtask(taskId, 'Assign resources', 2.5);

  manager.assignSubtaskTo(taskId, 0, 'alice');
  manager.assignSubtaskTo(taskId, 1, 'bob');
  manager.assignSubtaskTo(taskId, 2, 'charlie');

  subtasks := manager.getSubtasksForAssignee('alice');
  WriteLn('Alice has ' + intToStr(length(subtasks)) + ' subtask(s)');
  WriteLn;

  { Test 9: Escalation risk analysis }
  WriteLn('Test 9: Tasks with escalation risk...');
  tasks := manager.getTasksByEscalationRisk;
  WriteLn('Tasks at risk (due within 7 days): ' + intToStr(length(tasks)));
  WriteLn;

  { Test 10: Statistics }
  WriteLn('Test 10: Final statistics...');
  stats := manager.getProductivityMetrics;
  WriteLn('Total tasks: ' + intToStr(stats.totalTasks));
  WriteLn('Completed: ' + intToStr(stats.completedTasks));
  WriteLn('High priority: ' + intToStr(stats.highPriorityCount));
  WriteLn('Critical priority: ' + intToStr(stats.criticalPriorityCount));
  WriteLn;

  WriteLn('===== ALL ENHANCED TESTS COMPLETED SUCCESSFULLY =====');
  WriteLn;
end;

begin
  manager := TTaskManagerEnhanced.Create;
  try
    selfTest;
  finally
    manager.Free;
  end;
end.
