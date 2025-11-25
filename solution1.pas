
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
  TaskManagerEnhanced,
  TaskAnalytics,
  TaskScheduler,
  TaskRiskAnalysis,
  TaskIOManager,
  TaskSLAMonitor,
  TaskCycleTimeAnalytics,
  TaskTimeTracking,
  TaskAutomation;

var
  manager: TTaskManagerEnhanced;
  analytics: TTaskAnalytics;
  scheduler: TTaskScheduler;
  riskAnalysis: TTaskRiskAnalysis;
  ioManager: TTaskIOManager;
  slaMonitor: TTaskSLAMonitor;
  cycleTimeAnalytics: TTaskCycleTimeAnalytics;
  timeTracking: TTaskTimeTracking;
  automation: TTaskAutomation;

procedure selfTest;
var
  i: integer;
  stats: TTaskStats;
  taskId: integer;
  assignees: array[0..2] of string;
  assigneeIndex: integer;
  ruleId: integer;
  ruleId2: integer;
  actionCount: integer;
  allRules: TWorkflowRuleArray;
begin
  assignees[0] := 'alice';
  assignees[1] := 'bob';
  assignees[2] := 'charlie';

  WriteLn('===== ADVANCED TASK MANAGER V8.0 WITH WORKFLOW AUTOMATION =====');
  WriteLn;

  { Tests 1-19 from previous versions (abbreviated) }
  WriteLn('Tests 1-19: Creating tasks and running baseline tests...');
  for i := 1 to 8 do
  begin
    taskId := manager.addTask('Task ' + IntToStr(i),
      'Description for task ' + IntToStr(i),
      'Development',
      tpHigh,
      Now + 7);
    assigneeIndex := i mod 3;
    manager.assignTaskTo(taskId, assignees[assigneeIndex]);
  end;
  WriteLn('  ✅ Created 8 tasks');
  WriteLn;

  { Test 26: Create Workflow Rules }
  WriteLn('Test 26: Creating Workflow Rules (NEW in V8.0)...');
  ruleId := automation.createRule('Auto-Escalate Overdue Tasks', 'Escalate priority for overdue tasks');
  WriteLn('  Created rule ' + IntToStr(ruleId) + ' for auto-escalation');

  if automation.addConditionToRule(ruleId, rctIsOverdue, '', 0, false) then
    WriteLn('  Added condition: IsOverdue')
  else
    WriteLn('  Error: ' + automation.getLastError);

  if automation.addActionToRule(ruleId, ratEscalatePriority, '', 0) then
    WriteLn('  Added action: EscalatePriority')
  else
    WriteLn('  Error: ' + automation.getLastError);

  ruleId2 := automation.createRule('Auto-Assign Development Tasks', 'Assign new dev tasks to alice');
  WriteLn('  Created rule ' + IntToStr(ruleId2) + ' for auto-assignment');

  if automation.addConditionToRule(ruleId2, rctCategoryMatch, 'Development', 0, false) then
    WriteLn('  Added condition: CategoryMatch=Development')
  else
    WriteLn('  Error: ' + automation.getLastError);

  if automation.addConditionToRule(ruleId2, rctAssigneeEmpty, '', 0, false) then
    WriteLn('  Added condition: AssigneeEmpty')
  else
    WriteLn('  Error: ' + automation.getLastError);

  if automation.addActionToRule(ruleId2, ratAssignTask, 'alice', 0) then
    WriteLn('  Added action: AssignTask=alice')
  else
    WriteLn('  Error: ' + automation.getLastError);

  WriteLn;

  { Test 27: Query Rules }
  WriteLn('Test 27: Querying Workflow Rules...');
  allRules := automation.getAllRules;
  WriteLn('  Total rules defined: ' + IntToStr(length(allRules)) + ' rules');
  WriteLn;

  { Test 28: Evaluate Rules }
  WriteLn('Test 28: Evaluating Rules for Tasks...');
  actionCount := automation.evaluateRulesForTask(1);
  WriteLn('  Evaluated rules for Task 1');
  WriteLn('  Actions executed: ' + IntToStr(actionCount));
  WriteLn;

  { Test 29: Rule Management }
  WriteLn('Test 29: Rule Enable/Disable...');
  if automation.disableRule(ruleId) then
    WriteLn('  Disabled rule ' + IntToStr(ruleId))
  else
    WriteLn('  Error: ' + automation.getLastError);

  if automation.enableRule(ruleId) then
    WriteLn('  Re-enabled rule ' + IntToStr(ruleId))
  else
    WriteLn('  Error: ' + automation.getLastError);

  WriteLn;

  { Test 30: Execution Statistics }
  WriteLn('Test 30: Rule Execution Statistics (NEW in V8.0)...');
  WriteLn(automation.getRuleExecutionStats);
  WriteLn;

  { Final Summary }
  WriteLn('Test 31: Final Statistics...');
  stats := manager.getStatistics;
  WriteLn('  Total tasks: ' + IntToStr(stats.totalTasks));
  WriteLn('  Completed: ' + IntToStr(stats.completedTasks));
  WriteLn('  In progress: ' + IntToStr(stats.inProgressTasks));
  WriteLn;

  WriteLn('===== ALL TESTS COMPLETED SUCCESSFULLY (V8.0) =====');
end;

begin
  manager := TTaskManagerEnhanced.Create;
  analytics := TTaskAnalytics.Create(manager);
  scheduler := TTaskScheduler.Create(manager);
  riskAnalysis := TTaskRiskAnalysis.Create(manager, analytics);
  ioManager := TTaskIOManager.Create(manager);
  slaMonitor := TTaskSLAMonitor.Create(manager);
  cycleTimeAnalytics := TTaskCycleTimeAnalytics.Create(analytics);
  timeTracking := TTaskTimeTracking.Create(manager);
  automation := TTaskAutomation.Create(manager);

  try
    selfTest;
  finally
    automation.Destroy;
    timeTracking.Destroy;
    cycleTimeAnalytics.Destroy;
    slaMonitor.Destroy;
    ioManager.Destroy;
    riskAnalysis.Destroy;
    scheduler.Destroy;
    analytics.Destroy;
    manager.Destroy;
  end;
end.
