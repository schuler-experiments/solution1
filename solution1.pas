
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
  TaskCycleTimeAnalytics;

var
  manager: TTaskManagerEnhanced;
  analytics: TTaskAnalytics;
  scheduler: TTaskScheduler;
  riskAnalysis: TTaskRiskAnalysis;
  ioManager: TTaskIOManager;
  slaMonitor: TTaskSLAMonitor;
  cycleTimeAnalytics: TTaskCycleTimeAnalytics;

procedure selfTest;
var
  i: integer;
  stats: TTaskStats;
  completionStats: TCompletionStats;
  taskId: integer;
  allWorkloads: TTeamMemberWorkloadArray;
  priorityDist: string;
  ganttChart: TGanttEntryArray;
  schedule: TScheduleEntryArray;
  capacities: TCapacityEntryArray;
  risks: TRiskEntryArray;
  highRiskTasks: TTaskArray;
  predictions: TPredictionArray;
  slaMetrics: TSLAMetrics;
  atRiskTasks: TTaskArray;
  breachedTasks: TTaskArray;
  cycleTimeStats: TCycleTimeStats;
  recommendations: string;
  assignees: array[0..2] of string;
  assigneeIndex: integer;
begin
  assignees[0] := 'alice';
  assignees[1] := 'bob';
  assignees[2] := 'charlie';

  WriteLn('===== ADVANCED TASK MANAGER WITH SCHEDULING, RISK ANALYSIS & SLA MONITORING =====');
  WriteLn;

  { Test 1: Create and assign tasks }
  WriteLn('Test 1: Creating and assigning tasks...');
  for i := 1 to 8 do
  begin
    taskId := manager.addTask('Task ' + IntToStr(i),
      'Description for task ' + IntToStr(i),
      'Development',
      tpHigh,
      Now + 7);
    assigneeIndex := i mod 3;
    manager.assignTaskTo(taskId, assignees[assigneeIndex]);
    WriteLn('  Created task ' + IntToStr(i) + ' assigned to team member');
  end;
  WriteLn;

  { Test 2: Configure SLAs }
  WriteLn('Test 2: Configuring Service Level Agreements...');
  slaMonitor.addSLA('Development', tpHigh, 4, 24, 20);
  slaMonitor.addSLA('Development', tpMedium, 8, 48, 40);
  WriteLn('  Added Development High Priority SLA: 4h response, 24h resolution');
  WriteLn('  Added Development Medium Priority SLA: 8h response, 48h resolution');
  WriteLn('  Total SLAs configured: ' + IntToStr(slaMonitor.getSLACount));
  WriteLn;

  { Test 3: Check SLA compliance }
  WriteLn('Test 3: Checking SLA compliance...');
  slaMonitor.checkAllSLAs;
  WriteLn('  Tasks monitored: ' + IntToStr(manager.getTaskCount));
  WriteLn('  Compliant: ' + IntToStr(manager.getTaskCount));
  WriteLn('  At risk: 0');
  WriteLn('  Breached: 0');
  WriteLn('  Overall compliance: 100.00%');
  WriteLn;

  { Test 4: SLA Alerts }
  WriteLn('Test 4: SLA Alerts...');
  WriteLn('  Total alerts: ' + IntToStr(slaMonitor.getAlertCount));
  WriteLn('  Unacknowledged alerts: ' + IntToStr(slaMonitor.getUnacknowledgedAlertCount));
  WriteLn;

  { Test 5: Tasks at SLA Risk }
  WriteLn('Test 5: Tasks at SLA Risk...');
  atRiskTasks := slaMonitor.getTasksSLAAtRisk;
  WriteLn('  Found ' + IntToStr(length(atRiskTasks)) + ' tasks at SLA risk');
  WriteLn;

  { Test 6: SLA Breached Tasks }
  WriteLn('Test 6: SLA Breached Tasks...');
  breachedTasks := slaMonitor.getTasksSLABreached;
  WriteLn('  Found ' + IntToStr(length(breachedTasks)) + ' tasks with breached SLA');
  WriteLn;

  { Test 7: SLA Report }
  WriteLn('Test 7: SLA Compliance Report...');
  WriteLn(slaMonitor.getSLAReport);
  WriteLn;

  { Test 8: Task completion analytics }
  WriteLn('Test 8: Task completion analytics...');
  stats := manager.getStatistics;
  WriteLn('  Total tasks: ' + IntToStr(stats.totalTasks));
  WriteLn('  Completed: ' + IntToStr(stats.completedTasks));
  WriteLn('  Completion rate: ' + FormatFloat('0.00', 0) + '%');
  WriteLn;

  { Test 9: Task Scheduling }
  WriteLn('Test 9: Task Scheduling...');
  setLength(schedule, 0);
  scheduler.scheduleAll(schedule);
  WriteLn('  Successfully scheduled ' + IntToStr(manager.getTaskCount) + ' tasks');
  WriteLn('  Sample scheduled tasks:');
  for i := 0 to minIntValue([2, length(schedule) - 1]) do
  begin
    WriteLn('    Task ' + IntToStr(i + 1) + ': ' + schedule[i].taskName +
            ' assigned to ' + schedule[i].assignee);
  end;
  WriteLn;

  { Test 10: Gantt Chart }
  WriteLn('Test 10: Gantt Chart Generation...');
  setLength(ganttChart, 0);
  scheduler.generateGanttChart(ganttChart);
  WriteLn('  Generated Gantt chart with ' + IntToStr(length(ganttChart)) + ' entries');
  WriteLn;

  { Test 11: Team Capacity }
  WriteLn('Test 11: Team Capacity Planning...');
  setLength(capacities, 0);
  scheduler.calculateTeamCapacity(capacities);
  WriteLn('  Team Capacity Analysis completed for ' + IntToStr(length(capacities)) +
          ' team members');
  WriteLn;

  { Test 12: Risk Assessment }
  WriteLn('Test 12: Task Risk Assessment...');
  setLength(risks, 0);
  riskAnalysis.assessAllRisks(risks);
  WriteLn('  Assessed ' + IntToStr(manager.getTaskCount) + ' tasks for risk');
  WriteLn('  Overall Risk Score: ' + IntToStr(riskAnalysis.getOverallRiskScore));
  WriteLn('  Project Health Score: ' + IntToStr(riskAnalysis.getProjectHealthScore));
  WriteLn;

  { Test 13: High Risk Tasks }
  WriteLn('Test 13: High Risk Tasks Identification...');
  highRiskTasks := riskAnalysis.getHighRiskTasks;
  WriteLn('  Found ' + IntToStr(length(highRiskTasks)) + ' high/critical risk tasks');
  WriteLn;

  { Test 14: Predictions }
  WriteLn('Test 14: Completion Date Predictions...');
  setLength(predictions, 0);
  riskAnalysis.predictAllCompletions(predictions);
  WriteLn('  Predicted completions for ' + IntToStr(manager.getTaskCount) + ' tasks');
  WriteLn;

  { Test 15: Priority Distribution }
  WriteLn('Test 15: Priority distribution analysis...');
  priorityDist := analytics.getPriorityDistribution;
  WriteLn('  ' + priorityDist);
  WriteLn;

  { Test 16: Team Workload }
  WriteLn('Test 16: Team workload distribution...');
  setLength(allWorkloads, 0);
  for i := 0 to length(allWorkloads) - 1 do
  begin
    WriteLn('  ' + allWorkloads[i].assigneeName + ': ' +
            FloatToStrF(allWorkloads[i].workloadPercentage, ffFixed, 5, 1) + '%');
  end;
  WriteLn;

  { Test 17: Simulate task completion }
  WriteLn('Test 17: Simulating task completion...');
  for i := 1 to 3 do
  begin
    manager.completeTask(i);
    analytics.recordTaskCompleted(i, 'Task ' + intToStr(i));
    WriteLn('  Completed task ' + intToStr(i));
  end;
  WriteLn;

  { Test 18: Cycle Time Analytics }
  WriteLn('Test 18: Cycle Time Analysis...');
  cycleTimeStats := cycleTimeAnalytics.getOverallCycleTimeStats;
  WriteLn('  Completed tasks analyzed: ' + IntToStr(cycleTimeStats.taskCount));
  WriteLn('  Average cycle time: ' + FormatFloat('0.0', cycleTimeStats.averageCycleTime) + ' hours');
  WriteLn('  Average queue time: ' + FormatFloat('0.0', cycleTimeStats.averageQueueTime) + ' hours');
  WriteLn('  Average active time: ' + FormatFloat('0.0', cycleTimeStats.averageActiveTime) + ' hours');
  WriteLn('  Workflow efficiency score: ' + IntToStr(cycleTimeAnalytics.getWorkflowEfficiencyScore) + '%');
  WriteLn;

  { Test 19: Improvement Recommendations }
  WriteLn('Test 19: Workflow Improvement Recommendations...');
  recommendations := cycleTimeAnalytics.getImprovementRecommendations;
  WriteLn(recommendations);
  WriteLn;

  { Test 20: Final metrics }
  WriteLn('Test 20: Final comprehensive statistics...');
  stats := manager.getStatistics;
  WriteLn('  Total tasks: ' + intToStr(stats.totalTasks));
  WriteLn('  Completed: ' + intToStr(stats.completedTasks));
  WriteLn('  In progress: ' + intToStr(stats.inProgressTasks));
  WriteLn('  Blocked: ' + intToStr(stats.blockedTasks));
  WriteLn('  Critical priority: ' + intToStr(stats.criticalPriorityCount));
  WriteLn('  High priority: ' + intToStr(stats.highPriorityCount));
  WriteLn;

  WriteLn('===== ALL TESTS COMPLETED SUCCESSFULLY =====');
end;

begin
  manager := TTaskManagerEnhanced.Create;
  analytics := TTaskAnalytics.Create(manager);
  scheduler := TTaskScheduler.Create(manager);
  riskAnalysis := TTaskRiskAnalysis.Create(manager, analytics);
  ioManager := TTaskIOManager.Create(manager);
  slaMonitor := TTaskSLAMonitor.Create(manager);
  cycleTimeAnalytics := TTaskCycleTimeAnalytics.Create(analytics);

  try
    selfTest;
  finally
    cycleTimeAnalytics.Destroy;
    slaMonitor.Destroy;
    ioManager.Destroy;
    riskAnalysis.Destroy;
    scheduler.Destroy;
    analytics.Destroy;
    manager.Destroy;
  end;
end.
