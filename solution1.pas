
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
  TaskSLAMonitor;

var
  manager: TTaskManagerEnhanced;
  analytics: TTaskAnalytics;
  scheduler: TTaskScheduler;
  riskAnalysis: TTaskRiskAnalysis;
  ioManager: TTaskIOManager;
  slaMonitor: TTaskSLAMonitor;

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
  slaId: integer;
  slaMetrics: TSLAMetrics;
  atRiskTasks: TTaskArray;
  breachedTasks: TTaskArray;
begin
  WriteLn('===== ADVANCED TASK MANAGER WITH SCHEDULING, RISK ANALYSIS & SLA MONITORING =====');
  WriteLn;

  { Test 1: Create tasks with assignments }
  WriteLn('Test 1: Creating and assigning tasks...');
  for i := 1 to 8 do
  begin
    taskId := manager.addTask('Task ' + intToStr(i), 'Description for task ' + intToStr(i), 
                              'Development', tpHigh, now + i);
    analytics.recordTaskCreated(taskId, 'Task ' + intToStr(i));

    case i mod 3 of
      0: manager.assignTaskTo(taskId, 'alice');
      1: manager.assignTaskTo(taskId, 'bob');
      2: manager.assignTaskTo(taskId, 'charlie');
    end;

    WriteLn('  Created task ' + intToStr(i) + ' assigned to team member');
  end;
  WriteLn;

  { Test 2: Setup SLAs }
  WriteLn('Test 2: Configuring Service Level Agreements...');
  slaId := slaMonitor.addSLA('Development', tpHigh, 4, 24, 12);
  WriteLn('  Added Development High Priority SLA: 4h response, 24h resolution');
  slaId := slaMonitor.addSLA('Development', tpMedium, 8, 48, 24);
  WriteLn('  Added Development Medium Priority SLA: 8h response, 48h resolution');
  WriteLn('  Total SLAs configured: ' + intToStr(slaMonitor.getSLACount));
  WriteLn;

  { Test 3: Check SLA Compliance }
  WriteLn('Test 3: Checking SLA compliance...');
  slaMonitor.checkAllSLAs;
  slaMetrics := slaMonitor.calculateMetrics;
  WriteLn('  Tasks monitored: ' + intToStr(slaMetrics.totalTasksMonitored));
  WriteLn('  Compliant: ' + intToStr(slaMetrics.slaCompliantTasks));
  WriteLn('  At risk: ' + intToStr(slaMetrics.slaAtRiskTasks));
  WriteLn('  Breached: ' + intToStr(slaMetrics.slaBreachedTasks));
  WriteLn('  Overall compliance: ' + FloatToStrF(slaMetrics.complianceRate, ffFixed, 5, 2) + '%');
  WriteLn;

  { Test 4: Get SLA alerts }
  WriteLn('Test 4: SLA Alerts...');
  WriteLn('  Total alerts: ' + intToStr(slaMonitor.getAlertCount));
  WriteLn('  Unacknowledged alerts: ' + intToStr(slaMonitor.getUnacknowledgedAlertCount));
  WriteLn;

  { Test 5: Get tasks at risk }
  WriteLn('Test 5: Tasks at SLA Risk...');
  atRiskTasks := slaMonitor.getTasksSLAAtRisk;
  WriteLn('  Found ' + intToStr(length(atRiskTasks)) + ' tasks at SLA risk');
  WriteLn;

  { Test 6: Get breached tasks }
  WriteLn('Test 6: SLA Breached Tasks...');
  breachedTasks := slaMonitor.getTasksSLABreached;
  WriteLn('  Found ' + intToStr(length(breachedTasks)) + ' tasks with breached SLA');
  WriteLn;

  { Test 7: SLA Report }
  WriteLn('Test 7: SLA Compliance Report...');
  WriteLn(slaMonitor.getSLAReport);

  { Test 8: Completion Analytics (Original) }
  WriteLn('Test 8: Task completion analytics...');
  completionStats := analytics.getCompletionStats;
  WriteLn('  Total tasks: ' + intToStr(completionStats.totalTasks));
  WriteLn('  Completed: ' + intToStr(completionStats.completedTasks));
  WriteLn('  Completion rate: ' + FloatToStrF(completionStats.completionRate, ffFixed, 5, 2) + '%');
  WriteLn;

  { Test 9: Task Scheduling }
  WriteLn('Test 9: Task Scheduling...');
  if scheduler.scheduleAll(schedule) then
  begin
    WriteLn('  Successfully scheduled ' + intToStr(length(schedule)) + ' tasks');
    WriteLn('  Sample scheduled tasks:');
    for i := 0 to minIntValue([2, length(schedule) - 1]) do
    begin
      WriteLn('    Task ' + intToStr(schedule[i].taskId) + ': ' + schedule[i].taskName +
              ' assigned to ' + schedule[i].assignee);
    end;
  end
  else
    WriteLn('  Error scheduling tasks: ' + scheduler.getLastError);
  WriteLn;

  { Test 10: Gantt Chart Generation }
  WriteLn('Test 10: Gantt Chart Generation...');
  if scheduler.generateGanttChart(ganttChart) then
  begin
    WriteLn('  Generated Gantt chart with ' + intToStr(length(ganttChart)) + ' entries');
  end
  else
    WriteLn('  Error generating Gantt chart: ' + scheduler.getLastError);
  WriteLn;

  { Test 11: Team Capacity Planning }
  WriteLn('Test 11: Team Capacity Planning...');
  if scheduler.calculateTeamCapacity(capacities) then
  begin
    WriteLn('  Team Capacity Analysis completed for ' + intToStr(length(capacities)) + ' team members');
  end
  else
    WriteLn('  Error calculating capacity: ' + scheduler.getLastError);
  WriteLn;

  { Test 12: Risk Assessment }
  WriteLn('Test 12: Task Risk Assessment...');
  if riskAnalysis.assessAllRisks(risks) then
  begin
    WriteLn('  Assessed ' + intToStr(length(risks)) + ' tasks for risk');
    WriteLn('  Overall Risk Score: ' + intToStr(riskAnalysis.getOverallRiskScore));
    WriteLn('  Project Health Score: ' + intToStr(riskAnalysis.getProjectHealthScore));
  end
  else
    WriteLn('  Error assessing risks: ' + riskAnalysis.getLastError);
  WriteLn;

  { Test 13: High Risk Tasks }
  WriteLn('Test 13: High Risk Tasks Identification...');
  highRiskTasks := riskAnalysis.getHighRiskTasks;
  WriteLn('  Found ' + intToStr(length(highRiskTasks)) + ' high/critical risk tasks');
  WriteLn;

  { Test 14: Completion Prediction }
  WriteLn('Test 14: Completion Date Predictions...');
  if riskAnalysis.predictAllCompletions(predictions) then
  begin
    WriteLn('  Predicted completions for ' + intToStr(length(predictions)) + ' tasks');
  end
  else
    WriteLn('  Error predicting completions: ' + riskAnalysis.getLastError);
  WriteLn;

  { Test 15: Priority distribution }
  WriteLn('Test 15: Priority distribution analysis...');
  priorityDist := analytics.getPriorityDistribution;
  WriteLn('  ' + priorityDist);
  WriteLn;

  { Test 16: Team workload }
  WriteLn('Test 16: Team workload distribution...');
  allWorkloads := analytics.getAllTeamMembersWorkload;
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

  { Test 18: Final metrics }
  WriteLn('Test 18: Final comprehensive statistics...');
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

  try
    selfTest;
  finally
    slaMonitor.Destroy;
    analytics.Destroy;
    scheduler.Destroy;
    riskAnalysis.Destroy;
    ioManager.Destroy;
    manager.Destroy;
  end;
end.
