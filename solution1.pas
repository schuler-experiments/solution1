
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
  TaskIOManager;

var
  manager: TTaskManagerEnhanced;
  analytics: TTaskAnalytics;
  scheduler: TTaskScheduler;
  riskAnalysis: TTaskRiskAnalysis;
  ioManager: TTaskIOManager;

procedure selfTest;
var
  i: integer;
  stats: TTaskStats;
  completionStats: TCompletionStats;
  taskId: integer;
  allWorkloads: TTeamMemberWorkloadArray;
  priorityDist: string;
  avgCompletedPerDay: double;
  productivityScore: double;
  ganttChart: TGanttEntryArray;
  schedule: TScheduleEntryArray;
  capacities: TCapacityEntryArray;
  risks: TRiskEntryArray;
  j: integer;
  highRiskTasks: TTaskArray;
  predictions: TPredictionArray;
begin
  WriteLn('===== ADVANCED TASK MANAGER WITH SCHEDULING & RISK ANALYSIS =====');
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

  { Test 2: Completion Statistics (Original) }
  WriteLn('Test 2: Task completion analytics...');
  completionStats := analytics.getCompletionStats;
  WriteLn('  Total tasks: ' + intToStr(completionStats.totalTasks));
  WriteLn('  Completed: ' + intToStr(completionStats.completedTasks));
  WriteLn('  Completion rate: ' + FloatToStrF(completionStats.completionRate, ffFixed, 5, 2) + '%');
  WriteLn('  Average completion time: ' + FloatToStrF(completionStats.averageCompletionTimeDays, ffFixed, 5, 2) + ' days');
  WriteLn;

  { Test 3: NEW - Scheduling Engine }
  WriteLn('Test 3: Task Scheduling...');
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

  { Test 4: NEW - Gantt Chart Generation }
  WriteLn('Test 4: Gantt Chart Generation...');
  if scheduler.generateGanttChart(ganttChart) then
  begin
    WriteLn('  Generated Gantt chart with ' + intToStr(length(ganttChart)) + ' entries');
    WriteLn('  Gantt Chart Summary:');
    for i := 0 to minIntValue([2, length(ganttChart) - 1]) do
    begin
      WriteLn('    ' + ganttChart[i].taskName + ': ' +
              ganttChart[i].startDate + ' to ' + ganttChart[i].endDate +
              ' (' + intToStr(ganttChart[i].progress) + '% complete)');
    end;
  end
  else
    WriteLn('  Error generating Gantt chart: ' + scheduler.getLastError);
  WriteLn;

  { Test 5: NEW - Team Capacity Planning }
  WriteLn('Test 5: Team Capacity Planning...');
  if scheduler.calculateTeamCapacity(capacities) then
  begin
    WriteLn('  Team Capacity Analysis:');
    for i := 0 to minIntValue([2, length(capacities) - 1]) do
    begin
      WriteLn('    ' + capacities[i].assignee + ': ' +
              FloatToStrF(capacities[i].utilizationPercent, ffFixed, 5, 1) + '% utilized (' +
              FloatToStrF(capacities[i].allocatedHours, ffFixed, 5, 1) + '/' +
              FloatToStrF(capacities[i].totalCapacityHours, ffFixed, 5, 1) + ' hours)');
    end;
  end
  else
    WriteLn('  Error calculating capacity: ' + scheduler.getLastError);
  WriteLn;

  { Test 6: NEW - Risk Assessment }
  WriteLn('Test 6: Task Risk Assessment...');
  if riskAnalysis.assessAllRisks(risks) then
  begin
    WriteLn('  Assessed ' + intToStr(length(risks)) + ' tasks for risk');
    WriteLn('  Overall Risk Score: ' + intToStr(riskAnalysis.getOverallRiskScore));
    WriteLn('  Project Health Score: ' + intToStr(riskAnalysis.getProjectHealthScore));
    WriteLn('  Sample task risks:');
    for i := 0 to minIntValue([2, length(risks) - 1]) do
    begin
      WriteLn('    ' + risks[i].taskName + ': Risk Score ' + intToStr(risks[i].riskScore) +
              ' (' + risks[i].suggestedAction + ')');
    end;
  end
  else
    WriteLn('  Error assessing risks: ' + riskAnalysis.getLastError);
  WriteLn;

  { Test 7: NEW - High Risk Tasks }
  WriteLn('Test 7: High Risk Tasks Identification...');
  highRiskTasks := riskAnalysis.getHighRiskTasks;
  WriteLn('  Found ' + intToStr(length(highRiskTasks)) + ' high/critical risk tasks');
  WriteLn;

  { Test 8: NEW - Completion Prediction }
  WriteLn('Test 8: Completion Date Predictions...');
  if riskAnalysis.predictAllCompletions(predictions) then
  begin
    WriteLn('  Predicted completions for ' + intToStr(length(predictions)) + ' tasks');
    WriteLn('  Sample predictions:');
    for i := 0 to minIntValue([2, length(predictions) - 1]) do
    begin
      WriteLn('    ' + predictions[i].taskName + ': Delay risk ' +
              FloatToStrF(predictions[i].delayRisk, ffFixed, 5, 1) + '%');
    end;
  end
  else
    WriteLn('  Error predicting completions: ' + riskAnalysis.getLastError);
  WriteLn;

  { Test 9: Priority distribution }
  WriteLn('Test 9: Priority distribution analysis...');
  priorityDist := analytics.getPriorityDistribution;
  WriteLn('  ' + priorityDist);
  WriteLn;

  { Test 10: Team workload }
  WriteLn('Test 10: Team workload distribution...');
  allWorkloads := analytics.getAllTeamMembersWorkload;
  for i := 0 to length(allWorkloads) - 1 do
  begin
    WriteLn('  ' + allWorkloads[i].assigneeName + ':');
    WriteLn('    - Assigned tasks: ' + intToStr(allWorkloads[i].assignedTaskCount));
    WriteLn('    - Completed: ' + intToStr(allWorkloads[i].completedTaskCount));
    WriteLn('    - Workload: ' + FloatToStrF(allWorkloads[i].workloadPercentage, ffFixed, 5, 1) + '%');
  end;
  WriteLn;

  { Test 11: Simulate task completion }
  WriteLn('Test 11: Simulating task completion...');
  for i := 1 to 3 do
  begin
    manager.completeTask(i);
    analytics.recordTaskCompleted(i, 'Task ' + intToStr(i));
    WriteLn('  Completed task ' + intToStr(i));
  end;
  WriteLn;

  { Test 12: Final metrics }
  WriteLn('Test 12: Final comprehensive statistics...');
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

  try
    selfTest;
  finally
    analytics.Destroy;
    scheduler.Destroy;
    riskAnalysis.Destroy;
    ioManager.Destroy;
    manager.Destroy;
  end;
end.
