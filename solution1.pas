
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
  TaskAnalytics;

var
  manager: TTaskManagerEnhanced;
  analytics: TTaskAnalytics;

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
begin
  WriteLn('===== ADVANCED TASK MANAGER WITH ANALYTICS TEST =====');
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

  { Test 2: Completion Statistics }
  WriteLn('Test 2: Task completion analytics...');
  completionStats := analytics.getCompletionStats;
  WriteLn('  Total tasks: ' + intToStr(completionStats.totalTasks));
  WriteLn('  Completed: ' + intToStr(completionStats.completedTasks));
  WriteLn('  Completion rate: ' + FloatToStrF(completionStats.completionRate, ffFixed, 5, 2) + '%');
  WriteLn('  Average completion time: ' + FloatToStrF(completionStats.averageCompletionTimeDays, ffFixed, 5, 2) + ' days');
  WriteLn;

  { Test 3: Priority Distribution }
  WriteLn('Test 3: Priority distribution analysis...');
  priorityDist := analytics.getPriorityDistribution;
  WriteLn('  ' + priorityDist);
  WriteLn;

  { Test 4: Team Workload Analysis }
  WriteLn('Test 4: Team workload distribution...');
  allWorkloads := analytics.getAllTeamMembersWorkload;
  for i := 0 to High(allWorkloads) do
  begin
    with allWorkloads[i] do
    begin
      WriteLn('  ' + assigneeName + ':');
      WriteLn('    - Assigned tasks: ' + intToStr(assignedTaskCount));
      WriteLn('    - Completed: ' + intToStr(completedTaskCount));
      WriteLn('    - Workload: ' + FloatToStrF(workloadPercentage, ffFixed, 5, 1) + '%');
      if totalEstimatedHours > 0 then
        WriteLn('    - Efficiency: ' + FloatToStrF(efficiencyRatio, ffFixed, 5, 2));
    end;
  end;
  WriteLn;

  { Test 5: Workload Balance }
  WriteLn('Test 5: Team workload balance metrics...');
  WriteLn('  Most loaded: ' + analytics.getMostOverloadedTeamMember);
  WriteLn('  Least loaded: ' + analytics.getLeastLoadedTeamMember);
  WriteLn('  Workload variance: ' + FloatToStrF(analytics.getTeamWorkloadBalance, ffFixed, 5, 2));
  WriteLn;

  { Test 6: Complete some tasks }
  WriteLn('Test 6: Simulating task completion...');
  for i := 1 to 3 do
  begin
    if manager.completeTask(i) then
    begin
      analytics.recordTaskCompleted(i, 'Task ' + intToStr(i));
      WriteLn('  Completed task ' + intToStr(i));
    end;
  end;
  WriteLn;

  { Test 7: Productivity Metrics }
  WriteLn('Test 7: Productivity and performance metrics...');
  avgCompletedPerDay := analytics.getAverageTasksCompletedPerDay(7);
  productivityScore := analytics.getProductivityScore(7);
  WriteLn('  Average tasks completed per day (7 days): ' + FloatToStrF(avgCompletedPerDay, ffFixed, 5, 3));
  WriteLn('  Productivity score: ' + FloatToStrF(productivityScore, ffFixed, 5, 3));
  WriteLn;

  { Test 8: Activity Recording }
  WriteLn('Test 8: Activity tracking...');
  analytics.recordTaskAssigned(5, 'Task 5', 'diana');
  analytics.recordTaskStarted(6, 'Task 6');
  analytics.recordTaskBlocked(7, 'Task 7', 'Waiting for design review');
  WriteLn('  Recorded task activities (assigned, started, blocked)');
  WriteLn;

  { Test 9: Task Completion Categories }
  WriteLn('Test 9: Completion rates by category...');
  WriteLn('  Development completion rate: ' + 
          FloatToStrF(analytics.getCompletionRateByCategory('Development'), ffFixed, 5, 1) + '%');
  WriteLn;

  { Test 10: Summary Statistics }
  WriteLn('Test 10: Final comprehensive statistics...');
  stats := manager.getProductivityMetrics;
  completionStats := analytics.getCompletionStats;
  WriteLn('  Total tasks: ' + intToStr(stats.totalTasks));
  WriteLn('  Completed: ' + intToStr(completionStats.completedTasks));
  WriteLn('  In progress: ' + intToStr(completionStats.inProgressTasks));
  WriteLn('  Pending: ' + intToStr(completionStats.pendingTasks));
  WriteLn('  Critical priority: ' + intToStr(stats.criticalPriorityCount));
  WriteLn('  High priority: ' + intToStr(stats.highPriorityCount));
  WriteLn;

  WriteLn('===== ALL ANALYTICS TESTS COMPLETED SUCCESSFULLY =====');
  WriteLn;
end;

begin
  manager := TTaskManagerEnhanced.Create;
  analytics := TTaskAnalytics.Create(manager);
  try
    selfTest;
  finally
    analytics.Free;
    manager.Free;
  end;
end.
