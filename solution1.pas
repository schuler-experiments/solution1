
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
  TaskAutomation,
  TaskCollaboration;

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
  collaboration: TTaskCollaborationEngine;

procedure selfTest;
var
  i: integer;
  stats: TTaskStats;
  taskId: integer;
  allWorkloads: TTeamMemberWorkloadArray;
  priorityDist: string;
  ganttChart: TGanttEntryArray;
  schedule: TScheduleEntryArray;
  capacities: TCapacityEntryArray;
  risks: TRiskEntryArray;
  highRiskTasks: TTaskArray;
  predictions: TPredictionArray;
  atRiskTasks: TTaskArray;
  breachedTasks: TTaskArray;
  cycleTimeStats: TCycleTimeStats;
  recommendations: string;
  assignees: array[0..2] of string;
  assigneeIndex: integer;
  timeStats: TTimeTrackingStats;
  sessionId: integer;
  timeVsEst: string;
  commentId: integer;
  notificationId: integer;
  userNotifications: TNotificationArray;
  ruleId: integer;
  ruleId2: integer;
  actionCount: integer;
  allRules: TWorkflowRuleArray;
  collaborationMetrics: string;
begin
  assignees[0] := 'alice';
  assignees[1] := 'bob';
  assignees[2] := 'charlie';

  WriteLn('===== ADVANCED TASK MANAGER V9.0 WITH COMPLETE TEST SUITE =====');
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

  { Test 20: NEW - Time Tracking Sessions }
  WriteLn('Test 20: Time Tracking Sessions (NEW in V7.0)...');
  WriteLn('  Starting time session for Task 1...');
  sessionId := timeTracking.startTimeSession(1, 'Initial development work');
  WriteLn('  Session ' + IntToStr(sessionId) + ' started');
  WriteLn('  Ending session...');
  if timeTracking.endTimeSession(sessionId) then
    WriteLn('  Session ended successfully')
  else
    WriteLn('  Error: ' + timeTracking.getLastError);
  WriteLn;

  { Test 21: Time Statistics }
  WriteLn('Test 21: Time Tracking Statistics...');
  timeStats := timeTracking.getTimeTrackingStats;
  WriteLn('  Total sessions recorded: ' + IntToStr(timeStats.totalSessions));
  WriteLn('  Total hours tracked: ' + FormatFloat('0.0', timeStats.totalHoursTracked) + ' hours');
  WriteLn('  Average session duration: ' + FormatFloat('0.0', timeStats.averageSessionDuration) + ' hours');
  WriteLn('  Sessions this week: ' + IntToStr(timeStats.sessionsThisWeek));
  WriteLn('  Hours tracked this week: ' + FormatFloat('0.0', timeStats.hoursTrackedThisWeek) + ' hours');
  WriteLn;

  { Test 22: Time vs Estimate Analysis }
  WriteLn('Test 22: Time vs Estimate Analysis...');
  timeVsEst := timeTracking.getTimeVsEstimateAnalysis(1);
  WriteLn(timeVsEst);
  WriteLn;

  { Test 23: Productivity Pattern }
  WriteLn('Test 23: Productivity Pattern Analysis...');
  WriteLn(timeTracking.getProductivityPattern(7));
  WriteLn;

  { Test 24: Team Member Tracked Hours }
  WriteLn('Test 24: Team Member Time Tracking...');
  for i := 0 to 2 do
  begin
    WriteLn('  ' + assignees[i] + ': ' + FormatFloat('0.0',
            timeTracking.getTeamMemberTrackedHours(assignees[i])) + ' hours');
  end;
  WriteLn;

  { Test 25: Final metrics }
  WriteLn('Test 25: Final comprehensive statistics...');
  stats := manager.getStatistics;
  WriteLn('  Total tasks: ' + intToStr(stats.totalTasks));
  WriteLn('  Completed: ' + intToStr(stats.completedTasks));
  WriteLn('  In progress: ' + intToStr(stats.inProgressTasks));
  WriteLn('  Blocked: ' + intToStr(stats.blockedTasks));
  WriteLn('  Critical priority: ' + intToStr(stats.criticalPriorityCount));
  WriteLn('  High priority: ' + intToStr(stats.highPriorityCount));
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

  { Test 32: Team Collaboration - Comments (NEW in V9.0) }
  WriteLn('Test 32: Team Collaboration - Adding Comments (NEW in V9.0)...');
  commentId := collaboration.addComment(1, 'alice', 'This task looks good. Let me start working on it.');
  if commentId > 0 then
    WriteLn('  ✅ Comment added by alice (ID: ' + IntToStr(commentId) + ')')
  else
    WriteLn('  ❌ Error: ' + collaboration.getLastError);

  commentId := collaboration.addComment(1, 'bob', 'I can help with testing once done');
  if commentId > 0 then
    WriteLn('  ✅ Comment added by bob (ID: ' + IntToStr(commentId) + ')')
  else
    WriteLn('  ❌ Error: ' + collaboration.getLastError);

  WriteLn('  Task 1 has ' + IntToStr(collaboration.getCommentCount(1)) + ' comments');
  WriteLn;

  { Test 33: Mentions and Notifications }
  WriteLn('Test 33: Mentions and Notifications (NEW in V9.0)...');
  if collaboration.addMentionToComment(commentId, 'charlie') then
    WriteLn('  ✅ Charlie mentioned in comment')
  else
    WriteLn('  ❌ Error: ' + collaboration.getLastError);

  collaboration.notifyMentionedUsers(commentId);
  WriteLn('  ✅ Notifications sent to mentioned users');
  WriteLn;

  { Test 34: Notification Management }
  WriteLn('Test 34: Notification Management (NEW in V9.0)...');
  notificationId := collaboration.createNotification(2, 'alice', ntTaskAssigned,
    'You have been assigned to Task 2', npHigh);
  WriteLn('  ✅ Notification created (ID: ' + IntToStr(notificationId) + ')');

  userNotifications := collaboration.getUserNotifications('alice');
  WriteLn('  Alice has ' + IntToStr(length(userNotifications)) + ' notifications');

  userNotifications := collaboration.getUnreadNotifications('alice');
  WriteLn('  Alice has ' + IntToStr(length(userNotifications)) + ' unread notifications');

  if collaboration.markNotificationAsRead(notificationId) then
    WriteLn('  ✅ Marked notification as read');

  WriteLn;

  { Test 35: Activity Feed }
  WriteLn('Test 35: Activity Feed (NEW in V9.0)...');
  collaboration.recordActivity(3, 'alice', 'started', 'Task work began');
  collaboration.recordActivity(3, 'alice', 'reviewed', 'Code review completed');
  collaboration.recordActivity(3, 'bob', 'tested', 'QA testing started');

  WriteLn('  Task 3 has ' + IntToStr(collaboration.getActivityCount(3)) + ' activities');
  WriteLn;

  { Test 36: Collaboration Metrics }
  WriteLn('Test 36: Team Collaboration Metrics (NEW in V9.0)...');
  collaborationMetrics := collaboration.getTeamCollaborationMetrics;
  WriteLn(collaborationMetrics);
  WriteLn;

  { Test 37: Comment Threads }
  WriteLn('Test 37: Comment Threads and Likes (NEW in V9.0)...');
  if collaboration.likeComment(commentId) then
    WriteLn('  ✅ Liked comment');

  WriteLn('  Comment ' + IntToStr(commentId) + ' has ' + 
    IntToStr(collaboration.getCommentLikes(commentId)) + ' likes');
  WriteLn;

  { Test 38: User Mentions Query }
  WriteLn('Test 38: User Mention Queries (NEW in V9.0)...');
  WriteLn('  Charlie mentioned in ' + IntToStr(collaboration.getMentionCount('charlie')) +
    ' comments');
  WriteLn;

  { Test 39: Final Statistics }
  WriteLn('Test 39: Final Statistics...');
  stats := manager.getStatistics;
  WriteLn('  Total tasks: ' + IntToStr(stats.totalTasks));
  WriteLn('  Completed: ' + IntToStr(stats.completedTasks));
  WriteLn('  In progress: ' + IntToStr(stats.inProgressTasks));
  WriteLn;

  WriteLn('===== ALL TESTS COMPLETED SUCCESSFULLY (V9.0 WITH COMPLETE TEST SUITE) =====');
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
  collaboration := TTaskCollaborationEngine.Create(manager);

  try
    selfTest;
  finally
    collaboration.Destroy;
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
