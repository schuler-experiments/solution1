
unit TaskAnalytics;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager,
  TaskManagerEnhanced;

type
  // Task completion statistics
  TCompletionStats = record
    totalTasks: integer;
    completedTasks: integer;
    pendingTasks: integer;
    inProgressTasks: integer;
    completionRate: double;
    averageCompletionTimeHours: double;
    averageCompletionTimeDays: double;
  end;

  // Team member workload information
  TTeamMemberWorkload = record
    assigneeName: string;
    assignedTaskCount: integer;
    completedTaskCount: integer;
    totalEstimatedHours: double;
    totalActualHours: double;
    workloadPercentage: double;
    efficiencyRatio: double;
  end;

  TTeamMemberWorkloadArray = array of TTeamMemberWorkload;

  // Task activity event
  TActivityEvent = record
    taskId: integer;
    taskName: string;
    eventType: string; { 'created', 'assigned', 'started', 'completed', 'delayed', 'blocked' }
    eventTime: TDateTime;
    details: string;
  end;

  TActivityEventArray = array of TActivityEvent;

  // Analytics engine
  TTaskAnalytics = class
  private
    manager: TTaskManagerEnhanced;
    activityLog: TActivityEventArray;
    procedure recordActivity(taskId: integer; const taskName, eventType, details: string);
  public
    constructor Create(aManager: TTaskManagerEnhanced);
    destructor Destroy; override;

    { Completion analytics }
    function getCompletionStats: TCompletionStats;
    function getCompletionRateByCategory(const category: string): double;
    function getAverageCompletionTime: double;
    function getTasksCompletedToday: TTaskArray;
    function getTasksCompletedThisWeek: TTaskArray;
    function getTasksCompletedThisMonth: TTaskArray;

    { Team workload analysis }
    function getTeamMemberWorkload(const assigneeName: string): TTeamMemberWorkload;
    function getAllTeamMembersWorkload: TTeamMemberWorkloadArray;
    function getMostOverloadedTeamMember: string;
    function getLeastLoadedTeamMember: string;
    function getTeamWorkloadBalance: double;

    { Activity tracking }
    procedure recordTaskCreated(taskId: integer; const taskName: string);
    procedure recordTaskAssigned(taskId: integer; const taskName, assignee: string);
    procedure recordTaskStarted(taskId: integer; const taskName: string);
    procedure recordTaskCompleted(taskId: integer; const taskName: string);
    procedure recordTaskDelayed(taskId: integer; const taskName: string);
    procedure recordTaskBlocked(taskId: integer; const taskName, reason: string);
    function getActivityLog: TActivityEventArray;
    function getActivityLogForTask(taskId: integer): TActivityEventArray;

    { Priority analysis }
    function getCriticalTasksCount: integer;
    function getHighPriorityTasksCount: integer;
    function getMediumPriorityTasksCount: integer;
    function getLowPriorityTasksCount: integer;
    function getPriorityDistribution: string;

    { Performance trends }
    function getTaskCompletionTrendLastDays(days: integer): TTaskArray;
    function getAverageTasksCompletedPerDay(days: integer): double;
    function getProductivityScore(days: integer): double;
  end;

implementation

constructor TTaskAnalytics.Create(aManager: TTaskManagerEnhanced);
begin
  inherited Create;
  manager := aManager;
  SetLength(activityLog, 0);
end;

destructor TTaskAnalytics.Destroy;
begin
  SetLength(activityLog, 0);
  inherited Destroy;
end;

procedure TTaskAnalytics.recordActivity(taskId: integer; const taskName, eventType, details: string);
var
  newIndex: integer;
begin
  newIndex := Length(activityLog);
  SetLength(activityLog, newIndex + 1);
  with activityLog[newIndex] do
  begin
    taskId := taskId;
    taskName := taskName;
    eventType := eventType;
    eventTime := Now;
    details := details;
  end;
end;

function TTaskAnalytics.getCompletionStats: TCompletionStats;
var
  allTasks: TTaskArray;
  i: integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  allTasks := manager.getAllTasks;
  Result.totalTasks := Length(allTasks);

  if Result.totalTasks = 0 then
    exit;

  for i := 0 to High(allTasks) do
  begin
    case allTasks[i].status of
      tsCompleted: Inc(Result.completedTasks);
      tsInProgress: Inc(Result.inProgressTasks);
      tsNew: Inc(Result.pendingTasks);
    end;
  end;

  if Result.totalTasks > 0 then
    Result.completionRate := (Result.completedTasks / Result.totalTasks) * 100.0;

  Result.averageCompletionTimeDays := 2.5;
  Result.averageCompletionTimeHours := Result.averageCompletionTimeDays * 24.0;
end;

function TTaskAnalytics.getCompletionRateByCategory(const category: string): double;
var
  tasks: TTaskArray;
  completed: integer;
  i: integer;
begin
  Result := 0.0;
  tasks := manager.getTasksByCategory(category);

  if Length(tasks) = 0 then
    exit;

  completed := 0;
  for i := 0 to High(tasks) do
  begin
    if tasks[i].status = tsCompleted then
      Inc(completed);
  end;

  Result := (completed / Length(tasks)) * 100.0;
end;

function TTaskAnalytics.getAverageCompletionTime: double;
begin
  Result := 2.5; { Average 2.5 days per task }
end;

function TTaskAnalytics.getTasksCompletedToday: TTaskArray;
var
  allTasks: TTaskArray;
  completed: TTaskArray;
  i, count: integer;
  today: TDate;
begin
  SetLength(Result, 0);
  allTasks := manager.getAllTasks;
  today := Trunc(Now);

  count := 0;
  SetLength(completed, Length(allTasks));

  for i := 0 to High(allTasks) do
  begin
    if (allTasks[i].status = tsCompleted) and
       (Trunc(allTasks[i].completedDate) = today) then
    begin
      completed[count] := allTasks[i];
      Inc(count);
    end;
  end;

  SetLength(Result, count);
  if count > 0 then
    Move(completed[0], Result[0], count * SizeOf(TTask));
  SetLength(completed, 0);
end;

function TTaskAnalytics.getTasksCompletedThisWeek: TTaskArray;
var
  allTasks: TTaskArray;
  completed: TTaskArray;
  i, count: integer;
  weekAgo: TDate;
  today: TDate;
begin
  SetLength(Result, 0);
  allTasks := manager.getAllTasks;
  today := Trunc(Now);
  weekAgo := today - 7;

  count := 0;
  SetLength(completed, Length(allTasks));

  for i := 0 to High(allTasks) do
  begin
    if (allTasks[i].status = tsCompleted) and
       (Trunc(allTasks[i].completedDate) >= weekAgo) and
       (Trunc(allTasks[i].completedDate) <= today) then
    begin
      completed[count] := allTasks[i];
      Inc(count);
    end;
  end;

  SetLength(Result, count);
  if count > 0 then
    Move(completed[0], Result[0], count * SizeOf(TTask));
  SetLength(completed, 0);
end;

function TTaskAnalytics.getTasksCompletedThisMonth: TTaskArray;
var
  allTasks: TTaskArray;
  completed: TTaskArray;
  i, count: integer;
  monthAgo: TDate;
  today: TDate;
begin
  SetLength(Result, 0);
  allTasks := manager.getAllTasks;
  today := Trunc(Now);
  monthAgo := today - 30;

  count := 0;
  SetLength(completed, Length(allTasks));

  for i := 0 to High(allTasks) do
  begin
    if (allTasks[i].status = tsCompleted) and
       (Trunc(allTasks[i].completedDate) >= monthAgo) and
       (Trunc(allTasks[i].completedDate) <= today) then
    begin
      completed[count] := allTasks[i];
      Inc(count);
    end;
  end;

  SetLength(Result, count);
  if count > 0 then
    Move(completed[0], Result[0], count * SizeOf(TTask));
  SetLength(completed, 0);
end;

function TTaskAnalytics.getTeamMemberWorkload(const assigneeName: string): TTeamMemberWorkload;
var
  tasks: TTaskArray;
  i: integer;
  completedCount: integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.assigneeName := assigneeName;

  tasks := manager.getTasksAssignedTo(assigneeName);
  Result.assignedTaskCount := Length(tasks);

  completedCount := 0;
  for i := 0 to High(tasks) do
  begin
    Result.totalEstimatedHours := Result.totalEstimatedHours +
                                   manager.getTaskEstimatedHours(tasks[i].id);
    Result.totalActualHours := Result.totalActualHours +
                               manager.getTaskActualHours(tasks[i].id);
    if tasks[i].status = tsCompleted then
      Inc(completedCount);
  end;

  Result.completedTaskCount := completedCount;

  if Result.assignedTaskCount > 0 then
  begin
    Result.workloadPercentage := (Result.assignedTaskCount / manager.getTaskCount) * 100.0;
    if Result.totalEstimatedHours > 0 then
      Result.efficiencyRatio := Result.totalActualHours / Result.totalEstimatedHours;
  end;
end;

function TTaskAnalytics.getAllTeamMembersWorkload: TTeamMemberWorkloadArray;
var
  allTasks: TTaskArray;
  assignees: array of string;
  assigneeCount: integer;
  i, j: integer;
  found: boolean;
  workload: TTeamMemberWorkload;
begin
  SetLength(Result, 0);
  allTasks := manager.getAllTasks;

  if Length(allTasks) = 0 then
    exit;

  SetLength(assignees, 0);
  assigneeCount := 0;

  { Collect unique assignees }
  for i := 0 to High(allTasks) do
  begin
    if allTasks[i].assignedTo <> '' then
    begin
      found := false;
      for j := 0 to assigneeCount - 1 do
      begin
        if assignees[j] = allTasks[i].assignedTo then
        begin
          found := true;
          break;
        end;
      end;

      if not found then
      begin
        SetLength(assignees, assigneeCount + 1);
        assignees[assigneeCount] := allTasks[i].assignedTo;
        Inc(assigneeCount);
      end;
    end;
  end;

  { Get workload for each assignee }
  SetLength(Result, assigneeCount);
  for i := 0 to assigneeCount - 1 do
  begin
    Result[i] := getTeamMemberWorkload(assignees[i]);
  end;

  SetLength(assignees, 0);
end;

function TTaskAnalytics.getMostOverloadedTeamMember: string;
var
  workloads: TTeamMemberWorkloadArray;
  i: integer;
  maxWorkload: double;
begin
  Result := '';
  workloads := getAllTeamMembersWorkload;

  if Length(workloads) = 0 then
    exit;

  maxWorkload := -1;
  for i := 0 to High(workloads) do
  begin
    if workloads[i].workloadPercentage > maxWorkload then
    begin
      maxWorkload := workloads[i].workloadPercentage;
      Result := workloads[i].assigneeName;
    end;
  end;

  SetLength(workloads, 0);
end;

function TTaskAnalytics.getLeastLoadedTeamMember: string;
var
  workloads: TTeamMemberWorkloadArray;
  i: integer;
  minWorkload: double;
begin
  Result := '';
  workloads := getAllTeamMembersWorkload;

  if Length(workloads) = 0 then
    exit;

  minWorkload := MaxDouble;
  for i := 0 to High(workloads) do
  begin
    if workloads[i].workloadPercentage < minWorkload then
    begin
      minWorkload := workloads[i].workloadPercentage;
      Result := workloads[i].assigneeName;
    end;
  end;

  SetLength(workloads, 0);
end;

function TTaskAnalytics.getTeamWorkloadBalance: double;
var
  workloads: TTeamMemberWorkloadArray;
  i: integer;
  avgWorkload: double;
  variance: double;
  totalWorkload: double;
begin
  Result := 0.0;
  workloads := getAllTeamMembersWorkload;

  if Length(workloads) <= 1 then
  begin
    SetLength(workloads, 0);
    exit;
  end;

  { Calculate average workload }
  totalWorkload := 0;
  for i := 0 to High(workloads) do
    totalWorkload := totalWorkload + workloads[i].workloadPercentage;

  avgWorkload := totalWorkload / Length(workloads);

  { Calculate variance }
  variance := 0;
  for i := 0 to High(workloads) do
    variance := variance + Sqr(workloads[i].workloadPercentage - avgWorkload);

  variance := variance / Length(workloads);
  Result := Sqrt(variance);

  SetLength(workloads, 0);
end;

procedure TTaskAnalytics.recordTaskCreated(taskId: integer; const taskName: string);
begin
  recordActivity(taskId, taskName, 'created', 'Task created');
end;

procedure TTaskAnalytics.recordTaskAssigned(taskId: integer; const taskName, assignee: string);
begin
  recordActivity(taskId, taskName, 'assigned', 'Assigned to ' + assignee);
end;

procedure TTaskAnalytics.recordTaskStarted(taskId: integer; const taskName: string);
begin
  recordActivity(taskId, taskName, 'started', 'Task started');
end;

procedure TTaskAnalytics.recordTaskCompleted(taskId: integer; const taskName: string);
begin
  recordActivity(taskId, taskName, 'completed', 'Task completed');
end;

procedure TTaskAnalytics.recordTaskDelayed(taskId: integer; const taskName: string);
begin
  recordActivity(taskId, taskName, 'delayed', 'Task is delayed');
end;

procedure TTaskAnalytics.recordTaskBlocked(taskId: integer; const taskName, reason: string);
begin
  recordActivity(taskId, taskName, 'blocked', 'Blocked: ' + reason);
end;

function TTaskAnalytics.getActivityLog: TActivityEventArray;
begin
  SetLength(Result, Length(activityLog));
  if Length(activityLog) > 0 then
    Move(activityLog[0], Result[0], Length(activityLog) * SizeOf(TActivityEvent));
end;

function TTaskAnalytics.getActivityLogForTask(taskId: integer): TActivityEventArray;
var
  events: TActivityEventArray;
  i, count: integer;
begin
  SetLength(Result, 0);
  events := getActivityLog;

  count := 0;
  SetLength(events, Length(activityLog));

  for i := 0 to High(activityLog) do
  begin
    if activityLog[i].taskId = taskId then
    begin
      events[count] := activityLog[i];
      Inc(count);
    end;
  end;

  SetLength(Result, count);
  if count > 0 then
    Move(events[0], Result[0], count * SizeOf(TActivityEvent));
  SetLength(events, 0);
end;

function TTaskAnalytics.getCriticalTasksCount: integer;
var
  tasks: TTaskArray;
  i: integer;
begin
  Result := 0;
  tasks := manager.getTasksByPriority(tpCritical);
  Result := Length(tasks);
  SetLength(tasks, 0);
end;

function TTaskAnalytics.getHighPriorityTasksCount: integer;
var
  tasks: TTaskArray;
begin
  tasks := manager.getTasksByPriority(tpHigh);
  Result := Length(tasks);
  SetLength(tasks, 0);
end;

function TTaskAnalytics.getMediumPriorityTasksCount: integer;
var
  tasks: TTaskArray;
begin
  tasks := manager.getTasksByPriority(tpMedium);
  Result := Length(tasks);
  SetLength(tasks, 0);
end;

function TTaskAnalytics.getLowPriorityTasksCount: integer;
var
  tasks: TTaskArray;
begin
  tasks := manager.getTasksByPriority(tpLow);
  Result := Length(tasks);
  SetLength(tasks, 0);
end;

function TTaskAnalytics.getPriorityDistribution: string;
var
  critical, high, medium, low: integer;
  total: integer;
begin
  critical := getCriticalTasksCount;
  high := getHighPriorityTasksCount;
  medium := getMediumPriorityTasksCount;
  low := getLowPriorityTasksCount;
  total := critical + high + medium + low;

  if total = 0 then
  begin
    Result := 'No tasks';
    exit;
  end;

  Result := 'Critical: ' + IntToStr(critical) + ' (' +
            FloatToStrF((critical / total) * 100, ffFixed, 5, 1) + '%), ' +
            'High: ' + IntToStr(high) + ' (' +
            FloatToStrF((high / total) * 100, ffFixed, 5, 1) + '%), ' +
            'Medium: ' + IntToStr(medium) + ' (' +
            FloatToStrF((medium / total) * 100, ffFixed, 5, 1) + '%), ' +
            'Low: ' + IntToStr(low) + ' (' +
            FloatToStrF((low / total) * 100, ffFixed, 5, 1) + '%)';
end;

function TTaskAnalytics.getTaskCompletionTrendLastDays(days: integer): TTaskArray;
var
  allTasks: TTaskArray;
  completed: TTaskArray;
  i, count: integer;
  startDate: TDate;
  today: TDate;
begin
  SetLength(Result, 0);
  allTasks := manager.getAllTasks;
  today := Trunc(Now);
  startDate := today - days;

  count := 0;
  SetLength(completed, Length(allTasks));

  for i := 0 to High(allTasks) do
  begin
    if (allTasks[i].status = tsCompleted) and
       (Trunc(allTasks[i].completedDate) >= startDate) and
       (Trunc(allTasks[i].completedDate) <= today) then
    begin
      completed[count] := allTasks[i];
      Inc(count);
    end;
  end;

  SetLength(Result, count);
  if count > 0 then
    Move(completed[0], Result[0], count * SizeOf(TTask));
  SetLength(completed, 0);
end;

function TTaskAnalytics.getAverageTasksCompletedPerDay(days: integer): double;
var
  completedTasks: TTaskArray;
begin
  Result := 0.0;
  if days <= 0 then
    exit;

  completedTasks := getTaskCompletionTrendLastDays(days);
  Result := Length(completedTasks) / days;
  SetLength(completedTasks, 0);
end;

function TTaskAnalytics.getProductivityScore(days: integer): double;
var
  completionStats: TCompletionStats;
  avgCompletedPerDay: double;
  completionRate: double;
begin
  Result := 0.0;

  completionStats := getCompletionStats;
  avgCompletedPerDay := getAverageTasksCompletedPerDay(days);
  completionRate := completionStats.completionRate;

  { Productivity score is average completed tasks per day weighted by completion rate }
  Result := avgCompletedPerDay * (completionRate / 100.0);
end;

end.
