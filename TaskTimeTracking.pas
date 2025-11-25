
unit TaskTimeTracking;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager;

const
  maxTimeSessionsPerTask = 100;
  maxTimeSessionsTotal = 5000;

type
  TTimeSession = record
    sessionId: integer;
    taskId: integer;
    taskName: string[255];
    startTime: TDateTime;
    endTime: TDateTime;
    duration: double;
    description: string[255];
    isPaused: boolean;
    pausedAt: TDateTime;
    totalPausedTime: double;
  end;

  TTimeSessionArray = array of TTimeSession;

  TTimeTrackingStats = record
    totalSessions: integer;
    totalHoursTracked: double;
    averageSessionDuration: double;
    longestSession: double;
    shortestSession: double;
    sessionsThisWeek: integer;
    sessionsThisMonth: integer;
    hoursTrackedThisWeek: double;
    hoursTrackedThisMonth: double;
  end;

  TDailyTimeSummary = record
    date: TDateTime;
    totalHours: double;
    sessionCount: integer;
    taskCount: integer;
    averageSessionDuration: double;
  end;

  TDailySummaryArray = array of TDailyTimeSummary;

  TTaskTimeTracking = class
  private
    manager: TTaskManager;
    sessions: TTimeSessionArray;
    nextSessionId: integer;
    lastError: string;
    currentSessionId: integer;

    function findSessionIndex(sessionId: integer): integer;
    function isTaskValid(taskId: integer): boolean;
    function calculateSessionDuration(session: TTimeSession): double;
  public
    constructor Create(aManager: TTaskManager);
    destructor Destroy; override;

    function startTimeSession(taskId: integer; const description: string): integer;
    function endTimeSession(sessionId: integer): boolean;
    function pauseTimeSession(sessionId: integer): boolean;
    function resumeTimeSession(sessionId: integer): boolean;
    function cancelTimeSession(sessionId: integer): boolean;
    function getTimeSession(sessionId: integer; var session: TTimeSession): boolean;

    function getAllSessionsForTask(taskId: integer): TTimeSessionArray;
    function getAllSessions: TTimeSessionArray;
    function getSessionsForDay(date: TDateTime): TTimeSessionArray;
    function getSessionsForWeek(startDate: TDateTime): TTimeSessionArray;
    function getSessionsForMonth(year: integer; month: integer): TTimeSessionArray;
    function getSessionsByDescription(const searchTerm: string): TTimeSessionArray;

    function getTimeTrackingStats: TTimeTrackingStats;
    function getTaskTimeStats(taskId: integer): TTimeTrackingStats;
    function getTotalTrackedHoursForTask(taskId: integer): double;
    function getTotalTrackedHours: double;
    function getAverageSessionDuration: double;
    function getDailyTimeSummary(startDate: TDateTime; endDate: TDateTime): TDailySummaryArray;

    function getProductivityPattern(days: integer): string;
    function getTasksWithMostTimeSpent(limit: integer): TTaskArray;
    function getTasksWithLeastTimeSpent(limit: integer): TTaskArray;
    function getTimeVsEstimateAnalysis(taskId: integer): string;
    function getTeamMemberTrackedHours(const assignee: string): double;

    function syncTimeToTaskHours(taskId: integer): boolean;
    function syncAllTasKHours: integer;
    function reconcileTimeDiscrepancies: integer;

    function getTimeTrackingReport(startDate: TDateTime; endDate: TDateTime): string;

    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskTimeTracking.Create(aManager: TTaskManager);
begin
  inherited Create;
  manager := aManager;
  nextSessionId := 1;
  currentSessionId := 0;
  setLength(sessions, 0);
  lastError := '';
end;

destructor TTaskTimeTracking.Destroy;
begin
  setLength(sessions, 0);
  inherited Destroy;
end;

function TTaskTimeTracking.findSessionIndex(sessionId: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to length(sessions) - 1 do
  begin
    if sessions[i].sessionId = sessionId then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskTimeTracking.isTaskValid(taskId: integer): boolean;
var
  task: TTask;
begin
  result := manager.getTask(taskId, task);
end;

function TTaskTimeTracking.calculateSessionDuration(session: TTimeSession): double;
var
  duration: double;
  pauseTime: double;
begin
  if session.endTime <= session.startTime then
  begin
    result := 0;
    exit;
  end;

  duration := (session.endTime - session.startTime) * 24;
  pauseTime := session.totalPausedTime;

  if session.isPaused then
    pauseTime := pauseTime + ((now - session.pausedAt) * 24);

  result := max(0, duration - pauseTime);
end;

function TTaskTimeTracking.startTimeSession(taskId: integer; const description: string): integer;
var
  newSession: TTimeSession;
  newIndex: integer;
begin
  if not isTaskValid(taskId) then
  begin
    lastError := 'Invalid task ID';
    result := -1;
    exit;
  end;

  if currentSessionId <> 0 then
  begin
    lastError := 'Another session is already active';
    result := -1;
    exit;
  end;

  fillChar(newSession, sizeof(TTimeSession), 0);
  newSession.sessionId := nextSessionId;
  newSession.taskId := taskId;
  newSession.startTime := now;
  newSession.endTime := 0;
  newSession.duration := 0;
  newSession.description := description;
  newSession.isPaused := false;
  newSession.pausedAt := 0;
  newSession.totalPausedTime := 0;
  newSession.taskName := '';

  if length(sessions) >= maxTimeSessionsTotal then
  begin
    lastError := 'Maximum number of time sessions reached';
    result := -1;
    exit;
  end;

  newIndex := length(sessions);
  setLength(sessions, newIndex + 1);
  sessions[newIndex] := newSession;

  currentSessionId := nextSessionId;
  result := nextSessionId;
  inc(nextSessionId);
  lastError := '';
end;

function TTaskTimeTracking.endTimeSession(sessionId: integer): boolean;
var
  idx: integer;
begin
  idx := findSessionIndex(sessionId);
  if idx < 0 then
  begin
    lastError := 'Session not found';
    result := false;
    exit;
  end;

  if sessions[idx].endTime <> 0 then
  begin
    lastError := 'Session already ended';
    result := false;
    exit;
  end;

  sessions[idx].endTime := now;
  sessions[idx].isPaused := false;
  sessions[idx].duration := calculateSessionDuration(sessions[idx]);

  currentSessionId := 0;
  lastError := '';
  result := true;
end;

function TTaskTimeTracking.pauseTimeSession(sessionId: integer): boolean;
var
  idx: integer;
begin
  idx := findSessionIndex(sessionId);
  if idx < 0 then
  begin
    lastError := 'Session not found';
    result := false;
    exit;
  end;

  if sessions[idx].endTime <> 0 then
  begin
    lastError := 'Cannot pause an ended session';
    result := false;
    exit;
  end;

  if sessions[idx].isPaused then
  begin
    lastError := 'Session is already paused';
    result := false;
    exit;
  end;

  sessions[idx].isPaused := true;
  sessions[idx].pausedAt := now;
  lastError := '';
  result := true;
end;

function TTaskTimeTracking.resumeTimeSession(sessionId: integer): boolean;
var
  idx: integer;
  pausedDuration: double;
begin
  idx := findSessionIndex(sessionId);
  if idx < 0 then
  begin
    lastError := 'Session not found';
    result := false;
    exit;
  end;

  if not sessions[idx].isPaused then
  begin
    lastError := 'Session is not paused';
    result := false;
    exit;
  end;

  pausedDuration := (now - sessions[idx].pausedAt) * 24;
  sessions[idx].totalPausedTime := sessions[idx].totalPausedTime + pausedDuration;
  sessions[idx].isPaused := false;
  sessions[idx].pausedAt := 0;

  lastError := '';
  result := true;
end;

function TTaskTimeTracking.cancelTimeSession(sessionId: integer): boolean;
var
  idx: integer;
begin
  idx := findSessionIndex(sessionId);
  if idx < 0 then
  begin
    lastError := 'Session not found';
    result := false;
    exit;
  end;

  if sessions[idx].endTime <> 0 then
  begin
    lastError := 'Cannot cancel a completed session';
    result := false;
    exit;
  end;

  if idx < length(sessions) - 1 then
    move(sessions[idx + 1], sessions[idx], (length(sessions) - idx - 1) * sizeof(TTimeSession));

  setLength(sessions, length(sessions) - 1);
  currentSessionId := 0;
  lastError := '';
  result := true;
end;

function TTaskTimeTracking.getTimeSession(sessionId: integer; var session: TTimeSession): boolean;
var
  idx: integer;
begin
  idx := findSessionIndex(sessionId);
  if idx < 0 then
  begin
    lastError := 'Session not found';
    result := false;
    exit;
  end;

  session := sessions[idx];
  lastError := '';
  result := true;
end;

function TTaskTimeTracking.getAllSessionsForTask(taskId: integer): TTimeSessionArray;
var
  i: integer;
  count: integer;
  resultArray: TTimeSessionArray;
begin
  setLength(resultArray, 0);
  count := 0;

  for i := 0 to length(sessions) - 1 do
  begin
    if sessions[i].taskId = taskId then
    begin
      setLength(resultArray, count + 1);
      resultArray[count] := sessions[i];
      inc(count);
    end;
  end;

  result := resultArray;
  lastError := '';
end;

function TTaskTimeTracking.getAllSessions: TTimeSessionArray;
begin
  result := sessions;
  lastError := '';
end;

function TTaskTimeTracking.getSessionsForDay(date: TDateTime): TTimeSessionArray;
var
  i: integer;
  count: integer;
  resultArray: TTimeSessionArray;
  sessionDate: TDateTime;
begin
  setLength(resultArray, 0);
  count := 0;

  for i := 0 to length(sessions) - 1 do
  begin
    sessionDate := trunc(sessions[i].startTime);
    if sessionDate = trunc(date) then
    begin
      setLength(resultArray, count + 1);
      resultArray[count] := sessions[i];
      inc(count);
    end;
  end;

  result := resultArray;
  lastError := '';
end;

function TTaskTimeTracking.getSessionsForWeek(startDate: TDateTime): TTimeSessionArray;
var
  i: integer;
  count: integer;
  resultArray: TTimeSessionArray;
  sessionDate: TDateTime;
  endDate: TDateTime;
begin
  setLength(resultArray, 0);
  count := 0;
  endDate := startDate + 7;

  for i := 0 to length(sessions) - 1 do
  begin
    sessionDate := trunc(sessions[i].startTime);
    if (sessionDate >= trunc(startDate)) and (sessionDate < endDate) then
    begin
      setLength(resultArray, count + 1);
      resultArray[count] := sessions[i];
      inc(count);
    end;
  end;

  result := resultArray;
  lastError := '';
end;

function TTaskTimeTracking.getSessionsForMonth(year: integer; month: integer): TTimeSessionArray;
var
  i: integer;
  count: integer;
  resultArray: TTimeSessionArray;
  sessionYear: word;
  sessionMonth: word;
  sessionDay: word;
begin
  setLength(resultArray, 0);
  count := 0;

  for i := 0 to length(sessions) - 1 do
  begin
    decodeDate(sessions[i].startTime, sessionYear, sessionMonth, sessionDay);
    if (sessionYear = year) and (sessionMonth = month) then
    begin
      setLength(resultArray, count + 1);
      resultArray[count] := sessions[i];
      inc(count);
    end;
  end;

  result := resultArray;
  lastError := '';
end;

function TTaskTimeTracking.getSessionsByDescription(const searchTerm: string): TTimeSessionArray;
var
  i: integer;
  count: integer;
  resultArray: TTimeSessionArray;
begin
  setLength(resultArray, 0);
  count := 0;

  for i := 0 to length(sessions) - 1 do
  begin
    if pos(lowercase(searchTerm), lowercase(sessions[i].description)) > 0 then
    begin
      setLength(resultArray, count + 1);
      resultArray[count] := sessions[i];
      inc(count);
    end;
  end;

  result := resultArray;
  lastError := '';
end;

function TTaskTimeTracking.getTimeTrackingStats: TTimeTrackingStats;
var
  i: integer;
  stats: TTimeTrackingStats;
  thisWeek: TTimeSessionArray;
  thisMonth: TTimeSessionArray;
  weekStart: TDateTime;
  year: word;
  month: word;
  day: word;
begin
  fillChar(stats, sizeof(TTimeTrackingStats), 0);
  stats.totalSessions := length(sessions);
  stats.shortestSession := maxDouble;

  if stats.totalSessions = 0 then
  begin
    stats.shortestSession := 0;
    result := stats;
    exit;
  end;

  weekStart := trunc(now) - dayofweek(now) + 1;
  decodeDate(now, year, month, day);

  thisWeek := getSessionsForWeek(weekStart);
  thisMonth := getSessionsForMonth(year, month);

  stats.sessionsThisWeek := length(thisWeek);
  stats.sessionsThisMonth := length(thisMonth);

  for i := 0 to length(sessions) - 1 do
  begin
    stats.totalHoursTracked := stats.totalHoursTracked + sessions[i].duration;
    if sessions[i].duration > 0 then
    begin
      stats.longestSession := max(stats.longestSession, sessions[i].duration);
      stats.shortestSession := min(stats.shortestSession, sessions[i].duration);
    end;
  end;

  if stats.totalSessions > 0 then
    stats.averageSessionDuration := stats.totalHoursTracked / stats.totalSessions;

  for i := 0 to length(thisWeek) - 1 do
    stats.hoursTrackedThisWeek := stats.hoursTrackedThisWeek + thisWeek[i].duration;

  for i := 0 to length(thisMonth) - 1 do
    stats.hoursTrackedThisMonth := stats.hoursTrackedThisMonth + thisMonth[i].duration;

  setLength(thisWeek, 0);
  setLength(thisMonth, 0);

  result := stats;
  lastError := '';
end;

function TTaskTimeTracking.getTaskTimeStats(taskId: integer): TTimeTrackingStats;
var
  i: integer;
  stats: TTimeTrackingStats;
  taskSessions: TTimeSessionArray;
begin
  taskSessions := getAllSessionsForTask(taskId);

  fillChar(stats, sizeof(TTimeTrackingStats), 0);
  stats.totalSessions := length(taskSessions);
  stats.shortestSession := maxDouble;

  if stats.totalSessions = 0 then
  begin
    stats.shortestSession := 0;
    setLength(taskSessions, 0);
    result := stats;
    exit;
  end;

  for i := 0 to length(taskSessions) - 1 do
  begin
    stats.totalHoursTracked := stats.totalHoursTracked + taskSessions[i].duration;
    if taskSessions[i].duration > 0 then
    begin
      stats.longestSession := max(stats.longestSession, taskSessions[i].duration);
      stats.shortestSession := min(stats.shortestSession, taskSessions[i].duration);
    end;
  end;

  if stats.totalSessions > 0 then
    stats.averageSessionDuration := stats.totalHoursTracked / stats.totalSessions;

  setLength(taskSessions, 0);
  result := stats;
  lastError := '';
end;

function TTaskTimeTracking.getTotalTrackedHoursForTask(taskId: integer): double;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to length(sessions) - 1 do
  begin
    if sessions[i].taskId = taskId then
      total := total + sessions[i].duration;
  end;
  result := total;
  lastError := '';
end;

function TTaskTimeTracking.getTotalTrackedHours: double;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to length(sessions) - 1 do
    total := total + sessions[i].duration;
  result := total;
  lastError := '';
end;

function TTaskTimeTracking.getAverageSessionDuration: double;
var
  stats: TTimeTrackingStats;
begin
  stats := getTimeTrackingStats;
  result := stats.averageSessionDuration;
end;

function TTaskTimeTracking.getDailyTimeSummary(startDate: TDateTime; endDate: TDateTime): TDailySummaryArray;
var
  currentDate: TDateTime;
  daySessions: TTimeSessionArray;
  summaryArray: TDailySummaryArray;
  summaryIndex: integer;
  i: integer;
  dailySummary: TDailyTimeSummary;
  taskSet: array of integer;
  taskCount: integer;
  j: integer;
  found: boolean;
begin
  setLength(summaryArray, 0);
  summaryIndex := 0;

  currentDate := trunc(startDate);
  while currentDate <= trunc(endDate) do
  begin
    daySessions := getSessionsForDay(currentDate);
    if length(daySessions) > 0 then
    begin
      fillChar(dailySummary, sizeof(TDailyTimeSummary), 0);
      dailySummary.date := currentDate;
      dailySummary.sessionCount := length(daySessions);
      setLength(taskSet, 0);
      taskCount := 0;

      for i := 0 to length(daySessions) - 1 do
      begin
        dailySummary.totalHours := dailySummary.totalHours + daySessions[i].duration;

        found := false;
        for j := 0 to length(taskSet) - 1 do
        begin
          if taskSet[j] = daySessions[i].taskId then
          begin
            found := true;
            break;
          end;
        end;

        if not found then
        begin
          setLength(taskSet, taskCount + 1);
          taskSet[taskCount] := daySessions[i].taskId;
          inc(taskCount);
        end;
      end;

      dailySummary.taskCount := taskCount;
      if dailySummary.sessionCount > 0 then
        dailySummary.averageSessionDuration := dailySummary.totalHours / dailySummary.sessionCount;

      setLength(summaryArray, summaryIndex + 1);
      summaryArray[summaryIndex] := dailySummary;
      inc(summaryIndex);

      setLength(taskSet, 0);
    end;

    setLength(daySessions, 0);
    currentDate := currentDate + 1;
  end;

  result := summaryArray;
  lastError := '';
end;

function TTaskTimeTracking.getProductivityPattern(days: integer): string;
var
  startDate: TDateTime;
  dailySummaries: TDailySummaryArray;
  i: integer;
  totalHours: double;
  avgHours: double;
  result_str: string;
begin
  startDate := now - days;
  dailySummaries := getDailyTimeSummary(startDate, now);

  result_str := '';
  totalHours := 0;

  if length(dailySummaries) = 0 then
  begin
    result := 'No time tracking data available for the specified period.';
    exit;
  end;

  for i := 0 to length(dailySummaries) - 1 do
    totalHours := totalHours + dailySummaries[i].totalHours;

  avgHours := totalHours / max(1, length(dailySummaries));

  result_str := 'Productivity Pattern (Last ' + IntToStr(days) + ' days):' + #10;
  result_str := result_str + '  Days tracked: ' + IntToStr(length(dailySummaries)) + #10;
  result_str := result_str + '  Total hours: ' + FormatFloat('0.0', totalHours) + #10;
  result_str := result_str + '  Average hours/day: ' + FormatFloat('0.0', avgHours) + #10;

  if avgHours > 8 then
    result_str := result_str + '  Status: HIGH PRODUCTIVITY' + #10
  else if avgHours > 4 then
    result_str := result_str + '  Status: MODERATE PRODUCTIVITY' + #10
  else
    result_str := result_str + '  Status: LOW PRODUCTIVITY' + #10;

  setLength(dailySummaries, 0);
  result := result_str;
end;

function TTaskTimeTracking.getTasksWithMostTimeSpent(limit: integer): TTaskArray;
var
  i: integer;
  j: integer;
  maxCount: integer;
  resultArray: TTaskArray;
  task: TTask;
  taskHours: array of double;
  taskIds: array of integer;
begin
  setLength(resultArray, 0);
  setLength(taskHours, 0);
  setLength(taskIds, 0);
  maxCount := 0;

  for i := 0 to length(sessions) - 1 do
  begin
    j := 0;
    while j < maxCount do
    begin
      if taskIds[j] = sessions[i].taskId then
      begin
        taskHours[j] := taskHours[j] + sessions[i].duration;
        exit;
      end;
      inc(j);
    end;

    setLength(taskHours, maxCount + 1);
    setLength(taskIds, maxCount + 1);
    taskIds[maxCount] := sessions[i].taskId;
    taskHours[maxCount] := sessions[i].duration;
    inc(maxCount);
  end;

  i := 0;
  while i < maxCount - 1 do
  begin
    j := 0;
    while j < maxCount - 1 - i do
    begin
      if taskHours[j] < taskHours[j + 1] then
      begin
        taskHours[j] := taskHours[j] + taskHours[j + 1];
        taskHours[j + 1] := taskHours[j] - taskHours[j + 1];
        taskHours[j] := taskHours[j] - taskHours[j + 1];

        taskIds[j] := taskIds[j] + taskIds[j + 1];
        taskIds[j + 1] := taskIds[j] - taskIds[j + 1];
        taskIds[j] := taskIds[j] - taskIds[j + 1];
      end;
      inc(j);
    end;
    inc(i);
  end;

  maxCount := min(limit, maxCount);
  for i := 0 to maxCount - 1 do
  begin
    if manager.getTask(taskIds[i], task) then
    begin
      setLength(resultArray, length(resultArray) + 1);
      resultArray[length(resultArray) - 1] := task;
    end;
  end;

  setLength(taskHours, 0);
  setLength(taskIds, 0);
  result := resultArray;
  lastError := '';
end;

function TTaskTimeTracking.getTasksWithLeastTimeSpent(limit: integer): TTaskArray;
var
  i: integer;
  j: integer;
  maxCount: integer;
  resultArray: TTaskArray;
  task: TTask;
  taskHours: array of double;
  taskIds: array of integer;
begin
  setLength(resultArray, 0);
  setLength(taskHours, 0);
  setLength(taskIds, 0);
  maxCount := 0;

  for i := 0 to length(sessions) - 1 do
  begin
    j := 0;
    while j < maxCount do
    begin
      if taskIds[j] = sessions[i].taskId then
      begin
        taskHours[j] := taskHours[j] + sessions[i].duration;
        exit;
      end;
      inc(j);
    end;

    setLength(taskHours, maxCount + 1);
    setLength(taskIds, maxCount + 1);
    taskIds[maxCount] := sessions[i].taskId;
    taskHours[maxCount] := sessions[i].duration;
    inc(maxCount);
  end;

  i := 0;
  while i < maxCount - 1 do
  begin
    j := 0;
    while j < maxCount - 1 - i do
    begin
      if taskHours[j] > taskHours[j + 1] then
      begin
        taskHours[j] := taskHours[j] + taskHours[j + 1];
        taskHours[j + 1] := taskHours[j] - taskHours[j + 1];
        taskHours[j] := taskHours[j] - taskHours[j + 1];

        taskIds[j] := taskIds[j] + taskIds[j + 1];
        taskIds[j + 1] := taskIds[j] - taskIds[j + 1];
        taskIds[j] := taskIds[j] - taskIds[j + 1];
      end;
      inc(j);
    end;
    inc(i);
  end;

  maxCount := min(limit, maxCount);
  for i := 0 to maxCount - 1 do
  begin
    if manager.getTask(taskIds[i], task) then
    begin
      setLength(resultArray, length(resultArray) + 1);
      resultArray[length(resultArray) - 1] := task;
    end;
  end;

  setLength(taskHours, 0);
  setLength(taskIds, 0);
  result := resultArray;
  lastError := '';
end;

function TTaskTimeTracking.getTimeVsEstimateAnalysis(taskId: integer): string;
var
  task: TTask;
  trackedHours: double;
  estimatedHours: double;
  variance: double;
  percentVariance: double;
  analysis: string;
begin
  if not manager.getTask(taskId, task) then
  begin
    lastError := 'Task not found';
    result := '';
    exit;
  end;

  trackedHours := getTotalTrackedHoursForTask(taskId);
  estimatedHours := task.estimatedHours;

  analysis := 'Time vs Estimate Analysis for Task ' + task.name + ':' + #10;
  analysis := analysis + '  Estimated hours: ' + FormatFloat('0.0', estimatedHours) + #10;
  analysis := analysis + '  Tracked hours: ' + FormatFloat('0.0', trackedHours) + #10;

  if estimatedHours > 0 then
  begin
    variance := trackedHours - estimatedHours;
    percentVariance := (variance / estimatedHours) * 100;
    analysis := analysis + '  Variance: ' + FormatFloat('+0.0;-0.0', variance) + ' hours (' +
                FormatFloat('+0.0;-0.0', percentVariance) + '%)' + #10;

    if variance > 0 then
      analysis := analysis + '  Status: OVER ESTIMATE' + #10
    else if variance < 0 then
      analysis := analysis + '  Status: UNDER ESTIMATE' + #10
    else
      analysis := analysis + '  Status: ON TARGET' + #10;
  end;

  result := analysis;
  lastError := '';
end;

function TTaskTimeTracking.getTeamMemberTrackedHours(const assignee: string): double;
var
  i: integer;
  total: double;
  task: TTask;
begin
  total := 0;
  for i := 0 to length(sessions) - 1 do
  begin
    if manager.getTask(sessions[i].taskId, task) then
    begin
      if lowercase(task.assignedTo) = lowercase(assignee) then
        total := total + sessions[i].duration;
    end;
  end;
  result := total;
  lastError := '';
end;

function TTaskTimeTracking.syncTimeToTaskHours(taskId: integer): boolean;
var
  task: TTask;
  trackedHours: double;
begin
  if not manager.getTask(taskId, task) then
  begin
    lastError := 'Task not found';
    result := false;
    exit;
  end;

  trackedHours := getTotalTrackedHoursForTask(taskId);
  task.actualHours := trackedHours;

  if manager.updateTask(task) then
  begin
    lastError := '';
    result := true;
  end
  else
  begin
    lastError := 'Failed to update task';
    result := false;
  end;
end;

function TTaskTimeTracking.syncAllTasKHours: integer;
var
  i: integer;
  count: integer;
  allTasks: TTaskArray;
begin
  allTasks := manager.getAllTasks;
  count := 0;

  for i := 0 to length(allTasks) - 1 do
  begin
    if syncTimeToTaskHours(allTasks[i].id) then
      inc(count);
  end;

  setLength(allTasks, 0);
  result := count;
  lastError := '';
end;

function TTaskTimeTracking.reconcileTimeDiscrepancies: integer;
var
  i: integer;
  count: integer;
  allTasks: TTaskArray;
  trackedHours: double;
  difference: double;
begin
  allTasks := manager.getAllTasks;
  count := 0;

  for i := 0 to length(allTasks) - 1 do
  begin
    trackedHours := getTotalTrackedHoursForTask(allTasks[i].id);
    difference := abs(trackedHours - allTasks[i].actualHours);

    if difference > 0.01 then
    begin
      if syncTimeToTaskHours(allTasks[i].id) then
        inc(count);
    end;
  end;

  setLength(allTasks, 0);
  result := count;
  lastError := '';
end;

function TTaskTimeTracking.getTimeTrackingReport(startDate: TDateTime; endDate: TDateTime): string;
var
  stats: TTimeTrackingStats;
  dailySummaries: TDailySummaryArray;
  report: string;
  i: integer;
begin
  stats := getTimeTrackingStats;
  dailySummaries := getDailyTimeSummary(startDate, endDate);

  report := '===== TIME TRACKING REPORT =====' + #10;
  report := report + 'Period: ' + DateToStr(startDate) + ' to ' + DateToStr(endDate) + #10 + #10;

  report := report + 'Overall Statistics:' + #10;
  report := report + '  Total Sessions: ' + IntToStr(stats.totalSessions) + #10;
  report := report + '  Total Hours Tracked: ' + FormatFloat('0.0', stats.totalHoursTracked) + #10;
  report := report + '  Average Session Duration: ' + FormatFloat('0.0', stats.averageSessionDuration) + ' hours' + #10;
  report := report + '  Longest Session: ' + FormatFloat('0.0', stats.longestSession) + ' hours' + #10;
  report := report + '  Shortest Session: ' + FormatFloat('0.0', stats.shortestSession) + ' hours' + #10 + #10;

  report := report + 'This Week:' + #10;
  report := report + '  Sessions: ' + IntToStr(stats.sessionsThisWeek) + #10;
  report := report + '  Hours: ' + FormatFloat('0.0', stats.hoursTrackedThisWeek) + #10 + #10;

  report := report + 'This Month:' + #10;
  report := report + '  Sessions: ' + IntToStr(stats.sessionsThisMonth) + #10;
  report := report + '  Hours: ' + FormatFloat('0.0', stats.hoursTrackedThisMonth) + #10 + #10;

  report := report + 'Daily Breakdown:' + #10;
  for i := 0 to min(9, length(dailySummaries) - 1) do
  begin
    report := report + '  ' + DateToStr(dailySummaries[i].date) + ': ' +
              FormatFloat('0.0', dailySummaries[i].totalHours) + ' hours (' +
              IntToStr(dailySummaries[i].sessionCount) + ' sessions, ' +
              IntToStr(dailySummaries[i].taskCount) + ' tasks)' + #10;
  end;

  if length(dailySummaries) > 10 then
    report := report + '  ... and ' + IntToStr(length(dailySummaries) - 10) + ' more days' + #10;

  report := report + '===== END REPORT =====' + #10;

  setLength(dailySummaries, 0);
  result := report;
  lastError := '';
end;

function TTaskTimeTracking.getLastError: string;
begin
  result := lastError;
end;

procedure TTaskTimeTracking.clearError;
begin
  lastError := '';
end;

end.
