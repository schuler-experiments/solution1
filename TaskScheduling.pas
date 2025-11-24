
unit TaskScheduling;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils;

type
  { Recurrence pattern for recurring tasks }
  TRecurrencePattern = (rpNone, rpDaily, rpWeekly, rpBiWeekly, rpMonthly, rpQuarterly, rpYearly);

  TIntegerArray = array of integer;

  TRecurringTask = record
    taskId: integer;
    pattern: TRecurrencePattern;
    nextOccurrence: TDateTime;
    lastOccurrence: TDateTime;
    isActive: boolean;
  end;

  TRecurringTaskArray = array of TRecurringTask;

  { Time tracking session record }
  TTimeTrackingSession = record
    sessionId: integer;
    taskId: integer;
    startTime: TDateTime;
    endTime: TDateTime;
    hoursLogged: double;
    notes: string;
  end;

  TTimeSessionArray = array of TTimeTrackingSession;

  { Recurring task manager }
  TRecurringTaskManagerClass = class
  private
    recurringTasks: TRecurringTaskArray;
  public
    constructor Create();
    destructor Destroy();
    function AddRecurringTask(taskId: integer; pattern: TRecurrencePattern; startDate: TDateTime): boolean;
    function GetRecurringTask(taskId: integer): TRecurringTask;
    function IsRecurring(taskId: integer): boolean;
    procedure UpdateNextOccurrence(taskId: integer);
    function GetDueRecurringTasks(): TIntegerArray;
    function DeleteRecurringTask(taskId: integer): boolean;
    procedure SelfTest();
  end;

  { Time tracking manager for detailed time management }
  TTimeTrackingManagerClass = class
  private
    sessions: TTimeSessionArray;
    nextSessionId: integer;
  public
    constructor Create();
    destructor Destroy();
    function StartSession(taskId: integer): integer;
    function EndSession(sessionId: integer): boolean;
    function LogTime(taskId: integer; hours: double; notes: string): integer;
    function GetTotalTimeForTask(taskId: integer): double;
    function GetSessionsForTask(taskId: integer): TTimeSessionArray;
    function CalculateTimeRemainingForTask(taskId: integer; estimatedHours: double): double;
    function GetAverageTimePerTask(): double;
    procedure SelfTest();
  end;

implementation

{ ===== TRecurringTaskManagerClass ===== }

constructor TRecurringTaskManagerClass.Create();
begin
  inherited Create();
  SetLength(recurringTasks, 0);
end;

destructor TRecurringTaskManagerClass.Destroy();
begin
  SetLength(recurringTasks, 0);
  inherited Destroy();
end;

function TRecurringTaskManagerClass.AddRecurringTask(taskId: integer; pattern: TRecurrencePattern; startDate: TDateTime): boolean;
var
  len: integer;
  task: TRecurringTask;
begin
  { Check if task already recurring }
  if IsRecurring(taskId) then
  begin
    result := false;
    exit;
  end;

  task.taskId := taskId;
  task.pattern := pattern;
  task.nextOccurrence := startDate;
  task.lastOccurrence := startDate;
  task.isActive := true;

  len := Length(recurringTasks);
  SetLength(recurringTasks, len + 1);
  recurringTasks[len] := task;
  result := true;
end;

function TRecurringTaskManagerClass.GetRecurringTask(taskId: integer): TRecurringTask;
var
  i: integer;
begin
  for i := 0 to Length(recurringTasks) - 1 do
  begin
    if recurringTasks[i].taskId = taskId then
    begin
      result := recurringTasks[i];
      exit;
    end;
  end;
  result.taskId := -1;
end;

function TRecurringTaskManagerClass.IsRecurring(taskId: integer): boolean;
var
  i: integer;
begin
  for i := 0 to Length(recurringTasks) - 1 do
  begin
    if recurringTasks[i].taskId = taskId then
    begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

procedure TRecurringTaskManagerClass.UpdateNextOccurrence(taskId: integer);
var
  i: integer;
begin
  for i := 0 to Length(recurringTasks) - 1 do
  begin
    if recurringTasks[i].taskId = taskId then
    begin
      recurringTasks[i].lastOccurrence := recurringTasks[i].nextOccurrence;
      case recurringTasks[i].pattern of
        rpDaily:
          recurringTasks[i].nextOccurrence := IncDay(recurringTasks[i].nextOccurrence, 1);
        rpWeekly:
          recurringTasks[i].nextOccurrence := IncDay(recurringTasks[i].nextOccurrence, 7);
        rpBiWeekly:
          recurringTasks[i].nextOccurrence := IncDay(recurringTasks[i].nextOccurrence, 14);
        rpMonthly:
          recurringTasks[i].nextOccurrence := IncMonth(recurringTasks[i].nextOccurrence, 1);
        rpQuarterly:
          recurringTasks[i].nextOccurrence := IncMonth(recurringTasks[i].nextOccurrence, 3);
        rpYearly:
          recurringTasks[i].nextOccurrence := IncYear(recurringTasks[i].nextOccurrence, 1);
      end;
      exit;
    end;
  end;
end;

function TRecurringTaskManagerClass.GetDueRecurringTasks(): TIntegerArray;
var
  i, count: integer;
  resultArr: TIntegerArray;
  currentDate: TDateTime;
begin
  currentDate := Now();
  count := 0;
  SetLength(resultArr, 0);

  for i := 0 to Length(recurringTasks) - 1 do
  begin
    if (recurringTasks[i].isActive) and (recurringTasks[i].nextOccurrence <= currentDate) then
    begin
      SetLength(resultArr, count + 1);
      resultArr[count] := recurringTasks[i].taskId;
      inc(count);
    end;
  end;

  result := resultArr;
end;

function TRecurringTaskManagerClass.DeleteRecurringTask(taskId: integer): boolean;
var
  i, j: integer;
begin
  for i := 0 to Length(recurringTasks) - 1 do
  begin
    if recurringTasks[i].taskId = taskId then
    begin
      for j := i to Length(recurringTasks) - 2 do
        recurringTasks[j] := recurringTasks[j + 1];
      SetLength(recurringTasks, Length(recurringTasks) - 1);
      result := true;
      exit;
    end;
  end;
  result := false;
end;

procedure TRecurringTaskManagerClass.SelfTest();
var
  recurMgr: TRecurringTaskManagerClass;
begin
  WriteLn('=== TaskScheduling Recurring Task Manager Self Test ===');
  recurMgr := TRecurringTaskManagerClass.Create();

  { Add some recurring tasks }
  recurMgr.AddRecurringTask(1, rpDaily, Now());
  recurMgr.AddRecurringTask(2, rpWeekly, Now());
  recurMgr.AddRecurringTask(3, rpMonthly, Now());

  WriteLn('Added 3 recurring tasks (daily, weekly, monthly)');
  WriteLn('Task 1 is recurring: ' + BoolToStr(recurMgr.IsRecurring(1), true));
  WriteLn('Task 4 is recurring: ' + BoolToStr(recurMgr.IsRecurring(4), true));

  { Update next occurrence }
  recurMgr.UpdateNextOccurrence(1);
  WriteLn('✓ Updated next occurrence for daily task');

  { Delete recurring task }
  if recurMgr.DeleteRecurringTask(2) then
    WriteLn('✓ Deleted recurring task 2');

  recurMgr.Destroy();
  WriteLn('=== Recurring Task Manager Self Test Complete ===');
end;

{ ===== TTimeTrackingManagerClass ===== }

constructor TTimeTrackingManagerClass.Create();
begin
  inherited Create();
  SetLength(sessions, 0);
  nextSessionId := 1;
end;

destructor TTimeTrackingManagerClass.Destroy();
begin
  SetLength(sessions, 0);
  inherited Destroy();
end;

function TTimeTrackingManagerClass.StartSession(taskId: integer): integer;
var
  len: integer;
  session: TTimeTrackingSession;
begin
  session.sessionId := nextSessionId;
  session.taskId := taskId;
  session.startTime := Now();
  session.endTime := 0;
  session.hoursLogged := 0;
  session.notes := '';

  len := Length(sessions);
  SetLength(sessions, len + 1);
  sessions[len] := session;
  result := nextSessionId;
  inc(nextSessionId);
end;

function TTimeTrackingManagerClass.EndSession(sessionId: integer): boolean;
var
  i: integer;
begin
  for i := 0 to Length(sessions) - 1 do
  begin
    if sessions[i].sessionId = sessionId then
    begin
      sessions[i].endTime := Now();
      sessions[i].hoursLogged := (sessions[i].endTime - sessions[i].startTime) * 24;
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TTimeTrackingManagerClass.LogTime(taskId: integer; hours: double; notes: string): integer;
var
  len: integer;
  session: TTimeTrackingSession;
begin
  session.sessionId := nextSessionId;
  session.taskId := taskId;
  session.startTime := Now();
  session.endTime := Now();
  session.hoursLogged := hours;
  session.notes := notes;

  len := Length(sessions);
  SetLength(sessions, len + 1);
  sessions[len] := session;
  result := nextSessionId;
  inc(nextSessionId);
end;

function TTimeTrackingManagerClass.GetTotalTimeForTask(taskId: integer): double;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to Length(sessions) - 1 do
  begin
    if sessions[i].taskId = taskId then
      total := total + sessions[i].hoursLogged;
  end;
  result := total;
end;

function TTimeTrackingManagerClass.GetSessionsForTask(taskId: integer): TTimeSessionArray;
var
  i, count: integer;
  resultArr: TTimeSessionArray;
begin
  count := 0;
  SetLength(resultArr, 0);

  for i := 0 to Length(sessions) - 1 do
  begin
    if sessions[i].taskId = taskId then
    begin
      SetLength(resultArr, count + 1);
      resultArr[count] := sessions[i];
      inc(count);
    end;
  end;

  result := resultArr;
end;

function TTimeTrackingManagerClass.CalculateTimeRemainingForTask(taskId: integer; estimatedHours: double): double;
var
  spent: double;
begin
  spent := GetTotalTimeForTask(taskId);
  result := estimatedHours - spent;
  if result < 0 then
    result := 0;
end;

function TTimeTrackingManagerClass.GetAverageTimePerTask(): double;
var
  i, taskCount: integer;
  total: double;
  taskIds: array of integer;
  found: boolean;
  j: integer;
begin
  taskCount := 0;
  total := 0;
  SetLength(taskIds, 0);

  { Count unique tasks }
  for i := 0 to Length(sessions) - 1 do
  begin
    found := false;
    for j := 0 to Length(taskIds) - 1 do
    begin
      if taskIds[j] = sessions[i].taskId then
      begin
        found := true;
        break;
      end;
    end;
    if not found then
    begin
      SetLength(taskIds, taskCount + 1);
      taskIds[taskCount] := sessions[i].taskId;
      inc(taskCount);
    end;
  end;

  if taskCount = 0 then
  begin
    result := 0;
    exit;
  end;

  { Sum all hours }
  for i := 0 to Length(sessions) - 1 do
    total := total + sessions[i].hoursLogged;

  result := total / taskCount;
end;

procedure TTimeTrackingManagerClass.SelfTest();
var
  timeMgr: TTimeTrackingManagerClass;
begin
  WriteLn('=== TaskScheduling Time Tracking Manager Self Test ===');
  timeMgr := TTimeTrackingManagerClass.Create();

  { Log some time for tasks }
  timeMgr.LogTime(1, 2.5, 'Database design');
  timeMgr.LogTime(1, 1.5, 'Code review');
  timeMgr.LogTime(2, 3.0, 'Implementation');

  WriteLn('Logged time for 2 tasks');
  WriteLn(Format('Total time for task 1: %.1f hours', [timeMgr.GetTotalTimeForTask(1)]));
  WriteLn(Format('Total time for task 2: %.1f hours', [timeMgr.GetTotalTimeForTask(2)]));
  WriteLn(Format('Time remaining for task 1 (est 10h): %.1f hours', 
    [timeMgr.CalculateTimeRemainingForTask(1, 10)]));
  WriteLn(Format('Average time per task: %.1f hours', [timeMgr.GetAverageTimePerTask()]));

  timeMgr.Destroy();
  WriteLn('=== Time Tracking Manager Self Test Complete ===');
end;

end.
