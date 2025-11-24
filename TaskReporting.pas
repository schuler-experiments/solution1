
unit TaskReporting;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, Math, TaskTypes;

type
  { Reporting and analytics }
  TBurndownPoint = record
    date: TDateTime;
    tasksRemaining: integer;
    tasksCompleted: integer;
  end;

  TBurndownArray = array of TBurndownPoint;

  TProjectMetrics = record
    totalTasks: integer;
    completedTasks: integer;
    inProgressTasks: integer;
    onHoldTasks: integer;
    notStartedTasks: integer;
    completionPercentage: double;
    averageTasksPerDay: double;
    velocityPerDay: double;
    projectedCompletionDate: TDateTime;
    overdueTasks: integer;
    onTimeRate: double;
  end;

  TTaskReportClass = class
  private
    tasks: TTaskArray;
    function CountStatus(status: TTaskStatus): integer;
    function CountByPriority(priority: TTaskPriority): integer;
  public
    constructor Create(atasks: TTaskArray);
    destructor Destroy();
    
    function GetMetrics(): TProjectMetrics;
    function GenerateSummaryReport(): string;
    function GenerateDetailedReport(): string;
    function GeneratePriorityReport(): string;
    function GenerateStatusReport(): string;
    function GetProjectHealth(): string;
    function EstimateCompletionDate(): TDateTime;
    function CalculateVelocity(): double;
    function GenerateCompletionTrend(): string;
    function GetTasksAtRisk(): TTaskArray;
    procedure SelfTest();
  end;

implementation

constructor TTaskReportClass.Create(atasks: TTaskArray);
begin
  tasks := Copy(atasks, 0, Length(atasks));
end;

destructor TTaskReportClass.Destroy();
begin
  SetLength(tasks, 0);
  inherited Destroy();
end;

function TTaskReportClass.CountStatus(status: TTaskStatus): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(tasks) - 1 do
  begin
    if tasks[i].status = status then
      inc(count);
  end;
  result := count;
end;

function TTaskReportClass.CountByPriority(priority: TTaskPriority): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(tasks) - 1 do
  begin
    if tasks[i].priority = priority then
      inc(count);
  end;
  result := count;
end;

function TTaskReportClass.GetMetrics(): TProjectMetrics;
var
  i: integer;
begin
  result.totalTasks := Length(tasks);
  result.completedTasks := CountStatus(tsCompleted);
  result.inProgressTasks := CountStatus(tsInProgress);
  result.onHoldTasks := CountStatus(tsOnHold);
  result.notStartedTasks := CountStatus(tsNotStarted);
  result.overdueTasks := 0;

  { Count overdue tasks }
  for i := 0 to Length(tasks) - 1 do
  begin
    if (tasks[i].dueDate < Now()) and (tasks[i].status <> tsCompleted) then
      inc(result.overdueTasks);
  end;

  { Calculate percentages }
  if result.totalTasks > 0 then
    result.completionPercentage := (result.completedTasks / result.totalTasks) * 100.0
  else
    result.completionPercentage := 0;

  { Estimate velocity }
  if result.totalTasks > 0 then
    result.averageTasksPerDay := result.totalTasks / 10.0
  else
    result.averageTasksPerDay := 0;

  result.velocityPerDay := CalculateVelocity();
  result.projectedCompletionDate := EstimateCompletionDate();
  result.onTimeRate := 75.0;
end;

function TTaskReportClass.GenerateSummaryReport(): string;
var
  metrics: TProjectMetrics;
  report: string;
begin
  metrics := GetMetrics();
  report := '';
  report := report + '╔════════════════════════════════════╗' + #10;
  report := report + '║     PROJECT SUMMARY REPORT        ║' + #10;
  report := report + '╚════════════════════════════════════╝' + #10;
  report := report + #10;
  report := report + 'Total Tasks:        ' + IntToStr(metrics.totalTasks) + #10;
  report := report + 'Completed:          ' + IntToStr(metrics.completedTasks) + #10;
  report := report + 'In Progress:        ' + IntToStr(metrics.inProgressTasks) + #10;
  report := report + 'On Hold:            ' + IntToStr(metrics.onHoldTasks) + #10;
  report := report + 'Not Started:        ' + IntToStr(metrics.notStartedTasks) + #10;
  report := report + 'Overdue:            ' + IntToStr(metrics.overdueTasks) + #10;
  report := report + #10;
  report := report + 'Completion Rate:    ' + FormatFloat('0.0%', metrics.completionPercentage / 100.0) + #10;
  report := report + 'Project Health:     ' + GetProjectHealth() + #10;
  
  result := report;
end;

function TTaskReportClass.GenerateDetailedReport(): string;
var
  report: string;
  i: integer;
begin
  report := '';
  report := report + '╔════════════════════════════════════╗' + #10;
  report := report + '║    DETAILED TASK REPORT           ║' + #10;
  report := report + '╚════════════════════════════════════╝' + #10;
  report := report + #10;

  for i := 0 to Length(tasks) - 1 do
  begin
    report := report + 'Task #' + IntToStr(tasks[i].id) + ': ' + tasks[i].title + #10;
    report := report + '  Status: ' + StatusToString(tasks[i].status) + #10;
    report := report + '  Priority: ' + PriorityToString(tasks[i].priority) + #10;
    report := report + '  Est Hours: ' + FormatFloat('0.0', tasks[i].estimatedHours) + #10;
    report := report + #10;
  end;

  result := report;
end;

function TTaskReportClass.GeneratePriorityReport(): string;
var
  report: string;
begin
  report := '';
  report := report + '╔════════════════════════════════════╗' + #10;
  report := report + '║    PRIORITY BREAKDOWN REPORT      ║' + #10;
  report := report + '╚════════════════════════════════════╝' + #10;
  report := report + #10;
  report := report + 'High Priority:   ' + IntToStr(CountByPriority(tpHigh)) + ' tasks' + #10;
  report := report + 'Medium Priority: ' + IntToStr(CountByPriority(tpMedium)) + ' tasks' + #10;
  report := report + 'Low Priority:    ' + IntToStr(CountByPriority(tpLow)) + ' tasks' + #10;
  report := report + #10;

  result := report;
end;

function TTaskReportClass.GenerateStatusReport(): string;
var
  report: string;
begin
  report := '';
  report := report + '╔════════════════════════════════════╗' + #10;
  report := report + '║    STATUS BREAKDOWN REPORT        ║' + #10;
  report := report + '╚════════════════════════════════════╝' + #10;
  report := report + #10;
  report := report + 'Not Started: ' + IntToStr(CountStatus(tsNotStarted)) + ' tasks' + #10;
  report := report + 'In Progress: ' + IntToStr(CountStatus(tsInProgress)) + ' tasks' + #10;
  report := report + 'On Hold:     ' + IntToStr(CountStatus(tsOnHold)) + ' tasks' + #10;
  report := report + 'Completed:   ' + IntToStr(CountStatus(tsCompleted)) + ' tasks' + #10;
  report := report + #10;

  result := report;
end;

function TTaskReportClass.GetProjectHealth(): string;
var
  metrics: TProjectMetrics;
begin
  metrics := GetMetrics();

  if metrics.overdueTasks > 0 then
    result := '⚠ At Risk'
  else if metrics.completionPercentage >= 80 then
    result := '✓ Excellent'
  else if metrics.completionPercentage >= 50 then
    result := '◐ Good'
  else if metrics.completionPercentage >= 20 then
    result := '◑ Fair'
  else
    result := '✗ Poor';
end;

function TTaskReportClass.EstimateCompletionDate(): TDateTime;
var
  remainingTasks: integer;
  tasksPerDay: double;
  daysRemaining: integer;
begin
  remainingTasks := Length(tasks) - CountStatus(tsCompleted);
  
  if remainingTasks = 0 then
    result := Now()
  else
  begin
    tasksPerDay := CalculateVelocity();
    if tasksPerDay > 0 then
    begin
      daysRemaining := Trunc(remainingTasks / tasksPerDay);
      result := Now() + daysRemaining;
    end
    else
      result := Now() + 30;
  end;
end;

function TTaskReportClass.CalculateVelocity(): double;
var
  i: integer;
  totalCompleted: integer;
  daysSinceStart: integer;
begin
  totalCompleted := CountStatus(tsCompleted);

  if Length(tasks) = 0 then
  begin
    result := 0;
    exit;
  end;

  { Estimate velocity based on completed tasks }
  daysSinceStart := Trunc(Now() - tasks[0].createdDate);
  
  if daysSinceStart <= 0 then
    daysSinceStart := 1;

  result := totalCompleted / daysSinceStart;
end;

function TTaskReportClass.GenerateCompletionTrend(): string;
var
  report: string;
  metrics: TProjectMetrics;
begin
  metrics := GetMetrics();
  report := '';
  report := report + 'Project Completion Trend:' + #10;
  report := report + 'Current:   ' + FormatFloat('0.0%', metrics.completionPercentage / 100.0) + #10;
  report := report + 'Velocity:  ' + FormatFloat('0.00', metrics.velocityPerDay) + ' tasks/day' + #10;
  
  result := report;
end;

function TTaskReportClass.GetTasksAtRisk(): TTaskArray;
var
  i, count: integer;
begin
  count := 0;
  SetLength(result, 0);

  for i := 0 to Length(tasks) - 1 do
  begin
    if (tasks[i].dueDate < Now() + 3) and (tasks[i].status <> tsCompleted) then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      inc(count);
    end;
  end;
end;

procedure TTaskReportClass.SelfTest();
var
  tasks_array: TTaskArray;
  task1, task2, task3: TTask;
  rep: TTaskReportClass;
begin
  WriteLn('=== TaskReporting Self Test ===');

  SetLength(tasks_array, 3);
  task1 := CreateTask(1, 'Task 1', 'Description 1', tpHigh, Now() + 5);
  task1.status := tsInProgress;
  task2 := CreateTask(2, 'Task 2', 'Description 2', tpMedium, Now() + 10);
  task2.status := tsCompleted;
  task3 := CreateTask(3, 'Task 3', 'Description 3', tpLow, Now() + 15);
  task3.status := tsNotStarted;

  tasks_array[0] := task1;
  tasks_array[1] := task2;
  tasks_array[2] := task3;

  rep := TTaskReportClass.Create(tasks_array);
  try
    WriteLn('');
    WriteLn(rep.GenerateSummaryReport());
    WriteLn(rep.GeneratePriorityReport());
    WriteLn(rep.GenerateStatusReport());
  finally
    rep.Free();
  end;

  WriteLn('=== TaskReporting Self Test Complete ===');
end;

end.
