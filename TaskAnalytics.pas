unit TaskAnalytics;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes;

type
  TAnalyticsMetric = record
    label_: string;
    value: double;
  end;

  TAnalyticsMetricArray = array of TAnalyticsMetric;

  TBurndownPoint = record
    day: integer;
    tasksRemaining: integer;
    estimatedHours: double;
  end;

  TBurndownArray = array of TBurndownPoint;

  TProductivityTrend = record
    day: TDateTime;
    tasksCompleted: integer;
    hoursWorked: double;
  end;

  TProductivityArray = array of TProductivityTrend;

  TTrendAnalysis = record
    average: double;
    peak: double;
    minimum: double;
    trend: string;  { "increasing", "decreasing", "stable" }
  end;

  TTaskAnalyticsClass = class
  private
    function CalculateStandardDeviation(values: array of double): double;
  public
    constructor Create();
    destructor Destroy();
    function CalculateProductivity(totalTasks, completedTasks: integer;
                                   days: integer): double;
    function CalculateVelocity(completedCount: integer; sprintDays: integer): double;
    function EstimateCompletionDays(remainingTasks: integer;
                                    currentVelocity: double): integer;
    function CalculateBurndownData(totalTasks: integer;
                                   completedArray: array of integer): TBurndownArray;
    function AnalyzeTrend(values: array of double): TTrendAnalysis;
    function GetProductivityMetrics(completedDaily: array of integer): TAnalyticsMetricArray;
    function CalculateTaskComplexity(estimatedHours: double;
                                     actualHours: double): double;
    function GetAccuracyScore(estimatedHours: double;
                              actualHours: double): double;
    function CalculateTeamCapacity(memberCount: integer;
                                   hoursPerMember: double): double;
    function PredictDeadlineMiss(tasksRemaining: integer;
                                 daysRemaining: integer;
                                 velocity: double): boolean;
    function CalculateRiskScore(overdueCount: integer;
                                blockedCount: integer;
                                totalCount: integer): double;
    procedure SelfTest();
  end;

implementation

constructor TTaskAnalyticsClass.Create();
begin
  inherited Create();
end;

destructor TTaskAnalyticsClass.Destroy();
begin
  inherited Destroy();
end;

function TTaskAnalyticsClass.CalculateStandardDeviation(values: array of double): double;
var
  i: integer;
  avg, sum: double;
  count: integer;
begin
  count := Length(values);
  if count <= 1 then
  begin
    result := 0;
    exit;
  end;

  { Calculate average }
  sum := 0;
  for i := 0 to count - 1 do
    sum := sum + values[i];
  avg := sum / count;

  { Calculate variance }
  sum := 0;
  for i := 0 to count - 1 do
    sum := sum + Power(values[i] - avg, 2);

  { Return standard deviation }
  result := Sqrt(sum / count);
end;

function TTaskAnalyticsClass.CalculateProductivity(totalTasks, completedTasks: integer;
                                                    days: integer): double;
begin
  if days <= 0 then
  begin
    result := 0;
    exit;
  end;

  result := completedTasks / days;
end;

function TTaskAnalyticsClass.CalculateVelocity(completedCount: integer;
                                               sprintDays: integer): double;
begin
  if sprintDays <= 0 then
  begin
    result := 0;
    exit;
  end;

  result := completedCount / sprintDays;
end;

function TTaskAnalyticsClass.EstimateCompletionDays(remainingTasks: integer;
                                                     currentVelocity: double): integer;
begin
  if currentVelocity <= 0 then
  begin
    result := -1;
    exit;
  end;

  result := Trunc(remainingTasks / currentVelocity);
  if remainingTasks mod Trunc(currentVelocity) > 0 then
    inc(result);
end;

function TTaskAnalyticsClass.CalculateBurndownData(totalTasks: integer;
                                                    completedArray: array of integer): TBurndownArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := Length(completedArray);

  if count = 0 then
    exit;

  SetLength(result, count);
  for i := 0 to count - 1 do
  begin
    result[i].day := i + 1;
    result[i].tasksRemaining := totalTasks - completedArray[i];
    result[i].estimatedHours := (totalTasks - completedArray[i]) * 2.5;  { Estimate 2.5 hours per task }
  end;
end;

function TTaskAnalyticsClass.AnalyzeTrend(values: array of double): TTrendAnalysis;
var
  i: integer;
  sum, avg, max_val, min_val: double;
  count: integer;
begin
  FillChar(result, SizeOf(TTrendAnalysis), 0);
  count := Length(values);

  if count = 0 then
  begin
    result.trend := 'stable';
    exit;
  end;

  { Calculate average }
  sum := 0;
  max_val := values[0];
  min_val := values[0];

  for i := 0 to count - 1 do
  begin
    sum := sum + values[i];
    if values[i] > max_val then
      max_val := values[i];
    if values[i] < min_val then
      min_val := values[i];
  end;

  result.average := sum / count;
  result.peak := max_val;
  result.minimum := min_val;

  { Determine trend }
  if count >= 2 then
  begin
    if values[count - 1] > values[0] then
      result.trend := 'increasing'
    else if values[count - 1] < values[0] then
      result.trend := 'decreasing'
    else
      result.trend := 'stable';
  end
  else
    result.trend := 'stable';
end;

function TTaskAnalyticsClass.GetProductivityMetrics(completedDaily: array of integer): TAnalyticsMetricArray;
var
  count, i, total: integer;
  avg, peak, min_val: integer;
begin
  SetLength(result, 0);
  count := Length(completedDaily);

  if count = 0 then
    exit;

  SetLength(result, 5);

  { Calculate metrics }
  total := 0;
  peak := 0;
  min_val := 999999;

  for i := 0 to count - 1 do
  begin
    total := total + completedDaily[i];
    if completedDaily[i] > peak then
      peak := completedDaily[i];
    if completedDaily[i] < min_val then
      min_val := completedDaily[i];
  end;

  avg := total div count;

  result[0].label_ := 'Total Completed';
  result[0].value := total;

  result[1].label_ := 'Average Per Day';
  result[1].value := avg;

  result[2].label_ := 'Peak Day';
  result[2].value := peak;

  result[3].label_ := 'Minimum Day';
  result[3].value := min_val;

  result[4].label_ := 'Days Tracked';
  result[4].value := count;
end;

function TTaskAnalyticsClass.CalculateTaskComplexity(estimatedHours: double;
                                                      actualHours: double): double;
begin
  if estimatedHours <= 0 then
  begin
    result := 0;
    exit;
  end;

  result := actualHours / estimatedHours;
end;

function TTaskAnalyticsClass.GetAccuracyScore(estimatedHours: double;
                                              actualHours: double): double;
var
  diff: double;
begin
  if estimatedHours <= 0 then
  begin
    result := 0;
    exit;
  end;

  diff := Abs(estimatedHours - actualHours);
  result := Max(0, 100 - (diff / estimatedHours * 100));
end;

function TTaskAnalyticsClass.CalculateTeamCapacity(memberCount: integer;
                                                    hoursPerMember: double): double;
begin
  result := memberCount * hoursPerMember * 5;  { 5 working days per week }
end;

function TTaskAnalyticsClass.PredictDeadlineMiss(tasksRemaining: integer;
                                                 daysRemaining: integer;
                                                 velocity: double): boolean;
var
  daysNeeded: integer;
begin
  if velocity <= 0 then
  begin
    result := true;
    exit;
  end;

  daysNeeded := Trunc(tasksRemaining / velocity);
  result := daysNeeded > daysRemaining;
end;

function TTaskAnalyticsClass.CalculateRiskScore(overdueCount: integer;
                                                blockedCount: integer;
                                                totalCount: integer): double;
var
  overdueRatio, blockedRatio: double;
begin
  if totalCount <= 0 then
  begin
    result := 0;
    exit;
  end;

  overdueRatio := overdueCount / totalCount;
  blockedRatio := blockedCount / totalCount;

  result := (overdueRatio * 60) + (blockedRatio * 40);
  if result > 100 then
    result := 100;
end;

procedure TTaskAnalyticsClass.SelfTest();
var
  productivity, velocity, accuracy: double;
  daysToComplete: integer;
  willMiss: boolean;
  riskScore: double;
  metrics: TAnalyticsMetricArray;
  i: integer;
  completedDaily: array[0..4] of integer;
  trend: TTrendAnalysis;
  trendValues: array[0..4] of double;
begin
  WriteLn('');
  WriteLn('=== TaskAnalytics Self Test ===');

  { Test productivity calculation }
  productivity := CalculateProductivity(20, 15, 5);
  WriteLn('Productivity (20 tasks, 15 completed, 5 days): ' + FloatToStr(productivity) + ' tasks/day');

  { Test velocity }
  velocity := CalculateVelocity(25, 10);
  WriteLn('Velocity (25 completed in 10 days): ' + FloatToStr(velocity) + ' tasks/day');

  { Test completion estimate }
  daysToComplete := EstimateCompletionDays(30, velocity);
  WriteLn('Days to complete 30 tasks at ' + FloatToStr(velocity) + ' velocity: ' + IntToStr(daysToComplete));

  { Test deadline prediction }
  willMiss := PredictDeadlineMiss(20, 5, 3.0);
  if willMiss then
    WriteLn('✓ Deadline miss predicted for 20 tasks in 5 days at 3 tasks/day velocity')
  else
    WriteLn('No deadline miss predicted');

  { Test risk score }
  riskScore := CalculateRiskScore(5, 2, 50);
  WriteLn('Risk score (5 overdue, 2 blocked of 50 total): ' + FloatToStr(riskScore) + '%');

  { Test accuracy }
  accuracy := GetAccuracyScore(10.0, 12.5);
  WriteLn('Estimation accuracy (10h estimated, 12.5h actual): ' + FloatToStr(accuracy) + '%');

  { Test productivity metrics }
  completedDaily[0] := 5;
  completedDaily[1] := 8;
  completedDaily[2] := 6;
  completedDaily[3] := 7;
  completedDaily[4] := 9;
  metrics := GetProductivityMetrics(completedDaily);
  WriteLn('Productivity metrics: Total = ' + FloatToStr(metrics[0].value) + ', Avg = ' + FloatToStr(metrics[1].value));

  { Test trend analysis }
  trendValues[0] := 10.0;
  trendValues[1] := 12.0;
  trendValues[2] := 15.0;
  trendValues[3] := 14.0;
  trendValues[4] := 18.0;
  trend := AnalyzeTrend(trendValues);
  WriteLn('Trend analysis: ' + trend.trend + ' (avg=' + FloatToStr(trend.average) + ')');

  WriteLn('=== TaskAnalytics Self Test Complete ===');
end;

end.
