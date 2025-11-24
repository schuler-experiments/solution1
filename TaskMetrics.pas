
unit TaskMetrics;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, Math, TaskTypes;

type
  { Metrics for a single task }
  TTaskMetrics = record
    taskId: integer;
    estimatedHours: double;
    actualHours: double;
    completionRate: double;
    efficiency: double;
    daysOverdue: integer;
    isOnTrack: boolean;
  end;

  TTaskMetricsArray = array of TTaskMetrics;

  { Daily productivity metrics }
  TDailyMetrics = record
    date: TDateTime;
    tasksCompleted: integer;
    tasksStarted: integer;
    totalHoursLogged: double;
    averageTaskDuration: double;
  end;

  TDailyMetricsArray = array of TDailyMetrics;

  { Team member productivity metrics }
  TTeamMemberMetrics = record
    memberId: integer;
    memberName: string;
    tasksCompleted: integer;
    tasksInProgress: integer;
    totalHoursLogged: double;
    averageCompletionTime: double;
    productivityScore: double;
  end;

  TTeamMemberMetricsArray = array of TTeamMemberMetrics;

  { Metrics aggregation and analysis }
  TTaskMetricsManagerClass = class
  private
    taskMetrics: TTaskMetricsArray;
    dailyMetrics: TDailyMetricsArray;
    teamMetrics: TTeamMemberMetricsArray;
    nextId: integer;
    function FindTaskMetricIndex(taskId: integer): integer;
    function FindDailyMetricIndex(date: TDateTime): integer;
    function FindTeamMetricIndex(memberId: integer): integer;
  public
    constructor Create();
    destructor Destroy(); override;

    { Task metrics management }
    function AddTaskMetric(taskId: integer; estimated, actual: double;
                          completionRate: double; daysOverdue: integer): integer;
    function GetTaskMetric(taskId: integer): TTaskMetrics;
    function GetAllTaskMetrics(): TTaskMetricsArray;
    function UpdateTaskMetric(taskId: integer; newMetric: TTaskMetrics): boolean;

    { Daily metrics management }
    function RecordDailyMetrics(date: TDateTime; completed, started: integer;
                               hoursLogged: double): integer;
    function GetDailyMetric(date: TDateTime): TDailyMetrics;
    function GetDailyMetricsRange(startDate, endDate: TDateTime): TDailyMetricsArray;

    { Team metrics management }
    function AddTeamMemberMetric(memberId: integer; memberName: string;
                                completed, inProgress: integer;
                                hoursLogged, avgTime, prodScore: double): integer;
    function GetTeamMemberMetric(memberId: integer): TTeamMemberMetrics;
    function GetAllTeamMetrics(): TTeamMemberMetricsArray;
    function GetTopPerformers(limit: integer): TTeamMemberMetricsArray;

    { Analysis and calculations }
    function CalculateOverallProductivity(): double;
    function CalculateTeamVelocity(): double;
    function GetAverageTaskCompletionTime(): double;
    function GetTaskEfficiencyRate(): double;
    function CalculateProjectHealth(): string;
    function GetProductivityTrend(days: integer): double;
    function IdentifyBottlenecks(): string;
    function GetTeamCapacityUtilization(): double;
    function ProjectedCompletionDate(remainingTasks: integer): TDateTime;
    function CalculateQualityMetrics(): string;
    function CompareTeamMembersPerformance(): string;
    function GetProductivityInsights(): string;

    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskMetricsManagerClass.Create();
begin
  inherited Create();
  SetLength(taskMetrics, 0);
  SetLength(dailyMetrics, 0);
  SetLength(teamMetrics, 0);
  nextId := 1;
end;

destructor TTaskMetricsManagerClass.Destroy();
begin
  SetLength(taskMetrics, 0);
  SetLength(dailyMetrics, 0);
  SetLength(teamMetrics, 0);
  inherited Destroy();
end;

function TTaskMetricsManagerClass.FindTaskMetricIndex(taskId: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(taskMetrics) - 1 do
  begin
    if taskMetrics[i].taskId = taskId then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TTaskMetricsManagerClass.FindDailyMetricIndex(date: TDateTime): integer;
var
  i: integer;
  dateOnly: TDateTime;
begin
  result := -1;
  dateOnly := Trunc(date);
  for i := 0 to Length(dailyMetrics) - 1 do
  begin
    if Trunc(dailyMetrics[i].date) = dateOnly then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TTaskMetricsManagerClass.FindTeamMetricIndex(memberId: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(teamMetrics) - 1 do
  begin
    if teamMetrics[i].memberId = memberId then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TTaskMetricsManagerClass.AddTaskMetric(taskId: integer; estimated, actual: double;
                                               completionRate: double; daysOverdue: integer): integer;
var
  idx: integer;
  newMetric: TTaskMetrics;
begin
  idx := FindTaskMetricIndex(taskId);
  if idx >= 0 then
  begin
    result := -1;
    Exit;
  end;

  newMetric.taskId := taskId;
  newMetric.estimatedHours := estimated;
  newMetric.actualHours := actual;
  newMetric.completionRate := completionRate;
  if actual > 0 then
    newMetric.efficiency := estimated / actual
  else
    newMetric.efficiency := 0;
  newMetric.daysOverdue := daysOverdue;
  newMetric.isOnTrack := daysOverdue <= 0;

  SetLength(taskMetrics, Length(taskMetrics) + 1);
  taskMetrics[Length(taskMetrics) - 1] := newMetric;
  result := nextId;
  Inc(nextId);
end;

function TTaskMetricsManagerClass.GetTaskMetric(taskId: integer): TTaskMetrics;
var
  idx: integer;
begin
  idx := FindTaskMetricIndex(taskId);
  if idx >= 0 then
    result := taskMetrics[idx]
  else
  begin
    result.taskId := -1;
    result.efficiency := 0;
  end;
end;

function TTaskMetricsManagerClass.GetAllTaskMetrics(): TTaskMetricsArray;
begin
  result := Copy(taskMetrics, 0, Length(taskMetrics));
end;

function TTaskMetricsManagerClass.UpdateTaskMetric(taskId: integer; newMetric: TTaskMetrics): boolean;
var
  idx: integer;
begin
  idx := FindTaskMetricIndex(taskId);
  if idx >= 0 then
  begin
    taskMetrics[idx] := newMetric;
    result := True;
  end
  else
    result := False;
end;

function TTaskMetricsManagerClass.RecordDailyMetrics(date: TDateTime; completed, started: integer;
                                                    hoursLogged: double): integer;
var
  idx: integer;
  newMetric: TDailyMetrics;
begin
  idx := FindDailyMetricIndex(date);
  if idx >= 0 then
  begin
    result := -1;
    Exit;
  end;

  newMetric.date := date;
  newMetric.tasksCompleted := completed;
  newMetric.tasksStarted := started;
  newMetric.totalHoursLogged := hoursLogged;
  if completed > 0 then
    newMetric.averageTaskDuration := hoursLogged / completed
  else
    newMetric.averageTaskDuration := 0;

  SetLength(dailyMetrics, Length(dailyMetrics) + 1);
  dailyMetrics[Length(dailyMetrics) - 1] := newMetric;
  result := nextId;
  Inc(nextId);
end;

function TTaskMetricsManagerClass.GetDailyMetric(date: TDateTime): TDailyMetrics;
var
  idx: integer;
begin
  idx := FindDailyMetricIndex(date);
  if idx >= 0 then
    result := dailyMetrics[idx]
  else
  begin
    result.date := date;
    result.tasksCompleted := 0;
    result.totalHoursLogged := 0;
  end;
end;

function TTaskMetricsManagerClass.GetDailyMetricsRange(startDate, endDate: TDateTime): TDailyMetricsArray;
var
  i: integer;
begin
  SetLength(result, 0);
  for i := 0 to Length(dailyMetrics) - 1 do
  begin
    if (dailyMetrics[i].date >= startDate) and (dailyMetrics[i].date <= endDate) then
    begin
      SetLength(result, Length(result) + 1);
      result[Length(result) - 1] := dailyMetrics[i];
    end;
  end;
end;

function TTaskMetricsManagerClass.AddTeamMemberMetric(memberId: integer; memberName: string;
                                                     completed, inProgress: integer;
                                                     hoursLogged, avgTime, prodScore: double): integer;
var
  idx: integer;
  newMetric: TTeamMemberMetrics;
begin
  idx := FindTeamMetricIndex(memberId);
  if idx >= 0 then
  begin
    result := -1;
    Exit;
  end;

  newMetric.memberId := memberId;
  newMetric.memberName := memberName;
  newMetric.tasksCompleted := completed;
  newMetric.tasksInProgress := inProgress;
  newMetric.totalHoursLogged := hoursLogged;
  newMetric.averageCompletionTime := avgTime;
  newMetric.productivityScore := prodScore;

  SetLength(teamMetrics, Length(teamMetrics) + 1);
  teamMetrics[Length(teamMetrics) - 1] := newMetric;
  result := nextId;
  Inc(nextId);
end;

function TTaskMetricsManagerClass.GetTeamMemberMetric(memberId: integer): TTeamMemberMetrics;
var
  idx: integer;
begin
  idx := FindTeamMetricIndex(memberId);
  if idx >= 0 then
    result := teamMetrics[idx]
  else
  begin
    result.memberId := -1;
    result.productivityScore := 0;
  end;
end;

function TTaskMetricsManagerClass.GetAllTeamMetrics(): TTeamMemberMetricsArray;
begin
  result := Copy(teamMetrics, 0, Length(teamMetrics));
end;

function TTaskMetricsManagerClass.GetTopPerformers(limit: integer): TTeamMemberMetricsArray;
var
  i, j, count: integer;
  temp: TTeamMemberMetrics;
begin
  SetLength(result, 0);
  if Length(teamMetrics) = 0 then
    Exit;

  SetLength(result, Length(teamMetrics));
  for i := 0 to Length(teamMetrics) - 1 do
    result[i] := teamMetrics[i];

  for i := 0 to Length(result) - 2 do
  begin
    for j := i + 1 to Length(result) - 1 do
    begin
      if result[j].productivityScore > result[i].productivityScore then
      begin
        temp := result[i];
        result[i] := result[j];
        result[j] := temp;
      end;
    end;
  end;

  if limit < Length(result) then
    SetLength(result, limit);
end;

function TTaskMetricsManagerClass.CalculateOverallProductivity(): double;
var
  i: integer;
  totalScore: double;
begin
  result := 0;
  if Length(teamMetrics) = 0 then
    Exit;

  totalScore := 0;
  for i := 0 to Length(teamMetrics) - 1 do
    totalScore := totalScore + teamMetrics[i].productivityScore;

  result := totalScore / Length(teamMetrics);
end;

function TTaskMetricsManagerClass.CalculateTeamVelocity(): double;
var
  i: integer;
  totalCompleted: integer;
begin
  totalCompleted := 0;
  for i := 0 to Length(teamMetrics) - 1 do
    totalCompleted := totalCompleted + teamMetrics[i].tasksCompleted;

  if Length(teamMetrics) > 0 then
    result := totalCompleted / Length(teamMetrics)
  else
    result := 0;
end;

function TTaskMetricsManagerClass.GetAverageTaskCompletionTime(): double;
var
  i: integer;
  totalTime: double;
begin
  totalTime := 0;
  for i := 0 to Length(taskMetrics) - 1 do
    totalTime := totalTime + taskMetrics[i].actualHours;

  if Length(taskMetrics) > 0 then
    result := totalTime / Length(taskMetrics)
  else
    result := 0;
end;

function TTaskMetricsManagerClass.GetTaskEfficiencyRate(): double;
var
  i: integer;
  totalEfficiency: double;
  count: integer;
begin
  result := 0;
  totalEfficiency := 0;
  count := 0;
  for i := 0 to Length(taskMetrics) - 1 do
  begin
    if taskMetrics[i].efficiency > 0 then
    begin
      totalEfficiency := totalEfficiency + taskMetrics[i].efficiency;
      Inc(count);
    end;
  end;

  if count > 0 then
    result := totalEfficiency / count
  else
    result := 0;
end;

function TTaskMetricsManagerClass.CalculateProjectHealth(): string;
var
  productivity: double;
  efficiency: double;
begin
  productivity := CalculateOverallProductivity();
  efficiency := GetTaskEfficiencyRate();

  if (productivity > 0.75) and (efficiency > 0.9) then
    result := 'Excellent'
  else if (productivity > 0.5) and (efficiency > 0.7) then
    result := 'Good'
  else if (productivity > 0.25) and (efficiency > 0.5) then
    result := 'Fair'
  else
    result := 'Needs Improvement';
end;

function TTaskMetricsManagerClass.GetProductivityTrend(days: integer): double;
var
  i: integer;
  count: integer;
  recentHours: double;
  startDate: TDateTime;
begin
  result := 0;
  startDate := Now - days;
  recentHours := 0;
  count := 0;

  for i := 0 to Length(dailyMetrics) - 1 do
  begin
    if dailyMetrics[i].date >= startDate then
    begin
      recentHours := recentHours + dailyMetrics[i].totalHoursLogged;
      Inc(count);
    end;
  end;

  if count > 0 then
    result := recentHours / count
  else
    result := 0;
end;

function TTaskMetricsManagerClass.IdentifyBottlenecks(): string;
var
  i: integer;
  maxOverdue: integer;
  bottleneckFound: boolean;
begin
  result := '';
  maxOverdue := 0;
  bottleneckFound := False;

  for i := 0 to Length(taskMetrics) - 1 do
  begin
    if taskMetrics[i].daysOverdue > maxOverdue then
    begin
      maxOverdue := taskMetrics[i].daysOverdue;
      bottleneckFound := True;
    end;
  end;

  if bottleneckFound then
    result := 'High overdue count detected: ' + IntToStr(maxOverdue) + ' days'
  else
    result := 'No major bottlenecks detected';
end;

function TTaskMetricsManagerClass.GetTeamCapacityUtilization(): double;
var
  i: integer;
  totalTasks: integer;
  totalCapacity: integer;
begin
  result := 0;
  totalTasks := 0;
  totalCapacity := Length(teamMetrics) * 10;

  if totalCapacity = 0 then
    Exit;

  for i := 0 to Length(teamMetrics) - 1 do
    totalTasks := totalTasks + teamMetrics[i].tasksInProgress;

  result := (totalTasks / totalCapacity) * 100;
end;

function TTaskMetricsManagerClass.ProjectedCompletionDate(remainingTasks: integer): TDateTime;
var
  velocity: double;
  daysNeeded: double;
begin
  velocity := CalculateTeamVelocity();
  if velocity > 0 then
    daysNeeded := remainingTasks / velocity
  else
    daysNeeded := 0;

  result := Now + Trunc(daysNeeded);
end;

function TTaskMetricsManagerClass.CalculateQualityMetrics(): string;
var
  onTrackCount: integer;
  i: integer;
begin
  onTrackCount := 0;
  for i := 0 to Length(taskMetrics) - 1 do
  begin
    if taskMetrics[i].isOnTrack then
      Inc(onTrackCount);
  end;

  if Length(taskMetrics) > 0 then
    result := 'On-track tasks: ' + IntToStr(onTrackCount) + '/' + IntToStr(Length(taskMetrics))
  else
    result := 'No task metrics available';
end;

function TTaskMetricsManagerClass.CompareTeamMembersPerformance(): string;
var
  i: integer;
  topPerformers: TTeamMemberMetricsArray;
begin
  result := 'Team Performance Comparison:' + #10#13;
  topPerformers := GetTopPerformers(3);

  for i := 0 to Length(topPerformers) - 1 do
  begin
    result := result + topPerformers[i].memberName + ' - Score: ' +
              FormatFloat('0.00', topPerformers[i].productivityScore) + #10#13;
  end;

  SetLength(topPerformers, 0);
end;

function TTaskMetricsManagerClass.GetProductivityInsights(): string;
begin
  result := 'Productivity Insights:' + #10#13;
  result := result + 'Overall Productivity: ' +
            FormatFloat('0.00', CalculateOverallProductivity()) + #10#13;
  result := result + 'Team Velocity: ' +
            FormatFloat('0.00', CalculateTeamVelocity()) + ' tasks/member' + #10#13;
  result := result + 'Project Health: ' + CalculateProjectHealth() + #10#13;
  result := result + 'Capacity Utilization: ' +
            FormatFloat('0.00', GetTeamCapacityUtilization()) + '%' + #10#13;
  result := result + IdentifyBottlenecks() + #10#13;
end;

procedure TTaskMetricsManagerClass.SelfTest();
var
  mgr: TTaskMetricsManagerClass;
  i: integer;
  allMetrics: TTaskMetricsArray;
  allTeam: TTeamMemberMetricsArray;
  topPerformers: TTeamMemberMetricsArray;
begin
  WriteLn('');
  WriteLn('=== TaskMetrics Self Test ===');

  mgr := TTaskMetricsManagerClass.Create();

  WriteLn('Adding task metrics...');
  mgr.AddTaskMetric(1, 8.0, 7.5, 100, 0);
  mgr.AddTaskMetric(2, 10.0, 12.5, 85, 2);
  mgr.AddTaskMetric(3, 5.0, 4.8, 100, 0);
  WriteLn('Total task metrics: ', Length(mgr.GetAllTaskMetrics()));

  WriteLn('');
  WriteLn('Adding daily metrics...');
  mgr.RecordDailyMetrics(Now, 3, 2, 8.0);
  mgr.RecordDailyMetrics(Now - 1, 2, 3, 7.5);
  mgr.RecordDailyMetrics(Now - 2, 4, 1, 9.0);

  WriteLn('');
  WriteLn('Adding team member metrics...');
  mgr.AddTeamMemberMetric(1, 'Alice', 15, 2, 120.0, 8.0, 0.85);
  mgr.AddTeamMemberMetric(2, 'Bob', 12, 4, 110.0, 9.2, 0.78);
  mgr.AddTeamMemberMetric(3, 'Charlie', 18, 1, 140.0, 7.8, 0.92);
  WriteLn('Total team metrics: ', Length(mgr.GetAllTeamMetrics()));

  WriteLn('');
  WriteLn('Performance Analysis:');
  WriteLn('  Overall Productivity: ', FormatFloat('0.00', mgr.CalculateOverallProductivity()));
  WriteLn('  Team Velocity: ', FormatFloat('0.00', mgr.CalculateTeamVelocity()), ' tasks/member');
  WriteLn('  Task Efficiency: ', FormatFloat('0.00', mgr.GetTaskEfficiencyRate()));
  WriteLn('  Project Health: ', mgr.CalculateProjectHealth());
  WriteLn('  Capacity Utilization: ', FormatFloat('0.00', mgr.GetTeamCapacityUtilization()), '%');

  WriteLn('');
  WriteLn('Top Performers:');
  topPerformers := mgr.GetTopPerformers(2);
  for i := 0 to Length(topPerformers) - 1 do
  begin
    WriteLn('  ', topPerformers[i].memberName, ' - Score: ',
            FormatFloat('0.00', topPerformers[i].productivityScore));
  end;

  WriteLn('');
  WriteLn(mgr.GetProductivityInsights());

  WriteLn('Quality Metrics:');
  WriteLn(mgr.CalculateQualityMetrics());

  WriteLn('');
  WriteLn('Projected Completion Date: ',
          FormatDateTime('yyyy-mm-dd', mgr.ProjectedCompletionDate(10)));

  mgr.Free();
  SetLength(topPerformers, 0);

  WriteLn('=== TaskMetrics Self Test Complete ===');
  WriteLn('');
end;

end.
