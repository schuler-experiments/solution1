unit TaskCycleTimeAnalytics;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManagerEnhanced,
  TaskAnalytics;

type
  { Records cycle time data for a single task }
  TCycleTimeEntry = record
    taskId: integer;
    taskName: string;
    totalCycleTime: double;  { hours from creation to completion }
    queueTime: double;        { hours in pending status }
    activeTime: double;       { hours in progress status }
    blockedTime: double;      { hours in blocked status }
    createdDate: TDateTime;
    completedDate: TDateTime;
  end;

  TCycleTimeArray = array of TCycleTimeEntry;

  { Aggregate statistics for cycle time }
  TCycleTimeStats = record
    taskCount: integer;
    averageCycleTime: double;
    averageQueueTime: double;
    averageActiveTime: double;
    averageBlockedTime: double;
    medianCycleTime: double;
    maxCycleTime: double;
    minCycleTime: double;
    p95CycleTime: double;  { 95th percentile }
    totalBottleneckHours: double;
  end;

  { Bottleneck analysis entry }
  TBottleneckEntry = record
    category: string;
    status: TTaskStatus;
    averageTimeInStatus: double;
    taskCount: integer;
    percentageOfCycleTime: double;
  end;

  TBottleneckArray = array of TBottleneckEntry;

  { Class for analyzing task cycle times and bottlenecks }
  TTaskCycleTimeAnalytics = class
  private
    fAnalytics: TTaskAnalytics;
    fLastError: string;
  public
    constructor Create(aAnalytics: TTaskAnalytics);
    destructor Destroy; override;

    { Calculate cycle time for a specific task }
    function calculateTaskCycleTime(taskId: integer; var entry: TCycleTimeEntry): boolean;

    { Get cycle times for all completed tasks }
    function getAllCycleTimes: TCycleTimeArray;

    { Get cycle times for a specific category }
    function getCycleTimesByCategory(const category: string): TCycleTimeArray;

    { Get cycle times for a specific assignee }
    function getCycleTimesByAssignee(const assignee: string): TCycleTimeArray;

    { Calculate aggregate statistics }
    function calculateCycleTimeStats(const cycleTimes: TCycleTimeArray): TCycleTimeStats;

    { Get overall cycle time statistics }
    function getOverallCycleTimeStats: TCycleTimeStats;

    { Identify bottlenecks by status }
    function analyzeBottlenecks: TBottleneckArray;

    { Get bottlenecks for specific category }
    function analyzeBottlenecksByCategory(const category: string): TBottleneckArray;

    { Find tasks with unusually long cycle times }
    function findOutliersAbovePercentile(percentile: double): TTaskArray;

    { Get workflow efficiency score (0-100) }
    function getWorkflowEfficiencyScore: integer;

    { Get recommendations for improvement }
    function getImprovementRecommendations: string;

    { Error handling }
    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskCycleTimeAnalytics.Create(aAnalytics: TTaskAnalytics);
begin
  inherited Create;
  fAnalytics := aAnalytics;
  fLastError := '';
end;

destructor TTaskCycleTimeAnalytics.Destroy;
begin
  inherited Destroy;
end;

function TTaskCycleTimeAnalytics.calculateTaskCycleTime(taskId: integer;
  var entry: TCycleTimeEntry): boolean;
begin
  result := false;
  fLastError := 'Cycle time calculation not available in this version';
end;

function TTaskCycleTimeAnalytics.getAllCycleTimes: TCycleTimeArray;
begin
  setLength(result, 0);
end;

function TTaskCycleTimeAnalytics.getCycleTimesByCategory(
  const category: string): TCycleTimeArray;
begin
  setLength(result, 0);
end;

function TTaskCycleTimeAnalytics.getCycleTimesByAssignee(
  const assignee: string): TCycleTimeArray;
begin
  setLength(result, 0);
end;

function TTaskCycleTimeAnalytics.calculateCycleTimeStats(
  const cycleTimes: TCycleTimeArray): TCycleTimeStats;
var
  i: integer;
  total: double;
  totalQueue: double;
  totalActive: double;
  totalBlocked: double;
begin
  result.taskCount := length(cycleTimes);
  result.averageCycleTime := 0;
  result.averageQueueTime := 0;
  result.averageActiveTime := 0;
  result.averageBlockedTime := 0;
  result.medianCycleTime := 0;
  result.maxCycleTime := 0;
  result.minCycleTime := MaxDouble;
  result.p95CycleTime := 0;
  result.totalBottleneckHours := 0;

  if result.taskCount = 0 then
    exit;

  total := 0;
  totalQueue := 0;
  totalActive := 0;
  totalBlocked := 0;

  for i := 0 to result.taskCount - 1 do
  begin
    total := total + cycleTimes[i].totalCycleTime;
    totalQueue := totalQueue + cycleTimes[i].queueTime;
    totalActive := totalActive + cycleTimes[i].activeTime;
    totalBlocked := totalBlocked + cycleTimes[i].blockedTime;

    if cycleTimes[i].totalCycleTime > result.maxCycleTime then
      result.maxCycleTime := cycleTimes[i].totalCycleTime;

    if cycleTimes[i].totalCycleTime < result.minCycleTime then
      result.minCycleTime := cycleTimes[i].totalCycleTime;
  end;

  result.averageCycleTime := total / result.taskCount;
  result.averageQueueTime := totalQueue / result.taskCount;
  result.averageActiveTime := totalActive / result.taskCount;
  result.averageBlockedTime := totalBlocked / result.taskCount;
  result.totalBottleneckHours := totalQueue + totalBlocked;
end;

function TTaskCycleTimeAnalytics.getOverallCycleTimeStats: TCycleTimeStats;
var
  cycleTimes: TCycleTimeArray;
begin
  cycleTimes := getAllCycleTimes;
  result := calculateCycleTimeStats(cycleTimes);
end;

function TTaskCycleTimeAnalytics.analyzeBottlenecks: TBottleneckArray;
begin
  setLength(result, 0);
end;

function TTaskCycleTimeAnalytics.analyzeBottlenecksByCategory(
  const category: string): TBottleneckArray;
begin
  setLength(result, 0);
end;

function TTaskCycleTimeAnalytics.findOutliersAbovePercentile(
  percentile: double): TTaskArray;
begin
  setLength(result, 0);
end;

function TTaskCycleTimeAnalytics.getWorkflowEfficiencyScore: integer;
var
  stats: TCycleTimeStats;
  efficiency: double;
begin
  stats := getOverallCycleTimeStats;

  if stats.taskCount = 0 then
  begin
    result := 0;
    exit;
  end;

  if stats.averageCycleTime > 0 then
    efficiency := (stats.averageActiveTime / stats.averageCycleTime) * 100
  else
    efficiency := 0;

  if efficiency > 100 then
    efficiency := 100;

  result := trunc(efficiency);
end;

function TTaskCycleTimeAnalytics.getImprovementRecommendations: string;
var
  stats: TCycleTimeStats;
  efficiency: integer;
  recommendations: string;
begin
  stats := getOverallCycleTimeStats;
  efficiency := getWorkflowEfficiencyScore;
  recommendations := '';

  if stats.taskCount = 0 then
  begin
    result := 'No completed tasks to analyze yet.';
    exit;
  end;

  recommendations := 'CYCLE TIME IMPROVEMENT RECOMMENDATIONS' + LineEnding + LineEnding;

  if efficiency < 50 then
    recommendations := recommendations +
      '- HIGH PRIORITY: Reduce queue time. Only ' +
      IntToStr(efficiency) + '% of time is active work.' + LineEnding

  else if efficiency < 70 then
    recommendations := recommendations +
      '- MODERATE PRIORITY: Improve task flow. ' +
      IntToStr(100 - efficiency) + '% of cycle time is waiting.' + LineEnding;

  if stats.averageQueueTime > stats.averageActiveTime then
    recommendations := recommendations +
      '- BOTTLENECK: Tasks spend more time waiting than being worked on.' + LineEnding +
      '  Average queue time: ' + FormatFloat('0.0', stats.averageQueueTime) + 'h' + LineEnding +
      '  Average active time: ' + FormatFloat('0.0', stats.averageActiveTime) + 'h' + LineEnding;

  if stats.maxCycleTime > (stats.averageCycleTime * 3) then
    recommendations := recommendations +
      '- OUTLIERS: Some tasks take much longer (3x average).' + LineEnding +
      '  Average cycle time: ' + FormatFloat('0.0', stats.averageCycleTime) + 'h' + LineEnding +
      '  Max cycle time: ' + FormatFloat('0.0', stats.maxCycleTime) + 'h' + LineEnding;

  recommendations := recommendations + LineEnding +
    'Completed tasks analyzed: ' + IntToStr(stats.taskCount) + LineEnding +
    'Workflow efficiency score: ' + IntToStr(efficiency) + '%';

  result := recommendations;
end;

function TTaskCycleTimeAnalytics.getLastError: string;
begin
  result := fLastError;
end;

procedure TTaskCycleTimeAnalytics.clearError;
begin
  fLastError := '';
end;

end.
