
unit TaskPriorityScoring;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, Math, TaskTypes;

type
  { Priority score factors }
  TPriorityScoreFactor = record
    baseScore: double;           { Base priority score (1-10) }
    urgencyScore: double;        { Urgency based on due date (0-5) }
    impactScore: double;         { Business impact (0-5) }
    dependencyScore: double;     { Number of dependent tasks (0-5) }
    riskScore: double;           { Risk level (0-3) }
    estimatedHoursScore: double; { Time investment (0-2) }
  end;

  TPriorityScoreArray = array of TPriorityScoreFactor;

  { Priority scoring engine }
  TTaskPriorityScorerClass = class
  private
    scores: TPriorityScoreArray;
    taskPriorities: array of integer;
    
    function CalculateUrgencyScore(dueDate: TDateTime): double;
    function CalculateDependencyScore(dependentCount: integer): double;
    function CalculateTimeScore(estimatedHours: double): double;
  public
    constructor Create();
    destructor Destroy();
    
    function CalculatePriorityScore(task: TTask; 
                                     dependentCount: integer;
                                     riskLevel: double;
                                     daysUntilDue: integer): double;
    function ScoreTask(task: TTask; dependentCount: integer; 
                       riskLevel: double): double;
    function CompareTasks(task1, task2: TTask; 
                         dep1, dep2: integer; 
                         risk1, risk2: double): integer;
    function GetPriorityRecommendation(score: double): TTaskPriority;
    function GetScoreDetails(task: TTask; dependentCount: integer; 
                            riskLevel: double): TPriorityScoreFactor;
    function FindHighestPriorityTask(tasks: TTaskArray; 
                                     dependentCounts: array of integer;
                                     riskLevels: array of double): integer;
    procedure SelfTest();
  end;

implementation

constructor TTaskPriorityScorerClass.Create();
begin
  SetLength(scores, 0);
  SetLength(taskPriorities, 0);
end;

destructor TTaskPriorityScorerClass.Destroy();
begin
  SetLength(scores, 0);
  SetLength(taskPriorities, 0);
  inherited Destroy();
end;

function TTaskPriorityScorerClass.CalculateUrgencyScore(dueDate: TDateTime): double;
var
  daysUntilDue: integer;
begin
  if dueDate = 0 then
  begin
    result := 0;
    exit;
  end;

  daysUntilDue := Trunc(dueDate - Now());
  
  if daysUntilDue < 0 then
    result := 5.0  { Overdue }
  else if daysUntilDue = 0 then
    result := 5.0  { Due today }
  else if daysUntilDue = 1 then
    result := 4.5
  else if daysUntilDue <= 3 then
    result := 4.0
  else if daysUntilDue <= 7 then
    result := 3.0
  else if daysUntilDue <= 14 then
    result := 2.0
  else if daysUntilDue <= 30 then
    result := 1.0
  else
    result := 0.5;
end;

function TTaskPriorityScorerClass.CalculateDependencyScore(dependentCount: integer): double;
begin
  if dependentCount = 0 then
    result := 0
  else if dependentCount = 1 then
    result := 1.5
  else if dependentCount = 2 then
    result := 3.0
  else if dependentCount <= 5 then
    result := 4.0
  else
    result := 5.0;
end;

function TTaskPriorityScorerClass.CalculateTimeScore(estimatedHours: double): double;
begin
  if estimatedHours = 0 then
    result := 0.5
  else if estimatedHours <= 2 then
    result := 1.0
  else if estimatedHours <= 8 then
    result := 1.5
  else if estimatedHours <= 20 then
    result := 2.0
  else
    result := 2.0;
end;

function TTaskPriorityScorerClass.CalculatePriorityScore(task: TTask; 
                                                         dependentCount: integer;
                                                         riskLevel: double;
                                                         daysUntilDue: integer): double;
var
  baseScore: double;
  urgencyScore: double;
  dependencyScore: double;
  timeScore: double;
begin
  { Base score from priority level }
  case task.priority of
    tpLow: baseScore := 2.0;
    tpMedium: baseScore := 5.0;
    tpHigh: baseScore := 8.0;
  else
    baseScore := 5.0;
  end;

  urgencyScore := CalculateUrgencyScore(task.dueDate);
  dependencyScore := CalculateDependencyScore(dependentCount);
  timeScore := CalculateTimeScore(task.estimatedHours);

  { Weighted scoring }
  result := (baseScore * 0.3) + 
            (urgencyScore * 0.3) + 
            (dependencyScore * 0.2) + 
            (timeScore * 0.1) + 
            (Min(riskLevel, 3.0) * 0.1);
end;

function TTaskPriorityScorerClass.ScoreTask(task: TTask; dependentCount: integer; 
                                            riskLevel: double): double;
var
  daysUntilDue: integer;
begin
  if task.dueDate = 0 then
    daysUntilDue := 999
  else
    daysUntilDue := Trunc(task.dueDate - Now());

  result := CalculatePriorityScore(task, dependentCount, riskLevel, daysUntilDue);
end;

function TTaskPriorityScorerClass.CompareTasks(task1, task2: TTask; 
                                              dep1, dep2: integer; 
                                              risk1, risk2: double): integer;
var
  score1, score2: double;
begin
  score1 := ScoreTask(task1, dep1, risk1);
  score2 := ScoreTask(task2, dep2, risk2);

  if score1 > score2 then
    result := 1
  else if score1 < score2 then
    result := -1
  else
    result := 0;
end;

function TTaskPriorityScorerClass.GetPriorityRecommendation(score: double): TTaskPriority;
begin
  if score >= 7.0 then
    result := tpHigh
  else if score >= 4.0 then
    result := tpMedium
  else
    result := tpLow;
end;

function TTaskPriorityScorerClass.GetScoreDetails(task: TTask; dependentCount: integer; 
                                                  riskLevel: double): TPriorityScoreFactor;
var
  baseScore: double;
begin
  case task.priority of
    tpLow: baseScore := 2.0;
    tpMedium: baseScore := 5.0;
    tpHigh: baseScore := 8.0;
  else
    baseScore := 5.0;
  end;

  result.baseScore := baseScore;
  result.urgencyScore := CalculateUrgencyScore(task.dueDate);
  result.dependencyScore := CalculateDependencyScore(dependentCount);
  result.estimatedHoursScore := CalculateTimeScore(task.estimatedHours);
  result.riskScore := Min(riskLevel, 3.0);
  result.impactScore := 0;
end;

function TTaskPriorityScorerClass.FindHighestPriorityTask(tasks: TTaskArray; 
                                                         dependentCounts: array of integer;
                                                         riskLevels: array of double): integer;
var
  i: integer;
  maxScore: double;
  maxIdx: integer;
  currentScore: double;
begin
  maxScore := -1;
  maxIdx := -1;

  for i := 0 to Length(tasks) - 1 do
  begin
    if i < Length(dependentCounts) then
      currentScore := ScoreTask(tasks[i], dependentCounts[i], 
                               riskLevels[i])
    else
      currentScore := ScoreTask(tasks[i], 0, 0);

    if currentScore > maxScore then
    begin
      maxScore := currentScore;
      maxIdx := i;
    end;
  end;

  result := maxIdx;
end;

procedure TTaskPriorityScorerClass.SelfTest();
var
  task1, task2, task3: TTask;
  score1, score2, score3: double;
  deps: array [0..2] of integer;
  risks: array [0..2] of double;
  tasks: TTaskArray;
begin
  WriteLn('=== TaskPriorityScoring Self Test ===');
  
  { Create test tasks }
  task1 := CreateTask(1, 'Bug fix', 'Critical bug', tpHigh, Now() + 1);
  task1.estimatedHours := 4;
  
  task2 := CreateTask(2, 'Feature request', 'New feature', tpMedium, Now() + 10);
  task2.estimatedHours := 16;
  
  task3 := CreateTask(3, 'Documentation', 'Update docs', tpLow, Now() + 30);
  task3.estimatedHours := 2;
  
  { Score tasks }
  score1 := ScoreTask(task1, 5, 2.5);
  score2 := ScoreTask(task2, 0, 0.5);
  score3 := ScoreTask(task3, 0, 0.2);
  
  WriteLn('Task 1 (Bug fix) score: ', score1:5:2);
  WriteLn('Task 2 (Feature) score: ', score2:5:2);
  WriteLn('Task 3 (Documentation) score: ', score3:5:2);
  
  { Recommend priorities }
  WriteLn('Recommended priority for task1: ', PriorityToString(GetPriorityRecommendation(score1)));
  WriteLn('Recommended priority for task2: ', PriorityToString(GetPriorityRecommendation(score2)));
  WriteLn('Recommended priority for task3: ', PriorityToString(GetPriorityRecommendation(score3)));
  
  { Find highest priority }
  SetLength(tasks, 3);
  tasks[0] := task1;
  tasks[1] := task2;
  tasks[2] := task3;
  deps[0] := 5;
  deps[1] := 0;
  deps[2] := 0;
  risks[0] := 2.5;
  risks[1] := 0.5;
  risks[2] := 0.2;
  
  WriteLn('Highest priority task index: ', FindHighestPriorityTask(tasks, deps, risks));
  WriteLn('=== TaskPriorityScoring Self Test Complete ===');
end;

end.
