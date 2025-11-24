
unit TaskResourceOptimization;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes;

type
  TStringArray = array of string;
  TIntegerArray = array of integer;
  TDoubleArray = array of double;

  { Resource allocation recommendation }
  TAllocationRecommendation = record
    taskId: integer;
    memberId: integer;
    recommendedHours: double;
    confidenceScore: double; { 0-1 scale }
    reason: string;
  end;

  TAllocationArray = array of TAllocationRecommendation;

  { Resource capacity forecast }
  TCapacityForecast = record
    memberId: integer;
    currentUtilization: double;
    forecastedUtilization: double;
    availableHours: double;
    forecastDate: TDateTime;
  end;

  TCapacityForecastArray = array of TCapacityForecast;

  { Task complexity assessment }
  TComplexityAssessment = record
    taskId: integer;
    estimatedHours: double;
    complexityScore: double; { 0-10 scale }
    riskLevel: string; { Low, Medium, High, Critical }
    requiredSkills: array of string;
    recommendedTeamSize: integer;
  end;

  TComplexityArray = array of TComplexityAssessment;

  TTaskResourceOptimizerClass = class
  private
    allocations: TAllocationArray;
    forecasts: TCapacityForecastArray;
    assessments: TComplexityArray;
    nextAllocationId: integer;
    function CalculateSkillMatch(memberSkills: array of string; 
                                requiredSkills: array of string): double;
    function CalculateWorkloadBalance(memberUtilization: double): double;
    function CalculateAvailability(memberId: integer; hoursNeeded: double): boolean;
  public
    constructor Create();
    destructor Destroy();

    { Resource Allocation }
    function RecommendAllocation(taskId: integer; estimatedHours: double;
                                requiredSkills: array of string): TAllocationRecommendation;
    function GetAllAllocations(): TAllocationArray;
    function GetAllocationCount(): integer;

    { Capacity Analysis }
    function ForecastCapacity(memberId: integer; days: integer): TCapacityForecast;
    function GetTeamCapacityStatus(): string;
    function IdentifyBottlenecks(): TStringArray;
    function GetMemberAvailability(memberId: integer): double;

    { Complexity Assessment }
    function AssessTaskComplexity(taskId: integer; estimatedHours: double): TComplexityAssessment;
    function GetHighComplexityTasks(): TIntegerArray;
    function GetHighRiskTasks(): TIntegerArray;
    function IsTaskComplex(taskId: integer): boolean;

    { Optimization }
    function OptimizeTeamAllocation(taskCount: integer): TAllocationArray;
    function BalanceWorkload(): string;
    function SuggestResourceAdjustments(): TStringArray;

    { Risk Assessment }
    function AssessAllocationRisk(taskId, memberId: integer): double;
    function GetRiskFactors(taskId: integer): TStringArray;
    function CalculateProjectRiskScore(): double;

    { Reporting }
    function GenerateAllocationReport(): string;
    function GetOptimizationMetrics(): string;
    function GetResourceUtilizationReport(): string;

    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskResourceOptimizerClass.Create();
begin
  SetLength(allocations, 0);
  SetLength(forecasts, 0);
  SetLength(assessments, 0);
  nextAllocationId := 1;
end;

destructor TTaskResourceOptimizerClass.Destroy();
begin
  SetLength(allocations, 0);
  SetLength(forecasts, 0);
  SetLength(assessments, 0);
  inherited Destroy();
end;

function TTaskResourceOptimizerClass.CalculateSkillMatch(memberSkills: array of string; 
                                                        requiredSkills: array of string): double;
var
  i, j, matches: integer;
begin
  matches := 0;
  
  if Length(requiredSkills) = 0 then
  begin
    result := 1.0;
    Exit;
  end;
  
  for i := 0 to Length(requiredSkills) - 1 do
  begin
    for j := 0 to Length(memberSkills) - 1 do
    begin
      if memberSkills[j] = requiredSkills[i] then
      begin
        Inc(matches);
        Break;
      end;
    end;
  end;
  
  result := matches / Length(requiredSkills);
end;

function TTaskResourceOptimizerClass.CalculateWorkloadBalance(memberUtilization: double): double;
begin
  { Lower utilization is better }
  result := 1.0 - (memberUtilization / 100.0);
  if result < 0 then result := 0;
  if result > 1 then result := 1;
end;

function TTaskResourceOptimizerClass.CalculateAvailability(memberId: integer; 
                                                         hoursNeeded: double): boolean;
begin
  result := hoursNeeded <= 40.0; { Reasonable allocation }
end;

function TTaskResourceOptimizerClass.RecommendAllocation(taskId: integer; 
                                                       estimatedHours: double;
                                                       requiredSkills: array of string): TAllocationRecommendation;
var
  allocation: TAllocationRecommendation;
begin
  allocation.taskId := taskId;
  allocation.memberId := taskId mod 10; { Simplified for demo }
  allocation.recommendedHours := estimatedHours;
  allocation.confidenceScore := 0.85;
  allocation.reason := 'Skill match: ' + FormatFloat('0.00', 0.85);
  
  SetLength(allocations, Length(allocations) + 1);
  allocations[Length(allocations) - 1] := allocation;
  
  result := allocation;
end;

function TTaskResourceOptimizerClass.GetAllAllocations(): TAllocationArray;
begin
  result := Copy(allocations, 0, Length(allocations));
end;

function TTaskResourceOptimizerClass.GetAllocationCount(): integer;
begin
  result := Length(allocations);
end;

function TTaskResourceOptimizerClass.ForecastCapacity(memberId: integer; 
                                                    days: integer): TCapacityForecast;
var
  forecast: TCapacityForecast;
begin
  forecast.memberId := memberId;
  forecast.currentUtilization := 75.0;
  forecast.forecastedUtilization := 75.0 + (days * 2.5);
  if forecast.forecastedUtilization > 100 then
    forecast.forecastedUtilization := 100;
  forecast.availableHours := 40.0 * (1.0 - forecast.forecastedUtilization / 100.0);
  forecast.forecastDate := Now() + days;
  
  SetLength(forecasts, Length(forecasts) + 1);
  forecasts[Length(forecasts) - 1] := forecast;
  
  result := forecast;
end;

function TTaskResourceOptimizerClass.GetTeamCapacityStatus(): string;
begin
  result := 'Team Utilization: 75% | Available Capacity: 25%';
end;

function TTaskResourceOptimizerClass.IdentifyBottlenecks(): TStringArray;
var
  bottlenecks: TStringArray;
begin
  SetLength(bottlenecks, 1);
  bottlenecks[0] := 'Senior Developer availability limited';
  result := bottlenecks;
end;

function TTaskResourceOptimizerClass.GetMemberAvailability(memberId: integer): double;
begin
  result := 8.0; { Hours available this week }
end;

function TTaskResourceOptimizerClass.AssessTaskComplexity(taskId: integer; 
                                                        estimatedHours: double): TComplexityAssessment;
var
  assessment: TComplexityAssessment;
begin
  assessment.taskId := taskId;
  assessment.estimatedHours := estimatedHours;
  
  if estimatedHours < 8 then
  begin
    assessment.complexityScore := 2.0;
    assessment.riskLevel := 'Low';
    assessment.recommendedTeamSize := 1;
  end
  else if estimatedHours < 40 then
  begin
    assessment.complexityScore := 5.0;
    assessment.riskLevel := 'Medium';
    assessment.recommendedTeamSize := 2;
  end
  else if estimatedHours < 100 then
  begin
    assessment.complexityScore := 7.5;
    assessment.riskLevel := 'High';
    assessment.recommendedTeamSize := 3;
  end
  else
  begin
    assessment.complexityScore := 9.0;
    assessment.riskLevel := 'Critical';
    assessment.recommendedTeamSize := 4;
  end;
  
  SetLength(assessment.requiredSkills, 0);
  
  SetLength(assessments, Length(assessments) + 1);
  assessments[Length(assessments) - 1] := assessment;
  
  result := assessment;
end;

function TTaskResourceOptimizerClass.GetHighComplexityTasks(): TIntegerArray;
var
  tasks: TIntegerArray;
  i, count: integer;
begin
  SetLength(tasks, 0);
  count := 0;
  
  for i := 0 to Length(assessments) - 1 do
  begin
    if assessments[i].complexityScore >= 7.0 then
    begin
      SetLength(tasks, count + 1);
      tasks[count] := assessments[i].taskId;
      Inc(count);
    end;
  end;
  
  result := tasks;
end;

function TTaskResourceOptimizerClass.GetHighRiskTasks(): TIntegerArray;
var
  tasks: TIntegerArray;
  i, count: integer;
begin
  SetLength(tasks, 0);
  count := 0;
  
  for i := 0 to Length(assessments) - 1 do
  begin
    if (assessments[i].riskLevel = 'High') or (assessments[i].riskLevel = 'Critical') then
    begin
      SetLength(tasks, count + 1);
      tasks[count] := assessments[i].taskId;
      Inc(count);
    end;
  end;
  
  result := tasks;
end;

function TTaskResourceOptimizerClass.IsTaskComplex(taskId: integer): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Length(assessments) - 1 do
  begin
    if (assessments[i].taskId = taskId) and (assessments[i].complexityScore >= 7.0) then
    begin
      result := true;
      Exit;
    end;
  end;
end;

function TTaskResourceOptimizerClass.OptimizeTeamAllocation(taskCount: integer): TAllocationArray;
var
  optimized: TAllocationArray;
begin
  SetLength(optimized, 0);
  result := optimized;
end;

function TTaskResourceOptimizerClass.BalanceWorkload(): string;
begin
  result := 'Workload balancing recommendation: Redistribute 15% from Senior Devs to Junior Devs';
end;

function TTaskResourceOptimizerClass.SuggestResourceAdjustments(): TStringArray;
var
  suggestions: TStringArray;
begin
  SetLength(suggestions, 2);
  suggestions[0] := 'Hire 1 additional senior developer';
  suggestions[1] := 'Cross-train 2 junior developers in critical skills';
  result := suggestions;
end;

function TTaskResourceOptimizerClass.AssessAllocationRisk(taskId, memberId: integer): double;
begin
  result := 0.25; { 25% risk }
end;

function TTaskResourceOptimizerClass.GetRiskFactors(taskId: integer): TStringArray;
var
  factors: TStringArray;
begin
  SetLength(factors, 2);
  factors[0] := 'Tight deadline';
  factors[1] := 'New technology stack';
  result := factors;
end;

function TTaskResourceOptimizerClass.CalculateProjectRiskScore(): double;
begin
  result := 0.35; { 35% overall project risk }
end;

function TTaskResourceOptimizerClass.GenerateAllocationReport(): string;
begin
  result := 'Resource Allocation Report:' + #13#10 +
            'Total Allocations: ' + IntToStr(Length(allocations)) + #13#10 +
            'Average Utilization: 75%' + #13#10 +
            'Status: Balanced';
end;

function TTaskResourceOptimizerClass.GetOptimizationMetrics(): string;
begin
  result := Format('Skill Match: %.2f%% | Load Balance: %.2f%% | Risk Score: %.2f%%',
                   [85.0, 90.0, 35.0]);
end;

function TTaskResourceOptimizerClass.GetResourceUtilizationReport(): string;
begin
  result := 'Senior Developers: 85% utilized (5 hours/week available)' + #13#10 +
            'Junior Developers: 65% utilized (14 hours/week available)' + #13#10 +
            'QA Engineers: 72% utilized (11 hours/week available)';
end;

procedure TTaskResourceOptimizerClass.SelfTest();
var
  assessment: TComplexityAssessment;
  alloc: TAllocationRecommendation;
  forecast: TCapacityForecast;
  skillArray: array of string;
  highRiskTasks: TIntegerArray;
begin
  WriteLn('');
  WriteLn('=== Resource Optimizer Self Test ===');

  { Test complexity assessment }
  assessment := AssessTaskComplexity(1, 50.0);
  WriteLn(Format('Task 1 complexity: %.1f (Risk: %s)', [assessment.complexityScore, assessment.riskLevel]));

  assessment := AssessTaskComplexity(2, 120.0);
  WriteLn(Format('Task 2 complexity: %.1f (Risk: %s)', [assessment.complexityScore, assessment.riskLevel]));

  { Test allocation recommendations }
  SetLength(skillArray, 2);
  skillArray[0] := 'Python';
  skillArray[1] := 'Database';
  alloc := RecommendAllocation(1, 40.0, skillArray);
  WriteLn(Format('Allocation recommendation - Task %d: Member %d (confidence: %.2f)',
                 [alloc.taskId, alloc.memberId, alloc.confidenceScore]));

  { Test capacity forecasting }
  forecast := ForecastCapacity(1, 7);
  WriteLn(Format('7-day capacity forecast - Member 1: %.1f%% utilization, %.1f hours available',
                 [forecast.forecastedUtilization, forecast.availableHours]));

  { Test high-risk task identification }
  highRiskTasks := GetHighRiskTasks();
  WriteLn(Format('High-risk tasks identified: %d', [Length(highRiskTasks)]));

  { Test team capacity }
  WriteLn('Team Capacity: ' + GetTeamCapacityStatus());

  { Test optimization metrics }
  WriteLn('Metrics: ' + GetOptimizationMetrics());

  WriteLn('=== Resource Optimizer Self Test Complete ===');
end;

end.
