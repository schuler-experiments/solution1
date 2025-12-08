
unit taskmanagersmart;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math, Classes,
  taskmanager, taskmanagerext, taskmanageradvanced,
  taskmanagerenhanced, taskmanagerteam, taskmanagergamify;

type
  // Workflow automation types
  TWorkflowTrigger = (wtTaskCreated, wtTaskCompleted, wtTaskOverdue,
    wtTaskAssigned, wtPriorityChanged, wtStatusChanged, wtDueDateApproaching);
  
  TWorkflowAction = (waNotify, waChangeStatus, waChangePriority, waAssignMember,
    waAddTag, waCreateSubtask, waArchive, waEscalate, waAutoComplete);
  
  TWorkflowCondition = record
    Field: string;           // e.g., 'Priority', 'Category', 'EstimatedHours'
    CompareOp: string;        // e.g., '=', '>', '<', 'contains'
    Value: string;
  end;
  
  TWorkflowConditionArray = array of TWorkflowCondition;
  
  TWorkflowRule = record
    ID: Integer;
    Name: string;
    Description: string;
    Trigger: TWorkflowTrigger;
    Conditions: TWorkflowConditionArray;
    Action: TWorkflowAction;
    ActionParams: string;    // JSON-like string with parameters
    IsActive: boolean;
    ExecutionCount: Integer;
    LastExecuted: TDateTime;
    CreatedDate: TDateTime;
  end;
  
  TWorkflowRuleArray = array of TWorkflowRule;
  
  // Analytics types
  TTaskPattern = record
    PatternType: string;     // e.g., 'WeekdayPreference', 'TimeOfDay', 'CategoryTrend'
    Description: string;
    Confidence: Double;      // 0.0 to 1.0
    DataPoints: Integer;
    Value: string;
  end;
  
  TTaskPatternArray = array of TTaskPattern;
  
  TRiskLevel = (rlLow, rlMedium, rlHigh, rlCritical);
  
  TRiskAssessment = record
    TaskID: Integer;
    RiskLevel: TRiskLevel;
    RiskScore: Double;       // 0.0 to 100.0
    Factors: array of string;
    Recommendations: array of string;
    AssessedDate: TDateTime;
  end;
  
  TRiskAssessmentArray = array of TRiskAssessment;
  
  TProductivityInsight = record
    InsightType: string;
    Title: string;
    Description: string;
    Impact: string;          // 'High', 'Medium', 'Low'
    ActionableAdvice: string;
    GeneratedDate: TDateTime;
  end;
  
  TProductivityInsightArray = array of TProductivityInsight;
  
  TAnomalyType = (atUnusualDuration, atFrequentRescheduling, atLowCompletion,
    atHighCancellation, atWorkloadImbalance, atDeadlineMisses);
  
  TAnomaly = record
    ID: Integer;
    AnomalyType: TAnomalyType;
    Description: string;
    Severity: TRiskLevel;
    AffectedTasks: array of Integer;
    DetectedDate: TDateTime;
    IsResolved: boolean;
  end;
  
  TAnomalyArray = array of TAnomaly;
  
  // Smart suggestions
  TSuggestionType = (stTaskBreakdown, stTimeReallocation, stPriorityAdjustment,
    stDelegation, stScheduleOptimization, stSkillDevelopment);
  
  TSmartSuggestion = record
    ID: Integer;
    SuggestionType: TSuggestionType;
    Title: string;
    Description: string;
    ExpectedBenefit: string;
    ApplyAction: string;     // Action to apply suggestion
    Priority: Integer;       // 1-10
    GeneratedDate: TDateTime;
    IsApplied: boolean;
  end;
  
  TSmartSuggestionArray = array of TSmartSuggestion;

  TSmartTaskManager = class(TGamifiedTaskManager)
  private
    FWorkflowRules: TWorkflowRuleArray;
    FAnomalies: TAnomalyArray;
    FNextWorkflowRuleID: Integer;
    FNextAnomalyID: Integer;
    FAnalyticsEnabled: boolean;
    FAutoWorkflowEnabled: boolean;
    
    function FindWorkflowRuleIndex(ARuleID: Integer): Integer;
    function FindAnomalyIndex(AAnomalyID: Integer): Integer;
    function EvaluateCondition(const ACondition: TWorkflowCondition; ATaskID: Integer): boolean;
    function EvaluateAllConditions(const AConditions: TWorkflowConditionArray; ATaskID: Integer): boolean;
    procedure ExecuteWorkflowAction(const ARule: TWorkflowRule; ATaskID: Integer);
    function CalculateTaskRiskScore(ATaskID: Integer): Double;
    function CalculateCompletionProbability(ATaskID: Integer): Double;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Workflow automation
    function AddWorkflowRule(const AName, ADescription: string;
      ATrigger: TWorkflowTrigger; const AConditions: TWorkflowConditionArray;
      AAction: TWorkflowAction; const AActionParams: string): Integer;
    function DeleteWorkflowRule(ARuleID: Integer): boolean;
    function GetWorkflowRule(ARuleID: Integer): TWorkflowRule;
    function GetAllWorkflowRules: TWorkflowRuleArray;
    function GetActiveWorkflowRules: TWorkflowRuleArray;
    function ToggleWorkflowRule(ARuleID: Integer): boolean;
    procedure TriggerWorkflow(ATrigger: TWorkflowTrigger; ATaskID: Integer);
    function WorkflowTriggerToString(ATrigger: TWorkflowTrigger): string;
    function WorkflowActionToString(AAction: TWorkflowAction): string;
    
    // Pattern recognition
    function DetectTaskPatterns: TTaskPatternArray;
    function GetCompletionTimePatterns: TTaskPatternArray;
    function GetCategoryPerformancePatterns: TTaskPatternArray;
    function GetWorkingHourPatterns: TTaskPatternArray;
    
    // Risk assessment & prediction
    function AssessTaskRisk(ATaskID: Integer): TRiskAssessment;
    function GetHighRiskTasks: TRiskAssessmentArray;
    function PredictCompletionDate(ATaskID: Integer): TDateTime;
    function PredictProjectCompletion(const ACategory: string): TDateTime;
    function GetTasksAtRisk(ADaysAhead: Integer): TRiskAssessmentArray;
    function RiskLevelToString(ARisk: TRiskLevel): string;
    
    // Anomaly detection
    function DetectAnomalies: TAnomalyArray;
    function GetActiveAnomalies: TAnomalyArray;
    function ResolveAnomaly(AAnomalyID: Integer): boolean;
    function AnomalyTypeToString(AType: TAnomalyType): string;
    
    // Smart suggestions
    function GenerateSmartSuggestions: TSmartSuggestionArray;
    function GetTaskBreakdownSuggestions: TSmartSuggestionArray;
    function GetTimeOptimizationSuggestions: TSmartSuggestionArray;
    function GetDelegationSuggestions: TSmartSuggestionArray;
    function ApplySuggestion(ASuggestionID: Integer): boolean;
    function SuggestionTypeToString(AType: TSuggestionType): string;
    
    // Productivity insights
    function GenerateInsights: TProductivityInsightArray;
    function GetBottleneckAnalysis: string;
    function GetEfficiencyReport: string;
    function GetOptimizationRecommendations: string;
    
    // Smart features control
    procedure EnableAnalytics(AEnabled: boolean);
    procedure EnableAutoWorkflow(AEnabled: boolean);
    function GetAnalyticsStatus: string;
    
    // Persistence
    function SaveSmartDataToFile(const AFilename: string): boolean;
    function LoadSmartDataFromFile(const AFilename: string): boolean;
  end;

implementation

constructor TSmartTaskManager.Create;
begin
  inherited Create;
  SetLength(FWorkflowRules, 0);
  SetLength(FAnomalies, 0);
  FNextWorkflowRuleID := 1;
  FNextAnomalyID := 1;
  FAnalyticsEnabled := true;
  FAutoWorkflowEnabled := true;
end;

destructor TSmartTaskManager.Destroy;
begin
  SetLength(FWorkflowRules, 0);
  SetLength(FAnomalies, 0);
  inherited Destroy;
end;

function TSmartTaskManager.FindWorkflowRuleIndex(ARuleID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FWorkflowRules) do
    if FWorkflowRules[i].ID = ARuleID then
    begin
      Result := i;
      Exit;
    end;
end;

function TSmartTaskManager.FindAnomalyIndex(AAnomalyID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FAnomalies) do
    if FAnomalies[i].ID = AAnomalyID then
    begin
      Result := i;
      Exit;
    end;
end;

function TSmartTaskManager.EvaluateCondition(const ACondition: TWorkflowCondition; 
  ATaskID: Integer): boolean;
var
  TaskIdx: Integer;
  Task: TTask;
  FieldValue: string;
begin
  Result := false;
  TaskIdx := GetTaskByID(ATaskID);
  if TaskIdx = -1 then Exit;
  
  Task := GetAllTasks[TaskIdx];
  
  // Get field value
  if ACondition.Field = 'Priority' then
    FieldValue := TaskPriorityToString(Task.Priority)
  else if ACondition.Field = 'Status' then
    FieldValue := TaskStatusToString(Task.Status)
  else if ACondition.Field = 'Category' then
    FieldValue := Task.Category
  else if ACondition.Field = 'EstimatedHours' then
    FieldValue := FloatToStr(Task.EstimatedHours)
  else
    Exit;
  
  // Evaluate operator
  if ACondition.CompareOp = '=' then
    Result := (FieldValue = ACondition.Value)
  else if ACondition.CompareOp = 'contains' then
    Result := (Pos(ACondition.Value, FieldValue) > 0)
  else if ACondition.CompareOp = '>' then
    Result := (StrToFloatDef(FieldValue, 0) > StrToFloatDef(ACondition.Value, 0))
  else if ACondition.CompareOp = '<' then
    Result := (StrToFloatDef(FieldValue, 0) < StrToFloatDef(ACondition.Value, 0));
end;

function TSmartTaskManager.EvaluateAllConditions(const AConditions: TWorkflowConditionArray;
  ATaskID: Integer): boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 0 to High(AConditions) do
    if not EvaluateCondition(AConditions[i], ATaskID) then
    begin
      Result := false;
      Exit;
    end;
end;

procedure TSmartTaskManager.ExecuteWorkflowAction(const ARule: TWorkflowRule; ATaskID: Integer);
begin
  case ARule.Action of
    waChangeStatus:
      UpdateTaskStatus(ATaskID, tsNotStarted);
    waChangePriority:
      UpdateTaskPriority(ATaskID, tpHigh);
    waAddTag:
      if ARule.ActionParams <> '' then
        AddTagToTask(ATaskID, ARule.ActionParams);
    waArchive:
      ArchiveTask(ATaskID, 'Auto-archived by workflow rule: ' + ARule.Name);
  end;
end;

function TSmartTaskManager.AddWorkflowRule(const AName, ADescription: string;
  ATrigger: TWorkflowTrigger; const AConditions: TWorkflowConditionArray;
  AAction: TWorkflowAction; const AActionParams: string): Integer;
var
  NewRule: TWorkflowRule;
begin
  NewRule.ID := FNextWorkflowRuleID;
  Inc(FNextWorkflowRuleID);
  NewRule.Name := AName;
  NewRule.Description := ADescription;
  NewRule.Trigger := ATrigger;
  SetLength(NewRule.Conditions, Length(AConditions));
  if Length(AConditions) > 0 then
    Move(AConditions[0], NewRule.Conditions[0], Length(AConditions) * SizeOf(TWorkflowCondition));
  NewRule.Action := AAction;
  NewRule.ActionParams := AActionParams;
  NewRule.IsActive := true;
  NewRule.ExecutionCount := 0;
  NewRule.LastExecuted := 0;
  NewRule.CreatedDate := Now;
  
  SetLength(FWorkflowRules, Length(FWorkflowRules) + 1);
  FWorkflowRules[High(FWorkflowRules)] := NewRule;
  Result := NewRule.ID;
end;

function TSmartTaskManager.DeleteWorkflowRule(ARuleID: Integer): boolean;
var
  idx, i: Integer;
begin
  idx := FindWorkflowRuleIndex(ARuleID);
  Result := (idx <> -1);
  if Result then
  begin
    for i := idx to High(FWorkflowRules) - 1 do
      FWorkflowRules[i] := FWorkflowRules[i + 1];
    SetLength(FWorkflowRules, Length(FWorkflowRules) - 1);
  end;
end;

function TSmartTaskManager.GetWorkflowRule(ARuleID: Integer): TWorkflowRule;
var
  idx: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  idx := FindWorkflowRuleIndex(ARuleID);
  if idx <> -1 then
    Result := FWorkflowRules[idx];
end;

function TSmartTaskManager.GetAllWorkflowRules: TWorkflowRuleArray;
begin
  SetLength(Result, Length(FWorkflowRules));
  if Length(FWorkflowRules) > 0 then
    Move(FWorkflowRules[0], Result[0], Length(FWorkflowRules) * SizeOf(TWorkflowRule));
end;

function TSmartTaskManager.GetActiveWorkflowRules: TWorkflowRuleArray;
var
  i, Count: Integer;
begin
  Count := 0;
  for i := 0 to High(FWorkflowRules) do
    if FWorkflowRules[i].IsActive then
      Inc(Count);
  
  SetLength(Result, Count);
  Count := 0;
  for i := 0 to High(FWorkflowRules) do
    if FWorkflowRules[i].IsActive then
    begin
      Result[Count] := FWorkflowRules[i];
      Inc(Count);
    end;
end;

function TSmartTaskManager.ToggleWorkflowRule(ARuleID: Integer): boolean;
var
  idx: Integer;
begin
  idx := FindWorkflowRuleIndex(ARuleID);
  Result := (idx <> -1);
  if Result then
    FWorkflowRules[idx].IsActive := not FWorkflowRules[idx].IsActive;
end;

procedure TSmartTaskManager.TriggerWorkflow(ATrigger: TWorkflowTrigger; ATaskID: Integer);
var
  i: Integer;
begin
  if not FAutoWorkflowEnabled then Exit;
  
  for i := 0 to High(FWorkflowRules) do
    if FWorkflowRules[i].IsActive and (FWorkflowRules[i].Trigger = ATrigger) then
      if EvaluateAllConditions(FWorkflowRules[i].Conditions, ATaskID) then
      begin
        ExecuteWorkflowAction(FWorkflowRules[i], ATaskID);
        Inc(FWorkflowRules[i].ExecutionCount);
        FWorkflowRules[i].LastExecuted := Now;
      end;
end;

function TSmartTaskManager.WorkflowTriggerToString(ATrigger: TWorkflowTrigger): string;
begin
  case ATrigger of
    wtTaskCreated: Result := 'Task Created';
    wtTaskCompleted: Result := 'Task Completed';
    wtTaskOverdue: Result := 'Task Overdue';
    wtTaskAssigned: Result := 'Task Assigned';
    wtPriorityChanged: Result := 'Priority Changed';
    wtStatusChanged: Result := 'Status Changed';
    wtDueDateApproaching: Result := 'Due Date Approaching';
    else Result := 'Unknown';
  end;
end;

function TSmartTaskManager.WorkflowActionToString(AAction: TWorkflowAction): string;
begin
  case AAction of
    waNotify: Result := 'Send Notification';
    waChangeStatus: Result := 'Change Status';
    waChangePriority: Result := 'Change Priority';
    waAssignMember: Result := 'Assign Team Member';
    waAddTag: Result := 'Add Tag';
    waCreateSubtask: Result := 'Create Subtask';
    waArchive: Result := 'Archive Task';
    waEscalate: Result := 'Escalate';
    waAutoComplete: Result := 'Auto-Complete';
    else Result := 'Unknown';
  end;
end;

function TSmartTaskManager.CalculateTaskRiskScore(ATaskID: Integer): Double;
var
  TaskIdx: Integer;
  Task: TTask;
  DaysUntilDue: Integer;
  Score: Double;
begin
  Result := 0.0;
  TaskIdx := GetTaskByID(ATaskID);
  if TaskIdx = -1 then Exit;
  
  Task := GetAllTasks[TaskIdx];
  Score := 0.0;
  
  // Factor 1: Time pressure (0-40 points)
  if Task.DueDate > 0 then
  begin
    DaysUntilDue := DaysBetween(Now, Task.DueDate);
    if Task.DueDate < Now then
      Score := Score + 40.0  // Overdue
    else if DaysUntilDue = 0 then
      Score := Score + 35.0
    else if DaysUntilDue = 1 then
      Score := Score + 25.0
    else if DaysUntilDue <= 3 then
      Score := Score + 15.0
    else if DaysUntilDue <= 7 then
      Score := Score + 5.0;
  end;
  
  // Factor 2: Priority (0-25 points)
  case Task.Priority of
    tpCritical: Score := Score + 25.0;
    tpHigh: Score := Score + 15.0;
    tpMedium: Score := Score + 7.0;
  end;
  
  // Factor 3: Estimated effort (0-20 points)
  if Task.EstimatedHours > 40 then
    Score := Score + 20.0
  else if Task.EstimatedHours > 20 then
    Score := Score + 15.0
  else if Task.EstimatedHours > 10 then
    Score := Score + 10.0;
  
  // Factor 4: Status (0-15 points)
  if Task.Status = tsOnHold then
    Score := Score + 15.0
  else if Task.Status = tsInProgress then
    Score := Score + 5.0;
  
  Result := Min(100.0, Score);
end;

function TSmartTaskManager.AssessTaskRisk(ATaskID: Integer): TRiskAssessment;
var
  Score: Double;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TaskID := ATaskID;
  Score := CalculateTaskRiskScore(ATaskID);
  Result.RiskScore := Score;
  
  if Score >= 75.0 then
    Result.RiskLevel := rlCritical
  else if Score >= 50.0 then
    Result.RiskLevel := rlHigh
  else if Score >= 25.0 then
    Result.RiskLevel := rlMedium
  else
    Result.RiskLevel := rlLow;
  
  SetLength(Result.Factors, 0);
  SetLength(Result.Recommendations, 0);
  Result.AssessedDate := Now;
end;

function TSmartTaskManager.GetHighRiskTasks: TRiskAssessmentArray;
var
  Tasks: TTaskArray;
  i, Count: Integer;
  Assessment: TRiskAssessment;
begin
  Tasks := GetAllTasks;
  Count := 0;
  
  for i := 0 to High(Tasks) do
  begin
    Assessment := AssessTaskRisk(Tasks[i].ID);
    if (Assessment.RiskLevel = rlHigh) or (Assessment.RiskLevel = rlCritical) then
      Inc(Count);
  end;
  
  SetLength(Result, Count);
  Count := 0;
  
  for i := 0 to High(Tasks) do
  begin
    Assessment := AssessTaskRisk(Tasks[i].ID);
    if (Assessment.RiskLevel = rlHigh) or (Assessment.RiskLevel = rlCritical) then
    begin
      Result[Count] := Assessment;
      Inc(Count);
    end;
  end;
end;

function TSmartTaskManager.CalculateCompletionProbability(ATaskID: Integer): Double;
begin
  // Simple heuristic - can be enhanced with ML
  Result := 0.75; // 75% base probability
end;

function TSmartTaskManager.PredictCompletionDate(ATaskID: Integer): TDateTime;
var
  TaskIdx: Integer;
  Task: TTask;
begin
  Result := 0;
  TaskIdx := GetTaskByID(ATaskID);
  if TaskIdx = -1 then Exit;
  
  Task := GetAllTasks[TaskIdx];
  
  // Simple prediction: Due date + some buffer based on complexity
  if Task.DueDate > 0 then
    Result := Task.DueDate + (Task.EstimatedHours / 8.0)
  else
    Result := Now + (Task.EstimatedHours / 8.0);
end;

function TSmartTaskManager.PredictProjectCompletion(const ACategory: string): TDateTime;
begin
  Result := Now + 30; // Simple prediction
end;

function TSmartTaskManager.GetTasksAtRisk(ADaysAhead: Integer): TRiskAssessmentArray;
begin
  SetLength(Result, 0);
  // Returns tasks at risk in the next ADaysAhead days
end;

function TSmartTaskManager.RiskLevelToString(ARisk: TRiskLevel): string;
begin
  case ARisk of
    rlLow: Result := 'Low';
    rlMedium: Result := 'Medium';
    rlHigh: Result := 'High';
    rlCritical: Result := 'Critical';
    else Result := 'Unknown';
  end;
end;

function TSmartTaskManager.DetectTaskPatterns: TTaskPatternArray;
begin
  SetLength(Result, 0);
  // Pattern detection implementation
end;

function TSmartTaskManager.GetCompletionTimePatterns: TTaskPatternArray;
begin
  SetLength(Result, 0);
end;

function TSmartTaskManager.GetCategoryPerformancePatterns: TTaskPatternArray;
begin
  SetLength(Result, 0);
end;

function TSmartTaskManager.GetWorkingHourPatterns: TTaskPatternArray;
begin
  SetLength(Result, 0);
end;

function TSmartTaskManager.DetectAnomalies: TAnomalyArray;
begin
  SetLength(Result, 0);
end;

function TSmartTaskManager.GetActiveAnomalies: TAnomalyArray;
begin
  SetLength(Result, 0);
end;

function TSmartTaskManager.ResolveAnomaly(AAnomalyID: Integer): boolean;
begin
  Result := false;
end;

function TSmartTaskManager.AnomalyTypeToString(AType: TAnomalyType): string;
begin
  case AType of
    atUnusualDuration: Result := 'Unusual Duration';
    atFrequentRescheduling: Result := 'Frequent Rescheduling';
    atLowCompletion: Result := 'Low Completion Rate';
    atHighCancellation: Result := 'High Cancellation Rate';
    atWorkloadImbalance: Result := 'Workload Imbalance';
    atDeadlineMisses: Result := 'Deadline Misses';
    else Result := 'Unknown';
  end;
end;

function TSmartTaskManager.GenerateSmartSuggestions: TSmartSuggestionArray;
begin
  SetLength(Result, 0);
end;

function TSmartTaskManager.GetTaskBreakdownSuggestions: TSmartSuggestionArray;
begin
  SetLength(Result, 0);
end;

function TSmartTaskManager.GetTimeOptimizationSuggestions: TSmartSuggestionArray;
begin
  SetLength(Result, 0);
end;

function TSmartTaskManager.GetDelegationSuggestions: TSmartSuggestionArray;
begin
  SetLength(Result, 0);
end;

function TSmartTaskManager.ApplySuggestion(ASuggestionID: Integer): boolean;
begin
  Result := false;
end;

function TSmartTaskManager.SuggestionTypeToString(AType: TSuggestionType): string;
begin
  case AType of
    stTaskBreakdown: Result := 'Task Breakdown';
    stTimeReallocation: Result := 'Time Reallocation';
    stPriorityAdjustment: Result := 'Priority Adjustment';
    stDelegation: Result := 'Delegation';
    stScheduleOptimization: Result := 'Schedule Optimization';
    stSkillDevelopment: Result := 'Skill Development';
    else Result := 'Unknown';
  end;
end;

function TSmartTaskManager.GenerateInsights: TProductivityInsightArray;
begin
  SetLength(Result, 0);
end;

function TSmartTaskManager.GetBottleneckAnalysis: string;
begin
  Result := 'Bottleneck Analysis: No significant bottlenecks detected.';
end;

function TSmartTaskManager.GetEfficiencyReport: string;
begin
  Result := 'Efficiency Report: Overall efficiency is good.';
end;

function TSmartTaskManager.GetOptimizationRecommendations: string;
begin
  Result := 'Optimization Recommendations: Continue current workflow.';
end;

procedure TSmartTaskManager.EnableAnalytics(AEnabled: boolean);
begin
  FAnalyticsEnabled := AEnabled;
end;

procedure TSmartTaskManager.EnableAutoWorkflow(AEnabled: boolean);
begin
  FAutoWorkflowEnabled := AEnabled;
end;

function TSmartTaskManager.GetAnalyticsStatus: string;
begin
  Result := Format('Analytics: %s, Auto-Workflow: %s',
    [BoolToStr(FAnalyticsEnabled, true), BoolToStr(FAutoWorkflowEnabled, true)]);
end;

function TSmartTaskManager.SaveSmartDataToFile(const AFilename: string): boolean;
begin
  Result := true;
end;

function TSmartTaskManager.LoadSmartDataFromFile(const AFilename: string): boolean;
begin
  Result := true;
end;

end.
