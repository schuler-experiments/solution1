
unit TaskRiskAnalysis;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager,
  TaskManagerExtended,
  TaskManagerAdvanced,
  TaskManagerSubtasks,
  TaskManagerHistory,
  TaskManagerEnhanced,
  TaskAnalytics;

type
  { Risk assessment types }
  TRiskLevel = (rlLow, rlMedium, rlHigh, rlCritical);

  TRiskEntry = record
    taskId: integer;
    taskName: string;
    riskScore: integer; { 0-100 }
    riskLevel: TRiskLevel;
    riskFactors: string; { comma-separated reasons }
    suggestedAction: string;
    daysUntilDue: integer;
    completionProbability: double; { 0-100 }
  end;

  TRiskEntryArray = array of TRiskEntry;

  TPrediction = record
    taskId: integer;
    taskName: string;
    estimatedCompletionDate: TDateTime;
    confidenceLevel: double; { 0-100 }
    predictedOverrun: double; { hours }
    delayRisk: double; { 0-100 }
  end;

  TPredictionArray = array of TPrediction;

  TBlockerAnalysis = record
    taskId: integer;
    taskName: string;
    blockedByCount: integer;
    blockerNames: string;
    estimatedBlockedDays: double;
  end;

  TBlockerAnalysisArray = array of TBlockerAnalysis;

  { Risk Analysis Engine }
  TTaskRiskAnalysis = class
  private
    manager: TTaskManagerEnhanced;
    analytics: TTaskAnalytics;
    lastError: string;
    averageVelocity: double;

    function calculatePriorityRisk(priority: TTaskPriority): integer;
    function calculateDueDateRisk(dueDate: TDateTime): integer;
    function calculateDependencyRisk(taskId: integer): integer;
    function calculateResourceRisk(const assignee: string): integer;
    function calculateEstimationRisk(taskId: integer): integer;
    function calculateStatusRisk(status: TTaskStatus): integer;
    function getAverageVelocity: double;
  public
    constructor Create(aManager: TTaskManagerEnhanced; aAnalytics: TTaskAnalytics);
    destructor Destroy; override;

    { Risk assessment }
    function assessTaskRisk(taskId: integer; var riskEntry: TRiskEntry): boolean;
    function assessAllRisks(var risks: TRiskEntryArray): boolean;
    function getHighRiskTasks: TTaskArray;
    function getCriticalRiskTasks: TTaskArray;

    { Prediction functions }
    function predictCompletion(taskId: integer; var prediction: TPrediction): boolean;
    function predictAllCompletions(var predictions: TPredictionArray): boolean;
    function predictCompletionDate(taskId: integer): TDateTime;

    { Blocker analysis }
    function analyzeBlockers(taskId: integer; var analysis: TBlockerAnalysis): boolean;
    function findCriticalBlockers: TBlockerAnalysisArray;
    function findBlockedTasks: TTaskArray;

    { Risk metrics }
    function getOverallRiskScore: integer;
    function getTeamRiskScore(const teamMember: string): integer;
    function getCategoryRiskScore(const category: string): integer;
    function getProjectHealthScore: integer;

    { Risk trending }
    function getRiskTrend(days: integer): string;
    function getPredictedCriticalCount(daysAhead: integer): integer;

    { Configuration }
    procedure setAverageVelocity(velocity: double);
    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskRiskAnalysis.Create(aManager: TTaskManagerEnhanced; aAnalytics: TTaskAnalytics);
begin
  inherited Create;
  manager := aManager;
  analytics := aAnalytics;
  lastError := '';
  averageVelocity := 5.0; { default: 5 tasks/week }
end;

destructor TTaskRiskAnalysis.Destroy;
begin
  inherited Destroy;
end;

function TTaskRiskAnalysis.calculatePriorityRisk(priority: TTaskPriority): integer;
begin
  case priority of
    tpLow: result := 10;
    tpMedium: result := 30;
    tpHigh: result := 60;
    tpCritical: result := 85;
  else
    result := 20;
  end;
end;

function TTaskRiskAnalysis.calculateDueDateRisk(dueDate: TDateTime): integer;
var
  daysRemaining: integer;
begin
  daysRemaining := daysBetween(dueDate, now);

  if daysRemaining < 0 then
  begin
    { Task is overdue }
    result := 95;
  end
  else if daysRemaining = 0 then
  begin
    result := 90;
  end
  else if daysRemaining <= 1 then
  begin
    result := 80;
  end
  else if daysRemaining <= 3 then
  begin
    result := 60;
  end
  else if daysRemaining <= 7 then
  begin
    result := 40;
  end
  else if daysRemaining <= 14 then
  begin
    result := 25;
  end
  else if daysRemaining <= 30 then
  begin
    result := 15;
  end
  else
  begin
    result := 5;
  end;
end;

function TTaskRiskAnalysis.calculateDependencyRisk(taskId: integer): integer;
var
  deps: TIntArray;
  i: integer;
  uncompletedDeps: integer;
  depTask: TTask;
begin
  uncompletedDeps := 0;
  deps := manager.getTaskDependencies(taskId);

  for i := 0 to length(deps) - 1 do
  begin
    if manager.getTask(deps[i], depTask) then
    begin
      if depTask.status <> tsCompleted then
        inc(uncompletedDeps);
    end;
  end;

  if uncompletedDeps = 0 then
    result := 5
  else if uncompletedDeps = 1 then
    result := 30
  else if uncompletedDeps = 2 then
    result := 50
  else
    result := 70;
end;

function TTaskRiskAnalysis.calculateResourceRisk(const assignee: string): integer;
var
  tasks: TTaskArray;
begin
  tasks := manager.getTasksAssignedTo(assignee);

  if length(tasks) = 0 then
    result := 5
  else if length(tasks) <= 3 then
    result := 20
  else if length(tasks) <= 6 then
    result := 40
  else if length(tasks) <= 10 then
    result := 65
  else
    result := 85;
end;

function TTaskRiskAnalysis.calculateEstimationRisk(taskId: integer): integer;
var
  task: TTask;
  overrun: double;
begin
  result := 20;
  if manager.getTask(taskId, task) then
  begin
    overrun := manager.getTaskTimeOverrun(taskId);
    if overrun < 0 then
      result := 10
    else if overrun = 0 then
      result := 20
    else if overrun <= 5 then
      result := 40
    else if overrun <= 10 then
      result := 60
    else
      result := 85;
  end;
end;

function TTaskRiskAnalysis.calculateStatusRisk(status: TTaskStatus): integer;
begin
  case status of
    tsNew: result := 30;
    tsInProgress: result := 25;
    tsBlocked: result := 90;
    tsCompleted: result := 0;
    tsCancelled: result := 0;
  else
    result := 50;
  end;
end;

function TTaskRiskAnalysis.getAverageVelocity: double;
var
  stats: TCompletionStats;
begin
  stats := analytics.getCompletionStats;
  if stats.totalTasks > 0 then
    result := stats.completedTasks / ((stats.totalTasks / 2) + 1)
  else
    result := averageVelocity;
end;

function TTaskRiskAnalysis.assessTaskRisk(taskId: integer; var riskEntry: TRiskEntry): boolean;
var
  task: TTask;
  priorityRisk, dueDateRisk, dependencyRisk, resourceRisk, estimationRisk, statusRisk: integer;
  totalRisk: integer;
  riskFactors: string;
begin
  result := false;
  try
    if manager.getTask(taskId, task) then
    begin
      riskEntry.taskId := taskId;
      riskEntry.taskName := task.name;

      { Calculate individual risk factors }
      priorityRisk := calculatePriorityRisk(task.priority);
      dueDateRisk := calculateDueDateRisk(task.dueDate);
      dependencyRisk := calculateDependencyRisk(taskId);
      resourceRisk := calculateResourceRisk(task.assignedTo);
      estimationRisk := calculateEstimationRisk(taskId);
      statusRisk := calculateStatusRisk(task.status);

      { Calculate weighted average }
      totalRisk := (priorityRisk * 15 + dueDateRisk * 25 + dependencyRisk * 20 +
                    resourceRisk * 15 + estimationRisk * 15 + statusRisk * 10) div 100;

      riskEntry.riskScore := totalRisk;

      { Determine risk level }
      if totalRisk >= 75 then
        riskEntry.riskLevel := rlCritical
      else if totalRisk >= 50 then
        riskEntry.riskLevel := rlHigh
      else if totalRisk >= 25 then
        riskEntry.riskLevel := rlMedium
      else
        riskEntry.riskLevel := rlLow;

      { Build risk factors string }
      riskFactors := '';
      if priorityRisk >= 60 then
      begin
        if riskFactors <> '' then
          riskFactors := riskFactors + ';';
        riskFactors := riskFactors + 'HighPriority';
      end;
      if dueDateRisk >= 60 then
      begin
        if riskFactors <> '' then
          riskFactors := riskFactors + ';';
        riskFactors := riskFactors + 'ApproachingDeadline';
      end;
      if dependencyRisk >= 50 then
      begin
        if riskFactors <> '' then
          riskFactors := riskFactors + ';';
        riskFactors := riskFactors + 'DependencyIssues';
      end;
      if resourceRisk >= 60 then
      begin
        if riskFactors <> '' then
          riskFactors := riskFactors + ';';
        riskFactors := riskFactors + 'ResourceOverload';
      end;
      if estimationRisk >= 60 then
      begin
        if riskFactors <> '' then
          riskFactors := riskFactors + ';';
        riskFactors := riskFactors + 'EstimationError';
      end;
      if statusRisk >= 60 then
      begin
        if riskFactors <> '' then
          riskFactors := riskFactors + ';';
        riskFactors := riskFactors + 'Blocked';
      end;

      riskEntry.riskFactors := riskFactors;

      { Suggest action }
      case riskEntry.riskLevel of
        rlLow: riskEntry.suggestedAction := 'Monitor progress';
        rlMedium: riskEntry.suggestedAction := 'Review and adjust if needed';
        rlHigh: riskEntry.suggestedAction := 'Take immediate action';
        rlCritical: riskEntry.suggestedAction := 'Escalate immediately';
      end;

      riskEntry.daysUntilDue := daysBetween(task.dueDate, now);
      riskEntry.completionProbability := 100 - totalRisk;

      result := true;
    end
    else
      lastError := 'Task not found: ' + intToStr(taskId);
  except
    on e: Exception do
      lastError := 'Error assessing task risk: ' + e.Message;
  end;
end;

function TTaskRiskAnalysis.assessAllRisks(var risks: TRiskEntryArray): boolean;
var
  allTasks: TTaskArray;
  i: integer;
  riskEntry: TRiskEntry;
begin
  result := false;
  try
    allTasks := manager.getAllTasks;
    setLength(risks, 0);

    for i := 0 to length(allTasks) - 1 do
    begin
      if assessTaskRisk(allTasks[i].id, riskEntry) then
      begin
        setLength(risks, length(risks) + 1);
        risks[length(risks) - 1] := riskEntry;
      end;
    end;

    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error assessing all risks: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskRiskAnalysis.getHighRiskTasks: TTaskArray;
var
  risks: TRiskEntryArray;
  i: integer;
  count: integer;
  task: TTask;
begin
  setLength(result, 0);
  count := 0;

  if assessAllRisks(risks) then
  begin
    for i := 0 to length(risks) - 1 do
    begin
      if (risks[i].riskLevel = rlHigh) or (risks[i].riskLevel = rlCritical) then
      begin
        if manager.getTask(risks[i].taskId, task) then
        begin
          setLength(result, count + 1);
          result[count] := task;
          inc(count);
        end;
      end;
    end;
  end;
end;

function TTaskRiskAnalysis.getCriticalRiskTasks: TTaskArray;
var
  risks: TRiskEntryArray;
  i: integer;
  count: integer;
  task: TTask;
begin
  setLength(result, 0);
  count := 0;

  if assessAllRisks(risks) then
  begin
    for i := 0 to length(risks) - 1 do
    begin
      if risks[i].riskLevel = rlCritical then
      begin
        if manager.getTask(risks[i].taskId, task) then
        begin
          setLength(result, count + 1);
          result[count] := task;
          inc(count);
        end;
      end;
    end;
  end;
end;

function TTaskRiskAnalysis.predictCompletion(taskId: integer; var prediction: TPrediction): boolean;
var
  task: TTask;
  daysRemaining: integer;
begin
  result := false;
  try
    if manager.getTask(taskId, task) then
    begin
      prediction.taskId := taskId;
      prediction.taskName := task.name;

      daysRemaining := daysBetween(task.dueDate, now);

      prediction.estimatedCompletionDate := now + daysRemaining;
      prediction.confidenceLevel := 70;
      prediction.predictedOverrun := manager.getTaskTimeOverrun(taskId);
      prediction.delayRisk := calculateDueDateRisk(task.dueDate);

      result := true;
    end
    else
      lastError := 'Task not found: ' + intToStr(taskId);
  except
    on e: Exception do
      lastError := 'Error predicting completion: ' + e.Message;
  end;
end;

function TTaskRiskAnalysis.predictAllCompletions(var predictions: TPredictionArray): boolean;
var
  allTasks: TTaskArray;
  i: integer;
  prediction: TPrediction;
begin
  result := false;
  try
    allTasks := manager.getAllTasks;
    setLength(predictions, 0);

    for i := 0 to length(allTasks) - 1 do
    begin
      if predictCompletion(allTasks[i].id, prediction) then
      begin
        setLength(predictions, length(predictions) + 1);
        predictions[length(predictions) - 1] := prediction;
      end;
    end;

    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error predicting completions: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskRiskAnalysis.predictCompletionDate(taskId: integer): TDateTime;
var
  prediction: TPrediction;
begin
  result := now;
  if predictCompletion(taskId, prediction) then
    result := prediction.estimatedCompletionDate;
end;

function TTaskRiskAnalysis.analyzeBlockers(taskId: integer; var analysis: TBlockerAnalysis): boolean;
var
  deps: TIntArray;
  i: integer;
  depTask: TTask;
  dummyTask: TTask;
begin
  result := false;
  try
    if manager.getTask(taskId, dummyTask) then
    begin
      analysis.taskId := taskId;
      analysis.taskName := dummyTask.name;

      deps := manager.getTaskDependencies(taskId);
      analysis.blockedByCount := length(deps);

      analysis.blockerNames := '';
      for i := 0 to length(deps) - 1 do
      begin
        if manager.getTask(deps[i], depTask) then
        begin
          if i > 0 then
            analysis.blockerNames := analysis.blockerNames + ',';
          analysis.blockerNames := analysis.blockerNames + depTask.name;
        end;
      end;

      if analysis.blockedByCount > 0 then
        analysis.estimatedBlockedDays := analysis.blockedByCount * 2
      else
        analysis.estimatedBlockedDays := 0;

      result := true;
    end
    else
      lastError := 'Task not found: ' + intToStr(taskId);
  except
    on e: Exception do
      lastError := 'Error analyzing blockers: ' + e.Message;
  end;
end;

function TTaskRiskAnalysis.findCriticalBlockers: TBlockerAnalysisArray;
var
  allTasks: TTaskArray;
  i: integer;
  count: integer;
  analysis: TBlockerAnalysis;
begin
  setLength(result, 0);
  count := 0;

  allTasks := manager.getAllTasks;
  for i := 0 to length(allTasks) - 1 do
  begin
    if analyzeBlockers(allTasks[i].id, analysis) then
    begin
      if analysis.blockedByCount > 0 then
      begin
        setLength(result, count + 1);
        result[count] := analysis;
        inc(count);
      end;
    end;
  end;
end;

function TTaskRiskAnalysis.findBlockedTasks: TTaskArray;
var
  allTasks: TTaskArray;
  i: integer;
  count: integer;
begin
  setLength(result, 0);
  count := 0;

  allTasks := manager.getAllTasks;
  for i := 0 to length(allTasks) - 1 do
  begin
    if allTasks[i].status = tsBlocked then
    begin
      setLength(result, count + 1);
      result[count] := allTasks[i];
      inc(count);
    end;
  end;
end;

function TTaskRiskAnalysis.getOverallRiskScore: integer;
var
  risks: TRiskEntryArray;
  i: integer;
  totalScore: integer;
begin
  result := 0;
  if assessAllRisks(risks) then
  begin
    totalScore := 0;
    for i := 0 to length(risks) - 1 do
      totalScore := totalScore + risks[i].riskScore;

    if length(risks) > 0 then
      result := totalScore div length(risks)
    else
      result := 0;
  end;
end;

function TTaskRiskAnalysis.getTeamRiskScore(const teamMember: string): integer;
var
  tasks: TTaskArray;
  i: integer;
  totalScore: integer;
  riskEntry: TRiskEntry;
begin
  result := 0;
  tasks := manager.getTasksAssignedTo(teamMember);
  totalScore := 0;

  for i := 0 to length(tasks) - 1 do
  begin
    if assessTaskRisk(tasks[i].id, riskEntry) then
      totalScore := totalScore + riskEntry.riskScore;
  end;

  if length(tasks) > 0 then
    result := totalScore div length(tasks)
  else
    result := 0;
end;

function TTaskRiskAnalysis.getCategoryRiskScore(const category: string): integer;
var
  tasks: TTaskArray;
  i: integer;
  totalScore: integer;
  riskEntry: TRiskEntry;
begin
  result := 0;
  tasks := manager.getTasksByCategory(category);
  totalScore := 0;

  for i := 0 to length(tasks) - 1 do
  begin
    if assessTaskRisk(tasks[i].id, riskEntry) then
      totalScore := totalScore + riskEntry.riskScore;
  end;

  if length(tasks) > 0 then
    result := totalScore div length(tasks)
  else
    result := 0;
end;

function TTaskRiskAnalysis.getProjectHealthScore: integer;
var
  overallRisk: integer;
begin
  overallRisk := getOverallRiskScore;
  result := 100 - overallRisk;
  if result < 0 then
    result := 0;
end;

function TTaskRiskAnalysis.getRiskTrend(days: integer): string;
var
  risks: TRiskEntryArray;
  criticalCount, highCount: integer;
  i: integer;
begin
  result := '';
  if assessAllRisks(risks) then
  begin
    criticalCount := 0;
    highCount := 0;

    for i := 0 to length(risks) - 1 do
    begin
      if risks[i].riskLevel = rlCritical then
        inc(criticalCount)
      else if risks[i].riskLevel = rlHigh then
        inc(highCount);
    end;

    result := 'Critical: ' + intToStr(criticalCount) + ', High: ' + intToStr(highCount);
  end;
end;

function TTaskRiskAnalysis.getPredictedCriticalCount(daysAhead: integer): integer;
var
  risks: TRiskEntryArray;
  i: integer;
  predictedCritical: integer;
begin
  result := 0;
  if assessAllRisks(risks) then
  begin
    predictedCritical := 0;

    for i := 0 to length(risks) - 1 do
    begin
      if (risks[i].riskLevel = rlCritical) or
         ((risks[i].daysUntilDue > 0) and (risks[i].daysUntilDue <= daysAhead)) then
        inc(predictedCritical);
    end;

    result := predictedCritical;
  end;
end;

procedure TTaskRiskAnalysis.setAverageVelocity(velocity: double);
begin
  if velocity > 0 then
    averageVelocity := velocity;
end;

function TTaskRiskAnalysis.getLastError: string;
begin
  result := lastError;
end;

procedure TTaskRiskAnalysis.clearError;
begin
  lastError := '';
end;

end.
