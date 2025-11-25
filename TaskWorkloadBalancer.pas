unit TaskWorkloadBalancer;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager;

type
  { String array type for dynamic string collections }
  TStringArray = array of string;

  { Represents a team member's workload }
  TTeamMemberWorkload = record
    assigneeName: string;
    assignedTaskCount: integer;
    inProgressCount: integer;
    completedCount: integer;
    totalEstimatedHours: double;
    totalActualHours: double;
    pendingTasks: TTaskArray;
  end;

  TTeamMemberWorkloadArray = array of TTeamMemberWorkload;

  { Represents a suggestion for task reassignment }
  TAssignmentSuggestion = record
    taskId: integer;
    taskName: string;
    currentAssignee: string;
    suggestedAssignee: string;
    currentAssigneeWorkload: double;
    suggestedAssigneeWorkload: double;
    workloadImprovement: double;
    reason: string;
  end;

  TAssignmentSuggestionArray = array of TAssignmentSuggestion;

  { Workload balance metrics }
  TWorkloadMetrics = record
    averageWorkload: double;
    minWorkload: double;
    maxWorkload: double;
    workloadStandardDeviation: double;
    balanceIndex: double;  { 0-100, higher is better }
    overloadedCount: integer;
    underloadedCount: integer;
  end;

  { Task assignment analysis }
  TAssignmentAnalysis = record
    taskId: integer;
    taskName: string;
    currentAssignee: string;
    currentAssigneeWorkload: double;
    isOverassigned: boolean;
    canBeReassigned: boolean;
    optimalAssignee: string;
    workloadImprovement: double;
  end;

  TAssignmentAnalysisArray = array of TAssignmentAnalysis;

  { Main Workload Balancer class }
  TTaskWorkloadBalancer = class
  private
    manager: TTaskManager;
    lastError: string;
    maxWorkloadPercent: double;
    minWorkloadPercent: double;
    
    function getTeamMemberWorkload(const assigneeName: string): double;
    function getTotalTeamWorkload: double;
    function getAverageWorkload: double;
    function calculateWorkloadBalance: TWorkloadMetrics;
    function findBestAssigneeForTask(var task: TTask; var currentWorkloads: TTeamMemberWorkloadArray): string;
    
  public
    constructor Create(aManager: TTaskManager);
    destructor Destroy;
    
    { Workload Analysis }
    function analyzeTeamWorkload: TWorkloadMetrics;
    function getTeamMemberWorkloads: TTeamMemberWorkloadArray;
    function getWorkloadByMember(const assigneeName: string): double;
    function getMostOverloadedMember: string;
    function getLeastLoadedMember: string;
    
    { Reassignment Suggestions }
    function suggestReassignments: TAssignmentSuggestionArray;
    function analyzeTaskAssignment(taskId: integer): TAssignmentAnalysis;
    function suggestOptimalAssignee(taskId: integer): string;
    
    { Auto-Balancing }
    function autoBalanceWorkload: integer;
    function balanceSpecificMember(const assigneeName: string): integer;
    
    { Overload Detection }
    function getOverloadedMembers: TStringArray;
    function getUnderloadedMembers: TStringArray;
    function isTeamBalanced: boolean;
    function getImbalancePercentage: double;
    
    { Configuration }
    procedure setMaxWorkloadPercent(percent: double);
    procedure setMinWorkloadPercent(percent: double);
    function getMaxWorkloadPercent: double;
    function getMinWorkloadPercent: double;
    
    { Reporting }
    function getBalanceReport: string;
    function getAssignmentAdvice: string;
    function getWorkloadForecast(daysAhead: integer): string;
    
    { Utility }
    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskWorkloadBalancer.Create(aManager: TTaskManager);
begin
  manager := aManager;
  maxWorkloadPercent := 100;  { 100% workload threshold }
  minWorkloadPercent := 30;   { 30% minimum workload threshold }
  lastError := '';
end;

destructor TTaskWorkloadBalancer.Destroy;
begin
  inherited Destroy;
end;

function TTaskWorkloadBalancer.getTeamMemberWorkload(const assigneeName: string): double;
var
  i: integer;
  task: TTask;
  totalHours: double;
begin
  totalHours := 0;
  for i := 1 to manager.getTaskCount do
  begin
    if manager.getTask(i, task) then
    begin
      if task.assignedTo = assigneeName then
      begin
        if task.status <> tsCompleted then
          totalHours := totalHours + task.estimatedHours;
      end;
    end;
  end;
  result := totalHours;
end;

function TTaskWorkloadBalancer.getTotalTeamWorkload: double;
var
  i: integer;
  task: TTask;
  totalHours: double;
begin
  totalHours := 0;
  for i := 1 to manager.getTaskCount do
  begin
    if manager.getTask(i, task) then
    begin
      if task.status <> tsCompleted then
        totalHours := totalHours + task.estimatedHours;
    end;
  end;
  result := totalHours;
end;

function TTaskWorkloadBalancer.getAverageWorkload: double;
var
  workloads: TTeamMemberWorkloadArray;
  i: integer;
  totalWorkload: double;
begin
  workloads := getTeamMemberWorkloads;
  if length(workloads) = 0 then
  begin
    result := 0;
    exit;
  end;
  
  totalWorkload := 0;
  for i := 0 to length(workloads) - 1 do
    totalWorkload := totalWorkload + workloads[i].totalEstimatedHours;
  
  result := totalWorkload / length(workloads);
end;

function TTaskWorkloadBalancer.calculateWorkloadBalance: TWorkloadMetrics;
var
  workloads: TTeamMemberWorkloadArray;
  i: integer;
  avg, variance: double;
begin
  workloads := getTeamMemberWorkloads;
  
  result.averageWorkload := getAverageWorkload;
  
  if length(workloads) = 0 then
  begin
    result.minWorkload := 0;
    result.maxWorkload := 0;
    result.workloadStandardDeviation := 0;
    result.balanceIndex := 100;
    result.overloadedCount := 0;
    result.underloadedCount := 0;
    exit;
  end;
  
  { Find min and max }
  result.minWorkload := workloads[0].totalEstimatedHours;
  result.maxWorkload := workloads[0].totalEstimatedHours;
  
  for i := 1 to length(workloads) - 1 do
  begin
    if workloads[i].totalEstimatedHours < result.minWorkload then
      result.minWorkload := workloads[i].totalEstimatedHours;
    if workloads[i].totalEstimatedHours > result.maxWorkload then
      result.maxWorkload := workloads[i].totalEstimatedHours;
  end;
  
  { Calculate standard deviation }
  variance := 0;
  for i := 0 to length(workloads) - 1 do
    variance := variance + sqr(workloads[i].totalEstimatedHours - result.averageWorkload);
  variance := variance / length(workloads);
  result.workloadStandardDeviation := sqrt(variance);
  
  { Calculate balance index (0-100) }
  if result.maxWorkload > 0 then
    result.balanceIndex := (1 - (result.workloadStandardDeviation / result.maxWorkload)) * 100
  else
    result.balanceIndex := 100;
  
  if result.balanceIndex < 0 then
    result.balanceIndex := 0;
  if result.balanceIndex > 100 then
    result.balanceIndex := 100;
  
  { Count overloaded and underloaded members }
  result.overloadedCount := 0;
  result.underloadedCount := 0;
  for i := 0 to length(workloads) - 1 do
  begin
    if workloads[i].totalEstimatedHours > result.averageWorkload * 1.2 then
      inc(result.overloadedCount)
    else if workloads[i].totalEstimatedHours < result.averageWorkload * 0.5 then
      inc(result.underloadedCount);
  end;
end;

function TTaskWorkloadBalancer.findBestAssigneeForTask(var task: TTask; var currentWorkloads: TTeamMemberWorkloadArray): string;
var
  i: integer;
  minWorkload: double;
  bestAssignee: string;
begin
  result := '';
  
  if length(currentWorkloads) = 0 then
    exit;
  
  minWorkload := currentWorkloads[0].totalEstimatedHours;
  bestAssignee := currentWorkloads[0].assigneeName;
  
  for i := 1 to length(currentWorkloads) - 1 do
  begin
    if currentWorkloads[i].totalEstimatedHours < minWorkload then
    begin
      minWorkload := currentWorkloads[i].totalEstimatedHours;
      bestAssignee := currentWorkloads[i].assigneeName;
    end;
  end;
  
  result := bestAssignee;
end;

function TTaskWorkloadBalancer.analyzeTeamWorkload: TWorkloadMetrics;
begin
  result := calculateWorkloadBalance;
end;

function TTaskWorkloadBalancer.getTeamMemberWorkloads: TTeamMemberWorkloadArray;
var
  i: integer;
  task: TTask;
  assignees: array of string;
  assigneeCount: integer;
  idx, j: integer;
begin
  setLength(assignees, 0);
  assigneeCount := 0;
  
  { Collect unique assignees }
  for i := 1 to manager.getTaskCount do
  begin
    if manager.getTask(i, task) then
    begin
      if length(task.assignedTo) > 0 then
      begin
        idx := -1;
        for j := 0 to assigneeCount - 1 do
        begin
          if assignees[j] = task.assignedTo then
          begin
            idx := j;
            break;
          end;
        end;
        
        if idx < 0 then
        begin
          setLength(assignees, assigneeCount + 1);
          assignees[assigneeCount] := task.assignedTo;
          inc(assigneeCount);
        end;
      end;
    end;
  end;
  
  setLength(result, assigneeCount);
  
  { Build workload for each assignee }
  for i := 0 to assigneeCount - 1 do
  begin
    result[i].assigneeName := assignees[i];
    result[i].assignedTaskCount := 0;
    result[i].inProgressCount := 0;
    result[i].completedCount := 0;
    result[i].totalEstimatedHours := 0;
    result[i].totalActualHours := 0;
    setLength(result[i].pendingTasks, 0);
  end;
  
  { Count tasks and hours }
  for i := 1 to manager.getTaskCount do
  begin
    if manager.getTask(i, task) then
    begin
      if length(task.assignedTo) > 0 then
      begin
        for j := 0 to assigneeCount - 1 do
        begin
          if result[j].assigneeName = task.assignedTo then
          begin
            inc(result[j].assignedTaskCount);
            result[j].totalEstimatedHours := result[j].totalEstimatedHours + task.estimatedHours;
            result[j].totalActualHours := result[j].totalActualHours + task.actualHours;
            
            if task.status = tsCompleted then
              inc(result[j].completedCount)
            else if task.status = tsInProgress then
              inc(result[j].inProgressCount);
            
            if task.status <> tsCompleted then
            begin
              idx := length(result[j].pendingTasks);
              setLength(result[j].pendingTasks, idx + 1);
              result[j].pendingTasks[idx] := task;
            end;
            break;
          end;
        end;
      end;
    end;
  end;
end;

function TTaskWorkloadBalancer.getWorkloadByMember(const assigneeName: string): double;
begin
  result := getTeamMemberWorkload(assigneeName);
end;

function TTaskWorkloadBalancer.getMostOverloadedMember: string;
var
  workloads: TTeamMemberWorkloadArray;
  i: integer;
  maxWorkload: double;
begin
  workloads := getTeamMemberWorkloads;
  result := '';
  
  if length(workloads) = 0 then
    exit;
  
  maxWorkload := workloads[0].totalEstimatedHours;
  result := workloads[0].assigneeName;
  
  for i := 1 to length(workloads) - 1 do
  begin
    if workloads[i].totalEstimatedHours > maxWorkload then
    begin
      maxWorkload := workloads[i].totalEstimatedHours;
      result := workloads[i].assigneeName;
    end;
  end;
end;

function TTaskWorkloadBalancer.getLeastLoadedMember: string;
var
  workloads: TTeamMemberWorkloadArray;
  i: integer;
  minWorkload: double;
begin
  workloads := getTeamMemberWorkloads;
  result := '';
  
  if length(workloads) = 0 then
    exit;
  
  minWorkload := workloads[0].totalEstimatedHours;
  result := workloads[0].assigneeName;
  
  for i := 1 to length(workloads) - 1 do
  begin
    if workloads[i].totalEstimatedHours < minWorkload then
    begin
      minWorkload := workloads[i].totalEstimatedHours;
      result := workloads[i].assigneeName;
    end;
  end;
end;

function TTaskWorkloadBalancer.suggestReassignments: TAssignmentSuggestionArray;
var
  i, j, count: integer;
  task: TTask;
  workloads: TTeamMemberWorkloadArray;
  currentAssigneeIdx, bestAssigneeIdx: integer;
  suggestion: TAssignmentSuggestion;
  metrics: TWorkloadMetrics;
begin
  setLength(result, 0);
  count := 0;
  
  metrics := analyzeTeamWorkload;
  workloads := getTeamMemberWorkloads;
  
  if length(workloads) < 2 then
    exit;
  
  for i := 1 to manager.getTaskCount do
  begin
    if manager.getTask(i, task) then
    begin
      if (length(task.assignedTo) > 0) and (task.status <> tsCompleted) then
      begin
        { Find current assignee }
        currentAssigneeIdx := -1;
        for j := 0 to length(workloads) - 1 do
        begin
          if workloads[j].assigneeName = task.assignedTo then
          begin
            currentAssigneeIdx := j;
            break;
          end;
        end;
        
        if currentAssigneeIdx >= 0 then
        begin
          { Check if reassignment would improve balance }
          if workloads[currentAssigneeIdx].totalEstimatedHours > metrics.averageWorkload * 1.2 then
          begin
            { Find best assignee }
            bestAssigneeIdx := -1;
            for j := 0 to length(workloads) - 1 do
            begin
              if (j <> currentAssigneeIdx) and
                 (workloads[j].totalEstimatedHours < metrics.averageWorkload) then
              begin
                if (bestAssigneeIdx < 0) or
                   (workloads[j].totalEstimatedHours < workloads[bestAssigneeIdx].totalEstimatedHours) then
                  bestAssigneeIdx := j;
              end;
            end;
            
            if bestAssigneeIdx >= 0 then
            begin
              setLength(result, count + 1);
              suggestion.taskId := i;
              suggestion.taskName := task.name;
              suggestion.currentAssignee := workloads[currentAssigneeIdx].assigneeName;
              suggestion.suggestedAssignee := workloads[bestAssigneeIdx].assigneeName;
              suggestion.currentAssigneeWorkload := workloads[currentAssigneeIdx].totalEstimatedHours;
              suggestion.suggestedAssigneeWorkload := workloads[bestAssigneeIdx].totalEstimatedHours;
              suggestion.workloadImprovement := 
                abs(workloads[currentAssigneeIdx].totalEstimatedHours - task.estimatedHours - 
                    workloads[bestAssigneeIdx].totalEstimatedHours - task.estimatedHours) -
                abs(workloads[currentAssigneeIdx].totalEstimatedHours -
                    workloads[bestAssigneeIdx].totalEstimatedHours);
              suggestion.reason := 'Reassign to balance team workload';
              
              result[count] := suggestion;
              inc(count);
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TTaskWorkloadBalancer.analyzeTaskAssignment(taskId: integer): TAssignmentAnalysis;
var
  task: TTask;
  workloads: TTeamMemberWorkloadArray;
  i: integer;
  metrics: TWorkloadMetrics;
begin
  if not manager.getTask(taskId, task) then
  begin
    lastError := 'Task not found';
    result.taskId := -1;
    exit;
  end;
  
  metrics := analyzeTeamWorkload;
  workloads := getTeamMemberWorkloads;
  
  result.taskId := taskId;
  result.taskName := task.name;
  result.currentAssignee := task.assignedTo;
  result.currentAssigneeWorkload := getTeamMemberWorkload(task.assignedTo);
  result.isOverassigned := result.currentAssigneeWorkload > metrics.averageWorkload * 1.2;
  result.canBeReassigned := task.status <> tsCompleted;
  result.optimalAssignee := findBestAssigneeForTask(task, workloads);
  
  if result.optimalAssignee <> result.currentAssignee then
    result.workloadImprovement := getTeamMemberWorkload(result.currentAssignee) - task.estimatedHours -
                                   getTeamMemberWorkload(result.optimalAssignee)
  else
    result.workloadImprovement := 0;
end;

function TTaskWorkloadBalancer.suggestOptimalAssignee(taskId: integer): string;
var
  task: TTask;
  workloads: TTeamMemberWorkloadArray;
begin
  if not manager.getTask(taskId, task) then
  begin
    result := '';
    exit;
  end;
  
  workloads := getTeamMemberWorkloads;
  result := findBestAssigneeForTask(task, workloads);
end;

function TTaskWorkloadBalancer.autoBalanceWorkload: integer;
var
  suggestions: TAssignmentSuggestionArray;
begin
  { Returns the number of suggestions for rebalancing }
  { Actual reassignment should be done through the task manager interface }
  suggestions := suggestReassignments;
  result := length(suggestions);
end;

function TTaskWorkloadBalancer.balanceSpecificMember(const assigneeName: string): integer;
var
  i: integer;
  suggestions: TAssignmentSuggestionArray;
  reassignmentCount: integer;
begin
  { Returns the number of suggestions for rebalancing a specific member }
  { Actual reassignment should be done through the task manager interface }
  suggestions := suggestReassignments;
  reassignmentCount := 0;
  
  for i := 0 to length(suggestions) - 1 do
  begin
    if suggestions[i].currentAssignee = assigneeName then
      inc(reassignmentCount);
  end;
  
  result := reassignmentCount;
end;

function TTaskWorkloadBalancer.getOverloadedMembers: TStringArray;
var
  workloads: TTeamMemberWorkloadArray;
  metrics: TWorkloadMetrics;
  i, count: integer;
begin
  workloads := getTeamMemberWorkloads;
  metrics := analyzeTeamWorkload;
  
  count := 0;
  setLength(result, 0);
  
  for i := 0 to length(workloads) - 1 do
  begin
    if workloads[i].totalEstimatedHours > metrics.averageWorkload * 1.2 then
    begin
      setLength(result, count + 1);
      result[count] := workloads[i].assigneeName;
      inc(count);
    end;
  end;
end;

function TTaskWorkloadBalancer.getUnderloadedMembers: TStringArray;
var
  workloads: TTeamMemberWorkloadArray;
  metrics: TWorkloadMetrics;
  i, count: integer;
begin
  workloads := getTeamMemberWorkloads;
  metrics := analyzeTeamWorkload;
  
  count := 0;
  setLength(result, 0);
  
  for i := 0 to length(workloads) - 1 do
  begin
    if workloads[i].totalEstimatedHours < metrics.averageWorkload * 0.5 then
    begin
      setLength(result, count + 1);
      result[count] := workloads[i].assigneeName;
      inc(count);
    end;
  end;
end;

function TTaskWorkloadBalancer.isTeamBalanced: boolean;
var
  metrics: TWorkloadMetrics;
begin
  metrics := analyzeTeamWorkload;
  result := (metrics.overloadedCount = 0) and (metrics.underloadedCount = 0);
end;

function TTaskWorkloadBalancer.getImbalancePercentage: double;
var
  metrics: TWorkloadMetrics;
begin
  metrics := analyzeTeamWorkload;
  result := 100 - metrics.balanceIndex;
end;

procedure TTaskWorkloadBalancer.setMaxWorkloadPercent(percent: double);
begin
  if percent > 0 then
    maxWorkloadPercent := percent;
end;

procedure TTaskWorkloadBalancer.setMinWorkloadPercent(percent: double);
begin
  if percent >= 0 then
    minWorkloadPercent := percent;
end;

function TTaskWorkloadBalancer.getMaxWorkloadPercent: double;
begin
  result := maxWorkloadPercent;
end;

function TTaskWorkloadBalancer.getMinWorkloadPercent: double;
begin
  result := minWorkloadPercent;
end;

function TTaskWorkloadBalancer.getBalanceReport: string;
var
  metrics: TWorkloadMetrics;
  workloads: TTeamMemberWorkloadArray;
  i: integer;
begin
  metrics := analyzeTeamWorkload;
  workloads := getTeamMemberWorkloads;
  
  result := '===== WORKLOAD BALANCE REPORT =====' + #13#10;
  result := result + 'Team Size: ' + intToStr(length(workloads)) + #13#10;
  result := result + 'Average Workload: ' + FloatToStrF(metrics.averageWorkload, ffFixed, 5, 1) + ' hours' + #13#10;
  result := result + 'Min Workload: ' + FloatToStrF(metrics.minWorkload, ffFixed, 5, 1) + ' hours' + #13#10;
  result := result + 'Max Workload: ' + FloatToStrF(metrics.maxWorkload, ffFixed, 5, 1) + ' hours' + #13#10;
  result := result + 'Balance Index: ' + FloatToStrF(metrics.balanceIndex, ffFixed, 5, 2) + '%' + #13#10;
  result := result + 'Overloaded Members: ' + intToStr(metrics.overloadedCount) + #13#10;
  result := result + 'Underloaded Members: ' + intToStr(metrics.underloadedCount) + #13#10;
  result := result + #13#10;
  
  result := result + 'Member Details:' + #13#10;
  for i := 0 to length(workloads) - 1 do
  begin
    result := result + '  ' + workloads[i].assigneeName + ': ' +
              FloatToStrF(workloads[i].totalEstimatedHours, ffFixed, 5, 1) + ' hours (' +
              intToStr(workloads[i].assignedTaskCount) + ' tasks, ' +
              intToStr(workloads[i].inProgressCount) + ' in progress, ' +
              intToStr(workloads[i].completedCount) + ' completed)' + #13#10;
  end;
  
  result := result + #13#10 + 'Team Balanced: ';
  if isTeamBalanced then
    result := result + 'Yes'
  else
    result := result + 'No (Imbalance: ' + FloatToStrF(getImbalancePercentage, ffFixed, 5, 2) + '%)';
  result := result + #13#10;
end;

function TTaskWorkloadBalancer.getAssignmentAdvice: string;
var
  suggestions: TAssignmentSuggestionArray;
  i: integer;
begin
  suggestions := suggestReassignments;
  
  result := '===== ASSIGNMENT RECOMMENDATIONS =====' + #13#10;
  
  if length(suggestions) = 0 then
  begin
    result := result + 'No reassignments needed - team is well balanced!' + #13#10;
    exit;
  end;
  
  result := result + 'Recommended reassignments: ' + intToStr(length(suggestions)) + #13#10;
  result := result + #13#10;
  
  for i := 0 to minIntValue([4, length(suggestions) - 1]) do
  begin
    result := result + intToStr(i + 1) + '. ' + suggestions[i].taskName + #13#10;
    result := result + '   From: ' + suggestions[i].currentAssignee +
              ' (' + FloatToStrF(suggestions[i].currentAssigneeWorkload, ffFixed, 5, 1) + 'h)' + #13#10;
    result := result + '   To: ' + suggestions[i].suggestedAssignee +
              ' (' + FloatToStrF(suggestions[i].suggestedAssigneeWorkload, ffFixed, 5, 1) + 'h)' + #13#10;
    result := result + '   Reason: ' + suggestions[i].reason + #13#10;
  end;
end;

function TTaskWorkloadBalancer.getWorkloadForecast(daysAhead: integer): string;
var
  workloads: TTeamMemberWorkloadArray;
  metrics: TWorkloadMetrics;
  i: integer;
begin
  workloads := getTeamMemberWorkloads;
  metrics := analyzeTeamWorkload;
  
  result := '===== WORKLOAD FORECAST (' + intToStr(daysAhead) + ' DAYS) =====' + #13#10;
  result := result + 'Based on current task assignments and estimated hours...' + #13#10;
  result := result + #13#10;
  
  for i := 0 to length(workloads) - 1 do
  begin
    result := result + workloads[i].assigneeName + ': ' +
              FloatToStrF(workloads[i].totalEstimatedHours, ffFixed, 5, 1) + ' hours of work' + #13#10;
    result := result + '  Assuming 8h/day: ' +
              FloatToStrF(workloads[i].totalEstimatedHours / 8, ffFixed, 5, 1) + ' days' + #13#10;
  end;
end;

function TTaskWorkloadBalancer.getLastError: string;
begin
  result := lastError;
end;

procedure TTaskWorkloadBalancer.clearError;
begin
  lastError := '';
end;

end.
