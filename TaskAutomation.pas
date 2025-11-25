
unit TaskAutomation;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager;

const
  maxRulesPerManager = 100;
  maxRuleExecutionHistory = 1000;

type
  TRuleConditionType = (rctTaskStatus, rctTaskPriority, rctRiskScore, rctIsOverdue,
                        rctAssigneeEmpty, rctHasTag, rctCategoryMatch, rctEstimateOverrun,
                        rctInMilestone, rctHasBlocker);

  TRuleActionType = (ratChangeStatus, ratAssignTask, ratAddWatcher, ratAddTag,
                     ratSetPriority, ratRemoveTag, ratEscalatePriority, ratCompleteTask,
                     ratAddNote, ratBlockTask);

  TRuleCondition = record
    conditionType: TRuleConditionType;
    valueStr: string[255];
    valueInt: integer;
    valueBool: boolean;
  end;

  TRuleAction = record
    actionType: TRuleActionType;
    paramStr: string[255];
    paramInt: integer;
  end;

  TWorkflowRule = record
    ruleId: integer;
    name: string[100];
    description: string[255];
    enabled: boolean;
    conditions: array of TRuleCondition;
    actions: array of TRuleAction;
    createdDate: TDateTime;
    lastModified: TDateTime;
    executionCount: integer;
  end;

  TRuleExecutionEntry = record
    executionId: integer;
    ruleId: integer;
    taskId: integer;
    timestamp: TDateTime;
    success: boolean;
    details: string[255];
  end;

  TWorkflowRuleArray = array of TWorkflowRule;
  TExecutionHistoryArray = array of TRuleExecutionEntry;

  TTaskAutomation = class
  private
    manager: TTaskManager;
    rules: TWorkflowRuleArray;
    executionHistory: TExecutionHistoryArray;
    nextRuleId: integer;
    nextExecutionId: integer;
    lastError: string;
    isExecuting: boolean;

    function findRuleIndex(ruleId: integer): integer;
    function evaluateCondition(const condition: TRuleCondition; var task: TTask): boolean;
    function executeAction(const action: TRuleAction; taskId: integer): boolean;
    function checkCircularExecution(taskId: integer; ruleId: integer): boolean;
  public
    constructor Create(aManager: TTaskManager);
    destructor Destroy; override;

    function createRule(const name, description: string): integer;
    function deleteRule(ruleId: integer): boolean;
    function addConditionToRule(ruleId: integer; conditionType: TRuleConditionType;
                                const valueStr: string; valueInt: integer; valueBool: boolean): boolean;
    function addActionToRule(ruleId: integer; actionType: TRuleActionType;
                            const paramStr: string; paramInt: integer): boolean;
    function enableRule(ruleId: integer): boolean;
    function disableRule(ruleId: integer): boolean;
    function getRule(ruleId: integer; var rule: TWorkflowRule): boolean;
    function getAllRules: TWorkflowRuleArray;
    function getRulesByConditionType(conditionType: TRuleConditionType): TWorkflowRuleArray;

    function evaluateRulesForTask(taskId: integer): integer;
    function evaluateAllRules: integer;
    function getExecutionHistory(limit: integer): TExecutionHistoryArray;
    function getExecutionHistoryForRule(ruleId: integer; limit: integer): TExecutionHistoryArray;
    function getExecutionCount(ruleId: integer): integer;
    function getRuleExecutionStats: string;

    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskAutomation.Create(aManager: TTaskManager);
begin
  inherited Create;
  manager := aManager;
  nextRuleId := 1;
  nextExecutionId := 1;
  isExecuting := false;
  setLength(rules, 0);
  setLength(executionHistory, 0);
  lastError := '';
end;

destructor TTaskAutomation.Destroy;
var
  i: integer;
begin
  for i := 0 to length(rules) - 1 do
  begin
    setLength(rules[i].conditions, 0);
    setLength(rules[i].actions, 0);
  end;
  setLength(rules, 0);
  setLength(executionHistory, 0);
  inherited Destroy;
end;

function TTaskAutomation.findRuleIndex(ruleId: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to length(rules) - 1 do
  begin
    if rules[i].ruleId = ruleId then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskAutomation.evaluateCondition(const condition: TRuleCondition; var task: TTask): boolean;
var
  daysOverdue: double;
begin
  result := false;

  case condition.conditionType of
    rctTaskStatus:
      result := (ord(task.status) = condition.valueInt);

    rctTaskPriority:
      result := (ord(task.priority) = condition.valueInt);

    rctRiskScore:
      result := (condition.valueInt > 0);

    rctIsOverdue:
      begin
        if task.dueDate > 0 then
        begin
          daysOverdue := now - task.dueDate;
          result := (daysOverdue > 0) and (task.status <> tsCompleted) and (task.status <> tsCancelled);
        end;
      end;

    rctAssigneeEmpty:
      result := (length(task.assignedTo) = 0);

    rctHasTag:
      result := (length(task.tags) > 0);

    rctCategoryMatch:
      result := (lowercase(task.category) = lowercase(condition.valueStr));

    rctEstimateOverrun:
      result := (task.actualHours > task.estimatedHours) and (task.estimatedHours > 0);

    rctInMilestone:
      result := (task.milestoneId = condition.valueInt);

    rctHasBlocker:
      result := task.isBlocked;
  end;
end;

function TTaskAutomation.executeAction(const action: TRuleAction; taskId: integer): boolean;
var
  task: TTask;
  newStatus: TTaskStatus;
begin
  result := false;

  if not manager.getTask(taskId, task) then
  begin
    lastError := 'Task not found';
    exit;
  end;

  case action.actionType of
    ratChangeStatus:
      begin
        newStatus := TTaskStatus(action.paramInt);
        result := manager.setTaskStatus(taskId, newStatus);
      end;

    ratAddTag:
      result := manager.addTaskTag(taskId, action.paramStr);

    ratSetPriority:
      result := manager.setTaskPriority(taskId, TTaskPriority(action.paramInt));

    ratRemoveTag:
      result := manager.removeTaskTag(taskId, action.paramStr);

    ratEscalatePriority:
      begin
        if task.priority < tpCritical then
          result := manager.setTaskPriority(taskId, TTaskPriority(ord(task.priority) + 1))
        else
          result := true;
      end;

    ratCompleteTask:
      result := manager.completeTask(taskId);

    ratAssignTask, ratAddWatcher, ratAddNote, ratBlockTask:
      begin
        result := true;
      end;
  end;
end;

function TTaskAutomation.checkCircularExecution(taskId: integer; ruleId: integer): boolean;
var
  i: integer;
  recentExecutions: integer;
begin
  recentExecutions := 0;
  for i := max(0, length(executionHistory) - 10) to length(executionHistory) - 1 do
  begin
    if (executionHistory[i].taskId = taskId) and (executionHistory[i].ruleId = ruleId) then
      inc(recentExecutions);
  end;
  result := (recentExecutions > 3);
end;

function TTaskAutomation.createRule(const name, description: string): integer;
var
  newRule: TWorkflowRule;
  newIndex: integer;
begin
  if length(rules) >= maxRulesPerManager then
  begin
    lastError := 'Maximum rules reached';
    result := -1;
    exit;
  end;

  fillChar(newRule, sizeof(TWorkflowRule), 0);
  newRule.ruleId := nextRuleId;
  newRule.name := name;
  newRule.description := description;
  newRule.enabled := true;
  newRule.createdDate := now;
  newRule.lastModified := now;
  newRule.executionCount := 0;
  setLength(newRule.conditions, 0);
  setLength(newRule.actions, 0);

  newIndex := length(rules);
  setLength(rules, newIndex + 1);
  rules[newIndex] := newRule;

  result := nextRuleId;
  inc(nextRuleId);
  lastError := '';
end;

function TTaskAutomation.deleteRule(ruleId: integer): boolean;
var
  idx: integer;
begin
  idx := findRuleIndex(ruleId);
  if idx < 0 then
  begin
    lastError := 'Rule not found';
    result := false;
    exit;
  end;

  setLength(rules[idx].conditions, 0);
  setLength(rules[idx].actions, 0);

  if idx < length(rules) - 1 then
    move(rules[idx + 1], rules[idx], (length(rules) - idx - 1) * sizeof(TWorkflowRule));

  setLength(rules, length(rules) - 1);
  lastError := '';
  result := true;
end;

function TTaskAutomation.addConditionToRule(ruleId: integer; conditionType: TRuleConditionType;
                                const valueStr: string; valueInt: integer; valueBool: boolean): boolean;
var
  idx: integer;
  condIndex: integer;
  newCondition: TRuleCondition;
begin
  idx := findRuleIndex(ruleId);
  if idx < 0 then
  begin
    lastError := 'Rule not found';
    result := false;
    exit;
  end;

  newCondition.conditionType := conditionType;
  newCondition.valueStr := valueStr;
  newCondition.valueInt := valueInt;
  newCondition.valueBool := valueBool;

  condIndex := length(rules[idx].conditions);
  setLength(rules[idx].conditions, condIndex + 1);
  rules[idx].conditions[condIndex] := newCondition;
  rules[idx].lastModified := now;

  lastError := '';
  result := true;
end;

function TTaskAutomation.addActionToRule(ruleId: integer; actionType: TRuleActionType;
                              const paramStr: string; paramInt: integer): boolean;
var
  idx: integer;
  actIndex: integer;
  newAction: TRuleAction;
begin
  idx := findRuleIndex(ruleId);
  if idx < 0 then
  begin
    lastError := 'Rule not found';
    result := false;
    exit;
  end;

  newAction.actionType := actionType;
  newAction.paramStr := paramStr;
  newAction.paramInt := paramInt;

  actIndex := length(rules[idx].actions);
  setLength(rules[idx].actions, actIndex + 1);
  rules[idx].actions[actIndex] := newAction;
  rules[idx].lastModified := now;

  lastError := '';
  result := true;
end;

function TTaskAutomation.enableRule(ruleId: integer): boolean;
var
  idx: integer;
begin
  idx := findRuleIndex(ruleId);
  if idx < 0 then
  begin
    lastError := 'Rule not found';
    result := false;
    exit;
  end;

  rules[idx].enabled := true;
  rules[idx].lastModified := now;
  lastError := '';
  result := true;
end;

function TTaskAutomation.disableRule(ruleId: integer): boolean;
var
  idx: integer;
begin
  idx := findRuleIndex(ruleId);
  if idx < 0 then
  begin
    lastError := 'Rule not found';
    result := false;
    exit;
  end;

  rules[idx].enabled := false;
  rules[idx].lastModified := now;
  lastError := '';
  result := true;
end;

function TTaskAutomation.getRule(ruleId: integer; var rule: TWorkflowRule): boolean;
var
  idx: integer;
begin
  idx := findRuleIndex(ruleId);
  if idx < 0 then
  begin
    lastError := 'Rule not found';
    result := false;
    exit;
  end;

  rule := rules[idx];
  lastError := '';
  result := true;
end;

function TTaskAutomation.getAllRules: TWorkflowRuleArray;
begin
  result := rules;
  lastError := '';
end;

function TTaskAutomation.getRulesByConditionType(conditionType: TRuleConditionType): TWorkflowRuleArray;
var
  i: integer;
  j: integer;
  count: integer;
  resultArray: TWorkflowRuleArray;
  found: boolean;
begin
  setLength(resultArray, 0);
  count := 0;

  for i := 0 to length(rules) - 1 do
  begin
    found := false;
    for j := 0 to length(rules[i].conditions) - 1 do
    begin
      if rules[i].conditions[j].conditionType = conditionType then
      begin
        found := true;
        break;
      end;
    end;

    if found then
    begin
      setLength(resultArray, count + 1);
      resultArray[count] := rules[i];
      inc(count);
    end;
  end;

  result := resultArray;
  lastError := '';
end;

function TTaskAutomation.evaluateRulesForTask(taskId: integer): integer;
var
  i: integer;
  j: integer;
  k: integer;
  task: TTask;
  allConditionsMet: boolean;
  actionCount: integer;
  histIndex: integer;
  entry: TRuleExecutionEntry;
begin
  result := 0;

  if not manager.getTask(taskId, task) then
  begin
    lastError := 'Task not found';
    exit;
  end;

  if isExecuting then
  begin
    lastError := 'Rule execution already in progress';
    exit;
  end;

  isExecuting := true;

  try
    for i := 0 to length(rules) - 1 do
    begin
      if not rules[i].enabled then
        continue;

      if checkCircularExecution(taskId, rules[i].ruleId) then
        continue;

      if length(rules[i].conditions) = 0 then
        continue;

      allConditionsMet := true;
      for j := 0 to length(rules[i].conditions) - 1 do
      begin
        if not evaluateCondition(rules[i].conditions[j], task) then
        begin
          allConditionsMet := false;
          break;
        end;
      end;

      if allConditionsMet then
      begin
        for k := 0 to length(rules[i].actions) - 1 do
        begin
          if executeAction(rules[i].actions[k], taskId) then
          begin
            inc(result);
            inc(rules[i].executionCount);

            histIndex := length(executionHistory);
            if histIndex >= maxRuleExecutionHistory then
            begin
              move(executionHistory[1], executionHistory[0],
                   (maxRuleExecutionHistory - 1) * sizeof(TRuleExecutionEntry));
              histIndex := maxRuleExecutionHistory - 1;
            end
            else
              setLength(executionHistory, histIndex + 1);

            fillChar(entry, sizeof(TRuleExecutionEntry), 0);
            entry.executionId := nextExecutionId;
            entry.ruleId := rules[i].ruleId;
            entry.taskId := taskId;
            entry.timestamp := now;
            entry.success := true;
            entry.details := rules[i].name;
            executionHistory[histIndex] := entry;
            inc(nextExecutionId);
          end;
        end;
      end;
    end;

  finally
    isExecuting := false;
  end;

  lastError := '';
end;

function TTaskAutomation.evaluateAllRules: integer;
var
  i: integer;
  allTasks: TTaskArray;
  totalActions: integer;
begin
  allTasks := manager.getAllTasks;
  totalActions := 0;

  for i := 0 to length(allTasks) - 1 do
    totalActions := totalActions + evaluateRulesForTask(allTasks[i].id);

  setLength(allTasks, 0);
  result := totalActions;
  lastError := '';
end;

function TTaskAutomation.getExecutionHistory(limit: integer): TExecutionHistoryArray;
var
  i: integer;
  count: integer;
  startIndex: integer;
  resultArray: TExecutionHistoryArray;
begin
  setLength(resultArray, 0);

  if length(executionHistory) = 0 then
  begin
    result := resultArray;
    exit;
  end;

  count := min(limit, length(executionHistory));
  startIndex := length(executionHistory) - count;

  setLength(resultArray, count);
  for i := 0 to count - 1 do
    resultArray[i] := executionHistory[startIndex + i];

  result := resultArray;
  lastError := '';
end;

function TTaskAutomation.getExecutionHistoryForRule(ruleId: integer; limit: integer): TExecutionHistoryArray;
var
  i: integer;
  count: integer;
  resultArray: TExecutionHistoryArray;
begin
  setLength(resultArray, 0);
  count := 0;

  for i := max(0, length(executionHistory) - limit) to length(executionHistory) - 1 do
  begin
    if executionHistory[i].ruleId = ruleId then
    begin
      setLength(resultArray, count + 1);
      resultArray[count] := executionHistory[i];
      inc(count);
    end;
  end;

  result := resultArray;
  lastError := '';
end;

function TTaskAutomation.getExecutionCount(ruleId: integer): integer;
var
  idx: integer;
begin
  idx := findRuleIndex(ruleId);
  if idx < 0 then
  begin
    lastError := 'Rule not found';
    result := 0;
    exit;
  end;

  result := rules[idx].executionCount;
  lastError := '';
end;

function TTaskAutomation.getRuleExecutionStats: string;
var
  i: integer;
  stats: string;
  totalRules: integer;
  enabledRules: integer;
  totalExecutions: integer;
begin
  totalRules := length(rules);
  enabledRules := 0;
  totalExecutions := 0;

  for i := 0 to length(rules) - 1 do
  begin
    if rules[i].enabled then
      inc(enabledRules);
    totalExecutions := totalExecutions + rules[i].executionCount;
  end;

  stats := '===== RULE EXECUTION STATISTICS =====' + #10;
  stats := stats + '  Total Rules: ' + IntToStr(totalRules) + #10;
  stats := stats + '  Enabled Rules: ' + IntToStr(enabledRules) + #10;
  stats := stats + '  Disabled Rules: ' + IntToStr(totalRules - enabledRules) + #10;
  stats := stats + '  Total Executions: ' + IntToStr(totalExecutions) + #10;
  stats := stats + '  Execution History Size: ' + IntToStr(length(executionHistory)) + #10;

  if totalRules > 0 then
    stats := stats + '  Avg Executions per Rule: ' + FormatFloat('0.0', totalExecutions / totalRules) + #10;

  stats := stats + '===== END STATISTICS =====' + #10;

  result := stats;
  lastError := '';
end;

function TTaskAutomation.getLastError: string;
begin
  result := lastError;
end;

procedure TTaskAutomation.clearError;
begin
  lastError := '';
end;

end.
