
unit TaskAutomation;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  { Automation rule types }
  TAutomationTrigger = (atTaskCreated, atTaskCompleted, atTaskOverdue, 
                        atDateReached, atStatusChanged, atPriorityChanged,
                        atAssigneeChanged, atCustom);

  TAutomationAction = (aaCreateTask, aaUpdateStatus, aaUpdatePriority,
                       aaAddNote, aaAssignTo, aaSetDueDate, aaSendNotification,
                       aaEscalate, aaAddTag);

  { Automation rule record }
  TAutomationRule = record
    id: integer;
    name: string;
    description: string;
    trigger: TAutomationTrigger;
    triggerCondition: string;
    action: TAutomationAction;
    actionParameter: string;
    enabled: boolean;
    createdDate: TDateTime;
    lastExecutedDate: TDateTime;
    executionCount: integer;
  end;

  TAutomationRuleArray = array of TAutomationRule;

  { Automation execution log }
  TAutomationLog = record
    id: integer;
    ruleId: integer;
    taskId: integer;
    executedDate: TDateTime;
    status: string; { 'Success' or error message }
    message: string;
  end;

  TAutomationLogArray = array of TAutomationLog;

  TTaskAutomationManagerClass = class
  private
    rules: TAutomationRuleArray;
    logs: TAutomationLogArray;
    nextRuleId: integer;
    nextLogId: integer;
    function FindRuleIndex(id: integer): integer;
    function FindLogIndex(id: integer): integer;
    function TriggerToString(trigger: TAutomationTrigger): string;
    function ActionToString(action: TAutomationAction): string;
    function StringToTrigger(s: string): TAutomationTrigger;
    function StringToAction(s: string): TAutomationAction;
  public
    constructor Create();
    destructor Destroy();

    { Rule Management }
    function CreateRule(name, description: string; trigger: TAutomationTrigger;
                       triggerCondition: string; action: TAutomationAction;
                       actionParameter: string): integer;
    function GetRule(id: integer): TAutomationRule;
    function GetAllRules(): TAutomationRuleArray;
    function UpdateRule(id: integer; newRule: TAutomationRule): boolean;
    function DeleteRule(id: integer): boolean;
    function EnableRule(id: integer): boolean;
    function DisableRule(id: integer): boolean;
    function IsRuleEnabled(id: integer): boolean;

    { Rule Execution }
    function ExecuteRule(ruleId: integer; taskId: integer;
                        additionalData: string): boolean;
    function ExecuteRulesByTrigger(trigger: TAutomationTrigger;
                                   taskId: integer): integer;
    function GetRulesByTrigger(trigger: TAutomationTrigger): TAutomationRuleArray;

    { Logging and History }
    function GetExecutionLog(logId: integer): TAutomationLog;
    function GetRuleExecutionHistory(ruleId: integer): TAutomationLogArray;
    function GetTaskAutomationHistory(taskId: integer): TAutomationLogArray;
    function GetAllLogs(): TAutomationLogArray;
    function GetLogCount(): integer;
    function ClearOldLogs(beforeDate: TDateTime): integer;

    { Statistics }
    function GetRuleStatistics(ruleId: integer): string;
    function GetAutomationSummary(): string;
    function GetFailedExecutions(): TAutomationLogArray;
    function GetExecutionRate(ruleId: integer): double;

    procedure SelfTest();
  end;

implementation

constructor TTaskAutomationManagerClass.Create();
begin
  SetLength(rules, 0);
  SetLength(logs, 0);
  nextRuleId := 1;
  nextLogId := 1;
end;

destructor TTaskAutomationManagerClass.Destroy();
begin
  SetLength(rules, 0);
  SetLength(logs, 0);
  inherited;
end;

function TTaskAutomationManagerClass.FindRuleIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(rules) - 1 do
  begin
    if rules[i].id = id then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TTaskAutomationManagerClass.FindLogIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(logs) - 1 do
  begin
    if logs[i].id = id then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TTaskAutomationManagerClass.TriggerToString(trigger: TAutomationTrigger): string;
begin
  case trigger of
    atTaskCreated: result := 'TaskCreated';
    atTaskCompleted: result := 'TaskCompleted';
    atTaskOverdue: result := 'TaskOverdue';
    atDateReached: result := 'DateReached';
    atStatusChanged: result := 'StatusChanged';
    atPriorityChanged: result := 'PriorityChanged';
    atAssigneeChanged: result := 'AssigneeChanged';
    atCustom: result := 'Custom';
  else
    result := 'Unknown';
  end;
end;

function TTaskAutomationManagerClass.ActionToString(action: TAutomationAction): string;
begin
  case action of
    aaCreateTask: result := 'CreateTask';
    aaUpdateStatus: result := 'UpdateStatus';
    aaUpdatePriority: result := 'UpdatePriority';
    aaAddNote: result := 'AddNote';
    aaAssignTo: result := 'AssignTo';
    aaSetDueDate: result := 'SetDueDate';
    aaSendNotification: result := 'SendNotification';
    aaEscalate: result := 'Escalate';
    aaAddTag: result := 'AddTag';
  else
    result := 'Unknown';
  end;
end;

function TTaskAutomationManagerClass.StringToTrigger(s: string): TAutomationTrigger;
begin
  if s = 'TaskCreated' then result := atTaskCreated
  else if s = 'TaskCompleted' then result := atTaskCompleted
  else if s = 'TaskOverdue' then result := atTaskOverdue
  else if s = 'DateReached' then result := atDateReached
  else if s = 'StatusChanged' then result := atStatusChanged
  else if s = 'PriorityChanged' then result := atPriorityChanged
  else if s = 'AssigneeChanged' then result := atAssigneeChanged
  else result := atCustom;
end;

function TTaskAutomationManagerClass.StringToAction(s: string): TAutomationAction;
begin
  if s = 'CreateTask' then result := aaCreateTask
  else if s = 'UpdateStatus' then result := aaUpdateStatus
  else if s = 'UpdatePriority' then result := aaUpdatePriority
  else if s = 'AddNote' then result := aaAddNote
  else if s = 'AssignTo' then result := aaAssignTo
  else if s = 'SetDueDate' then result := aaSetDueDate
  else if s = 'SendNotification' then result := aaSendNotification
  else if s = 'Escalate' then result := aaEscalate
  else if s = 'AddTag' then result := aaAddTag
  else result := aaCreateTask;
end;

function TTaskAutomationManagerClass.CreateRule(name, description: string;
                                               trigger: TAutomationTrigger;
                                               triggerCondition: string;
                                               action: TAutomationAction;
                                               actionParameter: string): integer;
var
  newRule: TAutomationRule;
begin
  newRule.id := nextRuleId;
  newRule.name := name;
  newRule.description := description;
  newRule.trigger := trigger;
  newRule.triggerCondition := triggerCondition;
  newRule.action := action;
  newRule.actionParameter := actionParameter;
  newRule.enabled := true;
  newRule.createdDate := Now();
  newRule.lastExecutedDate := 0;
  newRule.executionCount := 0;

  SetLength(rules, Length(rules) + 1);
  rules[Length(rules) - 1] := newRule;

  result := nextRuleId;
  Inc(nextRuleId);
end;

function TTaskAutomationManagerClass.GetRule(id: integer): TAutomationRule;
var
  idx: integer;
begin
  idx := FindRuleIndex(id);
  if idx >= 0 then
    result := rules[idx]
  else
    FillChar(result, SizeOf(result), 0);
end;

function TTaskAutomationManagerClass.GetAllRules(): TAutomationRuleArray;
begin
  result := Copy(rules, 0, Length(rules));
end;

function TTaskAutomationManagerClass.UpdateRule(id: integer;
                                               newRule: TAutomationRule): boolean;
var
  idx: integer;
begin
  idx := FindRuleIndex(id);
  if idx >= 0 then
  begin
    rules[idx] := newRule;
    result := true;
  end
  else
    result := false;
end;

function TTaskAutomationManagerClass.DeleteRule(id: integer): boolean;
var
  idx, i: integer;
begin
  idx := FindRuleIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(rules) - 2 do
      rules[i] := rules[i + 1];
    SetLength(rules, Length(rules) - 1);
    result := true;
  end
  else
    result := false;
end;

function TTaskAutomationManagerClass.EnableRule(id: integer): boolean;
var
  idx: integer;
begin
  idx := FindRuleIndex(id);
  if idx >= 0 then
  begin
    rules[idx].enabled := true;
    result := true;
  end
  else
    result := false;
end;

function TTaskAutomationManagerClass.DisableRule(id: integer): boolean;
var
  idx: integer;
begin
  idx := FindRuleIndex(id);
  if idx >= 0 then
  begin
    rules[idx].enabled := false;
    result := true;
  end
  else
    result := false;
end;

function TTaskAutomationManagerClass.IsRuleEnabled(id: integer): boolean;
var
  idx: integer;
begin
  idx := FindRuleIndex(id);
  if idx >= 0 then
    result := rules[idx].enabled
  else
    result := false;
end;

function TTaskAutomationManagerClass.ExecuteRule(ruleId: integer; taskId: integer;
                                                additionalData: string): boolean;
var
  idx: integer;
  logEntry: TAutomationLog;
begin
  idx := FindRuleIndex(ruleId);
  if idx < 0 then
  begin
    result := false;
    Exit;
  end;

  if not rules[idx].enabled then
  begin
    result := false;
    Exit;
  end;

  { Log the execution }
  logEntry.id := nextLogId;
  logEntry.ruleId := ruleId;
  logEntry.taskId := taskId;
  logEntry.executedDate := Now();
  logEntry.status := 'Success';
  logEntry.message := 'Rule executed: ' + ActionToString(rules[idx].action);

  SetLength(logs, Length(logs) + 1);
  logs[Length(logs) - 1] := logEntry;
  Inc(nextLogId);

  { Update rule execution count }
  Inc(rules[idx].executionCount);
  rules[idx].lastExecutedDate := Now();

  result := true;
end;

function TTaskAutomationManagerClass.ExecuteRulesByTrigger(trigger: TAutomationTrigger;
                                                          taskId: integer): integer;
var
  i: integer;
  count: integer;
begin
  count := 0;
  for i := 0 to Length(rules) - 1 do
  begin
    if rules[i].trigger = trigger then
    begin
      if ExecuteRule(rules[i].id, taskId, '') then
        Inc(count);
    end;
  end;
  result := count;
end;

function TTaskAutomationManagerClass.GetRulesByTrigger(trigger: TAutomationTrigger): TAutomationRuleArray;
var
  i, count: integer;
  resultArray: TAutomationRuleArray;
begin
  SetLength(resultArray, 0);
  count := 0;

  for i := 0 to Length(rules) - 1 do
  begin
    if rules[i].trigger = trigger then
    begin
      SetLength(resultArray, count + 1);
      resultArray[count] := rules[i];
      Inc(count);
    end;
  end;

  result := resultArray;
end;

function TTaskAutomationManagerClass.GetExecutionLog(logId: integer): TAutomationLog;
var
  idx: integer;
begin
  idx := FindLogIndex(logId);
  if idx >= 0 then
    result := logs[idx]
  else
    FillChar(result, SizeOf(result), 0);
end;

function TTaskAutomationManagerClass.GetRuleExecutionHistory(ruleId: integer): TAutomationLogArray;
var
  i, count: integer;
  resultArray: TAutomationLogArray;
begin
  SetLength(resultArray, 0);
  count := 0;

  for i := 0 to Length(logs) - 1 do
  begin
    if logs[i].ruleId = ruleId then
    begin
      SetLength(resultArray, count + 1);
      resultArray[count] := logs[i];
      Inc(count);
    end;
  end;

  result := resultArray;
end;

function TTaskAutomationManagerClass.GetTaskAutomationHistory(taskId: integer): TAutomationLogArray;
var
  i, count: integer;
  resultArray: TAutomationLogArray;
begin
  SetLength(resultArray, 0);
  count := 0;

  for i := 0 to Length(logs) - 1 do
  begin
    if logs[i].taskId = taskId then
    begin
      SetLength(resultArray, count + 1);
      resultArray[count] := logs[i];
      Inc(count);
    end;
  end;

  result := resultArray;
end;

function TTaskAutomationManagerClass.GetAllLogs(): TAutomationLogArray;
begin
  result := Copy(logs, 0, Length(logs));
end;

function TTaskAutomationManagerClass.GetLogCount(): integer;
begin
  result := Length(logs);
end;

function TTaskAutomationManagerClass.ClearOldLogs(beforeDate: TDateTime): integer;
var
  i, count: integer;
begin
  count := 0;
  i := 0;
  while i < Length(logs) do
  begin
    if logs[i].executedDate < beforeDate then
    begin
      { Remove this log entry }
      if i < Length(logs) - 1 then
        logs[i] := logs[Length(logs) - 1];
      SetLength(logs, Length(logs) - 1);
      Inc(count);
    end
    else
      Inc(i);
  end;
  result := count;
end;

function TTaskAutomationManagerClass.GetRuleStatistics(ruleId: integer): string;
var
  idx: integer;
  history: TAutomationLogArray;
begin
  idx := FindRuleIndex(ruleId);
  if idx < 0 then
  begin
    result := 'Rule not found';
    Exit;
  end;

  history := GetRuleExecutionHistory(ruleId);
  result := Format('Rule: %s | Executions: %d | Last Executed: %s',
                   [rules[idx].name, rules[idx].executionCount,
                    DateTimeToStr(rules[idx].lastExecutedDate)]);
end;

function TTaskAutomationManagerClass.GetAutomationSummary(): string;
var
  i: integer;
  enabledCount, totalCount: integer;
begin
  enabledCount := 0;
  totalCount := Length(rules);

  for i := 0 to Length(rules) - 1 do
  begin
    if rules[i].enabled then
      Inc(enabledCount);
  end;

  result := Format('Total Rules: %d | Enabled: %d | Total Executions: %d | Log Entries: %d',
                   [totalCount, enabledCount, 
                    Length(logs), Length(logs)]);
end;

function TTaskAutomationManagerClass.GetFailedExecutions(): TAutomationLogArray;
var
  i, count: integer;
  resultArray: TAutomationLogArray;
begin
  SetLength(resultArray, 0);
  count := 0;

  for i := 0 to Length(logs) - 1 do
  begin
    if logs[i].status <> 'Success' then
    begin
      SetLength(resultArray, count + 1);
      resultArray[count] := logs[i];
      Inc(count);
    end;
  end;

  result := resultArray;
end;

function TTaskAutomationManagerClass.GetExecutionRate(ruleId: integer): double;
var
  idx: integer;
  ruleHistory: TAutomationLogArray;
  successCount, totalCount: integer;
  i: integer;
begin
  idx := FindRuleIndex(ruleId);
  if idx < 0 then
  begin
    result := 0;
    Exit;
  end;

  ruleHistory := GetRuleExecutionHistory(ruleId);
  totalCount := Length(ruleHistory);

  if totalCount = 0 then
  begin
    result := 0;
    Exit;
  end;

  successCount := 0;
  for i := 0 to Length(ruleHistory) - 1 do
  begin
    if ruleHistory[i].status = 'Success' then
      Inc(successCount);
  end;

  result := (successCount / totalCount) * 100;
end;

procedure TTaskAutomationManagerClass.SelfTest();
var
  ruleId1, ruleId2, ruleId3: integer;
  allRules: TAutomationRuleArray;
  executed: integer;
begin
  WriteLn('');
  WriteLn('=== Task Automation Manager Self Test ===');

  { Test rule creation }
  ruleId1 := CreateRule('Auto-escalate overdue', 
                        'Escalate tasks overdue for 3 days',
                        atTaskOverdue, 'days>3', aaEscalate, 'Manager');
  WriteLn(Format('✓ Created rule 1 with ID: %d', [ruleId1]));

  ruleId2 := CreateRule('Auto-create follow-up', 
                        'Create follow-up task when parent completed',
                        atTaskCompleted, 'createFollowUp=true', aaCreateTask,
                        'Follow-up Task');
  WriteLn(Format('✓ Created rule 2 with ID: %d', [ruleId2]));

  ruleId3 := CreateRule('Auto-notify on priority change',
                        'Send notification when priority changes',
                        atPriorityChanged, 'any', aaSendNotification,
                        'Priority changed');
  WriteLn(Format('✓ Created rule 3 with ID: %d', [ruleId3]));

  { Test rule retrieval }
  allRules := GetAllRules();
  WriteLn(Format('✓ Retrieved %d rules', [Length(allRules)]));

  { Test rule execution }
  executed := ExecuteRulesByTrigger(atTaskCompleted, 101);
  WriteLn(Format('✓ Executed %d rules for task 101', [executed]));

  { Test rule statistics }
  WriteLn(GetRuleStatistics(ruleId1));

  { Test automation summary }
  WriteLn(GetAutomationSummary());

  { Test enable/disable }
  if DisableRule(ruleId2) then
    WriteLn('✓ Disabled rule 2');

  if EnableRule(ruleId2) then
    WriteLn('✓ Re-enabled rule 2');

  WriteLn('=== Task Automation Manager Self Test Complete ===');
end;

end.
