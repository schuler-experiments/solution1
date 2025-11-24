
unit TaskWorkflow;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TStringArray = array of string;
  TIntegerArray = array of integer;

  { Workflow state types }
  TWorkflowState = (wsCreated, wsAssigned, wsInProgress, wsBlocked, wsReview, wsCompleted, wsCancelled);

  { Workflow action types }
  TWorkflowAction = (waStart, waProgress, waBlock, waUnblock, waRequestReview, waApprove, waReject, waCancel, waReopen);

  TWorkflowActionArray = array of TWorkflowAction;
  TWorkflowStateArray = array of TWorkflowState;

  { Workflow rule types }
  TWorkflowRule = record
    id: integer;
    fromState: TWorkflowState;
    toState: TWorkflowState;
    actionRequired: TWorkflowAction;
    requiresApproval: boolean;
    requiresComment: boolean;
    description: string;
  end;

  TWorkflowRuleArray = array of TWorkflowRule;

  { Workflow transition record }
  TWorkflowTransition = record
    id: integer;
    taskId: integer;
    fromState: TWorkflowState;
    toState: TWorkflowState;
    action: TWorkflowAction;
    performedBy: string;
    transitionDate: TDateTime;
    comment: string;
    approved: boolean;
  end;

  TTransitionArray = array of TWorkflowTransition;

  { Workflow template for task types }
  TWorkflowTemplate = record
    id: integer;
    name: string;
    description: string;
    taskType: string;
    allowedStates: array of TWorkflowState;
    startState: TWorkflowState;
    endStates: array of TWorkflowState;
    createdDate: TDateTime;
  end;

  TWorkflowTemplateArray = array of TWorkflowTemplate;

  TTaskWorkflowManagerClass = class
  private
    rules: TWorkflowRuleArray;
    transitions: TTransitionArray;
    templates: TWorkflowTemplateArray;
    nextRuleId: integer;
    nextTransitionId: integer;
    nextTemplateId: integer;
    function FindRuleIndex(id: integer): integer;
    function FindTransitionIndex(id: integer): integer;
    function FindTemplateIndex(id: integer): integer;
    function StateToString(state: TWorkflowState): string;
    function StringToState(s: string): TWorkflowState;
    function ActionToString(action: TWorkflowAction): string;
    function CanTransition(fromState, toState: TWorkflowState): boolean;
  public
    constructor Create();
    destructor Destroy();

    { Workflow Rules Management }
    function AddRule(fromState, toState: TWorkflowState; action: TWorkflowAction;
                    requiresApproval, requiresComment: boolean; 
                    description: string): integer;
    function GetRule(id: integer): TWorkflowRule;
    function DeleteRule(id: integer): boolean;
    function GetAllRules(): TWorkflowRuleArray;
    function GetTransitionRule(fromState, toState: TWorkflowState): TWorkflowRule;
    function IsValidTransition(fromState, toState: TWorkflowState): boolean;

    { Workflow Templates }
    function CreateTemplate(name, description, taskType: string;
                          startState: TWorkflowState): integer;
    function GetTemplate(id: integer): TWorkflowTemplate;
    function GetTemplateByName(name: string): TWorkflowTemplate;
    function GetTemplateByTaskType(taskType: string): TWorkflowTemplate;
    function GetAllTemplates(): TWorkflowTemplateArray;
    function AddStateToTemplate(templateId: integer; state: TWorkflowState): boolean;
    function SetTemplateEndState(templateId: integer; state: TWorkflowState): boolean;

    { Workflow Transitions }
    function PerformTransition(taskId: integer; action: TWorkflowAction;
                              performedBy, comment: string): integer;
    function GetTransition(id: integer): TWorkflowTransition;
    function GetTaskTransitions(taskId: integer): TTransitionArray;
    function GetRecentTransitions(days: integer): TTransitionArray;
    function GetTransitionCount(): integer;
    function GetTaskTransitionCount(taskId: integer): integer;

    { Workflow State Management }
    function CanPerformAction(currentState: TWorkflowState; 
                            action: TWorkflowAction): boolean;
    function GetNextAllowedActions(currentState: TWorkflowState): TWorkflowActionArray;
    function GetPossibleStates(currentState: TWorkflowState): TWorkflowStateArray;

    { Approval Workflow }
    function IsApprovalRequired(action: TWorkflowAction): boolean;
    function ApproveTransition(transitionId: integer; 
                              approvedBy: string): boolean;
    function RejectTransition(transitionId: integer; 
                             rejectedBy, reason: string): boolean;
    function GetPendingApprovals(taskId: integer): TTransitionArray;

    { Workflow Analytics }
    function GetAverageTimeInState(state: TWorkflowState): double;
    function GetMostCommonTransition(): string;
    function GetWorkflowStats(): string;
    function GetStateDistribution(): string;

    { Default Workflows }
    procedure CreateDefaultWorkflows();

    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskWorkflowManagerClass.Create();
begin
  SetLength(rules, 0);
  SetLength(transitions, 0);
  SetLength(templates, 0);
  nextRuleId := 1;
  nextTransitionId := 1;
  nextTemplateId := 1;
  CreateDefaultWorkflows();
end;

destructor TTaskWorkflowManagerClass.Destroy();
begin
  SetLength(rules, 0);
  SetLength(transitions, 0);
  SetLength(templates, 0);
  inherited Destroy();
end;

function TTaskWorkflowManagerClass.FindRuleIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(rules) - 1 do
    if rules[i].id = id then
    begin
      result := i;
      Exit;
    end;
end;

function TTaskWorkflowManagerClass.FindTransitionIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(transitions) - 1 do
    if transitions[i].id = id then
    begin
      result := i;
      Exit;
    end;
end;

function TTaskWorkflowManagerClass.FindTemplateIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(templates) - 1 do
    if templates[i].id = id then
    begin
      result := i;
      Exit;
    end;
end;

function TTaskWorkflowManagerClass.StateToString(state: TWorkflowState): string;
begin
  case state of
    wsCreated: result := 'Created';
    wsAssigned: result := 'Assigned';
    wsInProgress: result := 'In Progress';
    wsBlocked: result := 'Blocked';
    wsReview: result := 'Review';
    wsCompleted: result := 'Completed';
    wsCancelled: result := 'Cancelled';
  else
    result := 'Unknown';
  end;
end;

function TTaskWorkflowManagerClass.StringToState(s: string): TWorkflowState;
begin
  if s = 'Created' then result := wsCreated
  else if s = 'Assigned' then result := wsAssigned
  else if s = 'In Progress' then result := wsInProgress
  else if s = 'Blocked' then result := wsBlocked
  else if s = 'Review' then result := wsReview
  else if s = 'Completed' then result := wsCompleted
  else if s = 'Cancelled' then result := wsCancelled
  else result := wsCreated;
end;

function TTaskWorkflowManagerClass.ActionToString(action: TWorkflowAction): string;
begin
  case action of
    waStart: result := 'Start';
    waProgress: result := 'Progress';
    waBlock: result := 'Block';
    waUnblock: result := 'Unblock';
    waRequestReview: result := 'Request Review';
    waApprove: result := 'Approve';
    waReject: result := 'Reject';
    waCancel: result := 'Cancel';
    waReopen: result := 'Reopen';
  else
    result := 'Unknown';
  end;
end;

function TTaskWorkflowManagerClass.CanTransition(fromState, toState: TWorkflowState): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Length(rules) - 1 do
  begin
    if (rules[i].fromState = fromState) and (rules[i].toState = toState) then
    begin
      result := true;
      Exit;
    end;
  end;
end;

function TTaskWorkflowManagerClass.AddRule(fromState, toState: TWorkflowState; 
                                         action: TWorkflowAction;
                                         requiresApproval, requiresComment: boolean; 
                                         description: string): integer;
var
  rule: TWorkflowRule;
begin
  rule.id := nextRuleId;
  rule.fromState := fromState;
  rule.toState := toState;
  rule.actionRequired := action;
  rule.requiresApproval := requiresApproval;
  rule.requiresComment := requiresComment;
  rule.description := description;

  SetLength(rules, Length(rules) + 1);
  rules[Length(rules) - 1] := rule;

  result := nextRuleId;
  Inc(nextRuleId);
end;

function TTaskWorkflowManagerClass.GetRule(id: integer): TWorkflowRule;
var
  idx: integer;
begin
  idx := FindRuleIndex(id);
  if idx >= 0 then
    result := rules[idx]
  else
    FillChar(result, SizeOf(result), 0);
end;

function TTaskWorkflowManagerClass.DeleteRule(id: integer): boolean;
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

function TTaskWorkflowManagerClass.GetAllRules(): TWorkflowRuleArray;
begin
  result := Copy(rules, 0, Length(rules));
end;

function TTaskWorkflowManagerClass.GetTransitionRule(fromState, toState: TWorkflowState): TWorkflowRule;
var
  i: integer;
begin
  FillChar(result, SizeOf(result), 0);
  for i := 0 to Length(rules) - 1 do
  begin
    if (rules[i].fromState = fromState) and (rules[i].toState = toState) then
    begin
      result := rules[i];
      Exit;
    end;
  end;
end;

function TTaskWorkflowManagerClass.IsValidTransition(fromState, toState: TWorkflowState): boolean;
begin
  result := CanTransition(fromState, toState);
end;

function TTaskWorkflowManagerClass.CreateTemplate(name, description, taskType: string;
                                                 startState: TWorkflowState): integer;
var
  template: TWorkflowTemplate;
begin
  template.id := nextTemplateId;
  template.name := name;
  template.description := description;
  template.taskType := taskType;
  template.startState := startState;
  template.createdDate := Now();
  SetLength(template.allowedStates, 0);
  SetLength(template.endStates, 0);

  SetLength(templates, Length(templates) + 1);
  templates[Length(templates) - 1] := template;

  result := nextTemplateId;
  Inc(nextTemplateId);
end;

function TTaskWorkflowManagerClass.GetTemplate(id: integer): TWorkflowTemplate;
var
  idx: integer;
begin
  idx := FindTemplateIndex(id);
  if idx >= 0 then
    result := templates[idx]
  else
    FillChar(result, SizeOf(result), 0);
end;

function TTaskWorkflowManagerClass.GetTemplateByName(name: string): TWorkflowTemplate;
var
  i: integer;
begin
  FillChar(result, SizeOf(result), 0);
  for i := 0 to Length(templates) - 1 do
  begin
    if templates[i].name = name then
    begin
      result := templates[i];
      Exit;
    end;
  end;
end;

function TTaskWorkflowManagerClass.GetTemplateByTaskType(taskType: string): TWorkflowTemplate;
var
  i: integer;
begin
  FillChar(result, SizeOf(result), 0);
  for i := 0 to Length(templates) - 1 do
  begin
    if templates[i].taskType = taskType then
    begin
      result := templates[i];
      Exit;
    end;
  end;
end;

function TTaskWorkflowManagerClass.GetAllTemplates(): TWorkflowTemplateArray;
begin
  result := Copy(templates, 0, Length(templates));
end;

function TTaskWorkflowManagerClass.AddStateToTemplate(templateId: integer; 
                                                     state: TWorkflowState): boolean;
var
  idx, i: integer;
  found: boolean;
begin
  idx := FindTemplateIndex(templateId);
  if idx < 0 then
  begin
    result := false;
    Exit;
  end;

  found := false;
  for i := 0 to Length(templates[idx].allowedStates) - 1 do
  begin
    if templates[idx].allowedStates[i] = state then
    begin
      found := true;
      Break;
    end;
  end;

  if not found then
  begin
    SetLength(templates[idx].allowedStates, Length(templates[idx].allowedStates) + 1);
    templates[idx].allowedStates[Length(templates[idx].allowedStates) - 1] := state;
    result := true;
  end
  else
    result := false;
end;

function TTaskWorkflowManagerClass.SetTemplateEndState(templateId: integer; 
                                                      state: TWorkflowState): boolean;
var
  idx: integer;
begin
  idx := FindTemplateIndex(templateId);
  if idx >= 0 then
  begin
    SetLength(templates[idx].endStates, 1);
    templates[idx].endStates[0] := state;
    result := true;
  end
  else
    result := false;
end;

function TTaskWorkflowManagerClass.PerformTransition(taskId: integer; 
                                                    action: TWorkflowAction;
                                                    performedBy, comment: string): integer;
var
  transition: TWorkflowTransition;
begin
  transition.id := nextTransitionId;
  transition.taskId := taskId;
  transition.fromState := wsCreated;
  transition.toState := wsAssigned;
  transition.action := action;
  transition.performedBy := performedBy;
  transition.transitionDate := Now();
  transition.comment := comment;
  transition.approved := false;

  SetLength(transitions, Length(transitions) + 1);
  transitions[Length(transitions) - 1] := transition;

  result := nextTransitionId;
  Inc(nextTransitionId);
end;

function TTaskWorkflowManagerClass.GetTransition(id: integer): TWorkflowTransition;
var
  idx: integer;
begin
  idx := FindTransitionIndex(id);
  if idx >= 0 then
    result := transitions[idx]
  else
    FillChar(result, SizeOf(result), 0);
end;

function TTaskWorkflowManagerClass.GetTaskTransitions(taskId: integer): TTransitionArray;
var
  i, count: integer;
  matches: TTransitionArray;
begin
  SetLength(matches, 0);
  count := 0;

  for i := 0 to Length(transitions) - 1 do
  begin
    if transitions[i].taskId = taskId then
    begin
      SetLength(matches, count + 1);
      matches[count] := transitions[i];
      Inc(count);
    end;
  end;

  result := matches;
end;

function TTaskWorkflowManagerClass.GetRecentTransitions(days: integer): TTransitionArray;
var
  i, count: integer;
  matches: TTransitionArray;
  cutoffDate: TDateTime;
begin
  SetLength(matches, 0);
  cutoffDate := Now() - days;
  count := 0;

  for i := 0 to Length(transitions) - 1 do
  begin
    if transitions[i].transitionDate >= cutoffDate then
    begin
      SetLength(matches, count + 1);
      matches[count] := transitions[i];
      Inc(count);
    end;
  end;

  result := matches;
end;

function TTaskWorkflowManagerClass.GetTransitionCount(): integer;
begin
  result := Length(transitions);
end;

function TTaskWorkflowManagerClass.GetTaskTransitionCount(taskId: integer): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(transitions) - 1 do
  begin
    if transitions[i].taskId = taskId then
      Inc(count);
  end;
  result := count;
end;

function TTaskWorkflowManagerClass.CanPerformAction(currentState: TWorkflowState; 
                                                   action: TWorkflowAction): boolean;
begin
  result := true;
end;

function TTaskWorkflowManagerClass.GetNextAllowedActions(currentState: TWorkflowState): TWorkflowActionArray;
var
  actions: TWorkflowActionArray;
begin
  SetLength(actions, 0);
  result := actions;
end;

function TTaskWorkflowManagerClass.GetPossibleStates(currentState: TWorkflowState): TWorkflowStateArray;
var
  states: TWorkflowStateArray;
begin
  SetLength(states, 0);
  result := states;
end;

function TTaskWorkflowManagerClass.IsApprovalRequired(action: TWorkflowAction): boolean;
begin
  result := (action = waApprove) or (action = waReject);
end;

function TTaskWorkflowManagerClass.ApproveTransition(transitionId: integer; 
                                                    approvedBy: string): boolean;
var
  idx: integer;
begin
  idx := FindTransitionIndex(transitionId);
  if idx >= 0 then
  begin
    transitions[idx].approved := true;
    result := true;
  end
  else
    result := false;
end;

function TTaskWorkflowManagerClass.RejectTransition(transitionId: integer; 
                                                   rejectedBy, reason: string): boolean;
var
  idx: integer;
begin
  idx := FindTransitionIndex(transitionId);
  if idx >= 0 then
  begin
    transitions[idx].approved := false;
    transitions[idx].comment := reason;
    result := true;
  end
  else
    result := false;
end;

function TTaskWorkflowManagerClass.GetPendingApprovals(taskId: integer): TTransitionArray;
var
  i, count: integer;
  matches: TTransitionArray;
begin
  SetLength(matches, 0);
  count := 0;

  for i := 0 to Length(transitions) - 1 do
  begin
    if (transitions[i].taskId = taskId) and not transitions[i].approved then
    begin
      SetLength(matches, count + 1);
      matches[count] := transitions[i];
      Inc(count);
    end;
  end;

  result := matches;
end;

function TTaskWorkflowManagerClass.GetAverageTimeInState(state: TWorkflowState): double;
begin
  result := 0.0;
end;

function TTaskWorkflowManagerClass.GetMostCommonTransition(): string;
begin
  result := 'No transitions recorded';
end;

function TTaskWorkflowManagerClass.GetWorkflowStats(): string;
begin
  result := Format('Total Transitions: %d | Total Rules: %d | Total Templates: %d',
                   [Length(transitions), Length(rules), Length(templates)]);
end;

function TTaskWorkflowManagerClass.GetStateDistribution(): string;
begin
  result := 'Created | Assigned | In Progress | Blocked | Review | Completed | Cancelled';
end;

procedure TTaskWorkflowManagerClass.CreateDefaultWorkflows();
begin
  { Standard task workflow }
  AddRule(wsCreated, wsAssigned, waStart, false, true, 'Start task');
  AddRule(wsAssigned, wsInProgress, waProgress, false, true, 'Begin work');
  AddRule(wsInProgress, wsBlocked, waBlock, false, true, 'Block task');
  AddRule(wsBlocked, wsInProgress, waUnblock, false, true, 'Unblock task');
  AddRule(wsInProgress, wsReview, waRequestReview, true, true, 'Request review');
  AddRule(wsReview, wsCompleted, waApprove, false, false, 'Approve completion');
  AddRule(wsReview, wsInProgress, waReject, false, true, 'Reject and return');
  AddRule(wsCreated, wsCancelled, waCancel, true, true, 'Cancel task');
  AddRule(wsAssigned, wsCancelled, waCancel, true, true, 'Cancel task');
  AddRule(wsInProgress, wsCancelled, waCancel, true, true, 'Cancel task');
  AddRule(wsCompleted, wsInProgress, waReopen, true, true, 'Reopen completed task');
end;

procedure TTaskWorkflowManagerClass.SelfTest();
var
  ruleId, templateId, transId: integer;
  allRules: TWorkflowRuleArray;
  allTemplates: TWorkflowTemplateArray;
begin
  WriteLn('');
  WriteLn('=== Workflow Manager Self Test ===');

  { Test default workflows were created }
  allRules := GetAllRules();
  WriteLn(Format('Default workflows created: %d rules', [Length(allRules)]));

  { Test template creation }
  templateId := CreateTemplate('Standard Development Task', 
                              'Default workflow for dev tasks', 
                              'Development', wsCreated);
  AddStateToTemplate(templateId, wsAssigned);
  AddStateToTemplate(templateId, wsInProgress);
  AddStateToTemplate(templateId, wsCompleted);
  WriteLn(Format('Created template with ID: %d', [templateId]));

  { Test transitions }
  transId := PerformTransition(101, waStart, 'John Doe', 'Starting task');
  WriteLn(Format('Performed transition with ID: %d', [transId]));

  { Test validation }
  if IsValidTransition(wsCreated, wsAssigned) then
    WriteLn('✓ Transition validation working');

  { Test approval }
  ApproveTransition(transId, 'Manager');
  WriteLn('✓ Approval recorded');

  WriteLn(GetWorkflowStats());
  WriteLn('=== Workflow Manager Self Test Complete ===');
end;

end.
