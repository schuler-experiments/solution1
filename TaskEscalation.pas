
unit TaskEscalation;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TEscalationLevel = (elLevel1, elLevel2, elLevel3, elLevel4, elLevel5);

  TEscalationRule = record
    ID: string;
    Name: string;
    TaskID: string;
    TriggerCondition: string;
    CurrentLevel: TEscalationLevel;
    EscalationThreshold: integer;
    HoursOverdue: integer;
    Manager: string;
    Director: string;
    CTO: string;
    CreatedTime: TDateTime;
    LastEscalatedTime: TDateTime;
    IsActive: boolean;
  end;

  TEscalationRuleArray = array of TEscalationRule;

  TEscalationHistory = record
    ID: string;
    RuleID: string;
    TaskID: string;
    FromLevel: TEscalationLevel;
    ToLevel: TEscalationLevel;
    Reason: string;
    EscalatedTime: TDateTime;
    NotifiedTo: string;
  end;

  TEscalationHistoryArray = array of TEscalationHistory;

  TTaskEscalationManagerClass = class
  private
    FRules: TEscalationRuleArray;
    FHistory: TEscalationHistoryArray;
    FRuleCounter: integer;
    FHistoryCounter: integer;
    function GenerateRuleID(): string;
    function GenerateHistoryID(): string;
    function FindRuleIndex(aRuleID: string): integer;
  public
    constructor Create();
    destructor Destroy(); override;
    function CreateEscalationRule(aTaskID: string; aName: string;
                                  aManager, aDirector, aCTO: string): string;
    function GetRule(aRuleID: string): TEscalationRule;
    function GetTaskRules(aTaskID: string): TEscalationRuleArray;
    function GetActiveRules(): TEscalationRuleArray;
    procedure UpdateEscalationLevel(aRuleID: string; aNewLevel: TEscalationLevel;
                                    aReason: string; aNotifiedTo: string);
    procedure DeactivateRule(aRuleID: string);
    procedure ActivateRule(aRuleID: string);
    function GetEscalationLevel(aRuleID: string): TEscalationLevel;
    function GetLevelName(aLevel: TEscalationLevel): string;
    function GetRulesAtLevel(aLevel: TEscalationLevel): TEscalationRuleArray;
    function GetRulesbyStatus(aIsActive: boolean): TEscalationRuleArray;
    function GetEscalationHistory(aRuleID: string): TEscalationHistoryArray;
    function GetTotalEscalations(): integer;
    function GetAverageTimeToEscalation(): double;
    procedure ClearOldHistory(aDaysOld: integer);
    function GetAllRules(): TEscalationRuleArray;
    procedure SelfTest();
  end;

implementation

constructor TTaskEscalationManagerClass.Create();
begin
  inherited Create();
  SetLength(FRules, 0);
  SetLength(FHistory, 0);
  FRuleCounter := 0;
  FHistoryCounter := 0;
end;

destructor TTaskEscalationManagerClass.Destroy();
begin
  SetLength(FRules, 0);
  SetLength(FHistory, 0);
  inherited Destroy();
end;

function TTaskEscalationManagerClass.GenerateRuleID(): string;
begin
  inc(FRuleCounter);
  GenerateRuleID := 'ESC-RULE-' + IntToStr(FRuleCounter);
end;

function TTaskEscalationManagerClass.GenerateHistoryID(): string;
begin
  inc(FHistoryCounter);
  GenerateHistoryID := 'ESC-HIST-' + IntToStr(FHistoryCounter);
end;

function TTaskEscalationManagerClass.FindRuleIndex(aRuleID: string): integer;
var
  i: integer;
begin
  FindRuleIndex := -1;
  for i := 0 to Length(FRules) - 1 do
  begin
    if FRules[i].ID = aRuleID then
    begin
      FindRuleIndex := i;
      Exit;
    end;
  end;
end;

function TTaskEscalationManagerClass.CreateEscalationRule(aTaskID: string; aName: string;
                                                          aManager, aDirector, aCTO: string): string;
var
  newRule: TEscalationRule;
  idx: integer;
begin
  newRule.ID := GenerateRuleID();
  newRule.Name := aName;
  newRule.TaskID := aTaskID;
  newRule.TriggerCondition := 'Task Overdue';
  newRule.CurrentLevel := elLevel1;
  newRule.EscalationThreshold := 4;
  newRule.HoursOverdue := 0;
  newRule.Manager := aManager;
  newRule.Director := aDirector;
  newRule.CTO := aCTO;
  newRule.CreatedTime := Now();
  newRule.LastEscalatedTime := 0;
  newRule.IsActive := true;

  idx := Length(FRules);
  SetLength(FRules, idx + 1);
  FRules[idx] := newRule;

  CreateEscalationRule := newRule.ID;
end;

function TTaskEscalationManagerClass.GetRule(aRuleID: string): TEscalationRule;
var
  idx: integer;
  emptyRule: TEscalationRule;
begin
  FillChar(emptyRule, SizeOf(emptyRule), 0);
  idx := FindRuleIndex(aRuleID);
  if idx >= 0 then
  begin
    GetRule := FRules[idx];
  end
  else
  begin
    GetRule := emptyRule;
  end;
end;

function TTaskEscalationManagerClass.GetTaskRules(aTaskID: string): TEscalationRuleArray;
var
  i, count: integer;
  ruleArray: TEscalationRuleArray;
begin
  count := 0;
  SetLength(ruleArray, 0);
  for i := 0 to Length(FRules) - 1 do
  begin
    if FRules[i].TaskID = aTaskID then
    begin
      SetLength(ruleArray, count + 1);
      ruleArray[count] := FRules[i];
      inc(count);
    end;
  end;
  GetTaskRules := ruleArray;
end;

function TTaskEscalationManagerClass.GetActiveRules(): TEscalationRuleArray;
var
  i, count: integer;
  ruleArray: TEscalationRuleArray;
begin
  count := 0;
  SetLength(ruleArray, 0);
  for i := 0 to Length(FRules) - 1 do
  begin
    if FRules[i].IsActive then
    begin
      SetLength(ruleArray, count + 1);
      ruleArray[count] := FRules[i];
      inc(count);
    end;
  end;
  GetActiveRules := ruleArray;
end;

procedure TTaskEscalationManagerClass.UpdateEscalationLevel(aRuleID: string;
  aNewLevel: TEscalationLevel; aReason: string; aNotifiedTo: string);
var
  idx: integer;
  histEntry: TEscalationHistory;
  histIdx: integer;
begin
  idx := FindRuleIndex(aRuleID);
  if idx >= 0 then
  begin
    { Create history entry }
    histEntry.ID := GenerateHistoryID();
    histEntry.RuleID := aRuleID;
    histEntry.TaskID := FRules[idx].TaskID;
    histEntry.FromLevel := FRules[idx].CurrentLevel;
    histEntry.ToLevel := aNewLevel;
    histEntry.Reason := aReason;
    histEntry.EscalatedTime := Now();
    histEntry.NotifiedTo := aNotifiedTo;

    histIdx := Length(FHistory);
    SetLength(FHistory, histIdx + 1);
    FHistory[histIdx] := histEntry;

    { Update rule }
    FRules[idx].CurrentLevel := aNewLevel;
    FRules[idx].LastEscalatedTime := Now();
  end;
end;

procedure TTaskEscalationManagerClass.DeactivateRule(aRuleID: string);
var
  idx: integer;
begin
  idx := FindRuleIndex(aRuleID);
  if idx >= 0 then
    FRules[idx].IsActive := false;
end;

procedure TTaskEscalationManagerClass.ActivateRule(aRuleID: string);
var
  idx: integer;
begin
  idx := FindRuleIndex(aRuleID);
  if idx >= 0 then
    FRules[idx].IsActive := true;
end;

function TTaskEscalationManagerClass.GetEscalationLevel(aRuleID: string): TEscalationLevel;
var
  idx: integer;
begin
  idx := FindRuleIndex(aRuleID);
  if idx >= 0 then
  begin
    GetEscalationLevel := FRules[idx].CurrentLevel;
  end
  else
  begin
    GetEscalationLevel := elLevel1;
  end;
end;

function TTaskEscalationManagerClass.GetLevelName(aLevel: TEscalationLevel): string;
begin
  case aLevel of
    elLevel1: GetLevelName := 'Level 1 - Team Lead';
    elLevel2: GetLevelName := 'Level 2 - Manager';
    elLevel3: GetLevelName := 'Level 3 - Director';
    elLevel4: GetLevelName := 'Level 4 - CTO/VP';
    elLevel5: GetLevelName := 'Level 5 - Executive';
  else
    GetLevelName := 'Unknown';
  end;
end;

function TTaskEscalationManagerClass.GetRulesAtLevel(aLevel: TEscalationLevel): TEscalationRuleArray;
var
  i, count: integer;
  ruleArray: TEscalationRuleArray;
begin
  count := 0;
  SetLength(ruleArray, 0);
  for i := 0 to Length(FRules) - 1 do
  begin
    if FRules[i].CurrentLevel = aLevel then
    begin
      SetLength(ruleArray, count + 1);
      ruleArray[count] := FRules[i];
      inc(count);
    end;
  end;
  GetRulesAtLevel := ruleArray;
end;

function TTaskEscalationManagerClass.GetRulesbyStatus(aIsActive: boolean): TEscalationRuleArray;
var
  i, count: integer;
  ruleArray: TEscalationRuleArray;
begin
  count := 0;
  SetLength(ruleArray, 0);
  for i := 0 to Length(FRules) - 1 do
  begin
    if FRules[i].IsActive = aIsActive then
    begin
      SetLength(ruleArray, count + 1);
      ruleArray[count] := FRules[i];
      inc(count);
    end;
  end;
  GetRulesbyStatus := ruleArray;
end;

function TTaskEscalationManagerClass.GetEscalationHistory(aRuleID: string): TEscalationHistoryArray;
var
  i, count: integer;
  histArray: TEscalationHistoryArray;
begin
  count := 0;
  SetLength(histArray, 0);
  for i := 0 to Length(FHistory) - 1 do
  begin
    if FHistory[i].RuleID = aRuleID then
    begin
      SetLength(histArray, count + 1);
      histArray[count] := FHistory[i];
      inc(count);
    end;
  end;
  GetEscalationHistory := histArray;
end;

function TTaskEscalationManagerClass.GetTotalEscalations(): integer;
begin
  GetTotalEscalations := Length(FHistory);
end;

function TTaskEscalationManagerClass.GetAverageTimeToEscalation(): double;
var
  i: integer;
  totalTime: double;
  count: integer;
begin
  if Length(FHistory) = 0 then
  begin
    GetAverageTimeToEscalation := 0.0;
    Exit;
  end;

  totalTime := 0.0;
  count := 0;

  for i := 0 to Length(FHistory) - 1 do
  begin
    { Calculate time difference in hours }
    totalTime := totalTime + (FHistory[i].EscalatedTime - Now()) * 24;
    inc(count);
  end;

  if count > 0 then
    GetAverageTimeToEscalation := totalTime / count
  else
    GetAverageTimeToEscalation := 0.0;
end;

procedure TTaskEscalationManagerClass.ClearOldHistory(aDaysOld: integer);
var
  i, writeIdx: integer;
  cutoffDate: TDateTime;
begin
  cutoffDate := Now() - aDaysOld;
  writeIdx := 0;

  for i := 0 to Length(FHistory) - 1 do
  begin
    if FHistory[i].EscalatedTime > cutoffDate then
    begin
      FHistory[writeIdx] := FHistory[i];
      inc(writeIdx);
    end;
  end;

  SetLength(FHistory, writeIdx);
end;

function TTaskEscalationManagerClass.GetAllRules(): TEscalationRuleArray;
begin
  GetAllRules := FRules;
end;

procedure TTaskEscalationManagerClass.SelfTest();
var
  ruleID1, ruleID2: string;
  rule: TEscalationRule;
  rules: TEscalationRuleArray;
  activeRules: TEscalationRuleArray;
  hist: TEscalationHistoryArray;
begin
  WriteLn('--- Task Escalation Manager Self Test ---');

  { Test rule creation }
  ruleID1 := CreateEscalationRule('TASK-001', 'Critical Issue', 'john@company.com',
    'jane@company.com', 'cto@company.com');
  ruleID2 := CreateEscalationRule('TASK-002', 'High Priority', 'john@company.com',
    'jane@company.com', 'cto@company.com');

  WriteLn('✓ Created 2 escalation rules');

  { Test rule retrieval }
  rule := GetRule(ruleID1);
  if rule.ID = ruleID1 then
    WriteLn('✓ Rule retrieval works')
  else
    WriteLn('✗ Rule retrieval failed');

  { Test getting task rules }
  rules := GetTaskRules('TASK-001');
  if Length(rules) = 1 then
    WriteLn('✓ Task rule filtering works')
  else
    WriteLn('✗ Task rule filtering failed');

  { Test active rules }
  activeRules := GetActiveRules();
  if Length(activeRules) = 2 then
    WriteLn('✓ Active rules: ', Length(activeRules))
  else
    WriteLn('✗ Active rules count mismatch');

  { Test level name }
  WriteLn('✓ Level 2 Name: ', GetLevelName(elLevel2));

  { Test escalation }
  UpdateEscalationLevel(ruleID1, elLevel2, 'Task overdue by 2 hours',
    'jane@company.com');
  WriteLn('✓ Escalation level updated');

  { Test escalation history }
  hist := GetEscalationHistory(ruleID1);
  if Length(hist) = 1 then
    WriteLn('✓ Escalation history: ', Length(hist), ' event(s)')
  else
    WriteLn('✗ Escalation history failed');

  { Test rule deactivation }
  DeactivateRule(ruleID2);
  rules := GetRulesbyStatus(false);
  if Length(rules) = 1 then
    WriteLn('✓ Rule deactivation works')
  else
    WriteLn('✗ Rule deactivation failed');

  { Test total escalations }
  WriteLn('✓ Total escalations: ', GetTotalEscalations());

  WriteLn('--- Escalation Manager Tests Complete ---');
  WriteLn('');
end;

end.
