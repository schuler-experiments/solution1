
program solution6;
{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  taskmanager, taskmanagerext, taskmanageradvanced, 
  taskmanagerenhanced, taskmanagerteam, taskmanagergamify,
  taskmanagersmart;

procedure SelfTest;
var
  Manager: TSmartTaskManager;
  TaskID1, TaskID2, TaskID3, TaskID4: Integer;
  Rule1, Rule2: Integer;
  Conditions: TWorkflowConditionArray;
  Condition: TWorkflowCondition;
  AllRules: TWorkflowRuleArray;
  HighRiskTasks: TRiskAssessmentArray;
  Assessment: TRiskAssessment;
  i: Integer;
  PredictedDate: TDateTime;
begin
  WriteLn(StringOfChar('=', 70));
  WriteLn('TASK MANAGER - SOLUTION 6 (SMART ANALYTICS & AUTOMATION)');
  WriteLn('Testing Layer 6: AI-Like Intelligence, Workflow Automation');
  WriteLn(StringOfChar('=', 70));
  WriteLn;

  Manager := TSmartTaskManager.Create;
  try
    Manager.SetCurrentUser('DataScientist');
    
    // Test 1: Create tasks with varying risk levels
    WriteLn('TEST 1: Task Creation with Risk Assessment');
    WriteLn(StringOfChar('-', 70));
    
    TaskID1 := Manager.AddTaskWithAudit('Critical Bug Fix',
      'Production database connection failing intermittently', 'Backend',
      tpCritical, Now + 0.5, 8.0); // Due in 12 hours
    WriteLn('Created Task #', TaskID1, ': Critical Bug Fix (URGENT)');
    
    TaskID2 := Manager.AddTaskWithAudit('Optimize Query Performance',
      'Slow queries affecting user experience', 'Backend',
      tpHigh, Now + 3, 12.0); // Due in 3 days
    WriteLn('Created Task #', TaskID2, ': Optimize Query Performance');
    
    TaskID3 := Manager.AddTaskWithAudit('Update Documentation',
      'Add examples to API documentation', 'Documentation',
      tpLow, Now + 14, 4.0); // Due in 2 weeks
    WriteLn('Created Task #', TaskID3, ': Update Documentation');
    
    TaskID4 := Manager.AddTaskWithAudit('Refactor Legacy Code',
      'Clean up old authentication module', 'Backend',
      tpMedium, Now + 7, 20.0); // Due in 1 week
    WriteLn('Created Task #', TaskID4, ': Refactor Legacy Code');
    WriteLn;
    
    // Test 2: Risk Assessment
    WriteLn('TEST 2: Smart Risk Assessment');
    WriteLn(StringOfChar('-', 70));
    
    for i := 1 to 4 do
    begin
      Assessment := Manager.AssessTaskRisk(i);
      WriteLn(Format('Task #%d Risk: %s (Score: %.1f/100)',
        [Assessment.TaskID, Manager.RiskLevelToString(Assessment.RiskLevel),
         Assessment.RiskScore]));
    end;
    WriteLn;
    
    HighRiskTasks := Manager.GetHighRiskTasks;
    WriteLn(Format('High-risk tasks detected: %d', [Length(HighRiskTasks)]));
    for i := 0 to High(HighRiskTasks) do
      WriteLn('  - Task #', HighRiskTasks[i].TaskID, 
        ' (', Manager.RiskLevelToString(HighRiskTasks[i].RiskLevel), ')');
    WriteLn;
    
    // Test 3: Workflow Automation Rules
    WriteLn('TEST 3: Workflow Automation Rules');
    WriteLn(StringOfChar('-', 70));
    
    // Rule 1: Auto-tag high-priority tasks
    SetLength(Conditions, 1);
    Condition.Field := 'Priority';
    Condition.CompareOp := '=';
    Condition.Value := 'High';
    Conditions[0] := Condition;
    
    Rule1 := Manager.AddWorkflowRule(
      'Auto-tag High Priority',
      'Automatically tag tasks marked as high priority',
      wtTaskCreated,
      Conditions,
      waAddTag,
      'urgent'
    );
    WriteLn('Created workflow rule #', Rule1, ': Auto-tag High Priority');
    
    // Rule 2: Archive completed low-priority tasks
    SetLength(Conditions, 2);
    Condition.Field := 'Priority';
    Condition.CompareOp := '=';
    Condition.Value := 'Low';
    Conditions[0] := Condition;
    Condition.Field := 'Status';
    Condition.CompareOp := '=';
    Condition.Value := 'Completed';
    Conditions[1] := Condition;
    
    Rule2 := Manager.AddWorkflowRule(
      'Archive Low Priority Completed',
      'Auto-archive completed low-priority tasks',
      wtTaskCompleted,
      Conditions,
      waArchive,
      ''
    );
    WriteLn('Created workflow rule #', Rule2, ': Archive Low Priority Completed');
    WriteLn;
    
    AllRules := Manager.GetActiveWorkflowRules;
    WriteLn(Format('Active workflow rules: %d', [Length(AllRules)]));
    for i := 0 to High(AllRules) do
      WriteLn(Format('  Rule #%d: %s (Trigger: %s, Action: %s)',
        [AllRules[i].ID, AllRules[i].Name,
         Manager.WorkflowTriggerToString(AllRules[i].Trigger),
         Manager.WorkflowActionToString(AllRules[i].Action)]));
    WriteLn;
    
    // Test 4: Predictive Analytics
    WriteLn('TEST 4: Predictive Analytics');
    WriteLn(StringOfChar('-', 70));
    
    for i := 1 to 4 do
    begin
      PredictedDate := Manager.PredictCompletionDate(i);
      if PredictedDate > 0 then
        WriteLn(Format('Task #%d predicted completion: %s',
          [i, FormatDateTime('yyyy-mm-dd hh:nn', PredictedDate)]));
    end;
    WriteLn;
    
    // Test 5: Analytics Status
    WriteLn('TEST 5: Smart Features Status');
    WriteLn(StringOfChar('-', 70));
    WriteLn(Manager.GetAnalyticsStatus);
    WriteLn;
    
    WriteLn('Efficiency Report:');
    WriteLn(Manager.GetEfficiencyReport);
    WriteLn;
    
    WriteLn('Bottleneck Analysis:');
    WriteLn(Manager.GetBottleneckAnalysis);
    WriteLn;
    
    WriteLn('Optimization Recommendations:');
    WriteLn(Manager.GetOptimizationRecommendations);
    WriteLn;
    
    // Test 6: Integration with Gamification
    WriteLn('TEST 6: Integration with Previous Features');
    WriteLn(StringOfChar('-', 70));
    
    // Complete a task and check gamification integration
    Manager.CompleteTaskWithRewards(TaskID3);
    WriteLn('Completed Task #', TaskID3, ' with gamification rewards');
    
    WriteLn('Current XP: ', Manager.GetCurrentLevel.CurrentXP);
    WriteLn('Current Level: ', Manager.GetCurrentLevel.Level);
    WriteLn('Current Title: ', Manager.GetCurrentLevel.Title);
    WriteLn;
    
    // Trigger workflow on completion
    Manager.TriggerWorkflow(wtTaskCompleted, TaskID3);
    WriteLn('Triggered workflow automation for completed task');
    WriteLn;
    
    WriteLn(StringOfChar('=', 70));
    WriteLn('ALL TESTS COMPLETED SUCCESSFULLY!');
    WriteLn('Smart Analytics & Workflow Automation features are working!');
    WriteLn(StringOfChar('=', 70));
    
  finally
    Manager.Free;
  end;
end;

begin
  try
    SelfTest;
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end.
