
program TaskManagerApp;
{ Rebuild timestamp: 1763987021.8999279 }

{$mode objfpc}

uses
  SysUtils,
  DateUtils,
  TaskTypes,
  TaskManager,
  TaskPersistence,
  TaskQuery,
  TaskNotes,
  TaskScheduling,
  TaskDependencies,
  TaskValidation,
  TaskCategories,
  TaskPriorityScoring,
  TaskReporting,
  TaskTeam,
  TaskBudget,
  TaskRisk,
  TaskCollaboration,
  TaskReminders,
  TaskAnalytics,
  TaskExport,
  TaskSearch,
  TaskAudit,
  TaskAuditAnalysis,
  TaskSLA,
  TaskSLAAnalysis,
  TaskQuality,
  TaskQualityAnalysis,
  TaskKnowledgeBase,
  TaskKnowledgeBaseSearch,
  TaskStakeholder,
  TaskStakeholderExtended,
  TaskWorkflow,
  TaskWorkflowApproval,
  TaskResourceOptimization;

var
  mgr: TTaskManagerClass;
  persistence: TTaskPersistenceClass;
  query: TTaskQueryClass;
  notesMgr: TTaskNotesManagerClass;
  historyTracker: TTaskHistoryTrackerClass;
  recurringMgr: TRecurringTaskManagerClass;
  timeMgr: TTimeTrackingManagerClass;
  depMgr: TTaskDependencyManagerClass;
  validator: TTaskValidatorClass;
  catMgr: TTaskCategoryManagerClass;
  priorityScorer: TTaskPriorityScorerClass;
  rep: TTaskReportClass;
  teamMgr: TTaskTeamManagerClass;
  budgetMgr: TTaskBudgetManagerClass;
  riskMgr: TTaskRiskManagerClass;
  collabMgr: TTaskCollaborationManagerClass;
  reminderMgr: TTaskReminderManagerClass;
  analyticsMgr: TTaskAnalyticsClass;
  exporterMgr: TTaskExporterClass;
  searchMgr: TTaskSearchClass;
  auditMgr: TTaskAuditManagerClass;
  slaMgr: TTaskSLAManagerClass;
  qualityMgr: TTaskQualityManagerClass;
  knowledgeMgr: TKnowledgeBaseClass;
  stakeholderMgr: TTaskStakeholderManagerClass;
  workflowMgr: TTaskWorkflowManagerClass;
  optimizerMgr: TTaskResourceOptimizerClass;

begin
  WriteLn('========================================');
  WriteLn('   Free Pascal Advanced Task Manager');
  WriteLn('========================================');
  WriteLn('');
  
  { Create managers }
  mgr := TTaskManagerClass.Create();
  persistence := TTaskPersistenceClass.Create('tasks.csv');
  query := TTaskQueryClass.Create(mgr);
  notesMgr := TTaskNotesManagerClass.Create();
  historyTracker := TTaskHistoryTrackerClass.Create();
  recurringMgr := TRecurringTaskManagerClass.Create();
  timeMgr := TTimeTrackingManagerClass.Create();
  depMgr := TTaskDependencyManagerClass.Create();
  validator := TTaskValidatorClass.Create();
  catMgr := TTaskCategoryManagerClass.Create();
  priorityScorer := TTaskPriorityScorerClass.Create();
  teamMgr := TTaskTeamManagerClass.Create();
  budgetMgr := TTaskBudgetManagerClass.Create(50000.0);
  riskMgr := TTaskRiskManagerClass.Create();
  collabMgr := TTaskCollaborationManagerClass.Create();
  rep := nil;
  reminderMgr := TTaskReminderManagerClass.Create();
  analyticsMgr := TTaskAnalyticsClass.Create();
  exporterMgr := TTaskExporterClass.Create();
  searchMgr := TTaskSearchClass.Create();
  auditMgr := TTaskAuditManagerClass.Create();
  slaMgr := TTaskSLAManagerClass.Create();
  qualityMgr := TTaskQualityManagerClass.Create();
  knowledgeMgr := TKnowledgeBaseClass.Create();
  stakeholderMgr := TTaskStakeholderManagerClass.Create();
  workflowMgr := TTaskWorkflowManagerClass.Create();
  optimizerMgr := TTaskResourceOptimizerClass.Create();
  
  try
    { Run core self-tests }
    WriteLn('');
    mgr.SelfTest();
    
    { Run persistence self-tests }
    WriteLn('');
    persistence.SelfTest();
    
    { Run query self-tests }
    WriteLn('');
    query.SelfTest();
    
    { Run advanced features self-tests }
    WriteLn('');
    notesMgr.SelfTest();
    
    WriteLn('');
    historyTracker.SelfTest();
    
    WriteLn('');
    recurringMgr.SelfTest();
    
    WriteLn('');
    timeMgr.SelfTest();
    
    WriteLn('');
    depMgr.SelfTest();
    
    WriteLn('');
    validator.SelfTest();
    
    { Run new feature self-tests }
    WriteLn('');
    catMgr.SelfTest();
    
    WriteLn('');
    priorityScorer.SelfTest();
    
    WriteLn('');
    teamMgr.SelfTest();
    
    WriteLn('');
    budgetMgr.SelfTest();
    
    WriteLn('');
    riskMgr.SelfTest();

    WriteLn('');
    collabMgr.SelfTest();
    
    WriteLn('');
    if Length(mgr.GetAllTasks()) > 0 then
    begin
      rep := TTaskReportClass.Create(mgr.GetAllTasks());
      try
        rep.SelfTest();
      finally
        rep.Free();
        rep := nil;
      end;
    end;

    WriteLn('');
    reminderMgr.SelfTest();

    WriteLn('');
    analyticsMgr.SelfTest();

    WriteLn('');
    exporterMgr.SelfTest();

    WriteLn('');
    searchMgr.SelfTest();

    { Run new feature tests }
    WriteLn('');
    auditMgr.SelfTest();

    WriteLn('');
    slaMgr.SelfTest();

    WriteLn('');
    qualityMgr.SelfTest();

    { Run knowledge base and stakeholder tests }
    WriteLn('');
    knowledgeMgr.SelfTest();

    WriteLn('');
    stakeholderMgr.SelfTest();

    WriteLn('');
    workflowMgr.SelfTest();

    WriteLn('');
    optimizerMgr.SelfTest();
    
    WriteLn('');
    WriteLn('========================================');
    WriteLn('     All Self-Tests Completed!');
    WriteLn('========================================');
    WriteLn('');
    WriteLn('System Status: All modules operational');
    WriteLn('Ready for integration with UI frameworks.');
    WriteLn('');
    WriteLn('New Features Added:');
    WriteLn('  - Audit Trail Tracking');
    WriteLn('  - Service Level Agreement (SLA) Management');
    WriteLn('  - Quality Metrics & Analysis');
    WriteLn('  - Knowledge Base & Best Practices');
    WriteLn('  - Stakeholder Management & Engagement');
    WriteLn('  - Workflow & State Management');
    WriteLn('  - Resource Optimization & Allocation');
    WriteLn('');
    
  finally
    mgr.Free();
    persistence.Free();
    query.Free();
    notesMgr.Free();
    historyTracker.Free();
    recurringMgr.Free();
    timeMgr.Free();
    depMgr.Free();
    validator.Free();
    catMgr.Free();
    priorityScorer.Free();
    teamMgr.Free();
    budgetMgr.Free();
    riskMgr.Free();
    collabMgr.Free();
    reminderMgr.Free();
    analyticsMgr.Free();
    exporterMgr.Free();
    searchMgr.Free();
    auditMgr.Free();
    slaMgr.Free();
    qualityMgr.Free();
    knowledgeMgr.Free();
    stakeholderMgr.Free();
    workflowMgr.Free();
    optimizerMgr.Free();
    if rep <> nil then
      rep.Free();
  end;
end.
