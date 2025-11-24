
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
  TaskCollaboration;

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

begin
  WriteLn('╔════════════════════════════════════════╗');
  WriteLn('║   Free Pascal Advanced Task Manager    ║');
  WriteLn('╚════════════════════════════════════════╝');
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
    WriteLn('╔════════════════════════════════════════╗');
    WriteLn('║     All Self-Tests Completed!         ║');
    WriteLn('╚════════════════════════════════════════╝');
    WriteLn('');
    WriteLn('System Status: ✓ All modules operational');
    WriteLn('Ready for integration with UI frameworks.');
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
    if rep <> nil then
      rep.Free();
  end;
end.
