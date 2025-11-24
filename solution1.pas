program TaskManagerApp;

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
  TaskValidation;

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
  end;
end.
