
program TaskManagerApp;

{$mode objfpc}

uses
  SysUtils,
  DateUtils,
  TaskTypes,
  TaskManager,
  TaskPersistence,
  TaskQuery;

var
  mgr: TTaskManagerClass;
  persistence: TTaskPersistenceClass;
  query: TTaskQueryClass;

begin
  WriteLn('╔════════════════════════════════════════╗');
  WriteLn('║   Free Pascal Advanced Task Manager    ║');
  WriteLn('╚════════════════════════════════════════╝');
  WriteLn('');
  
  { Create manager }
  mgr := TTaskManagerClass.Create();
  persistence := TTaskPersistenceClass.Create('tasks.csv');
  query := TTaskQueryClass.Create(mgr);
  
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
  end;
end.
