
program task_manager_app;

{$mode objfpc}
{$h+}

uses
  SysUtils, utaskmanager, utasktemplates;

var
  manager: ttaskmanager;
  template_mgr: ttasktemplatemanager;

begin
  manager := ttaskmanager.create;
  template_mgr := ttasktemplatemanager.create;
  try
    manager.self_test;
    
    writeln('Testing Templates...');
    template_mgr.add_template('Bug', 'New Bug Report', 'Fix it', 'QA', tp_high);
    if template_mgr.apply_template('Bug', manager) <> -1 then
      writeln('Template applied successfully.');
      
  finally
    template_mgr.free;
    manager.free;
  end;
end.
