
program solution19;

{$mode objfpc}{$H+}

uses
  SysUtils, taskmanager, taskmanagertemplates;

var
  Manager: TTemplateTaskManager;

procedure SelfTest;
begin
  WriteLn('Task Manager with Template System - Comprehensive Test');
  WriteLn('=======================================================');
  WriteLn;
  
  Manager := TTemplateTaskManager.Create;
  try
    Manager.SelfTest;
  finally
    Manager.Free;
  end;
end;

begin
  SelfTest;
end.
