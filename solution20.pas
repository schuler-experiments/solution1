
program solution20;

{$mode objfpc}{$H+}

uses
  SysUtils, taskmanager, taskmanagerenhanced, taskmanagernotifications;

var
  Manager: TNotificationTaskManager;

procedure SelfTest;
begin
  WriteLn('Task Manager with Notification System - Comprehensive Test');
  WriteLn('==========================================================');
  WriteLn;
  
  Manager := TNotificationTaskManager.Create;
  try
    Manager.SelfTest;
  finally
    Manager.Free;
  end;
end;

begin
  SelfTest;
end.
