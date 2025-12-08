
program solution18;

{$mode objfpc}{$H+}

uses
  SysUtils, taskmanager, taskmanagercomments;

var
  Manager: TCommentedTaskManager;

procedure SelfTest;
begin
  WriteLn('Task Manager with Comments System - Comprehensive Test');
  WriteLn('======================================================');
  WriteLn;
  
  Manager := TCommentedTaskManager.Create;
  try
    Manager.SelfTest;
  finally
    Manager.Free;
  end;
end;

begin
  SelfTest;
end.
