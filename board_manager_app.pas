
program board_manager_app;
{$mode objfpc}

uses
  sysutils, uboardmanager;

var
  bm: TBoardManager;

begin
  bm := TBoardManager.create;
  try
    bm.self_test;
  finally
    bm.free;
  end;
end.
