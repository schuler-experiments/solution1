
program team_test;
{$mode objfpc}{$H+}
uses
  SysUtils, utaskmanager, uteammanager;
var
  TM: ttaskmanager;
  Team: TTeamManager;
begin
  TM := ttaskmanager.create;
  Team := TTeamManager.Create;
  try
    Team.self_test(TM);
  finally
    Team.Free;
    TM.free;
  end;
end.
