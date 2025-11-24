
unit TaskStakeholderExtended;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TStringArray = array of string;
  TIntegerArray = array of integer;

  { Stakeholder roles }
  TStakeholderRole = (srSponsor, srProjectManager, srTeamMember, srClient, srVendor, srExecutive);

  { Stakeholder engagement levels }
  TEngagementLevel = (elManage, elKeepInformed, elMonitor, elKeepSatisfied, elUnknown);

  { Stakeholder record }
  TStakeholder = record
    id: integer;
    name: string;
    email: string;
    role: TStakeholderRole;
    department: string;
    engagementLevel: TEngagementLevel;
    influenceScore: double;
    interestScore: double;
    createdDate: TDateTime;
    lastInteraction: TDateTime;
    relatedTaskIds: array of integer;
    communicationPreference: string;
  end;

  TStakeholderArray = array of TStakeholder;

  { Helper class for stakeholder analysis and extended operations }
  TStakeholderAnalysisHelper = class
  public
    { Analysis functions }
    function CalculateEngagementMatrix(var stakeholders: array of TStakeholder): string;
    function GetStakeholderPriority(id: integer; var stakeholders: array of TStakeholder): string;
    function GetHighInfluenceStakeholders(minScore: double; var stakeholders: array of TStakeholder): TStakeholderArray;
    function GetHighInterestStakeholders(minScore: double; var stakeholders: array of TStakeholder): TStakeholderArray;
    
    { Score update helpers }
    function UpdateInfluenceScoreInArray(id: integer; score: double; var stakeholders: array of TStakeholder): boolean;
    function UpdateInterestScoreInArray(id: integer; score: double; var stakeholders: array of TStakeholder): boolean;
    function UpdateCommunicationPrefInArray(id: integer; pref: string; var stakeholders: array of TStakeholder): boolean;
  end;

implementation

function TStakeholderAnalysisHelper.CalculateEngagementMatrix(var stakeholders: array of TStakeholder): string;
var
  manage, keepInformed, monitor, keepSatisfied: integer;
  i: integer;
begin
  manage := 0;
  keepInformed := 0;
  monitor := 0;
  keepSatisfied := 0;

  for i := 0 to Length(stakeholders) - 1 do
  begin
    case stakeholders[i].engagementLevel of
      elManage: Inc(manage);
      elKeepInformed: Inc(keepInformed);
      elMonitor: Inc(monitor);
      elKeepSatisfied: Inc(keepSatisfied);
    end;
  end;

  result := Format(
    'Engagement Matrix:'#10 +
    '  Manage: %d'#10 +
    '  Keep Informed: %d'#10 +
    '  Monitor: %d'#10 +
    '  Keep Satisfied: %d',
    [manage, keepInformed, monitor, keepSatisfied]
  );
end;

function TStakeholderAnalysisHelper.GetStakeholderPriority(id: integer; var stakeholders: array of TStakeholder): string;
var
  i: integer;
  influenceScore, interestScore: double;
begin
  result := 'Unknown';

  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].id = id then
    begin
      influenceScore := stakeholders[i].influenceScore;
      interestScore := stakeholders[i].interestScore;

      if (influenceScore >= 7) and (interestScore >= 7) then
        result := 'Manage Closely'
      else if (influenceScore >= 7) and (interestScore < 7) then
        result := 'Keep Satisfied'
      else if (influenceScore < 7) and (interestScore >= 7) then
        result := 'Keep Informed'
      else
        result := 'Monitor';

      exit;
    end;
  end;
end;

function TStakeholderAnalysisHelper.GetHighInfluenceStakeholders(minScore: double; var stakeholders: array of TStakeholder): TStakeholderArray;
var
  result_arr: TStakeholderArray;
  count: integer;
  i: integer;
begin
  SetLength(result_arr, 0);
  count := 0;

  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].influenceScore >= minScore then
    begin
      SetLength(result_arr, count + 1);
      result_arr[count] := stakeholders[i];
      Inc(count);
    end;
  end;

  result := result_arr;
end;

function TStakeholderAnalysisHelper.GetHighInterestStakeholders(minScore: double; var stakeholders: array of TStakeholder): TStakeholderArray;
var
  result_arr: TStakeholderArray;
  count: integer;
  i: integer;
begin
  SetLength(result_arr, 0);
  count := 0;

  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].interestScore >= minScore then
    begin
      SetLength(result_arr, count + 1);
      result_arr[count] := stakeholders[i];
      Inc(count);
    end;
  end;

  result := result_arr;
end;

function TStakeholderAnalysisHelper.UpdateInfluenceScoreInArray(id: integer; score: double; var stakeholders: array of TStakeholder): boolean;
var
  i: integer;
begin
  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].id = id then
    begin
      stakeholders[i].influenceScore := score;
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TStakeholderAnalysisHelper.UpdateInterestScoreInArray(id: integer; score: double; var stakeholders: array of TStakeholder): boolean;
var
  i: integer;
begin
  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].id = id then
    begin
      stakeholders[i].interestScore := score;
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TStakeholderAnalysisHelper.UpdateCommunicationPrefInArray(id: integer; pref: string; var stakeholders: array of TStakeholder): boolean;
var
  i: integer;
begin
  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].id = id then
    begin
      stakeholders[i].communicationPreference := pref;
      result := true;
      exit;
    end;
  end;
  result := false;
end;

end.
