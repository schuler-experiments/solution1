
unit TaskStakeholder;

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
    influenceScore: double; { 0-10 scale }
    interestScore: double;  { 0-10 scale }
    createdDate: TDateTime;
    lastInteraction: TDateTime;
    relatedTaskIds: array of integer;
    communicationPreference: string; { Email, Meeting, Phone, etc }
  end;

  TStakeholderArray = array of TStakeholder;

  { Stakeholder interaction record }
  TStakeholderInteraction = record
    id: integer;
    stakeholderId: integer;
    interactionType: string; { Email, Meeting, Call, Status Update, etc }
    subject: string;
    summary: string;
    date: TDateTime;
    outcome: string;
  end;

  TInteractionArray = array of TStakeholderInteraction;

  TTaskStakeholderManagerClass = class
  private
    stakeholders: TStakeholderArray;
    interactions: TInteractionArray;
    nextStakeholderId: integer;
    nextInteractionId: integer;
    function FindStakeholderIndex(id: integer): integer;
    function FindInteractionIndex(id: integer): integer;
    function RoleToString(role: TStakeholderRole): string;
    function StringToRole(s: string): TStakeholderRole;
    function EngagementToString(level: TEngagementLevel): string;
  public
    constructor Create();
    destructor Destroy();
    
    { Stakeholder Management }
    function AddStakeholder(name, email: string; role: TStakeholderRole;
                           department: string): integer;
    function GetStakeholder(id: integer): TStakeholder;
    function UpdateStakeholder(id: integer; newStakeholder: TStakeholder): boolean;
    function DeleteStakeholder(id: integer): boolean;
    function GetAllStakeholders(): TStakeholderArray;
    function GetStakeholderCount(): integer;
    
    { Stakeholder Filtering }
    function GetStakeholdersByRole(role: TStakeholderRole): TStakeholderArray;
    function GetStakeholdersByDepartment(dept: string): TStakeholderArray;
    function GetStakeholdersByEngagement(level: TEngagementLevel): TStakeholderArray;
    function GetHighInfluenceStakeholders(minScore: double): TStakeholderArray;
    function GetHighInterestStakeholders(minScore: double): TStakeholderArray;
    
    { Influence & Interest Matrix }
    function CalculateMatrix(): string;
    function GetStakeholderPriority(id: integer): string;
    
    { Engagement Management }
    function SetEngagementLevel(id: integer; level: TEngagementLevel): boolean;
    function UpdateInfluenceScore(id: integer; score: double): boolean;
    function UpdateInterestScore(id: integer; score: double): boolean;
    function UpdateCommunicationPreference(id: integer; pref: string): boolean;
    
    { Task Assignment }
    function LinkTaskToStakeholder(stakeholderId, taskId: integer): boolean;
    function UnlinkTaskFromStakeholder(stakeholderId, taskId: integer): boolean;
    function GetStakeholderTasks(stakeholderId: integer): TIntegerArray;
    
    { Interaction Tracking }
    function LogInteraction(stakeholderId: integer; interactionType, subject,
                           summary, outcome: string): integer;
    function GetStakeholderInteractions(stakeholderId: integer): TInteractionArray;
    function GetRecentInteractions(days: integer): TInteractionArray;
    function GetInteractionCount(stakeholderId: integer): integer;
    
    { Communication Planning }
    function GetCommunicationPlan(): string;
    function GetEngagementSummary(): string;
    
    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskStakeholderManagerClass.Create();
begin
  SetLength(stakeholders, 0);
  SetLength(interactions, 0);
  nextStakeholderId := 1;
  nextInteractionId := 1;
end;

destructor TTaskStakeholderManagerClass.Destroy();
begin
  SetLength(stakeholders, 0);
  SetLength(interactions, 0);
  inherited Destroy();
end;

function TTaskStakeholderManagerClass.FindStakeholderIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].id = id then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TTaskStakeholderManagerClass.FindInteractionIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(interactions) - 1 do
  begin
    if interactions[i].id = id then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TTaskStakeholderManagerClass.RoleToString(role: TStakeholderRole): string;
begin
  case role of
    srSponsor: result := 'Sponsor';
    srProjectManager: result := 'Project Manager';
    srTeamMember: result := 'Team Member';
    srClient: result := 'Client';
    srVendor: result := 'Vendor';
    srExecutive: result := 'Executive';
  else
    result := 'Unknown';
  end;
end;

function TTaskStakeholderManagerClass.StringToRole(s: string): TStakeholderRole;
begin
  if s = 'Sponsor' then
    result := srSponsor
  else if s = 'Project Manager' then
    result := srProjectManager
  else if s = 'Team Member' then
    result := srTeamMember
  else if s = 'Client' then
    result := srClient
  else if s = 'Vendor' then
    result := srVendor
  else if s = 'Executive' then
    result := srExecutive
  else
    result := srTeamMember;
end;

function TTaskStakeholderManagerClass.EngagementToString(level: TEngagementLevel): string;
begin
  case level of
    elManage: result := 'Manage';
    elKeepInformed: result := 'Keep Informed';
    elMonitor: result := 'Monitor';
    elKeepSatisfied: result := 'Keep Satisfied';
  else
    result := 'Unknown';
  end;
end;

function TTaskStakeholderManagerClass.AddStakeholder(name, email: string; 
                                                    role: TStakeholderRole;
                                                    department: string): integer;
var
  stakeholder: TStakeholder;
begin
  stakeholder.id := nextStakeholderId;
  stakeholder.name := name;
  stakeholder.email := email;
  stakeholder.role := role;
  stakeholder.department := department;
  stakeholder.engagementLevel := elMonitor;
  stakeholder.influenceScore := 5.0;
  stakeholder.interestScore := 5.0;
  stakeholder.createdDate := Now();
  stakeholder.lastInteraction := Now();
  SetLength(stakeholder.relatedTaskIds, 0);
  stakeholder.communicationPreference := 'Email';
  
  SetLength(stakeholders, Length(stakeholders) + 1);
  stakeholders[Length(stakeholders) - 1] := stakeholder;
  
  result := nextStakeholderId;
  Inc(nextStakeholderId);
end;

function TTaskStakeholderManagerClass.GetStakeholder(id: integer): TStakeholder;
var
  idx: integer;
begin
  idx := FindStakeholderIndex(id);
  if idx >= 0 then
  begin
    stakeholders[idx].lastInteraction := Now();
    result := stakeholders[idx];
  end
  else
  begin
    FillChar(result, SizeOf(result), 0);
    result.id := -1;
  end;
end;

function TTaskStakeholderManagerClass.UpdateStakeholder(id: integer; 
                                                       newStakeholder: TStakeholder): boolean;
var
  idx: integer;
begin
  idx := FindStakeholderIndex(id);
  if idx >= 0 then
  begin
    newStakeholder.id := id;
    newStakeholder.lastInteraction := Now();
    stakeholders[idx] := newStakeholder;
    result := true;
  end
  else
    result := false;
end;

function TTaskStakeholderManagerClass.DeleteStakeholder(id: integer): boolean;
var
  idx, i: integer;
begin
  idx := FindStakeholderIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(stakeholders) - 2 do
      stakeholders[i] := stakeholders[i + 1];
    SetLength(stakeholders, Length(stakeholders) - 1);
    result := true;
  end
  else
    result := false;
end;

function TTaskStakeholderManagerClass.GetAllStakeholders(): TStakeholderArray;
begin
  result := Copy(stakeholders, 0, Length(stakeholders));
end;

function TTaskStakeholderManagerClass.GetStakeholderCount(): integer;
begin
  result := Length(stakeholders);
end;

function TTaskStakeholderManagerClass.GetStakeholdersByRole(
                                    role: TStakeholderRole): TStakeholderArray;
var
  i, count: integer;
  matches: TStakeholderArray;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].role = role then
    begin
      SetLength(matches, count + 1);
      matches[count] := stakeholders[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TTaskStakeholderManagerClass.GetStakeholdersByDepartment(
                                    dept: string): TStakeholderArray;
var
  i, count: integer;
  matches: TStakeholderArray;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].department = dept then
    begin
      SetLength(matches, count + 1);
      matches[count] := stakeholders[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TTaskStakeholderManagerClass.GetStakeholdersByEngagement(
                                    level: TEngagementLevel): TStakeholderArray;
var
  i, count: integer;
  matches: TStakeholderArray;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].engagementLevel = level then
    begin
      SetLength(matches, count + 1);
      matches[count] := stakeholders[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TTaskStakeholderManagerClass.GetHighInfluenceStakeholders(
                                    minScore: double): TStakeholderArray;
var
  i, count: integer;
  matches: TStakeholderArray;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].influenceScore >= minScore then
    begin
      SetLength(matches, count + 1);
      matches[count] := stakeholders[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TTaskStakeholderManagerClass.GetHighInterestStakeholders(
                                    minScore: double): TStakeholderArray;
var
  i, count: integer;
  matches: TStakeholderArray;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].interestScore >= minScore then
    begin
      SetLength(matches, count + 1);
      matches[count] := stakeholders[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TTaskStakeholderManagerClass.CalculateMatrix(): string;
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
  
  result := Format('Manage: %d | Keep Informed: %d | Monitor: %d | Keep Satisfied: %d',
                   [manage, keepInformed, monitor, keepSatisfied]);
end;

function TTaskStakeholderManagerClass.GetStakeholderPriority(id: integer): string;
var
  idx: integer;
  influence, interest: double;
begin
  idx := FindStakeholderIndex(id);
  if idx < 0 then
  begin
    result := 'Unknown';
    Exit;
  end;
  
  influence := stakeholders[idx].influenceScore;
  interest := stakeholders[idx].interestScore;
  
  if (influence >= 7) and (interest >= 7) then
    result := 'Critical'
  else if (influence >= 7) or (interest >= 7) then
    result := 'High'
  else if (influence >= 4) or (interest >= 4) then
    result := 'Medium'
  else
    result := 'Low';
end;

function TTaskStakeholderManagerClass.SetEngagementLevel(id: integer; 
                                                       level: TEngagementLevel): boolean;
var
  idx: integer;
begin
  idx := FindStakeholderIndex(id);
  if idx >= 0 then
  begin
    stakeholders[idx].engagementLevel := level;
    result := true;
  end
  else
    result := false;
end;

function TTaskStakeholderManagerClass.UpdateInfluenceScore(id: integer; 
                                                         score: double): boolean;
var
  idx: integer;
begin
  idx := FindStakeholderIndex(id);
  if idx >= 0 then
  begin
    if score < 0 then score := 0;
    if score > 10 then score := 10;
    stakeholders[idx].influenceScore := score;
    result := true;
  end
  else
    result := false;
end;

function TTaskStakeholderManagerClass.UpdateInterestScore(id: integer; 
                                                        score: double): boolean;
var
  idx: integer;
begin
  idx := FindStakeholderIndex(id);
  if idx >= 0 then
  begin
    if score < 0 then score := 0;
    if score > 10 then score := 10;
    stakeholders[idx].interestScore := score;
    result := true;
  end
  else
    result := false;
end;

function TTaskStakeholderManagerClass.UpdateCommunicationPreference(id: integer; 
                                                                  pref: string): boolean;
var
  idx: integer;
begin
  idx := FindStakeholderIndex(id);
  if idx >= 0 then
  begin
    stakeholders[idx].communicationPreference := pref;
    result := true;
  end
  else
    result := false;
end;

function TTaskStakeholderManagerClass.LinkTaskToStakeholder(stakeholderId, 
                                                           taskId: integer): boolean;
var
  idx, i: integer;
  found: boolean;
begin
  idx := FindStakeholderIndex(stakeholderId);
  if idx < 0 then
  begin
    result := false;
    Exit;
  end;
  
  found := false;
  for i := 0 to Length(stakeholders[idx].relatedTaskIds) - 1 do
  begin
    if stakeholders[idx].relatedTaskIds[i] = taskId then
    begin
      found := true;
      Break;
    end;
  end;
  
  if not found then
  begin
    SetLength(stakeholders[idx].relatedTaskIds, Length(stakeholders[idx].relatedTaskIds) + 1);
    stakeholders[idx].relatedTaskIds[Length(stakeholders[idx].relatedTaskIds) - 1] := taskId;
    result := true;
  end
  else
    result := false;
end;

function TTaskStakeholderManagerClass.UnlinkTaskFromStakeholder(stakeholderId, 
                                                              taskId: integer): boolean;
var
  idx, i, j: integer;
begin
  idx := FindStakeholderIndex(stakeholderId);
  if idx < 0 then
  begin
    result := false;
    Exit;
  end;
  
  for i := 0 to Length(stakeholders[idx].relatedTaskIds) - 1 do
  begin
    if stakeholders[idx].relatedTaskIds[i] = taskId then
    begin
      for j := i to Length(stakeholders[idx].relatedTaskIds) - 2 do
        stakeholders[idx].relatedTaskIds[j] := stakeholders[idx].relatedTaskIds[j + 1];
      SetLength(stakeholders[idx].relatedTaskIds, Length(stakeholders[idx].relatedTaskIds) - 1);
      result := true;
      Exit;
    end;
  end;
  
  result := false;
end;

function TTaskStakeholderManagerClass.GetStakeholderTasks(
                                    stakeholderId: integer): TIntegerArray;
var
  idx: integer;
begin
  idx := FindStakeholderIndex(stakeholderId);
  if idx >= 0 then
    result := Copy(stakeholders[idx].relatedTaskIds, 0, Length(stakeholders[idx].relatedTaskIds))
  else
  begin
    SetLength(result, 0);
  end;
end;

function TTaskStakeholderManagerClass.LogInteraction(stakeholderId: integer; 
                                    interactionType, subject, summary, outcome: string): integer;
var
  interaction: TStakeholderInteraction;
begin
  interaction.id := nextInteractionId;
  interaction.stakeholderId := stakeholderId;
  interaction.interactionType := interactionType;
  interaction.subject := subject;
  interaction.summary := summary;
  interaction.date := Now();
  interaction.outcome := outcome;
  
  SetLength(interactions, Length(interactions) + 1);
  interactions[Length(interactions) - 1] := interaction;
  
  result := nextInteractionId;
  Inc(nextInteractionId);
end;

function TTaskStakeholderManagerClass.GetStakeholderInteractions(
                                    stakeholderId: integer): TInteractionArray;
var
  i, count: integer;
  matches: TInteractionArray;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(interactions) - 1 do
  begin
    if interactions[i].stakeholderId = stakeholderId then
    begin
      SetLength(matches, count + 1);
      matches[count] := interactions[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TTaskStakeholderManagerClass.GetRecentInteractions(days: integer): TInteractionArray;
var
  i, count: integer;
  matches: TInteractionArray;
  cutoffDate: TDateTime;
begin
  SetLength(matches, 0);
  cutoffDate := Now() - days;
  count := 0;
  
  for i := 0 to Length(interactions) - 1 do
  begin
    if interactions[i].date >= cutoffDate then
    begin
      SetLength(matches, count + 1);
      matches[count] := interactions[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TTaskStakeholderManagerClass.GetInteractionCount(stakeholderId: integer): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(interactions) - 1 do
  begin
    if interactions[i].stakeholderId = stakeholderId then
      Inc(count);
  end;
  result := count;
end;

function TTaskStakeholderManagerClass.GetCommunicationPlan(): string;
var
  i, manage, inform, satisfy: integer;
begin
  manage := 0;
  inform := 0;
  satisfy := 0;
  
  for i := 0 to Length(stakeholders) - 1 do
  begin
    case stakeholders[i].engagementLevel of
      elManage: Inc(manage);
      elKeepInformed: Inc(inform);
      elKeepSatisfied: Inc(satisfy);
    end;
  end;
  
  result := Format('Active Engagement: %d | Regular Updates: %d | Satisfaction Focus: %d',
                   [manage, inform, satisfy]);
end;

function TTaskStakeholderManagerClass.GetEngagementSummary(): string;
var
  totalStakeholders: integer;
  highInfluence, highInterest: integer;
  i: integer;
begin
  totalStakeholders := Length(stakeholders);
  highInfluence := 0;
  highInterest := 0;
  
  for i := 0 to Length(stakeholders) - 1 do
  begin
    if stakeholders[i].influenceScore >= 7 then
      Inc(highInfluence);
    if stakeholders[i].interestScore >= 7 then
      Inc(highInterest);
  end;
  
  result := Format('Total: %d | High Influence: %d | High Interest: %d',
                   [totalStakeholders, highInfluence, highInterest]);
end;

procedure TTaskStakeholderManagerClass.SelfTest();
var
  id1, id2, id3: integer;
  stakeholder: TStakeholder;
  allStakeholders: TStakeholderArray;
begin
  WriteLn('');
  WriteLn('=== Stakeholder Manager Self Test ===');
  
  { Add stakeholders }
  id1 := AddStakeholder('John Smith', 'john@example.com', srSponsor, 'Executive');
  id2 := AddStakeholder('Jane Doe', 'jane@example.com', srProjectManager, 'IT');
  id3 := AddStakeholder('Bob Wilson', 'bob@example.com', srClient, 'Sales');
  
  WriteLn(Format('Added 3 stakeholders with IDs: %d, %d, %d', [id1, id2, id3]));
  
  { Update scores }
  UpdateInfluenceScore(id1, 9.5);
  UpdateInterestScore(id1, 8.5);
  WriteLn('Updated influence and interest scores');
  
  { Set engagement }
  SetEngagementLevel(id1, elManage);
  SetEngagementLevel(id2, elKeepInformed);
  WriteLn('Set engagement levels');
  
  { Get stakeholders by role }
  allStakeholders := GetStakeholdersByRole(srSponsor);
  WriteLn(Format('Sponsors: %d', [Length(allStakeholders)]));
  
  { Log interactions }
  LogInteraction(id1, 'Meeting', 'Project Kickoff', 'Discussed project scope', 'Approved');
  LogInteraction(id2, 'Email', 'Status Update', 'Weekly status report', 'Received');
  WriteLn('Logged 2 interactions');
  
  { Link tasks }
  LinkTaskToStakeholder(id1, 101);
  LinkTaskToStakeholder(id2, 102);
  WriteLn('Linked stakeholders to tasks');
  
  { Get summary }
  WriteLn('Engagement Summary: ' + GetEngagementSummary());
  WriteLn('Priority Matrix: ' + CalculateMatrix());
  
  WriteLn('=== Stakeholder Manager Self Test Complete ===');
end;

end.
