
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

  { Task stakeholder manager - Core CRUD and basic operations }
  TTaskStakeholderManagerClass = class
  private
    stakeholders: TStakeholderArray;
    nextId: integer;
    
    function FindStakeholderIndex(id: integer): integer;
    function RoleToString(role: TStakeholderRole): string;
    function StringToRole(s: string): TStakeholderRole;
  public
    constructor Create();
    destructor Destroy(); override;
    
    { Core CRUD operations }
    function AddStakeholder(name, email: string; role: TStakeholderRole;
                           department: string): integer;
    function GetStakeholder(id: integer): TStakeholder;
    function UpdateStakeholder(id: integer; newStakeholder: TStakeholder): boolean;
    function DeleteStakeholder(id: integer): boolean;
    function GetAllStakeholders(): TStakeholderArray;
    function GetStakeholderCount(): integer;
    
    { Search operations }
    function GetStakeholdersByRole(role: TStakeholderRole): TStakeholderArray;
    function GetStakeholdersByDepartment(dept: string): TStakeholderArray;
    
    { Task linking }
    function LinkTaskToStakeholder(stakeholderId, taskId: integer): boolean;
    function UnlinkTaskFromStakeholder(stakeholderId, taskId: integer): boolean;
    function GetStakeholderTasks(stakeholderId: integer): TIntegerArray;
    
    { Engagement updates }
    function SetEngagementLevel(id: integer; level: TEngagementLevel): boolean;
    function UpdateInfluenceScore(id: integer; score: double): boolean;
    function UpdateInterestScore(id: integer; score: double): boolean;
    function UpdateCommunicationPreference(id: integer; pref: string): boolean;
    
    { Self test }
    procedure SelfTest();
  end;

implementation

constructor TTaskStakeholderManagerClass.Create();
begin
  inherited Create();
  SetLength(stakeholders, 0);
  nextId := 1;
end;

destructor TTaskStakeholderManagerClass.Destroy();
begin
  SetLength(stakeholders, 0);
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

function TTaskStakeholderManagerClass.AddStakeholder(name, email: string; 
                                                     role: TStakeholderRole;
                                                     department: string): integer;
var
  stakeholder: TStakeholder;
begin
  stakeholder.id := nextId;
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
  
  result := nextId;
  Inc(nextId);
end;

function TTaskStakeholderManagerClass.GetStakeholder(id: integer): TStakeholder;
var
  idx: integer;
begin
  idx := FindStakeholderIndex(id);
  if idx >= 0 then
    result := stakeholders[idx];
end;

function TTaskStakeholderManagerClass.UpdateStakeholder(id: integer; 
                                                       newStakeholder: TStakeholder): boolean;
var
  idx: integer;
begin
  idx := FindStakeholderIndex(id);
  if idx >= 0 then
  begin
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
    SetLength(result, 0);
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

procedure TTaskStakeholderManagerClass.SelfTest();
var
  id1, id2, id3: integer;
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
  
  { Link tasks }
  LinkTaskToStakeholder(id1, 101);
  LinkTaskToStakeholder(id2, 102);
  WriteLn('Linked stakeholders to tasks');
  
  { Get stakeholder count }
  WriteLn(Format('Total stakeholders: %d', [GetStakeholderCount()]));
  
  WriteLn('=== Stakeholder Manager Self Test Complete ===');
end;

end.
