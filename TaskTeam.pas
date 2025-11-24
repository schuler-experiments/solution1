
unit TaskTeam;

{$mode objfpc}

interface

uses
  SysUtils,
  Math,
  TaskTypes;

type
  { Team member record }
  TTeamMember = record
    ID: integer;
    Name: string;
    Email: string;
    Role: string;
    MaxTasksPerWeek: integer;
    SkillTags: array of string;
    Available: boolean;
    HourlyRate: double;
  end;

  { Array type for team members }
  TTeamMemberArray = array of TTeamMember;

  { Task assignment record }
  TTaskAssignment = record
    AssignmentID: integer;
    TaskID: integer;
    MemberID: integer;
    AssignedDate: TDateTime;
    DueDate: TDateTime;
    Status: TTaskStatus;
    HoursAllocated: double;
    EstimatedCost: double;
  end;

  { Array type for assignments }
  TTaskAssignmentArray = array of TTaskAssignment;

  { Team manager class }
  TTaskTeamManagerClass = class
  private
    fTeamMembers: TTeamMemberArray;
    fAssignments: TTaskAssignmentArray;
    fNextMemberID: integer;
    fNextAssignmentID: integer;
    function FindMemberIndex(id: integer): integer;
    function FindAssignmentIndex(id: integer): integer;
  public
    constructor Create();
    destructor Destroy();
    function AddTeamMember(name, email, role: string; maxTasks: integer): integer;
    function GetTeamMember(id: integer): TTeamMember;
    function GetAllTeamMembers(): TTeamMemberArray;
    function GetMemberCount(): integer;
    function UpdateMemberAvailability(memberId: integer; available: boolean): boolean;
    function AssignTaskToMember(taskId, memberId: integer; hoursAllocated: double; hourlyRate: double): integer;
    function GetMemberAssignments(memberId: integer): TTaskAssignmentArray;
    function GetTaskAssignment(taskId: integer): TTaskAssignment;
    function GetMemberWorkload(memberId: integer): double;
    function IsMemberAvailable(memberId: integer): boolean;
    function GetTeamCapacity(): double;
    function GetTeamUtilization(): double;
    function GetMemberSkillMatch(memberId: integer; requiredSkills: array of string): integer;
    function FindBestMemberForTask(requiredSkills: array of string): integer;
    procedure RemoveAssignment(assignmentId: integer);
    procedure SelfTest();
  end;

implementation

constructor TTaskTeamManagerClass.Create();
begin
  SetLength(fTeamMembers, 0);
  SetLength(fAssignments, 0);
  fNextMemberID := 1;
  fNextAssignmentID := 1;
end;

destructor TTaskTeamManagerClass.Destroy();
begin
  SetLength(fTeamMembers, 0);
  SetLength(fAssignments, 0);
  inherited Destroy();
end;

function TTaskTeamManagerClass.FindMemberIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(fTeamMembers) - 1 do
  begin
    if fTeamMembers[i].ID = id then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskTeamManagerClass.FindAssignmentIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(fAssignments) - 1 do
  begin
    if fAssignments[i].AssignmentID = id then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskTeamManagerClass.AddTeamMember(name, email, role: string; maxTasks: integer): integer;
var
  newMember: TTeamMember;
begin
  newMember.ID := fNextMemberID;
  newMember.Name := name;
  newMember.Email := email;
  newMember.Role := role;
  newMember.MaxTasksPerWeek := maxTasks;
  SetLength(newMember.SkillTags, 0);
  newMember.Available := true;
  newMember.HourlyRate := 50.0;
  
  SetLength(fTeamMembers, Length(fTeamMembers) + 1);
  fTeamMembers[Length(fTeamMembers) - 1] := newMember;
  
  result := fNextMemberID;
  fNextMemberID := fNextMemberID + 1;
end;

function TTaskTeamManagerClass.GetTeamMember(id: integer): TTeamMember;
var
  idx: integer;
begin
  idx := FindMemberIndex(id);
  if idx >= 0 then
    result := fTeamMembers[idx]
  else
  begin
    result.ID := -1;
    result.Name := '';
  end;
end;

function TTaskTeamManagerClass.GetAllTeamMembers(): TTeamMemberArray;
begin
  result := fTeamMembers;
end;

function TTaskTeamManagerClass.GetMemberCount(): integer;
begin
  result := Length(fTeamMembers);
end;

function TTaskTeamManagerClass.UpdateMemberAvailability(memberId: integer; available: boolean): boolean;
var
  idx: integer;
begin
  idx := FindMemberIndex(memberId);
  if idx >= 0 then
  begin
    fTeamMembers[idx].Available := available;
    result := true;
  end
  else
    result := false;
end;

function TTaskTeamManagerClass.AssignTaskToMember(taskId, memberId: integer; hoursAllocated: double; hourlyRate: double): integer;
var
  newAssignment: TTaskAssignment;
begin
  if FindMemberIndex(memberId) < 0 then
  begin
    result := -1;
    exit;
  end;

  newAssignment.AssignmentID := fNextAssignmentID;
  newAssignment.TaskID := taskId;
  newAssignment.MemberID := memberId;
  newAssignment.AssignedDate := Now();
  newAssignment.DueDate := Now() + 7;
  newAssignment.Status := tsNotStarted;
  newAssignment.HoursAllocated := hoursAllocated;
  newAssignment.EstimatedCost := hoursAllocated * hourlyRate;
  
  SetLength(fAssignments, Length(fAssignments) + 1);
  fAssignments[Length(fAssignments) - 1] := newAssignment;
  
  result := fNextAssignmentID;
  fNextAssignmentID := fNextAssignmentID + 1;
end;

function TTaskTeamManagerClass.GetMemberAssignments(memberId: integer): TTaskAssignmentArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  for i := 0 to Length(fAssignments) - 1 do
  begin
    if fAssignments[i].MemberID = memberId then
    begin
      SetLength(result, count + 1);
      result[count] := fAssignments[i];
      count := count + 1;
    end;
  end;
end;

function TTaskTeamManagerClass.GetTaskAssignment(taskId: integer): TTaskAssignment;
var
  i: integer;
begin
  for i := 0 to Length(fAssignments) - 1 do
  begin
    if fAssignments[i].TaskID = taskId then
    begin
      result := fAssignments[i];
      exit;
    end;
  end;
  result.AssignmentID := -1;
end;

function TTaskTeamManagerClass.GetMemberWorkload(memberId: integer): double;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to Length(fAssignments) - 1 do
  begin
    if fAssignments[i].MemberID = memberId then
      total := total + fAssignments[i].HoursAllocated;
  end;
  result := total;
end;

function TTaskTeamManagerClass.IsMemberAvailable(memberId: integer): boolean;
var
  idx: integer;
begin
  idx := FindMemberIndex(memberId);
  if idx >= 0 then
    result := fTeamMembers[idx].Available
  else
    result := false;
end;

function TTaskTeamManagerClass.GetTeamCapacity(): double;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to Length(fTeamMembers) - 1 do
  begin
    if fTeamMembers[i].Available then
      total := total + (fTeamMembers[i].MaxTasksPerWeek * 8);
  end;
  result := total;
end;

function TTaskTeamManagerClass.GetTeamUtilization(): double;
var
  capacity, utilization: double;
begin
  capacity := GetTeamCapacity();
  if capacity = 0 then
  begin
    result := 0;
    exit;
  end;
  utilization := 0;
  if Length(fAssignments) > 0 then
    utilization := (Length(fAssignments) / capacity) * 100;
  result := utilization;
end;

function TTaskTeamManagerClass.GetMemberSkillMatch(memberId: integer; requiredSkills: array of string): integer;
var
  i, j, matches: integer;
  member: TTeamMember;
begin
  member := GetTeamMember(memberId);
  if member.ID < 0 then
  begin
    result := 0;
    exit;
  end;
  
  matches := 0;
  for i := 0 to Length(requiredSkills) - 1 do
  begin
    for j := 0 to Length(member.SkillTags) - 1 do
    begin
      if member.SkillTags[j] = requiredSkills[i] then
      begin
        matches := matches + 1;
        break;
      end;
    end;
  end;
  result := matches;
end;

function TTaskTeamManagerClass.FindBestMemberForTask(requiredSkills: array of string): integer;
var
  i, bestMatch, skillMatch: integer;
begin
  bestMatch := -1;
  skillMatch := -1;
  
  for i := 0 to Length(fTeamMembers) - 1 do
  begin
    if fTeamMembers[i].Available then
    begin
      skillMatch := GetMemberSkillMatch(fTeamMembers[i].ID, requiredSkills);
      if skillMatch > 0 then
      begin
        result := fTeamMembers[i].ID;
        exit;
      end;
    end;
  end;
  
  result := -1;
end;

procedure TTaskTeamManagerClass.RemoveAssignment(assignmentId: integer);
var
  idx, i: integer;
begin
  idx := FindAssignmentIndex(assignmentId);
  if idx >= 0 then
  begin
    for i := idx to Length(fAssignments) - 2 do
      fAssignments[i] := fAssignments[i + 1];
    SetLength(fAssignments, Length(fAssignments) - 1);
  end;
end;

procedure TTaskTeamManagerClass.SelfTest();
var
  team_mgr: TTaskTeamManagerClass;
  memberId1, memberId2, assignmentId: integer;
  members: TTeamMemberArray;
  assignments: TTaskAssignmentArray;
begin
  WriteLn('');
  WriteLn('=== TaskTeam Manager Self Test ===');
  
  team_mgr := TTaskTeamManagerClass.Create();
  
  try
    { Add team members }
    memberId1 := team_mgr.AddTeamMember('Alice Johnson', 'alice@example.com', 'Developer', 5);
    memberId2 := team_mgr.AddTeamMember('Bob Smith', 'bob@example.com', 'Designer', 4);
    
    WriteLn('Added ' + IntToStr(team_mgr.GetMemberCount()) + ' team members');
    
    { Assign tasks }
    assignmentId := team_mgr.AssignTaskToMember(1, memberId1, 8.0, 50.0);
    WriteLn('Assigned task 1 to member 1, cost: $' + FormatFloat('0.00', 8.0 * 50.0));
    
    { Check member workload }
    WriteLn('Member 1 workload: ' + FormatFloat('0.0', team_mgr.GetMemberWorkload(memberId1)) + ' hours');
    
    { Check team capacity }
    WriteLn('Team capacity: ' + FormatFloat('0.0', team_mgr.GetTeamCapacity()) + ' hours/week');
    WriteLn('Team utilization: ' + FormatFloat('0.0', team_mgr.GetTeamUtilization()) + '%');
    
    { Check member availability }
    if team_mgr.IsMemberAvailable(memberId1) then
      WriteLn('✓ Member 1 is available');
    
    team_mgr.UpdateMemberAvailability(memberId1, false);
    if not team_mgr.IsMemberAvailable(memberId1) then
      WriteLn('✓ Member 1 availability updated');
    
    WriteLn('=== TaskTeam Manager Self Test Complete ===');
  finally
    team_mgr.Free();
  end;
end;

end.
