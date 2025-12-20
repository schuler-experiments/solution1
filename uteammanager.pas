
unit uteammanager;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, utaskmanager, Math;

type
  TTeamMember = record
    Name: string;
    Skills: ttagarray;
    MaxDailyEffort: double;
    XP: integer;
    Level: integer;
  end;

  TTeamMemberArray = array of TTeamMember;

  TAssignmentSuggestion = record
    MemberName: string;
    Score: double;
    Reason: string;
  end;

  TAssignmentSuggestionArray = array of TAssignmentSuggestion;

  TTeamManager = class
  private
    FMembers: TTeamMemberArray;
    function FindMemberIndex(const AName: string): integer;
    function CalculateSkillMatch(const AMember: TTeamMember; const ATask: ttask): double;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure AddMember(const AName: string; const ASkills: ttagarray; AMaxEffort: double);
    function GetMemberWorkload(const AName: string; ATaskManager: ttaskmanager): double;
    function SuggestBestMember(const ATask: ttask; ATaskManager: ttaskmanager): TAssignmentSuggestionArray;
    
    procedure AwardXP(const AName: string; Amount: integer);
    function GetLeaderboard: TTeamMemberArray;
    
    procedure self_test(ATaskManager: ttaskmanager);
  end;

implementation

constructor TTeamManager.Create;
begin
  inherited Create;
  SetLength(FMembers, 0);
end;

destructor TTeamManager.Destroy;
begin
  SetLength(FMembers, 0);
  inherited Destroy;
end;

function TTeamManager.FindMemberIndex(const AName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to High(FMembers) do
    if SameText(FMembers[i].Name, AName) then exit(i);
end;

procedure TTeamManager.AddMember(const AName: string; const ASkills: ttagarray; AMaxEffort: double);
var
  idx: integer;
begin
  idx := FindMemberIndex(AName);
  if idx = -1 then
  begin
    SetLength(FMembers, Length(FMembers) + 1);
    idx := High(FMembers);
  end;
  FMembers[idx].Name := AName;
  FMembers[idx].Skills := ASkills;
  FMembers[idx].MaxDailyEffort := AMaxEffort;
  FMembers[idx].XP := 0;
  FMembers[idx].Level := 1;
end;

function TTeamManager.GetMemberWorkload(const AName: string; ATaskManager: ttaskmanager): double;
var
  Tasks: ttaskarray;
  i: integer;
begin
  Result := 0;
  Tasks := ATaskManager.get_tasks;
  for i := 0 to High(Tasks) do
  begin
    if (SameText(Tasks[i].assigned_to, AName)) and (Tasks[i].status <> ts_done) and (Tasks[i].status <> ts_archived) then
      Result := Result + Tasks[i].estimated_hours;
  end;
end;

function TTeamManager.CalculateSkillMatch(const AMember: TTeamMember; const ATask: ttask): double;
var
  i, j: integer;
  MatchCount: integer;
begin
  MatchCount := 0;
  if Length(ATask.tags) = 0 then exit(0.5); 
  
  for i := 0 to High(ATask.tags) do
    for j := 0 to High(AMember.Skills) do
      if SameText(ATask.tags[i], AMember.Skills[j]) then
      begin
        inc(MatchCount);
        break;
      end;
      
  Result := MatchCount / Length(ATask.tags);
end;

function TTeamManager.SuggestBestMember(const ATask: ttask; ATaskManager: ttaskmanager): TAssignmentSuggestionArray;
var
  i, j: integer;
  Workload, SkillScore, FinalScore: double;
  Temp: TAssignmentSuggestion;
begin
  SetLength(Result, Length(FMembers));
  for i := 0 to High(FMembers) do
  begin
    Workload := GetMemberWorkload(FMembers[i].Name, ATaskManager);
    SkillScore := CalculateSkillMatch(FMembers[i], ATask);
    
    // Score: 60% Skills, 40% Availability
    FinalScore := (SkillScore * 0.6) + (Max(0, 1 - (Workload / (FMembers[i].MaxDailyEffort * 5))) * 0.4);
    
    Result[i].MemberName := FMembers[i].Name;
    Result[i].Score := FinalScore;
    Result[i].Reason := Format('Skills: %.0f%%, Workload: %.1f hrs', [SkillScore * 100, Workload]);
  end;
  
  for i := 0 to High(Result) - 1 do
    for j := i + 1 to High(Result) do
      if Result[i].Score < Result[j].Score then
      begin
        Temp := Result[i];
        Result[i] := Result[j];
        Result[j] := Temp;
      end;
end;

procedure TTeamManager.AwardXP(const AName: string; Amount: integer);
var
  idx: integer;
begin
  idx := FindMemberIndex(AName);
  if idx <> -1 then
  begin
    FMembers[idx].XP := FMembers[idx].XP + Amount;
    FMembers[idx].Level := Floor(Sqrt(FMembers[idx].XP / 100)) + 1;
  end;
end;

function TTeamManager.GetLeaderboard: TTeamMemberArray;
var
  i, j: integer;
  Temp: TTeamMember;
begin
  Result := FMembers;
  for i := 0 to High(Result) - 1 do
    for j := i + 1 to High(Result) do
      if Result[i].XP < Result[j].XP then
      begin
        Temp := Result[i];
        Result[i] := Result[j];
        Result[j] := Temp;
      end;
end;

procedure TTeamManager.self_test(ATaskManager: ttaskmanager);
var
  Skills: ttagarray;
  Suggestions: TAssignmentSuggestionArray;
  TestTask: ttask;
  i: integer;
begin
  WriteLn('--- Team Manager Self Test ---');
  SetLength(Skills, 2);
  Skills[0] := 'Pascal'; Skills[1] := 'SQL';
  AddMember('Alice', Skills, 8.0);
  
  SetLength(Skills, 2);
  Skills[0] := 'Python'; Skills[1] := 'Cloud';
  AddMember('Bob', Skills, 7.0);
  
  TestTask.title := 'Database Optimization';
  SetLength(TestTask.tags, 1);
  TestTask.tags[0] := 'SQL';
  TestTask.estimated_hours := 4.0;
  
  Suggestions := SuggestBestMember(TestTask, ATaskManager);
  WriteLn('Suggestions for: ', TestTask.title);
  for i := 0 to High(Suggestions) do
    WriteLn(Format(' - %s: %.2f (%s)', [Suggestions[i].MemberName, Suggestions[i].Score, Suggestions[i].Reason]));
    
  AwardXP('Alice', 250);
  WriteLn('Alice Level: ', FMembers[FindMemberIndex('Alice')].Level);
end;

end.
