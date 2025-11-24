
unit TaskStakeholderInteractions;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TStringArray = array of string;
  TIntegerArray = array of integer;

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

  { Stakeholder interaction manager }
  TStakeholderInteractionManager = class
  private
    interactions: TInteractionArray;
    nextInteractionId: integer;
    
    function FindInteractionIndex(id: integer): integer;
  public
    constructor Create();
    destructor Destroy(); override;
    
    { Interaction logging and retrieval }
    function LogInteraction(stakeholderId: integer; interactionType, subject, 
                           summary, outcome: string): integer;
    function GetStakeholderInteractions(stakeholderId: integer): TInteractionArray;
    function GetRecentInteractions(days: integer): TInteractionArray;
    function GetInteractionCount(stakeholderId: integer): integer;
    
    { Statistics }
    function GetInteractionStats(): string;
    
    { Self test }
    procedure SelfTest();
  end;

implementation

constructor TStakeholderInteractionManager.Create();
begin
  inherited Create();
  SetLength(interactions, 0);
  nextInteractionId := 1;
end;

destructor TStakeholderInteractionManager.Destroy();
begin
  SetLength(interactions, 0);
  inherited Destroy();
end;

function TStakeholderInteractionManager.FindInteractionIndex(id: integer): integer;
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

function TStakeholderInteractionManager.LogInteraction(stakeholderId: integer; 
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

function TStakeholderInteractionManager.GetStakeholderInteractions(
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

function TStakeholderInteractionManager.GetRecentInteractions(days: integer): TInteractionArray;
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

function TStakeholderInteractionManager.GetInteractionCount(stakeholderId: integer): integer;
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

function TStakeholderInteractionManager.GetInteractionStats(): string;
var
  totalCount: integer;
  emailCount, meetingCount: integer;
  i: integer;
begin
  totalCount := Length(interactions);
  emailCount := 0;
  meetingCount := 0;
  
  for i := 0 to Length(interactions) - 1 do
  begin
    if interactions[i].interactionType = 'Email' then
      Inc(emailCount)
    else if interactions[i].interactionType = 'Meeting' then
      Inc(meetingCount);
  end;
  
  result := Format('Total Interactions: %d | Emails: %d | Meetings: %d',
                   [totalCount, emailCount, meetingCount]);
end;

procedure TStakeholderInteractionManager.SelfTest();
var
  id1, iid1, iid2: integer;
  interactions_arr: TInteractionArray;
begin
  WriteLn('');
  WriteLn('=== Stakeholder Interaction Manager Self Test ===');
  
  id1 := 1; { Simulating a stakeholder ID }
  
  { Log interactions }
  iid1 := LogInteraction(id1, 'Email', 'Project Status', 'Weekly update sent', 'Received');
  iid2 := LogInteraction(id1, 'Meeting', 'Kickoff Meeting', 'Project kickoff discussion', 'Approved');
  WriteLn(Format('Logged 2 interactions with IDs: %d, %d', [iid1, iid2]));
  
  { Get interactions for stakeholder }
  interactions_arr := GetStakeholderInteractions(id1);
  WriteLn(Format('Retrieved %d interactions for stakeholder %d', [Length(interactions_arr), id1]));
  
  { Get recent interactions }
  interactions_arr := GetRecentInteractions(7);
  WriteLn(Format('Recent interactions (7 days): %d', [Length(interactions_arr)]));
  
  { Get interaction count }
  WriteLn(Format('Interaction count for stakeholder %d: %d', 
                 [id1, GetInteractionCount(id1)]));
  
  { Get stats }
  WriteLn('Stats: ' + GetInteractionStats());
  
  WriteLn('=== Stakeholder Interaction Manager Self Test Complete ===');
end;

end.
