
unit TaskStakeholderScoring;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes,
  TaskStakeholder;

type
  TStringArray = array of string;
  TIntegerArray = array of integer;

  { Stakeholder engagement levels }
  TEngagementLevel = (elManage, elKeepInformed, elMonitor, elKeepSatisfied, elUnknown);

  { Stakeholder scoring and engagement manager }
  TStakeholderScoringManager = class
  public
    { Score validation and normalization }
    function NormalizeScore(score: double): double;
    
    { Engagement level conversion }
    function EngagementToString(level: TEngagementLevel): string;
    function StringToEngagement(s: string): TEngagementLevel;
    
    { Priority calculation based on scores }
    function CalculatePriority(influenceScore, interestScore: double): string;
    
    { Score validators }
    function IsValidScore(score: double): boolean;
    function IsHighInfluence(score: double): boolean;
    function IsHighInterest(score: double): boolean;
    
    { Engagement matrix calculation }
    function CalculateEngagementMatrix(manageCount, keepInformedCount, 
                                       monitorCount, keepSatisfiedCount: integer): string;
    
    { Communication strategy based on scores }
    function GetCommunicationStrategy(priority: string): string;
    
    { Self test }
    procedure SelfTest();
  end;

implementation

function TStakeholderScoringManager.NormalizeScore(score: double): double;
begin
  if score < 0 then
    result := 0
  else if score > 10 then
    result := 10
  else
    result := score;
end;

function TStakeholderScoringManager.EngagementToString(level: TEngagementLevel): string;
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

function TStakeholderScoringManager.StringToEngagement(s: string): TEngagementLevel;
begin
  if s = 'Manage' then
    result := elManage
  else if s = 'Keep Informed' then
    result := elKeepInformed
  else if s = 'Monitor' then
    result := elMonitor
  else if s = 'Keep Satisfied' then
    result := elKeepSatisfied
  else
    result := elMonitor;
end;

function TStakeholderScoringManager.CalculatePriority(influenceScore, 
                                                      interestScore: double): string;
begin
  influenceScore := NormalizeScore(influenceScore);
  interestScore := NormalizeScore(interestScore);
  
  if (influenceScore >= 7) and (interestScore >= 7) then
    result := 'Critical'
  else if (influenceScore >= 7) or (interestScore >= 7) then
    result := 'High'
  else if (influenceScore >= 4) or (interestScore >= 4) then
    result := 'Medium'
  else
    result := 'Low';
end;

function TStakeholderScoringManager.IsValidScore(score: double): boolean;
begin
  result := (score >= 0) and (score <= 10);
end;

function TStakeholderScoringManager.IsHighInfluence(score: double): boolean;
begin
  result := NormalizeScore(score) >= 7;
end;

function TStakeholderScoringManager.IsHighInterest(score: double): boolean;
begin
  result := NormalizeScore(score) >= 7;
end;

function TStakeholderScoringManager.CalculateEngagementMatrix(manageCount, keepInformedCount,
                                                              monitorCount, keepSatisfiedCount: integer): string;
begin
  result := Format('Manage: %d | Keep Informed: %d | Monitor: %d | Keep Satisfied: %d',
                   [manageCount, keepInformedCount, monitorCount, keepSatisfiedCount]);
end;

function TStakeholderScoringManager.GetCommunicationStrategy(priority: string): string;
begin
  if priority = 'Critical' then
    result := 'Daily meetings, direct communication, escalation path'
  else if priority = 'High' then
    result := 'Weekly meetings, regular updates, assigned point of contact'
  else if priority = 'Medium' then
    result := 'Bi-weekly updates, email communication, status reports'
  else
    result := 'Monthly reviews, email notifications, self-service access';
end;

procedure TStakeholderScoringManager.SelfTest();
var
  priority: string;
  strategy: string;
begin
  WriteLn('');
  WriteLn('=== Stakeholder Scoring Manager Self Test ===');
  
  { Test score normalization }
  WriteLn(Format('Normalize 11.5: %.1f', [NormalizeScore(11.5)]));
  WriteLn(Format('Normalize -5: %.1f', [NormalizeScore(-5)]));
  WriteLn(Format('Normalize 7.5: %.1f', [NormalizeScore(7.5)]));
  
  { Test priority calculation }
  priority := CalculatePriority(8, 8);
  WriteLn('Priority (8,8): ' + priority);
  strategy := GetCommunicationStrategy(priority);
  WriteLn('Strategy: ' + strategy);
  
  priority := CalculatePriority(2, 2);
  WriteLn('Priority (2,2): ' + priority);
  
  { Test engagement matrix }
  WriteLn('Matrix: ' + CalculateEngagementMatrix(5, 10, 8, 3));
  
  WriteLn('=== Stakeholder Scoring Manager Self Test Complete ===');
end;

end.
