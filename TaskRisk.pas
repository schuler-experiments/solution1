
unit TaskRisk;

{$mode objfpc}

interface

uses
  SysUtils,
  Math,
  TaskTypes;

type
  { Array type for integers }
  TIntegerArray = array of integer;

  { Risk level enumeration }
  TRiskLevel = (rlLow, rlMedium, rlHigh, rlCritical);

  { Risk record }
  TTaskRisk = record
    ID: integer;
    TaskID: integer;
    Description: string;
    RiskLevel: TRiskLevel;
    Probability: double;
    Impact: double;
    MitigationStrategy: string;
    Owner: string;
    IdentificationDate: TDateTime;
    Status: string;
  end;

  { Array type for risks }
  TTaskRiskArray = array of TTaskRisk;

  { Risk metrics record }
  TRiskMetrics = record
    TotalRisks: integer;
    CriticalRisks: integer;
    HighRisks: integer;
    MediumRisks: integer;
    LowRisks: integer;
    AverageRiskScore: double;
    OverallProjectRisk: string;
  end;

  { Risk manager class }
  TTaskRiskManagerClass = class
  private
    fRisks: TTaskRiskArray;
    fNextRiskID: integer;
    function FindRiskIndex(id: integer): integer;
    function CalculateRiskScore(probability, impact: double): double;
    function RiskScoreToLevel(score: double): TRiskLevel;
  public
    constructor Create();
    destructor Destroy();
    function AddRisk(taskId: integer; description: string; probability, impact: double; mitigationStrategy: string): integer;
    function GetRisk(id: integer): TTaskRisk;
    function GetTaskRisks(taskId: integer): TTaskRiskArray;
    function GetAllRisks(): TTaskRiskArray;
    function GetRiskCount(): integer;
    function GetCriticalRisks(): TTaskRiskArray;
    function UpdateRiskStatus(riskId: integer; newStatus: string): boolean;
    function ExecuteMitigation(riskId: integer): boolean;
    function GetRiskMetrics(): TRiskMetrics;
    function CalculateProjectRiskScore(): double;
    function GetHighestRiskTasks(): TIntegerArray;
    function IdentifyRiskTrends(): string;
    procedure RemoveRisk(id: integer);
    procedure SelfTest();
  end;

implementation

constructor TTaskRiskManagerClass.Create();
begin
  SetLength(fRisks, 0);
  fNextRiskID := 1;
end;

destructor TTaskRiskManagerClass.Destroy();
begin
  SetLength(fRisks, 0);
  inherited Destroy();
end;

function TTaskRiskManagerClass.FindRiskIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(fRisks) - 1 do
  begin
    if fRisks[i].ID = id then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskRiskManagerClass.CalculateRiskScore(probability, impact: double): double;
begin
  result := probability * impact;
end;

function TTaskRiskManagerClass.RiskScoreToLevel(score: double): TRiskLevel;
begin
  if score >= 0.7 then
    result := rlCritical
  else if score >= 0.5 then
    result := rlHigh
  else if score >= 0.3 then
    result := rlMedium
  else
    result := rlLow;
end;

function TTaskRiskManagerClass.AddRisk(taskId: integer; description: string; probability, impact: double; mitigationStrategy: string): integer;
var
  newRisk: TTaskRisk;
begin
  newRisk.ID := fNextRiskID;
  newRisk.TaskID := taskId;
  newRisk.Description := description;
  newRisk.Probability := probability;
  newRisk.Impact := impact;
  newRisk.RiskLevel := RiskScoreToLevel(CalculateRiskScore(probability, impact));
  newRisk.MitigationStrategy := mitigationStrategy;
  newRisk.Owner := '';
  newRisk.IdentificationDate := Now();
  newRisk.Status := 'Identified';
  
  SetLength(fRisks, Length(fRisks) + 1);
  fRisks[Length(fRisks) - 1] := newRisk;
  
  result := fNextRiskID;
  fNextRiskID := fNextRiskID + 1;
end;

function TTaskRiskManagerClass.GetRisk(id: integer): TTaskRisk;
var
  idx: integer;
begin
  idx := FindRiskIndex(id);
  if idx >= 0 then
    result := fRisks[idx]
  else
  begin
    result.ID := -1;
    result.Description := '';
  end;
end;

function TTaskRiskManagerClass.GetTaskRisks(taskId: integer): TTaskRiskArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  for i := 0 to Length(fRisks) - 1 do
  begin
    if fRisks[i].TaskID = taskId then
    begin
      SetLength(result, count + 1);
      result[count] := fRisks[i];
      count := count + 1;
    end;
  end;
end;

function TTaskRiskManagerClass.GetAllRisks(): TTaskRiskArray;
begin
  result := fRisks;
end;

function TTaskRiskManagerClass.GetRiskCount(): integer;
begin
  result := Length(fRisks);
end;

function TTaskRiskManagerClass.GetCriticalRisks(): TTaskRiskArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  for i := 0 to Length(fRisks) - 1 do
  begin
    if fRisks[i].RiskLevel = rlCritical then
    begin
      SetLength(result, count + 1);
      result[count] := fRisks[i];
      count := count + 1;
    end;
  end;
end;

function TTaskRiskManagerClass.UpdateRiskStatus(riskId: integer; newStatus: string): boolean;
var
  idx: integer;
begin
  idx := FindRiskIndex(riskId);
  if idx >= 0 then
  begin
    fRisks[idx].Status := newStatus;
    result := true;
  end
  else
    result := false;
end;

function TTaskRiskManagerClass.ExecuteMitigation(riskId: integer): boolean;
var
  idx: integer;
begin
  idx := FindRiskIndex(riskId);
  if idx >= 0 then
  begin
    fRisks[idx].Status := 'Mitigated';
    result := true;
  end
  else
    result := false;
end;

function TTaskRiskManagerClass.GetRiskMetrics(): TRiskMetrics;
var
  i: integer;
  metrics: TRiskMetrics;
begin
  metrics.TotalRisks := Length(fRisks);
  metrics.CriticalRisks := 0;
  metrics.HighRisks := 0;
  metrics.MediumRisks := 0;
  metrics.LowRisks := 0;
  metrics.AverageRiskScore := 0;
  
  for i := 0 to Length(fRisks) - 1 do
  begin
    metrics.AverageRiskScore := metrics.AverageRiskScore + (fRisks[i].Probability * fRisks[i].Impact);
    
    case fRisks[i].RiskLevel of
      rlCritical: metrics.CriticalRisks := metrics.CriticalRisks + 1;
      rlHigh: metrics.HighRisks := metrics.HighRisks + 1;
      rlMedium: metrics.MediumRisks := metrics.MediumRisks + 1;
      rlLow: metrics.LowRisks := metrics.LowRisks + 1;
    end;
  end;
  
  if metrics.TotalRisks > 0 then
    metrics.AverageRiskScore := metrics.AverageRiskScore / metrics.TotalRisks;
  
  if metrics.AverageRiskScore >= 0.6 then
    metrics.OverallProjectRisk := 'High'
  else if metrics.AverageRiskScore >= 0.4 then
    metrics.OverallProjectRisk := 'Medium'
  else
    metrics.OverallProjectRisk := 'Low';
  
  result := metrics;
end;

function TTaskRiskManagerClass.CalculateProjectRiskScore(): double;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to Length(fRisks) - 1 do
    total := total + (fRisks[i].Probability * fRisks[i].Impact);
  
  if Length(fRisks) > 0 then
    result := total / Length(fRisks)
  else
    result := 0;
end;

function TTaskRiskManagerClass.GetHighestRiskTasks(): TIntegerArray;
var
  i, j, count: integer;
  highestTasks: TIntegerArray;
begin
  SetLength(highestTasks, 0);
  count := 0;
  
  { Collect unique task IDs with risks }
  for i := 0 to Length(fRisks) - 1 do
  begin
    j := 0;
    while j < count do
    begin
      if highestTasks[j] = fRisks[i].TaskID then
        break;
      j := j + 1;
    end;
    
    if j = count then
    begin
      SetLength(highestTasks, count + 1);
      highestTasks[count] := fRisks[i].TaskID;
      count := count + 1;
    end;
  end;
  
  result := highestTasks;
end;

function TTaskRiskManagerClass.IdentifyRiskTrends(): string;
var
  metrics: TRiskMetrics;
  trend: string;
begin
  metrics := GetRiskMetrics();
  
  trend := '';
  if metrics.CriticalRisks > 0 then
    trend := 'Critical risks identified - immediate action required'
  else if metrics.HighRisks > 2 then
    trend := 'Multiple high-risk items - escalation recommended'
  else if metrics.AverageRiskScore > 0.5 then
    trend := 'Overall project risk is elevated'
  else if metrics.TotalRisks = 0 then
    trend := 'No risks identified - project proceeding normally'
  else
    trend := 'Project risk is under control';
  
  result := trend;
end;

procedure TTaskRiskManagerClass.RemoveRisk(id: integer);
var
  idx, i: integer;
begin
  idx := FindRiskIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(fRisks) - 2 do
      fRisks[i] := fRisks[i + 1];
    SetLength(fRisks, Length(fRisks) - 1);
  end;
end;

procedure TTaskRiskManagerClass.SelfTest();
var
  risk_mgr: TTaskRiskManagerClass;
  riskId1, riskId2, riskId3: integer;
  metrics: TRiskMetrics;
begin
  WriteLn('');
  WriteLn('=== TaskRisk Manager Self Test ===');
  
  risk_mgr := TTaskRiskManagerClass.Create();
  
  try
    { Add risks }
    riskId1 := risk_mgr.AddRisk(1, 'Resource shortage', 0.7, 0.8, 'Hire temporary staff');
    riskId2 := risk_mgr.AddRisk(2, 'Technical complexity', 0.6, 0.6, 'Spike analysis');
    riskId3 := risk_mgr.AddRisk(3, 'Vendor delay', 0.5, 0.4, 'Identify backup vendors');
    WriteLn('Added 3 risks');
    
    { Get risk metrics }
    metrics := risk_mgr.GetRiskMetrics();
    WriteLn('Total Risks: ' + IntToStr(metrics.TotalRisks));
    WriteLn('Critical Risks: ' + IntToStr(metrics.CriticalRisks));
    WriteLn('High Risks: ' + IntToStr(metrics.HighRisks));
    WriteLn('Average Risk Score: ' + FormatFloat('0.00', metrics.AverageRiskScore));
    WriteLn('Overall Project Risk: ' + metrics.OverallProjectRisk);
    
    { Check risk trends }
    WriteLn('Risk Trend: ' + risk_mgr.IdentifyRiskTrends());
    
    { Execute mitigation }
    if risk_mgr.ExecuteMitigation(riskId1) then
      WriteLn('✓ Executed mitigation for risk 1');
    
    WriteLn('=== TaskRisk Manager Self Test Complete ===');
  finally
    risk_mgr.Free();
  end;
end;

end.
