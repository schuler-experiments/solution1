
unit TaskSLAAnalysis;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, TaskSLA;

type
  { SLA Analysis class }
  TTaskSLAAnalysisClass = class
  private
    slaMgr: TTaskSLAManagerClass;
  public
    constructor Create(mgr: TTaskSLAManagerClass);
    destructor Destroy(); override;
    
    { Metrics and analysis }
    function GetCompliancePercentage(): double;
    function GetAverageResponseTime(): double;
    function GetAverageResolutionTime(): double;
    function GetSLAsNearBreach(warningHoursThreshold: integer): TSLAArray;
    function GetSLAMetrics(): string;
    
    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskSLAAnalysisClass.Create(mgr: TTaskSLAManagerClass);
begin
  inherited Create();
  slaMgr := mgr;
end;

destructor TTaskSLAAnalysisClass.Destroy();
begin
  inherited Destroy();
end;

function TTaskSLAAnalysisClass.GetCompliancePercentage(): double;
var
  slas: TSLAArray;
  i: integer;
  metCount: integer;
begin
  metCount := 0;
  slas := slaMgr.GetActiveSLAs();
  
  if Length(slas) = 0 then
  begin
    Result := 0.0;
    SetLength(slas, 0);
    Exit;
  end;
  
  for i := 0 to Length(slas) - 1 do
  begin
    if slas[i].status = slaMet then
      Inc(metCount);
  end;
  
  Result := (metCount / Length(slas)) * 100.0;
  SetLength(slas, 0);
end;

function TTaskSLAAnalysisClass.GetAverageResponseTime(): double;
var
  slas: TSLAArray;
  i: integer;
  totalHours: double;
  count: integer;
begin
  totalHours := 0;
  count := 0;
  slas := slaMgr.GetActiveSLAs();
  
  for i := 0 to Length(slas) - 1 do
  begin
    if slas[i].actualResponseDate <> 0 then
    begin
      totalHours := totalHours + slas[i].responseTimeHours;
      Inc(count);
    end;
  end;
  
  if count > 0 then
    Result := totalHours / count
  else
    Result := 0.0;
  
  SetLength(slas, 0);
end;

function TTaskSLAAnalysisClass.GetAverageResolutionTime(): double;
var
  slas: TSLAArray;
  i: integer;
  totalHours: double;
  count: integer;
begin
  totalHours := 0;
  count := 0;
  slas := slaMgr.GetActiveSLAs();
  
  for i := 0 to Length(slas) - 1 do
  begin
    if slas[i].actualResolutionDate <> 0 then
    begin
      totalHours := totalHours + slas[i].resolutionTimeHours;
      Inc(count);
    end;
  end;
  
  if count > 0 then
    Result := totalHours / count
  else
    Result := 0.0;
  
  SetLength(slas, 0);
end;

function TTaskSLAAnalysisClass.GetSLAsNearBreach(warningHoursThreshold: integer): TSLAArray;
var
  slas: TSLAArray;
  i, j: integer;
  elapsed: double;
begin
  SetLength(Result, 0);
  j := 0;
  slas := slaMgr.GetActiveSLAs();
  
  for i := 0 to Length(slas) - 1 do
  begin
    elapsed := (Now() - slas[i].createdDate) * 24; { Convert to hours }
    
    if (elapsed >= (slas[i].resolutionTimeHours - warningHoursThreshold)) and
       (elapsed < slas[i].resolutionTimeHours) then
    begin
      SetLength(Result, j + 1);
      Result[j] := slas[i];
      Inc(j);
    end;
  end;
  
  SetLength(slas, 0);
end;

function TTaskSLAAnalysisClass.GetSLAMetrics(): string;
var
  compliance: double;
  avgResponse: double;
  avgResolution: double;
begin
  compliance := GetCompliancePercentage();
  avgResponse := GetAverageResponseTime();
  avgResolution := GetAverageResolutionTime();
  
  Result := Format('SLA Compliance: %.1f%% | Avg Response: %.1f hrs | Avg Resolution: %.1f hrs',
                   [compliance, avgResponse, avgResolution]);
end;

procedure TTaskSLAAnalysisClass.SelfTest();
var
  compliance: double;
  avgResp: double;
  metrics: string;
begin
  WriteLn('Testing SLA Analysis Manager...');
  
  compliance := GetCompliancePercentage();
  WriteLn('SLA Compliance: ', compliance:5:1, '%');
  
  avgResp := GetAverageResponseTime();
  WriteLn('Average Response Time: ', avgResp:5:1, ' hours');
  
  metrics := GetSLAMetrics();
  WriteLn('SLA Metrics: ', metrics);
  
  WriteLn('SLA Analysis test completed!');
end;

end.
