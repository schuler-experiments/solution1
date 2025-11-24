
unit TaskSLA;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils;

type
  { SLA Status }
  TSLAStatus = (slaMet, slaAtRisk, slaBreached, slaWaived);

  { SLA Priority Level }
  TSLAPriority = (slpCritical, slpHigh, slpNormal, slpLow);

  { SLA Service Level Agreement record }
  TSLA = record
    id: integer;
    taskId: integer;
    name: string;
    description: string;
    priority: TSLAPriority;
    responseTimeHours: integer;
    resolutionTimeHours: integer;
    createdDate: TDateTime;
    targetResponseDate: TDateTime;
    targetResolutionDate: TDateTime;
    actualResponseDate: TDateTime;
    actualResolutionDate: TDateTime;
    status: TSLAStatus;
    isActive: boolean;
    breachNotified: boolean;
  end;

  { Dynamic array type for SLAs }
  TSLAArray = array of TSLA;

  { SLA Manager class }
  TTaskSLAManagerClass = class
  private
    slas: TSLAArray;
    nextId: integer;
    function FindSLAIndex(id: integer): integer;
    function FindTaskSLAIndex(taskId: integer): integer;
    function StatusToString(status: TSLAStatus): string;
    function PriorityToString(priority: TSLAPriority): string;
    function StringToStatus(s: string): TSLAStatus;
    function StringToPriority(s: string): TSLAPriority;
  public
    constructor Create();
    destructor Destroy(); override;
    
    { Core SLA operations }
    function AddSLA(taskId: integer; name, description: string;
                   priority: TSLAPriority; responseHours, resolutionHours: integer): integer;
    function GetSLA(id: integer): TSLA;
    function GetTaskSLA(taskId: integer): TSLA;
    procedure UpdateSLA(id: integer; newSLA: TSLA);
    procedure DeleteSLA(id: integer);
    
    { SLA execution tracking }
    function RecordResponse(slaId: integer): boolean;
    function RecordResolution(slaId: integer): boolean;
    
    { Status operations }
    function GetSLAStatus(id: integer): TSLAStatus;
    function CalculateSLAStatus(sla: TSLA): TSLAStatus;
    procedure UpdateSLAStatus(id: integer);
    
    { Query operations }
    function GetSLACount(): integer;
    function GetActiveSLAs(): TSLAArray;
    function GetBreachedSLAs(): TSLAArray;
    function GetAtRiskSLAs(): TSLAArray;
    function GetMetSLAs(): TSLAArray;
    
    { Metrics and analysis }
    function GetCompliancePercentage(): double;
    function GetAverageResponseTime(): double;
    function GetAverageResolutionTime(): double;
    function GetSLAsNearBreach(warningHoursThreshold: integer): TSLAArray;
    function GetSLAMetrics(): string;
    
    { Waiver operations }
    function WaiveSLA(id: integer; reason: string): boolean;
    function UnwaveSLA(id: integer): boolean;
    
    { Cleanup }
    procedure ClearExpiredSLAs(beforeDate: TDateTime);
    
    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskSLAManagerClass.Create();
begin
  inherited Create();
  SetLength(slas, 0);
  nextId := 1;
end;

destructor TTaskSLAManagerClass.Destroy();
begin
  SetLength(slas, 0);
  inherited Destroy();
end;

function TTaskSLAManagerClass.FindSLAIndex(id: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(slas) - 1 do
  begin
    if slas[i].id = id then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTaskSLAManagerClass.FindTaskSLAIndex(taskId: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(slas) - 1 do
  begin
    if slas[i].taskId = taskId then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTaskSLAManagerClass.StatusToString(status: TSLAStatus): string;
begin
  case status of
    slaMet: Result := 'MET';
    slaAtRisk: Result := 'AT_RISK';
    slaBreached: Result := 'BREACHED';
    slaWaived: Result := 'WAIVED';
  else
    Result := 'UNKNOWN';
  end;
end;

function TTaskSLAManagerClass.PriorityToString(priority: TSLAPriority): string;
begin
  case priority of
    slpCritical: Result := 'CRITICAL';
    slpHigh: Result := 'HIGH';
    slpNormal: Result := 'NORMAL';
    slpLow: Result := 'LOW';
  else
    Result := 'UNKNOWN';
  end;
end;

function TTaskSLAManagerClass.StringToStatus(s: string): TSLAStatus;
begin
  s := UpperCase(s);
  if s = 'MET' then Result := slaMet
  else if s = 'AT_RISK' then Result := slaAtRisk
  else if s = 'BREACHED' then Result := slaBreached
  else if s = 'WAIVED' then Result := slaWaived
  else Result := slaMet;
end;

function TTaskSLAManagerClass.StringToPriority(s: string): TSLAPriority;
begin
  s := UpperCase(s);
  if s = 'CRITICAL' then Result := slpCritical
  else if s = 'HIGH' then Result := slpHigh
  else if s = 'NORMAL' then Result := slpNormal
  else if s = 'LOW' then Result := slpLow
  else Result := slpNormal;
end;

function TTaskSLAManagerClass.AddSLA(taskId: integer; name, description: string;
                                    priority: TSLAPriority; responseHours, resolutionHours: integer): integer;
var
  newSLA: TSLA;
begin
  newSLA.id := nextId;
  newSLA.taskId := taskId;
  newSLA.name := name;
  newSLA.description := description;
  newSLA.priority := priority;
  newSLA.responseTimeHours := responseHours;
  newSLA.resolutionTimeHours := resolutionHours;
  newSLA.createdDate := Now();
  newSLA.targetResponseDate := IncHour(Now(), responseHours);
  newSLA.targetResolutionDate := IncHour(Now(), resolutionHours);
  newSLA.actualResponseDate := 0;
  newSLA.actualResolutionDate := 0;
  newSLA.status := slaAtRisk;
  newSLA.isActive := True;
  newSLA.breachNotified := False;
  
  SetLength(slas, Length(slas) + 1);
  slas[Length(slas) - 1] := newSLA;
  
  Result := nextId;
  Inc(nextId);
end;

function TTaskSLAManagerClass.GetSLA(id: integer): TSLA;
var
  idx: integer;
begin
  idx := FindSLAIndex(id);
  if idx >= 0 then
    Result := slas[idx]
  else
  begin
    FillByte(Result, SizeOf(Result), 0);
    Result.id := -1;
  end;
end;

function TTaskSLAManagerClass.GetTaskSLA(taskId: integer): TSLA;
var
  idx: integer;
begin
  idx := FindTaskSLAIndex(taskId);
  if idx >= 0 then
    Result := slas[idx]
  else
  begin
    FillByte(Result, SizeOf(Result), 0);
    Result.id := -1;
  end;
end;

procedure TTaskSLAManagerClass.UpdateSLA(id: integer; newSLA: TSLA);
var
  idx: integer;
begin
  idx := FindSLAIndex(id);
  if idx >= 0 then
    slas[idx] := newSLA;
end;

procedure TTaskSLAManagerClass.DeleteSLA(id: integer);
var
  idx, i: integer;
begin
  idx := FindSLAIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(slas) - 2 do
      slas[i] := slas[i + 1];
    SetLength(slas, Length(slas) - 1);
  end;
end;

function TTaskSLAManagerClass.RecordResponse(slaId: integer): boolean;
var
  idx: integer;
begin
  idx := FindSLAIndex(slaId);
  if idx >= 0 then
  begin
    slas[idx].actualResponseDate := Now();
    Result := True;
  end else
    Result := False;
end;

function TTaskSLAManagerClass.RecordResolution(slaId: integer): boolean;
var
  idx: integer;
begin
  idx := FindSLAIndex(slaId);
  if idx >= 0 then
  begin
    slas[idx].actualResolutionDate := Now();
    UpdateSLAStatus(slaId);
    Result := True;
  end else
    Result := False;
end;

function TTaskSLAManagerClass.CalculateSLAStatus(sla: TSLA): TSLAStatus;
var
  checkTime: TDateTime;
begin
  if sla.status = slaWaived then
  begin
    Result := slaWaived;
    Exit;
  end;
  
  checkTime := Now();
  
  if sla.actualResolutionDate > 0 then
  begin
    if sla.actualResolutionDate <= sla.targetResolutionDate then
      Result := slaMet
    else
      Result := slaBreached;
  end else if checkTime > sla.targetResolutionDate then
    Result := slaBreached
  else if checkTime > (sla.targetResolutionDate - (sla.targetResolutionDate - sla.createdDate) * 0.1) then
    Result := slaAtRisk
  else
    Result := slaMet;
end;

function TTaskSLAManagerClass.GetSLAStatus(id: integer): TSLAStatus;
var
  sla: TSLA;
begin
  sla := GetSLA(id);
  if sla.id = -1 then
    Result := slaMet
  else
    Result := sla.status;
end;

procedure TTaskSLAManagerClass.UpdateSLAStatus(id: integer);
var
  idx: integer;
begin
  idx := FindSLAIndex(id);
  if idx >= 0 then
    slas[idx].status := CalculateSLAStatus(slas[idx]);
end;

function TTaskSLAManagerClass.GetSLACount(): integer;
begin
  Result := Length(slas);
end;

function TTaskSLAManagerClass.GetActiveSLAs(): TSLAArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to Length(slas) - 1 do
  begin
    if slas[i].isActive then
    begin
      SetLength(Result, count + 1);
      Result[count] := slas[i];
      Inc(count);
    end;
  end;
end;

function TTaskSLAManagerClass.GetBreachedSLAs(): TSLAArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to Length(slas) - 1 do
  begin
    UpdateSLAStatus(slas[i].id);
    if slas[i].status = slaBreached then
    begin
      SetLength(Result, count + 1);
      Result[count] := slas[i];
      Inc(count);
    end;
  end;
end;

function TTaskSLAManagerClass.GetAtRiskSLAs(): TSLAArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to Length(slas) - 1 do
  begin
    UpdateSLAStatus(slas[i].id);
    if slas[i].status = slaAtRisk then
    begin
      SetLength(Result, count + 1);
      Result[count] := slas[i];
      Inc(count);
    end;
  end;
end;

function TTaskSLAManagerClass.GetMetSLAs(): TSLAArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to Length(slas) - 1 do
  begin
    UpdateSLAStatus(slas[i].id);
    if slas[i].status = slaMet then
    begin
      SetLength(Result, count + 1);
      Result[count] := slas[i];
      Inc(count);
    end;
  end;
end;

function TTaskSLAManagerClass.GetCompliancePercentage(): double;
var
  i: integer;
  metCount, totalCount: integer;
begin
  metCount := 0;
  totalCount := Length(slas);
  
  if totalCount = 0 then
  begin
    Result := 0.0;
    Exit;
  end;
  
  for i := 0 to Length(slas) - 1 do
  begin
    UpdateSLAStatus(slas[i].id);
    if slas[i].status = slaMet then
      Inc(metCount);
  end;
  
  Result := (metCount / totalCount) * 100.0;
end;

function TTaskSLAManagerClass.GetAverageResponseTime(): double;
var
  i: integer;
  totalHours: double;
  count: integer;
begin
  totalHours := 0;
  count := 0;
  
  for i := 0 to Length(slas) - 1 do
  begin
    if slas[i].actualResponseDate > 0 then
    begin
      totalHours += HoursBetween(slas[i].createdDate, slas[i].actualResponseDate);
      Inc(count);
    end;
  end;
  
  if count = 0 then
    Result := 0.0
  else
    Result := totalHours / count;
end;

function TTaskSLAManagerClass.GetAverageResolutionTime(): double;
var
  i: integer;
  totalHours: double;
  count: integer;
begin
  totalHours := 0;
  count := 0;
  
  for i := 0 to Length(slas) - 1 do
  begin
    if slas[i].actualResolutionDate > 0 then
    begin
      totalHours += HoursBetween(slas[i].createdDate, slas[i].actualResolutionDate);
      Inc(count);
    end;
  end;
  
  if count = 0 then
    Result := 0.0
  else
    Result := totalHours / count;
end;

function TTaskSLAManagerClass.GetSLAsNearBreach(warningHoursThreshold: integer): TSLAArray;
var
  i, count: integer;
  checkTime: TDateTime;
  hoursRemaining: double;
begin
  SetLength(Result, 0);
  count := 0;
  checkTime := Now();
  
  for i := 0 to Length(slas) - 1 do
  begin
    if slas[i].isActive and (slas[i].actualResolutionDate = 0) then
    begin
      hoursRemaining := HoursBetween(checkTime, slas[i].targetResolutionDate);
      if (hoursRemaining > 0) and (hoursRemaining <= warningHoursThreshold) then
      begin
        SetLength(Result, count + 1);
        Result[count] := slas[i];
        Inc(count);
      end;
    end;
  end;
end;

function TTaskSLAManagerClass.GetSLAMetrics(): string;
begin
  Result := 'SLA Metrics:' + #10;
  Result += 'Total SLAs: ' + IntToStr(GetSLACount()) + #10;
  Result += 'Compliance: ' + FloatToStr(GetCompliancePercentage()) + '%' + #10;
  Result += 'Avg Response Time: ' + FloatToStr(GetAverageResponseTime()) + ' hours' + #10;
  Result += 'Avg Resolution Time: ' + FloatToStr(GetAverageResolutionTime()) + ' hours' + #10;
end;

function TTaskSLAManagerClass.WaiveSLA(id: integer; reason: string): boolean;
var
  idx: integer;
begin
  idx := FindSLAIndex(id);
  if idx >= 0 then
  begin
    slas[idx].status := slaWaived;
    Result := True;
  end else
    Result := False;
end;

function TTaskSLAManagerClass.UnwaveSLA(id: integer): boolean;
var
  idx: integer;
begin
  idx := FindSLAIndex(id);
  if idx >= 0 then
  begin
    slas[idx].status := slaAtRisk;
    UpdateSLAStatus(id);
    Result := True;
  end else
    Result := False;
end;

procedure TTaskSLAManagerClass.ClearExpiredSLAs(beforeDate: TDateTime);
var
  i, j: integer;
begin
  for i := Length(slas) - 1 downto 0 do
  begin
    if slas[i].targetResolutionDate < beforeDate then
    begin
      for j := i to Length(slas) - 2 do
        slas[j] := slas[j + 1];
      SetLength(slas, Length(slas) - 1);
    end;
  end;
end;

procedure TTaskSLAManagerClass.SelfTest();
var
  slaId: integer;
  sla: TSLA;
  activeCount: integer;
begin
  WriteLn('Testing SLA Manager...');
  
  slaId := AddSLA(1, 'Support Response', 'First response within 4 hours', 
                  slpHigh, 4, 24);
  WriteLn('Created SLA, ID: ', slaId);
  
  slaId := AddSLA(2, 'Critical Issue', 'Resolution within 1 hour', 
                  slpCritical, 1, 4);
  WriteLn('Created critical SLA, ID: ', slaId);
  
  AddSLA(3, 'Normal Task', 'Resolution within 48 hours', 
         slpNormal, 24, 48);
  
  WriteLn('Total SLAs: ', GetSLACount());
  
  sla := GetSLA(1);
  WriteLn('Retrieved SLA 1: ', sla.name);
  
  RecordResponse(1);
  WriteLn('Recorded response for SLA 1');
  
  RecordResolution(1);
  WriteLn('Recorded resolution for SLA 1');
  
  UpdateSLAStatus(1);
  WriteLn('SLA 1 status: ', StatusToString(GetSLAStatus(1)));
  
  WriteLn('Compliance percentage: ', FloatToStr(GetCompliancePercentage()), '%');
  
  activeCount := Length(GetActiveSLAs());
  WriteLn('Active SLAs: ', activeCount);
  
  WriteLn('SLA Manager test completed!');
end;

end.
