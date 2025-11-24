
unit TaskQualityAnalysis;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, TaskQuality;

type
  { Quality analysis class }
  TTaskQualityAnalysisClass = class
  private
    qualityMgr: TTaskQualityManagerClass;
  public
    constructor Create(mgr: TTaskQualityManagerClass);
    destructor Destroy(); override;
    
    { Quality analysis }
    function CalculateAverageQuality(): double;
    function CalculateQualityTrend(): TQualityTrend;
    function GetDefectRate(): double;
    function GetReworkPercentage(): double;
    function GetQualityMetrics(): string;
    
    { Quality by category }
    function GetAverageQualityByMonth(month, year: word): double;
    function GetQualityDelta(beforeDate, afterDate: TDateTime): double;
    function IsQualityImproving(): boolean;
    
    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskQualityAnalysisClass.Create(mgr: TTaskQualityManagerClass);
begin
  inherited Create();
  qualityMgr := mgr;
end;

destructor TTaskQualityAnalysisClass.Destroy();
begin
  inherited Destroy();
end;

function TTaskQualityAnalysisClass.CalculateAverageQuality(): double;
var
  scores: TQualityScoreArray;
  i: integer;
  sum: double;
begin
  sum := 0;
  scores := qualityMgr.GetAllQualityScores();
  
  if Length(scores) = 0 then
  begin
    Result := 0.0;
    SetLength(scores, 0);
    Exit;
  end;
  
  for i := 0 to Length(scores) - 1 do
    sum := sum + scores[i].overallScore;
  
  Result := sum / Length(scores);
  SetLength(scores, 0);
end;

function TTaskQualityAnalysisClass.CalculateQualityTrend(): TQualityTrend;
var
  scores: TQualityScoreArray;
  i: integer;
  count: integer;
  avgQuality: double;
  defectSum: integer;
  reworkCount: integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  scores := qualityMgr.GetAllQualityScores();
  
  if Length(scores) = 0 then
  begin
    SetLength(scores, 0);
    Exit;
  end;
  
  count := 0;
  defectSum := 0;
  reworkCount := 0;
  avgQuality := 0;
  
  for i := 0 to Length(scores) - 1 do
  begin
    avgQuality := avgQuality + scores[i].overallScore;
    defectSum := defectSum + scores[i].defectCount;
    if scores[i].reworkRequired then
      Inc(reworkCount);
    Inc(count);
  end;
  
  if count > 0 then
  begin
    Result.averageQuality := avgQuality / count;
    Result.defectRate := defectSum / count;
    Result.reworkPercentage := (reworkCount / count) * 100.0;
  end;
  
  SetLength(scores, 0);
end;

function TTaskQualityAnalysisClass.GetDefectRate(): double;
var
  trend: TQualityTrend;
begin
  trend := CalculateQualityTrend();
  Result := trend.defectRate;
end;

function TTaskQualityAnalysisClass.GetReworkPercentage(): double;
var
  trend: TQualityTrend;
begin
  trend := CalculateQualityTrend();
  Result := trend.reworkPercentage;
end;

function TTaskQualityAnalysisClass.GetQualityMetrics(): string;
var
  avgQuality: double;
  defectRate: double;
  reworkPct: double;
begin
  avgQuality := CalculateAverageQuality();
  defectRate := GetDefectRate();
  reworkPct := GetReworkPercentage();
  
  Result := Format('Quality Metrics - Avg: %.1f | Defects: %.1f | Rework: %.1f%%',
                   [avgQuality, defectRate, reworkPct]);
end;

function TTaskQualityAnalysisClass.GetAverageQualityByMonth(month, year: word): double;
var
  scores: TQualityScoreArray;
  i: integer;
  sum: double;
  count: integer;
  scoreMonth, scoreYear: word;
  scoreDay: word;
begin
  sum := 0;
  count := 0;
  scores := qualityMgr.GetAllQualityScores();
  
  for i := 0 to Length(scores) - 1 do
  begin
    DecodeDate(scores[i].scoredDate, scoreYear, scoreMonth, scoreDay);
    if (scoreMonth = month) and (scoreYear = year) then
    begin
      sum := sum + scores[i].overallScore;
      Inc(count);
    end;
  end;
  
  if count > 0 then
    Result := sum / count
  else
    Result := 0.0;
  
  SetLength(scores, 0);
end;

function TTaskQualityAnalysisClass.GetQualityDelta(beforeDate, afterDate: TDateTime): double;
var
  scores: TQualityScoreArray;
  i: integer;
  beforeSum, afterSum: double;
  beforeCount, afterCount: integer;
begin
  beforeSum := 0;
  afterSum := 0;
  beforeCount := 0;
  afterCount := 0;
  scores := qualityMgr.GetAllQualityScores();
  
  for i := 0 to Length(scores) - 1 do
  begin
    if scores[i].scoredDate < beforeDate then
    begin
      beforeSum := beforeSum + scores[i].overallScore;
      Inc(beforeCount);
    end
    else if scores[i].scoredDate >= afterDate then
    begin
      afterSum := afterSum + scores[i].overallScore;
      Inc(afterCount);
    end;
  end;
  
  if (beforeCount > 0) and (afterCount > 0) then
    Result := (afterSum / afterCount) - (beforeSum / beforeCount)
  else
    Result := 0.0;
  
  SetLength(scores, 0);
end;

function TTaskQualityAnalysisClass.IsQualityImproving(): boolean;
var
  trend: TQualityTrend;
begin
  trend := CalculateQualityTrend();
  Result := trend.improvementRate > 0;
end;

procedure TTaskQualityAnalysisClass.SelfTest();
var
  avgQuality: double;
  metrics: string;
begin
  WriteLn('Testing Quality Analysis Manager...');
  
  avgQuality := CalculateAverageQuality();
  WriteLn('Average Quality Score: ', avgQuality:5:1);
  
  metrics := GetQualityMetrics();
  WriteLn('Quality Metrics: ', metrics);
  
  WriteLn('Quality Analysis test completed!');
end;

end.
