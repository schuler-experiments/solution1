
unit TaskQuality;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils;

type
  { Quality metrics for a completed task }
  TQualityScore = record
    id: integer;
    taskId: integer;
    completionQuality: double;    { 0-10 scale }
    timelineAdherence: double;    { 0-10 scale }
    requirementsMet: double;      { 0-10 scale }
    userSatisfaction: double;     { 0-10 scale }
    defectCount: integer;
    reworkRequired: boolean;
    overallScore: double;         { Calculated average }
    scoredDate: TDateTime;
    scoredBy: string;
    notes: string;
  end;

  { Quality metrics trends }
  TQualityTrend = record
    averageQuality: double;
    improvementRate: double;
    defectRate: double;
    reworkPercentage: double;
  end;

  { Dynamic array types }
  TQualityScoreArray = array of TQualityScore;
  TIntegerArray = array of integer;

  { Quality Manager class }
  TTaskQualityManagerClass = class
  private
    qualityScores: TQualityScoreArray;
    nextId: integer;
    function FindScoreIndex(id: integer): integer;
    function FindTaskScoreIndex(taskId: integer): integer;
  public
    constructor Create();
    destructor Destroy(); override;
    
    { Core quality operations }
    function RecordQualityScore(taskId: integer; completion, timeline, 
                               requirements, satisfaction: double; 
                               defects: integer; scoredBy: string): integer;
    function GetQualityScore(id: integer): TQualityScore;
    function GetTaskQualityScore(taskId: integer): TQualityScore;
    procedure UpdateQualityScore(id: integer; newScore: TQualityScore);
    
    { Quality queries }
    function GetScoreCount(): integer;
    function GetHighQualityTasks(minScore: double): TIntegerArray;
    function GetLowQualityTasks(maxScore: double): TIntegerArray;
    function GetTasksRequiringRework(): TIntegerArray;
    
    { Data access for analysis }
    function GetAllQualityScores(): TQualityScoreArray;
    
    { Scoring adjustments }
    function AddDefect(taskId: integer): boolean;
    function ResolveDefect(taskId: integer): boolean;
    function MarkForRework(taskId: integer): boolean;
    function ClearRework(taskId: integer): boolean;
    
    { Data management }
    procedure DeleteQualityScore(id: integer);
    procedure ClearOldScores(beforeDate: TDateTime);
    
    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskQualityManagerClass.Create();
begin
  inherited Create();
  SetLength(qualityScores, 0);
  nextId := 1;
end;

destructor TTaskQualityManagerClass.Destroy();
begin
  SetLength(qualityScores, 0);
  inherited Destroy();
end;

function TTaskQualityManagerClass.FindScoreIndex(id: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(qualityScores) - 1 do
  begin
    if qualityScores[i].id = id then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTaskQualityManagerClass.FindTaskScoreIndex(taskId: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(qualityScores) - 1 do
  begin
    if qualityScores[i].taskId = taskId then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTaskQualityManagerClass.RecordQualityScore(taskId: integer; completion, timeline,
                                                    requirements, satisfaction: double;
                                                    defects: integer; scoredBy: string): integer;
var
  newScore: TQualityScore;
begin
  newScore.id := nextId;
  newScore.taskId := taskId;
  newScore.completionQuality := completion;
  newScore.timelineAdherence := timeline;
  newScore.requirementsMet := requirements;
  newScore.userSatisfaction := satisfaction;
  newScore.defectCount := defects;
  newScore.reworkRequired := False;
  newScore.overallScore := (completion + timeline + requirements + satisfaction) / 4.0;
  newScore.scoredDate := Now();
  newScore.scoredBy := scoredBy;
  newScore.notes := '';
  
  SetLength(qualityScores, Length(qualityScores) + 1);
  qualityScores[Length(qualityScores) - 1] := newScore;
  
  Result := nextId;
  Inc(nextId);
end;

function TTaskQualityManagerClass.GetQualityScore(id: integer): TQualityScore;
var
  idx: integer;
begin
  idx := FindScoreIndex(id);
  if idx >= 0 then
    Result := qualityScores[idx]
  else
  begin
    FillByte(Result, SizeOf(Result), 0);
    Result.id := -1;
  end;
end;

function TTaskQualityManagerClass.GetTaskQualityScore(taskId: integer): TQualityScore;
var
  idx: integer;
begin
  idx := FindTaskScoreIndex(taskId);
  if idx >= 0 then
    Result := qualityScores[idx]
  else
  begin
    FillByte(Result, SizeOf(Result), 0);
    Result.id := -1;
  end;
end;

procedure TTaskQualityManagerClass.UpdateQualityScore(id: integer; newScore: TQualityScore);
var
  idx: integer;
begin
  idx := FindScoreIndex(id);
  if idx >= 0 then
  begin
    newScore.overallScore := (newScore.completionQuality + newScore.timelineAdherence +
                              newScore.requirementsMet + newScore.userSatisfaction) / 4.0;
    qualityScores[idx] := newScore;
  end;
end;

function TTaskQualityManagerClass.GetScoreCount(): integer;
begin
  Result := Length(qualityScores);
end;

function TTaskQualityManagerClass.GetHighQualityTasks(minScore: double): TIntegerArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to Length(qualityScores) - 1 do
  begin
    if qualityScores[i].overallScore >= minScore then
    begin
      SetLength(Result, count + 1);
      Result[count] := qualityScores[i].taskId;
      Inc(count);
    end;
  end;
end;

function TTaskQualityManagerClass.GetLowQualityTasks(maxScore: double): TIntegerArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to Length(qualityScores) - 1 do
  begin
    if qualityScores[i].overallScore <= maxScore then
    begin
      SetLength(Result, count + 1);
      Result[count] := qualityScores[i].taskId;
      Inc(count);
    end;
  end;
end;

function TTaskQualityManagerClass.GetTasksRequiringRework(): TIntegerArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to Length(qualityScores) - 1 do
  begin
    if qualityScores[i].reworkRequired then
    begin
      SetLength(Result, count + 1);
      Result[count] := qualityScores[i].taskId;
      Inc(count);
    end;
  end;
end;

function TTaskQualityManagerClass.GetAllQualityScores(): TQualityScoreArray;
begin
  SetLength(Result, Length(qualityScores));
  if Length(qualityScores) > 0 then
    Move(qualityScores[0], Result[0], Length(qualityScores) * SizeOf(TQualityScore));
end;

function TTaskQualityManagerClass.AddDefect(taskId: integer): boolean;
var
  idx: integer;
begin
  idx := FindTaskScoreIndex(taskId);
  if idx >= 0 then
  begin
    Inc(qualityScores[idx].defectCount);
    Result := True;
  end else
    Result := False;
end;

function TTaskQualityManagerClass.ResolveDefect(taskId: integer): boolean;
var
  idx: integer;
begin
  idx := FindTaskScoreIndex(taskId);
  if idx >= 0 then
  begin
    if qualityScores[idx].defectCount > 0 then
      Dec(qualityScores[idx].defectCount);
    Result := True;
  end else
    Result := False;
end;

function TTaskQualityManagerClass.MarkForRework(taskId: integer): boolean;
var
  idx: integer;
begin
  idx := FindTaskScoreIndex(taskId);
  if idx >= 0 then
  begin
    qualityScores[idx].reworkRequired := True;
    Result := True;
  end else
    Result := False;
end;

function TTaskQualityManagerClass.ClearRework(taskId: integer): boolean;
var
  idx: integer;
begin
  idx := FindTaskScoreIndex(taskId);
  if idx >= 0 then
  begin
    qualityScores[idx].reworkRequired := False;
    Result := True;
  end else
    Result := False;
end;

procedure TTaskQualityManagerClass.DeleteQualityScore(id: integer);
var
  idx, i: integer;
begin
  idx := FindScoreIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(qualityScores) - 2 do
      qualityScores[i] := qualityScores[i + 1];
    SetLength(qualityScores, Length(qualityScores) - 1);
  end;
end;

procedure TTaskQualityManagerClass.ClearOldScores(beforeDate: TDateTime);
var
  i, j: integer;
begin
  for i := Length(qualityScores) - 1 downto 0 do
  begin
    if qualityScores[i].scoredDate < beforeDate then
    begin
      for j := i to Length(qualityScores) - 2 do
        qualityScores[j] := qualityScores[j + 1];
      SetLength(qualityScores, Length(qualityScores) - 1);
    end;
  end;
end;

procedure TTaskQualityManagerClass.SelfTest();
var
  scoreId: integer;
  score: TQualityScore;
  trend: TQualityTrend;
  highQuality: TIntegerArray;
  i: integer;
begin
  WriteLn('Testing Quality Manager...');
  
  scoreId := RecordQualityScore(1, 9.5, 9.0, 8.5, 9.0, 0, 'reviewer1');
  WriteLn('Recorded quality score, ID: ', scoreId);
  
  RecordQualityScore(2, 7.0, 6.5, 7.5, 6.0, 2, 'reviewer1');
  RecordQualityScore(3, 8.5, 8.0, 8.0, 8.5, 1, 'reviewer2');
  
  WriteLn('Total quality scores: ', GetScoreCount());
  
  score := GetQualityScore(1);
  WriteLn('Retrieved score 1, overall: ', FloatToStr(score.overallScore));
  
  AddDefect(2);
  WriteLn('Added defect to task 2');
  
  MarkForRework(2);
  WriteLn('Marked task 2 for rework');
  
  { WriteLn('Average quality: ', FloatToStr(CalculateAverageQuality())); }
  { WriteLn('Defect rate: ', FloatToStr(GetDefectRate())); }
  { WriteLn('Rework percentage: ', FloatToStr(GetReworkPercentage())); }
  
  { trend := CalculateQualityTrend(); }
  { WriteLn('Quality improving: ', BoolToStr(IsQualityImproving(), True)); }
  
  highQuality := GetHighQualityTasks(8.0);
  WriteLn('High quality tasks (>= 8.0): ', Length(highQuality));
  SetLength(highQuality, 0);
  
  WriteLn('Quality Manager test completed!');
end;

end.
