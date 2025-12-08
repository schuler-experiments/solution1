
unit taskmanagerwellbeing;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math,
  taskmanager, taskmanageradvanced, taskmanagerlifestyle;

type
  // Stress and energy levels
  TStressLevel = (slVeryLow, slLow, slModerate, slHigh, slVeryHigh);
  TEnergyLevel = (elVeryLow, elLow, elModerate, elHigh, elVeryHigh);
  TMoodLevel = (mlVeryNegative, mlNegative, mlNeutral, mlPositive, mlVeryPositive);
  
  // Burnout risk assessment
  TBurnoutRisk = (brNone, brLow, brModerate, brHigh, brCritical);
  
  // Wellbeing check-in
  TWellbeingCheckIn = record
    ID: Integer;
    CheckInTime: TDateTime;
    StressLevel: TStressLevel;
    EnergyLevel: TEnergyLevel;
    MoodLevel: TMoodLevel;
    SleepQuality: Integer;        // 1-10 scale
    WorkSatisfaction: Integer;    // 1-10 scale
    Notes: string;
    PhysicalSymptoms: array of string;  // headache, fatigue, etc.
  end;
  
  TWellbeingCheckInArray = array of TWellbeingCheckIn;
  
  // Break session
  TBreakType = (btMicroBreak, btShortBreak, btLongBreak, btMeal, btExercise, btMeditation);
  
  TBreakSession = record
    ID: Integer;
    BreakType: TBreakType;
    StartTime: TDateTime;
    EndTime: TDateTime;
    DurationMinutes: Integer;
    WasSkipped: Boolean;
    EffectivenessRating: Integer; // 1-10 how refreshing it was
    Notes: string;
  end;
  
  TBreakSessionArray = array of TBreakSession;
  
  // Work-life balance metrics
  TWorkLifeBalance = record
    WeekNumber: Integer;
    WorkHours: Double;
    PersonalHours: Double;
    WeekendWorkHours: Double;
    EveningWorkHours: Double;     // After 6 PM
    AverageStressLevel: Double;
    AverageMoodLevel: Double;
    BreaksTaken: Integer;
    BreaksSkipped: Integer;
    BalanceScore: Double;         // 0-100, higher is better
  end;
  
  TWorkLifeBalanceArray = array of TWorkLifeBalance;
  
  // Burnout indicators
  TBurnoutIndicator = record
    IndicatorType: string;
    Severity: TBurnoutRisk;
    Description: string;
    DetectedDate: TDateTime;
    RecommendedAction: string;
  end;
  
  TBurnoutIndicatorArray = array of TBurnoutIndicator;
  
  // Energy pattern
  TEnergyPattern = record
    HourOfDay: Integer;
    AverageEnergyLevel: Double;
    SampleCount: Integer;
    RecommendedTaskType: string;  // 'creative', 'administrative', 'meetings', etc.
  end;
  
  TEnergyPatternArray = array of TEnergyPattern;
  
  // Wellness recommendation
  TWellnessRecommendation = record
    ID: Integer;
    Category: string;             // 'break', 'workload', 'sleep', 'stress'
    Priority: Integer;            // 1-5
    Title: string;
    Description: string;
    ActionSteps: array of string;
    ExpectedBenefit: string;
    CreatedDate: TDateTime;
    IsApplied: Boolean;
  end;
  
  TWellnessRecommendationArray = array of TWellnessRecommendation;
  
  // Cognitive load tracking
  TCognitiveLoad = record
    Timestamp: TDateTime;
    ActiveTasks: Integer;
    ContextSwitches: Integer;
    MentalDemand: Integer;        // 1-10 scale
    LoadScore: Double;            // Calculated score
  end;
  
  TCognitiveLoadArray = array of TCognitiveLoad;
  
  // Wellbeing settings
  TWellbeingSettings = record
    MaxDailyWorkHours: Double;
    MaxWeeklyWorkHours: Double;
    MinDailyBreaks: Integer;
    MicroBreakIntervalMinutes: Integer;  // Every X minutes
    LongBreakIntervalMinutes: Integer;
    EnableBurnoutAlerts: Boolean;
    EnableBreakReminders: Boolean;
    WorkdayStartHour: Integer;
    WorkdayEndHour: Integer;
    StressThreshold: TStressLevel;
    EnergyThreshold: TEnergyLevel;
  end;

  TWellbeingTaskManager = class(TLifestyleTaskManager)
  private
    FCheckIns: TWellbeingCheckInArray;
    FBreakSessions: TBreakSessionArray;
    FWorkLifeMetrics: TWorkLifeBalanceArray;
    FBurnoutIndicators: TBurnoutIndicatorArray;
    FEnergyPatterns: TEnergyPatternArray;
    FRecommendations: TWellnessRecommendationArray;
    FCognitiveLoad: TCognitiveLoadArray;
    FSettings: TWellbeingSettings;
    FNextCheckInID: Integer;
    FNextBreakID: Integer;
    FNextRecommendationID: Integer;
    FLastBreakTime: TDateTime;
    FLastCheckInTime: TDateTime;
    
    function FindCheckInIndex(AID: Integer): Integer;
    function FindBreakIndex(AID: Integer): Integer;
    function FindRecommendationIndex(AID: Integer): Integer;
    function CalculateBurnoutRisk: TBurnoutRisk;
    function CalculateWorkLifeBalance(AWeek: Integer): TWorkLifeBalance;
    procedure UpdateEnergyPatterns;
    procedure GenerateWellnessRecommendations;
    function ShouldTakeBreak: Boolean;
    function CalculateCognitiveLoad: Double;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Wellbeing check-ins
    function RecordCheckIn(AStressLevel: TStressLevel; AEnergyLevel: TEnergyLevel;
      AMoodLevel: TMoodLevel; ASleepQuality, AWorkSatisfaction: Integer;
      const ANotes: string): Integer;
    function AddPhysicalSymptom(ACheckInID: Integer; const ASymptom: string): Boolean;
    function GetRecentCheckIns(ADays: Integer): TWellbeingCheckInArray;
    function GetCheckInByID(AID: Integer): TWellbeingCheckIn;
    function GetAverageStressLevel(ADays: Integer): Double;
    function GetAverageMoodLevel(ADays: Integer): Double;
    function GetAverageEnergyLevel(ADays: Integer): Double;
    
    // Break management
    function StartBreak(ABreakType: TBreakType): Integer;
    function EndBreak(ABreakID: Integer; AEffectiveness: Integer; const ANotes: string): Boolean;
    function SkipBreak(ABreakID: Integer): Boolean;
    function GetBreakHistory(ADays: Integer): TBreakSessionArray;
    function GetBreakComplianceRate(ADays: Integer): Double;
    function GetTimeSinceLastBreak: Integer; // minutes
    function SuggestBreakType: TBreakType;
    
    // Work-life balance
    function GetWorkLifeBalance(AWeek: Integer): TWorkLifeBalance;
    function GetRecentWorkLifeBalance(AWeeks: Integer): TWorkLifeBalanceArray;
    function GetBalanceTrend: string;
    function IsWorkingOutsideHours: Boolean;
    function GetOvertimeHours(AWeek: Integer): Double;
    
    // Burnout detection
    function AssessBurnoutRisk: TBurnoutRisk;
    function GetBurnoutIndicators: TBurnoutIndicatorArray;
    function GetBurnoutScore: Double; // 0-100
    function GetRecoveryRecommendations: TWellnessRecommendationArray;
    
    // Energy management
    function RecordEnergyLevel(ALevel: TEnergyLevel; const ANotes: string): Boolean;
    function GetEnergyPatterns: TEnergyPatternArray;
    function GetOptimalTimeForTask(const ATaskType: string): Integer; // hour of day
    function GetCurrentEnergyLevel: TEnergyLevel;
    function SuggestTaskBasedOnEnergy: string;
    
    // Cognitive load
    function TrackCognitiveLoad(AActiveTasks, AContextSwitches, AMentalDemand: Integer): Boolean;
    function GetCurrentCognitiveLoad: Double;
    function GetCognitiveLoadHistory(AHours: Integer): TCognitiveLoadArray;
    function IsCognitiveOverload: Boolean;
    function SuggestLoadReduction: string;
    
    // Wellness recommendations
    function GetWellnessRecommendations: TWellnessRecommendationArray;
    function GetPriorityRecommendations: TWellnessRecommendationArray;
    function ApplyRecommendation(ARecommendationID: Integer): Boolean;
    function DismissRecommendation(ARecommendationID: Integer): Boolean;
    
    // Settings and configuration
    procedure ConfigureSettings(AMaxDailyHours, AMaxWeeklyHours: Double;
      AMinBreaks, AMicroBreakInterval, ALongBreakInterval: Integer);
    procedure SetWorkingHours(AStartHour, AEndHour: Integer);
    procedure EnableFeature(const AFeature: string; AEnabled: Boolean);
    function GetSettings: TWellbeingSettings;
    
    // Reports and insights
    function GenerateWellbeingReport: string;
    function GetStressTriggers: string;
    function GetEnergyInsights: string;
    function GetWellnessSummary: string;
    function GetMentalHealthScore: Double; // 0-100
    
    // Helper functions
    function StressLevelToString(ALevel: TStressLevel): string;
    function EnergyLevelToString(ALevel: TEnergyLevel): string;
    function MoodLevelToString(ALevel: TMoodLevel): string;
    function BurnoutRiskToString(ARisk: TBurnoutRisk): string;
    function BreakTypeToString(AType: TBreakType): string;
    
    // Data persistence
    function SaveWellbeingDataToFile(const AFilename: string): Boolean;
    function LoadWellbeingDataFromFile(const AFilename: string): Boolean;
  end;

implementation

constructor TWellbeingTaskManager.Create;
var
  i: Integer;
begin
  inherited Create;
  SetLength(FCheckIns, 0);
  SetLength(FBreakSessions, 0);
  SetLength(FWorkLifeMetrics, 0);
  SetLength(FBurnoutIndicators, 0);
  SetLength(FEnergyPatterns, 24); // One for each hour
  SetLength(FRecommendations, 0);
  SetLength(FCognitiveLoad, 0);
  
  FNextCheckInID := 1;
  FNextBreakID := 1;
  FNextRecommendationID := 1;
  FLastBreakTime := Now;
  FLastCheckInTime := Now;
  
  // Default settings
  FSettings.MaxDailyWorkHours := 8.0;
  FSettings.MaxWeeklyWorkHours := 40.0;
  FSettings.MinDailyBreaks := 3;
  FSettings.MicroBreakIntervalMinutes := 25; // Pomodoro-style
  FSettings.LongBreakIntervalMinutes := 120;
  FSettings.EnableBurnoutAlerts := True;
  FSettings.EnableBreakReminders := True;
  FSettings.WorkdayStartHour := 9;
  FSettings.WorkdayEndHour := 17;
  FSettings.StressThreshold := slHigh;
  FSettings.EnergyThreshold := elLow;
  
  // Initialize energy patterns
  for i := 0 to 23 do
  begin
    FEnergyPatterns[i].HourOfDay := i;
    FEnergyPatterns[i].AverageEnergyLevel := 5.0;
    FEnergyPatterns[i].SampleCount := 0;
    FEnergyPatterns[i].RecommendedTaskType := 'general';
  end;
end;

destructor TWellbeingTaskManager.Destroy;
begin
  SetLength(FCheckIns, 0);
  SetLength(FBreakSessions, 0);
  SetLength(FWorkLifeMetrics, 0);
  SetLength(FBurnoutIndicators, 0);
  SetLength(FEnergyPatterns, 0);
  SetLength(FRecommendations, 0);
  SetLength(FCognitiveLoad, 0);
  inherited Destroy;
end;

function TWellbeingTaskManager.FindCheckInIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FCheckIns) do
    if FCheckIns[i].ID = AID then
      Exit(i);
end;

function TWellbeingTaskManager.FindBreakIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FBreakSessions) do
    if FBreakSessions[i].ID = AID then
      Exit(i);
end;

function TWellbeingTaskManager.FindRecommendationIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FRecommendations) do
    if FRecommendations[i].ID = AID then
      Exit(i);
end;

function TWellbeingTaskManager.RecordCheckIn(AStressLevel: TStressLevel;
  AEnergyLevel: TEnergyLevel; AMoodLevel: TMoodLevel;
  ASleepQuality, AWorkSatisfaction: Integer; const ANotes: string): Integer;
var
  checkIn: TWellbeingCheckIn;
begin
  checkIn.ID := FNextCheckInID;
  Inc(FNextCheckInID);
  checkIn.CheckInTime := Now;
  checkIn.StressLevel := AStressLevel;
  checkIn.EnergyLevel := AEnergyLevel;
  checkIn.MoodLevel := AMoodLevel;
  checkIn.SleepQuality := ASleepQuality;
  checkIn.WorkSatisfaction := AWorkSatisfaction;
  checkIn.Notes := ANotes;
  SetLength(checkIn.PhysicalSymptoms, 0);
  
  SetLength(FCheckIns, Length(FCheckIns) + 1);
  FCheckIns[High(FCheckIns)] := checkIn;
  
  FLastCheckInTime := Now;
  UpdateEnergyPatterns;
  
  // Generate recommendations if needed
  if (AStressLevel >= FSettings.StressThreshold) or
     (AEnergyLevel <= FSettings.EnergyThreshold) then
    GenerateWellnessRecommendations;
  
  Result := checkIn.ID;
end;

function TWellbeingTaskManager.AddPhysicalSymptom(ACheckInID: Integer;
  const ASymptom: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindCheckInIndex(ACheckInID);
  if idx >= 0 then
  begin
    SetLength(FCheckIns[idx].PhysicalSymptoms,
      Length(FCheckIns[idx].PhysicalSymptoms) + 1);
    FCheckIns[idx].PhysicalSymptoms[High(FCheckIns[idx].PhysicalSymptoms)] := ASymptom;
    Result := True;
  end;
end;

function TWellbeingTaskManager.GetRecentCheckIns(ADays: Integer): TWellbeingCheckInArray;
var
  i, count: Integer;
  cutoffDate: TDateTime;
begin
  cutoffDate := Now - ADays;
  count := 0;
  
  for i := 0 to High(FCheckIns) do
    if FCheckIns[i].CheckInTime >= cutoffDate then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  
  for i := 0 to High(FCheckIns) do
    if FCheckIns[i].CheckInTime >= cutoffDate then
    begin
      Result[count] := FCheckIns[i];
      Inc(count);
    end;
end;

function TWellbeingTaskManager.GetCheckInByID(AID: Integer): TWellbeingCheckIn;
var
  idx: Integer;
begin
  idx := FindCheckInIndex(AID);
  if idx >= 0 then
    Result := FCheckIns[idx]
  else
  begin
    Result.ID := -1;
    Result.CheckInTime := 0;
  end;
end;

function TWellbeingTaskManager.GetAverageStressLevel(ADays: Integer): Double;
var
  checkIns: TWellbeingCheckInArray;
  i, total: Integer;
begin
  Result := 0;
  checkIns := GetRecentCheckIns(ADays);
  if Length(checkIns) = 0 then Exit;
  
  total := 0;
  for i := 0 to High(checkIns) do
    total := total + Ord(checkIns[i].StressLevel);
  
  Result := total / Length(checkIns);
end;

function TWellbeingTaskManager.GetAverageMoodLevel(ADays: Integer): Double;
var
  checkIns: TWellbeingCheckInArray;
  i, total: Integer;
begin
  Result := 0;
  checkIns := GetRecentCheckIns(ADays);
  if Length(checkIns) = 0 then Exit;
  
  total := 0;
  for i := 0 to High(checkIns) do
    total := total + Ord(checkIns[i].MoodLevel);
  
  Result := total / Length(checkIns);
end;

function TWellbeingTaskManager.GetAverageEnergyLevel(ADays: Integer): Double;
var
  checkIns: TWellbeingCheckInArray;
  i, total: Integer;
begin
  Result := 0;
  checkIns := GetRecentCheckIns(ADays);
  if Length(checkIns) = 0 then Exit;
  
  total := 0;
  for i := 0 to High(checkIns) do
    total := total + Ord(checkIns[i].EnergyLevel);
  
  Result := total / Length(checkIns);
end;

function TWellbeingTaskManager.StartBreak(ABreakType: TBreakType): Integer;
var
  breakSession: TBreakSession;
begin
  breakSession.ID := FNextBreakID;
  Inc(FNextBreakID);
  breakSession.BreakType := ABreakType;
  breakSession.StartTime := Now;
  breakSession.EndTime := 0;
  breakSession.DurationMinutes := 0;
  breakSession.WasSkipped := False;
  breakSession.EffectivenessRating := 0;
  breakSession.Notes := '';
  
  SetLength(FBreakSessions, Length(FBreakSessions) + 1);
  FBreakSessions[High(FBreakSessions)] := breakSession;
  
  FLastBreakTime := Now;
  Result := breakSession.ID;
end;

function TWellbeingTaskManager.EndBreak(ABreakID: Integer;
  AEffectiveness: Integer; const ANotes: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindBreakIndex(ABreakID);
  if idx >= 0 then
  begin
    FBreakSessions[idx].EndTime := Now;
    FBreakSessions[idx].DurationMinutes :=
      MinutesBetween(FBreakSessions[idx].EndTime, FBreakSessions[idx].StartTime);
    FBreakSessions[idx].EffectivenessRating := AEffectiveness;
    FBreakSessions[idx].Notes := ANotes;
    Result := True;
  end;
end;

function TWellbeingTaskManager.SkipBreak(ABreakID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindBreakIndex(ABreakID);
  if idx >= 0 then
  begin
    FBreakSessions[idx].WasSkipped := True;
    FBreakSessions[idx].EndTime := Now;
    Result := True;
  end;
end;

function TWellbeingTaskManager.GetBreakHistory(ADays: Integer): TBreakSessionArray;
var
  i, count: Integer;
  cutoffDate: TDateTime;
begin
  cutoffDate := Now - ADays;
  count := 0;
  
  for i := 0 to High(FBreakSessions) do
    if FBreakSessions[i].StartTime >= cutoffDate then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  
  for i := 0 to High(FBreakSessions) do
    if FBreakSessions[i].StartTime >= cutoffDate then
    begin
      Result[count] := FBreakSessions[i];
      Inc(count);
    end;
end;

function TWellbeingTaskManager.GetBreakComplianceRate(ADays: Integer): Double;
var
  breaks: TBreakSessionArray;
  i, takenCount, totalDays: Integer;
begin
  Result := 0;
  breaks := GetBreakHistory(ADays);
  if Length(breaks) = 0 then Exit;
  
  takenCount := 0;
  for i := 0 to High(breaks) do
    if not breaks[i].WasSkipped then
      Inc(takenCount);
  
  totalDays := ADays;
  if totalDays = 0 then totalDays := 1;
  
  Result := (takenCount / (totalDays * FSettings.MinDailyBreaks)) * 100;
  if Result > 100 then Result := 100;
end;

function TWellbeingTaskManager.GetTimeSinceLastBreak: Integer;
begin
  Result := MinutesBetween(Now, FLastBreakTime);
end;

function TWellbeingTaskManager.SuggestBreakType: TBreakType;
var
  timeSince: Integer;
begin
  timeSince := GetTimeSinceLastBreak;
  
  if timeSince < 30 then
    Result := btMicroBreak
  else if timeSince < 120 then
    Result := btShortBreak
  else if timeSince < 240 then
    Result := btLongBreak
  else
    Result := btMeal;
end;

function TWellbeingTaskManager.ShouldTakeBreak: Boolean;
var
  timeSince: Integer;
begin
  if not FSettings.EnableBreakReminders then
    Exit(False);
  
  timeSince := GetTimeSinceLastBreak;
  Result := timeSince >= FSettings.MicroBreakIntervalMinutes;
end;

function TWellbeingTaskManager.CalculateBurnoutRisk: TBurnoutRisk;
var
  score: Double;
  stressAvg, energyAvg, moodAvg: Double;
begin
  score := 0;
  
  // Factor 1: Recent stress levels (0-40 points)
  stressAvg := GetAverageStressLevel(7);
  score := score + (stressAvg / 4 * 40);
  
  // Factor 2: Energy levels (0-30 points, inverse)
  energyAvg := GetAverageEnergyLevel(7);
  score := score + ((4 - energyAvg) / 4 * 30);
  
  // Factor 3: Mood levels (0-30 points, inverse)
  moodAvg := GetAverageMoodLevel(7);
  score := score + ((4 - moodAvg) / 4 * 30);
  
  // Classify risk
  if score < 20 then
    Result := brNone
  else if score < 40 then
    Result := brLow
  else if score < 60 then
    Result := brModerate
  else if score < 80 then
    Result := brHigh
  else
    Result := brCritical;
end;

function TWellbeingTaskManager.AssessBurnoutRisk: TBurnoutRisk;
var
  indicator: TBurnoutIndicator;
begin
  Result := CalculateBurnoutRisk;
  
  // Generate new indicators
  SetLength(FBurnoutIndicators, 0);
  
  if Result >= brModerate then
  begin
    indicator.IndicatorType := 'Elevated Stress';
    indicator.Severity := Result;
    indicator.Description := 'Your average stress levels have been elevated for the past week.';
    indicator.DetectedDate := Now;
    indicator.RecommendedAction := 'Consider taking a mental health day or reducing workload.';
    
    SetLength(FBurnoutIndicators, Length(FBurnoutIndicators) + 1);
    FBurnoutIndicators[High(FBurnoutIndicators)] := indicator;
  end;
end;

function TWellbeingTaskManager.GetBurnoutIndicators: TBurnoutIndicatorArray;
begin
  Result := FBurnoutIndicators;
end;

function TWellbeingTaskManager.GetBurnoutScore: Double;
var
  risk: TBurnoutRisk;
begin
  risk := CalculateBurnoutRisk;
  Result := Ord(risk) * 25; // Convert to 0-100 scale
end;

procedure TWellbeingTaskManager.UpdateEnergyPatterns;
var
  checkIns: TWellbeingCheckInArray;
  i, hour: Integer;
  energyValue: Double;
begin
  checkIns := GetRecentCheckIns(30);
  
  for i := 0 to High(checkIns) do
  begin
    hour := HourOf(checkIns[i].CheckInTime);
    energyValue := Ord(checkIns[i].EnergyLevel);
    
    if FEnergyPatterns[hour].SampleCount = 0 then
      FEnergyPatterns[hour].AverageEnergyLevel := energyValue
    else
      FEnergyPatterns[hour].AverageEnergyLevel :=
        (FEnergyPatterns[hour].AverageEnergyLevel * FEnergyPatterns[hour].SampleCount + energyValue) /
        (FEnergyPatterns[hour].SampleCount + 1);
    
    Inc(FEnergyPatterns[hour].SampleCount);
    
    // Recommend task types based on energy
    if FEnergyPatterns[hour].AverageEnergyLevel >= 3.5 then
      FEnergyPatterns[hour].RecommendedTaskType := 'creative'
    else if FEnergyPatterns[hour].AverageEnergyLevel >= 2.5 then
      FEnergyPatterns[hour].RecommendedTaskType := 'meetings'
    else
      FEnergyPatterns[hour].RecommendedTaskType := 'administrative';
  end;
end;

function TWellbeingTaskManager.GetEnergyPatterns: TEnergyPatternArray;
begin
  Result := FEnergyPatterns;
end;

function TWellbeingTaskManager.GetOptimalTimeForTask(const ATaskType: string): Integer;
var
  i, bestHour: Integer;
  bestEnergy: Double;
begin
  bestHour := 9; // Default
  bestEnergy := 0;
  
  for i := FSettings.WorkdayStartHour to FSettings.WorkdayEndHour do
  begin
    if (FEnergyPatterns[i].RecommendedTaskType = ATaskType) and
       (FEnergyPatterns[i].AverageEnergyLevel > bestEnergy) then
    begin
      bestEnergy := FEnergyPatterns[i].AverageEnergyLevel;
      bestHour := i;
    end;
  end;
  
  Result := bestHour;
end;

procedure TWellbeingTaskManager.GenerateWellnessRecommendations;
var
  rec: TWellnessRecommendation;
  stress: Double;
begin
  stress := GetAverageStressLevel(7);
  
  if stress >= 3.0 then
  begin
    rec.ID := FNextRecommendationID;
    Inc(FNextRecommendationID);
    rec.Category := 'stress';
    rec.Priority := 5;
    rec.Title := 'High Stress Detected';
    rec.Description := 'Your stress levels have been elevated this week.';
    SetLength(rec.ActionSteps, 3);
    rec.ActionSteps[0] := 'Take regular breaks throughout the day';
    rec.ActionSteps[1] := 'Practice deep breathing exercises';
    rec.ActionSteps[2] := 'Consider delegating some tasks';
    rec.ExpectedBenefit := 'Reduced stress and improved focus';
    rec.CreatedDate := Now;
    rec.IsApplied := False;
    
    SetLength(FRecommendations, Length(FRecommendations) + 1);
    FRecommendations[High(FRecommendations)] := rec;
  end;
end;

function TWellbeingTaskManager.GetWellnessRecommendations: TWellnessRecommendationArray;
begin
  Result := FRecommendations;
end;

function TWellbeingTaskManager.GetPriorityRecommendations: TWellnessRecommendationArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FRecommendations) do
    if (FRecommendations[i].Priority >= 4) and (not FRecommendations[i].IsApplied) then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  
  for i := 0 to High(FRecommendations) do
    if (FRecommendations[i].Priority >= 4) and (not FRecommendations[i].IsApplied) then
    begin
      Result[count] := FRecommendations[i];
      Inc(count);
    end;
end;

function TWellbeingTaskManager.GenerateWellbeingReport: string;
var
  stress, mood, energy: Double;
  burnout: TBurnoutRisk;
  breakRate: Double;
begin
  Result := 'WELLBEING REPORT' + sLineBreak + sLineBreak;
  
  stress := GetAverageStressLevel(7);
  mood := GetAverageMoodLevel(7);
  energy := GetAverageEnergyLevel(7);
  burnout := AssessBurnoutRisk;
  breakRate := GetBreakComplianceRate(7);
  
  Result := Result + 'Past 7 Days Summary:' + sLineBreak;
  Result := Result + Format('  Average Stress: %.1f/4', [stress]) + sLineBreak;
  Result := Result + Format('  Average Mood: %.1f/4', [mood]) + sLineBreak;
  Result := Result + Format('  Average Energy: %.1f/4', [energy]) + sLineBreak;
  Result := Result + '  Burnout Risk: ' + BurnoutRiskToString(burnout) + sLineBreak;
  Result := Result + Format('  Break Compliance: %.0f%%', [breakRate]) + sLineBreak;
  Result := Result + sLineBreak;
  
  Result := Result + 'Recommendations: ' + IntToStr(Length(FRecommendations)) + ' active' + sLineBreak;
end;

function TWellbeingTaskManager.GetMentalHealthScore: Double;
var
  stress, mood, energy, burnoutScore: Double;
begin
  stress := GetAverageStressLevel(7);
  mood := GetAverageMoodLevel(7);
  energy := GetAverageEnergyLevel(7);
  burnoutScore := GetBurnoutScore;
  
  // Calculate overall score (0-100, higher is better)
  Result := ((4 - stress) / 4 * 25) +  // 25 points for low stress
            (mood / 4 * 25) +            // 25 points for good mood
            (energy / 4 * 25) +          // 25 points for high energy
            ((100 - burnoutScore) / 100 * 25); // 25 points for low burnout
end;

function TWellbeingTaskManager.StressLevelToString(ALevel: TStressLevel): string;
begin
  case ALevel of
    slVeryLow: Result := 'Very Low';
    slLow: Result := 'Low';
    slModerate: Result := 'Moderate';
    slHigh: Result := 'High';
    slVeryHigh: Result := 'Very High';
  end;
end;

function TWellbeingTaskManager.EnergyLevelToString(ALevel: TEnergyLevel): string;
begin
  case ALevel of
    elVeryLow: Result := 'Very Low';
    elLow: Result := 'Low';
    elModerate: Result := 'Moderate';
    elHigh: Result := 'High';
    elVeryHigh: Result := 'Very High';
  end;
end;

function TWellbeingTaskManager.MoodLevelToString(ALevel: TMoodLevel): string;
begin
  case ALevel of
    mlVeryNegative: Result := 'Very Negative';
    mlNegative: Result := 'Negative';
    mlNeutral: Result := 'Neutral';
    mlPositive: Result := 'Positive';
    mlVeryPositive: Result := 'Very Positive';
  end;
end;

function TWellbeingTaskManager.BurnoutRiskToString(ARisk: TBurnoutRisk): string;
begin
  case ARisk of
    brNone: Result := 'None';
    brLow: Result := 'Low';
    brModerate: Result := 'Moderate';
    brHigh: Result := 'High';
    brCritical: Result := 'Critical';
  end;
end;

function TWellbeingTaskManager.BreakTypeToString(AType: TBreakType): string;
begin
  case AType of
    btMicroBreak: Result := 'Micro Break';
    btShortBreak: Result := 'Short Break';
    btLongBreak: Result := 'Long Break';
    btMeal: Result := 'Meal Break';
    btExercise: Result := 'Exercise';
    btMeditation: Result := 'Meditation';
  end;
end;

function TWellbeingTaskManager.SaveWellbeingDataToFile(const AFilename: string): Boolean;
begin
  Result := True; // Placeholder - would implement file I/O
end;

function TWellbeingTaskManager.LoadWellbeingDataFromFile(const AFilename: string): Boolean;
begin
  Result := True; // Placeholder - would implement file I/O
end;

// Stub implementations for other methods
function TWellbeingTaskManager.CalculateWorkLifeBalance(AWeek: Integer): TWorkLifeBalance;
begin
  Result.WeekNumber := AWeek;
  Result.BalanceScore := 75.0;
end;

function TWellbeingTaskManager.GetWorkLifeBalance(AWeek: Integer): TWorkLifeBalance;
begin
  Result := CalculateWorkLifeBalance(AWeek);
end;

function TWellbeingTaskManager.GetRecentWorkLifeBalance(AWeeks: Integer): TWorkLifeBalanceArray;
begin
  SetLength(Result, 0);
end;

function TWellbeingTaskManager.GetBalanceTrend: string;
begin
  Result := 'Stable';
end;

function TWellbeingTaskManager.IsWorkingOutsideHours: Boolean;
var
  currentHour: Integer;
begin
  currentHour := HourOf(Now);
  Result := (currentHour < FSettings.WorkdayStartHour) or
            (currentHour >= FSettings.WorkdayEndHour);
end;

function TWellbeingTaskManager.GetOvertimeHours(AWeek: Integer): Double;
begin
  Result := 0;
end;

function TWellbeingTaskManager.GetRecoveryRecommendations: TWellnessRecommendationArray;
begin
  Result := GetPriorityRecommendations;
end;

function TWellbeingTaskManager.RecordEnergyLevel(ALevel: TEnergyLevel; const ANotes: string): Boolean;
begin
  Result := RecordCheckIn(slModerate, ALevel, mlNeutral, 7, 7, ANotes) > 0;
end;

function TWellbeingTaskManager.GetCurrentEnergyLevel: TEnergyLevel;
var
  checkIns: TWellbeingCheckInArray;
begin
  checkIns := GetRecentCheckIns(1);
  if Length(checkIns) > 0 then
    Result := checkIns[High(checkIns)].EnergyLevel
  else
    Result := elModerate;
end;

function TWellbeingTaskManager.SuggestTaskBasedOnEnergy: string;
var
  currentHour: Integer;
begin
  currentHour := HourOf(Now);
  Result := FEnergyPatterns[currentHour].RecommendedTaskType;
end;

function TWellbeingTaskManager.TrackCognitiveLoad(AActiveTasks, AContextSwitches, AMentalDemand: Integer): Boolean;
var
  load: TCognitiveLoad;
begin
  load.Timestamp := Now;
  load.ActiveTasks := AActiveTasks;
  load.ContextSwitches := AContextSwitches;
  load.MentalDemand := AMentalDemand;
  load.LoadScore := (AActiveTasks * 10 + AContextSwitches * 5 + AMentalDemand * 3) / 1.8;
  
  SetLength(FCognitiveLoad, Length(FCognitiveLoad) + 1);
  FCognitiveLoad[High(FCognitiveLoad)] := load;
  
  Result := True;
end;

function TWellbeingTaskManager.GetCurrentCognitiveLoad: Double;
begin
  if Length(FCognitiveLoad) > 0 then
    Result := FCognitiveLoad[High(FCognitiveLoad)].LoadScore
  else
    Result := 0;
end;

function TWellbeingTaskManager.GetCognitiveLoadHistory(AHours: Integer): TCognitiveLoadArray;
var
  i, count: Integer;
  cutoff: TDateTime;
begin
  cutoff := Now - (AHours / 24);
  count := 0;
  
  for i := 0 to High(FCognitiveLoad) do
    if FCognitiveLoad[i].Timestamp >= cutoff then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  
  for i := 0 to High(FCognitiveLoad) do
    if FCognitiveLoad[i].Timestamp >= cutoff then
    begin
      Result[count] := FCognitiveLoad[i];
      Inc(count);
    end;
end;

function TWellbeingTaskManager.IsCognitiveOverload: Boolean;
begin
  Result := GetCurrentCognitiveLoad > 70;
end;

function TWellbeingTaskManager.SuggestLoadReduction: string;
begin
  if IsCognitiveOverload then
    Result := 'Consider reducing parallel tasks and taking a break'
  else
    Result := 'Cognitive load is manageable';
end;

function TWellbeingTaskManager.ApplyRecommendation(ARecommendationID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindRecommendationIndex(ARecommendationID);
  if idx >= 0 then
  begin
    FRecommendations[idx].IsApplied := True;
    Result := True;
  end;
end;

function TWellbeingTaskManager.DismissRecommendation(ARecommendationID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindRecommendationIndex(ARecommendationID);
  if idx >= 0 then
  begin
    for i := idx to High(FRecommendations) - 1 do
      FRecommendations[i] := FRecommendations[i + 1];
    SetLength(FRecommendations, Length(FRecommendations) - 1);
    Result := True;
  end;
end;

procedure TWellbeingTaskManager.ConfigureSettings(AMaxDailyHours, AMaxWeeklyHours: Double;
  AMinBreaks, AMicroBreakInterval, ALongBreakInterval: Integer);
begin
  FSettings.MaxDailyWorkHours := AMaxDailyHours;
  FSettings.MaxWeeklyWorkHours := AMaxWeeklyHours;
  FSettings.MinDailyBreaks := AMinBreaks;
  FSettings.MicroBreakIntervalMinutes := AMicroBreakInterval;
  FSettings.LongBreakIntervalMinutes := ALongBreakInterval;
end;

procedure TWellbeingTaskManager.SetWorkingHours(AStartHour, AEndHour: Integer);
begin
  FSettings.WorkdayStartHour := AStartHour;
  FSettings.WorkdayEndHour := AEndHour;
end;

procedure TWellbeingTaskManager.EnableFeature(const AFeature: string; AEnabled: Boolean);
begin
  if AFeature = 'burnout_alerts' then
    FSettings.EnableBurnoutAlerts := AEnabled
  else if AFeature = 'break_reminders' then
    FSettings.EnableBreakReminders := AEnabled;
end;

function TWellbeingTaskManager.GetSettings: TWellbeingSettings;
begin
  Result := FSettings;
end;

function TWellbeingTaskManager.GetStressTriggers: string;
begin
  Result := 'Stress triggers analysis would be implemented here';
end;

function TWellbeingTaskManager.GetEnergyInsights: string;
var
  i: Integer;
  bestHour, worstHour: Integer;
  bestEnergy, worstEnergy: Double;
begin
  Result := 'ENERGY INSIGHTS' + sLineBreak + sLineBreak;
  
  bestEnergy := 0;
  worstEnergy := 5;
  bestHour := 9;
  worstHour := 15;
  
  for i := FSettings.WorkdayStartHour to FSettings.WorkdayEndHour do
  begin
    if FEnergyPatterns[i].AverageEnergyLevel > bestEnergy then
    begin
      bestEnergy := FEnergyPatterns[i].AverageEnergyLevel;
      bestHour := i;
    end;
    if FEnergyPatterns[i].AverageEnergyLevel < worstEnergy then
    begin
      worstEnergy := FEnergyPatterns[i].AverageEnergyLevel;
      worstHour := i;
    end;
  end;
  
  Result := Result + Format('Peak energy time: %d:00 (Energy: %.1f)', [bestHour, bestEnergy]) + sLineBreak;
  Result := Result + Format('Lowest energy time: %d:00 (Energy: %.1f)', [worstHour, worstEnergy]) + sLineBreak;
end;

function TWellbeingTaskManager.GetWellnessSummary: string;
begin
  Result := GenerateWellbeingReport;
end;

function TWellbeingTaskManager.CalculateCognitiveLoad: Double;
begin
  Result := GetCurrentCognitiveLoad;
end;

end.
