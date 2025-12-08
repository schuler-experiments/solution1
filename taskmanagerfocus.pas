
unit taskmanagerfocus;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math, Classes,
  taskmanager, taskmanageradvanced;

type
  // Focus session types
  TFocusType = (ftDeepWork, ftShallowWork, ftAdministrative, ftMeeting, 
                ftCreative, ftLearning, ftCommunication);
  
  // Distraction types
  TDistractionType = (dtNotification, dtInterruption, dtContextSwitch, 
                      dtNoise, dtTechnical, dtPersonal, dtOther);
  
  // Flow state indicators
  TFlowState = (fsNoFlow, fsLowFlow, fsModerateFlow, fsHighFlow, fsPeakFlow);
  
  // Focus quality assessment
  TFocusQuality = (fqPoor, fqFair, fqGood, fqExcellent);
  
  // Pomodoro session record
  TPomodoroSession = record
    ID: Integer;
    TaskID: Integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
    PlannedMinutes: Integer;
    ActualMinutes: Integer;
    Completed: Boolean;
    InterruptionCount: Integer;
    FocusQuality: TFocusQuality;
    Notes: string;
    Created: TDateTime;
  end;
  
  TPomodoroSessionArray = array of TPomodoroSession;
  
  // Focus session with detailed tracking
  TFocusSession = record
    ID: Integer;
    TaskID: Integer;
    FocusType: TFocusType;
    StartTime: TDateTime;
    EndTime: TDateTime;
    DurationMinutes: Integer;
    FlowState: TFlowState;
    FlowStateScore: Integer; // 0-100
    DistractionCount: Integer;
    ContextSwitchCount: Integer;
    ProductivityRating: Integer; // 1-10
    EnergyBefore: Integer; // 1-10
    EnergyAfter: Integer; // 1-10
    Notes: string;
    Completed: Boolean;
    Created: TDateTime;
  end;
  
  TFocusSessionArray = array of TFocusSession;
  
  // Distraction log entry
  TDistraction = record
    ID: Integer;
    SessionID: Integer;
    TaskID: Integer;
    DistractType: TDistractionType;
    OccurredAt: TDateTime;
    DurationSeconds: Integer;
    Source: string;
    ImpactScore: Integer; // 1-10
    WasAvoidable: Boolean;
    Notes: string;
  end;
  
  TDistractionArray = array of TDistraction;
  
  // Context switch record
  TContextSwitch = record
    ID: Integer;
    FromTaskID: Integer;
    ToTaskID: Integer;
    SwitchTime: TDateTime;
    RecoveryTimeMinutes: Integer;
    CostScore: Integer; // 1-10, cost of switching
    WasPlanned: Boolean;
    Reason: string;
  end;
  
  TContextSwitchArray = array of TContextSwitch;
  
  // Deep work block - protected time
  TDeepWorkBlock = record
    ID: Integer;
    Title: string;
    StartTime: TDateTime;
    EndTime: TDateTime;
    TaskIDs: array of Integer;
    ProtectionLevel: Integer; // 1-10, how strictly to protect
    ActualFocusMinutes: Integer;
    InterruptionsAllowed: Integer;
    ActualInterruptions: Integer;
    Success: Boolean;
    Notes: string;
  end;
  
  TDeepWorkBlockArray = array of TDeepWorkBlock;
  
  // Focus statistics
  TFocusStats = record
    TotalFocusMinutes: Integer;
    DeepWorkMinutes: Integer;
    ShallowWorkMinutes: Integer;
    AverageFlowScore: Double;
    AverageProductivity: Double;
    TotalDistractions: Integer;
    TotalContextSwitches: Integer;
    AverageRecoveryTime: Double;
    FocusEfficiency: Double; // % of time in actual focus
    BestFocusTime: Integer; // Hour of day
    WorstFocusTime: Integer;
  end;

  // Flow state pattern
  TFlowPattern = record
    TimeOfDay: Integer; // Hour 0-23
    FocusType: TFocusType;
    AverageFlowScore: Double;
    SuccessRate: Double;
    SampleCount: Integer;
  end;
  
  TFlowPatternArray = array of TFlowPattern;

  TFocusTaskManager = class(TAdvancedTaskManager)
  private
    FPomodoroSessions: TPomodoroSessionArray;
    FFocusSessions: TFocusSessionArray;
    FDistractions: TDistractionArray;
    FContextSwitches: TContextSwitchArray;
    FDeepWorkBlocks: TDeepWorkBlockArray;
    FNextPomodoroID: Integer;
    FNextFocusID: Integer;
    FNextDistractionID: Integer;
    FNextSwitchID: Integer;
    FNextBlockID: Integer;
    FCurrentPomodoroID: Integer;
    FCurrentFocusID: Integer;
    FCurrentBlockID: Integer;
    
    // Default settings
    FPomodoroLength: Integer; // Default 25 minutes
    FShortBreakLength: Integer; // Default 5 minutes
    FLongBreakLength: Integer; // Default 15 minutes
    FPomodorosUntilLongBreak: Integer; // Default 4
    FFlowThreshold: Integer; // Minimum minutes for flow state
    
    function FindPomodoroIndex(AID: Integer): Integer;
    function FindFocusIndex(AID: Integer): Integer;
    function FindDistractionIndex(AID: Integer): Integer;
    function FindSwitchIndex(AID: Integer): Integer;
    function FindBlockIndex(AID: Integer): Integer;
    function CalculateFlowScore(const ASession: TFocusSession): Integer;
    function AnalyzeFlowState(AFocusMinutes, ADistractions, ASwitches: Integer): TFlowState;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Pomodoro timer management
    function StartPomodoro(ATaskID, AMinutes: Integer): Integer;
    function CompletePomodoro(APomodoroID: Integer; AQuality: TFocusQuality; 
      const ANotes: string): Boolean;
    function AbandonPomodoro(APomodoroID: Integer; const AReason: string): Boolean;
    function GetCurrentPomodoro: TPomodoroSession;
    function GetPomodoroHistory(ADays: Integer): TPomodoroSessionArray;
    function GetPomodoroStats(ADays: Integer): string;
    
    // Focus session management
    function StartFocusSession(ATaskID: Integer; AFocusType: TFocusType; 
      AEnergyBefore: Integer): Integer;
    function EndFocusSession(ASessionID, AProductivity, AEnergyAfter: Integer; 
      const ANotes: string): Boolean;
    function GetCurrentFocusSession: TFocusSession;
    function GetFocusSessions(ADays: Integer): TFocusSessionArray;
    function GetFocusStats(ADays: Integer): TFocusStats;
    
    // Distraction tracking
    function LogDistraction(ASessionID, ATaskID: Integer; AType: TDistractionType;
      const ASource: string; AImpact: Integer; AAvoidable: Boolean): Integer;
    function GetDistractions(ASessionID: Integer): TDistractionArray;
    function GetDistractionStats(ADays: Integer): string;
    function GetMostCommonDistractions: string;
    function GetAvoidableDistractionRate(ADays: Integer): Double;
    
    // Context switching analysis
    function LogContextSwitch(AFromTask, AToTask: Integer; 
      const AReason: string; APlanned: Boolean): Integer;
    function UpdateSwitchRecovery(ASwitchID, AMinutes, ACost: Integer): Boolean;
    function GetContextSwitches(ADays: Integer): TContextSwitchArray;
    function GetSwitchingCost(ADays: Integer): string;
    function GetAverageSwitchCost: Double;
    
    // Deep work block management
    function ScheduleDeepWorkBlock(const ATitle: string; AStart, AEnd: TDateTime;
      AProtectionLevel: Integer): Integer;
    function AddTaskToBlock(ABlockID, ATaskID: Integer): Boolean;
    function StartDeepWorkBlock(ABlockID: Integer): Boolean;
    function EndDeepWorkBlock(ABlockID: Integer; const ANotes: string): Boolean;
    function GetUpcomingBlocks: TDeepWorkBlockArray;
    function GetBlockEffectiveness: string;
    
    // Flow state analysis
    function IdentifyFlowPatterns: TFlowPatternArray;
    function GetBestTimeForDeepWork: Integer;
    function GetFlowStateRecommendations: string;
    function PredictFlowPotential(ATaskID, AHour: Integer): Double;
    
    // Focus optimization
    function GetFocusEfficiency(ADays: Integer): Double;
    function GetDeepWorkRatio(ADays: Integer): Double;
    function GetInterruptionImpact(ADays: Integer): string;
    function SuggestFocusImprovements: string;
    
    // Reporting
    function GenerateFocusReport(ADays: Integer): string;
    function GetProductivityByTimeOfDay: string;
    function GetEnergyCorrelation: string;
    
    // Configuration
    procedure SetPomodoroSettings(AWorkMinutes, AShortBreak, ALongBreak, ACycleCount: Integer);
    procedure SetFlowThreshold(AMinutes: Integer);
    function GetSettings: string;
    
    // Conversion helpers
    function FocusTypeToString(AFType: TFocusType): string;
    function FlowStateToString(AState: TFlowState): string;
    function FocusQualityToString(AQuality: TFocusQuality): string;
    function DistractionTypeToString(AType: TDistractionType): string;
    
    // Persistence
    function SaveFocusDataToFile(const AFilename: string): Boolean;
    function LoadFocusDataFromFile(const AFilename: string): Boolean;
  end;

implementation

{ TFocusTaskManager }

constructor TFocusTaskManager.Create;
begin
  inherited Create;
  SetLength(FPomodoroSessions, 0);
  SetLength(FFocusSessions, 0);
  SetLength(FDistractions, 0);
  SetLength(FContextSwitches, 0);
  SetLength(FDeepWorkBlocks, 0);
  FNextPomodoroID := 1;
  FNextFocusID := 1;
  FNextDistractionID := 1;
  FNextSwitchID := 1;
  FNextBlockID := 1;
  FCurrentPomodoroID := -1;
  FCurrentFocusID := -1;
  FCurrentBlockID := -1;
  
  // Default Pomodoro settings
  FPomodoroLength := 25;
  FShortBreakLength := 5;
  FLongBreakLength := 15;
  FPomodorosUntilLongBreak := 4;
  FFlowThreshold := 20; // Need 20+ minutes for flow state
end;

destructor TFocusTaskManager.Destroy;
begin
  SetLength(FPomodoroSessions, 0);
  SetLength(FFocusSessions, 0);
  SetLength(FDistractions, 0);
  SetLength(FContextSwitches, 0);
  SetLength(FDeepWorkBlocks, 0);
  inherited Destroy;
end;

function TFocusTaskManager.FindPomodoroIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FPomodoroSessions) do
    if FPomodoroSessions[i].ID = AID then
      Exit(i);
end;

function TFocusTaskManager.FindFocusIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FFocusSessions) do
    if FFocusSessions[i].ID = AID then
      Exit(i);
end;

function TFocusTaskManager.FindDistractionIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FDistractions) do
    if FDistractions[i].ID = AID then
      Exit(i);
end;

function TFocusTaskManager.FindSwitchIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FContextSwitches) do
    if FContextSwitches[i].ID = AID then
      Exit(i);
end;

function TFocusTaskManager.FindBlockIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FDeepWorkBlocks) do
    if FDeepWorkBlocks[i].ID = AID then
      Exit(i);
end;

function TFocusTaskManager.CalculateFlowScore(const ASession: TFocusSession): Integer;
var
  score: Double;
begin
  // Calculate flow score based on multiple factors
  score := 50.0; // Base score
  
  // Duration bonus (longer sessions more likely to achieve flow)
  if ASession.DurationMinutes >= 60 then
    score := score + 20
  else if ASession.DurationMinutes >= 30 then
    score := score + 10
  else if ASession.DurationMinutes >= FFlowThreshold then
    score := score + 5;
  
  // Distraction penalty
  score := score - (ASession.DistractionCount * 5);
  
  // Context switch penalty (heavier)
  score := score - (ASession.ContextSwitchCount * 10);
  
  // Productivity bonus
  if ASession.ProductivityRating > 0 then
    score := score + (ASession.ProductivityRating * 2);
  
  // Energy correlation
  if (ASession.EnergyBefore > 0) and (ASession.EnergyAfter > 0) then
  begin
    if ASession.EnergyBefore >= 7 then
      score := score + 10;
  end;
  
  // Clamp to 0-100
  if score < 0 then score := 0;
  if score > 100 then score := 100;
  
  Result := Round(score);
end;

function TFocusTaskManager.AnalyzeFlowState(AFocusMinutes, ADistractions, 
  ASwitches: Integer): TFlowState;
var
  flowScore: Integer;
begin
  flowScore := 50;
  
  if AFocusMinutes >= 60 then
    flowScore := flowScore + 30
  else if AFocusMinutes >= 30 then
    flowScore := flowScore + 20
  else if AFocusMinutes >= FFlowThreshold then
    flowScore := flowScore + 10
  else
    flowScore := flowScore - 20;
  
  flowScore := flowScore - (ADistractions * 8);
  flowScore := flowScore - (ASwitches * 15);
  
  if flowScore >= 80 then
    Result := fsPeakFlow
  else if flowScore >= 60 then
    Result := fsHighFlow
  else if flowScore >= 40 then
    Result := fsModerateFlow
  else if flowScore >= 20 then
    Result := fsLowFlow
  else
    Result := fsNoFlow;
end;

function TFocusTaskManager.StartPomodoro(ATaskID, AMinutes: Integer): Integer;
var
  idx: Integer;
begin
  if FCurrentPomodoroID <> -1 then
    Exit(-1); // Already have active pomodoro
  
  idx := Length(FPomodoroSessions);
  SetLength(FPomodoroSessions, idx + 1);
  
  with FPomodoroSessions[idx] do
  begin
    ID := FNextPomodoroID;
    TaskID := ATaskID;
    StartTime := Now;
    EndTime := 0;
    PlannedMinutes := AMinutes;
    ActualMinutes := 0;
    Completed := False;
    InterruptionCount := 0;
    FocusQuality := fqFair;
    Notes := '';
    Created := Now;
  end;
  
  FCurrentPomodoroID := FNextPomodoroID;
  Result := FNextPomodoroID;
  Inc(FNextPomodoroID);
end;

function TFocusTaskManager.CompletePomodoro(APomodoroID: Integer; 
  AQuality: TFocusQuality; const ANotes: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindPomodoroIndex(APomodoroID);
  if idx = -1 then Exit;
  
  with FPomodoroSessions[idx] do
  begin
    EndTime := Now;
    ActualMinutes := MinutesBetween(EndTime, StartTime);
    Completed := True;
    FocusQuality := AQuality;
    Notes := ANotes;
  end;
  
  if FCurrentPomodoroID = APomodoroID then
    FCurrentPomodoroID := -1;
  
  Result := True;
end;

function TFocusTaskManager.AbandonPomodoro(APomodoroID: Integer; 
  const AReason: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindPomodoroIndex(APomodoroID);
  if idx = -1 then Exit;
  
  with FPomodoroSessions[idx] do
  begin
    EndTime := Now;
    ActualMinutes := MinutesBetween(EndTime, StartTime);
    Completed := False;
    Notes := 'Abandoned: ' + AReason;
  end;
  
  if FCurrentPomodoroID = APomodoroID then
    FCurrentPomodoroID := -1;
  
  Result := True;
end;

function TFocusTaskManager.GetCurrentPomodoro: TPomodoroSession;
var
  idx: Integer;
  emptySession: TPomodoroSession;
begin
  if FCurrentPomodoroID = -1 then
  begin
    FillChar(emptySession, SizeOf(TPomodoroSession), 0);
    emptySession.ID := -1;
    Exit(emptySession);
  end;
  
  idx := FindPomodoroIndex(FCurrentPomodoroID);
  if idx <> -1 then
    Result := FPomodoroSessions[idx]
  else
  begin
    FillChar(emptySession, SizeOf(TPomodoroSession), 0);
    emptySession.ID := -1;
    Result := emptySession;
  end;
end;

function TFocusTaskManager.GetPomodoroHistory(ADays: Integer): TPomodoroSessionArray;
var
  i, count: Integer;
  cutoffDate: TDateTime;
begin
  cutoffDate := Now - ADays;
  count := 0;
  
  for i := 0 to High(FPomodoroSessions) do
    if FPomodoroSessions[i].StartTime >= cutoffDate then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  
  for i := 0 to High(FPomodoroSessions) do
    if FPomodoroSessions[i].StartTime >= cutoffDate then
    begin
      Result[count] := FPomodoroSessions[i];
      Inc(count);
    end;
end;

function TFocusTaskManager.GetPomodoroStats(ADays: Integer): string;
var
  sessions: TPomodoroSessionArray;
  i, completed, total, totalMinutes: Integer;
  avgMinutes: Double;
begin
  sessions := GetPomodoroHistory(ADays);
  total := Length(sessions);
  completed := 0;
  totalMinutes := 0;
  
  for i := 0 to High(sessions) do
  begin
    if sessions[i].Completed then
      Inc(completed);
    totalMinutes := totalMinutes + sessions[i].ActualMinutes;
  end;
  
  if total > 0 then
    avgMinutes := totalMinutes / total
  else
    avgMinutes := 0;
  
  Result := Format('Pomodoro Stats (Past %d days):'#13#10 +
                   'Total sessions: %d'#13#10 +
                   'Completed: %d (%.1f%%)'#13#10 +
                   'Total focus time: %d minutes'#13#10 +
                   'Average session: %.1f minutes',
                   [ADays, total, completed, 
                    (completed / Max(1, total)) * 100,
                    totalMinutes, avgMinutes]);
end;

function TFocusTaskManager.StartFocusSession(ATaskID: Integer; 
  AFocusType: TFocusType; AEnergyBefore: Integer): Integer;
var
  idx: Integer;
begin
  idx := Length(FFocusSessions);
  SetLength(FFocusSessions, idx + 1);
  
  with FFocusSessions[idx] do
  begin
    ID := FNextFocusID;
    TaskID := ATaskID;
    FocusType := AFocusType;
    StartTime := Now;
    EndTime := 0;
    DurationMinutes := 0;
    FlowState := fsNoFlow;
    FlowStateScore := 0;
    DistractionCount := 0;
    ContextSwitchCount := 0;
    ProductivityRating := 0;
    EnergyBefore := AEnergyBefore;
    EnergyAfter := 0;
    Notes := '';
    Completed := False;
    Created := Now;
  end;
  
  FCurrentFocusID := FNextFocusID;
  Result := FNextFocusID;
  Inc(FNextFocusID);
end;

function TFocusTaskManager.EndFocusSession(ASessionID, AProductivity, 
  AEnergyAfter: Integer; const ANotes: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindFocusIndex(ASessionID);
  if idx = -1 then Exit;
  
  with FFocusSessions[idx] do
  begin
    EndTime := Now;
    DurationMinutes := MinutesBetween(EndTime, StartTime);
    ProductivityRating := AProductivity;
    EnergyAfter := AEnergyAfter;
    Notes := ANotes;
    Completed := True;
    
    // Calculate flow state
    FlowState := AnalyzeFlowState(DurationMinutes, DistractionCount, ContextSwitchCount);
    FlowStateScore := CalculateFlowScore(FFocusSessions[idx]);
  end;
  
  if FCurrentFocusID = ASessionID then
    FCurrentFocusID := -1;
  
  Result := True;
end;

function TFocusTaskManager.GetCurrentFocusSession: TFocusSession;
var
  idx: Integer;
  emptySession: TFocusSession;
begin
  if FCurrentFocusID = -1 then
  begin
    FillChar(emptySession, SizeOf(TFocusSession), 0);
    emptySession.ID := -1;
    Exit(emptySession);
  end;
  
  idx := FindFocusIndex(FCurrentFocusID);
  if idx <> -1 then
    Result := FFocusSessions[idx]
  else
  begin
    FillChar(emptySession, SizeOf(TFocusSession), 0);
    emptySession.ID := -1;
    Result := emptySession;
  end;
end;

function TFocusTaskManager.GetFocusSessions(ADays: Integer): TFocusSessionArray;
var
  i, count: Integer;
  cutoffDate: TDateTime;
begin
  cutoffDate := Now - ADays;
  count := 0;
  
  for i := 0 to High(FFocusSessions) do
    if FFocusSessions[i].StartTime >= cutoffDate then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  
  for i := 0 to High(FFocusSessions) do
    if FFocusSessions[i].StartTime >= cutoffDate then
    begin
      Result[count] := FFocusSessions[i];
      Inc(count);
    end;
end;

function TFocusTaskManager.GetFocusStats(ADays: Integer): TFocusStats;
var
  sessions: TFocusSessionArray;
  i, deepWork, shallowWork: Integer;
  totalFlow, totalProd, totalRecovery: Double;
  recoveryCount: Integer;
begin
  FillChar(Result, SizeOf(TFocusStats), 0);
  sessions := GetFocusSessions(ADays);
  
  if Length(sessions) = 0 then Exit;
  
  deepWork := 0;
  shallowWork := 0;
  totalFlow := 0;
  totalProd := 0;
  
  for i := 0 to High(sessions) do
  begin
    Result.TotalFocusMinutes := Result.TotalFocusMinutes + sessions[i].DurationMinutes;
    
    if sessions[i].FocusType in [ftDeepWork, ftCreative, ftLearning] then
      deepWork := deepWork + sessions[i].DurationMinutes
    else
      shallowWork := shallowWork + sessions[i].DurationMinutes;
    
    totalFlow := totalFlow + sessions[i].FlowStateScore;
    totalProd := totalProd + sessions[i].ProductivityRating;
    Result.TotalDistractions := Result.TotalDistractions + sessions[i].DistractionCount;
    Result.TotalContextSwitches := Result.TotalContextSwitches + sessions[i].ContextSwitchCount;
  end;
  
  Result.DeepWorkMinutes := deepWork;
  Result.ShallowWorkMinutes := shallowWork;
  Result.AverageFlowScore := totalFlow / Length(sessions);
  Result.AverageProductivity := totalProd / Length(sessions);
  
  // Calculate average recovery time from context switches
  recoveryCount := 0;
  totalRecovery := 0;
  for i := 0 to High(FContextSwitches) do
  begin
    if FContextSwitches[i].SwitchTime >= (Now - ADays) then
    begin
      totalRecovery := totalRecovery + FContextSwitches[i].RecoveryTimeMinutes;
      Inc(recoveryCount);
    end;
  end;
  
  if recoveryCount > 0 then
    Result.AverageRecoveryTime := totalRecovery / recoveryCount;
  
  if Result.TotalFocusMinutes > 0 then
    Result.FocusEfficiency := (Result.TotalFocusMinutes - (Result.TotalDistractions * 2)) / 
                              Result.TotalFocusMinutes * 100;
end;

function TFocusTaskManager.LogDistraction(ASessionID, ATaskID: Integer; 
  AType: TDistractionType; const ASource: string; AImpact: Integer; 
  AAvoidable: Boolean): Integer;
var
  idx, sessionIdx: Integer;
begin
  idx := Length(FDistractions);
  SetLength(FDistractions, idx + 1);
  
  with FDistractions[idx] do
  begin
    ID := FNextDistractionID;
    SessionID := ASessionID;
    TaskID := ATaskID;
    DistractType := AType;
    OccurredAt := Now;
    DurationSeconds := 0;
    Source := ASource;
    ImpactScore := AImpact;
    WasAvoidable := AAvoidable;
    Notes := '';
  end;
  
  // Update session distraction count
  sessionIdx := FindFocusIndex(ASessionID);
  if sessionIdx <> -1 then
    Inc(FFocusSessions[sessionIdx].DistractionCount);
  
  Result := FNextDistractionID;
  Inc(FNextDistractionID);
end;

function TFocusTaskManager.GetDistractions(ASessionID: Integer): TDistractionArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FDistractions) do
    if FDistractions[i].SessionID = ASessionID then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  
  for i := 0 to High(FDistractions) do
    if FDistractions[i].SessionID = ASessionID then
    begin
      Result[count] := FDistractions[i];
      Inc(count);
    end;
end;

function TFocusTaskManager.GetDistractionStats(ADays: Integer): string;
var
  i, total: Integer;
  cutoffDate: TDateTime;
  totalImpact: Double;
begin
  cutoffDate := Now - ADays;
  total := 0;
  totalImpact := 0;
  
  for i := 0 to High(FDistractions) do
    if FDistractions[i].OccurredAt >= cutoffDate then
    begin
      Inc(total);
      totalImpact := totalImpact + FDistractions[i].ImpactScore;
    end;
  
  Result := Format('Distraction Stats (Past %d days):'#13#10 +
                   'Total distractions: %d'#13#10 +
                   'Average per day: %.1f'#13#10 +
                   'Average impact: %.1f/10',
                   [ADays, total, total / Max(1, ADays),
                    totalImpact / Max(1, total)]);
end;

function TFocusTaskManager.GetMostCommonDistractions: string;
var
  i: Integer;
  typeCounts: array[TDistractionType] of Integer;
  dtype: TDistractionType;
  maxCount: Integer;
  mostCommon: TDistractionType;
begin
  for dtype := Low(TDistractionType) to High(TDistractionType) do
    typeCounts[dtype] := 0;
  
  for i := 0 to High(FDistractions) do
    Inc(typeCounts[FDistractions[i].DistractType]);
  
  maxCount := 0;
  mostCommon := dtOther;
  for dtype := Low(TDistractionType) to High(TDistractionType) do
    if typeCounts[dtype] > maxCount then
    begin
      maxCount := typeCounts[dtype];
      mostCommon := dtype;
    end;
  
  Result := Format('Most common: %s (%d occurrences)', 
                   [DistractionTypeToString(mostCommon), maxCount]);
end;

function TFocusTaskManager.GetAvoidableDistractionRate(ADays: Integer): Double;
var
  i, total, avoidable: Integer;
  cutoffDate: TDateTime;
begin
  cutoffDate := Now - ADays;
  total := 0;
  avoidable := 0;
  
  for i := 0 to High(FDistractions) do
    if FDistractions[i].OccurredAt >= cutoffDate then
    begin
      Inc(total);
      if FDistractions[i].WasAvoidable then
        Inc(avoidable);
    end;
  
  if total > 0 then
    Result := (avoidable / total) * 100
  else
    Result := 0;
end;

function TFocusTaskManager.LogContextSwitch(AFromTask, AToTask: Integer; 
  const AReason: string; APlanned: Boolean): Integer;
var
  idx, sessionIdx: Integer;
begin
  idx := Length(FContextSwitches);
  SetLength(FContextSwitches, idx + 1);
  
  with FContextSwitches[idx] do
  begin
    ID := FNextSwitchID;
    FromTaskID := AFromTask;
    ToTaskID := AToTask;
    SwitchTime := Now;
    RecoveryTimeMinutes := 0;
    CostScore := 5; // Default medium cost
    WasPlanned := APlanned;
    Reason := AReason;
  end;
  
  // Update current session context switch count
  if FCurrentFocusID <> -1 then
  begin
    sessionIdx := FindFocusIndex(FCurrentFocusID);
    if sessionIdx <> -1 then
      Inc(FFocusSessions[sessionIdx].ContextSwitchCount);
  end;
  
  Result := FNextSwitchID;
  Inc(FNextSwitchID);
end;

function TFocusTaskManager.UpdateSwitchRecovery(ASwitchID, AMinutes, 
  ACost: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindSwitchIndex(ASwitchID);
  if idx = -1 then Exit;
  
  FContextSwitches[idx].RecoveryTimeMinutes := AMinutes;
  FContextSwitches[idx].CostScore := ACost;
  Result := True;
end;

function TFocusTaskManager.GetContextSwitches(ADays: Integer): TContextSwitchArray;
var
  i, count: Integer;
  cutoffDate: TDateTime;
begin
  cutoffDate := Now - ADays;
  count := 0;
  
  for i := 0 to High(FContextSwitches) do
    if FContextSwitches[i].SwitchTime >= cutoffDate then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  
  for i := 0 to High(FContextSwitches) do
    if FContextSwitches[i].SwitchTime >= cutoffDate then
    begin
      Result[count] := FContextSwitches[i];
      Inc(count);
    end;
end;

function TFocusTaskManager.GetSwitchingCost(ADays: Integer): string;
var
  switches: TContextSwitchArray;
  i, totalSwitches, plannedSwitches: Integer;
  totalRecovery, totalCost: Double;
begin
  switches := GetContextSwitches(ADays);
  totalSwitches := Length(switches);
  plannedSwitches := 0;
  totalRecovery := 0;
  totalCost := 0;
  
  for i := 0 to High(switches) do
  begin
    if switches[i].WasPlanned then
      Inc(plannedSwitches);
    totalRecovery := totalRecovery + switches[i].RecoveryTimeMinutes;
    totalCost := totalCost + switches[i].CostScore;
  end;
  
  Result := Format('Context Switch Analysis (Past %d days):'#13#10 +
                   'Total switches: %d'#13#10 +
                   'Planned: %d (%.1f%%)'#13#10 +
                   'Avg recovery time: %.1f minutes'#13#10 +
                   'Avg cost score: %.1f/10'#13#10 +
                   'Total lost time: %.0f minutes',
                   [ADays, totalSwitches, plannedSwitches,
                    (plannedSwitches / Max(1, totalSwitches)) * 100,
                    totalRecovery / Max(1, totalSwitches),
                    totalCost / Max(1, totalSwitches),
                    totalRecovery]);
end;

function TFocusTaskManager.GetAverageSwitchCost: Double;
var
  i: Integer;
  total: Double;
begin
  if Length(FContextSwitches) = 0 then
    Exit(0);
  
  total := 0;
  for i := 0 to High(FContextSwitches) do
    total := total + FContextSwitches[i].RecoveryTimeMinutes;
  
  Result := total / Length(FContextSwitches);
end;

function TFocusTaskManager.ScheduleDeepWorkBlock(const ATitle: string; 
  AStart, AEnd: TDateTime; AProtectionLevel: Integer): Integer;
var
  idx: Integer;
begin
  idx := Length(FDeepWorkBlocks);
  SetLength(FDeepWorkBlocks, idx + 1);
  
  with FDeepWorkBlocks[idx] do
  begin
    ID := FNextBlockID;
    Title := ATitle;
    StartTime := AStart;
    EndTime := AEnd;
    SetLength(TaskIDs, 0);
    ProtectionLevel := AProtectionLevel;
    ActualFocusMinutes := 0;
    InterruptionsAllowed := 0;
    ActualInterruptions := 0;
    Success := False;
    Notes := '';
  end;
  
  Result := FNextBlockID;
  Inc(FNextBlockID);
end;

function TFocusTaskManager.AddTaskToBlock(ABlockID, ATaskID: Integer): Boolean;
var
  idx, taskIdx: Integer;
begin
  Result := False;
  idx := FindBlockIndex(ABlockID);
  if idx = -1 then Exit;
  
  taskIdx := Length(FDeepWorkBlocks[idx].TaskIDs);
  SetLength(FDeepWorkBlocks[idx].TaskIDs, taskIdx + 1);
  FDeepWorkBlocks[idx].TaskIDs[taskIdx] := ATaskID;
  Result := True;
end;

function TFocusTaskManager.StartDeepWorkBlock(ABlockID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindBlockIndex(ABlockID);
  if idx = -1 then Exit;
  
  FCurrentBlockID := ABlockID;
  Result := True;
end;

function TFocusTaskManager.EndDeepWorkBlock(ABlockID: Integer; 
  const ANotes: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindBlockIndex(ABlockID);
  if idx = -1 then Exit;
  
  with FDeepWorkBlocks[idx] do
  begin
    Notes := ANotes;
    Success := (ActualInterruptions <= InterruptionsAllowed);
  end;
  
  if FCurrentBlockID = ABlockID then
    FCurrentBlockID := -1;
  
  Result := True;
end;

function TFocusTaskManager.GetUpcomingBlocks: TDeepWorkBlockArray;
var
  i, count: Integer;
  now: TDateTime;
begin
  now := Now;
  count := 0;
  
  for i := 0 to High(FDeepWorkBlocks) do
    if FDeepWorkBlocks[i].StartTime > now then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  
  for i := 0 to High(FDeepWorkBlocks) do
    if FDeepWorkBlocks[i].StartTime > now then
    begin
      Result[count] := FDeepWorkBlocks[i];
      Inc(count);
    end;
end;

function TFocusTaskManager.GetBlockEffectiveness: string;
var
  i, total, successful: Integer;
begin
  total := 0;
  successful := 0;
  
  for i := 0 to High(FDeepWorkBlocks) do
    if FDeepWorkBlocks[i].EndTime < Now then
    begin
      Inc(total);
      if FDeepWorkBlocks[i].Success then
        Inc(successful);
    end;
  
  Result := Format('Deep Work Block Success Rate: %.1f%% (%d/%d blocks)',
                   [(successful / Max(1, total)) * 100, successful, total]);
end;

function TFocusTaskManager.IdentifyFlowPatterns: TFlowPatternArray;
begin
  // Simplified version - would analyze sessions by time and type
  SetLength(Result, 0);
end;

function TFocusTaskManager.GetBestTimeForDeepWork: Integer;
var
  i, hour: Integer;
  hourScores: array[0..23] of record
    total: Double;
    count: Integer;
  end;
  bestHour, maxScore: Integer;
begin
  // Initialize
  for hour := 0 to 23 do
  begin
    hourScores[hour].total := 0;
    hourScores[hour].count := 0;
  end;
  
  // Analyze sessions
  for i := 0 to High(FFocusSessions) do
    if FFocusSessions[i].FocusType in [ftDeepWork, ftCreative, ftLearning] then
    begin
      hour := HourOf(FFocusSessions[i].StartTime);
      hourScores[hour].total := hourScores[hour].total + FFocusSessions[i].FlowStateScore;
      Inc(hourScores[hour].count);
    end;
  
  // Find best hour
  bestHour := 9; // Default to 9 AM
  maxScore := 0;
  for hour := 0 to 23 do
    if hourScores[hour].count > 0 then
      if Round(hourScores[hour].total) > maxScore then
      begin
        maxScore := Round(hourScores[hour].total);
        bestHour := hour;
      end;
  
  Result := bestHour;
end;

function TFocusTaskManager.GetFlowStateRecommendations: string;
var
  bestHour: Integer;
begin
  bestHour := GetBestTimeForDeepWork;
  Result := Format('Flow State Recommendations:'#13#10 +
                   '• Best time for deep work: %d:00'#13#10 +
                   '• Minimize distractions during this time'#13#10 +
                   '• Schedule creative tasks during peak hours'#13#10 +
                   '• Reserve low-energy times for admin work',
                   [bestHour]);
end;

function TFocusTaskManager.PredictFlowPotential(ATaskID, AHour: Integer): Double;
begin
  // Simplified - would use historical data
  Result := 0.5; // 50% probability
end;

function TFocusTaskManager.GetFocusEfficiency(ADays: Integer): Double;
var
  stats: TFocusStats;
begin
  stats := GetFocusStats(ADays);
  Result := stats.FocusEfficiency;
end;

function TFocusTaskManager.GetDeepWorkRatio(ADays: Integer): Double;
var
  stats: TFocusStats;
begin
  stats := GetFocusStats(ADays);
  if stats.TotalFocusMinutes > 0 then
    Result := (stats.DeepWorkMinutes / stats.TotalFocusMinutes) * 100
  else
    Result := 0;
end;

function TFocusTaskManager.GetInterruptionImpact(ADays: Integer): string;
var
  stats: TFocusStats;
  lostTime: Double;
begin
  stats := GetFocusStats(ADays);
  lostTime := stats.TotalDistractions * 5 + stats.AverageRecoveryTime * stats.TotalContextSwitches;
  
  Result := Format('Interruption Impact Analysis:'#13#10 +
                   'Total distractions: %d'#13#10 +
                   'Total context switches: %d'#13#10 +
                   'Estimated lost time: %.0f minutes'#13#10 +
                   'That''s %.1f hours of productivity!',
                   [stats.TotalDistractions, stats.TotalContextSwitches,
                    lostTime, lostTime / 60]);
end;

function TFocusTaskManager.SuggestFocusImprovements: string;
var
  avoidableRate: Double;
  deepWorkRatio: Double;
begin
  avoidableRate := GetAvoidableDistractionRate(7);
  deepWorkRatio := GetDeepWorkRatio(7);
  
  Result := 'Focus Improvement Suggestions:'#13#10;
  
  if avoidableRate > 50 then
    Result := Result + '• %.0f%% of distractions are avoidable - turn off notifications!'#13#10;
  
  if deepWorkRatio < 30 then
    Result := Result + '• Only %.0f%% deep work - schedule more focused blocks'#13#10;
  
  Result := Result + '• Use Pomodoro technique for better focus'#13#10 +
                     '• Batch similar tasks to reduce context switching'#13#10 +
                     '• Protect morning hours for deep work';
  
  Result := Format(Result, [avoidableRate, deepWorkRatio]);
end;

function TFocusTaskManager.GenerateFocusReport(ADays: Integer): string;
var
  stats: TFocusStats;
begin
  stats := GetFocusStats(ADays);
  
  Result := Format('=== FOCUS & DEEP WORK REPORT (Past %d days) ==='#13#10#13#10 +
                   'Time Allocation:'#13#10 +
                   '  Total focus time: %d minutes (%.1f hours)'#13#10 +
                   '  Deep work: %d minutes (%.1f%%)'#13#10 +
                   '  Shallow work: %d minutes (%.1f%%)'#13#10#13#10 +
                   'Quality Metrics:'#13#10 +
                   '  Average flow score: %.1f/100'#13#10 +
                   '  Average productivity: %.1f/10'#13#10 +
                   '  Focus efficiency: %.1f%%'#13#10#13#10 +
                   'Interruptions:'#13#10 +
                   '  Total distractions: %d'#13#10 +
                   '  Context switches: %d'#13#10 +
                   '  Avg recovery time: %.1f minutes'#13#10,
                   [ADays,
                    stats.TotalFocusMinutes, stats.TotalFocusMinutes / 60,
                    stats.DeepWorkMinutes, (stats.DeepWorkMinutes / Max(1, stats.TotalFocusMinutes)) * 100,
                    stats.ShallowWorkMinutes, (stats.ShallowWorkMinutes / Max(1, stats.TotalFocusMinutes)) * 100,
                    stats.AverageFlowScore,
                    stats.AverageProductivity,
                    stats.FocusEfficiency,
                    stats.TotalDistractions,
                    stats.TotalContextSwitches,
                    stats.AverageRecoveryTime]);
end;

function TFocusTaskManager.GetProductivityByTimeOfDay: string;
begin
  Result := 'Productivity peaks at ' + IntToStr(GetBestTimeForDeepWork) + ':00';
end;

function TFocusTaskManager.GetEnergyCorrelation: string;
var
  i, highEnergyProd, lowEnergyProd, highCount, lowCount: Integer;
begin
  highEnergyProd := 0;
  lowEnergyProd := 0;
  highCount := 0;
  lowCount := 0;
  
  for i := 0 to High(FFocusSessions) do
  begin
    if FFocusSessions[i].EnergyBefore >= 7 then
    begin
      highEnergyProd := highEnergyProd + FFocusSessions[i].ProductivityRating;
      Inc(highCount);
    end
    else if FFocusSessions[i].EnergyBefore <= 4 then
    begin
      lowEnergyProd := lowEnergyProd + FFocusSessions[i].ProductivityRating;
      Inc(lowCount);
    end;
  end;
  
  Result := Format('Energy-Productivity Correlation:'#13#10 +
                   'High energy sessions: %.1f/10 avg productivity'#13#10 +
                   'Low energy sessions: %.1f/10 avg productivity',
                   [highEnergyProd / Max(1, highCount),
                    lowEnergyProd / Max(1, lowCount)]);
end;

procedure TFocusTaskManager.SetPomodoroSettings(AWorkMinutes, AShortBreak, 
  ALongBreak, ACycleCount: Integer);
begin
  FPomodoroLength := AWorkMinutes;
  FShortBreakLength := AShortBreak;
  FLongBreakLength := ALongBreak;
  FPomodorosUntilLongBreak := ACycleCount;
end;

procedure TFocusTaskManager.SetFlowThreshold(AMinutes: Integer);
begin
  FFlowThreshold := AMinutes;
end;

function TFocusTaskManager.GetSettings: string;
begin
  Result := Format('Focus Manager Settings:'#13#10 +
                   'Pomodoro: %d minutes work, %d min short break, %d min long break'#13#10 +
                   'Long break after: %d pomodoros'#13#10 +
                   'Flow threshold: %d minutes',
                   [FPomodoroLength, FShortBreakLength, FLongBreakLength,
                    FPomodorosUntilLongBreak, FFlowThreshold]);
end;

function TFocusTaskManager.FocusTypeToString(AFType: TFocusType): string;
begin
  case AFType of
    ftDeepWork: Result := 'Deep Work';
    ftShallowWork: Result := 'Shallow Work';
    ftAdministrative: Result := 'Administrative';
    ftMeeting: Result := 'Meeting';
    ftCreative: Result := 'Creative';
    ftLearning: Result := 'Learning';
    ftCommunication: Result := 'Communication';
  else
    Result := 'Unknown';
  end;
end;

function TFocusTaskManager.FlowStateToString(AState: TFlowState): string;
begin
  case AState of
    fsNoFlow: Result := 'No Flow';
    fsLowFlow: Result := 'Low Flow';
    fsModerateFlow: Result := 'Moderate Flow';
    fsHighFlow: Result := 'High Flow';
    fsPeakFlow: Result := 'Peak Flow';
  else
    Result := 'Unknown';
  end;
end;

function TFocusTaskManager.FocusQualityToString(AQuality: TFocusQuality): string;
begin
  case AQuality of
    fqPoor: Result := 'Poor';
    fqFair: Result := 'Fair';
    fqGood: Result := 'Good';
    fqExcellent: Result := 'Excellent';
  else
    Result := 'Unknown';
  end;
end;

function TFocusTaskManager.DistractionTypeToString(AType: TDistractionType): string;
begin
  case AType of
    dtNotification: Result := 'Notification';
    dtInterruption: Result := 'Interruption';
    dtContextSwitch: Result := 'Context Switch';
    dtNoise: Result := 'Noise';
    dtTechnical: Result := 'Technical Issue';
    dtPersonal: Result := 'Personal';
    dtOther: Result := 'Other';
  else
    Result := 'Unknown';
  end;
end;

function TFocusTaskManager.SaveFocusDataToFile(const AFilename: string): Boolean;
begin
  Result := False;
  // Simplified - would implement full serialization
end;

function TFocusTaskManager.LoadFocusDataFromFile(const AFilename: string): Boolean;
begin
  Result := False;
  // Simplified - would implement full deserialization
end;

end.
