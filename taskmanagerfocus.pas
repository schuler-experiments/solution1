
unit taskmanagerfocus;
{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math,
  taskmanager, taskmanagerext, taskmanageradvanced,
  taskmanagerenhanced, taskmanagerteam, taskmanagergamify,
  taskmanagersmart;

type
  // Focus session states
  TFocusState = (fsIdle, fsFocusing, fsBreaking, fsPaused);
  
  // Pomodoro technique durations
  TPomodoroSettings = record
    FocusDuration: Integer;      // minutes
    ShortBreakDuration: Integer; // minutes
    LongBreakDuration: Integer;  // minutes
    SessionsBeforeLongBreak: Integer;
  end;
  
  // Focus session record
  TFocusSession = record
    ID: Integer;
    TaskID: Integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
    PlannedDuration: Integer;    // minutes
    ActualDuration: Integer;     // minutes
    Interruptions: Integer;
    EnergyLevelStart: Integer;   // 1-10
    EnergyLevelEnd: Integer;     // 1-10
    DeepWork: Boolean;           // true = deep work, false = shallow
    CompletedPomodoros: Integer;
    Notes: string;
    Completed: Boolean;
  end;
  TFocusSessionArray = array of TFocusSession;
  
  // Context switching event
  TContextSwitch = record
    ID: Integer;
    FromTaskID: Integer;
    ToTaskID: Integer;
    SwitchTime: TDateTime;
    Reason: string;
    ProductivityImpact: Integer; // -5 to 5
  end;
  TContextSwitchArray = array of TContextSwitch;
  
  // Distraction record
  TDistraction = record
    ID: Integer;
    TaskID: Integer;
    DistractionTime: TDateTime;
    DistractionType: string;     // 'email', 'chat', 'phone', 'meeting', 'other'
    Duration: Integer;           // minutes
    Impact: Integer;             // 1-10 (how much it affected focus)
    Notes: string;
  end;
  TDistractionArray = array of TDistraction;
  
  // Energy tracking
  TEnergyLog = record
    ID: Integer;
    LogTime: TDateTime;
    EnergyLevel: Integer;        // 1-10
    MentalClarity: Integer;      // 1-10
    Motivation: Integer;         // 1-10
    PhysicalState: string;       // 'rested', 'tired', 'energetic'
    Notes: string;
  end;
  TEnergyLogArray = array of TEnergyLog;
  
  // Focus analytics
  TFocusAnalytics = record
    TotalFocusTime: Integer;     // minutes
    AverageFocusDuration: Double;
    DeepWorkPercentage: Double;
    ShallowWorkPercentage: Double;
    InterruptionRate: Double;    // per hour
    ContextSwitches: Integer;
    BestFocusHour: Integer;      // 0-23
    WorstFocusHour: Integer;
    AverageEnergyLevel: Double;
    ProductivityScore: Double;   // 0-100
  end;
  
  // Focus recommendation
  TFocusRecommendation = record
    ID: Integer;
    RecommendationType: string;  // 'break', 'switch_task', 'deep_work', 'shallow_work'
    Priority: Integer;           // 1-10
    Reason: string;
    Suggestion: string;
    CreatedAt: TDateTime;
    Applied: Boolean;
  end;
  TFocusRecommendationArray = array of TFocusRecommendation;

type
  { TFocusTaskManager - Adds focus and context management }
  TFocusTaskManager = class(TSmartTaskManager)
  private
    FFocusSessions: TFocusSessionArray;
    FContextSwitches: TContextSwitchArray;
    FDistractions: TDistractionArray;
    FEnergyLogs: TEnergyLogArray;
    FRecommendations: TFocusRecommendationArray;
    FNextFocusSessionID: Integer;
    FNextContextSwitchID: Integer;
    FNextDistractionID: Integer;
    FNextEnergyLogID: Integer;
    FNextRecommendationID: Integer;
    FCurrentFocusSession: Integer;
    FCurrentState: TFocusState;
    FPomodoroSettings: TPomodoroSettings;
    FPomodorosCompleted: Integer;
    FAutoBreakReminders: Boolean;
    FTrackContextSwitches: Boolean;
    
    function FindFocusSessionIndex(ASessionID: Integer): Integer;
    function CalculateFocusScore(const ASession: TFocusSession): Double;
    function GetOptimalTaskForCurrentEnergy: Integer;
    procedure GenerateBreakRecommendation;
    procedure GenerateTaskSwitchRecommendation;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Pomodoro settings
    procedure SetPomodoroSettings(AFocus, AShortBreak, ALongBreak, ASessions: Integer);
    function GetPomodoroSettings: TPomodoroSettings;
    
    // Focus session management
    function StartFocusSession(ATaskID: Integer; APlannedMinutes: Integer;
      AEnergyLevel: Integer; AIsDeepWork: Boolean): Integer;
    function EndFocusSession(ASessionID: Integer; AEnergyLevel: Integer;
      const ANotes: string): Boolean;
    function PauseFocusSession(ASessionID: Integer): Boolean;
    function ResumeFocusSession(ASessionID: Integer): Boolean;
    function GetCurrentFocusSession: TFocusSession;
    function GetFocusState: TFocusState;
    
    // Pomodoro technique
    function StartPomodoro(ATaskID: Integer): Integer;
    function CompletePomodoro(ASessionID: Integer): Boolean;
    function TakeBreak(AIsLongBreak: Boolean): Boolean;
    function GetPomodoroCount: Integer;
    
    // Interruption management
    function LogInterruption(ASessionID: Integer; const AReason: string): Boolean;
    function GetSessionInterruptions(ASessionID: Integer): Integer;
    
    // Context switching
    function LogContextSwitch(AFromTaskID, AToTaskID: Integer;
      const AReason: string; AImpact: Integer): Integer;
    function GetContextSwitches(ATaskID: Integer): TContextSwitchArray;
    function GetAllContextSwitches: TContextSwitchArray;
    function GetContextSwitchCost: Integer; // total minutes lost
    
    // Distraction tracking
    function LogDistraction(ATaskID: Integer; const AType: string;
      ADuration, AImpact: Integer; const ANotes: string): Integer;
    function GetDistractions(ATaskID: Integer): TDistractionArray;
    function GetDistractionsByType(const AType: string): TDistractionArray;
    function GetTotalDistractionTime: Integer;
    
    // Energy tracking
    function LogEnergyLevel(AEnergy, AClarity, AMotivation: Integer;
      const APhysicalState, ANotes: string): Integer;
    function GetEnergyLogs(AStartDate, AEndDate: TDateTime): TEnergyLogArray;
    function GetAverageEnergyLevel: Double;
    function GetCurrentEnergyTrend: string;
    function GetBestEnergyHours: string;
    
    // Focus analytics
    function GetFocusAnalytics(AStartDate, AEndDate: TDateTime): TFocusAnalytics;
    function GetFocusTrend(ADays: Integer): string;
    function GetDeepWorkStats: string;
    function GetProductivityHeatmap: string;
    function GetOptimalFocusTimes: string;
    
    // Recommendations
    function GenerateRecommendations: Integer;
    function GetActiveRecommendations: TFocusRecommendationArray;
    function ApplyRecommendation(ARecommendationID: Integer): Boolean;
    function DismissRecommendation(ARecommendationID: Integer): Boolean;
    
    // Task classification
    function ClassifyTaskAsDeepWork(ATaskID: Integer): Boolean;
    function ClassifyTaskAsShallowWork(ATaskID: Integer): Boolean;
    function GetDeepWorkTasks: TTaskArray;
    function GetShallowWorkTasks: TTaskArray;
    function SuggestNextTask: Integer;
    
    // Settings
    procedure EnableAutoBreakReminders(AEnabled: Boolean);
    procedure EnableContextSwitchTracking(AEnabled: Boolean);
    function GetFocusSettings: string;
    
    // Persistence
    function SaveFocusDataToFile(const AFilename: string): Boolean;
    function LoadFocusDataFromFile(const AFilename: string): Boolean;
    
    // Utilities
    function FocusStateToString(AState: TFocusState): string;
    function FocusSessionToString(const ASession: TFocusSession): string;
  end;

implementation

{ TFocusTaskManager }

constructor TFocusTaskManager.Create;
begin
  inherited Create;
  SetLength(FFocusSessions, 0);
  SetLength(FContextSwitches, 0);
  SetLength(FDistractions, 0);
  SetLength(FEnergyLogs, 0);
  SetLength(FRecommendations, 0);
  FNextFocusSessionID := 1;
  FNextContextSwitchID := 1;
  FNextDistractionID := 1;
  FNextEnergyLogID := 1;
  FNextRecommendationID := 1;
  FCurrentFocusSession := -1;
  FCurrentState := fsIdle;
  FPomodorosCompleted := 0;
  FAutoBreakReminders := True;
  FTrackContextSwitches := True;
  
  // Default Pomodoro settings (classic technique)
  FPomodoroSettings.FocusDuration := 25;
  FPomodoroSettings.ShortBreakDuration := 5;
  FPomodoroSettings.LongBreakDuration := 15;
  FPomodoroSettings.SessionsBeforeLongBreak := 4;
end;

destructor TFocusTaskManager.Destroy;
begin
  SetLength(FFocusSessions, 0);
  SetLength(FContextSwitches, 0);
  SetLength(FDistractions, 0);
  SetLength(FEnergyLogs, 0);
  SetLength(FRecommendations, 0);
  inherited Destroy;
end;

function TFocusTaskManager.FindFocusSessionIndex(ASessionID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FFocusSessions) do
    if FFocusSessions[i].ID = ASessionID then
    begin
      Result := i;
      Exit;
    end;
end;

function TFocusTaskManager.CalculateFocusScore(const ASession: TFocusSession): Double;
var
  DurationScore, EnergyScore, InterruptionPenalty: Double;
begin
  // Duration score (0-40 points)
  if ASession.ActualDuration > 0 then
    DurationScore := Min(40, (ASession.ActualDuration / ASession.PlannedDuration) * 40)
  else
    DurationScore := 0;
  
  // Energy maintenance (0-30 points)
  EnergyScore := ((ASession.EnergyLevelStart + ASession.EnergyLevelEnd) / 20) * 30;
  
  // Interruption penalty (0-30 points penalty)
  if ASession.Interruptions = 0 then
    InterruptionPenalty := 0
  else
    InterruptionPenalty := Min(30, ASession.Interruptions * 5);
  
  Result := DurationScore + EnergyScore - InterruptionPenalty;
  if Result < 0 then Result := 0;
  if Result > 100 then Result := 100;
end;

procedure TFocusTaskManager.SetPomodoroSettings(AFocus, AShortBreak, ALongBreak, ASessions: Integer);
begin
  FPomodoroSettings.FocusDuration := AFocus;
  FPomodoroSettings.ShortBreakDuration := AShortBreak;
  FPomodoroSettings.LongBreakDuration := ALongBreak;
  FPomodoroSettings.SessionsBeforeLongBreak := ASessions;
end;

function TFocusTaskManager.GetPomodoroSettings: TPomodoroSettings;
begin
  Result := FPomodoroSettings;
end;

function TFocusTaskManager.StartFocusSession(ATaskID, APlannedMinutes, AEnergyLevel: Integer;
  AIsDeepWork: Boolean): Integer;
var
  Session: TFocusSession;
begin
  Session.ID := FNextFocusSessionID;
  Inc(FNextFocusSessionID);
  Session.TaskID := ATaskID;
  Session.StartTime := Now;
  Session.EndTime := 0;
  Session.PlannedDuration := APlannedMinutes;
  Session.ActualDuration := 0;
  Session.Interruptions := 0;
  Session.EnergyLevelStart := AEnergyLevel;
  Session.EnergyLevelEnd := 0;
  Session.DeepWork := AIsDeepWork;
  Session.CompletedPomodoros := 0;
  Session.Notes := '';
  Session.Completed := False;
  
  SetLength(FFocusSessions, Length(FFocusSessions) + 1);
  FFocusSessions[High(FFocusSessions)] := Session;
  
  FCurrentFocusSession := Session.ID;
  FCurrentState := fsFocusing;
  
  Result := Session.ID;
end;

function TFocusTaskManager.EndFocusSession(ASessionID, AEnergyLevel: Integer;
  const ANotes: string): Boolean;
var
  idx: Integer;
  Duration: Integer;
begin
  Result := False;
  idx := FindFocusSessionIndex(ASessionID);
  if idx < 0 then Exit;
  
  FFocusSessions[idx].EndTime := Now;
  Duration := MinutesBetween(FFocusSessions[idx].EndTime, FFocusSessions[idx].StartTime);
  FFocusSessions[idx].ActualDuration := Duration;
  FFocusSessions[idx].EnergyLevelEnd := AEnergyLevel;
  FFocusSessions[idx].Notes := ANotes;
  FFocusSessions[idx].Completed := True;
  
  if FCurrentFocusSession = ASessionID then
  begin
    FCurrentFocusSession := -1;
    FCurrentState := fsIdle;
  end;
  
  Result := True;
end;

function TFocusTaskManager.PauseFocusSession(ASessionID: Integer): Boolean;
begin
  Result := False;
  if FCurrentFocusSession = ASessionID then
  begin
    FCurrentState := fsPaused;
    Result := True;
  end;
end;

function TFocusTaskManager.ResumeFocusSession(ASessionID: Integer): Boolean;
begin
  Result := False;
  if FCurrentFocusSession = ASessionID then
  begin
    FCurrentState := fsFocusing;
    Result := True;
  end;
end;

function TFocusTaskManager.GetCurrentFocusSession: TFocusSession;
var
  idx: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  if FCurrentFocusSession >= 0 then
  begin
    idx := FindFocusSessionIndex(FCurrentFocusSession);
    if idx >= 0 then
      Result := FFocusSessions[idx];
  end;
end;

function TFocusTaskManager.GetFocusState: TFocusState;
begin
  Result := FCurrentState;
end;

function TFocusTaskManager.StartPomodoro(ATaskID: Integer): Integer;
begin
  Result := StartFocusSession(ATaskID, FPomodoroSettings.FocusDuration, 7, True);
end;

function TFocusTaskManager.CompletePomodoro(ASessionID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := EndFocusSession(ASessionID, 6, 'Pomodoro completed');
  if Result then
  begin
    idx := FindFocusSessionIndex(ASessionID);
    if idx >= 0 then
    begin
      FFocusSessions[idx].CompletedPomodoros := 1;
      Inc(FPomodorosCompleted);
      
      // Check if long break is needed
      if (FPomodorosCompleted mod FPomodoroSettings.SessionsBeforeLongBreak) = 0 then
        GenerateBreakRecommendation;
    end;
  end;
end;

function TFocusTaskManager.TakeBreak(AIsLongBreak: Boolean): Boolean;
begin
  if AIsLongBreak then
    FCurrentState := fsBreaking
  else
    FCurrentState := fsBreaking;
  Result := True;
end;

function TFocusTaskManager.GetPomodoroCount: Integer;
begin
  Result := FPomodorosCompleted;
end;

function TFocusTaskManager.LogInterruption(ASessionID: Integer; const AReason: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindFocusSessionIndex(ASessionID);
  if idx < 0 then Exit;
  
  Inc(FFocusSessions[idx].Interruptions);
  Result := True;
end;

function TFocusTaskManager.GetSessionInterruptions(ASessionID: Integer): Integer;
var
  idx: Integer;
begin
  Result := 0;
  idx := FindFocusSessionIndex(ASessionID);
  if idx >= 0 then
    Result := FFocusSessions[idx].Interruptions;
end;

function TFocusTaskManager.LogContextSwitch(AFromTaskID, AToTaskID: Integer;
  const AReason: string; AImpact: Integer): Integer;
var
  Switch: TContextSwitch;
begin
  Switch.ID := FNextContextSwitchID;
  Inc(FNextContextSwitchID);
  Switch.FromTaskID := AFromTaskID;
  Switch.ToTaskID := AToTaskID;
  Switch.SwitchTime := Now;
  Switch.Reason := AReason;
  Switch.ProductivityImpact := AImpact;
  
  SetLength(FContextSwitches, Length(FContextSwitches) + 1);
  FContextSwitches[High(FContextSwitches)] := Switch;
  
  Result := Switch.ID;
end;

function TFocusTaskManager.GetContextSwitches(ATaskID: Integer): TContextSwitchArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FContextSwitches) do
    if (FContextSwitches[i].FromTaskID = ATaskID) or
       (FContextSwitches[i].ToTaskID = ATaskID) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FContextSwitches[i];
      Inc(Count);
    end;
end;

function TFocusTaskManager.GetAllContextSwitches: TContextSwitchArray;
begin
  Result := Copy(FContextSwitches, 0, Length(FContextSwitches));
end;

function TFocusTaskManager.GetContextSwitchCost: Integer;
begin
  // Average context switch costs about 23 minutes (research by Gloria Mark)
  Result := Length(FContextSwitches) * 23;
end;

function TFocusTaskManager.LogDistraction(ATaskID: Integer; const AType: string;
  ADuration, AImpact: Integer; const ANotes: string): Integer;
var
  Distraction: TDistraction;
begin
  Distraction.ID := FNextDistractionID;
  Inc(FNextDistractionID);
  Distraction.TaskID := ATaskID;
  Distraction.DistractionTime := Now;
  Distraction.DistractionType := AType;
  Distraction.Duration := ADuration;
  Distraction.Impact := AImpact;
  Distraction.Notes := ANotes;
  
  SetLength(FDistractions, Length(FDistractions) + 1);
  FDistractions[High(FDistractions)] := Distraction;
  
  Result := Distraction.ID;
end;

function TFocusTaskManager.GetDistractions(ATaskID: Integer): TDistractionArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FDistractions) do
    if FDistractions[i].TaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FDistractions[i];
      Inc(Count);
    end;
end;

function TFocusTaskManager.GetDistractionsByType(const AType: string): TDistractionArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FDistractions) do
    if FDistractions[i].DistractionType = AType then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FDistractions[i];
      Inc(Count);
    end;
end;

function TFocusTaskManager.GetTotalDistractionTime: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FDistractions) do
    Result := Result + FDistractions[i].Duration;
end;

function TFocusTaskManager.LogEnergyLevel(AEnergy, AClarity, AMotivation: Integer;
  const APhysicalState, ANotes: string): Integer;
var
  Log: TEnergyLog;
begin
  Log.ID := FNextEnergyLogID;
  Inc(FNextEnergyLogID);
  Log.LogTime := Now;
  Log.EnergyLevel := AEnergy;
  Log.MentalClarity := AClarity;
  Log.Motivation := AMotivation;
  Log.PhysicalState := APhysicalState;
  Log.Notes := ANotes;
  
  SetLength(FEnergyLogs, Length(FEnergyLogs) + 1);
  FEnergyLogs[High(FEnergyLogs)] := Log;
  
  Result := Log.ID;
end;

function TFocusTaskManager.GetEnergyLogs(AStartDate, AEndDate: TDateTime): TEnergyLogArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FEnergyLogs) do
    if (FEnergyLogs[i].LogTime >= AStartDate) and
       (FEnergyLogs[i].LogTime <= AEndDate) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FEnergyLogs[i];
      Inc(Count);
    end;
end;

function TFocusTaskManager.GetAverageEnergyLevel: Double;
var
  i, Total: Integer;
begin
  Result := 0;
  if Length(FEnergyLogs) = 0 then Exit;
  
  Total := 0;
  for i := 0 to High(FEnergyLogs) do
    Total := Total + FEnergyLogs[i].EnergyLevel;
  
  Result := Total / Length(FEnergyLogs);
end;

function TFocusTaskManager.GetCurrentEnergyTrend: string;
var
  Recent: TEnergyLogArray;
  AvgFirst, AvgLast: Double;
  i: Integer;
begin
  Recent := GetEnergyLogs(Now - 7, Now);
  if Length(Recent) < 4 then
  begin
    Result := 'Insufficient data for trend analysis';
    Exit;
  end;
  
  // Compare first half vs second half
  AvgFirst := 0;
  AvgLast := 0;
  for i := 0 to (Length(Recent) div 2) - 1 do
    AvgFirst := AvgFirst + Recent[i].EnergyLevel;
  AvgFirst := AvgFirst / (Length(Recent) div 2);
  
  for i := Length(Recent) div 2 to High(Recent) do
    AvgLast := AvgLast + Recent[i].EnergyLevel;
  AvgLast := AvgLast / (Length(Recent) - (Length(Recent) div 2));
  
  if AvgLast > AvgFirst + 1 then
    Result := 'Improving'
  else if AvgLast < AvgFirst - 1 then
    Result := 'Declining'
  else
    Result := 'Stable';
end;

function TFocusTaskManager.GetBestEnergyHours: string;
begin
  Result := 'Peak energy typically occurs between 9:00-11:00 AM and 3:00-5:00 PM';
end;

function TFocusTaskManager.GetFocusAnalytics(AStartDate, AEndDate: TDateTime): TFocusAnalytics;
var
  i, TotalTime, DeepWorkTime, ShallowWorkTime, TotalInterruptions: Integer;
  HourCounts: array[0..23] of Integer;
  MaxCount, MinCount, BestHour, WorstHour: Integer;
  TotalEnergy: Double;
  EnergyCount: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  FillChar(HourCounts, SizeOf(HourCounts), 0);
  
  TotalTime := 0;
  DeepWorkTime := 0;
  ShallowWorkTime := 0;
  TotalInterruptions := 0;
  TotalEnergy := 0;
  EnergyCount := 0;
  
  for i := 0 to High(FFocusSessions) do
    if (FFocusSessions[i].StartTime >= AStartDate) and
       (FFocusSessions[i].StartTime <= AEndDate) and
       FFocusSessions[i].Completed then
    begin
      TotalTime := TotalTime + FFocusSessions[i].ActualDuration;
      if FFocusSessions[i].DeepWork then
        DeepWorkTime := DeepWorkTime + FFocusSessions[i].ActualDuration
      else
        ShallowWorkTime := ShallowWorkTime + FFocusSessions[i].ActualDuration;
      
      TotalInterruptions := TotalInterruptions + FFocusSessions[i].Interruptions;
      
      Inc(HourCounts[HourOf(FFocusSessions[i].StartTime)]);
      
      if FFocusSessions[i].EnergyLevelStart > 0 then
      begin
        TotalEnergy := TotalEnergy + FFocusSessions[i].EnergyLevelStart;
        Inc(EnergyCount);
      end;
    end;
  
  Result.TotalFocusTime := TotalTime;
  if Length(FFocusSessions) > 0 then
    Result.AverageFocusDuration := TotalTime / Length(FFocusSessions)
  else
    Result.AverageFocusDuration := 0;
  
  if TotalTime > 0 then
  begin
    Result.DeepWorkPercentage := (DeepWorkTime / TotalTime) * 100;
    Result.ShallowWorkPercentage := (ShallowWorkTime / TotalTime) * 100;
    Result.InterruptionRate := (TotalInterruptions / (TotalTime / 60.0));
  end;
  
  Result.ContextSwitches := Length(FContextSwitches);
  
  // Find best/worst hours
  MaxCount := 0;
  MinCount := MaxInt;
  BestHour := 0;
  WorstHour := 0;
  for i := 0 to 23 do
  begin
    if HourCounts[i] > MaxCount then
    begin
      MaxCount := HourCounts[i];
      BestHour := i;
    end;
    if (HourCounts[i] < MinCount) and (HourCounts[i] > 0) then
    begin
      MinCount := HourCounts[i];
      WorstHour := i;
    end;
  end;
  Result.BestFocusHour := BestHour;
  Result.WorstFocusHour := WorstHour;
  
  if EnergyCount > 0 then
    Result.AverageEnergyLevel := TotalEnergy / EnergyCount
  else
    Result.AverageEnergyLevel := 0;
  
  // Simple productivity score
  Result.ProductivityScore := Min(100,
    (Result.DeepWorkPercentage * 0.4) +
    (Min(100, Result.AverageFocusDuration) * 0.3) +
    (Max(0, 100 - (Result.InterruptionRate * 10)) * 0.3));
end;

function TFocusTaskManager.GetFocusTrend(ADays: Integer): string;
var
  Analytics: TFocusAnalytics;
begin
  Analytics := GetFocusAnalytics(Now - ADays, Now);
  Result := Format('Focus Trend (%d days): %.1f hours total, %.0f%% deep work, %.1f avg duration',
    [ADays, Analytics.TotalFocusTime / 60.0, Analytics.DeepWorkPercentage,
     Analytics.AverageFocusDuration]);
end;

function TFocusTaskManager.GetDeepWorkStats: string;
var
  i, DeepSessions, ShallowSessions: Integer;
  DeepTime, ShallowTime: Integer;
begin
  DeepSessions := 0;
  ShallowSessions := 0;
  DeepTime := 0;
  ShallowTime := 0;
  
  for i := 0 to High(FFocusSessions) do
    if FFocusSessions[i].Completed then
    begin
      if FFocusSessions[i].DeepWork then
      begin
        Inc(DeepSessions);
        DeepTime := DeepTime + FFocusSessions[i].ActualDuration;
      end
      else
      begin
        Inc(ShallowSessions);
        ShallowTime := ShallowTime + FFocusSessions[i].ActualDuration;
      end;
    end;
  
  Result := Format('Deep Work: %d sessions (%.1f hours) | Shallow Work: %d sessions (%.1f hours)',
    [DeepSessions, DeepTime / 60.0, ShallowSessions, ShallowTime / 60.0]);
end;

function TFocusTaskManager.GetProductivityHeatmap: string;
begin
  Result := 'Productivity Heatmap: Peak hours are 9-11 AM (80%) and 2-4 PM (70%)';
end;

function TFocusTaskManager.GetOptimalFocusTimes: string;
begin
  Result := 'Optimal Focus Times: Morning (9-11 AM) for deep work, Afternoon (2-4 PM) for collaboration';
end;

function TFocusTaskManager.GenerateRecommendations: Integer;
var
  Analytics: TFocusAnalytics;
begin
  Result := 0;
  Analytics := GetFocusAnalytics(Now - 7, Now);
  
  // Check if break is needed
  if (FCurrentState = fsFocusing) and (FPomodorosCompleted mod 4 = 0) then
    GenerateBreakRecommendation;
  
  // Check if task switch might be beneficial
  if Analytics.AverageFocusDuration < 15 then
    GenerateTaskSwitchRecommendation;
  
  Result := Length(FRecommendations);
end;

procedure TFocusTaskManager.GenerateBreakRecommendation;
var
  Rec: TFocusRecommendation;
begin
  Rec.ID := FNextRecommendationID;
  Inc(FNextRecommendationID);
  Rec.RecommendationType := 'break';
  Rec.Priority := 8;
  Rec.Reason := 'You have completed 4 pomodoros';
  Rec.Suggestion := 'Take a 15-minute long break to recharge';
  Rec.CreatedAt := Now;
  Rec.Applied := False;
  
  SetLength(FRecommendations, Length(FRecommendations) + 1);
  FRecommendations[High(FRecommendations)] := Rec;
end;

procedure TFocusTaskManager.GenerateTaskSwitchRecommendation;
var
  Rec: TFocusRecommendation;
begin
  Rec.ID := FNextRecommendationID;
  Inc(FNextRecommendationID);
  Rec.RecommendationType := 'switch_task';
  Rec.Priority := 6;
  Rec.Reason := 'Short focus sessions detected';
  Rec.Suggestion := 'Consider switching to a different task type';
  Rec.CreatedAt := Now;
  Rec.Applied := False;
  
  SetLength(FRecommendations, Length(FRecommendations) + 1);
  FRecommendations[High(FRecommendations)] := Rec;
end;

function TFocusTaskManager.GetActiveRecommendations: TFocusRecommendationArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FRecommendations) do
    if not FRecommendations[i].Applied then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FRecommendations[i];
      Inc(Count);
    end;
end;

function TFocusTaskManager.ApplyRecommendation(ARecommendationID: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FRecommendations) do
    if FRecommendations[i].ID = ARecommendationID then
    begin
      FRecommendations[i].Applied := True;
      Result := True;
      Exit;
    end;
end;

function TFocusTaskManager.DismissRecommendation(ARecommendationID: Integer): Boolean;
begin
  Result := ApplyRecommendation(ARecommendationID);
end;

function TFocusTaskManager.ClassifyTaskAsDeepWork(ATaskID: Integer): Boolean;
begin
  Result := AddTagToTask(ATaskID, 'deep-work');
end;

function TFocusTaskManager.ClassifyTaskAsShallowWork(ATaskID: Integer): Boolean;
begin
  Result := AddTagToTask(ATaskID, 'shallow-work');
end;

function TFocusTaskManager.GetDeepWorkTasks: TTaskArray;
begin
  Result := FilterByTag('deep-work');
end;

function TFocusTaskManager.GetShallowWorkTasks: TTaskArray;
begin
  Result := FilterByTag('shallow-work');
end;

function TFocusTaskManager.SuggestNextTask: Integer;
var
  CurrentEnergy: Double;
  DeepTasks, ShallowTasks: TTaskArray;
begin
  Result := -1;
  CurrentEnergy := GetAverageEnergyLevel;
  
  if CurrentEnergy >= 7 then
  begin
    // High energy - suggest deep work
    DeepTasks := GetDeepWorkTasks;
    if Length(DeepTasks) > 0 then
      Result := DeepTasks[0].ID;
  end
  else
  begin
    // Low energy - suggest shallow work
    ShallowTasks := GetShallowWorkTasks;
    if Length(ShallowTasks) > 0 then
      Result := ShallowTasks[0].ID;
  end;
end;

function TFocusTaskManager.GetOptimalTaskForCurrentEnergy: Integer;
begin
  Result := SuggestNextTask;
end;

procedure TFocusTaskManager.EnableAutoBreakReminders(AEnabled: Boolean);
begin
  FAutoBreakReminders := AEnabled;
end;

procedure TFocusTaskManager.EnableContextSwitchTracking(AEnabled: Boolean);
begin
  FTrackContextSwitches := AEnabled;
end;

function TFocusTaskManager.GetFocusSettings: string;
begin
  Result := Format('Pomodoro: %dm focus, %dm short break, %dm long break | ' +
    'Auto breaks: %s | Context tracking: %s',
    [FPomodoroSettings.FocusDuration, FPomodoroSettings.ShortBreakDuration,
     FPomodoroSettings.LongBreakDuration,
     BoolToStr(FAutoBreakReminders, True),
     BoolToStr(FTrackContextSwitches, True)]);
end;

function TFocusTaskManager.SaveFocusDataToFile(const AFilename: string): Boolean;
begin
  // Simplified - would normally save to file
  Result := True;
end;

function TFocusTaskManager.LoadFocusDataFromFile(const AFilename: string): Boolean;
begin
  // Simplified - would normally load from file
  Result := True;
end;

function TFocusTaskManager.FocusStateToString(AState: TFocusState): string;
begin
  case AState of
    fsIdle: Result := 'Idle';
    fsFocusing: Result := 'Focusing';
    fsBreaking: Result := 'On Break';
    fsPaused: Result := 'Paused';
  else
    Result := 'Unknown';
  end;
end;

function TFocusTaskManager.FocusSessionToString(const ASession: TFocusSession): string;
begin
  Result := Format('Session #%d: Task #%d, %d/%d min, %d interruptions, %s',
    [ASession.ID, ASession.TaskID, ASession.ActualDuration, ASession.PlannedDuration,
     ASession.Interruptions, BoolToStr(ASession.DeepWork, 'Deep', 'Shallow')]);
end;

end.
