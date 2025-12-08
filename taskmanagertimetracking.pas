
unit taskmanagertimetracking;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math,
  taskmanager, taskmanageradvanced, taskmanagerlifestyle, taskmanagerwellbeing;

type
  // Time tracking session status
  TTimerStatus = (tsNotStarted, tsRunning, tsPaused, tsCompleted, tsCancelled);
  
  // Pomodoro session types
  TPomodoroType = (ptWork, ptShortBreak, ptLongBreak);
  
  // Time entry for detailed logging
  TTimeEntry = record
    ID: Integer;
    TaskID: Integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
    DurationMinutes: Double;
    Status: TTimerStatus;
    Notes: string;
    WasPomodoro: Boolean;
    PomodoroCount: Integer;  // Which pomodoro in the session
    InterruptionCount: Integer;
    Tags: array of string;
  end;
  
  TTimeEntryArray = array of TTimeEntry;
  
  // Active timer tracking
  TActiveTimer = record
    ID: Integer;
    TaskID: Integer;
    StartTime: TDateTime;
    PausedTime: TDateTime;
    TotalPausedMinutes: Double;
    Status: TTimerStatus;
    IsPomodoro: Boolean;
    PomodoroNumber: Integer;
  end;
  
  TActiveTimerArray = array of TActiveTimer;
  
  // Pomodoro session configuration
  TPomodoroConfig = record
    WorkDuration: Integer;      // minutes (default 25)
    ShortBreakDuration: Integer; // minutes (default 5)
    LongBreakDuration: Integer;  // minutes (default 15)
    PomodorosBeforeLongBreak: Integer; // default 4
    AutoStartBreaks: Boolean;
    AutoStartNextPomodoro: Boolean;
    PlaySoundOnComplete: Boolean;
  end;
  
  // Pomodoro session tracking
  TPomodoroSession = record
    ID: Integer;
    TaskID: Integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
    SessionType: TPomodoroType;
    PlannedDuration: Integer;
    ActualDuration: Integer;
    WasCompleted: Boolean;
    WasInterrupted: Boolean;
    InterruptionReason: string;
    PomodoroNumber: Integer;
  end;
  
  TPomodoroSessionArray = array of TPomodoroSession;
  
  // Time blocking for scheduling focused work
  TTimeBlock = record
    ID: Integer;
    TaskID: Integer;
    BlockName: string;
    StartTime: TDateTime;
    EndTime: TDateTime;
    DurationMinutes: Integer;
    IsRecurring: Boolean;
    RecurrencePattern: string;
    Color: string;
    Notes: string;
  end;
  
  TTimeBlockArray = array of TTimeBlock;
  
  // Productivity analytics
  TProductivityMetrics = record
    TotalTimeTracked: Double;    // hours
    FocusedTimePercent: Double;  // percentage
    AverageSessionLength: Double; // minutes
    PomodorosCompleted: Integer;
    TasksCompleted: Integer;
    EstimateAccuracy: Double;    // percentage
    PeakProductivityHour: Integer; // 0-23
    InterruptionsPerDay: Double;
  end;
  
  // Time comparison report
  TTimeComparison = record
    TaskID: Integer;
    TaskTitle: string;
    EstimatedHours: Double;
    ActualHours: Double;
    Variance: Double;            // percentage
    VarianceHours: Double;
    Status: string;              // Under/Over/OnTarget
  end;
  
  TTimeComparisonArray = array of TTimeComparison;

  { TTimeTrackingTaskManager }
  TTimeTrackingTaskManager = class(TWellbeingTaskManager)
  private
    FTimeEntries: array of TTimeEntry;
    FActiveTimers: array of TActiveTimer;
    FPomodoroSessions: array of TPomodoroSession;
    FTimeBlocks: array of TTimeBlock;
    FPomodoroConfig: TPomodoroConfig;
    FNextTimeEntryID: Integer;
    FNextTimerID: Integer;
    FNextPomodoroID: Integer;
    FNextBlockID: Integer;
    
    function FindTimeEntryIndex(AID: Integer): Integer;
    function FindActiveTimerIndex(AID: Integer): Integer;
    function FindTimerByTaskID(ATaskID: Integer): Integer;
    function FindPomodoroIndex(AID: Integer): Integer;
    function FindTimeBlockIndex(AID: Integer): Integer;
    function CalculateElapsedMinutes(ATimer: TActiveTimer): Double;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Timer management
    function StartTimer(ATaskID: Integer; const ANotes: string): Integer;
    function StartPomodoroTimer(ATaskID: Integer): Integer;
    function PauseTimer(ATimerID: Integer): Boolean;
    function ResumeTimer(ATimerID: Integer): Boolean;
    function StopTimer(ATimerID: Integer; const ANotes: string): Integer;
    function CancelTimer(ATimerID: Integer): Boolean;
    function GetActiveTimer(ATaskID: Integer): TActiveTimer;
    function GetAllActiveTimers: TActiveTimerArray;
    function IsTaskTimerRunning(ATaskID: Integer): Boolean;
    
    // Pomodoro management
    function ConfigurePomodoro(AWorkMin, AShortBreakMin, ALongBreakMin, 
                               APomodorosBeforeLong: Integer): Boolean;
    function StartPomodoroSession(ATaskID: Integer; AType: TPomodoroType): Integer;
    function CompletePomodoroSession(ASessionID: Integer): Boolean;
    function InterruptPomodoro(ASessionID: Integer; const AReason: string): Boolean;
    function GetActivePomodoroSession(ATaskID: Integer): TPomodoroSession;
    function GetPomodoroHistory(ATaskID: Integer): TPomodoroSessionArray;
    function GetTodaysPomodoroCount: Integer;
    function GetPomodoroStreak: Integer;
    
    // Time entries and logs
    function AddManualTimeEntry(ATaskID: Integer; AStartTime, AEndTime: TDateTime;
                                const ANotes: string): Integer;
    function GetTaskTimeEntries(ATaskID: Integer): TTimeEntryArray;
    function GetTimeEntriesForPeriod(AStart, AEnd: TDateTime): TTimeEntryArray;
    function GetTotalTimeForTask(ATaskID: Integer): Double;
    function GetTotalTimeForPeriod(AStart, AEnd: TDateTime): Double;
    function DeleteTimeEntry(AEntryID: Integer): Boolean;
    function UpdateTimeEntryNotes(AEntryID: Integer; const ANotes: string): Boolean;
    
    // Time blocking
    function CreateTimeBlock(ATaskID: Integer; const ABlockName: string;
                            AStartTime: TDateTime; ADurationMin: Integer;
                            const ANotes: string): Integer;
    function CreateRecurringTimeBlock(ATaskID: Integer; const ABlockName: string;
                                     AStartTime: TDateTime; ADurationMin: Integer;
                                     const ARecurrence, ANotes: string): Integer;
    function MoveTimeBlock(ABlockID: Integer; ANewStartTime: TDateTime): Boolean;
    function DeleteTimeBlock(ABlockID: Integer): Boolean;
    function GetTimeBlocksForDay(ADate: TDateTime): TTimeBlockArray;
    function GetTaskTimeBlocks(ATaskID: Integer): TTimeBlockArray;
    function FindAvailableTimeSlot(ADurationMin: Integer; 
                                   APreferredStart: TDateTime): TDateTime;
    
    // Analytics and reports
    function GetProductivityMetrics(AStartDate, AEndDate: TDateTime): TProductivityMetrics;
    function GetTimeComparisonReport: TTimeComparisonArray;
    function GetDailyTimeLog(ADate: TDateTime): string;
    function GetWeeklyTimeReport(AWeekStart: TDateTime): string;
    function GetTaskTimeReport(ATaskID: Integer): string;
    function GetPomodoroStats(ADays: Integer): string;
    function GetProductivityByHour: string;
    function GetProductivityByDayOfWeek: string;
    function GetTopTimeConsumingTasks(ACount: Integer): string;
    function GetEstimateAccuracyReport: string;
    
    // Utility functions
    function FormatDuration(AMinutes: Double): string;
    function TimerStatusToString(AStatus: TTimerStatus): string;
    function PomodoroTypeToString(AType: TPomodoroType): string;
    
    // Data persistence
    function SaveTimeTrackingDataToFile(const AFilename: string): Boolean;
    function LoadTimeTrackingDataFromFile(const AFilename: string): Boolean;
    
    // Properties
    property PomodoroConfig: TPomodoroConfig read FPomodoroConfig;
  end;

implementation

{ TTimeTrackingTaskManager }

constructor TTimeTrackingTaskManager.Create;
begin
  inherited Create;
  SetLength(FTimeEntries, 0);
  SetLength(FActiveTimers, 0);
  SetLength(FPomodoroSessions, 0);
  SetLength(FTimeBlocks, 0);
  
  FNextTimeEntryID := 1;
  FNextTimerID := 1;
  FNextPomodoroID := 1;
  FNextBlockID := 1;
  
  // Set default Pomodoro configuration
  FPomodoroConfig.WorkDuration := 25;
  FPomodoroConfig.ShortBreakDuration := 5;
  FPomodoroConfig.LongBreakDuration := 15;
  FPomodoroConfig.PomodorosBeforeLongBreak := 4;
  FPomodoroConfig.AutoStartBreaks := False;
  FPomodoroConfig.AutoStartNextPomodoro := False;
  FPomodoroConfig.PlaySoundOnComplete := True;
end;

destructor TTimeTrackingTaskManager.Destroy;
begin
  SetLength(FTimeEntries, 0);
  SetLength(FActiveTimers, 0);
  SetLength(FPomodoroSessions, 0);
  SetLength(FTimeBlocks, 0);
  inherited Destroy;
end;

function TTimeTrackingTaskManager.FindTimeEntryIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTimeEntries) do
    if FTimeEntries[i].ID = AID then
    begin
      Result := i;
      Exit;
    end;
end;

function TTimeTrackingTaskManager.FindActiveTimerIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FActiveTimers) do
    if FActiveTimers[i].ID = AID then
    begin
      Result := i;
      Exit;
    end;
end;

function TTimeTrackingTaskManager.FindTimerByTaskID(ATaskID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FActiveTimers) do
    if (FActiveTimers[i].TaskID = ATaskID) and 
       (FActiveTimers[i].Status in [tsRunning, tsPaused]) then
    begin
      Result := i;
      Exit;
    end;
end;

function TTimeTrackingTaskManager.FindPomodoroIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FPomodoroSessions) do
    if FPomodoroSessions[i].ID = AID then
    begin
      Result := i;
      Exit;
    end;
end;

function TTimeTrackingTaskManager.FindTimeBlockIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTimeBlocks) do
    if FTimeBlocks[i].ID = AID then
    begin
      Result := i;
      Exit;
    end;
end;

function TTimeTrackingTaskManager.CalculateElapsedMinutes(ATimer: TActiveTimer): Double;
var
  EndTime: TDateTime;
begin
  if ATimer.Status = tsRunning then
    EndTime := Now
  else if ATimer.Status = tsPaused then
    EndTime := ATimer.PausedTime
  else
    EndTime := ATimer.StartTime;
    
  Result := MinutesBetween(EndTime, ATimer.StartTime) - ATimer.TotalPausedMinutes;
end;

function TTimeTrackingTaskManager.StartTimer(ATaskID: Integer; const ANotes: string): Integer;
var
  NewTimer: TActiveTimer;
  ExistingIdx: Integer;
begin
  Result := -1;
  
  // Check if task exists
  if GetTaskByID(ATaskID) = -1 then
    Exit;
    
  // Check if timer already running for this task
  ExistingIdx := FindTimerByTaskID(ATaskID);
  if ExistingIdx <> -1 then
  begin
    Result := FActiveTimers[ExistingIdx].ID;
    Exit;
  end;
  
  NewTimer.ID := FNextTimerID;
  Inc(FNextTimerID);
  NewTimer.TaskID := ATaskID;
  NewTimer.StartTime := Now;
  NewTimer.PausedTime := 0;
  NewTimer.TotalPausedMinutes := 0;
  NewTimer.Status := tsRunning;
  NewTimer.IsPomodoro := False;
  NewTimer.PomodoroNumber := 0;
  
  SetLength(FActiveTimers, Length(FActiveTimers) + 1);
  FActiveTimers[High(FActiveTimers)] := NewTimer;
  
  Result := NewTimer.ID;
end;

function TTimeTrackingTaskManager.StartPomodoroTimer(ATaskID: Integer): Integer;
var
  NewTimer: TActiveTimer;
  PomodoroCount: Integer;
begin
  Result := -1;
  
  if GetTaskByID(ATaskID) = -1 then
    Exit;
    
  PomodoroCount := GetTodaysPomodoroCount + 1;
  
  NewTimer.ID := FNextTimerID;
  Inc(FNextTimerID);
  NewTimer.TaskID := ATaskID;
  NewTimer.StartTime := Now;
  NewTimer.PausedTime := 0;
  NewTimer.TotalPausedMinutes := 0;
  NewTimer.Status := tsRunning;
  NewTimer.IsPomodoro := True;
  NewTimer.PomodoroNumber := PomodoroCount;
  
  SetLength(FActiveTimers, Length(FActiveTimers) + 1);
  FActiveTimers[High(FActiveTimers)] := NewTimer;
  
  Result := NewTimer.ID;
end;

function TTimeTrackingTaskManager.PauseTimer(ATimerID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindActiveTimerIndex(ATimerID);
  if (idx = -1) or (FActiveTimers[idx].Status <> tsRunning) then
    Exit;
    
  FActiveTimers[idx].Status := tsPaused;
  FActiveTimers[idx].PausedTime := Now;
  Result := True;
end;

function TTimeTrackingTaskManager.ResumeTimer(ATimerID: Integer): Boolean;
var
  idx: Integer;
  PauseDuration: Double;
begin
  Result := False;
  idx := FindActiveTimerIndex(ATimerID);
  if (idx = -1) or (FActiveTimers[idx].Status <> tsPaused) then
    Exit;
    
  PauseDuration := MinutesBetween(Now, FActiveTimers[idx].PausedTime);
  FActiveTimers[idx].TotalPausedMinutes := FActiveTimers[idx].TotalPausedMinutes + PauseDuration;
  FActiveTimers[idx].Status := tsRunning;
  Result := True;
end;

function TTimeTrackingTaskManager.StopTimer(ATimerID: Integer; const ANotes: string): Integer;
var
  idx: Integer;
  Timer: TActiveTimer;
  NewEntry: TTimeEntry;
  Duration: Double;
begin
  Result := -1;
  idx := FindActiveTimerIndex(ATimerID);
  if idx = -1 then
    Exit;
    
  Timer := FActiveTimers[idx];
  
  if Timer.Status = tsPaused then
    Duration := MinutesBetween(Timer.PausedTime, Timer.StartTime) - Timer.TotalPausedMinutes
  else
    Duration := CalculateElapsedMinutes(Timer);
  
  NewEntry.ID := FNextTimeEntryID;
  Inc(FNextTimeEntryID);
  NewEntry.TaskID := Timer.TaskID;
  NewEntry.StartTime := Timer.StartTime;
  NewEntry.EndTime := Now;
  NewEntry.DurationMinutes := Duration;
  NewEntry.Status := tsCompleted;
  NewEntry.Notes := ANotes;
  NewEntry.WasPomodoro := Timer.IsPomodoro;
  NewEntry.PomodoroCount := Timer.PomodoroNumber;
  NewEntry.InterruptionCount := 0;
  SetLength(NewEntry.Tags, 0);
  
  SetLength(FTimeEntries, Length(FTimeEntries) + 1);
  FTimeEntries[High(FTimeEntries)] := NewEntry;
  
  // Remove from active timers
  if idx < High(FActiveTimers) then
    FActiveTimers[idx] := FActiveTimers[High(FActiveTimers)];
  SetLength(FActiveTimers, Length(FActiveTimers) - 1);
  
  Result := NewEntry.ID;
end;

function TTimeTrackingTaskManager.CancelTimer(ATimerID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindActiveTimerIndex(ATimerID);
  if idx = -1 then
    Exit;
    
  // Remove from active timers
  if idx < High(FActiveTimers) then
    FActiveTimers[idx] := FActiveTimers[High(FActiveTimers)];
  SetLength(FActiveTimers, Length(FActiveTimers) - 1);
  
  Result := True;
end;

function TTimeTrackingTaskManager.GetActiveTimer(ATaskID: Integer): TActiveTimer;
var
  idx: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  idx := FindTimerByTaskID(ATaskID);
  if idx <> -1 then
    Result := FActiveTimers[idx];
end;

function TTimeTrackingTaskManager.GetAllActiveTimers: TActiveTimerArray;
begin
  Result := Copy(FActiveTimers, 0, Length(FActiveTimers));
end;

function TTimeTrackingTaskManager.IsTaskTimerRunning(ATaskID: Integer): Boolean;
begin
  Result := FindTimerByTaskID(ATaskID) <> -1;
end;

function TTimeTrackingTaskManager.ConfigurePomodoro(AWorkMin, AShortBreakMin, 
  ALongBreakMin, APomodorosBeforeLong: Integer): Boolean;
begin
  Result := True;
  FPomodoroConfig.WorkDuration := AWorkMin;
  FPomodoroConfig.ShortBreakDuration := AShortBreakMin;
  FPomodoroConfig.LongBreakDuration := ALongBreakMin;
  FPomodoroConfig.PomodorosBeforeLongBreak := APomodorosBeforeLong;
end;

function TTimeTrackingTaskManager.StartPomodoroSession(ATaskID: Integer; 
  AType: TPomodoroType): Integer;
var
  NewSession: TPomodoroSession;
  Duration: Integer;
begin
  Result := -1;
  
  if GetTaskByID(ATaskID) = -1 then
    Exit;
    
  case AType of
    ptWork: Duration := FPomodoroConfig.WorkDuration;
    ptShortBreak: Duration := FPomodoroConfig.ShortBreakDuration;
    ptLongBreak: Duration := FPomodoroConfig.LongBreakDuration;
  end;
  
  NewSession.ID := FNextPomodoroID;
  Inc(FNextPomodoroID);
  NewSession.TaskID := ATaskID;
  NewSession.StartTime := Now;
  NewSession.EndTime := 0;
  NewSession.SessionType := AType;
  NewSession.PlannedDuration := Duration;
  NewSession.ActualDuration := 0;
  NewSession.WasCompleted := False;
  NewSession.WasInterrupted := False;
  NewSession.InterruptionReason := '';
  NewSession.PomodoroNumber := GetTodaysPomodoroCount + 1;
  
  SetLength(FPomodoroSessions, Length(FPomodoroSessions) + 1);
  FPomodoroSessions[High(FPomodoroSessions)] := NewSession;
  
  Result := NewSession.ID;
end;

function TTimeTrackingTaskManager.CompletePomodoroSession(ASessionID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindPomodoroIndex(ASessionID);
  if idx = -1 then
    Exit;
    
  FPomodoroSessions[idx].EndTime := Now;
  FPomodoroSessions[idx].ActualDuration := 
    MinutesBetween(FPomodoroSessions[idx].EndTime, FPomodoroSessions[idx].StartTime);
  FPomodoroSessions[idx].WasCompleted := True;
  
  Result := True;
end;

function TTimeTrackingTaskManager.InterruptPomodoro(ASessionID: Integer; 
  const AReason: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindPomodoroIndex(ASessionID);
  if idx = -1 then
    Exit;
    
  FPomodoroSessions[idx].EndTime := Now;
  FPomodoroSessions[idx].ActualDuration := 
    MinutesBetween(FPomodoroSessions[idx].EndTime, FPomodoroSessions[idx].StartTime);
  FPomodoroSessions[idx].WasInterrupted := True;
  FPomodoroSessions[idx].InterruptionReason := AReason;
  
  Result := True;
end;

function TTimeTrackingTaskManager.GetActivePomodoroSession(ATaskID: Integer): TPomodoroSession;
var
  i: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  for i := High(FPomodoroSessions) downto 0 do
    if (FPomodoroSessions[i].TaskID = ATaskID) and
       (FPomodoroSessions[i].EndTime = 0) then
    begin
      Result := FPomodoroSessions[i];
      Exit;
    end;
end;

function TTimeTrackingTaskManager.GetPomodoroHistory(ATaskID: Integer): TPomodoroSessionArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FPomodoroSessions) do
    if FPomodoroSessions[i].TaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FPomodoroSessions[i];
      Inc(Count);
    end;
end;

function TTimeTrackingTaskManager.GetTodaysPomodoroCount: Integer;
var
  i: Integer;
  Today: TDateTime;
begin
  Result := 0;
  Today := Date;
  
  for i := 0 to High(FPomodoroSessions) do
    if (DateOf(FPomodoroSessions[i].StartTime) = Today) and
       (FPomodoroSessions[i].SessionType = ptWork) and
       FPomodoroSessions[i].WasCompleted then
      Inc(Result);
end;

function TTimeTrackingTaskManager.GetPomodoroStreak: Integer;
var
  i: Integer;
  CurrentDate: TDateTime;
  FoundToday: Boolean;
begin
  Result := 0;
  CurrentDate := Date;
  
  while True do
  begin
    FoundToday := False;
    for i := 0 to High(FPomodoroSessions) do
      if (DateOf(FPomodoroSessions[i].StartTime) = CurrentDate) and
         (FPomodoroSessions[i].SessionType = ptWork) and
         FPomodoroSessions[i].WasCompleted then
      begin
        FoundToday := True;
        Break;
      end;
      
    if not FoundToday then
      Break;
      
    Inc(Result);
    CurrentDate := IncDay(CurrentDate, -1);
  end;
end;

function TTimeTrackingTaskManager.AddManualTimeEntry(ATaskID: Integer; 
  AStartTime, AEndTime: TDateTime; const ANotes: string): Integer;
var
  NewEntry: TTimeEntry;
begin
  Result := -1;
  
  if GetTaskByID(ATaskID) = -1 then
    Exit;
    
  NewEntry.ID := FNextTimeEntryID;
  Inc(FNextTimeEntryID);
  NewEntry.TaskID := ATaskID;
  NewEntry.StartTime := AStartTime;
  NewEntry.EndTime := AEndTime;
  NewEntry.DurationMinutes := MinutesBetween(AEndTime, AStartTime);
  NewEntry.Status := tsCompleted;
  NewEntry.Notes := ANotes;
  NewEntry.WasPomodoro := False;
  NewEntry.PomodoroCount := 0;
  NewEntry.InterruptionCount := 0;
  SetLength(NewEntry.Tags, 0);
  
  SetLength(FTimeEntries, Length(FTimeEntries) + 1);
  FTimeEntries[High(FTimeEntries)] := NewEntry;
  
  Result := NewEntry.ID;
end;

function TTimeTrackingTaskManager.GetTaskTimeEntries(ATaskID: Integer): TTimeEntryArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FTimeEntries) do
    if FTimeEntries[i].TaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTimeEntries[i];
      Inc(Count);
    end;
end;

function TTimeTrackingTaskManager.GetTimeEntriesForPeriod(AStart, AEnd: TDateTime): TTimeEntryArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FTimeEntries) do
    if (FTimeEntries[i].StartTime >= AStart) and (FTimeEntries[i].EndTime <= AEnd) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTimeEntries[i];
      Inc(Count);
    end;
end;

function TTimeTrackingTaskManager.GetTotalTimeForTask(ATaskID: Integer): Double;
var
  i: Integer;
  Total: Double;
begin
  Total := 0;
  for i := 0 to High(FTimeEntries) do
    if FTimeEntries[i].TaskID = ATaskID then
      Total := Total + FTimeEntries[i].DurationMinutes;
  Result := Total / 60.0; // Convert to hours
end;

function TTimeTrackingTaskManager.GetTotalTimeForPeriod(AStart, AEnd: TDateTime): Double;
var
  i: Integer;
  Total: Double;
begin
  Total := 0;
  for i := 0 to High(FTimeEntries) do
    if (FTimeEntries[i].StartTime >= AStart) and (FTimeEntries[i].EndTime <= AEnd) then
      Total := Total + FTimeEntries[i].DurationMinutes;
  Result := Total / 60.0;
end;

function TTimeTrackingTaskManager.DeleteTimeEntry(AEntryID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindTimeEntryIndex(AEntryID);
  if idx = -1 then
    Exit;
    
  if idx < High(FTimeEntries) then
    FTimeEntries[idx] := FTimeEntries[High(FTimeEntries)];
  SetLength(FTimeEntries, Length(FTimeEntries) - 1);
  
  Result := True;
end;

function TTimeTrackingTaskManager.UpdateTimeEntryNotes(AEntryID: Integer; 
  const ANotes: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindTimeEntryIndex(AEntryID);
  if idx = -1 then
    Exit;
    
  FTimeEntries[idx].Notes := ANotes;
  Result := True;
end;

function TTimeTrackingTaskManager.CreateTimeBlock(ATaskID: Integer; 
  const ABlockName: string; AStartTime: TDateTime; ADurationMin: Integer; 
  const ANotes: string): Integer;
var
  NewBlock: TTimeBlock;
begin
  Result := -1;
  
  if GetTaskByID(ATaskID) = -1 then
    Exit;
    
  NewBlock.ID := FNextBlockID;
  Inc(FNextBlockID);
  NewBlock.TaskID := ATaskID;
  NewBlock.BlockName := ABlockName;
  NewBlock.StartTime := AStartTime;
  NewBlock.EndTime := IncMinute(AStartTime, ADurationMin);
  NewBlock.DurationMinutes := ADurationMin;
  NewBlock.IsRecurring := False;
  NewBlock.RecurrencePattern := '';
  NewBlock.Color := '#3498db';
  NewBlock.Notes := ANotes;
  
  SetLength(FTimeBlocks, Length(FTimeBlocks) + 1);
  FTimeBlocks[High(FTimeBlocks)] := NewBlock;
  
  Result := NewBlock.ID;
end;

function TTimeTrackingTaskManager.CreateRecurringTimeBlock(ATaskID: Integer; 
  const ABlockName: string; AStartTime: TDateTime; ADurationMin: Integer; 
  const ARecurrence, ANotes: string): Integer;
var
  NewBlock: TTimeBlock;
begin
  Result := -1;
  
  if GetTaskByID(ATaskID) = -1 then
    Exit;
    
  NewBlock.ID := FNextBlockID;
  Inc(FNextBlockID);
  NewBlock.TaskID := ATaskID;
  NewBlock.BlockName := ABlockName;
  NewBlock.StartTime := AStartTime;
  NewBlock.EndTime := IncMinute(AStartTime, ADurationMin);
  NewBlock.DurationMinutes := ADurationMin;
  NewBlock.IsRecurring := True;
  NewBlock.RecurrencePattern := ARecurrence;
  NewBlock.Color := '#9b59b6';
  NewBlock.Notes := ANotes;
  
  SetLength(FTimeBlocks, Length(FTimeBlocks) + 1);
  FTimeBlocks[High(FTimeBlocks)] := NewBlock;
  
  Result := NewBlock.ID;
end;

function TTimeTrackingTaskManager.MoveTimeBlock(ABlockID: Integer; 
  ANewStartTime: TDateTime): Boolean;
var
  idx: Integer;
  Duration: Integer;
begin
  Result := False;
  idx := FindTimeBlockIndex(ABlockID);
  if idx = -1 then
    Exit;
    
  Duration := FTimeBlocks[idx].DurationMinutes;
  FTimeBlocks[idx].StartTime := ANewStartTime;
  FTimeBlocks[idx].EndTime := IncMinute(ANewStartTime, Duration);
  
  Result := True;
end;

function TTimeTrackingTaskManager.DeleteTimeBlock(ABlockID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindTimeBlockIndex(ABlockID);
  if idx = -1 then
    Exit;
    
  if idx < High(FTimeBlocks) then
    FTimeBlocks[idx] := FTimeBlocks[High(FTimeBlocks)];
  SetLength(FTimeBlocks, Length(FTimeBlocks) - 1);
  
  Result := True;
end;

function TTimeTrackingTaskManager.GetTimeBlocksForDay(ADate: TDateTime): TTimeBlockArray;
var
  i, Count: Integer;
  TargetDate: TDateTime;
begin
  SetLength(Result, 0);
  Count := 0;
  TargetDate := DateOf(ADate);
  
  for i := 0 to High(FTimeBlocks) do
    if DateOf(FTimeBlocks[i].StartTime) = TargetDate then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTimeBlocks[i];
      Inc(Count);
    end;
end;

function TTimeTrackingTaskManager.GetTaskTimeBlocks(ATaskID: Integer): TTimeBlockArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FTimeBlocks) do
    if FTimeBlocks[i].TaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTimeBlocks[i];
      Inc(Count);
    end;
end;

function TTimeTrackingTaskManager.FindAvailableTimeSlot(ADurationMin: Integer; 
  APreferredStart: TDateTime): TDateTime;
var
  CurrentSlot: TDateTime;
  i: Integer;
  HasConflict: Boolean;
begin
  CurrentSlot := APreferredStart;
  
  while True do
  begin
    HasConflict := False;
    
    for i := 0 to High(FTimeBlocks) do
      if (CurrentSlot >= FTimeBlocks[i].StartTime) and 
         (CurrentSlot < FTimeBlocks[i].EndTime) then
      begin
        HasConflict := True;
        CurrentSlot := FTimeBlocks[i].EndTime;
        Break;
      end;
      
    if not HasConflict then
    begin
      Result := CurrentSlot;
      Exit;
    end;
  end;
end;

function TTimeTrackingTaskManager.GetProductivityMetrics(AStartDate, 
  AEndDate: TDateTime): TProductivityMetrics;
var
  i: Integer;
  TotalMinutes, FocusedMinutes: Double;
  SessionCount, CompletedPomodoros: Integer;
  HourCounts: array[0..23] of Integer;
  MaxHour, MaxCount: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  TotalMinutes := 0;
  FocusedMinutes := 0;
  SessionCount := 0;
  CompletedPomodoros := 0;
  FillChar(HourCounts, SizeOf(HourCounts), 0);
  
  for i := 0 to High(FTimeEntries) do
    if (FTimeEntries[i].StartTime >= AStartDate) and 
       (FTimeEntries[i].EndTime <= AEndDate) then
    begin
      TotalMinutes := TotalMinutes + FTimeEntries[i].DurationMinutes;
      Inc(SessionCount);
      
      if FTimeEntries[i].WasPomodoro then
        FocusedMinutes := FocusedMinutes + FTimeEntries[i].DurationMinutes;
        
      Inc(HourCounts[HourOf(FTimeEntries[i].StartTime)]);
    end;
    
  for i := 0 to High(FPomodoroSessions) do
    if (FPomodoroSessions[i].StartTime >= AStartDate) and
       (FPomodoroSessions[i].EndTime <= AEndDate) and
       FPomodoroSessions[i].WasCompleted and
       (FPomodoroSessions[i].SessionType = ptWork) then
      Inc(CompletedPomodoros);
  
  Result.TotalTimeTracked := TotalMinutes / 60.0;
  
  if TotalMinutes > 0 then
    Result.FocusedTimePercent := (FocusedMinutes / TotalMinutes) * 100.0
  else
    Result.FocusedTimePercent := 0;
    
  if SessionCount > 0 then
    Result.AverageSessionLength := TotalMinutes / SessionCount
  else
    Result.AverageSessionLength := 0;
    
  Result.PomodorosCompleted := CompletedPomodoros;
  
  MaxHour := 0;
  MaxCount := 0;
  for i := 0 to 23 do
    if HourCounts[i] > MaxCount then
    begin
      MaxCount := HourCounts[i];
      MaxHour := i;
    end;
  Result.PeakProductivityHour := MaxHour;
end;

function TTimeTrackingTaskManager.GetTimeComparisonReport: TTimeComparisonArray;
var
  i, Count: Integer;
  Comparison: TTimeComparison;
  Task: TTask;
  ActualHours: Double;
  AllTasks: TTaskArray;
begin
  SetLength(Result, 0);
  Count := 0;
  
  AllTasks := GetAllTasks;
  for i := 0 to High(AllTasks) do
  begin
    Task := AllTasks[i];
    ActualHours := GetTotalTimeForTask(Task.ID);
    
    if (Task.EstimatedHours > 0) or (ActualHours > 0) then
    begin
      Comparison.TaskID := Task.ID;
      Comparison.TaskTitle := Task.Title;
      Comparison.EstimatedHours := Task.EstimatedHours;
      Comparison.ActualHours := ActualHours;
      Comparison.VarianceHours := ActualHours - Task.EstimatedHours;
      
      if Task.EstimatedHours > 0 then
        Comparison.Variance := (Comparison.VarianceHours / Task.EstimatedHours) * 100.0
      else
        Comparison.Variance := 0;
        
      if Abs(Comparison.Variance) <= 10 then
        Comparison.Status := 'On Target'
      else if Comparison.Variance < 0 then
        Comparison.Status := 'Under Estimate'
      else
        Comparison.Status := 'Over Estimate';
        
      SetLength(Result, Count + 1);
      Result[Count] := Comparison;
      Inc(Count);
    end;
  end;
end;

function TTimeTrackingTaskManager.GetDailyTimeLog(ADate: TDateTime): string;
var
  Entries: TTimeEntryArray;
  i: Integer;
  Total: Double;
  Task: TTask;
  TaskIdx: Integer;
  AllTasks: TTaskArray;
begin
  Result := Format('=== Daily Time Log for %s ===', 
                   [FormatDateTime('yyyy-mm-dd', ADate)]) + LineEnding;
  Result := Result + LineEnding;
  
  Entries := GetTimeEntriesForPeriod(DateOf(ADate), DateOf(ADate) + 1);
  
  if Length(Entries) = 0 then
  begin
    Result := Result + 'No time entries recorded.' + LineEnding;
    Exit;
  end;
  
  Total := 0;
  for i := 0 to High(Entries) do
  begin
    TaskIdx := GetTaskByID(Entries[i].TaskID);
    if TaskIdx <> -1 then
    begin
      AllTasks := GetAllTasks;
      Task := AllTasks[TaskIdx];
      Result := Result + Format('%s - %s: %s (%s)', [
        FormatDateTime('hh:nn', Entries[i].StartTime),
        FormatDateTime('hh:nn', Entries[i].EndTime),
        Task.Title,
        FormatDuration(Entries[i].DurationMinutes)
      ]) + LineEnding;
      
      if Entries[i].Notes <> '' then
        Result := Result + Format('  Notes: %s', [Entries[i].Notes]) + LineEnding;
        
      Total := Total + Entries[i].DurationMinutes;
    end;
  end;
  
  Result := Result + LineEnding;
  Result := Result + Format('Total time tracked: %s', [FormatDuration(Total)]) + LineEnding;
end;

function TTimeTrackingTaskManager.GetWeeklyTimeReport(AWeekStart: TDateTime): string;
var
  i, Day: Integer;
  DayStart, DayEnd: TDateTime;
  DayTotal: Double;
  WeekTotal: Double;
  DayNames: array[1..7] of string = ('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun');
begin
  Result := Format('=== Weekly Time Report (Week of %s) ===', 
                   [FormatDateTime('yyyy-mm-dd', AWeekStart)]) + LineEnding;
  Result := Result + LineEnding;
  
  WeekTotal := 0;
  
  for Day := 0 to 6 do
  begin
    DayStart := DateOf(AWeekStart) + Day;
    DayEnd := DayStart + 1;
    DayTotal := GetTotalTimeForPeriod(DayStart, DayEnd);
    
    Result := Result + Format('%s %s: %s', [
      DayNames[(Day mod 7) + 1],
      FormatDateTime('mm-dd', DayStart),
      FormatDuration(DayTotal * 60)
    ]) + LineEnding;
    
    WeekTotal := WeekTotal + DayTotal;
  end;
  
  Result := Result + LineEnding;
  Result := Result + Format('Total for week: %s', [FormatDuration(WeekTotal * 60)]) + LineEnding;
  Result := Result + Format('Daily average: %s', [FormatDuration((WeekTotal / 7) * 60)]) + LineEnding;
end;

function TTimeTrackingTaskManager.GetTaskTimeReport(ATaskID: Integer): string;
var
  TaskIdx: Integer;
  Task: TTask;
  Entries: TTimeEntryArray;
  TotalHours: Double;
  i: Integer;
  AllTasks: TTaskArray;
begin
  TaskIdx := GetTaskByID(ATaskID);
  if TaskIdx = -1 then
  begin
    Result := 'Task not found.';
    Exit;
  end;
  
  AllTasks := GetAllTasks;
  Task := AllTasks[TaskIdx];
  Entries := GetTaskTimeEntries(ATaskID);
  TotalHours := GetTotalTimeForTask(ATaskID);
  
  Result := Format('=== Time Report for Task: %s ===', [Task.Title]) + LineEnding;
  Result := Result + LineEnding;
  Result := Result + Format('Estimated time: %.2f hours', [Task.EstimatedHours]) + LineEnding;
  Result := Result + Format('Actual time: %.2f hours', [TotalHours]) + LineEnding;
  
  if Task.EstimatedHours > 0 then
  begin
    Result := Result + Format('Variance: %.2f hours (%.1f%%)', [
      TotalHours - Task.EstimatedHours,
      ((TotalHours - Task.EstimatedHours) / Task.EstimatedHours) * 100
    ]) + LineEnding;
  end;
  
  Result := Result + LineEnding;
  Result := Result + Format('Time entries: %d', [Length(Entries)]) + LineEnding;
  Result := Result + LineEnding;
  
  for i := 0 to High(Entries) do
  begin
    Result := Result + Format('%s: %s', [
      FormatDateTime('yyyy-mm-dd hh:nn', Entries[i].StartTime),
      FormatDuration(Entries[i].DurationMinutes)
    ]) + LineEnding;
    
    if Entries[i].Notes <> '' then
      Result := Result + Format('  %s', [Entries[i].Notes]) + LineEnding;
  end;
end;

function TTimeTrackingTaskManager.GetPomodoroStats(ADays: Integer): string;
var
  i, CompletedCount, InterruptedCount: Integer;
  StartDate: TDateTime;
  TotalWorkMinutes: Double;
begin
  StartDate := Now - ADays;
  CompletedCount := 0;
  InterruptedCount := 0;
  TotalWorkMinutes := 0;
  
  for i := 0 to High(FPomodoroSessions) do
    if (FPomodoroSessions[i].StartTime >= StartDate) and
       (FPomodoroSessions[i].SessionType = ptWork) then
    begin
      if FPomodoroSessions[i].WasCompleted then
      begin
        Inc(CompletedCount);
        TotalWorkMinutes := TotalWorkMinutes + FPomodoroSessions[i].ActualDuration;
      end
      else if FPomodoroSessions[i].WasInterrupted then
        Inc(InterruptedCount);
    end;
  
  Result := Format('=== Pomodoro Statistics (Last %d Days) ===', [ADays]) + LineEnding;
  Result := Result + LineEnding;
  Result := Result + Format('Completed pomodoros: %d', [CompletedCount]) + LineEnding;
  Result := Result + Format('Interrupted pomodoros: %d', [InterruptedCount]) + LineEnding;
  
  if (CompletedCount + InterruptedCount) > 0 then
    Result := Result + Format('Completion rate: %.1f%%', [
      (CompletedCount / (CompletedCount + InterruptedCount)) * 100
    ]) + LineEnding;
    
  Result := Result + Format('Total focused time: %s', [FormatDuration(TotalWorkMinutes)]) + LineEnding;
  Result := Result + Format('Current streak: %d days', [GetPomodoroStreak]) + LineEnding;
  Result := Result + Format('Today''s pomodoros: %d', [GetTodaysPomodoroCount]) + LineEnding;
end;

function TTimeTrackingTaskManager.GetProductivityByHour: string;
var
  HourCounts: array[0..23] of Double;
  i, Hour: Integer;
begin
  FillChar(HourCounts, SizeOf(HourCounts), 0);
  
  for i := 0 to High(FTimeEntries) do
  begin
    Hour := HourOf(FTimeEntries[i].StartTime);
    HourCounts[Hour] := HourCounts[Hour] + FTimeEntries[i].DurationMinutes;
  end;
  
  Result := '=== Productivity by Hour of Day ===' + LineEnding;
  Result := Result + LineEnding;
  
  for Hour := 0 to 23 do
    if HourCounts[Hour] > 0 then
      Result := Result + Format('%02d:00 - %s', [
        Hour,
        FormatDuration(HourCounts[Hour])
      ]) + LineEnding;
end;

function TTimeTrackingTaskManager.GetProductivityByDayOfWeek: string;
var
  DayCounts: array[1..7] of Double;
  i, DayOfWeek: Integer;
  DayNames: array[1..7] of string = ('Monday', 'Tuesday', 'Wednesday', 
                                      'Thursday', 'Friday', 'Saturday', 'Sunday');
begin
  FillChar(DayCounts, SizeOf(DayCounts), 0);
  
  for i := 0 to High(FTimeEntries) do
  begin
    DayOfWeek := DayOfTheWeek(FTimeEntries[i].StartTime);
    DayCounts[DayOfWeek] := DayCounts[DayOfWeek] + FTimeEntries[i].DurationMinutes;
  end;
  
  Result := '=== Productivity by Day of Week ===' + LineEnding;
  Result := Result + LineEnding;
  
  for i := 1 to 7 do
    Result := Result + Format('%s: %s', [
      DayNames[i],
      FormatDuration(DayCounts[i])
    ]) + LineEnding;
end;

function TTimeTrackingTaskManager.GetTopTimeConsumingTasks(ACount: Integer): string;
type
  TTaskTime = record
    TaskID: Integer;
    Title: string;
    TotalHours: Double;
  end;
var
  TaskTimes: array of TTaskTime;
  i, j, TaskIdx: Integer;
  Task: TTask;
  Temp: TTaskTime;
  AllTasks: TTaskArray;
begin
  SetLength(TaskTimes, 0);
  
  AllTasks := GetAllTasks;
  for i := 0 to High(AllTasks) do
  begin
    Task := AllTasks[i];
    SetLength(TaskTimes, Length(TaskTimes) + 1);
    TaskTimes[High(TaskTimes)].TaskID := Task.ID;
    TaskTimes[High(TaskTimes)].Title := Task.Title;
    TaskTimes[High(TaskTimes)].TotalHours := GetTotalTimeForTask(Task.ID);
  end;
  
  // Simple bubble sort
  for i := 0 to High(TaskTimes) - 1 do
    for j := i + 1 to High(TaskTimes) do
      if TaskTimes[j].TotalHours > TaskTimes[i].TotalHours then
      begin
        Temp := TaskTimes[i];
        TaskTimes[i] := TaskTimes[j];
        TaskTimes[j] := Temp;
      end;
  
  Result := Format('=== Top %d Time-Consuming Tasks ===', [ACount]) + LineEnding;
  Result := Result + LineEnding;
  
  for i := 0 to Min(ACount - 1, High(TaskTimes)) do
    if TaskTimes[i].TotalHours > 0 then
      Result := Result + Format('%d. %s: %.2f hours', [
        i + 1,
        TaskTimes[i].Title,
        TaskTimes[i].TotalHours
      ]) + LineEnding;
end;

function TTimeTrackingTaskManager.GetEstimateAccuracyReport: string;
var
  Comparisons: TTimeComparisonArray;
  i: Integer;
  TotalVariance, AvgVariance: Double;
  UnderCount, OverCount, OnTargetCount: Integer;
begin
  Comparisons := GetTimeComparisonReport;
  
  Result := '=== Estimate Accuracy Report ===' + LineEnding;
  Result := Result + LineEnding;
  
  if Length(Comparisons) = 0 then
  begin
    Result := Result + 'No data available.' + LineEnding;
    Exit;
  end;
  
  TotalVariance := 0;
  UnderCount := 0;
  OverCount := 0;
  OnTargetCount := 0;
  
  for i := 0 to High(Comparisons) do
  begin
    TotalVariance := TotalVariance + Abs(Comparisons[i].Variance);
    
    if Comparisons[i].Status = 'Under Estimate' then
      Inc(UnderCount)
    else if Comparisons[i].Status = 'Over Estimate' then
      Inc(OverCount)
    else
      Inc(OnTargetCount);
  end;
  
  AvgVariance := TotalVariance / Length(Comparisons);
  
  Result := Result + Format('Tasks analyzed: %d', [Length(Comparisons)]) + LineEnding;
  Result := Result + Format('Average variance: %.1f%%', [AvgVariance]) + LineEnding;
  Result := Result + LineEnding;
  Result := Result + Format('On target (±10%%): %d (%.1f%%)', [
    OnTargetCount,
    (OnTargetCount / Length(Comparisons)) * 100
  ]) + LineEnding;
  Result := Result + Format('Under-estimated: %d (%.1f%%)', [
    UnderCount,
    (UnderCount / Length(Comparisons)) * 100
  ]) + LineEnding;
  Result := Result + Format('Over-estimated: %d (%.1f%%)', [
    OverCount,
    (OverCount / Length(Comparisons)) * 100
  ]) + LineEnding;
end;

function TTimeTrackingTaskManager.FormatDuration(AMinutes: Double): string;
var
  Hours, Minutes: Integer;
begin
  Hours := Trunc(AMinutes / 60);
  Minutes := Trunc(AMinutes) mod 60;
  
  if Hours > 0 then
    Result := Format('%dh %dm', [Hours, Minutes])
  else
    Result := Format('%dm', [Minutes]);
end;

function TTimeTrackingTaskManager.TimerStatusToString(AStatus: TTimerStatus): string;
begin
  case AStatus of
    tsNotStarted: Result := 'Not Started';
    tsRunning: Result := 'Running';
    tsPaused: Result := 'Paused';
    tsCompleted: Result := 'Completed';
    tsCancelled: Result := 'Cancelled';
  else
    Result := 'Unknown';
  end;
end;

function TTimeTrackingTaskManager.PomodoroTypeToString(AType: TPomodoroType): string;
begin
  case AType of
    ptWork: Result := 'Work';
    ptShortBreak: Result := 'Short Break';
    ptLongBreak: Result := 'Long Break';
  else
    Result := 'Unknown';
  end;
end;

function TTimeTrackingTaskManager.SaveTimeTrackingDataToFile(const AFilename: string): Boolean;
var
  F: TextFile;
  i, j: Integer;
begin
  Result := False;
  try
    AssignFile(F, AFilename);
    Rewrite(F);
    
    WriteLn(F, '[TimeEntries]');
    WriteLn(F, Length(FTimeEntries));
    for i := 0 to High(FTimeEntries) do
    begin
      with FTimeEntries[i] do
      begin
        WriteLn(F, ID);
        WriteLn(F, TaskID);
        WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', StartTime));
        WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', EndTime));
        WriteLn(F, DurationMinutes:0:2);
        WriteLn(F, Ord(Status));
        WriteLn(F, Notes);
        WriteLn(F, Ord(WasPomodoro));
        WriteLn(F, PomodoroCount);
        WriteLn(F, InterruptionCount);
      end;
    end;
    
    WriteLn(F, '[PomodoroSessions]');
    WriteLn(F, Length(FPomodoroSessions));
    for i := 0 to High(FPomodoroSessions) do
    begin
      with FPomodoroSessions[i] do
      begin
        WriteLn(F, ID);
        WriteLn(F, TaskID);
        WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', StartTime));
        if EndTime > 0 then
          WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', EndTime))
        else
          WriteLn(F, '0');
        WriteLn(F, Ord(SessionType));
        WriteLn(F, PlannedDuration);
        WriteLn(F, ActualDuration);
        WriteLn(F, Ord(WasCompleted));
        WriteLn(F, Ord(WasInterrupted));
        WriteLn(F, InterruptionReason);
        WriteLn(F, PomodoroNumber);
      end;
    end;
    
    CloseFile(F);
    Result := True;
  except
    Result := False;
  end;
end;

function TTimeTrackingTaskManager.LoadTimeTrackingDataFromFile(const AFilename: string): Boolean;
var
  F: TextFile;
  Section: string;
  Count, i: Integer;
  Entry: TTimeEntry;
  Session: TPomodoroSession;
  DateStr: string;
  BoolVal: Integer;
begin
  Result := False;
  if not FileExists(AFilename) then
    Exit;
    
  try
    AssignFile(F, AFilename);
    Reset(F);
    
    while not Eof(F) do
    begin
      ReadLn(F, Section);
      
      if Section = '[TimeEntries]' then
      begin
        ReadLn(F, Count);
        SetLength(FTimeEntries, Count);
        
        for i := 0 to Count - 1 do
        begin
          ReadLn(F, Entry.ID);
          ReadLn(F, Entry.TaskID);
          ReadLn(F, DateStr);
          Entry.StartTime := StrToDateTime(DateStr);
          ReadLn(F, DateStr);
          Entry.EndTime := StrToDateTime(DateStr);
          ReadLn(F, Entry.DurationMinutes);
          ReadLn(F, BoolVal);
          Entry.Status := TTimerStatus(BoolVal);
          ReadLn(F, Entry.Notes);
          ReadLn(F, BoolVal);
          Entry.WasPomodoro := BoolVal <> 0;
          ReadLn(F, Entry.PomodoroCount);
          ReadLn(F, Entry.InterruptionCount);
          SetLength(Entry.Tags, 0);
          
          FTimeEntries[i] := Entry;
          
          if Entry.ID >= FNextTimeEntryID then
            FNextTimeEntryID := Entry.ID + 1;
        end;
      end
      else if Section = '[PomodoroSessions]' then
      begin
        ReadLn(F, Count);
        SetLength(FPomodoroSessions, Count);
        
        for i := 0 to Count - 1 do
        begin
          ReadLn(F, Session.ID);
          ReadLn(F, Session.TaskID);
          ReadLn(F, DateStr);
          Session.StartTime := StrToDateTime(DateStr);
          ReadLn(F, DateStr);
          if DateStr <> '0' then
            Session.EndTime := StrToDateTime(DateStr)
          else
            Session.EndTime := 0;
          ReadLn(F, BoolVal);
          Session.SessionType := TPomodoroType(BoolVal);
          ReadLn(F, Session.PlannedDuration);
          ReadLn(F, Session.ActualDuration);
          ReadLn(F, BoolVal);
          Session.WasCompleted := BoolVal <> 0;
          ReadLn(F, BoolVal);
          Session.WasInterrupted := BoolVal <> 0;
          ReadLn(F, Session.InterruptionReason);
          ReadLn(F, Session.PomodoroNumber);
          
          FPomodoroSessions[i] := Session;
          
          if Session.ID >= FNextPomodoroID then
            FNextPomodoroID := Session.ID + 1;
        end;
      end;
    end;
    
    CloseFile(F);
    Result := True;
  except
    Result := False;
  end;
end;

end.
