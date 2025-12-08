
unit taskmanagergamify;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math,
  taskmanager, taskmanagerext, taskmanageradvanced, 
  taskmanagerenhanced, taskmanagerteam;

type
  // Achievement types
  TAchievementType = (
    atFirstTask,           // Complete first task
    atTenTasks,            // Complete 10 tasks
    atFiftyTasks,          // Complete 50 tasks
    atHundredTasks,        // Complete 100 tasks
    atPerfectWeek,         // Complete all tasks in a week
    atEarlyBird,           // Complete task before due date
    atSpeedDemon,          // Complete task in under 50% estimated time
    atMarathoner,          // Work session over 4 hours
    atDedicatedWeek,       // 7 day streak
    atDedicatedMonth,      // 30 day streak
    atTeamPlayer,          // Help on 10 different tasks
    atMultitasker,         // Work on 5 tasks in one day
    atPriorityMaster,      // Complete 20 high priority tasks
    atOrganizer,           // Create 50 tasks
    atMentor,              // Add 100 notes/comments
    atTimeWizard           // Accurate time estimation (within 10%)
  );

  TAchievementStatus = (asLocked, asUnlocked, asCompleted);

  TAchievement = record
    AchievementID: Integer;
    AchievementType: TAchievementType;
    Title: string;
    Description: string;
    PointsAwarded: Integer;
    UnlockedDate: TDateTime;
    Status: TAchievementStatus;
    Progress: Integer;        // Current progress toward achievement
    ProgressMax: Integer;     // Max progress needed
  end;

  TAchievementArray = array of TAchievement;

  // User level and experience
  TUserLevel = record
    Level: Integer;
    CurrentXP: Integer;
    XPForNextLevel: Integer;
    Title: string;             // "Beginner", "Intermediate", etc.
  end;

  // Productivity metrics
  TProductivityMetrics = record
    TasksCompletedToday: Integer;
    TasksCompletedThisWeek: Integer;
    TasksCompletedThisMonth: Integer;
    CurrentStreak: Integer;       // Consecutive days with completed tasks
    LongestStreak: Integer;
    TotalPoints: Integer;
    AverageTasksPerDay: Double;
    ProductivityScore: Double;    // 0-100 scale
    FocusScore: Double;           // Based on work sessions
    VelocityTrend: string;        // "Increasing", "Stable", "Decreasing"
  end;

  // Daily activity
  TDailyActivity = record
    ActivityDate: TDateTime;
    TasksCompleted: Integer;
    HoursWorked: Double;
    PointsEarned: Integer;
    WasProductive: Boolean;      // Met minimum threshold
  end;

  TDailyActivityArray = array of TDailyActivity;

  // Points for different actions
  TPointSystem = record
    TaskCompleted: Integer;
    HighPriorityTask: Integer;
    TaskCompletedEarly: Integer;
    TaskCompletedOnTime: Integer;
    WorkSessionCompleted: Integer;
    NoteAdded: Integer;
    DependencyResolved: Integer;
    StreakBonus: Integer;         // Multiplier for streaks
  end;

  TGamifiedTaskManager = class(TTeamTaskManager)
  private
    FAchievements: TAchievementArray;
    FDailyActivities: TDailyActivityArray;
    FUserLevel: TUserLevel;
    FProductivityMetrics: TProductivityMetrics;
    FPointSystem: TPointSystem;
    FLastActivityDate: TDateTime;
    
    procedure InitializeAchievements;
    procedure InitializePointSystem;
    procedure UpdateUserLevel;
    procedure CheckAchievements;
    procedure CheckAchievement(AType: TAchievementType);
    procedure UnlockAchievement(AType: TAchievementType);
    function FindAchievementIndex(AType: TAchievementType): Integer;
    function CalculateProductivityScore: Double;
    function CalculateFocusScore: Double;
    procedure UpdateDailyActivity;
    procedure UpdateStreaks;
    function GetActivityIndex(ADate: TDateTime): Integer;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Gamified task operations
    function CompleteTaskWithRewards(ATaskID: Integer): Integer;
    function CompleteWorkSessionWithRewards(ASessionID: Integer): Integer;
    function AddNoteWithRewards(ATaskID: Integer; const AAuthor, AContent: string): Integer;
    
    // Achievement management
    function GetAllAchievements: TAchievementArray;
    function GetUnlockedAchievements: TAchievementArray;
    function GetLockedAchievements: TAchievementArray;
    function GetAchievementProgress(AType: TAchievementType): string;
    
    // Level and XP
    function GetCurrentLevel: TUserLevel;
    function GetLevelTitle(ALevel: Integer): string;
    function CalculateXPForLevel(ALevel: Integer): Integer;
    function AwardPoints(APoints: Integer): Integer;
    
    // Productivity metrics
    function GetProductivityMetrics: TProductivityMetrics;
    function GetProductivityReport: string;
    function GetMotivationalMessage: string;
    function GetProductivityTrend: string;
    
    // Daily activity
    function GetDailyActivities(ADays: Integer): TDailyActivityArray;
    function GetActivityCalendar: string;
    function GetStreakInfo: string;
    
    // Leaderboard simulation (for single user)
    function GetPersonalBests: string;
    function GetMilestones: string;
    
    // String conversions
    function AchievementTypeToString(AType: TAchievementType): string;
    function AchievementStatusToString(AStatus: TAchievementStatus): string;
    function AchievementToString(const AAchievement: TAchievement): string;
  end;

implementation

constructor TGamifiedTaskManager.Create;
begin
  inherited Create;
  SetLength(FAchievements, 0);
  SetLength(FDailyActivities, 0);
  
  FUserLevel.Level := 1;
  FUserLevel.CurrentXP := 0;
  FUserLevel.XPForNextLevel := 100;
  FUserLevel.Title := 'Beginner';
  
  FillChar(FProductivityMetrics, SizeOf(FProductivityMetrics), 0);
  FLastActivityDate := 0;
  
  InitializeAchievements;
  InitializePointSystem;
end;

destructor TGamifiedTaskManager.Destroy;
begin
  SetLength(FAchievements, 0);
  SetLength(FDailyActivities, 0);
  inherited Destroy;
end;

procedure TGamifiedTaskManager.InitializeAchievements;
var
  Achievement: TAchievement;
begin
  // First Task
  Achievement.AchievementID := 1;
  Achievement.AchievementType := atFirstTask;
  Achievement.Title := 'Getting Started';
  Achievement.Description := 'Complete your first task';
  Achievement.PointsAwarded := 10;
  Achievement.Status := asLocked;
  Achievement.Progress := 0;
  Achievement.ProgressMax := 1;
  SetLength(FAchievements, Length(FAchievements) + 1);
  FAchievements[High(FAchievements)] := Achievement;
  
  // Ten Tasks
  Achievement.AchievementID := 2;
  Achievement.AchievementType := atTenTasks;
  Achievement.Title := 'Task Warrior';
  Achievement.Description := 'Complete 10 tasks';
  Achievement.PointsAwarded := 50;
  Achievement.Status := asLocked;
  Achievement.Progress := 0;
  Achievement.ProgressMax := 10;
  SetLength(FAchievements, Length(FAchievements) + 1);
  FAchievements[High(FAchievements)] := Achievement;
  
  // Fifty Tasks
  Achievement.AchievementID := 3;
  Achievement.AchievementType := atFiftyTasks;
  Achievement.Title := 'Productivity Champion';
  Achievement.Description := 'Complete 50 tasks';
  Achievement.PointsAwarded := 200;
  Achievement.Status := asLocked;
  Achievement.Progress := 0;
  Achievement.ProgressMax := 50;
  SetLength(FAchievements, Length(FAchievements) + 1);
  FAchievements[High(FAchievements)] := Achievement;
  
  // Early Bird
  Achievement.AchievementID := 4;
  Achievement.AchievementType := atEarlyBird;
  Achievement.Title := 'Early Bird';
  Achievement.Description := 'Complete a task before its due date';
  Achievement.PointsAwarded := 25;
  Achievement.Status := asLocked;
  Achievement.Progress := 0;
  Achievement.ProgressMax := 1;
  SetLength(FAchievements, Length(FAchievements) + 1);
  FAchievements[High(FAchievements)] := Achievement;
  
  // 7 Day Streak
  Achievement.AchievementID := 5;
  Achievement.AchievementType := atDedicatedWeek;
  Achievement.Title := 'Week Warrior';
  Achievement.Description := 'Maintain a 7-day productivity streak';
  Achievement.PointsAwarded := 100;
  Achievement.Status := asLocked;
  Achievement.Progress := 0;
  Achievement.ProgressMax := 7;
  SetLength(FAchievements, Length(FAchievements) + 1);
  FAchievements[High(FAchievements)] := Achievement;
  
  // Priority Master
  Achievement.AchievementID := 6;
  Achievement.AchievementType := atPriorityMaster;
  Achievement.Title := 'Priority Master';
  Achievement.Description := 'Complete 20 high-priority tasks';
  Achievement.PointsAwarded := 150;
  Achievement.Status := asLocked;
  Achievement.Progress := 0;
  Achievement.ProgressMax := 20;
  SetLength(FAchievements, Length(FAchievements) + 1);
  FAchievements[High(FAchievements)] := Achievement;
end;

procedure TGamifiedTaskManager.InitializePointSystem;
begin
  FPointSystem.TaskCompleted := 10;
  FPointSystem.HighPriorityTask := 20;
  FPointSystem.TaskCompletedEarly := 15;
  FPointSystem.TaskCompletedOnTime := 10;
  FPointSystem.WorkSessionCompleted := 5;
  FPointSystem.NoteAdded := 2;
  FPointSystem.DependencyResolved := 10;
  FPointSystem.StreakBonus := 2; // Multiplier
end;

function TGamifiedTaskManager.CompleteTaskWithRewards(ATaskID: Integer): Integer;
var
  TaskIndex: Integer;
  Task: TTask;
  PointsEarned: Integer;
  DaysBeforeDue: Integer;
begin
  Result := 0;
  PointsEarned := FPointSystem.TaskCompleted;
  
  TaskIndex := GetTaskByID(ATaskID);
  if TaskIndex = -1 then Exit;
  
  Task := GetAllTasks[TaskIndex];
  
  // Update task status
  if not UpdateTaskStatus(ATaskID, tsCompleted) then Exit;
  
  // Award points based on priority
  if Task.Priority = tpHigh then
    PointsEarned := PointsEarned + FPointSystem.HighPriorityTask;
  
  // Check if completed early
  if Task.DueDate > 0 then
  begin
    DaysBeforeDue := DaysBetween(Now, Task.DueDate);
    if Now < Task.DueDate then
    begin
      PointsEarned := PointsEarned + FPointSystem.TaskCompletedEarly;
    end
    else if SameDate(Now, Task.DueDate) then
    begin
      PointsEarned := PointsEarned + FPointSystem.TaskCompletedOnTime;
    end;
  end;
  
  // Apply streak bonus
  if FProductivityMetrics.CurrentStreak > 0 then
    PointsEarned := PointsEarned * (1 + FProductivityMetrics.CurrentStreak div FPointSystem.StreakBonus);
  
  Result := AwardPoints(PointsEarned);
  
  // Update metrics
  UpdateDailyActivity;
  Inc(FProductivityMetrics.TasksCompletedToday);
  Inc(FProductivityMetrics.TasksCompletedThisWeek);
  Inc(FProductivityMetrics.TasksCompletedThisMonth);
  
  UpdateStreaks;
  CheckAchievements;
end;

function TGamifiedTaskManager.CompleteWorkSessionWithRewards(ASessionID: Integer): Integer;
begin
  Result := 0;
  if EndWorkSession(ASessionID, True) then
  begin
    Result := AwardPoints(FPointSystem.WorkSessionCompleted);
    CheckAchievements;
  end;
end;

function TGamifiedTaskManager.AddNoteWithRewards(ATaskID: Integer; 
  const AAuthor, AContent: string): Integer;
begin
  Result := AddNote(ATaskID, AAuthor, AContent, 'General');
  if Result <> -1 then
  begin
    AwardPoints(FPointSystem.NoteAdded);
    CheckAchievements;
  end;
end;

procedure TGamifiedTaskManager.UpdateUserLevel;
var
  NewLevel: Integer;
begin
  NewLevel := FUserLevel.Level;
  
  while FUserLevel.CurrentXP >= FUserLevel.XPForNextLevel do
  begin
    Inc(NewLevel);
    FUserLevel.CurrentXP := FUserLevel.CurrentXP - FUserLevel.XPForNextLevel;
    FUserLevel.XPForNextLevel := CalculateXPForLevel(NewLevel);
  end;
  
  if NewLevel <> FUserLevel.Level then
  begin
    FUserLevel.Level := NewLevel;
    FUserLevel.Title := GetLevelTitle(NewLevel);
  end;
end;

function TGamifiedTaskManager.CalculateXPForLevel(ALevel: Integer): Integer;
begin
  // XP required increases exponentially
  Result := Round(100 * Power(1.5, ALevel - 1));
end;

function TGamifiedTaskManager.GetLevelTitle(ALevel: Integer): string;
begin
  case ALevel of
    1..5: Result := 'Beginner';
    6..10: Result := 'Novice';
    11..15: Result := 'Intermediate';
    16..20: Result := 'Advanced';
    21..25: Result := 'Expert';
    26..30: Result := 'Master';
    else Result := 'Grand Master';
  end;
end;

function TGamifiedTaskManager.AwardPoints(APoints: Integer): Integer;
begin
  Inc(FUserLevel.CurrentXP, APoints);
  Inc(FProductivityMetrics.TotalPoints, APoints);
  UpdateUserLevel;
  Result := APoints;
end;

procedure TGamifiedTaskManager.CheckAchievements;
begin
  CheckAchievement(atFirstTask);
  CheckAchievement(atTenTasks);
  CheckAchievement(atFiftyTasks);
  CheckAchievement(atEarlyBird);
  CheckAchievement(atDedicatedWeek);
  CheckAchievement(atPriorityMaster);
end;

procedure TGamifiedTaskManager.CheckAchievement(AType: TAchievementType);
var
  Idx: Integer;
  CompletedTasks: Integer;
  HighPriorityCompleted: Integer;
  Tasks: TTaskArray;
  i: Integer;
begin
  Idx := FindAchievementIndex(AType);
  if Idx = -1 then Exit;
  if FAchievements[Idx].Status = asCompleted then Exit;
  
  case AType of
    atFirstTask:
      begin
        CompletedTasks := GetCompletedCount;
        FAchievements[Idx].Progress := CompletedTasks;
        if CompletedTasks >= 1 then
          UnlockAchievement(AType);
      end;
      
    atTenTasks:
      begin
        CompletedTasks := GetCompletedCount;
        FAchievements[Idx].Progress := CompletedTasks;
        if CompletedTasks >= 10 then
          UnlockAchievement(AType);
      end;
      
    atFiftyTasks:
      begin
        CompletedTasks := GetCompletedCount;
        FAchievements[Idx].Progress := CompletedTasks;
        if CompletedTasks >= 50 then
          UnlockAchievement(AType);
      end;
      
    atPriorityMaster:
      begin
        Tasks := FilterByStatus(tsCompleted);
        HighPriorityCompleted := 0;
        for i := 0 to High(Tasks) do
          if Tasks[i].Priority = tpHigh then
            Inc(HighPriorityCompleted);
        FAchievements[Idx].Progress := HighPriorityCompleted;
        if HighPriorityCompleted >= 20 then
          UnlockAchievement(AType);
      end;
      
    atDedicatedWeek:
      begin
        FAchievements[Idx].Progress := FProductivityMetrics.CurrentStreak;
        if FProductivityMetrics.CurrentStreak >= 7 then
          UnlockAchievement(AType);
      end;
  end;
end;

procedure TGamifiedTaskManager.UnlockAchievement(AType: TAchievementType);
var
  Idx: Integer;
begin
  Idx := FindAchievementIndex(AType);
  if Idx = -1 then Exit;
  
  if FAchievements[Idx].Status <> asCompleted then
  begin
    FAchievements[Idx].Status := asCompleted;
    FAchievements[Idx].UnlockedDate := Now;
    AwardPoints(FAchievements[Idx].PointsAwarded);
  end;
end;

function TGamifiedTaskManager.FindAchievementIndex(AType: TAchievementType): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FAchievements) do
    if FAchievements[i].AchievementType = AType then
    begin
      Result := i;
      Break;
    end;
end;

procedure TGamifiedTaskManager.UpdateDailyActivity;
var
  Today: TDateTime;
  Idx: Integer;
begin
  Today := Date;
  Idx := GetActivityIndex(Today);
  
  if Idx = -1 then
  begin
    // Create new activity record
    SetLength(FDailyActivities, Length(FDailyActivities) + 1);
    Idx := High(FDailyActivities);
    FDailyActivities[Idx].ActivityDate := Today;
    FDailyActivities[Idx].TasksCompleted := 0;
    FDailyActivities[Idx].HoursWorked := 0;
    FDailyActivities[Idx].PointsEarned := 0;
    FDailyActivities[Idx].WasProductive := False;
  end;
  
  Inc(FDailyActivities[Idx].TasksCompleted);
  FDailyActivities[Idx].PointsEarned := FProductivityMetrics.TotalPoints;
  FDailyActivities[Idx].WasProductive := FDailyActivities[Idx].TasksCompleted > 0;
end;

procedure TGamifiedTaskManager.UpdateStreaks;
var
  Today: TDateTime;
  Yesterday: TDateTime;
  YesterdayIdx: Integer;
begin
  Today := Date;
  Yesterday := Today - 1;
  
  if FLastActivityDate = 0 then
  begin
    FProductivityMetrics.CurrentStreak := 1;
    FLastActivityDate := Today;
  end
  else if SameDate(FLastActivityDate, Today) then
  begin
    // Same day, streak continues
  end
  else if SameDate(FLastActivityDate, Yesterday) then
  begin
    // Consecutive day
    Inc(FProductivityMetrics.CurrentStreak);
    FLastActivityDate := Today;
    if FProductivityMetrics.CurrentStreak > FProductivityMetrics.LongestStreak then
      FProductivityMetrics.LongestStreak := FProductivityMetrics.CurrentStreak;
  end
  else
  begin
    // Streak broken
    FProductivityMetrics.CurrentStreak := 1;
    FLastActivityDate := Today;
  end;
end;

function TGamifiedTaskManager.GetActivityIndex(ADate: TDateTime): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FDailyActivities) do
    if SameDate(FDailyActivities[i].ActivityDate, ADate) then
    begin
      Result := i;
      Break;
    end;
end;

function TGamifiedTaskManager.CalculateProductivityScore: Double;
var
  CompletionRate: Double;
  StreakBonus: Double;
  VelocityScore: Double;
begin
  CompletionRate := GetCompletionRate;
  StreakBonus := Min(FProductivityMetrics.CurrentStreak * 2, 30);
  VelocityScore := Min(FProductivityMetrics.TasksCompletedThisWeek * 2, 40);
  
  Result := Min(CompletionRate * 0.3 + StreakBonus + VelocityScore, 100);
end;

function TGamifiedTaskManager.CalculateFocusScore: Double;
var
  Sessions: TWorkSessionArray;
  TotalMinutes: Integer;
  i: Integer;
begin
  Sessions := GetAllWorkSessions;
  TotalMinutes := 0;
  
  for i := 0 to High(Sessions) do
    if Sessions[i].EndTime > 0 then
      TotalMinutes := TotalMinutes + MinutesBetween(Sessions[i].EndTime, Sessions[i].StartTime);
  
  // Focus score based on work session minutes (max 100 for 8+ hours)
  Result := Min((TotalMinutes / 480.0) * 100, 100);
end;

function TGamifiedTaskManager.GetProductivityMetrics: TProductivityMetrics;
begin
  FProductivityMetrics.ProductivityScore := CalculateProductivityScore;
  FProductivityMetrics.FocusScore := CalculateFocusScore;
  
  if Length(FDailyActivities) > 0 then
    FProductivityMetrics.AverageTasksPerDay := 
      GetCompletedCount / Max(Length(FDailyActivities), 1)
  else
    FProductivityMetrics.AverageTasksPerDay := 0;
    
  Result := FProductivityMetrics;
end;

function TGamifiedTaskManager.GetAllAchievements: TAchievementArray;
begin
  Result := Copy(FAchievements, 0, Length(FAchievements));
end;

function TGamifiedTaskManager.GetUnlockedAchievements: TAchievementArray;
var
  i, Count: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  
  for i := 0 to High(FAchievements) do
    if FAchievements[i].Status = asCompleted then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FAchievements[i];
      Inc(Count);
    end;
end;

function TGamifiedTaskManager.GetLockedAchievements: TAchievementArray;
var
  i, Count: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  
  for i := 0 to High(FAchievements) do
    if FAchievements[i].Status = asLocked then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FAchievements[i];
      Inc(Count);
    end;
end;

function TGamifiedTaskManager.GetCurrentLevel: TUserLevel;
begin
  Result := FUserLevel;
end;

function TGamifiedTaskManager.GetProductivityReport: string;
var
  Metrics: TProductivityMetrics;
begin
  Metrics := GetProductivityMetrics;
  
  Result := 'Productivity Report:' + LineEnding;
  Result := Result + '==================' + LineEnding;
  Result := Result + Format('Overall Productivity Score: %.1f/100', [Metrics.ProductivityScore]) + LineEnding;
  Result := Result + Format('Focus Score: %.1f/100', [Metrics.FocusScore]) + LineEnding;
  Result := Result + Format('Tasks Completed Today: %d', [Metrics.TasksCompletedToday]) + LineEnding;
  Result := Result + Format('Tasks Completed This Week: %d', [Metrics.TasksCompletedThisWeek]) + LineEnding;
  Result := Result + Format('Current Streak: %d days', [Metrics.CurrentStreak]) + LineEnding;
  Result := Result + Format('Longest Streak: %d days', [Metrics.LongestStreak]) + LineEnding;
  Result := Result + Format('Total Points: %d', [Metrics.TotalPoints]) + LineEnding;
  Result := Result + Format('Average Tasks/Day: %.2f', [Metrics.AverageTasksPerDay]) + LineEnding;
end;

function TGamifiedTaskManager.GetMotivationalMessage: string;
var
  Score: Double;
begin
  Score := CalculateProductivityScore;
  
  if Score >= 90 then
    Result := '🌟 Outstanding! You''re crushing it!'
  else if Score >= 75 then
    Result := '🎯 Great work! Keep up the momentum!'
  else if Score >= 60 then
    Result := '👍 Good progress! You''re doing well!'
  else if Score >= 40 then
    Result := '💪 Keep going! You''ve got this!'
  else
    Result := '🚀 Let''s get started! Small steps lead to big wins!';
end;

function TGamifiedTaskManager.GetStreakInfo: string;
begin
  Result := Format('Current Streak: %d days | Longest Streak: %d days',
    [FProductivityMetrics.CurrentStreak, FProductivityMetrics.LongestStreak]);
    
  if FProductivityMetrics.CurrentStreak >= 7 then
    Result := Result + ' 🔥'
  else if FProductivityMetrics.CurrentStreak >= 3 then
    Result := Result + ' ⚡';
end;

function TGamifiedTaskManager.AchievementTypeToString(AType: TAchievementType): string;
begin
  case AType of
    atFirstTask: Result := 'First Task';
    atTenTasks: Result := 'Ten Tasks';
    atFiftyTasks: Result := 'Fifty Tasks';
    atHundredTasks: Result := 'Hundred Tasks';
    atPerfectWeek: Result := 'Perfect Week';
    atEarlyBird: Result := 'Early Bird';
    atSpeedDemon: Result := 'Speed Demon';
    atMarathoner: Result := 'Marathoner';
    atDedicatedWeek: Result := 'Dedicated Week';
    atDedicatedMonth: Result := 'Dedicated Month';
    atTeamPlayer: Result := 'Team Player';
    atMultitasker: Result := 'Multitasker';
    atPriorityMaster: Result := 'Priority Master';
    atOrganizer: Result := 'Organizer';
    atMentor: Result := 'Mentor';
    atTimeWizard: Result := 'Time Wizard';
  else
    Result := 'Unknown';
  end;
end;

function TGamifiedTaskManager.AchievementStatusToString(AStatus: TAchievementStatus): string;
begin
  case AStatus of
    asLocked: Result := 'Locked';
    asUnlocked: Result := 'Unlocked';
    asCompleted: Result := 'Completed';
  else
    Result := 'Unknown';
  end;
end;

function TGamifiedTaskManager.AchievementToString(const AAchievement: TAchievement): string;
var
  StatusIcon: string;
begin
  if AAchievement.Status = asCompleted then
    StatusIcon := '✓'
  else
    StatusIcon := '○';
    
  Result := Format('%s [%s] %s - %s (%d/%d) [%d points]',
    [StatusIcon, AchievementStatusToString(AAchievement.Status),
     AAchievement.Title, AAchievement.Description,
     AAchievement.Progress, AAchievement.ProgressMax,
     AAchievement.PointsAwarded]);
end;

function TGamifiedTaskManager.GetAchievementProgress(AType: TAchievementType): string;
var
  Idx: Integer;
begin
  Idx := FindAchievementIndex(AType);
  if Idx = -1 then
    Result := 'Achievement not found'
  else
    Result := AchievementToString(FAchievements[Idx]);
end;

function TGamifiedTaskManager.GetDailyActivities(ADays: Integer): TDailyActivityArray;
var
  StartDate: TDateTime;
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  StartDate := Date - ADays;
  
  for i := 0 to High(FDailyActivities) do
    if FDailyActivities[i].ActivityDate >= StartDate then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FDailyActivities[i];
      Inc(Count);
    end;
end;

function TGamifiedTaskManager.GetActivityCalendar: string;
var
  i: Integer;
  Activities: TDailyActivityArray;
begin
  Activities := GetDailyActivities(30);
  Result := 'Last 30 Days Activity:' + LineEnding;
  Result := Result + StringOfChar('-', 40) + LineEnding;
  
  for i := 0 to High(Activities) do
  begin
    Result := Result + Format('%s: %d tasks, %.1f hours',
      [DateToStr(Activities[i].ActivityDate),
       Activities[i].TasksCompleted,
       Activities[i].HoursWorked]);
       
    if Activities[i].WasProductive then
      Result := Result + ' ✓';
      
    Result := Result + LineEnding;
  end;
end;

function TGamifiedTaskManager.GetPersonalBests: string;
begin
  Result := 'Personal Best Records:' + LineEnding;
  Result := Result + '=====================' + LineEnding;
  Result := Result + Format('Longest Streak: %d days', [FProductivityMetrics.LongestStreak]) + LineEnding;
  Result := Result + Format('Total Points Earned: %d', [FProductivityMetrics.TotalPoints]) + LineEnding;
  Result := Result + Format('Total Tasks Completed: %d', [GetCompletedCount]) + LineEnding;
  Result := Result + Format('Current Level: %d (%s)', [FUserLevel.Level, FUserLevel.Title]) + LineEnding;
  Result := Result + Format('Achievements Unlocked: %d/%d',
    [Length(GetUnlockedAchievements), Length(FAchievements)]) + LineEnding;
end;

function TGamifiedTaskManager.GetMilestones: string;
var
  Unlocked: TAchievementArray;
  i: Integer;
begin
  Unlocked := GetUnlockedAchievements;
  
  Result := 'Unlocked Achievements:' + LineEnding;
  Result := Result + '=====================' + LineEnding;
  
  if Length(Unlocked) = 0 then
    Result := Result + 'No achievements unlocked yet. Start completing tasks!' + LineEnding
  else
    for i := 0 to High(Unlocked) do
      Result := Result + AchievementToString(Unlocked[i]) + LineEnding;
end;

function TGamifiedTaskManager.GetProductivityTrend: string;
var
  ThisWeek, LastWeek: Integer;
begin
  ThisWeek := FProductivityMetrics.TasksCompletedThisWeek;
  // Simplified - in real implementation would track last week
  LastWeek := ThisWeek - 2;
  
  if ThisWeek > LastWeek then
    Result := 'Trending Up 📈'
  else if ThisWeek < LastWeek then
    Result := 'Trending Down 📉'
  else
    Result := 'Stable ➡️';
end;

end.
