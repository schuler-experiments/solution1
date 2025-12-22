
# Gamification Module

## Overview

The **Gamification Module** (`taskmanagergamify.pas`) adds a gamification layer to the task management system, turning productivity into an engaging experience. It implements achievements, experience points (XP), user levels, streaks, leaderboards, and productivity metrics.

This module motivates users by providing visible rewards, progress tracking, and competitive elements while maintaining focus on actual task completion.

## Key Features

### 1. Achievement System

Users can unlock achievements by reaching milestones and performing specific actions. Each achievement:
- Has a unique ID and type
- Provides a point reward
- Tracks completion progress
- Records when it was unlocked

#### Achievement Types

| Achievement | Unlock Condition |
|---|---|
| **FirstTask** | Complete your first task |
| **TenTasks** | Complete 10 tasks total |
| **FiftyTasks** | Complete 50 tasks total |
| **HundredTasks** | Complete 100 tasks total |
| **PerfectWeek** | Complete all tasks created in a calendar week |
| **EarlyBird** | Complete a task before its due date |
| **SpeedDemon** | Complete a task in less than 50% of estimated time |
| **Marathoner** | Work in a single session for 4+ hours |
| **DedicatedWeek** | Maintain a 7-day completion streak |
| **DedicatedMonth** | Maintain a 30-day completion streak |
| **TeamPlayer** | Contribute to 10 different team tasks |
| **Multitasker** | Work on 5 different tasks in a single day |
| **PriorityMaster** | Complete 20 high-priority tasks |
| **Organizer** | Create 50 tasks |
| **Mentor** | Add 100 notes or comments to tasks |
| **TimeWizard** | Maintain time estimation accuracy within 10% |

```pascal
TAchievementType = (
  atFirstTask, atTenTasks, atFiftyTasks, atHundredTasks,
  atPerfectWeek, atEarlyBird, atSpeedDemon, atMarathoner,
  atDedicatedWeek, atDedicatedMonth, atTeamPlayer, atMultitasker,
  atPriorityMaster, atOrganizer, atMentor, atTimeWizard
);
```

#### Achievement Status

Each achievement can be in one of three states:

```pascal
TAchievementStatus = (
  asLocked,      // Not yet unlocked
  asUnlocked,    // Unlocked but not fully completed
  asCompleted    // Fully completed
);
```

### 2. User Level and Experience System

Users progress through levels by earning experience points (XP):

```pascal
TUserLevel = record
  Level: Integer;              // Current level (starts at 1)
  CurrentXP: Integer;          // XP earned toward next level
  XPForNextLevel: Integer;     // XP required to reach next level
  Title: string;               // "Beginner", "Intermediate", etc.
end;
```

#### Level Progression

- **Level 1**: Beginner (0 XP)
- **Level 2**: Apprentice (100 XP)
- **Level 3**: Craftsman (300 XP)
- **Level 4**: Expert (600 XP)
- **Level 5+**: Master and beyond

Each task completion awards XP based on priority:
- Low priority: 10 XP
- Medium priority: 25 XP
- High priority: 50 XP
- Critical priority: 100 XP

### 3. Productivity Metrics

The module tracks comprehensive productivity data:

```pascal
TProductivityMetrics = record
  TasksCompletedToday: Integer;        // Tasks finished today
  TasksCompletedThisWeek: Integer;     // Tasks finished this week
  TasksCompletedThisMonth: Integer;    // Tasks finished this month
  CurrentStreak: Integer;              // Consecutive days with completions
  LongestStreak: Integer;              // Best streak ever
  TotalPoints: Integer;                // Cumulative achievement points
  AverageTasksPerDay: Double;          // Historical average
  ProductivityScore: Double;           // 0-100 scale
  FocusScore: Double;                  // Based on work sessions
  VelocityTrend: string;               // "Increasing", "Stable", "Decreasing"
end;
```

#### Productivity Score Calculation

The productivity score (0-100) is calculated based on:
- Task completion rate (40%)
- Streak maintenance (20%)
- Time estimation accuracy (20%)
- Achievement progress (20%)

#### Velocity Trend

The velocity trend analyzes task completion over time:
- **Increasing**: Completing more tasks recently than historical average
- **Stable**: Consistent completion rate
- **Decreasing**: Completing fewer tasks recently than historical average

### 4. Daily Activity Tracking

The module records daily activity for historical analysis:

```pascal
TDailyActivity = record
  ActivityDate: TDateTime;      // Date of activity
  TasksCompleted: Integer;      // How many tasks completed
  HoursWorked: Double;          // Total hours worked
  PointsEarned: Integer;        // Points earned this day
  WasProductive: Boolean;       // Met minimum productivity threshold
  MoodRating: Integer;          // Optional 1-5 rating
  Notes: string;                // User notes for the day
end;
```

### 5. Leaderboard System

Tracks user rankings for competitive motivation:

```pascal
TLeaderboardEntry = record
  Rank: Integer;                // Position in leaderboard
  UserName: string;
  TotalPoints: Integer;         // Achievement points
  Level: Integer;               // Current level
  CompletedTasks: Integer;      // Total tasks completed
  CurrentStreak: Integer;       // Active streak
  ProductivityScore: Double;    // Current productivity score
end;
```

Leaderboards can be filtered by:
- Overall (all-time)
- This month
- This week
- This specific achievement

### 6. Progress Tracking

Multi-level progress tracking for user motivation:

```pascal
TProgressLevel = record
  ProgressType: string;         // "Achievements", "Level", "Streak", etc.
  CurrentProgress: Integer;     // Current value
  TargetProgress: Integer;      // Target value
  PercentComplete: Double;      // 0-100%
  ETA: TDateTime;              // Estimated completion time
end;
```

## Main Functions

### Achievement Management

```pascal
function UnlockAchievement(const UserID: string; 
                          AAchievementType: TAchievementType): Boolean;
```
Unlocks an achievement for a user. Awards XP if it's the first unlock.

```pascal
function CheckAchievementProgress(const UserID: string; 
                                 AAchievementType: TAchievementType;
                                 var AProgress: Integer): Boolean;
```
Checks current progress toward an achievement.

```pascal
function GetUserAchievements(const UserID: string): TAchievementArray;
```
Retrieves all achievements for a user with their current status.

### User Level Management

```pascal
function GetUserLevel(const UserID: string): TUserLevel;
```
Retrieves the current level information for a user.

```pascal
procedure AwardXP(const UserID: string; AXPAmount: Integer);
```
Awards experience points to a user and handles level-ups automatically.

```pascal
function CheckLevelUp(const UserID: string): Boolean;
```
Checks if a user should be leveled up and performs the level-up if needed.

### Productivity Tracking

```pascal
function GetProductivityMetrics(const UserID: string): TProductivityMetrics;
```
Retrieves comprehensive productivity metrics for a user.

```pascal
function CalculateProductivityScore(const UserID: string): Double;
```
Recalculates and returns the productivity score (0-100).

```pascal
procedure RecordDailyActivity(const UserID: string; 
                             ATasksCompleted, AHoursWorked, 
                             APointsEarned: Integer;
                             AWasProductive: Boolean);
```
Records a day's activity for historical tracking.

### Streak Management

```pascal
function GetCurrentStreak(const UserID: string): Integer;
```
Returns the current consecutive-day completion streak.

```pascal
function GetLongestStreak(const UserID: string): Integer;
```
Returns the longest streak the user has ever achieved.

```pascal
procedure UpdateStreak(const UserID: string; 
                      ATodayCompletedTasks: Integer);
```
Updates the user's streak based on today's activity.

### Leaderboard Operations

```pascal
function GetLeaderboard(const ATimeRange: string; 
                       ATopCount: Integer = 10): array of TLeaderboardEntry;
```
Retrieves top users for the specified time range.

**Time ranges**: 'AllTime', 'Month', 'Week'

```pascal
function GetUserRank(const UserID: string; 
                     const ATimeRange: string): Integer;
```
Returns a specific user's rank in the leaderboard.

### Progress and Goals

```pascal
function GetProgressLevels(const UserID: string): array of TProgressLevel;
```
Returns progress tracking for all active goals.

```pascal
function GetNextMilestone(const UserID: string): TProgressLevel;
```
Returns the next milestone the user is working toward.

## Usage Examples

### Example 1: Recording Task Completion and Awarding Achievements

```pascal
procedure CompleteTask(Manager: TTaskManager; const UserID: string; 
                      TaskID: Integer);
var
  Task: TTask;
  XPToAward: Integer;
begin
  // Get the task
  Task := Manager.GetTaskByID(TaskID);
  
  // Update status
  Manager.UpdateTaskStatus(TaskID, tsCompleted);
  Manager.UpdateTaskActualHours(TaskID, 2.5);
  
  // Calculate XP based on priority
  case Task.Priority of
    tpLow:      XPToAward := 10;
    tpMedium:   XPToAward := 25;
    tpHigh:     XPToAward := 50;
    tpCritical: XPToAward := 100;
  end;
  
  // Award XP
  AwardXP(UserID, XPToAward);
  
  // Check for achievements
  if CheckAchievementProgress(UserID, atEarlyBird, XPToAward) then
    UnlockAchievement(UserID, atEarlyBird);
    
  // Update streak
  UpdateStreak(UserID, Manager.GetTaskCountByStatus(tsCompleted));
end;
```

### Example 2: Displaying User Progress

```pascal
procedure DisplayUserStats(const UserID: string);
var
  Level: TUserLevel;
  Metrics: TProductivityMetrics;
  PctToNext: Double;
begin
  Level := GetUserLevel(UserID);
  Metrics := GetProductivityMetrics(UserID);
  
  WriteLn('=== User Statistics ===');
  WriteLn('Level: ', Level.Level, ' (', Level.Title, ')');
  
  // Calculate progress to next level
  PctToNext := (Level.CurrentXP / Level.XPForNextLevel) * 100;
  WriteLn('Progress to next level: ', PctToNext:0:1, '%');
  
  WriteLn('Productivity Score: ', Metrics.ProductivityScore:0:1, '/100');
  WriteLn('Current Streak: ', Metrics.CurrentStreak, ' days');
  WriteLn('Longest Streak: ', Metrics.LongestStreak, ' days');
  WriteLn('Total Points: ', Metrics.TotalPoints);
  WriteLn('Tasks Completed This Month: ', Metrics.TasksCompletedThisMonth);
  WriteLn('Average Tasks/Day: ', Metrics.AverageTasksPerDay:0:1);
  WriteLn('Velocity Trend: ', Metrics.VelocityTrend);
end;
```

### Example 3: Checking Achievement Status

```pascal
procedure ShowAchievementStatus(const UserID: string);
var
  Achievements: TAchievementArray;
  i: Integer;
  StatusStr: string;
begin
  Achievements := GetUserAchievements(UserID);
  
  WriteLn('=== Achievements ===');
  for i := 0 to Length(Achievements) - 1 do
  begin
    case Achievements[i].Status of
      asLocked:    StatusStr := '[LOCKED]';
      asUnlocked:  StatusStr := '[UNLOCKED]';
      asCompleted: StatusStr := '[COMPLETED]';
    end;
    
    WriteLn(StatusStr, ' ', Achievements[i].Title);
    WriteLn('  Description: ', Achievements[i].Description);
    WriteLn('  Progress: ', Achievements[i].Progress, '/', 
            Achievements[i].ProgressMax);
    WriteLn('  Points: ', Achievements[i].PointsAwarded);
  end;
end;
```

## Integration Points

The Gamification module integrates with:

- **taskmanager.pas**: Uses core task data
- **taskmanageradvanced.pas**: Uses advanced filtering
- **taskmanagerenhanced.pas**: Uses enhanced features
- **taskmanagerteam.pas**: For team-based achievements
- **taskmanagernotifications.pas**: Sends achievement notifications
- **taskmanagerfocus.pas**: Uses focus session data

## Design Patterns

### Progress Calculation
The module uses a "percentage complete" pattern for all progress tracking, making it easy to create progress bars and visual indicators.

### Streak Maintenance
Streaks are maintained by checking daily for at least one completed task. A missed day breaks the streak but doesn't erase the "longest streak" record.

### Level Progression
Uses exponential XP requirements: each level requires more XP than the previous one, creating a natural difficulty curve.

### Achievement Rarity
Critical achievements (PerfectWeek, DedicatedMonth) are more rewarding and require greater effort, making them more prestigious.

## Performance Considerations

- Achievement checking happens on task completion
- Leaderboards can be cached and updated periodically
- Productivity metrics can be calculated incrementally
- Daily activity records should be archived after 1 year to maintain performance

## Best Practices

1. **Celebrate achievements**: Provide immediate visual feedback when users unlock achievements
2. **Show next milestone**: Always display what the user needs to do to reach the next achievement
3. **Consider reset periods**: Weekly/monthly metrics motivate recurring engagement
4. **Balance difficulty**: Ensure achievements are challenging but achievable
5. **Monitor burnout**: If productivity scores drop significantly, alert the user
6. **Use friendly language**: Level titles ("Beginner", "Expert") are more engaging than numbers

## Related Documentation

- [README.md](README.md) - System overview
- [FEATURES_SUMMARY.md](FEATURES_SUMMARY.md) - Feature overview
- [README_NOTIFICATIONS.md](README_NOTIFICATIONS.md) - How achievements trigger notifications
- [README_SMART_FEATURES.md](README_SMART_FEATURES.md) - Intelligence-based achievements
