
# Time Tracking & Pomodoro Integration

## Overview

The Time Tracking module adds comprehensive time management capabilities to the task manager, including:

- **Timer Management**: Start, stop, pause, and resume timers for tasks
- **Pomodoro Technique**: Built-in Pomodoro timer with customizable intervals
- **Time Logging**: Automatic and manual time entry tracking
- **Time Blocking**: Schedule dedicated focus time for tasks
- **Analytics**: Detailed productivity metrics and reports
- **Estimate Accuracy**: Track actual vs estimated time

## Features

### 1. Timer Management

Track time spent on tasks with precise start/stop/pause functionality:

```pascal
// Start a timer
TimerID := TM.StartTimer(TaskID, 'Starting work on feature');

// Pause timer (e.g., for interruption)
TM.PauseTimer(TimerID);

// Resume timer
TM.ResumeTimer(TimerID);

// Stop timer and create time entry
EntryID := TM.StopTimer(TimerID, 'Completed initial implementation');

// Cancel timer without logging
TM.CancelTimer(TimerID);

// Check if task has active timer
IsRunning := TM.IsTaskTimerRunning(TaskID);
```

### 2. Pomodoro Technique

Implement the Pomodoro Technique for focused work sessions:

```pascal
// Configure Pomodoro settings
TM.ConfigurePomodoro(
  25,  // Work duration (minutes)
  5,   // Short break duration
  15,  // Long break duration
  4    // Pomodoros before long break
);

// Start a Pomodoro work session
SessionID := TM.StartPomodoroSession(TaskID, ptWork);

// Complete the session
TM.CompletePomodoroSession(SessionID);

// Or mark as interrupted
TM.InterruptPomodoro(SessionID, 'Phone call');

// Get statistics
PomodoroCount := TM.GetTodaysPomodoroCount;
Streak := TM.GetPomodoroStreak;
```

### 3. Time Entry Logging

Automatic logging when timers stop, plus manual entry support:

```pascal
// Add manual time entry
EntryID := TM.AddManualTimeEntry(
  TaskID,
  StartDateTime,
  EndDateTime,
  'Notes about this work session'
);

// Get all time entries for a task
Entries := TM.GetTaskTimeEntries(TaskID);

// Get entries for a date range
Entries := TM.GetTimeEntriesForPeriod(StartDate, EndDate);

// Get total time spent on task
TotalHours := TM.GetTotalTimeForTask(TaskID);
```

### 4. Time Blocking

Schedule dedicated focus time for tasks:

```pascal
// Create a one-time time block
BlockID := TM.CreateTimeBlock(
  TaskID,
  'Deep Work Session',
  StartDateTime,
  120,  // Duration in minutes
  'No interruptions'
);

// Create recurring time block
BlockID := TM.CreateRecurringTimeBlock(
  TaskID,
  'Daily Review',
  StartDateTime,
  30,
  'Daily',
  'Review progress and plan'
);

// Move a time block
TM.MoveTimeBlock(BlockID, NewStartTime);

// Find available time slot
AvailableSlot := TM.FindAvailableTimeSlot(60, PreferredStart);
```

### 5. Productivity Analytics

Comprehensive metrics and insights:

```pascal
// Get overall productivity metrics
Metrics := TM.GetProductivityMetrics(StartDate, EndDate);
// Returns: TotalTimeTracked, FocusedTimePercent, 
//          AverageSessionLength, PomodorosCompleted,
//          PeakProductivityHour, etc.

// Time comparison (estimated vs actual)
Comparisons := TM.GetTimeComparisonReport;
for i := 0 to High(Comparisons) do
  WriteLn(Format('%s: Est=%.1fh, Act=%.1fh, Var=%.1f%%',
    [Comparisons[i].TaskTitle,
     Comparisons[i].EstimatedHours,
     Comparisons[i].ActualHours,
     Comparisons[i].Variance]));

// Various reports
TM.GetDailyTimeLog(Date);
TM.GetWeeklyTimeReport(StartOfWeek);
TM.GetTaskTimeReport(TaskID);
TM.GetPomodoroStats(7);  // Last 7 days
TM.GetProductivityByHour;
TM.GetProductivityByDayOfWeek;
TM.GetTopTimeConsumingTasks(10);
TM.GetEstimateAccuracyReport;
```

## Data Structures

### TTimeEntry
```pascal
TTimeEntry = record
  ID: Integer;
  TaskID: Integer;
  StartTime: TDateTime;
  EndTime: TDateTime;
  DurationMinutes: Double;
  Status: TTimerStatus;
  Notes: string;
  WasPomodoro: Boolean;
  PomodoroCount: Integer;
  InterruptionCount: Integer;
  Tags: array of string;
end;
```

### TPomodoroSession
```pascal
TPomodoroSession = record
  ID: Integer;
  TaskID: Integer;
  StartTime: TDateTime;
  EndTime: TDateTime;
  SessionType: TPomodoroType;  // ptWork, ptShortBreak, ptLongBreak
  PlannedDuration: Integer;
  ActualDuration: Integer;
  WasCompleted: Boolean;
  WasInterrupted: Boolean;
  InterruptionReason: string;
  PomodoroNumber: Integer;
end;
```

### TTimeBlock
```pascal
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
```

### TProductivityMetrics
```pascal
TProductivityMetrics = record
  TotalTimeTracked: Double;     // hours
  FocusedTimePercent: Double;   // percentage
  AverageSessionLength: Double; // minutes
  PomodorosCompleted: Integer;
  TasksCompleted: Integer;
  EstimateAccuracy: Double;     // percentage
  PeakProductivityHour: Integer; // 0-23
  InterruptionsPerDay: Double;
end;
```

## Usage Examples

### Example 1: Basic Time Tracking Workflow

```pascal
var
  TM: TTimeTrackingTaskManager;
  TaskID, TimerID, EntryID: Integer;
begin
  TM := TTimeTrackingTaskManager.Create;
  try
    // Create task with estimate
    TaskID := TM.AddTask('Write report', 'Q1 financial report',
                         'Finance', tpHigh, Tomorrow, 3.0);
    
    // Start working
    TimerID := TM.StartTimer(TaskID, 'Starting report');
    
    // ... do work ...
    
    // Take a break
    TM.PauseTimer(TimerID);
    // ... break time ...
    TM.ResumeTimer(TimerID);
    
    // Finish work
    EntryID := TM.StopTimer(TimerID, 'Completed draft');
    
    // Check actual vs estimate
    WriteLn(TM.GetTaskTimeReport(TaskID));
    
  finally
    TM.Free;
  end;
end;
```

### Example 2: Pomodoro Workflow

```pascal
var
  TM: TTimeTrackingTaskManager;
  TaskID, PomodoroID: Integer;
  PomodoroNum: Integer;
begin
  TM := TTimeTrackingTaskManager.Create;
  try
    TaskID := TM.AddTask('Deep work', 'Focused coding session',
                         'Development', tpHigh, Today, 4.0);
    
    // Do 4 pomodoros
    for PomodoroNum := 1 to 4 do
    begin
      // Work session (25 min)
      PomodoroID := TM.StartPomodoroSession(TaskID, ptWork);
      // ... work for 25 minutes ...
      TM.CompletePomodoroSession(PomodoroID);
      
      // Break (5 or 15 min)
      if PomodoroNum mod 4 = 0 then
        PomodoroID := TM.StartPomodoroSession(TaskID, ptLongBreak)
      else
        PomodoroID := TM.StartPomodoroSession(TaskID, ptShortBreak);
      // ... take break ...
      TM.CompletePomodoroSession(PomodoroID);
    end;
    
    WriteLn(TM.GetPomodoroStats(1));
    
  finally
    TM.Free;
  end;
end;
```

### Example 3: Weekly Planning with Time Blocks

```pascal
var
  TM: TTimeTrackingTaskManager;
  MondayMorning: TDateTime;
  TaskID, BlockID: Integer;
begin
  TM := TTimeTrackingTaskManager.Create;
  try
    MondayMorning := EncodeDateTime(2024, 3, 4, 9, 0, 0, 0);
    
    TaskID := TM.AddTask('Strategic planning', '', 'Management', tpHigh, 0, 2.0);
    
    // Block Monday 9-11am
    BlockID := TM.CreateTimeBlock(TaskID, 'Weekly Planning',
                                  MondayMorning, 120,
                                  'Deep focus - no meetings');
    
    // Check schedule
    Blocks := TM.GetTimeBlocksForDay(MondayMorning);
    for i := 0 to High(Blocks) do
      WriteLn(Format('%s: %s - %s',
        [Blocks[i].BlockName,
         FormatDateTime('hh:nn', Blocks[i].StartTime),
         FormatDateTime('hh:nn', Blocks[i].EndTime)]));
    
  finally
    TM.Free;
  end;
end;
```

## Benefits

1. **Accurate Time Tracking**: Know exactly where your time goes
2. **Improved Estimates**: Learn from past performance to estimate better
3. **Focus Enhancement**: Pomodoro technique reduces burnout and maintains focus
4. **Productivity Insights**: Discover your peak productivity hours
5. **Time Optimization**: Identify time-wasting activities
6. **Work-Life Balance**: Track overtime and maintain healthy boundaries
7. **Data-Driven Decisions**: Make informed decisions based on time data

## Integration with Other Features

The Time Tracking module integrates seamlessly with:

- **Wellbeing Module**: Suggests breaks based on work duration
- **Task Management**: Enhances tasks with actual time data
- **Project Management**: Provides accurate project time estimates
- **Team Collaboration**: Share time insights with team members

## Configuration

### Pomodoro Settings

Default configuration:
- Work duration: 25 minutes
- Short break: 5 minutes
- Long break: 15 minutes
- Pomodoros before long break: 4

Customize with `ConfigurePomodoro()` method.

## Data Persistence

Save and load time tracking data:

```pascal
// Save all time tracking data
TM.SaveTimeTrackingDataToFile('timetracking.dat');

// Load time tracking data
TM.LoadTimeTrackingDataFromFile('timetracking.dat');
```

## Best Practices

1. **Start Timer Immediately**: Begin timing when you start work
2. **Use Pause for Interruptions**: Don't stop timer for brief interruptions
3. **Complete Pomodoros**: Try to finish each 25-min session
4. **Review Weekly**: Check your time reports weekly
5. **Adjust Estimates**: Use actual time data to improve future estimates
6. **Block Focus Time**: Schedule uninterrupted work blocks
7. **Take Breaks**: Honor the Pomodoro break times

## Future Enhancements

Potential additions:
- Visual timeline view
- Integration with calendar apps
- Automated time entry suggestions based on activity
- Team time tracking and comparison
- Billing and invoicing based on tracked time
- Time prediction using machine learning

## Technical Notes

- All times stored in TDateTime format
- Duration calculations account for paused time
- Supports multiple concurrent timers
- Thread-safe timer operations
- Efficient indexing for fast lookups

---

**Module**: taskmanagertimetracking.pas  
**Inherits From**: TWellbeingTaskManager  
**Test Program**: solution21.pas  
**Version**: 1.0  
**Last Updated**: 2024-12-08
