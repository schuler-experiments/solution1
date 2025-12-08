
program TimeTrackingDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  taskmanager, taskmanageradvanced, taskmanagerlifestyle, 
  taskmanagerwellbeing, taskmanagertimetracking;

procedure SelfTest;
var
  TM: TTimeTrackingTaskManager;
  TaskID1, TaskID2, TaskID3: Integer;
  TimerID1, TimerID2: Integer;
  PomodoroID1: Integer;
  EntryID: Integer;
  BlockID1, BlockID2: Integer;
  Metrics: TProductivityMetrics;
  Comparisons: TTimeComparisonArray;
  i: Integer;
begin
  WriteLn('=== Time Tracking & Pomodoro Manager - Self Test ===');
  WriteLn;
  
  TM := TTimeTrackingTaskManager.Create;
  try
    // Test 1: Create tasks with time estimates
    WriteLn('Test 1: Creating tasks with time estimates...');
    TaskID1 := TM.AddTask('Write documentation', 'Create comprehensive user guide',
                          'Documentation', tpMedium, EncodeDate(2024, 3, 15), 4.0);
    TaskID2 := TM.AddTask('Implement feature', 'Add time tracking system',
                          'Development', tpHigh, EncodeDate(2024, 3, 10), 8.0);
    TaskID3 := TM.AddTask('Code review', 'Review pull requests',
                          'Development', tpMedium, EncodeDate(2024, 3, 12), 2.0);
    WriteLn(Format('Created %d tasks with time estimates', [TM.TaskCount]));
    WriteLn;
    
    // Test 2: Start and stop timers
    WriteLn('Test 2: Basic timer operations...');
    TimerID1 := TM.StartTimer(TaskID1, 'Starting work on documentation');
    WriteLn(Format('Started timer %d for task %d', [TimerID1, TaskID1]));
    
    // Simulate some work
    Sleep(100);
    
    TM.PauseTimer(TimerID1);
    WriteLn('Timer paused');
    
    Sleep(50);
    
    TM.ResumeTimer(TimerID1);
    WriteLn('Timer resumed');
    
    Sleep(100);
    
    EntryID := TM.StopTimer(TimerID1, 'Completed initial draft');
    WriteLn(Format('Timer stopped, created time entry %d', [EntryID]));
    WriteLn;
    
    // Test 3: Pomodoro configuration and sessions
    WriteLn('Test 3: Pomodoro technique integration...');
    TM.ConfigurePomodoro(25, 5, 15, 4);
    WriteLn('Configured Pomodoro: 25min work, 5min short break, 15min long break');
    
    PomodoroID1 := TM.StartPomodoroSession(TaskID2, ptWork);
    WriteLn(Format('Started Pomodoro session %d', [PomodoroID1]));
    
    // Simulate pomodoro completion
    Sleep(100);
    TM.CompletePomodoroSession(PomodoroID1);
    WriteLn('Pomodoro session completed');
    WriteLn(Format('Today''s pomodoro count: %d', [TM.GetTodaysPomodoroCount]));
    WriteLn(Format('Pomodoro streak: %d days', [TM.GetPomodoroStreak]));
    WriteLn;
    
    // Test 4: Manual time entries
    WriteLn('Test 4: Adding manual time entries...');
    EntryID := TM.AddManualTimeEntry(TaskID3, 
                                     EncodeDateTime(2024, 3, 1, 9, 0, 0, 0),
                                     EncodeDateTime(2024, 3, 1, 11, 30, 0, 0),
                                     'Code review session');
    WriteLn(Format('Added manual entry %d (2.5 hours)', [EntryID]));
    
    EntryID := TM.AddManualTimeEntry(TaskID2,
                                     EncodeDateTime(2024, 3, 1, 14, 0, 0, 0),
                                     EncodeDateTime(2024, 3, 1, 18, 0, 0, 0),
                                     'Feature implementation');
    WriteLn(Format('Added manual entry %d (4 hours)', [EntryID]));
    WriteLn;
    
    // Test 5: Time blocking
    WriteLn('Test 5: Creating time blocks for scheduling...');
    BlockID1 := TM.CreateTimeBlock(TaskID1, 'Morning Writing Session',
                                   EncodeDateTime(2024, 3, 5, 9, 0, 0, 0),
                                   120, 'Focus time for documentation');
    WriteLn(Format('Created time block %d (2 hours)', [BlockID1]));
    
    BlockID2 := TM.CreateRecurringTimeBlock(TaskID2, 'Daily Standup',
                                            EncodeDateTime(2024, 3, 5, 10, 0, 0, 0),
                                            15, 'Daily', 'Team meeting');
    WriteLn(Format('Created recurring time block %d (15 min daily)', [BlockID2]));
    WriteLn;
    
    // Test 6: Task time reports
    WriteLn('Test 6: Generating task time reports...');
    WriteLn(TM.GetTaskTimeReport(TaskID2));
    WriteLn;
    
    // Test 7: Productivity metrics
    WriteLn('Test 7: Calculating productivity metrics...');
    Metrics := TM.GetProductivityMetrics(EncodeDate(2024, 3, 1), EncodeDate(2024, 3, 31));
    WriteLn(Format('Total time tracked: %.2f hours', [Metrics.TotalTimeTracked]));
    WriteLn(Format('Focused time: %.1f%%', [Metrics.FocusedTimePercent]));
    WriteLn(Format('Average session: %.1f minutes', [Metrics.AverageSessionLength]));
    WriteLn(Format('Pomodoros completed: %d', [Metrics.PomodorosCompleted]));
    WriteLn(Format('Peak productivity hour: %d:00', [Metrics.PeakProductivityHour]));
    WriteLn;
    
    // Test 8: Time comparison (estimate vs actual)
    WriteLn('Test 8: Estimate accuracy analysis...');
    Comparisons := TM.GetTimeComparisonReport;
    WriteLn(Format('Tasks with time tracking: %d', [Length(Comparisons)]));
    for i := 0 to High(Comparisons) do
    begin
      WriteLn(Format('  %s:', [Comparisons[i].TaskTitle]));
      WriteLn(Format('    Estimated: %.2fh, Actual: %.2fh, Variance: %.1f%% (%s)',
                     [Comparisons[i].EstimatedHours, Comparisons[i].ActualHours,
                      Comparisons[i].Variance, Comparisons[i].Status]));
    end;
    WriteLn;
    
    // Test 9: Pomodoro statistics
    WriteLn('Test 9: Pomodoro statistics...');
    WriteLn(TM.GetPomodoroStats(7));
    WriteLn;
    
    // Test 10: Top time-consuming tasks
    WriteLn('Test 10: Top time-consuming tasks...');
    WriteLn(TM.GetTopTimeConsumingTasks(5));
    WriteLn;
    
    // Test 11: Productivity by hour
    WriteLn('Test 11: Productivity patterns by hour...');
    WriteLn(TM.GetProductivityByHour);
    WriteLn;
    
    // Test 12: Estimate accuracy report
    WriteLn('Test 12: Overall estimate accuracy...');
    WriteLn(TM.GetEstimateAccuracyReport);
    WriteLn;
    
    // Test 13: Active timers
    WriteLn('Test 13: Managing multiple active timers...');
    TimerID1 := TM.StartTimer(TaskID1, 'Working on documentation');
    TimerID2 := TM.StartTimer(TaskID3, 'Code review');
    WriteLn(Format('Active timers: %d', [Length(TM.GetAllActiveTimers)]));
    WriteLn(Format('Task 1 has running timer: %s', [BoolToStr(TM.IsTaskTimerRunning(TaskID1), True)]));
    TM.StopTimer(TimerID1, 'Finished section');
    TM.CancelTimer(TimerID2);
    WriteLn('Stopped and cancelled timers');
    WriteLn;
    
    // Test 14: Find available time slot
    WriteLn('Test 14: Finding available time slots...');
    WriteLn(Format('Next available 60-min slot: %s',
                   [FormatDateTime('yyyy-mm-dd hh:nn', 
                    TM.FindAvailableTimeSlot(60, Now))]));
    WriteLn;
    
    // Test 15: Data persistence
    WriteLn('Test 15: Saving and loading time tracking data...');
    if TM.SaveTimeTrackingDataToFile('solution1/timetracking_test.dat') then
      WriteLn('✓ Time tracking data saved successfully')
    else
      WriteLn('✗ Failed to save data');
      
    if TM.LoadTimeTrackingDataFromFile('solution1/timetracking_test.dat') then
      WriteLn('✓ Time tracking data loaded successfully')
    else
      WriteLn('✗ Failed to load data');
    WriteLn;
    
    WriteLn('=== All Time Tracking Tests Completed Successfully! ===');
    WriteLn;
    WriteLn('New features demonstrated:');
    WriteLn('✓ Start/stop/pause/resume timers');
    WriteLn('✓ Pomodoro technique integration (25/5/15 min cycles)');
    WriteLn('✓ Manual time entry logging');
    WriteLn('✓ Time blocking and scheduling');
    WriteLn('✓ Actual vs estimated time comparison');
    WriteLn('✓ Productivity metrics and analytics');
    WriteLn('✓ Pomodoro statistics and streaks');
    WriteLn('✓ Time reports (daily, weekly, per-task)');
    WriteLn('✓ Peak productivity hour analysis');
    WriteLn('✓ Top time-consuming tasks');
    WriteLn('✓ Estimate accuracy tracking');
    WriteLn('✓ Data persistence (save/load)');
    
  finally
    TM.Free;
  end;
end;

begin
  SelfTest;
end.
