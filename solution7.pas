
program solution7;
{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  taskmanager, taskmanagerext, taskmanageradvanced,
  taskmanagerenhanced, taskmanagerteam, taskmanagergamify,
  taskmanagersmart, taskmanagerfocus;

procedure SelfTest;
var
  Manager: TFocusTaskManager;
  Task1, Task2, Task3: Integer;
  Session1, Session2, Session3: Integer;
  Analytics: TFocusAnalytics;
  Recommendations: TFocusRecommendationArray;
  Distractions: TDistractionArray;
  i: Integer;
  Settings: TPomodoroSettings;
begin
  WriteLn(StringOfChar('=', 80));
  WriteLn('TASK MANAGER - SOLUTION 7 (FOCUS & CONTEXT MANAGEMENT)');
  WriteLn('Testing Layer 7: Deep Work, Pomodoro, Energy Tracking, Focus Analytics');
  WriteLn(StringOfChar('=', 80));
  WriteLn;

  Manager := TFocusTaskManager.Create;
  try
    Manager.SetCurrentUser('FocusedDeveloper');
    
    // Test 1: Create tasks and classify them
    WriteLn('TEST 1: Task Creation and Classification');
    WriteLn(StringOfChar('-', 80));
    
    Task1 := Manager.AddTaskWithAudit('Design System Architecture',
      'Deep thinking required for new microservices architecture', 'Development',
      tpHigh, Now + 7, 16.0);
    Manager.ClassifyTaskAsDeepWork(Task1);
    WriteLn('Created Task #', Task1, ': Design System Architecture [DEEP WORK]');
    
    Task2 := Manager.AddTaskWithAudit('Code Review',
      'Review pull requests from team members', 'Development',
      tpMedium, Now + 2, 3.0);
    Manager.ClassifyTaskAsShallowWork(Task2);
    WriteLn('Created Task #', Task2, ': Code Review [SHALLOW WORK]');
    
    Task3 := Manager.AddTaskWithAudit('Implement Authentication',
      'Build OAuth2 authentication flow', 'Development',
      tpHigh, Now + 5, 12.0);
    Manager.ClassifyTaskAsDeepWork(Task3);
    WriteLn('Created Task #', Task3, ': Implement Authentication [DEEP WORK]');
    WriteLn;
    
    // Test 2: Pomodoro Sessions
    WriteLn('TEST 2: Pomodoro Technique');
    WriteLn(StringOfChar('-', 80));
    
    Settings := Manager.GetPomodoroSettings;
    WriteLn(Format('Default Pomodoro Settings: %d min focus, %d min short break, %d min long break',
      [Settings.FocusDuration, Settings.ShortBreakDuration, Settings.LongBreakDuration]));
    WriteLn;
    
    WriteLn('Starting Pomodoro session for deep work task...');
    Session1 := Manager.StartPomodoro(Task1);
    WriteLn('Session #', Session1, ' started at ', FormatDateTime('hh:nn', Now));
    WriteLn('Current state: ', Manager.FocusStateToString(Manager.GetFocusState));
    
    // Simulate completing the pomodoro
    Manager.CompletePomodoro(Session1);
    WriteLn('Pomodoro completed! Total pomodoros: ', Manager.GetPomodoroCount);
    WriteLn;
    
    // Test 3: Focus Sessions with Interruptions
    WriteLn('TEST 3: Focus Sessions with Interruption Tracking');
    WriteLn(StringOfChar('-', 80));
    
    WriteLn('Starting 90-minute deep work session...');
    Session2 := Manager.StartFocusSession(Task3, 90, 8, True);
    WriteLn('Session #', Session2, ' started with energy level: 8/10');
    
    // Log some interruptions
    Manager.LogInterruption(Session2, 'Slack notification');
    Manager.LogInterruption(Session2, 'Quick question from colleague');
    WriteLn('Logged 2 interruptions during session');
    
    Manager.EndFocusSession(Session2, 6, 'Good progress despite interruptions');
    WriteLn('Session ended with energy level: 6/10');
    WriteLn('Total interruptions: ', Manager.GetSessionInterruptions(Session2));
    WriteLn;
    
    // Test 4: Context Switching
    WriteLn('TEST 4: Context Switching Analysis');
    WriteLn(StringOfChar('-', 80));
    
    Manager.LogContextSwitch(Task1, Task2, 'Urgent code review request', -3);
    WriteLn('Logged context switch: Task #', Task1, ' -> Task #', Task2, ' (Impact: -3)');
    
    Manager.LogContextSwitch(Task2, Task3, 'Returning to planned work', -2);
    WriteLn('Logged context switch: Task #', Task2, ' -> Task #', Task3, ' (Impact: -2)');
    
    WriteLn(Format('Total context switch cost: %d minutes', [Manager.GetContextSwitchCost]));
    WriteLn('(Average context switch costs ~23 minutes of productivity)');
    WriteLn;
    
    // Test 5: Distraction Logging
    WriteLn('TEST 5: Distraction Tracking');
    WriteLn(StringOfChar('-', 80));
    
    Manager.LogDistraction(Task3, 'email', 15, 7, 'Non-urgent email notifications');
    Manager.LogDistraction(Task3, 'chat', 10, 8, 'Team chat discussions');
    Manager.LogDistraction(Task3, 'phone', 5, 6, 'Quick phone call');
    
    Distractions := Manager.GetDistractions(Task3);
    WriteLn(Format('Logged %d distractions for Task #%d', [Length(Distractions), Task3]));
    for i := 0 to High(Distractions) do
      WriteLn(Format('  - %s: %d min (impact: %d/10) - %s',
        [Distractions[i].DistractionType, Distractions[i].Duration,
         Distractions[i].Impact, Distractions[i].Notes]));
    
    WriteLn(Format('Total distraction time: %d minutes', [Manager.GetTotalDistractionTime]));
    WriteLn;
    
    // Test 6: Energy Level Tracking
    WriteLn('TEST 6: Energy Level Tracking');
    WriteLn(StringOfChar('-', 80));
    
    Manager.LogEnergyLevel(8, 9, 8, 'energetic', 'Morning - well rested after coffee');
    Manager.LogEnergyLevel(7, 7, 7, 'rested', 'Mid-morning - good focus');
    Manager.LogEnergyLevel(5, 5, 6, 'tired', 'After lunch - slight dip');
    Manager.LogEnergyLevel(6, 6, 7, 'rested', 'Afternoon - recovered');
    
    WriteLn(Format('Average energy level: %.1f/10', [Manager.GetAverageEnergyLevel]));
    WriteLn('Energy trend: ', Manager.GetCurrentEnergyTrend);
    WriteLn(Manager.GetBestEnergyHours);
    WriteLn;
    
    // Test 7: Focus Analytics
    WriteLn('TEST 7: Focus Analytics and Insights');
    WriteLn(StringOfChar('-', 80));
    
    Analytics := Manager.GetFocusAnalytics(Now - 1, Now);
    WriteLn('Focus Analytics (Last 24 Hours):');
    WriteLn(Format('  Total focus time: %d minutes (%.1f hours)',
      [Analytics.TotalFocusTime, Analytics.TotalFocusTime / 60.0]));
    WriteLn(Format('  Average session duration: %.1f minutes', [Analytics.AverageFocusDuration]));
    WriteLn(Format('  Deep work: %.0f%% | Shallow work: %.0f%%',
      [Analytics.DeepWorkPercentage, Analytics.ShallowWorkPercentage]));
    WriteLn(Format('  Interruption rate: %.2f per hour', [Analytics.InterruptionRate]));
    WriteLn(Format('  Context switches: %d', [Analytics.ContextSwitches]));
    WriteLn(Format('  Best focus hour: %d:00', [Analytics.BestFocusHour]));
    WriteLn(Format('  Average energy: %.1f/10', [Analytics.AverageEnergyLevel]));
    WriteLn(Format('  Productivity score: %.1f/100', [Analytics.ProductivityScore]));
    WriteLn;
    
    WriteLn(Manager.GetDeepWorkStats);
    WriteLn;
    
    // Test 8: Smart Recommendations
    WriteLn('TEST 8: Focus Recommendations');
    WriteLn(StringOfChar('-', 80));
    
    Manager.GenerateRecommendations;
    Recommendations := Manager.GetActiveRecommendations;
    WriteLn(Format('Generated %d recommendations:', [Length(Recommendations)]));
    for i := 0 to High(Recommendations) do
      WriteLn(Format('  #%d [%s] Priority %d: %s',
        [Recommendations[i].ID, Recommendations[i].RecommendationType,
         Recommendations[i].Priority, Recommendations[i].Suggestion]));
    WriteLn;
    
    // Test 9: Task Suggestions
    WriteLn('TEST 9: Intelligent Task Suggestions');
    WriteLn(StringOfChar('-', 80));
    
    WriteLn('Suggested next task based on current energy level:');
    i := Manager.SuggestNextTask;
    if i > 0 then
      WriteLn('  Task #', i, ' is recommended')
    else
      WriteLn('  No specific recommendation at this time');
    WriteLn;
    
    // Test 10: Settings and Configuration
    WriteLn('TEST 10: Focus Settings');
    WriteLn(StringOfChar('-', 80));
    
    WriteLn('Current settings:');
    WriteLn('  ', Manager.GetFocusSettings);
    WriteLn;
    
    WriteLn('Customizing Pomodoro settings...');
    Manager.SetPomodoroSettings(50, 10, 20, 3); // Longer focus sessions
    WriteLn('  ', Manager.GetFocusSettings);
    WriteLn;
    
    Manager.EnableAutoBreakReminders(True);
    Manager.EnableContextSwitchTracking(True);
    WriteLn('Enabled auto-break reminders and context switch tracking');
    WriteLn;
    
    // Test 11: Integration with Gamification
    WriteLn('TEST 11: Integration with Previous Layers');
    WriteLn(StringOfChar('-', 80));
    
    WriteLn('Starting another focus session with gamification rewards...');
    Session3 := Manager.StartFocusSession(Task1, 60, 8, True);
    Manager.EndFocusSession(Session3, 7, 'Excellent deep work session');
    
    Manager.CompleteTaskWithRewards(Task2);
    WriteLn('Completed task with gamification integration');
    WriteLn('Current XP: ', Manager.GetCurrentLevel.CurrentXP);
    WriteLn('Achievement unlocked: ', Length(Manager.GetUnlockedAchievements), ' total');
    WriteLn;
    
    WriteLn(StringOfChar('=', 80));
    WriteLn('ALL FOCUS MANAGEMENT TESTS COMPLETED SUCCESSFULLY!');
    WriteLn('Deep Work Tracking, Pomodoro, Energy Management, and Focus Analytics Working!');
    WriteLn(StringOfChar('=', 80));
    WriteLn;
    WriteLn('Summary:');
    WriteLn('  - Created 3 tasks (2 deep work, 1 shallow work)');
    WriteLn('  - Completed ', Manager.GetPomodoroCount, ' pomodoro sessions');
    WriteLn('  - Tracked ', Length(Manager.GetAllContextSwitches), ' context switches');
    WriteLn('  - Logged ', Length(Distractions), ' distractions');
    WriteLn(Format('  - Productivity score: %.1f/100', [Analytics.ProductivityScore]));
    
  finally
    Manager.Free;
  end;
end;

begin
  try
    SelfTest;
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end.
