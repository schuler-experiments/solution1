
program solution12;

{$mode objfpc}

uses
  SysUtils, DateUtils,
  taskmanager, taskmanagerlifestyle;

procedure SelfTest;
var
  tm: TLifestyleTaskManager;
  templateID, taskID, habitID, timeBoxID, bundleID: Integer;
  focusSessionID, entryID: Integer;
  templates: TTaskTemplateArray;
  habits: THabitArray;
  i: Integer;
begin
  WriteLn('=== Lifestyle Task Manager Self Test ===');
  WriteLn;
  
  tm := TLifestyleTaskManager.Create;
  try
    // Test 1: Task Templates
    WriteLn('Test 1: Creating task templates...');
    templateID := tm.CreateTemplate('Daily Review', 'Review and plan daily tasks',
      'Planning', tpHigh, 0.5, elMedium);
    tm.AddChecklistItemToTemplate(templateID, 'Review yesterday''s progress');
    tm.AddChecklistItemToTemplate(templateID, 'Plan top 3 priorities');
    tm.AddChecklistItemToTemplate(templateID, 'Schedule deep work blocks');
    tm.AddDefaultTagToTemplate(templateID, 'daily');
    tm.AddDefaultTagToTemplate(templateID, 'planning');
    WriteLn('  Created template "Daily Review" (ID: ', templateID, ')');
    
    templateID := tm.CreateTemplate('Code Review', 'Review pull requests',
      'Development', tpMedium, 1.0, elHigh);
    tm.AddChecklistItemToTemplate(templateID, 'Check code style');
    tm.AddChecklistItemToTemplate(templateID, 'Test functionality');
    tm.AddChecklistItemToTemplate(templateID, 'Review documentation');
    WriteLn('  Created template "Code Review" (ID: ', templateID, ')');
    
    templateID := tm.CreateTemplate('Team Meeting', 'Weekly team sync',
      'Meetings', tpLow, 1.0, elMedium);
    WriteLn('  Created template "Team Meeting" (ID: ', templateID, ')');
    WriteLn;
    
    // Test 2: Create tasks from templates
    WriteLn('Test 2: Creating tasks from templates...');
    taskID := tm.CreateTaskFromTemplate(1, Now + 1);
    WriteLn('  Created task from template 1 (Task ID: ', taskID, ')');
    taskID := tm.CreateTaskFromTemplate(2, Now + 2);
    WriteLn('  Created task from template 2 (Task ID: ', taskID, ')');
    WriteLn;
    
    // Test 3: Eisenhower Matrix
    WriteLn('Test 3: Adding tasks to Eisenhower Matrix...');
    taskID := tm.AddTask('Fix critical bug', 'Production issue', tpCritical, Now + 0.5);
    entryID := tm.AddToEisenhowerMatrix(taskID, 10, 10, 'Crisis - do immediately');
    WriteLn('  Added to Q1 (Urgent & Important): Entry ID ', entryID);
    
    taskID := tm.AddTask('Learn new framework', 'Professional development', tpMedium, Now + 30);
    entryID := tm.AddToEisenhowerMatrix(taskID, 3, 9, 'Important but not urgent');
    WriteLn('  Added to Q2 (Not Urgent & Important): Entry ID ', entryID);
    
    taskID := tm.AddTask('Answer emails', 'Daily communications', tpLow, Now + 1);
    entryID := tm.AddToEisenhowerMatrix(taskID, 8, 4, 'Urgent but less important');
    WriteLn('  Added to Q3 (Urgent & Not Important): Entry ID ', entryID);
    
    WriteLn;
    WriteLn(tm.GetEisenhowerSummary);
    WriteLn;
    
    // Test 4: Habit Tracking
    WriteLn('Test 4: Creating and tracking habits...');
    habitID := tm.CreateHabit('Morning Exercise', '30 minutes of exercise',
      hfDaily, 30);
    WriteLn('  Created habit "Morning Exercise" (ID: ', habitID, ')');
    
    habitID := tm.CreateHabit('Read Technical Book', 'Read 20 pages',
      hfDaily, 21);
    WriteLn('  Created habit "Read Technical Book" (ID: ', habitID, ')');
    
    habitID := tm.CreateHabit('Weekly Planning', 'Plan upcoming week',
      hfWeekly, 12);
    WriteLn('  Created habit "Weekly Planning" (ID: ', habitID, ')');
    WriteLn;
    
    // Test 5: Log habit completions
    WriteLn('Test 5: Logging habit completions...');
    tm.LogHabitCompletion(1, 'Great workout!', 'Energized');
    tm.LogHabitCompletion(1, 'Tough but done', 'Tired but satisfied');
    tm.LogHabitCompletion(2, 'Finished chapter 3', 'Focused');
    WriteLn('  Logged 3 habit completions');
    WriteLn('  Morning Exercise streak: ', tm.GetHabitStreak(1), ' days');
    WriteLn;
    WriteLn(tm.GetHabitStatistics(1));
    WriteLn;
    
    // Test 6: Time Boxing
    WriteLn('Test 6: Time boxing tasks...');
    taskID := tm.AddTask('Write report', 'Monthly report', tpHigh, Now + 2);
    timeBoxID := tm.CreateTimeBox(taskID, Now, 60);
    WriteLn('  Created time box (ID: ', timeBoxID, ') - 60 minutes allocated');
    tm.CompleteTimeBox(timeBoxID, 55, True);
    WriteLn('  Completed in 55 minutes - Success!');
    
    taskID := tm.AddTask('Research competitor', 'Market analysis', tpMedium, Now + 5);
    timeBoxID := tm.CreateTimeBox(taskID, Now, 90);
    tm.RecordInterruption(timeBoxID);
    tm.RecordInterruption(timeBoxID);
    tm.CompleteTimeBox(timeBoxID, 105, False);
    WriteLn('  Time box with 2 interruptions, took 105 min instead of 90');
    WriteLn('  Time box efficiency: ', tm.GetTimeBoxEfficiency:0:1, '%');
    WriteLn;
    
    // Test 7: Task Bundling
    WriteLn('Test 7: Creating task bundles...');
    bundleID := tm.CreateTaskBundle('Email Batch', 'Process all emails together',
      'Communication');
    WriteLn('  Created bundle "Email Batch" (ID: ', bundleID, ')');
    
    taskID := tm.AddTask('Reply to client A', '', tpMedium, Now + 1);
    tm.AddTaskToBundle(bundleID, taskID);
    taskID := tm.AddTask('Reply to client B', '', tpMedium, Now + 1);
    tm.AddTaskToBundle(bundleID, taskID);
    taskID := tm.AddTask('Send weekly update', '', tpLow, Now + 1);
    tm.AddTaskToBundle(bundleID, taskID);
    WriteLn('  Added 3 tasks to bundle');
    WriteLn;
    
    // Test 8: Context Switching
    WriteLn('Test 8: Recording context switches...');
    tm.RecordContextSwitch(1, 2, 'Urgent request');
    tm.RecordContextSwitch(2, 3, 'Meeting started');
    tm.RecordContextSwitch(3, 1, 'Back to original task');
    WriteLn('  Recorded 3 context switches');
    WriteLn(tm.GetContextSwitchReport);
    WriteLn;
    
    // Test 9: Focus Sessions
    WriteLn('Test 9: Focus sessions for deep work...');
    taskID := tm.AddTask('Deep work: Architecture design', 'System design',
      tpHigh, Now + 3);
    focusSessionID := tm.StartFocusSession(taskID, 90);
    WriteLn('  Started focus session (ID: ', focusSessionID, ') - 90 minutes planned');
    tm.EndFocusSession(focusSessionID, 8, 1, 'Very productive session');
    WriteLn('  Ended session with productivity rating 8/10, 1 distraction');
    WriteLn('  Average focus quality: ', tm.GetAverageFocusQuality:0:1, '/10');
    WriteLn('  Best focus time: ', tm.GetBestFocusTime);
    WriteLn;
    
    // Test 10: Task Mood Tracking
    WriteLn('Test 10: Tracking mood during tasks...');
    taskID := tm.AddTask('Creative brainstorming', 'New product ideas', tpMedium, Now + 7);
    tm.RecordTaskMood(taskID, mtVeryGood, elPeak, 'Very creative today!');
    taskID := tm.AddTask('Bug fixing', 'Fix reported bugs', tpHigh, Now + 1);
    tm.RecordTaskMood(taskID, mtNeutral, elMedium, 'Methodical work');
    WriteLn('  Recorded mood for 2 tasks');
    WriteLn(tm.GetMoodInsights);
    WriteLn;
    
    // Test 11: Productivity Rhythm
    WriteLn('Test 11: Learning productivity rhythm...');
    tm.RecordProductivitySample(1, 9, 85.0);  // Monday 9 AM - high
    tm.RecordProductivitySample(1, 10, 90.0); // Monday 10 AM - peak
    tm.RecordProductivitySample(1, 14, 60.0); // Monday 2 PM - post-lunch dip
    tm.RecordProductivitySample(1, 15, 75.0); // Monday 3 PM - recovery
    WriteLn('  Recorded productivity samples');
    WriteLn(tm.GetOptimalWorkingHours);
    WriteLn;
    
    // Test 12: Display templates
    WriteLn('Test 12: Listing all templates...');
    templates := tm.GetAllTemplates;
    WriteLn('  Total templates: ', Length(templates));
    for i := 0 to High(templates) do
      WriteLn('    - ', templates[i].Name, ' (Used ', templates[i].UsageCount, ' times)');
    WriteLn;
    
    // Test 13: Display habits
    WriteLn('Test 13: Listing active habits...');
    habits := tm.GetActiveHabits;
    WriteLn('  Active habits: ', Length(habits));
    for i := 0 to High(habits) do
      WriteLn('    - ', habits[i].Name, ' (Streak: ', habits[i].CurrentStreak, 
              ', Longest: ', habits[i].LongestStreak, ')');
    WriteLn;
    
    // Test 14: Habits needing attention
    WriteLn('Test 14: Checking habits needing attention...');
    habits := tm.GetHabitsNeedingAttention;
    if Length(habits) > 0 then
    begin
      WriteLn('  Habits needing attention: ', Length(habits));
      for i := 0 to High(habits) do
        WriteLn('    - ', habits[i].Name);
    end
    else
      WriteLn('  All habits are up to date!');
    WriteLn;
    
    // Test 15: Productivity heatmap
    WriteLn('Test 15: Productivity heatmap visualization...');
    WriteLn(tm.GetProductivityHeatmap);
    WriteLn;
    
    WriteLn('=== All Lifestyle Features Tests Completed Successfully! ===');
    WriteLn;
    WriteLn('New lifestyle features demonstrated:');
    WriteLn('  1. Task templates with checklists');
    WriteLn('  2. Eisenhower Matrix (Urgent/Important quadrants)');
    WriteLn('  3. Habit tracking with streaks');
    WriteLn('  4. Time boxing with interruption tracking');
    WriteLn('  5. Task bundling for efficiency');
    WriteLn('  6. Context switching cost analysis');
    WriteLn('  7. Focus sessions for deep work');
    WriteLn('  8. Task mood tracking');
    WriteLn('  9. Productivity rhythm learning');
    WriteLn('  10. Energy level optimization');
    WriteLn('  11. Template usage statistics');
    WriteLn('  12. Habit attention alerts');
    WriteLn('  13. Visual productivity heatmap');
    
  finally
    tm.Free;
  end;
end;

begin
  SelfTest;
end.
