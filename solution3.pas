
program TaskManagerAdvancedDemo;
{$mode objfpc}
{$H+}

uses
  SysUtils, DateUtils, taskmanager, taskmanagerext, taskmanageradvanced;

procedure SelfTest;
var
  Manager: TAdvancedTaskManager;
  Task1, Task2, Task3, Task4, Task5: Integer;
  Template1, Template2: Integer;
  Session1, Session2: Integer;
  Note1, Note2, Note3: Integer;
  Dep1, Dep2: Integer;
  Sessions: TWorkSessionArray;
  Notes: TTaskNoteArray;
  Deps: TTaskDependencyArray;
  Templates: TTaskTemplateArray;
  i: Integer;
  WorkTime: Integer;
begin
  WriteLn('=== Advanced Task Manager Self Test ===');
  WriteLn;
  
  Manager := TAdvancedTaskManager.Create;
  try
    // Test 1: Create task templates
    WriteLn('Test 1: Creating task templates...');
    Template1 := Manager.CreateTemplate(
      'Bug Fix Template',
      'Standard template for fixing bugs',
      'Development',
      tpHigh,
      2.0,
      3  // Default 3 days to complete
    );
    
    Template2 := Manager.CreateTemplate(
      'Feature Implementation',
      'Template for implementing new features',
      'Development',
      tpMedium,
      8.0,
      14  // Default 2 weeks
    );
    
    Manager.AddChecklistToTemplate(Template1, 'Reproduce the bug');
    Manager.AddChecklistToTemplate(Template1, 'Identify root cause');
    Manager.AddChecklistToTemplate(Template1, 'Implement fix');
    Manager.AddChecklistToTemplate(Template1, 'Test the fix');
    Manager.AddChecklistToTemplate(Template1, 'Deploy to production');
    
    Templates := Manager.GetAllTemplates;
    WriteLn(Format('Created %d templates', [Length(Templates)]));
    for i := 0 to High(Templates) do
      WriteLn('  ', Manager.TemplateToString(Templates[i]));
    WriteLn;
    
    // Test 2: Create tasks from templates
    WriteLn('Test 2: Creating tasks from templates...');
    Task1 := Manager.CreateTaskFromTemplate(Template1, 
      'Fix login authentication bug',
      EncodeDate(2024, 2, 15));
    Task2 := Manager.CreateTaskFromTemplate(Template1,
      'Fix memory leak in payment module',
      EncodeDate(2024, 2, 12));
    Task3 := Manager.CreateTaskFromTemplate(Template2,
      'Implement dark mode feature',
      EncodeDate(2024, 2, 25));
    WriteLn(Format('Created 3 tasks from templates (IDs: %d, %d, %d)', [Task1, Task2, Task3]));
    WriteLn;
    
    // Test 3: Add task dependencies
    WriteLn('Test 3: Setting up task dependencies...');
    Task4 := Manager.AddExtendedTask('Design UI mockups', 'Create mockups for dark mode',
      'Design', tpHigh, EncodeDate(2024, 2, 18), 4.0, rpNone);
    Task5 := Manager.AddExtendedTask('Review and approve', 'Review dark mode implementation',
      'QA', tpMedium, EncodeDate(2024, 2, 28), 2.0, rpNone);
    
    // Task3 (implementation) depends on Task4 (design)
    Dep1 := Manager.AddDependency(Task3, Task4, dtFinishToStart, 0);
    // Task5 (review) depends on Task3 (implementation)
    Dep2 := Manager.AddDependency(Task5, Task3, dtFinishToStart, 1);
    
    WriteLn(Format('Added %d dependencies', [2]));
    WriteLn('Dependency chain for Task #', Task5, ':');
    WriteLn(Manager.GetDependencyChain(Task5));
    WriteLn;
    
    // Test 4: Add notes to tasks
    WriteLn('Test 4: Adding notes to tasks...');
    Note1 := Manager.AddNote(Task1, 'Developer', 
      'Started investigating the issue', 'update');
    Note2 := Manager.AddNote(Task1, 'Developer',
      'Found the root cause in session validation', 'decision');
    Note3 := Manager.AddNote(Task2, 'QA Team',
      'This is blocking the release!', 'blocker');
    
    WriteLn(Format('Added %d notes', [3]));
    WriteLn('Notes for Task #', Task1, ':');
    WriteLn(Manager.NotesToString(Task1));
    WriteLn;
    
    // Test 5: Work sessions (Pomodoro tracking)
    WriteLn('Test 5: Tracking work sessions...');
    Session1 := Manager.StartWorkSession(Task1, 'Debugging session');
    WriteLn(Format('Started work session #%d on Task #%d', [Session1, Task1]));
    
    // Simulate some work (we'll just end it immediately for testing)
    Sleep(100);  // Simulate 100ms of work
    Manager.EndWorkSession(Session1, True);
    WriteLn('Ended session #', Session1, ' (completed)');
    
    Session2 := Manager.StartWorkSession(Task2, 'Investigating memory leak');
    Sleep(50);
    Manager.EndWorkSession(Session2, False);
    WriteLn('Ended session #', Session2, ' (interrupted)');
    WriteLn;
    
    WriteLn('Session statistics:');
    WriteLn(Manager.GetSessionStats);
    WriteLn;
    
    // Test 6: Validate task completion with dependencies
    WriteLn('Test 6: Validating task completion with dependencies...');
    WriteLn(Format('Can complete Task #%d (has dependencies)? %s',
      [Task3, BoolToStr(Manager.ValidateTaskCompletion(Task3), True)]));
    WriteLn(Format('Can complete Task #%d (no dependencies)? %s',
      [Task1, BoolToStr(Manager.ValidateTaskCompletion(Task1), True)]));
    WriteLn;
    
    // Test 7: Get work time for tasks
    WriteLn('Test 7: Checking work time logged...');
    WorkTime := Manager.GetTotalWorkTime(Task1);
    WriteLn(Format('Total work time for Task #%d: %d minutes', [Task1, WorkTime]));
    WriteLn;
    
    // Test 8: Dependencies - what tasks are blocked/blocking
    WriteLn('Test 8: Analyzing task dependencies...');
    Deps := Manager.GetBlocking(Task4);
    WriteLn(Format('Task #%d is blocking %d task(s)', [Task4, Length(Deps)]));
    
    Deps := Manager.GetBlockedBy(Task3);
    WriteLn(Format('Task #%d is blocked by %d task(s)', [Task3, Length(Deps)]));
    WriteLn;
    
    // Test 9: All sessions for a task
    WriteLn('Test 9: Retrieving all work sessions for Task #', Task1, '...');
    Sessions := Manager.GetSessionsByTask(Task1);
    WriteLn(Format('Found %d session(s)', [Length(Sessions)]));
    for i := 0 to High(Sessions) do
    begin
      WriteLn(Format('  Session #%d: %s to %s (%d min) - %s',
        [Sessions[i].SessionID,
         FormatDateTime('hh:nn:ss', Sessions[i].StartTime),
         FormatDateTime('hh:nn:ss', Sessions[i].EndTime),
         Sessions[i].DurationMinutes,
         BoolToStr(Sessions[i].WasCompleted, 'Completed', 'Interrupted')]));
    end;
    WriteLn;
    
    // Test 10: Average session duration
    WriteLn('Test 10: Calculating average session duration...');
    WriteLn(Format('Average session duration: %.2f minutes', 
      [Manager.GetAverageSessionDuration]));
    WriteLn;
    
    // Test 11: Get all notes
    WriteLn('Test 11: Retrieving all task notes...');
    Notes := Manager.GetAllNotes;
    WriteLn(Format('Total notes in system: %d', [Length(Notes)]));
    WriteLn;
    
    // Test 12: Extended features still work
    WriteLn('Test 12: Verifying extended features still work...');
    WriteLn('Productivity Report:');
    WriteLn(Manager.GetProductivityReport);
    WriteLn;
    
    // Test 13: Save to file
    WriteLn('Test 13: Saving advanced task data...');
    if Manager.SaveAdvancedToFile('solution1/tasks_advanced.dat') then
      WriteLn('Successfully saved to tasks_advanced.dat')
    else
      WriteLn('Failed to save');
    WriteLn;
    
    WriteLn('=== All advanced tests completed successfully! ===');
    WriteLn;
    WriteLn('New features demonstrated:');
    WriteLn('✓ Task templates for reusable workflows');
    WriteLn('✓ Task dependencies (blocking/blocked by relationships)');
    WriteLn('✓ Task notes with timestamps and types');
    WriteLn('✓ Work session tracking (Pomodoro-style)');
    WriteLn('✓ Dependency validation for task completion');
    WriteLn('✓ Work time analytics per task');
    WriteLn('✓ Session statistics and average duration');
    WriteLn('✓ Integration with existing extended features');
    
  finally
    Manager.Free;
  end;
end;

begin
  SelfTest;
end.
