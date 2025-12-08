
program solution14;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, Math,
  taskmanager, taskmanagerfocus;

procedure SelfTest;
var
  tm: TFocusTaskManager;
  taskID, pomodoroID, sessionID, distractionID, switchID, blockID: Integer;
  stats: TFocusStats;
  report: string;
  i: Integer;
begin
  WriteLn('=== Focus & Deep Work Manager Self Test ===');
  WriteLn;
  
  tm := TFocusTaskManager.Create;
  try
    // Test 1: Configure Pomodoro settings
    WriteLn('Test 1: Configuring Pomodoro and Focus settings...');
    tm.SetPomodoroSettings(25, 5, 15, 4);
    tm.SetFlowThreshold(20);
    WriteLn('  ✓ Pomodoro: 25 min work, 5 min break, 15 min long break');
    WriteLn('  ✓ Long break after 4 pomodoros');
    WriteLn('  ✓ Flow threshold: 20 minutes');
    WriteLn;
    
    // Test 2: Create some tasks for focus work
    WriteLn('Test 2: Creating tasks...');
    taskID := tm.AddTask('Write Research Paper', 'Deep work on quantum computing paper',
                         tpHigh, IncDay(Now, 7));
    WriteLn('  Created task ID ', taskID, ': Write Research Paper');
    
    tm.AddTask('Code Review', 'Review pull requests', tpMedium, IncDay(Now, 2));
    tm.AddTask('Answer Emails', 'Process inbox', tpLow, Now);
    WriteLn('  ✓ Created 3 tasks');
    WriteLn;
    
    // Test 3: Start and complete Pomodoro sessions
    WriteLn('Test 3: Pomodoro technique...');
    pomodoroID := tm.StartPomodoro(taskID, 25);
    WriteLn('  Started Pomodoro session (ID: ', pomodoroID, ') for 25 minutes');
    
    Sleep(100); // Simulate work time
    
    tm.CompletePomodoro(pomodoroID, fqExcellent, 'Great focus, made good progress');
    WriteLn('  ✓ Completed Pomodoro with Excellent quality');
    
    // Start and abandon one
    pomodoroID := tm.StartPomodoro(taskID, 25);
    tm.AbandonPomodoro(pomodoroID, 'Urgent meeting came up');
    WriteLn('  ✗ Abandoned one Pomodoro (urgent meeting)');
    
    // Complete a few more
    for i := 1 to 3 do
    begin
      pomodoroID := tm.StartPomodoro(taskID, 25);
      Sleep(50);
      tm.CompletePomodoro(pomodoroID, fqGood, 'Session ' + IntToStr(i));
    end;
    WriteLn('  ✓ Completed 3 more Pomodoros');
    WriteLn;
    
    // Test 4: Pomodoro statistics
    WriteLn('Test 4: Pomodoro statistics...');
    WriteLn(tm.GetPomodoroStats(7));
    WriteLn;
    
    // Test 5: Focus sessions with flow state tracking
    WriteLn('Test 5: Deep work focus sessions...');
    sessionID := tm.StartFocusSession(taskID, ftDeepWork, 8);
    WriteLn('  Started deep work session (ID: ', sessionID, ') with energy level 8/10');
    
    Sleep(100);
    
    tm.EndFocusSession(sessionID, 9, 7, 'Excellent progress on paper introduction');
    WriteLn('  ✓ Ended session: productivity 9/10, energy dropped to 7/10');
    
    // Creative session with lower focus
    sessionID := tm.StartFocusSession(taskID, ftCreative, 6);
    distractionID := tm.LogDistraction(sessionID, taskID, dtNotification, 
                                       'Slack message', 3, True);
    WriteLn('  Logged distraction: Slack notification (avoidable)');
    
    distractionID := tm.LogDistraction(sessionID, taskID, dtInterruption,
                                       'Colleague question', 7, False);
    WriteLn('  Logged distraction: Colleague interruption (unavoidable)');
    
    tm.EndFocusSession(sessionID, 6, 5, 'Distractions hurt productivity');
    WriteLn('  ✓ Completed session with distractions: productivity 6/10');
    WriteLn;
    
    // Test 6: Context switching analysis
    WriteLn('Test 6: Context switching tracking...');
    switchID := tm.LogContextSwitch(taskID, 2, 'Email needed immediate response', False);
    WriteLn('  Logged unplanned switch from task ', taskID, ' to task 2');
    tm.UpdateSwitchRecovery(switchID, 15, 8);
    WriteLn('  Updated: 15 minutes recovery time, cost score 8/10');
    
    switchID := tm.LogContextSwitch(2, taskID, 'Back to research', True);
    tm.UpdateSwitchRecovery(switchID, 10, 5);
    WriteLn('  Logged planned switch back: 10 min recovery, cost 5/10');
    WriteLn;
    WriteLn(tm.GetSwitchingCost(7));
    WriteLn;
    
    // Test 7: Deep work block scheduling
    WriteLn('Test 7: Scheduling deep work blocks...');
    blockID := tm.ScheduleDeepWorkBlock('Morning Deep Work',
                                        IncDay(Now, 1) + EncodeTime(9, 0, 0, 0),
                                        IncDay(Now, 1) + EncodeTime(11, 0, 0, 0),
                                        9);
    WriteLn('  Scheduled deep work block (ID: ', blockID, ')');
    WriteLn('  Tomorrow 9:00-11:00, protection level 9/10');
    
    tm.AddTaskToBlock(blockID, taskID);
    WriteLn('  ✓ Added research paper task to block');
    WriteLn;
    
    // Test 8: Distraction analysis
    WriteLn('Test 8: Analyzing distractions...');
    WriteLn(tm.GetDistractionStats(7));
    WriteLn(tm.GetMostCommonDistractions);
    WriteLn('  Avoidable distraction rate: ', 
            Format('%.1f%%', [tm.GetAvoidableDistractionRate(7)]));
    WriteLn;
    
    // Test 9: Focus statistics
    WriteLn('Test 9: Overall focus statistics...');
    stats := tm.GetFocusStats(7);
    WriteLn('  Total focus time: ', stats.TotalFocusMinutes, ' minutes');
    WriteLn('  Deep work: ', stats.DeepWorkMinutes, ' minutes');
    WriteLn('  Average flow score: ', Format('%.1f/100', [stats.AverageFlowScore]));
    WriteLn('  Average productivity: ', Format('%.1f/10', [stats.AverageProductivity]));
    WriteLn('  Focus efficiency: ', Format('%.1f%%', [stats.FocusEfficiency]));
    WriteLn;
    
    // Test 10: Flow state recommendations
    WriteLn('Test 10: Flow state optimization...');
    WriteLn(tm.GetFlowStateRecommendations);
    WriteLn;
    WriteLn('  Best time for deep work: ', tm.GetBestTimeForDeepWork, ':00');
    WriteLn('  Deep work ratio: ', Format('%.1f%%', [tm.GetDeepWorkRatio(7)]));
    WriteLn;
    
    // Test 11: Interruption impact
    WriteLn('Test 11: Understanding interruption costs...');
    WriteLn(tm.GetInterruptionImpact(7));
    WriteLn;
    
    // Test 12: Focus improvement suggestions
    WriteLn('Test 12: Getting personalized improvement suggestions...');
    WriteLn(tm.SuggestFocusImprovements);
    WriteLn;
    
    // Test 13: Energy correlation analysis
    WriteLn('Test 13: Energy and productivity correlation...');
    WriteLn(tm.GetEnergyCorrelation);
    WriteLn;
    
    // Test 14: Comprehensive focus report
    WriteLn('Test 14: Generating comprehensive focus report...');
    WriteLn;
    WriteLn(tm.GenerateFocusReport(7));
    WriteLn;
    
    WriteLn('=== All Focus & Deep Work Tests Completed Successfully! ===');
    WriteLn;
    WriteLn('New focus management features demonstrated:');
    WriteLn('  1. Pomodoro timer with quality tracking');
    WriteLn('  2. Focus sessions with flow state detection');
    WriteLn('  3. Distraction logging and analysis');
    WriteLn('  4. Context switching cost measurement');
    WriteLn('  5. Deep work block scheduling and protection');
    WriteLn('  6. Flow pattern identification');
    WriteLn('  7. Energy-productivity correlation');
    WriteLn('  8. Interruption impact quantification');
    WriteLn('  9. Personalized focus improvement suggestions');
    WriteLn('  10. Time-of-day productivity optimization');
    WriteLn;
    WriteLn('This module helps maximize deep work capacity and');
    WriteLn('minimize the hidden costs of distractions! 🎯');
    
  finally
    tm.Free;
  end;
end;

begin
  SelfTest;
end.
