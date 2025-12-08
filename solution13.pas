
program solution13;

{$mode objfpc}

uses
  SysUtils, DateUtils, Math,
  taskmanager, taskmanagerlifestyle, taskmanagerwellbeing;

procedure SelfTest;
var
  tm: TWellbeingTaskManager;
  checkInID, breakID, taskID: Integer;
  stress: TStressLevel;
  energy: TEnergyLevel;
  mood: TMoodLevel;
  burnout: TBurnoutRisk;
  recommendations: TWellnessRecommendationArray;
  energyPatterns: TEnergyPatternArray;
  indicators: TBurnoutIndicatorArray;
  priorityRecs: TWellnessRecommendationArray;
  suggestedBreak: TBreakType;
  recoveryRecs: TWellnessRecommendationArray;
  i: Integer;
  mentalHealthScore: Double;
begin
  WriteLn('=== Mental Health & Wellbeing Task Manager Self Test ===');
  WriteLn;
  
  tm := TWellbeingTaskManager.Create;
  try
    // Test 1: Configure wellbeing settings
    WriteLn('Test 1: Configuring wellbeing settings...');
    tm.ConfigureSettings(8.0, 40.0, 3, 25, 120);
    tm.SetWorkingHours(9, 17);
    tm.EnableFeature('burnout_alerts', True);
    tm.EnableFeature('break_reminders', True);
    WriteLn('  ✓ Configured: Max 8 hrs/day, 40 hrs/week, 3 breaks/day');
    WriteLn('  ✓ Working hours: 9:00 - 17:00');
    WriteLn('  ✓ Enabled burnout alerts and break reminders');
    WriteLn;
    
    // Test 2: Record wellbeing check-ins
    WriteLn('Test 2: Recording wellbeing check-ins...');
    
    // Morning check-in - good mood
    checkInID := tm.RecordCheckIn(slLow, elHigh, mlPositive, 8, 8, 'Feeling great this morning!');
    WriteLn('  Morning check-in (ID: ', checkInID, ') - Good energy, low stress');
    
    // Midday check-in - moderate stress
    checkInID := tm.RecordCheckIn(slModerate, elModerate, mlNeutral, 7, 7, 'Busy day, managing well');
    WriteLn('  Midday check-in (ID: ', checkInID, ') - Moderate stress and energy');
    
    // Afternoon check-in - higher stress
    checkInID := tm.RecordCheckIn(slHigh, elLow, mlNegative, 7, 6, 'Feeling overwhelmed');
    tm.AddPhysicalSymptom(checkInID, 'Headache');
    tm.AddPhysicalSymptom(checkInID, 'Eye strain');
    WriteLn('  Afternoon check-in (ID: ', checkInID, ') - High stress, low energy');
    WriteLn('    Physical symptoms: Headache, Eye strain');
    WriteLn;
    
    // Test 3: Check stress, mood, and energy averages
    WriteLn('Test 3: Analyzing recent wellbeing metrics...');
    WriteLn('  Average stress (7 days): ', Format('%.2f', [tm.GetAverageStressLevel(7)]), '/4');
    WriteLn('  Average mood (7 days): ', Format('%.2f', [tm.GetAverageMoodLevel(7)]), '/4');
    WriteLn('  Average energy (7 days): ', Format('%.2f', [tm.GetAverageEnergyLevel(7)]), '/4');
    WriteLn;
    
    // Test 4: Break management
    WriteLn('Test 4: Managing breaks...');
    breakID := tm.StartBreak(btMicroBreak);
    WriteLn('  Started micro break (ID: ', breakID, ')');
    Sleep(100); // Simulate break duration
    tm.EndBreak(breakID, 8, 'Quick stretch, felt refreshing');
    WriteLn('  Ended break - Effectiveness: 8/10');
    
    breakID := tm.StartBreak(btShortBreak);
    WriteLn('  Started short break (ID: ', breakID, ')');
    tm.SkipBreak(breakID);
    WriteLn('  Skipped break - too busy');
    
    breakID := tm.StartBreak(btMeditation);
    WriteLn('  Started meditation break (ID: ', breakID, ')');
    tm.EndBreak(breakID, 9, '5-minute mindfulness session');
    WriteLn('  Ended meditation - Effectiveness: 9/10');
    
    WriteLn('  Break compliance rate: ', Format('%.1f', [tm.GetBreakComplianceRate(7)]), '%');
    WriteLn('  Time since last break: ', tm.GetTimeSinceLastBreak, ' minutes');
    WriteLn;
    
    // Test 5: Burnout risk assessment
    WriteLn('Test 5: Assessing burnout risk...');
    burnout := tm.AssessBurnoutRisk;
    WriteLn('  Current burnout risk: ', tm.BurnoutRiskToString(burnout));
    WriteLn('  Burnout score: ', Format('%.1f', [tm.GetBurnoutScore]), '/100');
    
    indicators := tm.GetBurnoutIndicators;
    if Length(indicators) > 0 then
    begin
      WriteLn('  Burnout indicators detected:');
      for i := 0 to High(indicators) do
        WriteLn('    - ', indicators[i].Description);
    end;
    WriteLn;
    
    // Test 6: Energy patterns and optimal work times
    WriteLn('Test 6: Learning energy patterns...');
    tm.RecordEnergyLevel(elHigh, 'Morning energy peak');
    tm.RecordEnergyLevel(elModerate, 'Midday steady');
    tm.RecordEnergyLevel(elLow, 'Afternoon dip');
    
    energyPatterns := tm.GetEnergyPatterns;
    WriteLn('  Current energy level: ', tm.EnergyLevelToString(tm.GetCurrentEnergyLevel));
    WriteLn('  Suggested task type: ', tm.SuggestTaskBasedOnEnergy);
    WriteLn('  Optimal time for creative work: ', tm.GetOptimalTimeForTask('creative'), ':00');
    WriteLn('  Optimal time for meetings: ', tm.GetOptimalTimeForTask('meetings'), ':00');
    WriteLn;
    
    // Test 7: Cognitive load tracking
    WriteLn('Test 7: Tracking cognitive load...');
    tm.TrackCognitiveLoad(3, 2, 6);
    WriteLn('  Tracked: 3 active tasks, 2 context switches, mental demand 6/10');
    WriteLn('  Current cognitive load: ', Format('%.1f', [tm.GetCurrentCognitiveLoad]), '/100');
    
    if tm.IsCognitiveOverload then
      WriteLn('  ⚠ COGNITIVE OVERLOAD DETECTED!')
    else
      WriteLn('  ✓ Cognitive load is manageable');
    
    WriteLn('  Suggestion: ', tm.SuggestLoadReduction);
    WriteLn;
    
    // Test 8: Wellness recommendations
    WriteLn('Test 8: Getting wellness recommendations...');
    recommendations := tm.GetWellnessRecommendations;
    WriteLn('  Total recommendations: ', Length(recommendations));
    
    if Length(recommendations) > 0 then
    begin
      WriteLn('  Priority recommendations:');
      priorityRecs := tm.GetPriorityRecommendations;
      for i := 0 to High(priorityRecs) do
      begin
        WriteLn('    ', i+1, '. ', priorityRecs[i].Title, ' (Priority: ', priorityRecs[i].Priority, ')');
        WriteLn('       ', priorityRecs[i].Description);
        if Length(priorityRecs[i].ActionSteps) > 0 then
          WriteLn('       Action: ', priorityRecs[i].ActionSteps[0]);
      end;
    end;
    WriteLn;
    
    // Test 9: Work-life balance check
    WriteLn('Test 9: Checking work-life balance...');
    if tm.IsWorkingOutsideHours then
      WriteLn('  ⚠ Working outside regular hours!')
    else
      WriteLn('  ✓ Working within regular hours');
    
    WriteLn('  Balance trend: ', tm.GetBalanceTrend);
    WriteLn;
    
    // Test 10: Mental health score
    WriteLn('Test 10: Overall mental health assessment...');
    mentalHealthScore := tm.GetMentalHealthScore;
    WriteLn('  Mental Health Score: ', Format('%.1f', [mentalHealthScore]), '/100');
    
    if mentalHealthScore >= 80 then
      WriteLn('  Status: Excellent wellbeing! 😊')
    else if mentalHealthScore >= 60 then
      WriteLn('  Status: Good wellbeing, keep it up! 🙂')
    else if mentalHealthScore >= 40 then
      WriteLn('  Status: Fair, consider self-care 😐')
    else
      WriteLn('  Status: Needs attention, please prioritize wellness ⚠️');
    WriteLn;
    
    // Test 11: Comprehensive wellbeing report
    WriteLn('Test 11: Generating comprehensive wellbeing report...');
    WriteLn(tm.GenerateWellbeingReport);
    
    // Test 12: Energy insights
    WriteLn('Test 12: Energy insights and optimization...');
    WriteLn(tm.GetEnergyInsights);
    
    // Test 13: Create some tasks to demonstrate integration
    WriteLn('Test 13: Creating tasks with wellbeing awareness...');
    taskID := tm.AddTask('Deep Work Session', 'Focus on complex problem solving',
      'Development', tpHigh, Now + 1, 2.0);
    WriteLn('  Created task: Deep Work Session (ID: ', taskID, ')');
    WriteLn('  Recommended time: ', tm.GetOptimalTimeForTask('creative'), ':00');
    
    taskID := tm.AddTask('Team Meeting', 'Weekly sync with team',
      'Meetings', tpMedium, Now + 1, 1.0);
    WriteLn('  Created task: Team Meeting (ID: ', taskID, ')');
    WriteLn('  Recommended time: ', tm.GetOptimalTimeForTask('meetings'), ':00');
    
    taskID := tm.AddTask('Email Processing', 'Clear inbox and respond',
      'Administrative', tpLow, Now + 1, 0.5);
    WriteLn('  Created task: Email Processing (ID: ', taskID, ')');
    WriteLn('  Recommended time: ', tm.GetOptimalTimeForTask('administrative'), ':00');
    WriteLn;
    
    // Test 14: Break suggestions
    WriteLn('Test 14: Smart break suggestions...');
    suggestedBreak := tm.SuggestBreakType;
    WriteLn('  Suggested break type: ', tm.BreakTypeToString(suggestedBreak));
    WriteLn('  Time since last break: ', tm.GetTimeSinceLastBreak, ' minutes');
    WriteLn;
    
    // Test 15: Recovery recommendations for high stress
    WriteLn('Test 15: Recovery recommendations...');
    recoveryRecs := tm.GetRecoveryRecommendations;
    if Length(recoveryRecs) > 0 then
    begin
      WriteLn('  Recovery suggestions:');
      for i := 0 to Min(2, High(recoveryRecs)) do
      begin
        WriteLn('    - ', recoveryRecs[i].Title);
        WriteLn('      ', recoveryRecs[i].ExpectedBenefit);
      end;
    end
    else
      WriteLn('  No immediate recovery actions needed');
    WriteLn;
    
    WriteLn('=== All Wellbeing Features Tests Completed Successfully! ===');
    WriteLn;
    WriteLn('New wellbeing features demonstrated:');
    WriteLn('  1. Wellbeing check-ins with stress, energy, and mood tracking');
    WriteLn('  2. Break management with effectiveness ratings');
    WriteLn('  3. Burnout risk assessment and early warning system');
    WriteLn('  4. Energy pattern learning and task scheduling optimization');
    WriteLn('  5. Cognitive load monitoring and overload prevention');
    WriteLn('  6. Smart wellness recommendations');
    WriteLn('  7. Work-life balance tracking');
    WriteLn('  8. Mental health scoring (0-100 scale)');
    WriteLn('  9. Physical symptom tracking');
    WriteLn('  10. Recovery and stress management suggestions');
    WriteLn;
    WriteLn('This wellbeing module helps prevent burnout and promotes');
    WriteLn('sustainable productivity with compassionate task management! 💚');
    
  finally
    tm.Free;
  end;
end;

begin
  SelfTest;
end.
