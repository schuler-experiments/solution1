
# Mental Health & Wellbeing Task Manager Module

## Overview

The `taskmanagerwellbeing.pas` unit extends the task manager with comprehensive mental health and wellbeing features. This module takes a human-centered approach to productivity, helping prevent burnout and promoting sustainable work practices.

## Features

### 1. Wellbeing Check-Ins
Track your mental and physical state throughout the day:
- **Stress Level Monitoring** (Very Low to Very High)
- **Energy Level Tracking** (Very Low to Very High)
- **Mood Assessment** (Very Negative to Very Positive)
- **Sleep Quality Rating** (1-10 scale)
- **Work Satisfaction Scoring** (1-10 scale)
- **Physical Symptom Logging** (headaches, fatigue, eye strain, etc.)

```pascal
// Record a wellbeing check-in
checkInID := tm.RecordCheckIn(slModerate, elHigh, mlPositive, 8, 8, 'Feeling great!');
tm.AddPhysicalSymptom(checkInID, 'Headache');

// Get averages
stressAvg := tm.GetAverageStressLevel(7);  // Last 7 days
moodAvg := tm.GetAverageMoodLevel(7);
energyAvg := tm.GetAverageEnergyLevel(7);
```

### 2. Break Management
Smart break tracking and reminders:
- **Break Types**: Micro breaks, short breaks, long breaks, meals, exercise, meditation
- **Effectiveness Ratings**: Rate how refreshing each break was
- **Break Compliance Tracking**: Monitor if you're taking enough breaks
- **Smart Break Suggestions**: Get recommendations based on time since last break

```pascal
// Start and end a break
breakID := tm.StartBreak(btMicroBreak);
tm.EndBreak(breakID, 8, 'Quick stretch, felt refreshing');

// Check compliance
complianceRate := tm.GetBreakComplianceRate(7);  // Percentage for last 7 days
timeSince := tm.GetTimeSinceLastBreak;  // Minutes since last break
suggestedType := tm.SuggestBreakType;  // What kind of break to take
```

### 3. Burnout Detection & Prevention
Early warning system for burnout risk:
- **Risk Levels**: None, Low, Moderate, High, Critical
- **Burnout Score**: 0-100 scale based on multiple factors
- **Burnout Indicators**: Specific warnings with recommended actions
- **Recovery Recommendations**: Personalized suggestions to prevent burnout

```pascal
// Assess burnout risk
risk := tm.AssessBurnoutRisk;
score := tm.GetBurnoutScore;  // 0-100
indicators := tm.GetBurnoutIndicators;

// Get recovery suggestions
recommendations := tm.GetRecoveryRecommendations;
```

### 4. Energy Pattern Learning
Optimize task scheduling based on your energy levels:
- **24-Hour Energy Patterns**: Learn when you have peak energy
- **Task Type Recommendations**: Match tasks to energy levels
  - High energy → Creative work
  - Medium energy → Meetings
  - Low energy → Administrative tasks
- **Optimal Time Suggestions**: Find best times for different activities

```pascal
// Record energy levels
tm.RecordEnergyLevel(elHigh, 'Morning energy peak');

// Get optimal scheduling
currentEnergy := tm.GetCurrentEnergyLevel;
taskType := tm.SuggestTaskBasedOnEnergy;
optimalHour := tm.GetOptimalTimeForTask('creative');  // Returns hour (0-23)
```

### 5. Cognitive Load Management
Prevent mental overload:
- **Load Tracking**: Monitor active tasks, context switches, mental demand
- **Overload Detection**: Automatic alerts when load is too high
- **Load Reduction Suggestions**: Smart recommendations to reduce cognitive burden

```pascal
// Track cognitive load
tm.TrackCognitiveLoad(activeTasks := 5, contextSwitches := 3, mentalDemand := 8);
currentLoad := tm.GetCurrentCognitiveLoad;  // 0-100 scale

if tm.IsCognitiveOverload then
  suggestion := tm.SuggestLoadReduction;
```

### 6. Work-Life Balance Tracking
Monitor and maintain healthy boundaries:
- **Work Hours Tracking**: Daily, weekly, weekend work monitoring
- **Outside Hours Detection**: Alerts when working beyond set hours
- **Balance Score**: 0-100 metric of work-life balance
- **Overtime Tracking**: Monitor excessive work hours

```pascal
// Configure working hours
tm.SetWorkingHours(9, 17);  // 9 AM to 5 PM
tm.ConfigureSettings(maxDailyHours := 8.0, maxWeeklyHours := 40.0, 
                     minBreaks := 3, microBreakInterval := 25, 
                     longBreakInterval := 120);

// Check balance
if tm.IsWorkingOutsideHours then
  WriteLn('Working outside regular hours!');
  
balance := tm.GetWorkLifeBalance(weekNumber);
trend := tm.GetBalanceTrend;  // 'Improving', 'Stable', 'Declining'
```

### 7. Wellness Recommendations
Intelligent suggestions for wellbeing:
- **Categorized Recommendations**: Break, workload, sleep, stress categories
- **Priority Levels**: 1-5 priority ranking
- **Action Steps**: Specific actions to take
- **Expected Benefits**: What improvements to expect

```pascal
// Get recommendations
recommendations := tm.GetWellnessRecommendations;
priorityRecs := tm.GetPriorityRecommendations;  // High priority only

// Apply or dismiss
tm.ApplyRecommendation(recommendationID);
tm.DismissRecommendation(recommendationID);
```

### 8. Mental Health Scoring
Overall wellbeing assessment:
- **Composite Score**: 0-100 based on stress, mood, energy, burnout risk
- **Interpretation**:
  - 80-100: Excellent wellbeing
  - 60-79: Good wellbeing
  - 40-59: Fair, consider self-care
  - 0-39: Needs attention, prioritize wellness

```pascal
score := tm.GetMentalHealthScore;  // 0-100

if score >= 80 then
  WriteLn('Excellent wellbeing! 😊')
else if score >= 60 then
  WriteLn('Good wellbeing, keep it up! 🙂')
else if score >= 40 then
  WriteLn('Fair, consider self-care 😐')
else
  WriteLn('Needs attention, please prioritize wellness ⚠️');
```

### 9. Comprehensive Reports
Detailed wellbeing analytics:
- **Wellbeing Report**: 7-day summary of all metrics
- **Stress Trigger Analysis**: Identify what causes stress
- **Energy Insights**: Peak and low energy times with recommendations
- **Wellness Summary**: Quick overview of current state

```pascal
report := tm.GenerateWellbeingReport;
triggers := tm.GetStressTriggers;
insights := tm.GetEnergyInsights;
summary := tm.GetWellnessSummary;
```

## Configuration

### Default Settings
- **Max Daily Work Hours**: 8.0
- **Max Weekly Work Hours**: 40.0
- **Minimum Daily Breaks**: 3
- **Micro Break Interval**: 25 minutes (Pomodoro-style)
- **Long Break Interval**: 120 minutes
- **Working Hours**: 9:00 AM - 5:00 PM
- **Burnout Alerts**: Enabled
- **Break Reminders**: Enabled

### Customization
```pascal
tm.ConfigureSettings(
  maxDailyHours := 6.0,      // Shorter workday
  maxWeeklyHours := 30.0,    // Part-time work
  minBreaks := 5,            // More frequent breaks
  microBreakInterval := 20,  // Every 20 minutes
  longBreakInterval := 90    // Every 90 minutes
);

tm.SetWorkingHours(10, 18);  // 10 AM to 6 PM
tm.EnableFeature('burnout_alerts', True);
tm.EnableFeature('break_reminders', True);
```

## Usage Example

```pascal
program WellbeingDemo;

uses
  SysUtils, DateUtils,
  taskmanager, taskmanagerlifestyle, taskmanagerwellbeing;

var
  tm: TWellbeingTaskManager;
  checkInID: Integer;
  score: Double;

begin
  tm := TWellbeingTaskManager.Create;
  try
    // Configure for your needs
    tm.ConfigureSettings(8.0, 40.0, 3, 25, 120);
    tm.SetWorkingHours(9, 17);
    
    // Morning check-in
    checkInID := tm.RecordCheckIn(slLow, elHigh, mlPositive, 8, 8, 
                                  'Feeling energized and ready!');
    
    // Track work session
    tm.TrackCognitiveLoad(2, 0, 4);  // 2 tasks, no switches, medium demand
    
    // Take a break
    var breakID := tm.StartBreak(btMicroBreak);
    Sleep(300000);  // 5 minutes
    tm.EndBreak(breakID, 9, 'Walked outside, very refreshing');
    
    // Assess wellbeing
    score := tm.GetMentalHealthScore;
    WriteLn('Mental Health Score: ', score:0:1, '/100');
    
    // Get recommendations
    var recs := tm.GetPriorityRecommendations;
    for var i := 0 to High(recs) do
      WriteLn('Suggestion: ', recs[i].Title);
    
  finally
    tm.Free;
  end;
end.
```

## Integration with Other Modules

The wellbeing module extends `TLifestyleTaskManager`, which means it includes all features from:
- **Base Task Manager**: Core task management
- **Advanced Features**: Work sessions, notes, dependencies
- **Lifestyle Features**: Habits, time boxing, Eisenhower matrix, focus sessions

This creates a complete system that balances productivity with wellbeing.

## Benefits

1. **Prevent Burnout**: Early detection and intervention
2. **Optimize Energy**: Work with your natural rhythms, not against them
3. **Improve Work-Life Balance**: Clear boundaries and monitoring
4. **Reduce Stress**: Track triggers and get actionable recommendations
5. **Sustainable Productivity**: Long-term effectiveness over short-term gains
6. **Compassionate Management**: Human-centered approach to task management

## Philosophy

This module is built on the principle that **sustainable productivity requires wellbeing**. By monitoring mental health metrics alongside task completion, it helps you maintain a healthy relationship with work and achieve your goals without sacrificing your health.

The wellbeing module treats you as a whole person, not just a productivity machine. It recognizes that rest, recovery, and mental health are essential components of long-term success.

---

**Remember**: Your wellbeing is not a nice-to-have, it's a must-have for sustainable success! 💚
