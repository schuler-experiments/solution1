
# Task Manager v5 Quick Start Guide

## New Features Overview

Version 5 adds two powerful systems to your task manager:

1. **Task Scheduling Engine** - Intelligent task scheduling with capacity planning
2. **Risk Analysis System** - Risk assessment and completion prediction

## Quick Examples

### 1. Schedule All Tasks and Generate Timeline

```pascal
var
  scheduler: TTaskScheduler;
  schedule: TScheduleEntryArray;
  ganttChart: TGanttEntryArray;

// Create scheduler
scheduler := TTaskScheduler.Create(manager);

// Schedule all tasks
if scheduler.scheduleAll(schedule) then
begin
  WriteLn('Scheduled ' + IntToStr(length(schedule)) + ' tasks');
  // Use schedule data for planning
end;

// Generate Gantt chart
if scheduler.generateGanttChart(ganttChart) then
begin
  // Each ganttChart[i] has: taskName, startDate, endDate, progress, etc.
  WriteLn('Generated Gantt chart with ' + IntToStr(length(ganttChart)) + ' entries');
end;

// Export as CSV
WriteLn(scheduler.exportGanttChartAsText());
```

### 2. Check Team Capacity

```pascal
var
  capacities: TCapacityEntryArray;
  i: integer;

// Calculate team capacity
if scheduler.calculateTeamCapacity(capacities) then
begin
  for i := 0 to length(capacities) - 1 do
  begin
    WriteLn(capacities[i].assignee + ': ' + 
            FloatToStr(capacities[i].utilizationPercent) + '% utilized');
  end;
  
  // Find overloaded team members
  var overloaded := scheduler.findOverallocatedAssignees();
  for i := 0 to length(overloaded) - 1 do
    WriteLn('OVERLOADED: ' + overloaded[i]);
end;
```

### 3. Assess Risk for All Tasks

```pascal
var
  riskAnalysis: TTaskRiskAnalysis;
  risks: TRiskEntryArray;
  i: integer;

// Create risk analyzer
riskAnalysis := TTaskRiskAnalysis.Create(manager, analytics);

// Get risk scores
if riskAnalysis.assessAllRisks(risks) then
begin
  WriteLn('Overall risk: ' + IntToStr(riskAnalysis.getOverallRiskScore));
  WriteLn('Health score: ' + IntToStr(riskAnalysis.getProjectHealthScore));
  
  // Show high-risk tasks
  for i := 0 to length(risks) - 1 do
  begin
    if risks[i].riskLevel = rlCritical then
      WriteLn('⚠ CRITICAL: ' + risks[i].taskName + 
              ' - ' + risks[i].suggestedAction);
  end;
end;
```

### 4. Get Task Completion Predictions

```pascal
var
  predictions: TPredictionArray;
  i: integer;

if riskAnalysis.predictAllCompletions(predictions) then
begin
  for i := 0 to length(predictions) - 1 do
  begin
    WriteLn(predictions[i].taskName + 
            ' - Delay risk: ' + 
            FloatToStr(predictions[i].delayRisk) + '%');
  end;
end;
```

### 5. Identify Blocking Issues

```pascal
var
  blockers: TBlockerAnalysisArray;
  i: integer;

blockers := riskAnalysis.findCriticalBlockers();
for i := 0 to length(blockers) - 1 do
begin
  if blockers[i].blockedByCount > 0 then
    WriteLn('Task ' + blockers[i].taskName + 
            ' is blocked by: ' + blockers[i].blockerNames);
end;
```

## Common Workflows

### Workflow 1: Daily Stand-up Report
```pascal
procedure GenerateDailyReport;
begin
  // Get health metrics
  WriteLn('Project Health: ' + IntToStr(riskAnalysis.getProjectHealthScore));
  WriteLn('Risk Status: ' + riskAnalysis.getRiskTrend(0));
  
  // Show critical tasks
  var critical := riskAnalysis.getCriticalRiskTasks();
  WriteLn('Critical tasks to address: ' + IntToStr(length(critical)));
  
  // Show team workload
  var capacities: TCapacityEntryArray;
  if scheduler.calculateTeamCapacity(capacities) then
  begin
    for var cap in capacities do
      WriteLn(cap.assignee + ': ' + FloatToStr(cap.utilizationPercent) + '%');
  end;
end;
```

### Workflow 2: Project Planning
```pascal
procedure PlanNewProject;
var
  i: integer;
begin
  // Create all tasks first
  // ... add tasks to manager ...
  
  // Generate schedule
  var schedule: TScheduleEntryArray;
  scheduler.scheduleAll(schedule);
  
  // Check capacity
  var capacities: TCapacityEntryArray;
  scheduler.calculateTeamCapacity(capacities);
  
  // Show Gantt chart
  WriteLn(scheduler.exportGanttChartAsText());
  
  // Assess risks
  var risks: TRiskEntryArray;
  riskAnalysis.assessAllRisks(risks);
  
  // Report
  WriteLn('Timeline: ' + IntToStr(length(schedule)) + ' tasks scheduled');
  WriteLn('Capacity: All team members OK');
  WriteLn('Risk: ' + IntToStr(riskAnalysis.getOverallRiskScore) + '/100');
end;
```

### Workflow 3: Risk Management
```pascal
procedure ManageRisks;
begin
  // Find all high-risk tasks
  var highRisk := riskAnalysis.getHighRiskTasks();
  
  for var task in highRisk do
  begin
    // Get detailed risk info
    var risk: TRiskEntry;
    riskAnalysis.assessTaskRisk(task.id, risk);
    
    WriteLn('Task: ' + risk.taskName);
    WriteLn('Risk Score: ' + IntToStr(risk.riskScore));
    WriteLn('Reason: ' + risk.riskFactors);
    WriteLn('Action: ' + risk.suggestedAction);
  end;
  
  // Find blocking issues
  var blockers := riskAnalysis.findCriticalBlockers();
  WriteLn('Tasks to unblock: ' + IntToStr(length(blockers)));
end;
```

## Configuration

```pascal
// Set work hours per day (default 8)
scheduler.setWorkHoursPerDay(8.0);

// Set team velocity for better predictions
riskAnalysis.setAverageVelocity(5.0);  // 5 tasks per week
```

## Error Handling

```pascal
if not scheduler.scheduleAll(schedule) then
  WriteLn('Error: ' + scheduler.getLastError());

if not riskAnalysis.assessAllRisks(risks) then
  WriteLn('Error: ' + riskAnalysis.getLastError());
```

## Key Metrics to Track

| Metric | Function | What It Means |
|--------|----------|---------------|
| Project Health Score | `getProjectHealthScore()` | 0-100: Higher is better |
| Overall Risk Score | `getOverallRiskScore()` | 0-100: Lower is better |
| Team Utilization | `calculateTeamCapacity()` | >100% = overloaded |
| Critical Task Count | `getPredictedCriticalCount()` | How many critical tasks exist |
| Completion Probability | Risk entry field | % chance task completes on time |

## Troubleshooting

**Q: Team capacity shows 0% utilization**
A: Tasks don't have estimated hours set. Use `manager.setTaskEstimatedHours()`.

**Q: Risk scores seem too high/low**
A: Risk weighting is: Due Date (25%) > Dependency (20%) > Priority (15%) > others.
Adjust approaching deadlines to reduce risk.

**Q: Predictions seem inaccurate**
A: Set more accurate `averageVelocity` with `riskAnalysis.setAverageVelocity()`.

**Q: Gantt chart shows no dependencies**
A: Add dependencies with `manager.addTaskDependency()`.

---

For detailed documentation, see:
- `VERSION_5_NEW_FEATURES.md` - Complete feature documentation
- `FEATURES.md` - All task manager features
- Main README - General information

