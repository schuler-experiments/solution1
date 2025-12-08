
# Recurring Tasks and Portfolio Management Features

## Overview
This document describes the **Recurring Tasks** and **Multi-Project Portfolio Management** features added to the task manager system. These features are implemented in the `taskmanagerrecurring.pas` unit and extend the `TSmartTaskManager` class.

## New Class: TRecurringTaskManager

### Recurring Task Features

#### Recurrence Patterns
The system supports multiple recurrence patterns:

- **Daily**: Tasks that repeat every N days
- **Weekly**: Tasks that repeat on specific days of the week (e.g., every Monday, Wednesday, Friday)
- **Monthly**: Tasks that repeat on a specific day of each month (e.g., 1st of every month)
- **Yearly**: Tasks that repeat on a specific date each year (e.g., January 1st annually)

#### Pattern Configuration
Each recurrence pattern can be configured with:

- **Interval**: How often the task repeats (e.g., every 2 weeks, every 3 months)
- **Start Date**: When the recurrence begins
- **End Conditions**:
  - Never ending
  - End after a specific number of occurrences
  - End by a specific date

#### Key Functions

```pascal
// Create recurrence patterns
function CreateDailyPattern(AInterval: Integer; AStartDate: TDateTime): Integer;
function CreateWeeklyPattern(AInterval: Integer; ADaysOfWeek: TDaysOfWeekSet; AStartDate: TDateTime): Integer;
function CreateMonthlyPattern(AInterval, ADayOfMonth: Integer; AStartDate: TDateTime): Integer;
function CreateYearlyPattern(AInterval, AMonthOfYear, ADayOfMonth: Integer; AStartDate: TDateTime): Integer;

// Configure pattern end conditions
function SetPatternEndDate(APatternID: Integer; AEndDate: TDateTime): Boolean;
function SetPatternMaxOccurrences(APatternID: Integer; AMaxOccurrences: Integer): Boolean;

// Create and manage recurring tasks
function CreateRecurringTask(ATemplateTaskID, APatternID: Integer): Integer;
function GetNextOccurrenceDate(ARecurringTaskID: Integer): TDateTime;
function ActivateRecurringTask(ARecurringTaskID: Integer): Boolean;
function DeactivateRecurringTask(ARecurringTaskID: Integer): Boolean;
```

### Portfolio Management Features

#### Project Tracking
The system now supports multi-project portfolio management with:

- **Project Status**: Planning, Active, On Hold, Closed, Cancelled
- **Budget Tracking**: Total budget and spent amounts
- **Completion Tracking**: Percentage completion
- **Timeline Management**: Start and end dates
- **Priority Levels**: 1-10 scale for project prioritization

#### Project Health Monitoring
Automatic health assessment based on:

- **Budget Status**: Healthy, At Risk, Over Budget
- **Schedule Status**: On Track, At Risk, Delayed
- **Overall Health**: Derived from budget and schedule status

#### Task-Project Linking
Tasks can be linked to one or more projects, allowing:

- Project-based task organization
- Task count per project
- Project progress tracking based on linked tasks

#### Key Functions

```pascal
// Project management
function CreateProject(const AName, ADescription: string; AStartDate, AEndDate: TDateTime; ABudget: Double): Integer;
function SetProjectStatus(AProjectID: Integer; AStatus: TProjectStatus): Boolean;
function SetProjectBudget(AProjectID: Integer; ABudget, ASpentAmount: Double): Boolean;
function SetProjectCompletion(AProjectID: Integer; APercentage: Double): Boolean;

// Task-project linking
function LinkTaskToProject(ATaskID, AProjectID: Integer): Integer;
function GetProjectTasks(AProjectID: Integer): TTaskArray;
function GetTaskProjects(ATaskID: Integer): TProjectArray;

// Portfolio analytics
function GetPortfolioSummary: string;
function GetProjectHealth(AProjectID: Integer): string;
function GetOverBudgetProjects: TProjectArray;
function GetDelayedProjects: TProjectArray;
function GetPortfolioValue: Double;
```

## Usage Examples

### Creating a Recurring Daily Standup

```pascal
var
  TM: TRecurringTaskManager;
  TaskID, PatternID, RecurringTaskID: Integer;
begin
  TM := TRecurringTaskManager.Create;
  
  // Create template task
  TaskID := TM.AddTask('Daily Standup', 'Team standup meeting', 
    'Meetings', tpMedium, IncDay(Now, 1), 0.25);
  
  // Create daily pattern
  PatternID := TM.CreateDailyPattern(1, Now); // Every day
  TM.SetPatternMaxOccurrences(PatternID, 30); // For 30 days
  
  // Create recurring task
  RecurringTaskID := TM.CreateRecurringTask(TaskID, PatternID);
  
  TM.Free;
end;
```

### Creating a Weekly Report Task

```pascal
var
  DaysOfWeek: TDaysOfWeekSet;
begin
  // Create template
  TaskID := TM.AddTask('Weekly Report', 'Submit progress report',
    'Reporting', tpHigh, IncDay(Now, 7), 2.0);
  
  // Every Monday, Wednesday, Friday
  DaysOfWeek := [dowMonday, dowWednesday, dowFriday];
  PatternID := TM.CreateWeeklyPattern(1, DaysOfWeek, Now);
  
  // End after 90 days
  TM.SetPatternEndDate(PatternID, IncDay(Now, 90));
  
  RecurringTaskID := TM.CreateRecurringTask(TaskID, PatternID);
end;
```

### Managing a Project Portfolio

```pascal
var
  ProjectID: Integer;
  Projects: TProjectArray;
begin
  // Create project
  ProjectID := TM.CreateProject('Website Redesign',
    'Complete redesign of company website',
    Now, IncDay(Now, 90), 50000.0);
  
  // Update project status
  TM.SetProjectStatus(ProjectID, psActive);
  TM.SetProjectCompletion(ProjectID, 25.0);
  TM.SetProjectBudget(ProjectID, 50000.0, 10000.0); // $10k spent
  
  // Link tasks to project
  TM.LinkTaskToProject(TaskID1, ProjectID);
  TM.LinkTaskToProject(TaskID2, ProjectID);
  
  // Get project health
  WriteLn(TM.GetProjectHealth(ProjectID));
  
  // Find over-budget projects
  Projects := TM.GetOverBudgetProjects;
  
  // Portfolio summary
  WriteLn(TM.GetPortfolioSummary);
end;
```

## Data Structures

### TRecurrencePattern
```pascal
type
  TRecurrencePattern = record
    ID: Integer;
    RecurrenceType: TRecurrenceType;
    Interval: Integer;
    DaysOfWeek: TDaysOfWeekSet;
    DayOfMonth: Integer;
    MonthOfYear: Integer;
    StartDate: TDateTime;
    EndType: TRecurrenceEndType;
    EndDate: TDateTime;
    MaxOccurrences: Integer;
    OccurrenceCount: Integer;
    LastGenerated: TDateTime;
    IsActive: Boolean;
  end;
```

### TProject
```pascal
type
  TProject = record
    ID: Integer;
    Name: string;
    Description: string;
    Status: TProjectStatus;
    StartDate: TDateTime;
    EndDate: TDateTime;
    Budget: Double;
    SpentAmount: Double;
    CompletionPercentage: Double;
    Priority: Integer;
    ManagerID: Integer;
    Tags: array of string;
    CreatedDate: TDateTime;
    IsActive: Boolean;
  end;
```

## Integration with Existing Features

The `TRecurringTaskManager` extends `TSmartTaskManager`, which means it inherits all features from:

- **TSmartTaskManager**: Workflow automation, risk assessment, AI-like predictions
- **TGamifiedTaskManager**: Achievements, points, productivity tracking
- **TTeamTaskManager**: Team member management, task assignments, custom fields
- **TEnhancedTaskManager**: Reminders, audit trails, archiving, attachments
- **TAdvancedTaskManager**: Work sessions, notes, dependencies, templates
- **TTaskManager**: Core task management functionality

This allows you to combine recurring tasks with all existing features, such as:
- Setting up recurring tasks with automatic reminders
- Linking recurring tasks to team members
- Tracking work sessions on recurring task instances
- Applying workflow rules to recurring tasks
- Earning achievements for completing recurring tasks

## Persistence

The recurring task and project data can be saved and loaded:

```pascal
function SaveRecurringDataToFile(const AFilename: string): Boolean;
function LoadRecurringDataFromFile(const AFilename: string): Boolean;
```

## Benefits

1. **Automation**: Automatically generate tasks on schedules without manual intervention
2. **Consistency**: Ensure recurring activities are never forgotten
3. **Portfolio Visibility**: Get a comprehensive view of all projects and their health
4. **Budget Control**: Track spending and identify over-budget projects early
5. **Resource Planning**: Understand workload across multiple projects
6. **Performance Tracking**: Monitor project progress and identify bottlenecks

## Future Enhancements

Potential future additions:
- Automatic task generation based on recurrence patterns
- Calendar integration for recurring tasks
- Project dependencies and critical path analysis
- Resource allocation across projects
- Gantt chart data export
- Project templates for common project types
- Milestone tracking within projects
- Project risk assessment and forecasting

## Test Coverage

The `solution10.pas` program provides comprehensive testing of all recurring task and portfolio management features, including:
- Creating various recurrence patterns
- Setting end conditions
- Next occurrence calculation
- Project creation and management
- Task-project linking
- Portfolio analytics
- Health monitoring
- Budget tracking

Run the test with:
```bash
fpc solution1/solution10.pas -obin/task_manager10 -O1 -Mobjfpc
bin/task_manager10
```

## Conclusion

The Recurring Tasks and Portfolio Management features significantly enhance the task manager's capabilities, making it suitable for complex project management scenarios while maintaining ease of use for personal task tracking.
