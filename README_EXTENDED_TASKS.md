# TaskManager Extended (taskmanagerext.pas)

## Overview

The **TaskManager Extended** module (`taskmanagerext.pas`) extends the core TaskManager functionality with advanced features for managing complex task hierarchies, recurring tasks, and batch operations. It provides enterprise-grade capabilities for sophisticated task management scenarios.

## Key Features

### 1. **Extended Task Structure**

The module defines an `TExtendedTask` record that builds upon the base `TTask` type with additional fields:

```pascal
TExtendedTask = record
  BaseTask: TTask;                    // Core task data
  ParentID: Integer;                  // For subtask relationships (0 = no parent)
  RecurrencePattern: TRecurrencePattern;  // Daily, Weekly, Monthly, etc.
  RecurrenceCount: Integer;           // Number of times task has recurred
  NextRecurrenceDate: TDateTime;      // When next occurrence is due
  PriorityScore: Double;              // Auto-calculated priority metric
  IsRecurring: Boolean;               // Whether task repeats
  IsSubtask: Boolean;                 // Whether this is a child task
  SubtaskIDs: array of Integer;       // IDs of child tasks
  LastModifiedDate: TDateTime;        // Track last change
  NotificationDays: Integer;          // Alert N days before due date
end;
```

### 2. **Recurrence Patterns**

Tasks can be configured to recur on various schedules:

```pascal
TRecurrencePattern = (
  rpNone,        // One-time task
  rpDaily,       // Every day
  rpWeekly,      // Every week
  rpBiWeekly,    // Every two weeks
  rpMonthly,     // Every month
  rpQuarterly,   // Every three months
  rpYearly       // Every year
);
```

### 3. **Task Hierarchy Support**

The module enables parent-child task relationships, allowing complex project decomposition:

- **Parent Tasks**: Top-level tasks that can have subtasks
- **Subtasks**: Child tasks that belong to a parent task
- **Hierarchical Navigation**: Retrieve all subtasks for a parent or get the complete task hierarchy

### 4. **Priority Scoring System**

Automatic priority calculation based on:
- Task priority level
- Days until due date
- Recurrence status
- Completion status

```pascal
function CalculatePriorityScore(const ATask: TExtendedTask): Double;
function UpdatePriorityScores: Integer;
function GetTopPriorityTasks(ACount: Integer): TExtendedTaskArray;
```

### 5. **Recurring Task Management**

Automatically generate new task instances based on recurrence patterns:

```pascal
function SetTaskRecurrence(ATaskID: Integer; APattern: TRecurrencePattern): Boolean;
function GenerateNextRecurrence(ATaskID: Integer): Integer;
procedure UpdateAllRecurringTasks;
function GetRecurringTasks: TExtendedTaskArray;
```

### 6. **Batch Operations**

Perform bulk updates efficiently:

```pascal
// All batch operations return TBatchOperationResult with success/failure counts
function BatchUpdateStatus(const ATaskIDs: array of Integer; ANewStatus: TTaskStatus): TBatchOperationResult;
function BatchUpdatePriority(const ATaskIDs: array of Integer; ANewPriority: TTaskPriority): TBatchOperationResult;
function BatchUpdateCategory(const ATaskIDs: array of Integer; const ANewCategory: string): TBatchOperationResult;
function BatchDeleteTasks(const ATaskIDs: array of Integer): TBatchOperationResult;
function BatchAddTag(const ATaskIDs: array of Integer; const ATag: string): TBatchOperationResult;
```

#### Batch Operation Result Structure

```pascal
TBatchOperationResult = record
  SuccessCount: Integer;      // Number of successful operations
  FailureCount: Integer;      // Number of failed operations
  TotalProcessed: Integer;    // Total items processed
  Message: string;            // Status message
end;
```

## Core API Reference

### Task Creation & Management

#### Add Extended Task
```pascal
function AddExtendedTask(
  const ATitle, ADescription, ACategory: string;
  APriority: TTaskPriority;
  ADueDate: TDateTime;
  AEstimatedHours: Double;
  ARecurrence: TRecurrencePattern
): Integer;
```
Creates a new extended task and returns its ID. The task can be configured as a one-time or recurring task.

#### Add Subtask
```pascal
function AddSubtask(
  AParentID: Integer;
  const ATitle, ADescription: string;
  APriority: TTaskPriority;
  ADueDate: TDateTime
): Integer;
```
Creates a subtask linked to a parent task. Useful for breaking down complex projects into smaller units.

#### Retrieve Tasks
```pascal
function GetSubtasks(AParentID: Integer): TExtendedTaskArray;
function GetTaskHierarchy(ATaskID: Integer): string;
function GetAllExtendedTasks: TExtendedTaskArray;
```

### Recurrence Management

```pascal
function SetTaskRecurrence(ATaskID: Integer; APattern: TRecurrencePattern): Boolean;
function GetRecurringTasks: TExtendedTaskArray;
function GenerateNextRecurrence(ATaskID: Integer): Integer;
procedure UpdateAllRecurringTasks;
function RecurrencePatternToString(APattern: TRecurrencePattern): string;
```

### Priority & Urgency

```pascal
function GetTopPriorityTasks(ACount: Integer): TExtendedTaskArray;
function GetTasksNeedingAttention: TExtendedTaskArray;
function GetTasksDueSoon(ADays: Integer): TExtendedTaskArray;
```

### Reporting & Analytics

The module provides comprehensive reporting capabilities:

```pascal
function GetProductivityReport: string;           // Overall productivity metrics
function GetCategoryPerformance: string;          // Performance by category
function GetTimeManagementReport: string;         // Time allocation analysis
function GetTaskComplexityAnalysis: string;       // Task complexity assessment
```

### Data Persistence

```pascal
function ExportExtendedToCSV: string;
function SaveExtendedToFile(const AFilename: string): Boolean;
function LoadExtendedFromFile(const AFilename: string): Boolean;
function ExtendedTaskToString(const ATask: TExtendedTask): string;
```

## Usage Examples

### Example 1: Create a Recurring Task

```pascal
var
  Manager: TExtendedTaskManager;
  TaskID: Integer;
begin
  Manager := TExtendedTaskManager.Create;
  try
    // Create a weekly team meeting task
    TaskID := Manager.AddExtendedTask(
      'Weekly Team Meeting',
      'Discuss project progress and blockers',
      'Meetings',
      tpHigh,
      EncodeDate(2024, 2, 15),
      1.0,  // 1 hour estimated
      rpWeekly  // Recurs every week
    );
    WriteLn('Created recurring task with ID: ', TaskID);
  finally
    Manager.Free;
  end;
end;
```

### Example 2: Create Task Hierarchy

```pascal
var
  Manager: TExtendedTaskManager;
  ProjectID, SubtaskID1, SubtaskID2: Integer;
begin
  Manager := TExtendedTaskManager.Create;
  try
    // Create main project task
    ProjectID := Manager.AddExtendedTask(
      'Website Redesign',
      'Complete redesign of company website',
      'Frontend',
      tpHigh,
      EncodeDate(2024, 3, 31),
      40.0,
      rpNone
    );
    
    // Add subtasks
    SubtaskID1 := Manager.AddSubtask(
      ProjectID,
      'Design mockups',
      'Create UI mockups in Figma',
      tpHigh,
      EncodeDate(2024, 2, 28)
    );
    
    SubtaskID2 := Manager.AddSubtask(
      ProjectID,
      'Implement frontend',
      'Build responsive components',
      tpHigh,
      EncodeDate(2024, 3, 15)
    );
    
  finally
    Manager.Free;
  end;
end;
```

### Example 3: Batch Operations

```pascal
var
  Manager: TExtendedTaskManager;
  TaskIDs: array[0..2] of Integer;
  Result: TBatchOperationResult;
begin
  Manager := TExtendedTaskManager.Create;
  try
    TaskIDs[0] := 1;
    TaskIDs[1] := 2;
    TaskIDs[2] := 3;
    
    // Update status for multiple tasks
    Result := Manager.BatchUpdateStatus(TaskIDs, tsCompleted);
    WriteLn(Format('Batch update: %d succeeded, %d failed',
      [Result.SuccessCount, Result.FailureCount]));
    WriteLn('Message: ', Result.Message);
  finally
    Manager.Free;
  end;
end;
```

### Example 4: Priority-based Task Retrieval

```pascal
var
  Manager: TExtendedTaskManager;
  TopTasks: TExtendedTaskArray;
  i: Integer;
begin
  Manager := TExtendedTaskManager.Create;
  try
    // Update priority scores
    Manager.UpdatePriorityScores;
    
    // Get top 5 priority tasks
    TopTasks := Manager.GetTopPriorityTasks(5);
    
    for i := Low(TopTasks) to High(TopTasks) do
      WriteLn(Format('Task: %s (Score: %.2f)',
        [TopTasks[i].BaseTask.Title, TopTasks[i].PriorityScore]));
  finally
    Manager.Free;
  end;
end;
```

## Integration with Core TaskManager

The `TExtendedTaskManager` class inherits from `TTaskManager`, meaning it provides all core functionality plus extended features:

- All basic task CRUD operations
- Search, filter, and sort capabilities
- Tag and category management
- Time tracking features
- All extended features described above

## Performance Considerations

1. **Batch Operations**: Use batch functions when updating multiple tasks to improve performance
2. **Recurrence Processing**: Call `UpdateAllRecurringTasks` periodically (e.g., daily) to generate new recurring instances
3. **Priority Scoring**: The `UpdatePriorityScores` function recalculates scores for all tasks—use judiciously for large datasets

## Data Consistency

- Subtask IDs are maintained in the `SubtaskIDs` array of the parent task
- Parent-child relationships are bidirectional (parent tracks children, children track parent)
- Deleting a parent task can be configured to cascade-delete subtasks or reassign them
- Recurring task generation respects the recurrence pattern and frequency

## File Format Support

The module supports CSV export and file persistence for backup and data sharing:

```pascal
// Export all tasks to CSV
var CSVData := Manager.ExportExtendedToCSV;

// Save to file
Manager.SaveExtendedToFile('tasks_backup.txt');

// Load from file
Manager.LoadExtendedFromFile('tasks_backup.txt');
```

## Related Modules

- **taskmanager.pas**: Core task management functionality
- **taskmanagerrecurring.pas**: Additional recurring task features
- **taskmanagerteam.pas**: Team collaboration features that work with extended tasks
- **taskmanagertimetracking.pas**: Enhanced time tracking capabilities

## Summary

The TaskManager Extended module provides sophisticated task management capabilities suitable for:
- Complex project management
- Recurring task automation
- Priority-based task organization
- Team collaboration with task hierarchies
- Batch operations for efficiency
- Comprehensive analytics and reporting

It extends the core TaskManager with enterprise-grade features while maintaining backward compatibility and ease of use.
