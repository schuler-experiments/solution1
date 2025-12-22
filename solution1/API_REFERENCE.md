
# Task Manager - Core API Reference

## Overview

This document provides a comprehensive reference for all functions and procedures in the core `taskmanager.pas` module. This is the foundation of the entire Task Manager system.

## Table of Contents

- [Data Types](#data-types)
- [Enumerations](#enumerations)
- [Records](#records)
- [TTaskManager Class](#ttaskmanager-class)
- [Core Operations](#core-operations)
- [Filtering Operations](#filtering-operations)
- [Sorting Operations](#sorting-operations)
- [Tag Operations](#tag-operations)
- [Statistics Operations](#statistics-operations)
- [Persistence Operations](#persistence-operations)
- [Utility Functions](#utility-functions)

---

## Data Types

### TTaskStatus
**Purpose**: Represents the current status/state of a task in its lifecycle.

**Values**:
- `tsNotStarted` (0) - Task has not been started
- `tsInProgress` (1) - Task is currently being worked on
- `tsCompleted` (2) - Task has been successfully completed
- `tsCancelled` (3) - Task was cancelled and will not be completed
- `tsOnHold` (4) - Task is temporarily on hold/paused

**Usage Example**:
```pascal
var
  Status: TTaskStatus;
begin
  Status := tsInProgress;
  if Status = tsCompleted then
    WriteLn('Task is done!');
end;
```

### TTaskPriority
**Purpose**: Represents the relative importance/urgency of a task.

**Values**:
- `tpLow` (0) - Low priority, can be done anytime
- `tpMedium` (1) - Medium priority, should be done soon
- `tpHigh` (2) - High priority, should be prioritized
- `tpCritical` (3) - Critical priority, needs immediate attention

**Usage Example**:
```pascal
CriticalTasks := Manager.FilterByPriority(tpCritical);
```

### TSortCriteria
**Purpose**: Specifies the field by which tasks should be sorted.

**Values**:
- `scTitle` (0) - Sort alphabetically by task title
- `scPriority` (1) - Sort by priority (Low to Critical)
- `scDueDate` (2) - Sort by due date (earliest first)
- `scCreatedDate` (3) - Sort by creation date (oldest first)
- `scStatus` (4) - Sort by status
- `scCategory` (5) - Sort alphabetically by category

**Usage Example**:
```pascal
UrgentTasks := Manager.SortTasks(scDueDate);  // Earliest due dates first
```

---

## Records

### TTask Record
**Purpose**: Contains all data for a single task.

**Fields**:

| Field | Type | Purpose |
|-------|------|---------|
| `ID` | Integer | Unique identifier (auto-generated, starts at 1) |
| `Title` | string | Task name/title (required) |
| `Description` | string | Detailed description of the task |
| `Status` | TTaskStatus | Current state of the task |
| `Priority` | TTaskPriority | Importance level of the task |
| `Category` | string | Organizational category (e.g., "Backend", "Frontend") |
| `CreatedDate` | TDateTime | Date/time when task was created |
| `DueDate` | TDateTime | Target completion date/time |
| `CompletedDate` | TDateTime | Date/time when task was completed |
| `EstimatedHours` | Double | Estimated hours to complete (for planning) |
| `ActualHours` | Double | Actual hours spent on the task |
| `Tags` | array of string | Dynamic array of string tags for flexible organization |

**Important Notes**:
- `ID` is automatically assigned by `TTaskManager` when adding a task
- `Tags` is a dynamic array with no limit on number of tags
- `CompletedDate` is automatically set when status changes to `tsCompleted`
- All date/time fields use TDateTime format (use `EncodeDate()`, `Now()`, etc.)

### TTaskArray Type
**Definition**: `TTaskArray = array of TTask`

**Purpose**: Dynamic array type for returning multiple tasks from queries.

---

## TTaskManager Class

### Constructor: Create
**Signature**:
```pascal
constructor Create;
```

**Purpose**: Initializes a new TaskManager instance. Must be called before using the manager.

**Example**:
```pascal
var
  Manager: TTaskManager;
begin
  Manager := TTaskManager.Create;
  try
    // Use Manager here
  finally
    Manager.Free;
  end;
end;
```

### Destructor: Destroy
**Signature**:
```pascal
destructor Destroy; override;
```

**Purpose**: Cleans up and releases memory. Always call `Manager.Free` to avoid memory leaks.

---

## Core Operations

### Function: AddTask (Overload 1 - Simple)
**Signature**:
```pascal
function AddTask(const ATitle, ADescription: string; 
                 APriority: TTaskPriority; ADueDate: TDateTime): Integer;
```

**Purpose**: Creates and adds a new task with basic parameters.

**Returns**: Integer - The ID of the newly created task

**Example**:
```pascal
var
  TaskID: Integer;
begin
  TaskID := Manager.AddTask(
    'Write documentation',
    'Document all API functions',
    tpMedium,
    EncodeDate(2024, 12, 31)
  );
end;
```

### Function: AddTask (Overload 2 - Full)
**Signature**:
```pascal
function AddTask(const ATitle, ADescription, ACategory: string;
                 APriority: TTaskPriority; ADueDate: TDateTime;
                 AEstimatedHours: Double): Integer;
```

**Purpose**: Creates and adds a new task with category and estimated hours.

**Returns**: Integer - The ID of the newly created task

### Function: DeleteTask
**Signature**:
```pascal
function DeleteTask(ATaskID: Integer): Boolean;
```

**Purpose**: Removes a task from the task list by its ID.

**Returns**: True if successful, False if task not found

### Function: GetAllTasks
**Signature**:
```pascal
function GetAllTasks: TTaskArray;
```

**Purpose**: Retrieves all tasks in the manager.

**Returns**: TTaskArray - Array containing all tasks

### Function: GetTaskByID
**Signature**:
```pascal
function GetTaskByID(ATaskID: Integer): Integer;
```

**Purpose**: Finds a task by its ID and returns its array index.

**Returns**: Index if found, -1 if not found

---

## Update Operations

### Function: UpdateTaskTitle
**Signature**:
```pascal
function UpdateTaskTitle(ATaskID: Integer; const ANewTitle: string): Boolean;
```

### Function: UpdateTaskDescription
**Signature**:
```pascal
function UpdateTaskDescription(ATaskID: Integer; const ANewDesc: string): Boolean;
```

### Function: UpdateTaskStatus
**Signature**:
```pascal
function UpdateTaskStatus(ATaskID: Integer; ANewStatus: TTaskStatus): Boolean;
```

**Note**: Setting status to `tsCompleted` automatically sets `CompletedDate`.

### Function: UpdateTaskPriority
**Signature**:
```pascal
function UpdateTaskPriority(ATaskID: Integer; ANewPriority: TTaskPriority): Boolean;
```

### Function: UpdateTaskDueDate
**Signature**:
```pascal
function UpdateTaskDueDate(ATaskID: Integer; ANewDueDate: TDateTime): Boolean;
```

### Function: UpdateTaskCategory
**Signature**:
```pascal
function UpdateTaskCategory(ATaskID: Integer; const ANewCategory: string): Boolean;
```

### Function: UpdateTaskEstimatedHours
**Signature**:
```pascal
function UpdateTaskEstimatedHours(ATaskID: Integer; AHours: Double): Boolean;
```

### Function: UpdateTaskActualHours
**Signature**:
```pascal
function UpdateTaskActualHours(ATaskID: Integer; AHours: Double): Boolean;
```

---

## Filtering Operations

### Function: FilterByStatus
**Signature**:
```pascal
function FilterByStatus(AStatus: TTaskStatus): TTaskArray;
```

**Purpose**: Returns all tasks with a specific status.

### Function: FilterByPriority
**Signature**:
```pascal
function FilterByPriority(APriority: TTaskPriority): TTaskArray;
```

**Purpose**: Returns all tasks with a specific priority level.

### Function: FilterByCategory
**Signature**:
```pascal
function FilterByCategory(const ACategory: string): TTaskArray;
```

**Purpose**: Returns all tasks in a specific category.

### Function: FilterByDateRange
**Signature**:
```pascal
function FilterByDateRange(AStartDate, AEndDate: TDateTime): TTaskArray;
```

**Purpose**: Returns all tasks with due dates within a specified range (inclusive).

### Function: FilterByTag
**Signature**:
```pascal
function FilterByTag(const ATag: string): TTaskArray;
```

**Purpose**: Returns all tasks that have a specific tag.

### Function: SearchByTitle
**Signature**:
```pascal
function SearchByTitle(const ASearchTerm: string): TTaskArray;
```

**Purpose**: Returns all tasks whose title contains the search term (case-insensitive).

---

## Sorting Operations

### Function: SortTasks
**Signature**:
```pascal
function SortTasks(ACriteria: TSortCriteria): TTaskArray;
```

**Purpose**: Returns all tasks sorted in ascending order by the specified criteria.

**Time Complexity**: O(n log n) using QuickSort algorithm

### Function: SortTasksDescending
**Signature**:
```pascal
function SortTasksDescending(ACriteria: TSortCriteria): TTaskArray;
```

**Purpose**: Returns all tasks sorted in descending order by the specified criteria.

---

## Tag Operations

### Function: AddTagToTask
**Signature**:
```pascal
function AddTagToTask(ATaskID: Integer; const ATag: string): Boolean;
```

**Purpose**: Adds a tag to a task.

### Function: RemoveTagFromTask
**Signature**:
```pascal
function RemoveTagFromTask(ATaskID: Integer; const ATag: string): Boolean;
```

**Purpose**: Removes the first occurrence of a tag from a task.

---

## Statistics Operations

### Function: GetTaskCount
**Signature**:
```pascal
function GetTaskCount: Integer;
```

**Purpose**: Returns the total number of tasks.

### Function: GetCompletedCount
**Signature**:
```pascal
function GetCompletedCount: Integer;
```

**Purpose**: Returns the number of completed tasks.

### Function: GetPendingCount
**Signature**:
```pascal
function GetPendingCount: Integer;
```

**Purpose**: Returns the number of pending (not completed) tasks.

### Function: GetOverdueCount
**Signature**:
```pascal
function GetOverdueCount: Integer;
```

**Purpose**: Returns the number of tasks past their due date and not completed.

### Function: GetCompletionRate
**Signature**:
```pascal
function GetCompletionRate: Double;
```

**Purpose**: Returns the completion percentage (0.0 to 100.0).

**Formula**: (Completed / Total) * 100

### Function: GetAverageCompletionTime
**Signature**:
```pascal
function GetAverageCompletionTime: Double;
```

**Purpose**: Returns average days from creation to completion for completed tasks.

### Function: GetTotalEstimatedHours
**Signature**:
```pascal
function GetTotalEstimatedHours: Double;
```

**Purpose**: Returns the sum of all estimated hours across all tasks.

### Function: GetTotalActualHours
**Signature**:
```pascal
function GetTotalActualHours: Double;
```

**Purpose**: Returns the sum of all actual hours spent across all tasks.

### Function: GetTasksByCategory
**Signature**:
```pascal
function GetTasksByCategory: string;
```

**Purpose**: Returns a formatted string showing task counts per category.

---

## Persistence Operations

### Function: SaveToFile
**Signature**:
```pascal
function SaveToFile(const AFilename: string): Boolean;
```

**Purpose**: Saves all tasks to a binary file.

**Returns**: True if successful, False if write error

### Function: LoadFromFile
**Signature**:
```pascal
function LoadFromFile(const AFilename: string): Boolean;
```

**Purpose**: Loads all tasks from a previously saved file.

**Returns**: True if successful, False if read error

### Function: ExportToCSV
**Signature**:
```pascal
function ExportToCSV: string;
```

**Purpose**: Returns all tasks formatted as CSV (Comma-Separated Values).

**Columns**: ID, Title, Description, Status, Priority, Category, DueDate, EstimatedHours, ActualHours, Tags

### Procedure: ClearAllTasks
**Signature**:
```pascal
procedure ClearAllTasks;
```

**Purpose**: Removes all tasks from the manager.

**Warning**: This operation cannot be undone!

---

## Utility Functions

### Function: TaskStatusToString
**Signature**:
```pascal
function TaskStatusToString(AStatus: TTaskStatus): string;
```

**Purpose**: Converts a TTaskStatus enumeration value to a human-readable string.

**Mappings**:
- `tsNotStarted` → "Not Started"
- `tsInProgress` → "In Progress"
- `tsCompleted` → "Completed"
- `tsCancelled` → "Cancelled"
- `tsOnHold` → "On Hold"

### Function: TaskPriorityToString
**Signature**:
```pascal
function TaskPriorityToString(APriority: TTaskPriority): string;
```

**Purpose**: Converts a TTaskPriority enumeration value to a human-readable string.

**Mappings**:
- `tpLow` → "Low"
- `tpMedium` → "Medium"
- `tpHigh` → "High"
- `tpCritical` → "Critical"

### Function: TaskToString
**Signature**:
```pascal
function TaskToString(const ATask: TTask): string;
```

**Purpose**: Converts a TTask record to a formatted string representation.

---

## Quick Reference - Common Patterns

### Pattern 1: Create and Add Tasks
```pascal
var
  TM: TTaskManager;
begin
  TM := TTaskManager.Create;
  try
    TM.AddTask('Task 1', 'Description', tpHigh, Now + 7);
    TM.AddTask('Task 2', 'Description', 'Backend', tpMedium, Now + 14, 5.0);
  finally
    TM.Free;
  end;
end;
```

### Pattern 2: Filter and Display
```pascal
var
  HighPriority: TTaskArray;
  i: Integer;
begin
  HighPriority := Manager.FilterByPriority(tpHigh);
  for i := 0 to Length(HighPriority) - 1 do
    WriteLn(HighPriority[i].Title);
end;
```

### Pattern 3: Update and Track Progress
```pascal
begin
  Manager.UpdateTaskStatus(TaskID, tsInProgress);
  Manager.UpdateTaskActualHours(TaskID, 2.5);
  Manager.UpdateTaskStatus(TaskID, tsCompleted);
end;
```

### Pattern 4: Statistics and Reporting
```pascal
begin
  WriteLn('Total: ', Manager.GetTaskCount);
  WriteLn('Done: ', Manager.GetCompletedCount);
  WriteLn('Pending: ', Manager.GetPendingCount);
  WriteLn('Overdue: ', Manager.GetOverdueCount);
  WriteLn(Format('Rate: %.1f%%', [Manager.GetCompletionRate]));
end;
```

### Pattern 5: Persistence
```pascal
begin
  Manager.SaveToFile('tasks.dat');
  if FileExists('tasks.dat') then
    Manager.LoadFromFile('tasks.dat');
end;
```

---

## Error Handling Notes

- Most update/delete operations return Boolean - always check the return value
- `GetTaskByID` returns -1 if not found (not 0, since IDs start at 1)
- Filtering operations return empty array if no matches (not nil)
- All operations are memory-safe when TTaskManager is properly freed

---

**Document Version**: 1.0
**Last Updated**: 2024
**Applicable to**: taskmanager.pas core module
