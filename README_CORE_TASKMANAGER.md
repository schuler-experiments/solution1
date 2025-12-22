
# Core Task Manager Module

## Overview

The **Task Manager Core Module** (`taskmanager.pas`) is the foundation of the entire task management system. It provides the fundamental data structures, types, and operations for managing tasks in Free Pascal.

This module implements the basic CRUD (Create, Read, Update, Delete) operations, filtering, searching, sorting, and tag management functionality that all other modules build upon.

## Architecture

### Core Data Structures

#### Task Status
Tasks can be in one of five states:
- **tsNotStarted**: Task has been created but work hasn't begun
- **tsInProgress**: Active work is being done on the task
- **tsCompleted**: Task has been finished
- **tsCancelled**: Task has been abandoned
- **tsOnHold**: Task is temporarily paused

```pascal
TTaskStatus = (tsNotStarted, tsInProgress, tsCompleted, tsCancelled, tsOnHold);
```

#### Task Priority
Tasks are prioritized on a four-level scale:
- **tpLow**: Non-urgent, can be deferred
- **tpMedium**: Normal priority
- **tpHigh**: Requires prompt attention
- **tpCritical**: Requires immediate action

```pascal
TTaskPriority = (tpLow, tpMedium, tpHigh, tpCritical);
```

#### Task Record (TTask)
The fundamental data structure representing a single task:

```pascal
TTask = record
  ID: Integer;                    // Unique task identifier
  Title: string;                  // Task title (required)
  Description: string;            // Detailed description
  Status: TTaskStatus;            // Current task status
  Priority: TTaskPriority;        // Task priority level
  Category: string;               // Organizational category
  CreatedDate: TDateTime;         // When task was created
  DueDate: TDateTime;             // When task is due
  CompletedDate: TDateTime;       // When task was completed
  EstimatedHours: Double;         // Estimated work hours
  ActualHours: Double;            // Actual hours spent
  Tags: array of string;          // Flexible labeling system
end;
```

#### Sort Criteria
Tasks can be sorted by any of these criteria:
- **scTitle**: Alphabetical by title
- **scPriority**: By priority level
- **scDueDate**: By due date
- **scCreatedDate**: By creation date
- **scStatus**: By current status
- **scCategory**: By category

```pascal
TSortCriteria = (scTitle, scPriority, scDueDate, scCreatedDate, scStatus, scCategory);
```

## Core Class: TTaskManager

The `TTaskManager` class manages a collection of tasks and provides all basic operations.

### Initialization and Cleanup

```pascal
constructor Create;              // Initialize the task manager
destructor Destroy; override;     // Clean up resources
```

### Task Creation

#### AddTask (Overload 1 - Basic)
```pascal
function AddTask(const ATitle, ADescription: string; 
                 APriority: TTaskPriority; ADueDate: TDateTime): Integer;
```
Creates a basic task with title, description, priority, and due date.

**Returns**: Task ID if successful, -1 if failed

**Example**:
```pascal
var
  Manager: TTaskManager;
  TaskID: Integer;
begin
  Manager := TTaskManager.Create;
  TaskID := Manager.AddTask('Review Code', 'Review pull requests', tpHigh, Now + 1);
  // TaskID now contains the unique ID of the new task
  Manager.Free;
end;
```

#### AddTask (Overload 2 - Extended)
```pascal
function AddTask(const ATitle, ADescription, ACategory: string;
                 APriority: TTaskPriority; ADueDate: TDateTime;
                 AEstimatedHours: Double): Integer;
```
Creates a task with additional category and time estimation information.

**Returns**: Task ID if successful, -1 if failed

### Task Deletion

```pascal
function DeleteTask(ATaskID: Integer): Boolean;
```
Removes a task from the system permanently.

**Returns**: `True` if deletion successful, `False` if task not found

### Task Updates

The module provides specialized update functions for each task property:

```pascal
function UpdateTaskTitle(ATaskID: Integer; const ANewTitle: string): Boolean;
function UpdateTaskDescription(ATaskID: Integer; const ANewDesc: string): Boolean;
function UpdateTaskStatus(ATaskID: Integer; ANewStatus: TTaskStatus): Boolean;
function UpdateTaskPriority(ATaskID: Integer; ANewPriority: TTaskPriority): Boolean;
function UpdateTaskDueDate(ATaskID: Integer; ANewDueDate: TDateTime): Boolean;
function UpdateTaskCategory(ATaskID: Integer; const ANewCategory: string): Boolean;
function UpdateTaskEstimatedHours(ATaskID: Integer; AHours: Double): Boolean;
function UpdateTaskActualHours(ATaskID: Integer; AHours: Double): Boolean;
```

All update functions return `True` if successful, `False` if the task ID is not found.

### Task Retrieval

#### GetTaskByID
```pascal
function GetTaskByID(ATaskID: Integer): Integer;
```
Retrieves the array index of a task by its ID.

**Returns**: Array index if found, -1 if not found

#### GetAllTasks
```pascal
function GetAllTasks: TTaskArray;
```
Retrieves a copy of all tasks in the system.

**Returns**: Dynamic array of all tasks

### Filtering Operations

#### Filter by Status
```pascal
function FilterByStatus(AStatus: TTaskStatus): TTaskArray;
```
Returns all tasks with a specific status.

**Example**:
```pascal
var
  CompletedTasks: TTaskArray;
begin
  CompletedTasks := Manager.FilterByStatus(tsCompleted);
  WriteLn('Completed tasks: ', Length(CompletedTasks));
end;
```

#### Filter by Priority
```pascal
function FilterByPriority(APriority: TTaskPriority): TTaskArray;
```
Returns all tasks with a specific priority level.

#### Filter by Date Range
```pascal
function FilterByDateRange(AStartDate, AEndDate: TDateTime): TTaskArray;
```
Returns all tasks with due dates within the specified range.

#### Filter by Category
```pascal
function FilterByCategory(const ACategory: string): TTaskArray;
```
Returns all tasks in a specific category.

#### Filter by Tags
```pascal
function FilterByTag(const ATag: string): TTaskArray;
```
Returns all tasks that have the specified tag.

### Search Functionality

```pascal
function SearchByTitle(const ASearchTerm: string): TTaskArray;
```
Searches for tasks by title (case-insensitive partial match).

**Example**:
```pascal
var
  Results: TTaskArray;
begin
  Results := Manager.SearchByTitle('Documentation');
  WriteLn('Found ', Length(Results), ' matching tasks');
end;
```

### Tag Management

Tags provide a flexible alternative to categories for organizing tasks. A single task can have multiple tags.

#### Add Tag to Task
```pascal
function AddTagToTask(ATaskID: Integer; const ATag: string): Boolean;
```
Adds a tag to a task (prevents duplicates).

**Returns**: `True` if successful, `False` if task not found or tag already exists

#### Remove Tag from Task
```pascal
function RemoveTagFromTask(ATaskID: Integer; const ATag: string): Boolean;
```
Removes a specific tag from a task.

**Returns**: `True` if successful, `False` if task or tag not found

#### Get Tags for Task
```pascal
function GetTaskTags(ATaskID: Integer): array of string;
```
Retrieves all tags associated with a task.

### Sorting

```pascal
function SortTasks(ACriteria: TSortCriteria; AAscending: Boolean = True): TTaskArray;
```
Returns all tasks sorted according to the specified criteria.

**Parameters**:
- `ACriteria`: The field to sort by
- `AAscending`: `True` for ascending order, `False` for descending

**Example**:
```pascal
var
  HighPriorityFirst: TTaskArray;
begin
  // Sort by priority in descending order (highest first)
  HighPriorityFirst := Manager.SortTasks(scPriority, False);
end;
```

### Statistics

#### Task Count
```pascal
function GetTaskCount: Integer;
property TaskCount: Integer read GetTaskCount;
```
Returns the total number of tasks in the system.

#### Count by Status
```pascal
function GetTaskCountByStatus(AStatus: TTaskStatus): Integer;
```
Returns count of tasks with a specific status.

#### Count Completed
```pascal
function GetCompletedTaskCount: Integer;
```
Returns number of completed tasks.

#### Count Overdue
```pascal
function GetOverdueTaskCount: Integer;
```
Returns number of tasks past their due date with status other than Completed or Cancelled.

#### Completion Rate
```pascal
function GetCompletionRate: Double;
```
Returns percentage of completed tasks (0-100).

#### Count by Priority
```pascal
function GetTaskCountByPriority(APriority: TTaskPriority): Integer;
```
Returns count of tasks with a specific priority level.

#### Count by Category
```pascal
function GetTaskCountByCategory(const ACategory: string): Integer;
```
Returns count of tasks in a specific category.

## Usage Patterns

### Basic Task Management Workflow

```pascal
program TaskManagerExample;

uses
  SysUtils, taskmanager;

var
  Manager: TTaskManager;
  TaskID: Integer;
  AllTasks: TTaskArray;
  i: Integer;
begin
  { Initialize }
  Manager := TTaskManager.Create;
  
  { Add some tasks }
  TaskID := Manager.AddTask(
    'Implement Login',
    'Add user authentication',
    'Backend',
    tpHigh,
    Now + 3,
    8.0
  );
  
  { Update task status }
  Manager.UpdateTaskStatus(TaskID, tsInProgress);
  
  { Track time }
  Manager.UpdateTaskActualHours(TaskID, 2.5);
  
  { Add tags }
  Manager.AddTagToTask(TaskID, 'security');
  Manager.AddTagToTask(TaskID, 'priority');
  
  { Retrieve and display }
  AllTasks := Manager.GetAllTasks;
  for i := 0 to Length(AllTasks) - 1 do
  begin
    WriteLn('Task ', AllTasks[i].ID, ': ', AllTasks[i].Title);
  end;
  
  { Cleanup }
  Manager.Free;
end.
```

### Filtering and Searching

```pascal
var
  HighPriorityTasks: TTaskArray;
  BackendTasks: TTaskArray;
  SearchResults: TTaskArray;
begin
  { Get all high-priority tasks }
  HighPriorityTasks := Manager.FilterByPriority(tpHigh);
  
  { Get all backend tasks }
  BackendTasks := Manager.FilterByCategory('Backend');
  
  { Search for specific tasks }
  SearchResults := Manager.SearchByTitle('login');
  
  WriteLn('Found ', Length(SearchResults), ' tasks matching "login"');
end;
```

## Internal Implementation Details

### Sorting Algorithm

The module uses **QuickSort** for efficient task sorting:

```pascal
procedure QuickSortTasks(var ATasks: TTaskArray; ALeft, ARight: Integer; 
                        ACriteria: TSortCriteria);
```

This private procedure implements the quick sort algorithm with O(n log n) average complexity.

### Task Comparison

```pascal
function CompareTasks(const A, B: TTask; ACriteria: TSortCriteria): Integer;
```

This private function handles the comparison logic for different sort criteria, returning:
- Negative value if A < B
- Zero if A = B
- Positive value if A > B

## Integration with Other Modules

The Core Task Manager is the foundation for all other modules:

- **taskmanageradvanced.pas**: Adds advanced filtering and analytics
- **taskmanagerext.pas**: Extends core functionality
- **taskmanagerenhanced.pas**: Provides enhanced features
- **taskmanagerfocus.pas**: Adds focus and time management
- **taskmanagertimetracking.pas**: Extends time tracking capabilities
- **taskmanagercomments.pas**: Adds collaboration features
- And many more...

All extension modules inherit from or use `TTaskManager` as their base.

## Best Practices

1. **Always create and free the manager**: Use `try...finally` to ensure cleanup
   ```pascal
   Manager := TTaskManager.Create;
   try
     // Use manager
   finally
     Manager.Free;
   end;
   ```

2. **Check return values**: Always verify that operations succeeded
   ```pascal
   if not Manager.UpdateTaskStatus(TaskID, tsCompleted) then
     WriteLn('Failed to update task');
   ```

3. **Use overloaded AddTask appropriately**: 
   - Use the basic version for simple tasks
   - Use the extended version when category and time estimation are needed

4. **Leverage tags for flexibility**: Use tags in addition to categories for multi-dimensional organization

5. **Validate task IDs**: Always check that a task exists before operating on it
   ```pascal
   if Manager.GetTaskByID(TaskID) >= 0 then
     Manager.DeleteTask(TaskID);
   ```

## Performance Considerations

- **GetAllTasks()**: Returns a copy of all tasks - O(n) complexity
- **FilterByStatus/Priority/Category**: Linear search - O(n) complexity
- **SearchByTitle**: Case-insensitive search - O(n) complexity
- **SortTasks**: QuickSort - O(n log n) average, O(n²) worst case
- **AddTask**: O(1) amortized complexity
- **DeleteTask**: O(n) due to array reorganization

For large task lists (>10,000 tasks), consider implementing indexing or caching strategies in higher-level modules.

## Related Documentation

- [README.md](README.md) - General system overview
- [README_EXTENDED.md](README_EXTENDED.md) - Extended functionality
- [README_ADVANCED_SUMMARY.md](README_ADVANCED_SUMMARY.md) - Advanced features
- [README_TIME_TRACKING.md](README_TIME_TRACKING.md) - Time tracking details
