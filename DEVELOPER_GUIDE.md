
# Task Manager System - Developer Guide

## Introduction

This guide is designed for developers who want to understand, modify, or extend the Task Manager system. It covers the codebase architecture, key components, development practices, and how to add new features.

## Table of Contents

1. [Project Structure](#project-structure)
2. [Core Architecture](#core-architecture)
3. [Data Structures](#data-structures)
4. [Core Module (taskmanager.pas)](#core-module-taskmanagerpas)
5. [Working with the Code](#working-with-the-code)
6. [Adding New Features](#adding-new-features)
7. [Coding Standards](#coding-standards)
8. [Debugging Tips](#debugging-tips)

---

## Project Structure

```
solution1/
├── taskmanager.pas                    # Core module - Basic task management
├── taskmanageradvanced.pas            # Advanced features (sessions, notes, dependencies)
├── taskmanagerboards.pas              # Kanban board functionality
├── taskmanagercomments.pas            # Comments and discussions on tasks
├── taskmanagerenhanced.pas            # Enhanced features (recurring, subtasks)
├── taskmanagerext.pas                 # Extended features (analytics, batch ops)
├── taskmanagerfocus.pas               # Focus mode and distraction management
├── taskmanagergamify.pas              # Gamification features (rewards, achievements)
├── taskmanagerintelligence.pas        # AI-powered features and analytics
├── taskmanagerknowledge.pas           # Knowledge base and learning features
├── taskmanagerlifestyle.pas           # Lifestyle and wellness tracking
├── taskmanagermeetings.pas            # Meeting scheduling and management
├── taskmanagernotifications.pas       # Notification system
├── taskmanagerrecurring.pas           # Recurring task patterns
├── taskmanagerresource.pas            # Resource allocation and planning
├── taskmanagersearch.pas              # Advanced search functionality
├── taskmanagersmart.pas               # Smart recommendations
├── taskmanagerteam.pas                # Team collaboration features
├── taskmanagertemplates.pas           # Task templates and workflows
├── taskmanagertimetracking.pas        # Time tracking and analytics
├── taskmanagerwellbeing.pas           # User wellbeing and productivity
├── solution1.pas - solution22.pas     # Demo and test programs
└── bin/                               # Compiled binaries and object files
```

### File Organization Philosophy

- **Core**: `taskmanager.pas` contains fundamental functionality
- **Extensions**: Each additional module extends the core with specialized features
- **Demos**: `solution*.pas` files demonstrate features and serve as usage examples
- **Include files**: `.inc` files contain helper code for modular organization

---

## Core Architecture

### Layered Design

The Task Manager follows a **layered architecture**:

```
┌─────────────────────────────────────┐
│   Application/Feature Modules       │  Layer 3
│  (taskmanagerX.pas, solutionX.pas)  │
├─────────────────────────────────────┤
│   Extension Modules                 │  Layer 2
│  (taskmanageradvanced.pas, etc.)    │
├─────────────────────────────────────┤
│   Core Module (taskmanager.pas)     │  Layer 1
│   - TTask, TTaskManager             │
│   - Basic CRUD operations           │
│   - Filtering, sorting, searching   │
└─────────────────────────────────────┘
```

### Key Design Principles

1. **Object-Oriented Programming**: Uses Pascal classes (`TTaskManager`) for abstraction
2. **Extensibility**: Core functionality in `taskmanager.pas`, extensions build upon it
3. **Type Safety**: Uses enumerations and records for data integrity
4. **Efficiency**: Implements sorting algorithms (QuickSort) for performance
5. **Modularity**: Each feature lives in its own unit for maintainability

---

## Data Structures

### TTaskStatus Enumeration

Represents the lifecycle state of a task:

```pascal
TTaskStatus = (
  tsNotStarted,    // Task has not begun
  tsInProgress,    // Task is currently being worked on
  tsCompleted,     // Task has been finished
  tsCancelled,     // Task was cancelled
  tsOnHold         // Task is temporarily paused
);
```

### TTaskPriority Enumeration

Defines the importance level of a task:

```pascal
TTaskPriority = (
  tpLow,           // Low priority
  tpMedium,        // Medium priority
  tpHigh,          // High priority
  tpCritical       // Critical - needs immediate attention
);
```

### TSortCriteria Enumeration

Specifies how tasks can be sorted:

```pascal
TSortCriteria = (
  scTitle,         // Sort alphabetically by title
  scPriority,      // Sort by priority level
  scDueDate,       // Sort by due date
  scCreatedDate,   // Sort by creation date
  scStatus,        // Sort by task status
  scCategory       // Sort by category
);
```

### TTask Record

The fundamental data structure representing a single task:

```pascal
TTask = record
  ID: Integer;                    // Unique identifier
  Title: string;                  // Task name
  Description: string;            // Detailed description
  Status: TTaskStatus;            // Current status
  Priority: TTaskPriority;        // Priority level
  Category: string;               // Organizational category
  CreatedDate: TDateTime;         // When task was created
  DueDate: TDateTime;             // When task is due
  CompletedDate: TDateTime;       // When task was completed
  EstimatedHours: Double;         // Estimated hours to complete
  ActualHours: Double;            // Actual hours spent
  Tags: array of string;          // Multiple tags for organization
end;
```

**Usage Tips:**
- `ID` is auto-generated by `TTaskManager`
- `Tags` is a dynamic array - can have any number of tags
- `EstimatedHours` helps with project planning
- `ActualHours` enables time tracking

### TTaskArray Type

Dynamic array of tasks used for returning multiple tasks:

```pascal
TTaskArray = array of TTask;
```

---

## Core Module (taskmanager.pas)

### TTaskManager Class

The main class managing all task operations. Here's how to use it:

#### Creating an Instance

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

#### Core Operations

##### Adding Tasks

```pascal
// Simple add (without category/estimated hours)
TaskID := Manager.AddTask(
  'Implement feature X',
  'Develop the new search functionality',
  tpHigh,
  EncodeDate(2024, 12, 31)
);

// Full add (with category and estimated hours)
TaskID := Manager.AddTask(
  'Code review',
  'Review pull requests from team',
  'Backend',
  tpMedium,
  EncodeDate(2024, 12, 25),
  2.5  // estimated hours
);
```

##### Retrieving Tasks

```pascal
// Get all tasks
AllTasks := Manager.GetAllTasks;

// Get specific task by ID
TaskIndex := Manager.GetTaskByID(TaskID);
if TaskIndex >= 0 then
  WriteLn('Found: ' + Manager.GetAllTasks[TaskIndex].Title);
```

##### Updating Tasks

```pascal
// Update individual properties
Manager.UpdateTaskTitle(TaskID, 'New title');
Manager.UpdateTaskStatus(TaskID, tsInProgress);
Manager.UpdateTaskPriority(TaskID, tpCritical);
Manager.UpdateTaskDueDate(TaskID, EncodeDate(2024, 12, 20));
Manager.UpdateTaskCategory(TaskID, 'Frontend');
Manager.UpdateTaskEstimatedHours(TaskID, 5.0);
Manager.UpdateTaskActualHours(TaskID, 3.5);
```

##### Deleting Tasks

```pascal
if Manager.DeleteTask(TaskID) then
  WriteLn('Task deleted successfully')
else
  WriteLn('Task not found');
```

#### Filtering Tasks

```pascal
// Filter by status
CompletedTasks := Manager.FilterByStatus(tsCompleted);

// Filter by priority
CriticalTasks := Manager.FilterByPriority(tpCritical);

// Filter by date range
StartDate := EncodeDate(2024, 1, 1);
EndDate := EncodeDate(2024, 12, 31);
TasksDueSoon := Manager.FilterByDateRange(StartDate, EndDate);

// Filter by category
BackendTasks := Manager.FilterByCategory('Backend');

// Filter by tag
ImportantTasks := Manager.FilterByTag('important');
```

#### Searching Tasks

```pascal
// Search by title (case-insensitive substring match)
Results := Manager.SearchByTitle('feature');
```

#### Sorting Tasks

```pascal
// Sort ascending
SortedByDue := Manager.SortTasks(scDueDate);

// Sort descending
ReverseOrder := Manager.SortTasksDescending(scTitle);
```

#### Statistics and Reporting

```pascal
// Get counts
Total := Manager.GetTaskCount;
Completed := Manager.GetCompletedCount;
Pending := Manager.GetPendingCount;
Overdue := Manager.GetOverdueCount;

// Get rates
CompRate := Manager.GetCompletionRate;  // Returns 0.0 to 100.0
AvgTime := Manager.GetAverageCompletionTime;

// Get hours
EstTotal := Manager.GetTotalEstimatedHours;
ActTotal := Manager.GetTotalActualHours;

// Get categories
CatStats := Manager.GetTasksByCategory;

// Get completion rate as percentage
WriteLn(Format('Completion: %.1f%%', [Manager.GetCompletionRate]));
```

#### Tag Management

```pascal
// Add tag to task
Manager.AddTagToTask(TaskID, 'urgent');
Manager.AddTagToTask(TaskID, 'review');

// Remove tag from task
Manager.RemoveTagFromTask(TaskID, 'review');

// Filter by tag
TaggedTasks := Manager.FilterByTag('urgent');
```

#### Persistence

```pascal
// Save to file
if Manager.SaveToFile('tasks.dat') then
  WriteLn('Saved successfully')
else
  WriteLn('Save failed');

// Load from file
if Manager.LoadFromFile('tasks.dat') then
  WriteLn('Loaded successfully')
else
  WriteLn('Load failed');

// Export to CSV
CSVData := Manager.ExportToCSV;
SaveStringToFile(CSVData, 'tasks.csv');
```

#### Clearing All Tasks

```pascal
Manager.ClearAllTasks;  // Removes all tasks from memory
```

#### String Conversion Utilities

```pascal
// Convert enum to string
StatusStr := Manager.TaskStatusToString(tsInProgress);   // Returns 'In Progress'
PriorityStr := Manager.TaskPriorityToString(tpHigh);     // Returns 'High'

// Convert task to formatted string
TaskStr := Manager.TaskToString(MyTask);
WriteLn(TaskStr);
```

---

## Working with the Code

### Understanding the Module System

Each module in the Task Manager extends the core functionality:

**Module Pattern:**

```pascal
unit taskmanagerXXX;

interface

uses
  taskmanager;  // Uses core module

type
  TTaskManagerXXX = class(TTaskManager)  // Extends TTaskManager
  private
    // Private fields and methods
  public
    // New public methods
  end;

implementation

// Implementation of new methods

end.
```

### Adding to an Existing Module

To add functionality to `taskmanager.pas`:

1. Add new data structures at the top (types section)
2. Add method declarations in the `TTaskManager` class (interface section)
3. Implement methods in the implementation section
4. Update any relevant statistics methods
5. Test with a solution file

Example:

```pascal
// In interface section of taskmanager.pas
type
  TTaskManager = class
  public
    // New method declaration
    function GetTaskDays: Integer;  // Returns days until due
  end;

// In implementation section
function TTaskManager.GetTaskDays: Integer;
begin
  // Implementation
end;
```

### Debugging Common Issues

**Issue: Task not found**
```pascal
// Always check return value
Index := Manager.GetTaskByID(TaskID);
if Index < 0 then
  WriteLn('Task not found')
else
  WriteLn('Task found at index: ', Index);
```

**Issue: Array bounds**
```pascal
// Always check array bounds before accessing
Tasks := Manager.GetAllTasks;
if Length(Tasks) > 0 then
  WriteLn('First task: ', Tasks[0].Title)
else
  WriteLn('No tasks');
```

**Issue: Memory leaks**
```pascal
// Always use try/finally to ensure cleanup
Manager := TTaskManager.Create;
try
  // Use Manager
finally
  Manager.Free;  // This is crucial
end;
```

---

## Adding New Features

### Creating a New Module

To create a new feature module:

1. **Create a new .pas file** (e.g., `taskmanagernewfeature.pas`)
2. **Declare the unit**:

```pascal
unit taskmanagernewfeature;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, DateUtils, taskmanager;

type
  // Your new record or enumeration
  TNewFeature = record
    // Fields
  end;

  // Extend TTaskManager
  TTaskManagerNewFeature = class(TTaskManager)
  private
    // Private fields
  public
    procedure NewMethod;
    function NewFunction: string;
  end;

implementation

procedure TTaskManagerNewFeature.NewMethod;
begin
  // Implementation
end;

function TTaskManagerNewFeature.NewFunction: string;
begin
  Result := 'New functionality';
end;

end.
```

3. **Test with a solution file** (create a new `solutionN.pas` to demo)
4. **Document in markdown**

### Modifying Core Functionality

**Important**: Be cautious when modifying `taskmanager.pas` as all other modules depend on it.

**Safe approach**:

1. Add new methods rather than modifying existing ones
2. Test thoroughly with multiple solution files
3. Ensure backward compatibility
4. Document the change
5. Commit to git with detailed message

### Extension Best Practices

1. **Don't duplicate code** - Call parent methods when possible
2. **Maintain consistent naming** - Use "Task" prefix for task-related methods
3. **Add validation** - Check for invalid inputs
4. **Handle edge cases** - What if array is empty? What if ID doesn't exist?
5. **Update documentation** - Create/update .md files
6. **Write usage examples** - Add a demo in a solution file

---

## Coding Standards

### Naming Conventions

**Classes:**
```pascal
TTaskManager        // Prefix with 'T'
TMyNewClass         // PascalCase
```

**Methods and Procedures:**
```pascal
GetTaskCount        // Verb-Noun format
FilterByPriority    // Descriptive names
AddTask             // Start with action verb
```

**Variables:**
```pascal
var
  TaskID: Integer;          // Descriptive names
  Manager: TTaskManager;    // Noun format
  i: Integer;               // Loop counters ok with single letter
```

**Constants:**
```pascal
const
  MaxTasks = 10000;
  DefaultCategory = 'General';
```

### Code Style

**Indentation:**
```pascal
if Condition then
begin
  // Two space indent
  DoSomething;
  if NestedCondition then
  begin
    DoMore;
  end;
end;
```

**Comments:**
```pascal
// Use single-line comments for brief explanations
procedure DoWork;
begin
  { Use brace comments for block explanations
    spanning multiple lines }
  DoSomething;
end;
```

**Error Handling:**
```pascal
function SafeOperation: Boolean;
begin
  try
    // Risky code
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      Result := False;
    end;
  end;
end;
```

### Type Declarations

```pascal
type
  // Records first
  TMyRecord = record
    Field1: Integer;
    Field2: string;
  end;

  // Then arrays
  TMyArray = array of TMyRecord;

  // Then classes
  TMyClass = class
  private
    FField: Integer;
  public
    property Field: Integer read FField;
  end;
```

---

## Debugging Tips

### Using WriteLn for Debugging

```pascal
// Print variable values
WriteLn('Debug: TaskID = ', TaskID);
WriteLn('Debug: Task count = ', Manager.GetTaskCount);

// Print arrays
Tasks := Manager.GetAllTasks;
WriteLn('Total tasks: ', Length(Tasks));
for i := 0 to Length(Tasks) - 1 do
  WriteLn('  Task ', i, ': ', Tasks[i].Title);
```

### Checking Task Properties

```pascal
Task := GetAllTasks[Index];
WriteLn('Title: ', Task.Title);
WriteLn('Status: ', TaskStatusToString(Task.Status));
WriteLn('Priority: ', TaskPriorityToString(Task.Priority));
WriteLn('Due Date: ', DateToStr(Task.DueDate));
WriteLn('Tags: ', Length(Task.Tags), ' tags');
```

### Testing Edge Cases

```pascal
// Test with empty task manager
Manager := TTaskManager.Create;
if Manager.GetTaskCount = 0 then
  WriteLn('✓ Empty manager works');

// Test with single task
TaskID := Manager.AddTask('Test', 'Desc', tpHigh, Now + 1);
if Manager.GetTaskCount = 1 then
  WriteLn('✓ Single task works');

// Test deletion
if Manager.DeleteTask(TaskID) then
  if Manager.GetTaskCount = 0 then
    WriteLn('✓ Deletion works');
```

### Performance Profiling

```pascal
var
  StartTime: TDateTime;
  Elapsed: Int64;
begin
  StartTime := Now;
  
  // Operation to test
  for i := 0 to 999 do
    Manager.AddTask('Task ' + IntToStr(i), 'Desc', tpLow, Now);
  
  Elapsed := MilliSecondsBetween(Now, StartTime);
  WriteLn('Added 1000 tasks in ', Elapsed, ' ms');
end;
```

---

## Next Steps

- Read the specific feature documentation (README_*.md) for modules you're extending
- Look at existing solution files to understand implementation patterns
- Study the extension modules to learn how to properly extend the core
- Create a small feature of your own to practice the patterns
- Join the development discussions for questions

---

## Frequently Asked Questions

**Q: How do I add a new feature module?**
A: Create a new `.pas` file, declare a class extending `TTaskManager`, implement your methods, test with a solution file, and document it.

**Q: What if I want to modify the core `taskmanager.pas`?**
A: Be very careful. Add new methods rather than modifying existing ones. Test extensively and ensure backward compatibility.

**Q: How do I ensure my changes don't break existing code?**
A: Run all solution files to ensure they still work. Add unit tests. Keep method signatures compatible.

**Q: Where do I add documentation?**
A: Add .md files in the solution1 folder. Use consistent formatting with existing docs. Include code examples.

**Q: What's the best way to handle errors?**
A: Use try/except blocks. Return Boolean from procedures that can fail. Use meaningful error messages.

---

**Last Updated**: 2024
**For questions**: Refer to the README files or examine existing modules for patterns.
