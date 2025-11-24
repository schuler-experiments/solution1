
# Free Pascal Task Manager

A lightweight, object-oriented task management system written in Free Pascal (FPC). This project demonstrates advanced Pascal programming concepts including dynamic arrays, object-oriented design, memory management, and modular architecture.

## Project Overview

The Task Manager is a comprehensive, reusable library designed to be integrated with graphical user interfaces or command-line applications. It provides a complete API for managing tasks with features like priority levels, status tracking, date management, advanced filtering, notes, history tracking, recurring tasks, and time tracking.

## Architecture

The project is organized into modular units with clear separation of concerns. Each module stays under 500 lines for maintainability:

### **TaskTypes.pas** (201 lines)
Core data structures and type definitions:
- `TTaskStatus`: Enum for task states (NotStarted, InProgress, Completed, OnHold)
- `TTaskPriority`: Enum for priority levels (Low, Medium, High)
- `TTask`: Record containing task data (id, title, description, status, priority, dates, category, tags, assignee, estimated/actual hours)
- `TTaskArray`: Dynamic array type for multiple tasks
- Helper functions for enum-to-string conversions and task creation
- Tag management utilities

### **TaskManager.pas** (344 lines)
Core task management class `TTaskManagerClass`:

#### Core CRUD Operations
- `AddTask()`: Create new tasks with automatic ID generation
- `GetTask()`: Retrieve task by ID
- `UpdateTask()`: Modify existing task
- `DeleteTask()`: Remove tasks from collection

#### Query Operations
- `GetTaskCount()`: Get total number of tasks
- `GetAllTasks()`: Retrieve all tasks
- `GetTasksByStatus()`: Filter by task status
- `GetTasksByPriority()`: Filter by priority level
- `GetOverdueTasks()`: Find tasks past their due date

#### Status Management
- `SetTaskStatus()`: Change task status
- `CompleteTask()`: Mark task as completed with timestamp

#### Sorting Operations
- `SortByPriority()`: Order tasks by priority (High → Low)
- `SortByDueDate()`: Order tasks by due date (earliest first)
- `SortByTitle()`: Alphabetical ordering

### **TaskPersistence.pas** (284 lines)
File I/O and data persistence class `TTaskPersistenceClass`:
- `SaveAsCSV()`: Export tasks to CSV format
- `LoadFromCSV()`: Import tasks from CSV file
- `SaveAsText()`: Export tasks to human-readable text format
- `LoadFromText()`: Import tasks from text file
- `CreateBackup()`: Create backup of task data
- `FileExists()`: Check if data file exists
- `DeleteFile()`: Remove data file
- DateTime parsing and formatting utilities

### **TaskQuery.pas** (422 lines)
Advanced query and analytics class `TTaskQueryClass`:
- `FilterByKeyword()`: Search tasks by keyword
- `FilterByDateRange()`: Find tasks within date range
- `GetTasksDueToday()`: Query tasks due today
- `GetTasksDueTomorrow()`: Query tasks due tomorrow
- `GetTasksDueThisWeek()`: Query tasks due this week
- `GetTasksDueThisMonth()`: Query tasks due this month
- `GetStats()`: Get comprehensive task statistics
- `GetCompletionRate()`: Calculate percentage of completed tasks
- `GetAverageTasksPerDay()`: Analyze task creation rate
- Priority breakdown functions
- Task creation and completion date tracking

### **TaskNotes.pas** (326 lines)
Task notes and history tracking:

#### TTaskNotesManagerClass
- `AddNote()`: Add notes/comments to tasks
- `GetNotesForTask()`: Retrieve all notes for a task
- `GetNoteCount()`: Count notes on a task
- `DeleteNote()`: Remove a specific note
- `UpdateNote()`: Modify note text
- Each note includes: id, taskId, text, author, timestamp

#### TTaskHistoryTrackerClass
- `RecordStatusChange()`: Log task status changes with audit trail
- `GetTaskHistory()`: Retrieve all changes for a task
- `GetChangeCount()`: Count status changes
- `GetLastStatusChange()`: Get most recent status change
- `ClearHistory()`: Remove history for a task
- Each entry includes: id, taskId, old status, new status, timestamp, changed by

### **TaskScheduling.pas** (421 lines)
Recurring tasks and time tracking:

#### TRecurringTaskManagerClass
- `AddRecurringTask()`: Create recurring task with pattern
- `GetRecurringTask()`: Get recurrence details
- `IsRecurring()`: Check if task is recurring
- `UpdateNextOccurrence()`: Calculate next occurrence date
- `GetDueRecurringTasks()`: Find recurring tasks due
- `DeleteRecurringTask()`: Remove recurrence
- Supports patterns: Daily, Weekly, BiWeekly, Monthly, Quarterly, Yearly

#### TTimeTrackingManagerClass
- `StartSession()`: Begin time tracking session
- `EndSession()`: End session and calculate hours
- `LogTime()`: Directly log hours for a task
- `GetTotalTimeForTask()`: Sum all hours logged
- `GetSessionsForTask()`: Retrieve all time sessions
- `CalculateTimeRemainingForTask()`: Compare actual vs estimated hours
- `GetAverageTimePerTask()`: Calculate average hours per task

### **solution1.pas** (57 lines)
Main program entry point that:
- Initializes all manager instances
- Runs comprehensive self-tests for all modules
- Demonstrates integration of all features
- Verifies system status

## Features

### Task Properties
- **ID**: Unique identifier (auto-generated)
- **Title**: Task name/summary
- **Description**: Detailed task information
- **Status**: Current state (NotStarted, InProgress, Completed, OnHold)
- **Priority**: Importance level (Low, Medium, High)
- **Category**: Task grouping/project association
- **Tags**: Multiple keywords for organization
- **Assignee**: Person responsible for task
- **Due Date**: Target completion date/time
- **Created Date**: Timestamp when task was created
- **Completed Date**: Timestamp when task was marked complete
- **Estimated Hours**: Initial time estimate
- **Actual Hours**: Actual time spent (from time tracking)

### Key Capabilities
- ✓ Dynamic task collection (grows/shrinks as needed)
- ✓ Automatic ID assignment
- ✓ Multiple filtering options (status, priority, date, keyword)
- ✓ Flexible sorting (priority, date, title)
- ✓ Date-aware overdue detection
- ✓ Completion tracking with timestamps
- ✓ Task notes and comments system
- ✓ Full audit trail of status changes
- ✓ Recurring task support with multiple patterns
- ✓ Time tracking with sessions and logging
- ✓ CSV and text file persistence
- ✓ Backup and restore capabilities
- ✓ Advanced statistics and analytics
- ✓ Memory-efficient implementation
- ✓ No external dependencies (uses only SysUtils and DateUtils)

## Compilation

### Prerequisites
- Free Pascal Compiler (FPC) version 3.2+
- Linux/Unix environment (or Windows with appropriate path adjustments)

### Build Instructions

From the project root directory:

```bash
cd solution1
fpc solution1.pas -obin/task_manager -O1 -Mobjfpc
```

Output: `solution1/bin/task_manager` (executable)

### Compilation Flags Explained
- `-obin/task_manager`: Output executable to bin/ folder
- `-O1`: Level 1 optimization
- `-Mobjfpc`: Object Pascal mode for advanced features (dynamic arrays, classes, etc.)

## Running the Application

```bash
cd solution1
./bin/task_manager
```

This will execute the comprehensive self-test demonstration, showing:
- Task creation and management
- Status updates and tracking
- Priority-based filtering
- Sorting capabilities
- Task deletion
- File persistence (CSV and text formats)
- Advanced query and filtering
- Task notes and commenting
- Status change history
- Recurring task scheduling
- Time tracking and analytics

## Usage Example (In Pascal Code)

```pascal
var
  mgr: TTaskManagerClass;
  notesMgr: TTaskNotesManagerClass;
  timeMgr: TTimeTrackingManagerClass;
  taskId, noteId: integer;
  tomorrow: TDateTime;
begin
  { Create manager instances }
  mgr := TTaskManagerClass.Create();
  notesMgr := TTaskNotesManagerClass.Create();
  timeMgr := TTimeTrackingManagerClass.Create();
  
  try
    { Add a task }
    tomorrow := IncDay(Now(), 1);
    taskId := mgr.AddTask(
      'Complete project documentation',
      'Write comprehensive API docs',
      tpHigh,
      tomorrow
    );
    
    { Add notes to task }
    noteId := notesMgr.AddNote(taskId, 'Start with architecture overview', 'DevLead');
    notesMgr.AddNote(taskId, 'Include code examples', 'DevLead');
    
    { Start time tracking }
    timeMgr.LogTime(taskId, 2.5, 'Initial research and planning');
    
    { Query tasks }
    var highPriority := mgr.GetTasksByPriority(tpHigh);
    
    { Update task status }
    mgr.SetTaskStatus(taskId, tsInProgress);
    
    { Log more time }
    timeMgr.LogTime(taskId, 3.0, 'Writing documentation');
    
    { Mark as complete }
    mgr.CompleteTask(taskId);
    
    { Check time spent vs estimated }
    var timeSpent := timeMgr.GetTotalTimeForTask(taskId);
    
  finally
    mgr.Free();
    notesMgr.Free();
    timeMgr.Free();
  end;
end;
```

## Code Quality

### Memory Management
- Proper use of dynamic arrays with `SetLength()`
- Destructors ensure cleanup of all allocated memory
- No memory leaks or dangling pointers
- Reference parameters used to avoid unnecessary copying

### Code Organization
- Each file kept under 500 lines for maintainability
- Clear separation of concerns between modules
- Comprehensive error handling
- Consistent naming conventions (Hungarian notation for types)

### Testing
- Self-test method in each module with static test data
- Demonstrates all major features
- No user input required (suitable for GUI integration)
- Safe to call multiple times
- Comprehensive test output showing all functionality

## Design Patterns Used

1. **Object-Oriented Design**: Class-based architecture with encapsulation
2. **Factory Pattern**: `CreateTask()` helper function
3. **Repository Pattern**: `TTaskManagerClass` manages task collection
4. **Filter Pattern**: Multiple filter methods for querying
5. **Strategy Pattern**: Different sorting strategies (priority, date, title)
6. **Observer Pattern**: History tracking for status changes
7. **Manager Pattern**: Separate manager classes for different concerns

## Module Dependencies

```
solution1.pas
├── TaskTypes (core types)
├── TaskManager (CRUD operations)
├── TaskPersistence (file I/O)
├── TaskQuery (advanced queries)
├── TaskNotes (notes and history)
└── TaskScheduling (recurring tasks and time tracking)
```

## Development Notes

### Modular Design Benefits
- Code reusability across projects
- Easy to test individual components
- Simple to extend with new features
- Clear API for GUI applications
- Lightweight deployment
- Each module can be used independently

### Performance Characteristics
- O(n) for most operations (linear search/sort)
- Can be optimized with hash tables or trees for large task lists
- Current design suitable for 1000+ tasks
- No external dependencies = fast startup
- Efficient dynamic array management

## Compatibility

- **Language**: Free Pascal (FPC)
- **Mode**: Object Pascal (-Mobjfpc)
- **OS**: Linux/Unix (primary), Windows (with path adjustments)
- **Architecture**: 64-bit x86_64
- **FPC Version**: 3.2+

## Feature Summary

### Core Features
- ✓ Full CRUD operations
- ✓ Multiple task properties (status, priority, category, tags, assignee)
- ✓ Dynamic arrays for memory efficiency
- ✓ Multiple filtering and sorting options

### Advanced Features
- ✓ File persistence (CSV and text formats)
- ✓ Backup and restore
- ✓ Task notes and comments
- ✓ Complete audit trail of changes
- ✓ Recurring task patterns
- ✓ Time tracking with sessions
- ✓ Advanced analytics and reporting

## License

This project is provided as-is for educational and development purposes.

## Author Notes

This task manager demonstrates professional Pascal programming practices:
- Proper memory management with dynamic arrays
- Object-oriented design with classes and encapsulation
- Comprehensive API with multiple access patterns
- Self-contained with no external dependencies
- Production-ready code suitable for integration with UI frameworks
- Complete feature set for task management needs

The modular design allows each file to remain under 500 lines while providing a comprehensive, feature-rich task management system suitable for various applications and frameworks.
