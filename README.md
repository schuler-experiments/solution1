
# Free Pascal Task Manager

A lightweight, object-oriented task management system written in Free Pascal (FPC). This project demonstrates advanced Pascal programming concepts including dynamic arrays, object-oriented design, and memory management.

## Project Overview

The Task Manager is a reusable library designed to be integrated with graphical user interfaces or command-line applications. It provides a comprehensive API for managing tasks with features like priority levels, status tracking, date management, and advanced filtering.

## Architecture

The project is organized into modular units with clear separation of concerns:

### **TaskTypes.pas** (99 lines)
Defines the core data structures and type definitions:
- `TTaskStatus`: Enum for task states (NotStarted, InProgress, Completed, OnHold)
- `TTaskPriority`: Enum for priority levels (Low, Medium, High)
- `TTask`: Record containing task data (id, title, description, status, priority, dates)
- `TTaskArray`: Dynamic array type for multiple tasks
- Helper functions for enum-to-string conversions and task creation

### **TaskManager.pas** (344 lines)
Implements the core task management class `TTaskManagerClass`:

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

#### Testing
- `SelfTest()`: Comprehensive self-test with static data demonstrating all features

### **solution1.pas** (30 lines)
Main program entry point that:
- Initializes the task manager
- Runs self-tests
- Demonstrates basic usage patterns

## Features

### Task Properties
- **ID**: Unique identifier (auto-generated)
- **Title**: Task name/summary
- **Description**: Detailed task information
- **Status**: Current state (NotStarted, InProgress, Completed, OnHold)
- **Priority**: Importance level (Low, Medium, High)
- **Due Date**: Target completion date/time
- **Created Date**: Timestamp when task was created
- **Completed Date**: Timestamp when task was marked complete

### Key Capabilities
- ✓ Dynamic task collection (grows/shrinks as needed)
- ✓ Automatic ID assignment
- ✓ Multiple filtering options
- ✓ Flexible sorting
- ✓ Date-aware overdue detection
- ✓ Completion tracking with timestamps
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
./bin/task_manager
```

This will execute the self-test demonstration, showing:
- Task creation
- Status updates
- Priority-based filtering
- Sorting capabilities
- Task deletion

## Usage Example (In Pascal Code)

```pascal
var
  mgr: TTaskManagerClass;
  taskId: integer;
  tomorrow: TDateTime;
begin
  { Create manager instance }
  mgr := TTaskManagerClass.Create();
  try
    { Add a task }
    tomorrow := IncDay(Now(), 1);
    taskId := mgr.AddTask(
      'Complete project documentation',
      'Write comprehensive API docs',
      tpHigh,
      tomorrow
    );
    
    { Query tasks }
    var highPriority := mgr.GetTasksByPriority(tpHigh);
    
    { Update task status }
    mgr.SetTaskStatus(taskId, tsInProgress);
    
    { Mark as complete }
    mgr.CompleteTask(taskId);
    
    { Sort and retrieve }
    mgr.SortByPriority();
    var allTasks := mgr.GetAllTasks();
    
  finally
    mgr.Free();
  end;
end;
```

## Code Quality

### Memory Management
- Proper use of dynamic arrays with `SetLength()`
- Destructor ensures cleanup of all allocated memory
- No memory leaks or dangling pointers

### Code Organization
- Each file kept under 500 lines for maintainability
- Clear separation of concerns between modules
- Comprehensive error handling
- Consistent naming conventions (Hungarian notation for types)

### Testing
- Self-test method with static test data
- Demonstrates all major features
- No user input required (suitable for GUI integration)
- Safe to call multiple times

## Design Patterns Used

1. **Object-Oriented Design**: Class-based architecture with encapsulation
2. **Factory Pattern**: `CreateTask()` helper function
3. **Repository Pattern**: `TTaskManagerClass` manages task collection
4. **Filter Pattern**: Multiple filter methods for querying
5. **Strategy Pattern**: Different sorting strategies (priority, date, title)

## Future Enhancement Ideas

- **Persistence Layer**: Save/load tasks to/from file or database
- **Task Dependencies**: Support task ordering and prerequisites
- **Recurring Tasks**: Support for repeating tasks
- **Task Categories**: Group tasks by project or category
- **Search Functionality**: Full-text search in titles and descriptions
- **Time Tracking**: Track time spent on tasks
- **Collaboration**: Assign tasks to team members
- **Notifications**: Alert system for overdue tasks
- **Undo/Redo**: History management for task changes
- **Import/Export**: Support for common formats (CSV, JSON, iCal)

## Development Notes

### Modular Design Benefits
- Code reusability across projects
- Easy to test individual components
- Simple to extend with new features
- Clear API for GUI applications
- Lightweight deployment

### Performance Characteristics
- O(n) for most operations (linear search/sort)
- Can be optimized with hash tables or trees for large task lists
- Current design suitable for 1000+ tasks
- No external dependencies = fast startup

## Compatibility

- **Language**: Free Pascal (FPC)
- **Mode**: Object Pascal (-Mobjfpc)
- **OS**: Linux/Unix (primary), Windows (with path adjustments)
- **Architecture**: 64-bit x86_64

## License

This project is provided as-is for educational and development purposes.

## Author Notes

This task manager demonstrates professional Pascal programming practices:
- Proper memory management with dynamic arrays
- Object-oriented design with classes and encapsulation
- Comprehensive API with multiple access patterns
- Self-contained with no external dependencies
- Production-ready code suitable for integration with UI frameworks

The modular design allows each file to remain under 500 lines while providing a complete, feature-rich task management system.
