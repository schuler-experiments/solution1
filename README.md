
# Pascal Task Manager

A comprehensive task management system written in Free Pascal (FPC) with support for creating, organizing, and tracking tasks.

## Features

### Core Functionality
- **Create Tasks**: Add new tasks with name, description, category, priority, and due date
- **Retrieve Tasks**: Get individual tasks by ID or retrieve all tasks
- **Update Tasks**: Modify task details including status and priority
- **Delete Tasks**: Remove tasks from the system
- **Task Count**: Query the total number of tasks

### Task Organization
- **Status Management**: Track tasks through states (New, In Progress, Blocked, Completed, Cancelled)
- **Priority Levels**: Assign priorities (Low, Medium, High, Critical) to tasks
- **Categories**: Organize tasks by categories/projects
- **Due Dates**: Set and track due dates for tasks

### Advanced Features

#### Filtering
- Get tasks by status
- Get tasks by priority level
- Get tasks by category
- Find overdue tasks (incomplete tasks past their due date)

#### Sorting
- Sort tasks by due date (earliest first)
- Sort tasks by priority (highest first)
- Sort tasks by status

#### Search
- Search tasks by name (case-insensitive)
- Search tasks by description (case-insensitive)
- General search across both name and description

#### Statistics
- Total task count
- Count of tasks in each status
- Count of high and critical priority tasks

#### Persistence
- Save all tasks to a binary file (default: `tasks.dat`)
- Load tasks from a binary file
- Supports custom filenames for save/load operations

## Architecture

### Units

#### TaskTypes.pas
Defines all data types and constants:
- `TTask`: Main task record structure
- `TTaskStatus`: Enumeration for task states
- `TTaskPriority`: Enumeration for priority levels
- `TTaskArray`: Dynamic array type for tasks
- `TTaskStats`: Statistics record
- Constants for default filenames and string lengths

#### TaskManager.pas
Implements the task management class `TTaskManager` with:
- CRUD operations
- Filtering and searching
- Sorting capabilities
- File persistence
- Error handling with `getLastError()` and `clearError()`

#### solution1.pas
Main program demonstrating all features through comprehensive self-tests:
- 17 test cases covering all functionality
- Creates sample tasks
- Tests all CRUD operations
- Demonstrates filtering, sorting, and searching
- Tests file persistence

## Compilation

To compile the task manager:

```bash
fpc solution1.pas -obin/task_manager -O1 -Mobjfpc
```

## Running

Execute the compiled binary:

```bash
./bin/task_manager
```

This runs the comprehensive self-test suite demonstrating all features.

## Data Structures

### TTask Record
```
id: integer           - Unique task identifier
name: string          - Task name (max 255 characters)
description: string   - Task description (max 255 characters)
status: TTaskStatus   - Current task status
priority: TTaskPriority - Task priority level
dueDate: TDateTime    - Due date for the task
createdDate: TDateTime - When the task was created
completedDate: TDateTime - When the task was completed (0 if not completed)
category: string      - Task category (max 100 characters)
```

### TTaskStatus Enumeration
- `tsNew`: New task, not yet started
- `tsInProgress`: Currently being worked on
- `tsBlocked`: Blocked by dependencies or issues
- `tsCompleted`: Task is complete
- `tsCancelled`: Task has been cancelled

### TTaskPriority Enumeration
- `tpLow`: Low priority
- `tpMedium`: Medium priority
- `tpHigh`: High priority
- `tpCritical`: Critical priority

## Usage Example

```pascal
var
  manager: TTaskManager;
  taskId: integer;
  task: TTask;
  tasks: TTaskArray;
  stats: TTaskStats;

begin
  manager := TTaskManager.Create;
  try
    // Add a task
    taskId := manager.addTask('Design API', 'Create REST endpoints', 
                               'Development', tpHigh, now + 7);
    
    // Get a task
    if manager.getTask(taskId, task) then
      WriteLn('Task: ', task.name);
    
    // Update status
    manager.setTaskStatus(taskId, tsInProgress);
    
    // Search for tasks
    tasks := manager.searchTasksByName('API');
    WriteLn('Found ', length(tasks), ' tasks');
    
    // Get statistics
    stats := manager.getStatistics;
    WriteLn('Total tasks: ', stats.totalTasks);
    
    // Save to file
    manager.saveTasks('my_tasks.dat');
  finally
    manager.Free;
  end;
end.
```

## Error Handling

All operations support error handling:

```pascal
if manager.addTask(...) = -1 then
  WriteLn('Error: ', manager.getLastError);
```

## Testing

The self-test suite in `solution1.pas` validates:
1. Task creation with various parameters
2. Task retrieval by ID
3. Task status updates
4. Task completion
5. Statistics calculations
6. Filtering by status, priority, and category
7. Priority changes
8. Task deletion
9. Sorting by due date and priority
10. Search by name, description, and general search
11. File persistence operations

All 17 tests pass successfully, verifying correct functionality.

## Implementation Notes

- Dynamic arrays are used instead of fixed-size arrays for flexibility
- All Pascal reserved words are in lowercase
- No user input (ReadLn) in the core code - designed for GUI integration
- Comprehensive error handling with meaningful error messages
- Memory-efficient implementation with proper cleanup
- Binary file format for efficient storage

## Future Enhancements

Potential features for future versions:
- Task subtasks/hierarchies
- Task dependencies and blocking relationships
- Time tracking and duration estimation
- Recurring tasks
- Task tags/labels for flexible organization
- Task comments/notes history
- Batch operations
- Import/export to other formats (CSV, JSON)
- Advanced filtering and sorting options
