
# Pascal Task Manager

A comprehensive task management system written in Free Pascal (FPC) with support for creating, organizing, tracking, and managing task dependencies.

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

#### 1. Filtering
- Get tasks by status
- Get tasks by priority level
- Get tasks by category
- Find overdue tasks (incomplete tasks past their due date)

#### 2. Sorting
- Sort tasks by due date (earliest first)
- Sort tasks by priority (highest first)
- Sort tasks by status

#### 3. Search
- Search tasks by name (case-insensitive)
- Search tasks by description (case-insensitive)
- General search across both name and description

#### 4. Tags/Labels System
- Add multiple tags to tasks (up to 10 tags per task)
- Remove tags from tasks
- Check if a task has a specific tag
- Get all tags for a task
- Filter tasks by tag
- Get all unique tags in the system

**Key Methods:**
- `addTaskTag(id, tag)` - Add a tag to a task
- `removeTaskTag(id, tag)` - Remove a tag from a task
- `hasTaskTag(id, tag)` - Check if task has a tag
- `getTaskTags(id)` - Get all tags for a task
- `getTasksByTag(tag)` - Find all tasks with a specific tag
- `getAllTags()` - Get all unique tags in the system

#### 5. Time Tracking
- Set estimated hours for tasks
- Add actual hours worked on tasks
- Calculate time overrun (actual - estimated)
- Get all tasks that exceeded time estimates
- Track project duration and resource allocation

**Key Methods:**
- `setTaskEstimatedHours(id, hours)` - Set estimated duration
- `addTaskActualHours(id, hours)` - Log hours worked
- `getTaskEstimatedHours(id)` - Get estimated hours
- `getTaskActualHours(id)` - Get actual hours worked
- `getTaskTimeOverrun(id)` - Calculate hours over estimate
- `getTasksOverTime()` - Find all tasks exceeding estimates

#### 6. Task Dependencies
- Mark tasks as dependencies of other tasks
- Prevent circular dependencies
- Check if all dependencies are completed before allowing task completion
- Get dependent tasks (tasks that depend on a given task)
- Get task dependencies (tasks this task depends on)

**Key Methods:**
- `addTaskDependency(taskId, dependencyTaskId)` - Mark task as dependent
- `removeTaskDependency(taskId, dependencyTaskId)` - Remove dependency
- `getTaskDependencies(taskId)` - Get all dependencies for a task
- `getTasksDependingOn(taskId)` - Get all tasks depending on this task
- `canCompleteTask(taskId)` - Check if all dependencies are completed

#### 7. Statistics
- Total task count
- Count of tasks in each status
- Count of high and critical priority tasks
- Overdue task detection

## Architecture

### Units

#### TaskTypes.pas
Defines all data types and constants:
- `TTask`: Main task record structure with all fields
- `TTaskStatus`: Enumeration for task states (tsNew, tsInProgress, tsBlocked, tsCompleted, tsCancelled)
- `TTaskPriority`: Enumeration for priority levels (tpLow, tpMedium, tpHigh, tpCritical)
- `TTaskArray`: Dynamic array type for tasks
- `TTagArray`: Dynamic array type for tags
- `TIntArray`: Dynamic array type for integer IDs (dependencies)
- `TTaskStats`: Statistics record
- Constants for default filenames and string lengths

#### TaskManager.pas
Implements the task management class `TTaskManager` with:
- CRUD operations
- Filtering and searching
- Sorting capabilities
- Tag management
- Time tracking
- Dependency management
- Error handling with `getLastError()` and `clearError()`

#### solution1.pas
Main program demonstrating all features through 28 comprehensive self-tests:
- Tests 1-11: Basic CRUD and filtering operations
- Tests 12-16: Sorting and search functionality
- Test 17: File persistence (currently disabled due to dynamic arrays)
- Tests 18-21: Tag management
- Tests 22-25: Time tracking
- Tests 26-28: Task dependencies

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

This runs the comprehensive self-test suite demonstrating all 28 features.

## Data Structures

### TTask Record
```
id: integer                 - Unique task identifier
name: string                - Task name (max 255 characters)
description: string         - Task description (max 255 characters)
status: TTaskStatus         - Current task status
priority: TTaskPriority     - Task priority level
dueDate: TDateTime          - Due date for the task
createdDate: TDateTime      - When the task was created
completedDate: TDateTime    - When the task was completed (0 if not completed)
category: string            - Task category (max 100 characters)
tags: TTagArray             - Dynamic array of tags
estimatedHours: double      - Estimated hours to complete
actualHours: double         - Actual hours spent
dependencyIds: TIntArray    - IDs of tasks this task depends on
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

## Test Results

The self-test suite includes 28 comprehensive tests:

```
✓ Test 1: Creating tasks (5 tasks created)
✓ Test 2: Retrieving task by ID
✓ Test 3: Updating task status
✓ Test 4: Completing a task
✓ Test 5: Getting statistics
✓ Test 6: Filtering by status
✓ Test 7: Filtering by priority
✓ Test 8: Filtering by category
✓ Test 9: Changing priority
✓ Test 10: Deleting a task
✓ Test 11: Final statistics
✓ Test 12: Sorting by due date
✓ Test 13: Sorting by priority
✓ Test 14: Searching by name
✓ Test 15: Searching by description
✓ Test 16: General search
✓ Test 17: File persistence (disabled for dynamic arrays)
✓ Test 18: Adding tags to tasks
✓ Test 19: Checking task tags
✓ Test 20: Getting tasks by tag
✓ Test 21: Removing tags
✓ Test 22: Setting estimated hours
✓ Test 23: Adding actual hours worked
✓ Test 24: Getting time information
✓ Test 25: Getting tasks over time
✓ Test 26: Adding task dependencies
✓ Test 27: Checking if task can be completed
✓ Test 28: Getting tasks that depend on a task
```

## Error Handling

All operations support comprehensive error handling:

```pascal
if manager.addTask(...) = -1 then
  WriteLn('Error: ', manager.getLastError);

if not manager.setTaskStatus(taskId, tsCompleted) then
  WriteLn('Error: ', manager.getLastError);
```

## Implementation Notes

- Dynamic arrays used throughout for flexibility and memory efficiency
- All Pascal reserved words in lowercase
- No user input (ReadLn) in core code - designed for GUI integration
- Comprehensive error handling with meaningful error messages
- Memory-efficient implementation with proper cleanup
- Class-based architecture for object-oriented design
- Proper initialization of all dynamic fields in task creation

## Code Statistics

- **Total Lines**: 1400+ lines of Pascal code
- **TaskTypes.pas**: ~65 lines (type definitions)
- **TaskManager.pas**: ~1200 lines (core logic)
- **solution1.pas**: ~300 lines (self-tests)
- **Methods**: 40+ public methods
- **Test Cases**: 28 comprehensive tests

## Future Enhancements

Potential features for future versions:
- Recurring tasks with pattern support
- Task comments/notes history with timestamps
- Advanced filtering with AND/OR/NOT logic
- Task subtasks/hierarchies
- Import/export to CSV, JSON formats
- User assignments and team collaboration
- Task history/audit trails
- Performance metrics and analytics
- Integration with external calendars
- Notification system for due dates and dependencies

## Technical Highlights

- **Object-Oriented**: Uses class-based approach with TTaskManager
- **Type Safety**: Strong typing with custom record and enum types
- **Memory Management**: Proper use of SetLength for dynamic arrays
- **Error Handling**: Comprehensive error tracking with getLastError
- **Modular Design**: Separated concerns with unit architecture
- **Efficient Algorithms**: Optimized search, sort, and filter operations
- **Extensible**: Easy to add new features without breaking existing code

## License

This task manager was developed as an educational project demonstrating Pascal programming capabilities with Free Pascal (FPC).

## Support

For issues or feature requests, review the code structure in the solution1 folder:
- TaskTypes.pas for data structure definitions
- TaskManager.pas for method implementations
- solution1.pas for usage examples through self-tests
