# Pascal Task Manager - Version 5

**✨ NEW in v5**: Task Scheduling Engine & Risk Analysis System! See [V5_QUICKSTART.md](V5_QUICKSTART.md) and [VERSION_5_NEW_FEATURES.md](VERSION_5_NEW_FEATURES.md)

# Pascal Task Manager

A comprehensive task management system written in Free Pascal (FPC) with support for creating, organizing, tracking, and managing task dependencies.

## Overview

This task manager provides a complete solution for task organization with 40+ features including core CRUD operations, advanced filtering and sorting, time tracking, tagging system, task dependencies, and now extended features like task notes, assignments, milestones, and reusable task templates.

## Core Features

### Task Management
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

## Advanced Features

### 1. Filtering & Retrieval
- Get tasks by status
- Get tasks by priority level
- Get tasks by category
- Find overdue tasks (incomplete tasks past their due date)

### 2. Sorting
- Sort tasks by due date (earliest first)
- Sort tasks by priority (highest first)
- Sort tasks by status

### 3. Search Capabilities
- Search tasks by name (case-insensitive)
- Search tasks by description (case-insensitive)
- General search across both name and description

### 4. Tags/Labels System
- Add multiple tags to tasks (up to 10 tags per task)
- Remove tags from tasks
- Check if a task has a specific tag
- Get all tags for a task
- Filter tasks by tag
- Get all tags in use

### 5. Time Tracking
- Set estimated hours for tasks
- Track actual hours worked
- Calculate time overruns
- Identify tasks exceeding time estimates
- Analyze time spent vs. estimated

### 6. Task Dependencies
- Add dependencies between tasks
- Remove dependencies
- Check if a task can be completed (dependencies met)
- Get all tasks depending on a specific task
- Validate dependency chains

### 7. Task Notes/Comments (NEW)
- Add notes to tasks with timestamps
- Support for important flag on notes
- Up to 20 notes per task
- Remove notes as needed
- Get all notes for a task
- Get all tasks with important notes

### 8. Task Assignment (NEW)
- Assign tasks to team members
- Track task ownership
- Get all tasks assigned to a specific person
- Clear or change assignments

### 9. Milestones (NEW)
- Create project milestones
- Assign tasks to milestones
- Track milestone progress with statistics
- View all tasks in a milestone
- Set milestone target dates
- Monitor milestone status

### 10. Task Templates (NEW)
- Create reusable task templates
- Define template priority and estimated hours
- Pre-define tags for templates
- Create tasks from templates with automatic field population
- Manage template library
- Delete unused templates

## Statistics & Reporting
- Get comprehensive task statistics
- Count tasks by status
- Count tasks by priority
- Count overdue tasks
- Track completion rates
- Task count by milestone
- Task count by assignee

## Error Handling
- Comprehensive error tracking with `getLastError()` method
- Error clearing capability
- Validation of inputs before operations
- Clear error messages for debugging

## Architecture

The task manager is built with a modular architecture:

- **TaskTypes.pas**: Core data type definitions (TTask, TTaskStatus, TTaskPriority, etc.)
- **TaskManager.pas**: Base task manager class with 30+ core methods
- **TaskManagerExtended.pas**: Extended functionality class for notes, assignments, milestones, and templates
- **solution1.pas**: Main program with comprehensive self-test suite

### Type System

All dynamic array results are defined as types for proper Pascal type checking:
- `TTaskArray`: Array of TTask records
- `TTagArray`: Array of strings (tags)
- `TNoteArray`: Array of TTaskNote records
- `TTemplateArray`: Array of TTaskTemplate records
- `TMilestoneArray`: Array of TMilestone records

## Self-Test

The program includes a comprehensive self-test that validates all features:
- Task CRUD operations
- Filtering and sorting
- Search functionality
- Tag management
- Time tracking
- Dependencies validation
- Notes management
- Task assignments
- Milestone creation and tracking
- Template creation and usage
- Statistics calculation

## Compilation

Compile with:
```bash
fpc solution1/solution1.pas -obin/task_manager -O1 -Mobjfpc
```

## Running

Execute the compiled binary:
```bash
./bin/task_manager
```

The program will run the comprehensive self-test suite without requiring any user input.

## Design Notes

- **No User Input**: The code is designed as a reusable library suitable for GUI applications
- **Dynamic Arrays**: Uses Pascal dynamic arrays for flexible data management
- **Error Handling**: All operations return boolean success indicators with error messages
- **Type Safety**: Proper use of record types and enumerations for type safety
- **Inheritance**: TaskManagerExtended inherits from TaskManager, allowing backward compatibility
- **Protected Members**: Parent class members are protected to allow extension
