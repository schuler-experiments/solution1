
# Task Manager - Plain Pascal Implementation

A modular, feature-rich task management system written in Plain Pascal (Free Pascal Compiler).

## Version
**2.0.0** - Enhanced with Categories, Dependencies, Time Tracking, and Advanced Exports

## Overview

This is a comprehensive task manager implementation featuring:
- **Modular Architecture**: Clean separation of concerns across multiple units
- **CRUD Operations**: Create, Read, Update, and Delete tasks
- **Task Properties**: ID, Title, Description, Priority, Status, Dates, Tags, Categories
- **Categories/Projects**: Organize tasks into categories or projects
- **Task Dependencies**: Define prerequisite tasks and dependency chains
- **Time Tracking**: Track estimated and actual hours for each task
- **Audit Trail**: Complete history of all task changes
- **Persistence**: Binary file storage with versioned format (backward compatible)
- **Search & Filter**: Multiple search criteria including category filtering
- **Export Capabilities**: JSON, HTML, Markdown, CSV, and text format exports
- **Comprehensive Reports**: HTML reports with statistics and category breakdowns
- **Statistics**: Overall and category-based task statistics
- **Self-Testing**: Built-in test suite demonstrating all functionality

## Project Structure

```
solution1/
├── src/
│   ├── TaskTypes.pas         - Core data types and constants
│   ├── TaskManager.pas        - Task management operations
│   ├── TaskStorage.pas        - File persistence layer (v2 format)
│   ├── TaskExport.pas         - Export to JSON, HTML, Markdown
│   ├── TaskHistory.pas        - Audit trail and change history
│   └── TaskManagerMain.pas    - Main program with comprehensive self-test
├── bin/
│   └── task_manager           - Compiled executable
├── tasks.dat                  - Binary task storage file (v2 format)
├── tasks_history.dat          - Task change history
├── tasks_export.json          - JSON export
├── tasks_export.html          - HTML export
├── tasks_export.md            - Markdown export
├── tasks_export.csv           - CSV export
├── tasks_report.html          - Comprehensive HTML report
├── README.md                  - This file
├── ARCHITECTURE.md            - Architecture documentation
└── CHANGELOG.md               - Version history
```

## Compilation

To compile the task manager:

```bash
fpc solution1/src/TaskManagerMain.pas -obin/task_manager -O1 -Mobjfpc -Fusolution1/src -FUsolution1/src
```

## Running

After compilation, run the executable:

```bash
./bin/task_manager
```

The program will execute a comprehensive self-test demonstrating all features.

## Features

### Task Properties

Each task includes:
- **ID**: Unique identifier (auto-incremented)
- **Title**: Short task description
- **Description**: Detailed task information
- **Category**: Project or category name for organization
- **Priority**: Low, Medium, High, or Critical
- **Status**: New, Pending, In Progress, Completed, or Cancelled
- **Created Date**: Automatically set on creation
- **Due Date**: Optional deadline
- **Completed Date**: Automatically set when completed
- **Last Modified Date**: Tracks when task was last changed
- **Tags**: Comma-separated tags for flexible organization
- **Dependencies**: IDs of prerequisite tasks that must be completed first
- **Estimated Hours**: Estimated time to complete the task
- **Actual Hours**: Actual time spent on the task
- **Active Flag**: Soft delete mechanism

### Task Operations

1. **Create Tasks**: Add new tasks with category, time estimates, and all properties
2. **Update Tasks**: Modify task properties with automatic last-modified tracking
3. **Delete Tasks**: Soft delete (mark as inactive)
4. **Set Status**: Change task status with history tracking
5. **Complete Task**: Shortcut to mark as completed with timestamp
6. **Cancel Task**: Mark task as cancelled

### Category Management

1. **Set Category**: Assign tasks to categories or projects
2. **Get by Category**: Retrieve all tasks in a specific category
3. **Category Statistics**: Get statistics broken down by category

### Dependency Management

1. **Add Dependency**: Define that a task depends on another task
2. **Remove Dependency**: Remove a dependency relationship
3. **Get Dependencies**: Get list of all prerequisite tasks
4. **Can Start Task**: Check if all dependencies are completed

### Time Tracking

1. **Set Estimated Hours**: Define expected time for task completion
2. **Set Actual Hours**: Record actual time spent
3. **Add Actual Hours**: Incrementally add time spent on task
4. **Time Statistics**: Track total estimated vs actual hours

### Query Operations

1. **Get All Tasks**: Retrieve all active tasks
2. **Get Active Tasks**: Retrieve tasks that aren't completed or cancelled
3. **Search Tasks**: Search with custom criteria (title, description, status, priority, tags, category)
4. **Get by Status**: Filter tasks by specific status
5. **Get by Priority**: Filter tasks by priority level
6. **Get Overdue**: Find tasks past their due date
7. **Get by Category**: Filter tasks by category

### History & Audit Trail

1. **Track Changes**: Automatically log all task modifications
2. **Task History**: View complete change history for any task
3. **Recent History**: Get most recent changes across all tasks
4. **Field-Level Tracking**: See what changed, when, and what the old/new values were

### Persistence

1. **Binary Storage**: Efficient binary file format for task storage (v2)
2. **Versioned Format**: Backward compatible with v1 format
3. **Load/Save**: Load tasks from file or save to file
4. **History Storage**: Separate history file for audit trail

### Export Formats

1. **JSON Export**: Modern data interchange format with proper escaping
2. **HTML Export**: Beautiful styled HTML tables with color coding
3. **Markdown Export**: Documentation-friendly markdown tables
4. **CSV Export**: Standard comma-separated values with all fields
5. **Text Export**: Human-readable formatted text
6. **HTML Reports**: Comprehensive reports with statistics and category breakdowns

### Statistics

1. **Overall Statistics**:
   - Total, Active, Completed, Cancelled tasks
   - High priority task count
   - Overdue task count
   - Total estimated and actual hours
   - Tasks with dependencies count

2. **Category Statistics**:
   - Per-category task counts
   - Active vs completed breakdown
   - Estimated and actual hours by category

## What's New in Version 2.0.0

### Major Features Added

1. **Categories/Projects**
   - Organize tasks into logical groups
   - Category-based filtering and statistics
   - Multi-project support

2. **Task Dependencies**
   - Define prerequisite tasks
   - Dependency chain tracking
   - Check if tasks can start based on dependencies

3. **Time Tracking**
   - Estimated hours for planning
   - Actual hours for tracking effort
   - Time statistics overall and by category

4. **Audit Trail**
   - Complete history of all changes
   - Field-level change tracking
   - View history by task or overall

5. **Enhanced Exports**
   - JSON export for data interchange
   - Beautiful HTML reports with styling
   - Markdown export for documentation
   - Comprehensive reports with statistics

6. **Enhanced Data Model**
   - LastModifiedDate tracking
   - Versioned storage format (v2)
   - Backward compatible with v1

### Technical Improvements

- Modular architecture with separate units for export and history
- Type-safe dynamic array handling
- TFileStream-based binary storage
- Proper HTML/JSON escaping
- Category-based statistics aggregation

## Architecture

The system is built with a clean, modular architecture:

- **TaskTypes**: Core data types, enumerations, and helper functions
- **TaskManager**: Business logic and task operations
- **TaskStorage**: Binary persistence with versioned format
- **TaskExport**: Multiple export formats (JSON, HTML, Markdown)
- **TaskHistory**: Audit trail and change tracking
- **TaskManagerMain**: Main program and comprehensive self-test

See [ARCHITECTURE.md](ARCHITECTURE.md) for detailed architecture documentation.

## Development

### Code Style
- All Pascal reserved words in lowercase
- Modular design with single responsibility
- Comprehensive inline documentation
- Type-safe dynamic arrays
- No global variables in business logic

### Testing
- Built-in self-test in TaskManagerMain
- Tests all CRUD operations
- Tests all new features (categories, dependencies, time tracking)
- Tests all export formats
- Verifies persistence and reload

## Future Enhancements

Potential areas for future development:
- Web-based interface (mORMot2 integration)
- SQLite database backend
- User authentication and multi-user support
- Task attachments and comments
- Email notifications for due dates
- Recurring tasks
- Task templates
- Gantt chart visualization
- REST API for integration

## License

Open source - free to use and modify.

## Author

Created with passion for clean, modular Pascal code.

---

**Task Manager v2.0.0** - A powerful, flexible task management solution in Plain Pascal.
