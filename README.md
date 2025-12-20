
# Pascal Task Manager

A robust, modular task management system written in Free Pascal.

## Features
- **Task Management**: Create, update, and remove tasks with titles, descriptions, categories, and priorities.
- **Hierarchical Tasks**: Support for subtasks (parent-child relationships).
- **Time Tracking**: Integrated timer system to log actual effort spent on tasks.
- **Task Templates**: Predefined templates for quick task creation (e.g., Bug Reports, Feature Requests).
- **Dependency Management**: Track task dependencies to ensure correct workflow.
- **Visualizations**:
  - **Gantt Chart**: Text-based timeline view of task schedules.
  - **Eisenhower Matrix**: Categorize tasks by urgency and importance.
  - **Mermaid Export**: Generate diagrams for task relationships.
- **Reporting**: Export task lists to JSON, CSV, and styled HTML.
- **Persistence**: Save and load task data to/from disk.
- **Self-Testing**: Comprehensive automated test suite included.

## Architecture
- `utaskmanager.pas`: Core logic, data structures, and task manipulation.
- `utasktemplates.pas`: Template management system.
- `task_manager.pas`: Main application entry point and test runner.

## Compilation
To compile the project, use the following command:
```bash
fpc solution1/task_manager.pas -obin/task_manager -O1 -Mobjfpc
```

## Usage
The application runs a comprehensive self-test by default, demonstrating all features including subtasks, time tracking, and visualizations.

## Team Management & Collaboration (uteammanager.pas)
This new module introduces human resource management to the task manager:
- **Skill-based Assignment**: Suggests the best team member for a task by matching task tags with member skills.
- **Workload Balancing**: Considers current assigned effort to avoid overloading members.
- **Gamification**: Implements an XP and Leveling system to track and reward member contributions.
