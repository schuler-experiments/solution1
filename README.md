# Pascal Task Manager

A robust, feature-rich Task Manager written in Free Pascal.

## Features

*   **Core CRUD:** Create, Read, Update, Delete tasks.
*   **Task Attributes:** Title, Description, Priority, Due Date, Status.
*   **Tagging System:** Add multiple tags to tasks for categorization.
*   **Search:** Filter tasks by title or description.
*   **Dependencies:** Link tasks (Task B depends on Task A). Includes circular dependency protection and "Can Start" logic.
*   **Time Tracking:** Track time spent on tasks (Start/Stop timer).
*   **Statistics:** View real-time stats (Total, Pending, Completed, Overdue, Blocked).
*   **HTML Export:** Generate a styled HTML report of all tasks.
*   **Persistence:** Save and load tasks from a custom pipe-delimited file format.
*   **Self-Testing:** Built-in self-test suite to verify all functionality.

## Compilation

To compile the project, use the following command:

```bash
fpc solution1/task_manager.pas -obin/task_manager -O1 -Mobjfpc
```

## Usage

Run the compiled binary to execute the self-test suite:

```bash
bin/task_manager
```

## Structure

*   `task_manager.pas`: Main program entry point and self-test suite.
*   `task_types.pas`: Core logic unit containing the `TTaskManager` class and `TTask` record.
*   `bin/`: Directory for the compiled executable.
