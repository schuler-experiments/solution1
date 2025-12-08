
# Pascal Task Manager

A robust, command-line task manager written in Free Pascal.

## Features

*   **Task Management**: Create, Read, Update, and Delete (CRUD) tasks.
*   **Dynamic Arrays**: Uses dynamic arrays for efficient memory management.
*   **Tagging System**: Add multiple tags to tasks for better organization.
*   **Priorities**: Assign Low, Medium, or High priority to tasks.
*   **Due Dates**: Set due dates and track overdue tasks.
*   **Persistence**: Save and load tasks to/from a text file (`.db`).
*   **Sorting**: Sort tasks by priority.

## Project Structure

*   `task_manager.pas`: Main program file containing the `SelfTest` procedure.
*   `task_types.pas`: Unit containing the `TTask` record, `TTaskManager` class, and core logic.
*   `bin/`: Directory where the compiled executable is stored.

## Compilation

To compile the project, use the following command:

```bash
fpc solution1/task_manager.pas -obin/task_manager -O1 -Mobjfpc
```

## Usage

Currently, the program runs a comprehensive self-test suite to verify all functionalities.

```bash
./bin/task_manager
```

## Implementation Details

*   **Language**: Free Pascal (FPC)
*   **Mode**: `{$mode objfpc}`
*   **Data Storage**: In-memory dynamic arrays with file persistence support.
*   **No User Input**: Designed as a reusable library/backend logic.
