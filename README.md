
# Task Manager (Pascal)

A robust command-line Task Manager implementation in Free Pascal, demonstrating advanced features like dynamic arrays, object-oriented design, JSON/CSV serialization, and Undo/Redo functionality.

## Features

*   **Task Management:** Add, update, delete, and list tasks.
*   **Task Attributes:** Title, Description, Status (Pending, InProgress, Completed), Priority (Low, Medium, High), Due Date, Tags.
*   **Dependencies:** Support for task dependencies (Task B cannot start until Task A is completed) with circular dependency detection.
*   **Time Tracking:** Start/Stop timers for tasks and track total time spent.
*   **Search & Filtering:** Search by text, filter by status or tags, identify overdue tasks.
*   **Bulk Operations:** Complete all tasks with a specific tag.
*   **Undo/Redo:** Full support for undoing and redoing Add, Update, and Delete operations.
*   **Persistence:**
    *   Binary format (custom) for efficient storage.
    *   JSON import/export for interoperability.
    *   **CSV import/export for spreadsheet integration.**
    *   HTML report generation.
*   **Statistics:** Get counts of pending, completed, overdue, and blocked tasks.

## Project Structure

*   `task_manager.pas`: Main program file containing the `SelfTest` procedure to verify all functionality.
*   `task_types.pas`: Core unit defining data structures (`TTask`, `TTaskManager`, `TUndoManager`) and logic.
*   `task_json_utils.pas`: Unit for JSON import/export functionality.
*   `task_csv_utils.pas`: Unit for CSV import/export functionality.

## Compilation

To compile the project, use the following command:

```bash
fpc solution1/task_manager.pas -obin/task_manager -O1 -Mobjfpc
```

## Running

After compilation, run the executable:

```bash
bin/task_manager
```

This will execute the comprehensive self-test suite, printing the results to the console.

## Implementation Details

*   **Dynamic Arrays:** Used extensively for managing tasks, tags, dependencies, and the undo/redo stack.
*   **Object Pascal:** Uses classes (`TTaskManager`, `TUndoManager`) and records (`TTask`) for structured code.
*   **Memory Management:** Proper cleanup in destructors.
*   **Error Handling:** Robust checks for invalid IDs, circular dependencies, and file I/O errors.
