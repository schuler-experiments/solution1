
# Task Manager (Pascal)

A simple, robust Task Manager library written in Free Pascal.

## Features

*   **Core Task Management:** Create, Read, Update, and Delete (CRUD) tasks.
*   **Prioritization:** Assign Low, Medium, or High priority to tasks.
*   **Sorting:** Sort tasks by priority (High -> Low).
*   **Due Dates:** Assign due dates to tasks and find overdue items.
*   **Tagging:** Add tags to tasks and filter by tags (case-insensitive).
*   **Status Tracking:** Track task status (Pending, In Progress, Completed).

## File Structure

*   `task_types.pas`: The core unit containing the `TTask` record, `TTaskManager` class, and all logic.
*   `task_manager.pas`: The main program file used for testing and demonstration (`SelfTest`).

## Compilation

To compile the project using Free Pascal (FPC):

```bash
fpc solution1/task_manager.pas -obin/task_manager -O1 -Mobjfpc
```

## Usage

The `TTaskManager` class is the entry point.

```pascal
uses task_types;

var
  Manager: TTaskManager;
begin
  Manager := TTaskManager.Create;
  try
    Manager.AddTask('My Task', 'Description', tpHigh);
    // ... use other methods
  finally
    Manager.Free;
  end;
end;
```
