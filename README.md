
# Pascal Task Manager

## Overview
This is a robust Task Manager application written in Free Pascal (FPC). It is designed to be a core logic component, suitable for integration with various interfaces. The project emphasizes clean code, dynamic memory management, and self-testing capabilities.

## Features
*   **Dynamic Task Management:** Uses dynamic arrays to handle a flexible number of tasks.
*   **Task Structure:** Each task includes an ID, Title, Description, Status, Priority, and Creation Date.
*   **CRUD Operations:**
    *   **Create:** Add new tasks with optional priority.
    *   **Read:** Retrieve tasks by Index, find by ID, or filter by Status.
    *   **Update:** Change task status (e.g., Pending -> Completed).
    *   **Delete:** Remove tasks by ID with automatic array resizing.
*   **Sorting:** Built-in Bubble Sort to order tasks by Priority (High -> Medium -> Low).
*   **Self-Testing:** Includes a comprehensive `SelfTest` procedure verifying all logic without user input.

## Technical Details
*   **Compiler:** Free Pascal (FPC) 3.2.2+
*   **Mode:** `{$mode objfpc}`
*   **Memory:** Dynamic arrays for storage.
*   **Architecture:** Separated logic (`task_types.pas`) and execution (`task_manager.pas`).

## Building
To build the project, run:
`fpc solution1/task_manager.pas -obin/task_manager -O1 -Mobjfpc`

## Usage
Run the generated executable in the `bin` folder to execute the self-tests:
`./bin/task_manager`
