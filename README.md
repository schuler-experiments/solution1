
# Pascal Task Manager

## Overview
This is a robust Task Manager application written in Free Pascal (FPC). It is designed to be a core logic component, suitable for integration with various interfaces. The project emphasizes clean code, dynamic memory management, and self-testing capabilities.

## Features
*   **Dynamic Task Management:** Uses dynamic arrays to handle a flexible number of tasks.
*   **Task Structure:** Each task includes an ID, Title, Description, Status, and Creation Date.
*   **Self-Testing:** Includes a built-in `SelfTest` procedure to verify functionality without user input.
*   **Compilation:** Optimized for the `fpc` compiler with `objfpc` mode.

## Building
To build the project, run:
`fpc solution1/task_manager.pas -obin/task_manager -O1 -Mobjfpc`

## Usage
Run the generated executable in the `bin` folder to execute the self-tests.
