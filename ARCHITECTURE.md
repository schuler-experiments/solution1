# Task Manager - System Architecture

## Overview

The Task Manager is built using a modular, object-oriented architecture in Free Pascal (FPC) with the {$mode objfpc} compiler directive. The system consists of 21 independent modules that provide comprehensive task management capabilities.

## Architecture Principles

1. **Modularity**: Each feature is implemented in a separate unit
2. **Reusability**: Code is designed to be framework-agnostic
3. **Extensibility**: New features can be added without modifying existing code
4. **Type Safety**: Strong typing with custom types and enumerations
5. **Memory Safety**: Proper resource management with constructors/destructors
6. **Testability**: Each module includes self-tests

## Module Dependency Graph

```
solution1.pas (main)
    |
    +-- TaskManager (core)
    |   +-- TaskTypes (base types)
    |
    +-- TaskPersistence
    |   +-- TaskTypes
    |   +-- TaskManager
    |
    +-- TaskQuery
    |   +-- TaskTypes
    |   +-- TaskManager
    |
    +-- TaskNotes
    |   +-- TaskTypes
    |
    +-- TaskScheduling
    |   +-- TaskTypes
    |   +-- DateUtils
    |
    +-- TaskDependencies
    |   +-- TaskTypes
    |
    +-- TaskValidation
    |   +-- TaskTypes
    |
    +-- TaskCategories
    |   +-- TaskTypes
    |
    +-- TaskPriorityScoring
    |   +-- TaskTypes
    |
    +-- TaskReporting
    |   +-- TaskTypes
    |
    +-- TaskTeam
    |   +-- TaskTypes
    |
    +-- TaskBudget
    |   +-- TaskTypes
    |
    +-- TaskRisk
    |   +-- TaskTypes
    |
    +-- TaskCollaboration
    |   +-- TaskTypes
    |
    +-- TaskJSON
    |   +-- TaskTypes
    |   +-- TaskManager
    |
    +-- TaskReminders
    |   +-- TaskTypes
    |   +-- DateUtils
    |
    +-- TaskAnalytics
    |   +-- TaskTypes
    |   +-- DateUtils
    |   +-- Math
    |
    +-- TaskExport
    |   +-- TaskTypes
    |   +-- SysUtils
    |   +-- Classes
    |
    +-- TaskSearch
    |   +-- TaskTypes
    |   +-- SysUtils
    |   +-- DateUtils
```

## Core Data Types

### TTaskStatus
```pascal
type
  TTaskStatus = (tsNotStarted, tsInProgress, tsCompleted, tsOnHold);
```

### TTaskPriority
```pascal
type
  TTaskPriority = (tpLow, tpMedium, tpHigh, tpCritical);
```

### TTask
```pascal
type
  TTask = record
    id: integer;
    title: string;
    description: string;
    priority: TTaskPriority;
    status: TTaskStatus;
    dueDate: TDateTime;
    createdDate: TDateTime;
    completedDate: TDateTime;
    category: string;
    assignee: string;
    estimatedHours: double;
    tags: array of string;
  end;

  TTaskArray = array of TTask;
```

## Class Hierarchy

All manager classes inherit from TObject and follow a consistent pattern:

```pascal
TTaskManagerClass = class
public
  constructor Create();
  destructor Destroy();
  { Public methods }
  procedure SelfTest();
private
  { Private data }
  { Private helper methods }
end;
```

## Key Design Patterns

### Factory Pattern
- Task creation methods return IDs that can be used to retrieve objects
- Example: `AddTask()` returns task ID

### Repository Pattern
- Manager classes act as repositories for their domain
- Example: `TTaskManagerClass` manages all task operations

### Observer Pattern (implicit)
- History tracking records all changes
- Activity logs track collaboration events

### Strategy Pattern
- Multiple export formats (JSON, XML, CSV, HTML)
- Multiple search strategies (full-text, advanced)

## Data Flow

### Task Lifecycle

```
1. Create Task
   |
   v
2. Assign Properties
   |
   v
3. Add to Manager
   |
   v
4. Update/Modify (optional)
   |
   v
5. Track Time (optional)
   |
   v
6. Add Comments (optional)
   |
   v
7. Complete Task
   |
   v
8. Archive/Export
```

### Query Flow

```
Raw Task List
    |
    v
Filter by Criteria
    |
    v
Sort Results
    |
    v
Calculate Metrics
    |
    v
Return Results
```

## Memory Management

- **Dynamic Arrays**: All collections use dynamic arrays
- **SetLength()**: Used to allocate/deallocate memory
- **Constructors**: Initialize objects properly
- **Destructors**: Clean up resources with inherited Destroy()

## Compilation

```bash
fpc solution1.pas -obin/task_manager -O1 -Mobjfpc
```

### Compiler Flags
- `-O1`: Optimization level 1
- `-Mobjfpc`: Object Pascal mode for advanced OOP features

## Performance Characteristics

- **Task Lookup**: O(n) linear search
- **Task Filtering**: O(n) iteration with predicates
- **Sorting**: Depends on algorithm (typically O(n log n))
- **Memory**: Dynamic allocation only when needed

## Integration Architecture

### API Surface

Each module exposes:
1. **Type Definitions**: Record types, enumerations
2. **Manager Class**: Main interface for operations
3. **Helper Functions**: Utility functions for conversions

### Export Formats

- **JSON**: Structured data interchange
- **XML**: Hierarchical data representation
- **CSV**: Spreadsheet-compatible format
- **HTML**: Web-ready reports

### Import Mechanisms

- **CSV Import**: Load from spreadsheet exports
- **JSON Import**: Restore from backups
- **Text Import**: Parse human-readable formats

## Error Handling

- **Try-Except Blocks**: Wrap file I/O operations
- **Validation**: Input validation before processing
- **Status Returns**: Boolean or record return types for error info

## Testing Strategy

Each module includes a `SelfTest()` procedure that:
1. Creates test data
2. Exercises all public methods
3. Validates results
4. Cleans up resources

## Future Extensibility

### Adding New Modules

1. Create new unit file
2. Define types in interface section
3. Implement manager class
4. Add SelfTest procedure
5. Update solution1.pas to instantiate
6. Keep file under 500 lines

### Adding New Features

Features should:
- Be modular and independent
- Follow existing patterns
- Include comprehensive tests
- Document public API
- Avoid circular dependencies

## Code Guidelines

1. **Lowercase Keywords**: All Pascal reserved words in lowercase
2. **Naming Conventions**: 
   - Types: T prefix (TTask, TTaskArray)
   - Classes: Class suffix (TTaskManagerClass)
   - Variables: camelCase (taskId, dueDate)
3. **Memory**: Always Free() objects when done
4. **Arrays**: Use SetLength for dynamic allocation
5. **Strings**: Use string type (managed in FPC)
6. **Types**: Create type aliases for complex structures
