
# Task Manager Architecture Documentation

## System Overview

The Task Manager is designed with a layered architecture promoting separation of concerns, maintainability, and testability.

## Architecture Layers

```
┌─────────────────────────────────────┐
│   Presentation Layer                │
│   (TaskManagerMain.pas)             │
│   - Self-test suite                 │
│   - Output formatting               │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│   Business Logic Layer              │
│   (TaskManager.pas)                 │
│   - Task operations (CRUD)          │
│   - Search & filtering              │
│   - Statistics calculation          │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│   Data Access Layer                 │
│   (TaskStorage.pas)                 │
│   - File I/O operations             │
│   - Serialization                   │
│   - Export functionality            │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│   Data Model Layer                  │
│   (TaskTypes.pas)                   │
│   - Type definitions                │
│   - Constants                       │
│   - Helper functions                │
└─────────────────────────────────────┘
```

## Component Details

### TaskTypes.pas - Data Model Layer

**Purpose**: Define core data structures and types used throughout the system.

**Key Components**:
- `TTaskPriority`: Enumerated type for priority levels
- `TTaskStatus`: Enumerated type for task states
- `TTask`: Record containing all task properties
- `TTaskArray`: Dynamic array type for task collections
- `TSearchCriteria`: Record for search parameters
- `TTaskStatistics`: Record for statistical data

**Design Decisions**:
- Used records instead of classes for lightweight data structures
- Enumerations provide type safety for priority and status
- Dynamic arrays for flexible collection sizes
- Helper functions for string conversions

**Dependencies**: SysUtils, DateUtils

### TaskManager.pas - Business Logic Layer

**Purpose**: Implement core task management functionality.

**Key Components**:
- `TTaskManagerCore`: Main class managing task operations

**Public Methods**:
- `AddTask`: Create new task
- `DeleteTask`: Soft delete (mark inactive)
- `UpdateTask`: Modify task properties
- `GetTask`: Retrieve single task
- `SetTaskStatus`, `CompleteTask`, `CancelTask`: Status management
- `GetAllTasks`, `GetActiveTasks`: Retrieve task lists
- `SearchTasks`: Flexible search with criteria
- `GetTasksByStatus`, `GetTasksByPriority`: Filtered retrieval
- `GetOverdueTasks`: Find overdue tasks
- `GetStatistics`: Calculate statistics
- `ClearAll`: Reset all tasks
- `GetTasksArray`, `SetTasksArray`: Bulk operations for persistence

**Private Fields**:
- `FTasks`: Dynamic array storing all tasks
- `FNextID`: Auto-increment counter for task IDs
- `FModified`: Dirty flag for unsaved changes

**Private Methods**:
- `FindTaskIndex`: Internal lookup by ID
- `GetTaskCount`: Calculate active task count

**Design Decisions**:
- Class-based for encapsulation and state management
- Soft delete preserves data integrity
- Linear search acceptable for typical task counts
- Filtered search using criteria record pattern
- Statistics calculated on-demand (no caching)

**Dependencies**: SysUtils, DateUtils, Math, TaskTypes

### TaskStorage.pas - Data Access Layer

**Purpose**: Handle persistence and export operations.

**Key Components**:
- `TTaskStorage`: Class managing file operations

**Public Methods**:
- `SaveTasks`: Write tasks to binary file
- `LoadTasks`: Read tasks from binary file
- `ExportToCSV`: Export to CSV format
- `ExportToText`: Export to human-readable text

**Binary File Format**:
```
[4 bytes] Next ID
[4 bytes] Task Count
For each task:
  [4 bytes] ID
  [1 byte]  Priority (enum)
  [1 byte]  Status (enum)
  [8 bytes] Created Date (TDateTime)
  [8 bytes] Due Date (TDateTime)
  [8 bytes] Completed Date (TDateTime)
  [1 byte]  IsActive (boolean)
  [4 bytes] Title Length
  [N bytes] Title String
  [4 bytes] Description Length
  [N bytes] Description String
  [4 bytes] Tags Length
  [N bytes] Tags String
```

**Design Decisions**:
- Binary format for efficiency
- Variable-length strings with length prefix
- Fixed-field data written first for alignment
- CSV with proper escaping for special characters
- Text format optimized for human readability
- Exception handling for all file operations

**Dependencies**: SysUtils, Classes, TaskTypes

### TaskManagerMain.pas - Presentation Layer

**Purpose**: Demonstrate functionality through comprehensive testing.

**Key Components**:
- `SelfTest`: Main test procedure
- `PrintTask`: Format single task for display
- `PrintTaskList`: Format task collection
- `PrintStatistics`: Format statistics

**Test Scenarios**:
1. Task creation with various properties
2. Listing and display operations
3. Status updates and transitions
4. Filtering and search operations
5. Statistics calculation
6. Persistence (save/load cycle)
7. Export operations
8. Data integrity verification

**Design Decisions**:
- No user input (ReadLn) for automation
- Comprehensive output for verification
- Tests arranged in logical progression
- Demonstrates both typical and edge cases
- Clean resource management (try-finally)

**Dependencies**: SysUtils, DateUtils, Math, TaskTypes, TaskManager, TaskStorage

## Data Flow

### Creating a Task
```
User/Test → AddTask() → TaskManager.FTasks[] → FModified = true
```

### Searching Tasks
```
SearchCriteria → SearchTasks() → Filter FTasks[] → Return TTaskArray
```

### Persisting Tasks
```
TaskManager.GetTasksArray() → TaskStorage.SaveTasks() → Binary File
```

### Loading Tasks
```
Binary File → TaskStorage.LoadTasks() → TaskManager.SetTasksArray()
```

## Memory Management

- **Dynamic Arrays**: All task collections use dynamic arrays
- **String Management**: Automatic (long strings with reference counting)
- **Object Lifecycle**: Manual (Create/Free pattern)
- **No Memory Leaks**: Proper cleanup in destructors

## Error Handling Strategy

- **File Operations**: Try-except with WriteLn error messages
- **Search Operations**: Return empty arrays on no match
- **Lookup Operations**: Return boolean success flag
- **Invalid Input**: Return false or default values

## Performance Characteristics

- **Task Lookup**: O(n) linear search
- **Task Creation**: O(1) amortized (dynamic array growth)
- **Task Deletion**: O(n) (soft delete, mark inactive)
- **Search**: O(n) with early termination
- **Statistics**: O(n) single pass

## Scalability Considerations

**Current Design**:
- Suitable for hundreds to low thousands of tasks
- All tasks loaded in memory
- Linear search acceptable for small datasets

**Future Optimizations** (if needed):
- Hash table for O(1) ID lookup
- Indexed searches for common queries
- Lazy loading for large datasets
- Database backend for persistence
- Pagination for large result sets

## Extensibility Points

1. **New Task Properties**: Add to TTask record
2. **New Search Criteria**: Extend TSearchCriteria
3. **New Export Formats**: Add methods to TTaskStorage
4. **New Statistics**: Add fields to TTaskStatistics
5. **Custom Sorting**: Add comparison functions
6. **Validation Rules**: Add to TaskManager methods

## Testing Strategy

- **Unit-level**: Each module compilable independently
- **Integration**: SelfTest validates cross-module operations
- **Data Integrity**: Load/save cycle verification
- **Edge Cases**: Empty lists, invalid IDs, overdue detection
- **Output Verification**: All operations produce visible output

## Code Quality Standards

- **Naming**: Descriptive names with type prefixes (T for types, F for fields, a for parameters)
- **Documentation**: Comments for all public interfaces
- **Error Messages**: Descriptive error output
- **Magic Numbers**: Named constants
- **Code Layout**: Consistent indentation and spacing
- **Type Safety**: Strong typing, enumerations over integers
