
# Free Pascal Task Manager Library - Software Specification

**Version:** 1.0.0  
**Date:** December 2024  
**Language:** Free Pascal / Object Pascal  
**Type:** Reusable Library Component

---

## Table of Contents

1. [Overview of Software Architecture](#1-overview-of-software-architecture)
2. [Detailed Module/Component Descriptions](#2-detailed-modulecomponent-descriptions)
3. [Data Models and Structures](#3-data-models-and-structures)
4. [API Endpoints and Usage](#4-api-endpoints-and-usage)
5. [User Interface Designs](#5-user-interface-designs)
6. [Third-Party Libraries and Services](#6-third-party-libraries-and-services)
7. [Deployment and Scaling Strategies](#7-deployment-and-scaling-strategies)
8. [Testing Strategies and Coverage](#8-testing-strategies-and-coverage)
9. [Class Diagrams and Methods/Properties](#9-class-diagrams-and-methodsproperties)
10. [Source Code Organization and File Structure](#10-source-code-organization-and-file-structure)
11. [Coding Task List](#11-coding-task-list)

---

## 1. Overview of Software Architecture

### 1.1 Introduction

The Free Pascal Task Manager Library is a modular, reusable component designed to provide comprehensive task management functionality without any user interface dependencies. This library can be integrated into console applications, GUI applications (Lazarus, fpGUI, MSEgui), web services, or any other Free Pascal project requiring task management capabilities.

### 1.2 Architectural Principles

The architecture follows these key principles:

- **Separation of Concerns**: Clear separation between data models, business logic, and persistence
- **Modularity**: Each component is self-contained and can be used independently
- **No UI Dependencies**: Zero coupling to any user interface framework (no ReadLn, WriteLn for user interaction)
- **Testability**: All components are designed to be easily unit-testable
- **Extensibility**: Open for extension through inheritance and interfaces
- **Type Safety**: Strong typing throughout with proper use of Object Pascal features

### 1.3 High-Level Architecture

The library is structured in three main layers:

```
┌─────────────────────────────────────────────────────────────┐
│                     Consumer Application                     │
│            (Console, GUI, Web Service, etc.)                 │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                   TASK MANAGER LIBRARY                       │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────────────────────────────────────────────┐   │
│  │         Business Logic Layer                         │   │
│  │  • TTaskManager (CRUD operations)                    │   │
│  │  • TTaskFilter (filtering & searching)               │   │
│  │  • TTaskValidator (validation logic)                 │   │
│  │  • TTaskStatistics (analytics & reporting)           │   │
│  └─────────────────────────────────────────────────────┘   │
│                           │                                   │
│                           ▼                                   │
│  ┌─────────────────────────────────────────────────────┐   │
│  │         Data Model Layer                             │   │
│  │  • TTask (task entity)                               │   │
│  │  • TTaskList (collection)                            │   │
│  │  • Enumerations (Status, Priority, Category)         │   │
│  └─────────────────────────────────────────────────────┘   │
│                           │                                   │
│                           ▼                                   │
│  ┌─────────────────────────────────────────────────────┐   │
│  │         Persistence Layer                            │   │
│  │  • ITaskStorage (interface)                          │   │
│  │  • TJSONTaskStorage (JSON implementation)            │   │
│  │  • TXMLTaskStorage (XML implementation)              │   │
│  │  • TCSVTaskStorage (CSV implementation)              │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

### 1.4 Core Components Overview

1. **Data Model Layer**: Defines the task entity and related data structures
2. **Business Logic Layer**: Implements task management operations, filtering, and validation
3. **Persistence Layer**: Handles loading and saving tasks in various formats
4. **Utility Layer**: Provides helper functions for date/time, string manipulation, etc.

### 1.5 Design Patterns Used

- **Repository Pattern**: For data access abstraction (ITaskStorage interface)
- **Strategy Pattern**: For different persistence implementations
- **Factory Pattern**: For creating storage instances based on configuration
- **Observer Pattern**: For task change notifications (optional event system)
- **Singleton Pattern**: For TaskManager instance management (optional)

---

## 2. Detailed Module/Component Descriptions

### 2.1 Data Model Module (`TaskModel.pas`)

**Purpose**: Defines the core task entity and related enumerations.

**Key Classes/Types**:
- `TTask`: The main task entity class
- `TTaskStatus`: Enumeration for task states
- `TTaskPriority`: Enumeration for task priorities
- `TTaskCategory`: Enumeration for task categories

**Responsibilities**:
- Store task data (title, description, dates, etc.)
- Provide property accessors with validation
- Support serialization/deserialization
- Implement task cloning and comparison

**Dependencies**: None (base module)

### 2.2 Task Collection Module (`TaskList.pas`)

**Purpose**: Manages collections of tasks with efficient operations.

**Key Classes**:
- `TTaskList`: Generic list implementation for tasks
- `TTaskListEnumerator`: For foreach iteration support

**Responsibilities**:
- Store multiple tasks in memory
- Provide indexed access to tasks
- Support addition, removal, and sorting
- Implement filtering and searching
- Memory management for task objects

**Dependencies**: `TaskModel.pas`

### 2.3 Task Manager Module (`TaskManager.pas`)

**Purpose**: Core business logic for task management operations.

**Key Classes**:
- `TTaskManager`: Main manager class for CRUD operations

**Responsibilities**:
- Create, read, update, delete tasks
- Coordinate between data model and persistence
- Maintain task list in memory
- Provide task querying capabilities
- Handle task validation before operations
- Trigger events on task changes

**Dependencies**: `TaskModel.pas`, `TaskList.pas`, `TaskStorage.pas`

### 2.4 Task Filter Module (`TaskFilter.pas`)

**Purpose**: Advanced filtering and searching capabilities.

**Key Classes**:
- `TTaskFilter`: Filter builder for complex queries
- `TTaskSearchCriteria`: Search criteria container

**Responsibilities**:
- Filter tasks by status, priority, category
- Filter by date ranges (created, due, completed)
- Text search in title and description
- Combine multiple filter criteria
- Sort results by various fields

**Dependencies**: `TaskModel.pas`, `TaskList.pas`

### 2.5 Task Validator Module (`TaskValidator.pas`)

**Purpose**: Validation logic for task data.

**Key Classes**:
- `TTaskValidator`: Validator class with validation rules

**Responsibilities**:
- Validate task title (required, length limits)
- Validate dates (due date after creation, etc.)
- Validate priority and status combinations
- Check for duplicate IDs
- Return validation results with error messages

**Dependencies**: `TaskModel.pas`

### 2.6 Task Storage Interface Module (`TaskStorage.pas`)

**Purpose**: Abstract interface for persistence implementations.

**Key Interfaces**:
- `ITaskStorage`: Base interface for all storage implementations

**Responsibilities**:
- Define contract for load/save operations
- Define transaction support methods
- Define error handling contract

**Dependencies**: `TaskModel.pas`, `TaskList.pas`

### 2.7 JSON Storage Module (`TaskStorageJSON.pas`)

**Purpose**: JSON-based persistence implementation.

**Key Classes**:
- `TJSONTaskStorage`: Implements ITaskStorage for JSON format

**Responsibilities**:
- Serialize tasks to JSON format
- Deserialize JSON to task objects
- Handle file I/O operations
- Manage backup files
- Format JSON for readability

**Dependencies**: `TaskStorage.pas`, `fpjson`, `jsonparser`

### 2.8 XML Storage Module (`TaskStorageXML.pas`)

**Purpose**: XML-based persistence implementation.

**Key Classes**:
- `TXMLTaskStorage`: Implements ITaskStorage for XML format

**Responsibilities**:
- Serialize tasks to XML format
- Deserialize XML to task objects
- Handle XML schema validation
- Support XPath queries (optional)

**Dependencies**: `TaskStorage.pas`, `DOM`, `XMLRead`, `XMLWrite`

### 2.9 CSV Storage Module (`TaskStorageCSV.pas`)

**Purpose**: CSV-based persistence for simple import/export.

**Key Classes**:
- `TCSVTaskStorage`: Implements ITaskStorage for CSV format

**Responsibilities**:
- Export tasks to CSV for spreadsheet use
- Import tasks from CSV files
- Handle CSV formatting and escaping
- Support custom delimiters

**Dependencies**: `TaskStorage.pas`, `csvdocument`

### 2.10 Task Statistics Module (`TaskStatistics.pas`)

**Purpose**: Analytics and reporting on task data.

**Key Classes**:
- `TTaskStatistics`: Statistical analysis class

**Responsibilities**:
- Calculate completion rates
- Count tasks by status/priority/category
- Calculate average completion time
- Generate productivity metrics
- Identify overdue tasks
- Trend analysis

**Dependencies**: `TaskModel.pas`, `TaskList.pas`

### 2.11 Utility Module (`TaskUtils.pas`)

**Purpose**: Helper functions and utilities.

**Key Functions**:
- Date/time formatting and parsing
- String manipulation for task text
- ID generation (GUID or incremental)
- Enum conversion utilities
- Common validation helpers

**Dependencies**: `SysUtils`, `DateUtils`

---

## 3. Data Models and Structures

### 3.1 TTaskStatus Enumeration

Represents the current state of a task.

```pascal
type
  TTaskStatus = (
    tsNotStarted,    // Task created but not yet started
    tsInProgress,    // Task is currently being worked on
    tsOnHold,        // Task temporarily paused
    tsCompleted,     // Task successfully completed
    tsCancelled,     // Task cancelled/abandoned
    tsDeferred       // Task postponed to later date
  );
```

### 3.2 TTaskPriority Enumeration

Represents the importance/urgency of a task.

```pascal
type
  TTaskPriority = (
    tpLowest,        // Minimal priority
    tpLow,           // Below normal priority
    tpNormal,        // Default priority
    tpHigh,          // Above normal priority
    tpHighest,       // Critical/urgent priority
    tpCritical       // Emergency priority
  );
```

### 3.3 TTaskCategory Enumeration

Represents the classification/type of a task.

```pascal
type
  TTaskCategory = (
    tcPersonal,      // Personal tasks
    tcWork,          // Work-related tasks
    tcShopping,      // Shopping/errands
    tcHealth,        // Health and fitness
    tcFinance,       // Financial tasks
    tcEducation,     // Learning and education
    tcHome,          // Home maintenance
    tcSocial,        // Social events and activities
    tcOther          // Miscellaneous
  );
```

### 3.4 TTask Class

The core task entity with all properties and methods.

```pascal
type
  TTask = class(TObject)
  private
    FID: string;
    FTitle: string;
    FDescription: string;
    FStatus: TTaskStatus;
    FPriority: TTaskPriority;
    FCategory: TTaskCategory;
    FCreatedDate: TDateTime;
    FDueDate: TDateTime;
    FCompletedDate: TDateTime;
    FEstimatedMinutes: Integer;
    FActualMinutes: Integer;
    FTags: TStringList;
    FNotes: string;
    
    procedure SetTitle(const AValue: string);
    procedure SetDueDate(const AValue: TDateTime);
    function GetIsOverdue: Boolean;
    function GetIsCompleted: Boolean;
  public
    constructor Create; overload;
    constructor Create(const ATitle: string); overload;
    destructor Destroy; override;
    
    function Clone: TTask;
    function IsEqual(ATask: TTask): Boolean;
    procedure MarkAsCompleted;
    procedure MarkAsStarted;
    function GetDaysUntilDue: Integer;
    function GetCompletionPercentage: Integer;
    
    property ID: string read FID write FID;
    property Title: string read FTitle write SetTitle;
    property Description: string read FDescription write FDescription;
    property Status: TTaskStatus read FStatus write FStatus;
    property Priority: TTaskPriority read FPriority write FPriority;
    property Category: TTaskCategory read FCategory write FCategory;
    property CreatedDate: TDateTime read FCreatedDate write FCreatedDate;
    property DueDate: TDateTime read FDueDate write SetDueDate;
    property CompletedDate: TDateTime read FCompletedDate write FCompletedDate;
    property EstimatedMinutes: Integer read FEstimatedMinutes write FEstimatedMinutes;
    property ActualMinutes: Integer read FActualMinutes write FActualMinutes;
    property Tags: TStringList read FTags;
    property Notes: string read FNotes write FNotes;
    property IsOverdue: Boolean read GetIsOverdue;
    property IsCompleted: Boolean read GetIsCompleted;
  end;
```

### 3.5 TTaskList Class

Collection class for managing multiple tasks.

```pascal
type
  TTaskList = class(TObject)
  private
    FItems: array of TTask;
    FCount: Integer;
    FCapacity: Integer;
    FOwnsObjects: Boolean;
    
    function GetItem(Index: Integer): TTask;
    procedure SetItem(Index: Integer; const Value: TTask);
    procedure Grow;
  public
    constructor Create(AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    
    function Add(ATask: TTask): Integer;
    procedure Insert(Index: Integer; ATask: TTask);
    procedure Delete(Index: Integer);
    procedure Remove(ATask: TTask);
    procedure Clear;
    function IndexOf(ATask: TTask): Integer;
    function FindByID(const AID: string): TTask;
    procedure Sort(CompareFunc: TTaskCompareFunc);
    
    property Items[Index: Integer]: TTask read GetItem write SetItem; default;
    property Count: Integer read FCount;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;
  
  TTaskCompareFunc = function(Task1, Task2: TTask): Integer;
```

### 3.6 TValidationResult Record

Structure to hold validation results.

```pascal
type
  TValidationResult = record
    IsValid: Boolean;
    ErrorMessages: TStringList;
    
    procedure AddError(const AMessage: string);
    function GetErrorText: string;
  end;
```

### 3.7 TTaskFilterCriteria Record

Structure for filter criteria.

```pascal
type
  TTaskFilterCriteria = record
    FilterByStatus: Boolean;
    StatusFilter: TTaskStatus;
    
    FilterByPriority: Boolean;
    PriorityFilter: TTaskPriority;
    
    FilterByCategory: Boolean;
    CategoryFilter: TTaskCategory;
    
    FilterByDueDate: Boolean;
    DueDateFrom: TDateTime;
    DueDateTo: TDateTime;
    
    FilterByText: Boolean;
    SearchText: string;
    SearchInDescription: Boolean;
    
    FilterByTags: Boolean;
    RequiredTags: TStringList;
    
    procedure Reset;
  end;
```

### 3.8 TTaskStatisticsData Record

Structure for statistical data.

```pascal
type
  TTaskStatisticsData = record
    TotalTasks: Integer;
    CompletedTasks: Integer;
    InProgressTasks: Integer;
    OverdueTasks: Integer;
    CompletionRate: Double;
    AverageCompletionDays: Double;
    TasksByPriority: array[TTaskPriority] of Integer;
    TasksByCategory: array[TTaskCategory] of Integer;
    TasksByStatus: array[TTaskStatus] of Integer;
  end;
```



---

## 4. API Endpoints and Usage

### 4.1 Overview

This library does not expose HTTP/REST API endpoints as it is a component library designed to be integrated directly into Free Pascal applications. Instead, this section documents the **Public Programming Interface** - the classes, methods, and usage patterns that consumer applications will use.

### 4.2 Core API Classes

#### 4.2.1 TTaskManager - Main Entry Point

The `TTaskManager` class is the primary interface for task management operations.

**Initialization:**

```pascal
uses
  TaskManager, TaskModel, TaskList, TaskStorageJSON;

var
  TaskMgr: TTaskManager;
begin
  // Create task manager with JSON storage
  TaskMgr := TTaskManager.Create('tasks.json', TJSONTaskStorage);
  try
    // Use the task manager
  finally
    TaskMgr.Free;
  end;
end;
```

**Key Methods:**

```pascal
// Create a new task
function CreateTask(const ATitle: string): TTask;
function CreateTask(const ATitle, ADescription: string; 
                   APriority: TTaskPriority; 
                   ACategory: TTaskCategory): TTask;

// Retrieve tasks
function GetTaskByID(const AID: string): TTask;
function GetAllTasks: TTaskList;
function GetTasksByStatus(AStatus: TTaskStatus): TTaskList;
function GetTasksByPriority(APriority: TTaskPriority): TTaskList;
function GetTasksByCategory(ACategory: TTaskCategory): TTaskList;

// Update tasks
function UpdateTask(ATask: TTask): Boolean;
function UpdateTaskStatus(const AID: string; AStatus: TTaskStatus): Boolean;
function UpdateTaskPriority(const AID: string; APriority: TTaskPriority): Boolean;

// Delete tasks
function DeleteTask(const AID: string): Boolean;
function DeleteTask(ATask: TTask): Boolean;
procedure DeleteCompletedTasks;
procedure DeleteCancelledTasks;

// Persistence
function LoadTasks: Boolean;
function SaveTasks: Boolean;
function AutoSave: Boolean; // Saves only if changes detected

// Statistics
function GetStatistics: TTaskStatisticsData;
function GetOverdueTasks: TTaskList;
```

#### 4.2.2 TTask - Task Entity

**Creating and Manipulating Tasks:**

```pascal
var
  MyTask: TTask;
begin
  // Create a new task
  MyTask := TTask.Create('Complete project documentation');
  try
    MyTask.Description := 'Write comprehensive documentation for the task manager library';
    MyTask.Priority := tpHigh;
    MyTask.Category := tcWork;
    MyTask.DueDate := EncodeDate(2024, 12, 31);
    MyTask.EstimatedMinutes := 240; // 4 hours
    MyTask.Tags.Add('documentation');
    MyTask.Tags.Add('urgent');
    
    // Mark task as started
    MyTask.MarkAsStarted;
    
    // Check status
    if MyTask.IsOverdue then
      WriteLn('Task is overdue!');
      
    // Check days until due
    WriteLn('Days until due: ', MyTask.GetDaysUntilDue);
    
    // Mark as completed
    MyTask.MarkAsCompleted;
  finally
    MyTask.Free;
  end;
end;
```

#### 4.2.3 TTaskFilter - Advanced Filtering

**Building Complex Filters:**

```pascal
var
  Filter: TTaskFilter;
  Criteria: TTaskFilterCriteria;
  FilteredTasks: TTaskList;
begin
  Filter := TTaskFilter.Create(TaskMgr.GetAllTasks);
  try
    // Initialize criteria
    Criteria.Reset;
    
    // Filter by status
    Criteria.FilterByStatus := True;
    Criteria.StatusFilter := tsInProgress;
    
    // Filter by priority
    Criteria.FilterByPriority := True;
    Criteria.PriorityFilter := tpHigh;
    
    // Filter by due date range
    Criteria.FilterByDueDate := True;
    Criteria.DueDateFrom := Now;
    Criteria.DueDateTo := Now + 7; // Next 7 days
    
    // Text search
    Criteria.FilterByText := True;
    Criteria.SearchText := 'project';
    Criteria.SearchInDescription := True;
    
    // Apply filter
    FilteredTasks := Filter.ApplyFilter(Criteria);
    try
      // Use filtered results
      for i := 0 to FilteredTasks.Count - 1 do
        WriteLn(FilteredTasks[i].Title);
    finally
      FilteredTasks.Free;
    end;
  finally
    Filter.Free;
  end;
end;
```

#### 4.2.4 TTaskValidator - Validation

**Validating Task Data:**

```pascal
var
  Task: TTask;
  Validator: TTaskValidator;
  ValidationResult: TValidationResult;
begin
  Validator := TTaskValidator.Create;
  try
    Task := TTask.Create;
    try
      Task.Title := ''; // Invalid: empty title
      Task.DueDate := Now - 1; // Invalid: due date in the past
      
      ValidationResult := Validator.ValidateTask(Task);
      
      if not ValidationResult.IsValid then
      begin
        WriteLn('Validation failed:');
        WriteLn(ValidationResult.GetErrorText);
      end;
    finally
      Task.Free;
    end;
  finally
    Validator.Free;
  end;
end;
```

#### 4.2.5 TTaskStatistics - Analytics

**Generating Statistics:**

```pascal
var
  Stats: TTaskStatistics;
  Data: TTaskStatisticsData;
  Priority: TTaskPriority;
begin
  Stats := TTaskStatistics.Create(TaskMgr.GetAllTasks);
  try
    Data := Stats.Calculate;
    
    WriteLn('Total Tasks: ', Data.TotalTasks);
    WriteLn('Completed: ', Data.CompletedTasks);
    WriteLn('In Progress: ', Data.InProgressTasks);
    WriteLn('Overdue: ', Data.OverdueTasks);
    WriteLn('Completion Rate: ', Data.CompletionRate:0:2, '%');
    WriteLn('Avg Completion Days: ', Data.AverageCompletionDays:0:1);
    
    WriteLn('Tasks by Priority:');
    for Priority := Low(TTaskPriority) to High(TTaskPriority) do
      WriteLn('  ', GetEnumName(TypeInfo(TTaskPriority), Ord(Priority)), ': ', 
              Data.TasksByPriority[Priority]);
  finally
    Stats.Free;
  end;
end;
```

### 4.3 Storage API

#### 4.3.1 ITaskStorage Interface

All storage implementations follow this interface:

```pascal
type
  ITaskStorage = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    
    // Load all tasks from storage
    function LoadTasks(TaskList: TTaskList): Boolean;
    
    // Save all tasks to storage
    function SaveTasks(TaskList: TTaskList): Boolean;
    
    // Check if storage file exists
    function Exists: Boolean;
    
    // Get last error message
    function GetLastError: string;
    
    // Backup current storage
    function CreateBackup: Boolean;
    
    // Set storage file path
    procedure SetFilePath(const APath: string);
    function GetFilePath: string;
    
    property FilePath: string read GetFilePath write SetFilePath;
    property LastError: string read GetLastError;
  end;
```

#### 4.3.2 Using Different Storage Formats

**JSON Storage (Default):**

```pascal
var
  Storage: ITaskStorage;
  Tasks: TTaskList;
begin
  Storage := TJSONTaskStorage.Create('tasks.json');
  Tasks := TTaskList.Create;
  try
    if Storage.LoadTasks(Tasks) then
      WriteLn('Loaded ', Tasks.Count, ' tasks')
    else
      WriteLn('Error: ', Storage.LastError);
  finally
    Tasks.Free;
  end;
end;
```

**XML Storage:**

```pascal
Storage := TXMLTaskStorage.Create('tasks.xml');
// Use same interface as JSON storage
```

**CSV Storage (for import/export):**

```pascal
Storage := TCSVTaskStorage.Create('tasks.csv');
// Use same interface - ideal for Excel integration
```

### 4.4 Common Usage Patterns

#### 4.4.1 Complete CRUD Example

```pascal
program TaskManagerExample;

uses
  TaskManager, TaskModel, TaskList, TaskStorageJSON;

var
  Manager: TTaskManager;
  NewTask: TTask;
  AllTasks: TTaskList;
  i: Integer;
  
begin
  // Initialize manager
  Manager := TTaskManager.Create('myapp_tasks.json', TJSONTaskStorage);
  try
    // Load existing tasks
    if not Manager.LoadTasks then
      WriteLn('Starting with empty task list');
    
    // CREATE: Add new task
    NewTask := Manager.CreateTask(
      'Buy groceries',
      'Milk, bread, eggs, coffee',
      tpNormal,
      tcShopping
    );
    NewTask.DueDate := Now + 1; // Tomorrow
    NewTask.Tags.Add('errands');
    
    // READ: Get all tasks
    AllTasks := Manager.GetAllTasks;
    try
      WriteLn('Total tasks: ', AllTasks.Count);
      for i := 0 to AllTasks.Count - 1 do
        WriteLn('  - ', AllTasks[i].Title);
    finally
      AllTasks.Free;
    end;
    
    // UPDATE: Change task status
    Manager.UpdateTaskStatus(NewTask.ID, tsCompleted);
    
    // DELETE: Remove completed tasks
    Manager.DeleteCompletedTasks;
    
    // Save changes
    if Manager.SaveTasks then
      WriteLn('Tasks saved successfully')
    else
      WriteLn('Error saving tasks');
      
  finally
    Manager.Free;
  end;
end.
```

#### 4.4.2 Task Filtering and Searching

```pascal
procedure ShowHighPriorityOverdueTasks(Manager: TTaskManager);
var
  Filter: TTaskFilter;
  Criteria: TTaskFilterCriteria;
  Results: TTaskList;
  Task: TTask;
begin
  Filter := TTaskFilter.Create(Manager.GetAllTasks);
  try
    Criteria.Reset;
    Criteria.FilterByPriority := True;
    Criteria.PriorityFilter := tpHigh;
    
    Results := Filter.ApplyFilter(Criteria);
    try
      // Further filter for overdue
      for i := Results.Count - 1 downto 0 do
      begin
        if not Results[i].IsOverdue then
          Results.Delete(i);
      end;
      
      WriteLn('High priority overdue tasks: ', Results.Count);
      for Task in Results do
        WriteLn('  [OVERDUE] ', Task.Title, ' - Due: ', 
                DateToStr(Task.DueDate));
    finally
      Results.Free;
    end;
  finally
    Filter.Free;
  end;
end;
```

#### 4.4.3 Batch Operations

```pascal
procedure MarkAllWorkTasksAsHighPriority(Manager: TTaskManager);
var
  WorkTasks: TTaskList;
  Task: TTask;
begin
  WorkTasks := Manager.GetTasksByCategory(tcWork);
  try
    for Task in WorkTasks do
    begin
      Task.Priority := tpHigh;
      Manager.UpdateTask(Task);
    end;
    Manager.SaveTasks;
  finally
    WorkTasks.Free;
  end;
end;
```

#### 4.4.4 Event Handling (Optional)

```pascal
type
  TTaskChangeEvent = procedure(Sender: TObject; Task: TTask) of object;

// In your application
procedure TMyApp.OnTaskCreated(Sender: TObject; Task: TTask);
begin
  WriteLn('New task created: ', Task.Title);
  // Update UI, send notification, etc.
end;

procedure TMyApp.OnTaskCompleted(Sender: TObject; Task: TTask);
begin
  WriteLn('Task completed: ', Task.Title);
  // Show celebration, update statistics, etc.
end;

// Register events
Manager.OnTaskCreated := @OnTaskCreated;
Manager.OnTaskCompleted := @OnTaskCompleted;
```

### 4.5 Error Handling

The library uses exceptions for critical errors and return values for expected failures:

```pascal
try
  Manager := TTaskManager.Create('tasks.json', TJSONTaskStorage);
  try
    // Operations that might fail gracefully
    if not Manager.LoadTasks then
      WriteLn('Could not load tasks: ', Manager.LastError);
    
    // Operations that might raise exceptions
    try
      Task := Manager.GetTaskByID('invalid-id');
      if Task = nil then
        WriteLn('Task not found');
    except
      on E: ETaskNotFoundException do
        WriteLn('Error: ', E.Message);
    end;
    
  finally
    Manager.Free;
  end;
except
  on E: Exception do
    WriteLn('Fatal error: ', E.Message);
end;
```

### 4.6 Thread Safety Considerations

The library is **not thread-safe by default**. For multi-threaded applications:

```pascal
// Option 1: Use separate TTaskManager instances per thread
// Option 2: Implement your own synchronization

var
  TaskManagerLock: TCriticalSection;

TaskManagerLock := TCriticalSection.Create;
try
  TaskManagerLock.Enter;
  try
    // Perform task operations
    Manager.CreateTask('Thread-safe task');
    Manager.SaveTasks;
  finally
    TaskManagerLock.Leave;
  end;
finally
  TaskManagerLock.Free;
end;
```

### 4.7 Memory Management

The library follows Object Pascal memory management conventions:

- **TTaskManager**: Owns the internal task list and storage objects
- **TTaskList**: Owns task objects when `OwnsObjects = True` (default)
- **Returned TTask objects**: Caller owns and must free
- **Returned TTaskList objects**: Caller owns and must free

```pascal
var
  Task: TTask;
  Tasks: TTaskList;
begin
  // Task returned by CreateTask is managed by TaskManager
  Task := Manager.CreateTask('Test');
  // Don't free Task - it's in Manager's list
  
  // Task returned by Clone must be freed
  Task := OriginalTask.Clone;
  try
    // Use cloned task
  finally
    Task.Free;
  end;
  
  // TTaskList returned by GetAllTasks must be freed
  Tasks := Manager.GetAllTasks;
  try
    // Use tasks
  finally
    Tasks.Free; // This frees the list but not the task objects (they're in Manager)
  end;
end;
```

