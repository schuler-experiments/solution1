
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

```
┌─────────────────────────────────────────────────────────────┐
│  ┌─────────────────────────────────────────────────────┐   │
│  │         Business Logic Layer                         │   │
│  │  • TTaskManager (CRUD operations)                    │   │
│  │  • TTaskFilter (filtering & searching)               │   │
│  │  • TTaskValidator (validation logic)                 │   │
│  │  • TTaskStatistics (analytics & reporting)           │   │
│  │  • TTaskDependencyManager (dependency management)    │   │  ← NEW
  │  • TRecurrenceEngine (recurring task generation)       │   │  ← NEW
│  └─────────────────────────────────────────────────────┘   │
```


### 1.4 Core Components Overview

1. **Data Model Layer**: Defines the task entity and related data structures
2. **Business Logic Layer**: Implements task management operations, filtering, and validation
3. **Persistence Layer**: Handles loading and saving tasks in various formats
4. **Utility Layer**: Provides helper functions for date/time, string manipulation, etc.
5. **Recurrence Engine Layer**: Generates task instances from recurring task patterns and manages recurrence schedules
6. **Task Dependency Management Layer**: Manages relationships between tasks, detects circular dependencies, and provides dependency graph operations

**Recurrence Engine Layer**: Generates task instances from recurring task patterns and manages recurrence schedules. This layer handles various recurrence types (daily, weekly, monthly, yearly) and calculates next occurrence dates based on complex recurrence rules.

**Task Dependency Management Layer**: Manages relationships between tasks, detects circular dependencies, and provides dependency graph operations.


### 1.5 Design Patterns Used

- **Repository Pattern**: For data access abstraction (ITaskStorage interface)
- **Strategy Pattern**: For different persistence implementations
- **Factory Pattern**: For creating storage instances based on configuration
- **Template Method Pattern**: For recurrence pattern calculation algorithms with customizable date calculation strategies
- **Observer Pattern**: For task change notifications (optional event system)
- **Singleton Pattern**: For TaskManager instance management (optional)

---


### 1.6 Dependency Management and Injection Patterns

#### 1.6.1 Dependency Injection Philosophy

The library follows **Constructor Injection** as the primary dependency injection pattern, enabling loose coupling and testability. This approach aligns with Object Pascal's strong typing while avoiding the complexity of full DI frameworks.

**Key Principles:**
- Dependencies are explicitly declared in constructor parameters
- No hidden dependencies or global state (except where absolutely necessary)
- Interface-based dependencies for swappable implementations
- Default implementations provided for convenience

#### 1.6.2 Dependency Injection in Practice

**Example: TTaskManager with Storage Dependency**

```pascal
type
  TTaskManager = class
  private
    FStorage: ITaskStorage;
    FTaskList: TTaskList;
  public
    // Constructor injection - explicit dependency
    constructor Create(AStorage: ITaskStorage);
    destructor Destroy; override;
    
    // Factory method for convenience with default storage
    class function CreateWithJSONStorage(const AFileName: string): TTaskManager;
  end;

implementation

constructor TTaskManager.Create(AStorage: ITaskStorage);
begin
  inherited Create;
  if AStorage = nil then
    raise ETaskManagerException.Create('Storage cannot be nil');
  FStorage := AStorage;
  FTaskList := TTaskList.Create;
end;

class function TTaskManager.CreateWithJSONStorage(const AFileName: string): TTaskManager;
begin
  Result := TTaskManager.Create(TJSONTaskStorage.Create(AFileName));
end;
```

**Benefits:**
- Easy to test with mock storage implementations
- Clear dependency graph visible in code
- Supports runtime strategy switching
- No framework overhead

#### 1.6.3 Dependency Graph

```
TTaskManager
    ├── ITaskStorage (required) ──┬── TJSONTaskStorage
    │                             ├── TXMLTaskStorage
    │                             └── TCSVTaskStorage
    ├── TTaskList (owned)
    │   └── TTask[] (owned items)
    └── TTaskValidator (optional)

TTaskFilter
    └── TTaskList (reference, not owned)

TTaskStatistics
    └── TTaskList (reference, not owned)

TTask
    ├── No external dependencies
    └── Self-contained value object

TTaskValidator
    └── No external dependencies
    └── Pure validation logic
```

#### 1.6.4 Ownership and Lifecycle Management

**Clear Ownership Rules:**

1. **TTaskManager Owns:**
   - Its storage instance (ITaskStorage)
   - The task list (TTaskList)
   - All tasks within the list (TTask instances)

2. **TTaskList Owns:**
   - All TTask instances added to it
   - Responsible for freeing tasks on destruction

3. **Filters and Statistics Do NOT Own:**
   - They receive read-only or reference access to TTaskList
   - They never free the task list or its contents

**Example Lifecycle:**

```pascal
procedure DemoLifecycle;
var
  Manager: TTaskManager;
  Storage: ITaskStorage;
  Task: TTask;
begin
  // Create storage (will be owned by manager)
  Storage := TJSONTaskStorage.Create('tasks.json');
  
  // Create manager (takes ownership of storage)
  Manager := TTaskManager.Create(Storage);
  try
    // Create task (ownership transfers to manager on AddTask)
    Task := TTask.Create;
    Task.Title := 'Example Task';
    Manager.AddTask(Task);  // Manager now owns Task
    
    // Load from storage
    Manager.LoadTasks;
    
    // Do work...
    
    // Save to storage
    Manager.SaveTasks;
  finally
    Manager.Free;  // Frees storage, task list, and all tasks
  end;
end;
```

### 1.7 Interface Design and Segregation Principles

#### 1.7.1 Interface Segregation Principle (ISP)

The library applies ISP by creating focused, role-specific interfaces rather than monolithic ones. This prevents clients from depending on methods they don't use.

**Storage Interface Segregation:**

```pascal
// Base storage interface - minimal required operations
type
  ITaskStorage = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function LoadTasks: TTaskList;
    procedure SaveTasks(ATaskList: TTaskList);
  end;

// Extended interface for storages that support streaming
type
  ITaskStorageStream = interface(ITaskStorage)
    ['{12345678-1234-1234-1234-123456789013}']
    function LoadFromStream(AStream: TStream): TTaskList;
    procedure SaveToStream(ATaskList: TTaskList; AStream: TStream);
  end;

// Extended interface for storages with query capabilities
type
  ITaskStorageQuery = interface(ITaskStorage)
    ['{12345678-1234-1234-1234-123456789014}']
    function FindTasksByStatus(AStatus: TTaskStatus): TTaskList;
    function FindTasksByPriority(APriority: TTaskPriority): TTaskList;
  end;
```

#### 1.7.2 Interface Usage Patterns

**Pattern 1: Basic Storage (All implementations must support)**

```pascal
var
  Storage: ITaskStorage;
  TaskList: TTaskList;
begin
  Storage := TJSONTaskStorage.Create('tasks.json');
  TaskList := Storage.LoadTasks;
  try
    // Work with tasks
  finally
    Storage.SaveTasks(TaskList);
    TaskList.Free;
  end;
end;
```

**Pattern 2: Feature Detection (Optional capabilities)**

```pascal
var
  Storage: ITaskStorage;
  StreamStorage: ITaskStorageStream;
  FileStream: TFileStream;
begin
  Storage := TJSONTaskStorage.Create('tasks.json');
  
  // Check if storage supports streaming
  if Supports(Storage, ITaskStorageStream, StreamStorage) then
  begin
    FileStream := TFileStream.Create('export.json', fmCreate);
    try
      StreamStorage.SaveToStream(TaskList, FileStream);
    finally
      FileStream.Free;
    end;
  end;
end;
```

#### 1.7.3 Interface vs Abstract Classes

**When to Use Interfaces:**
- Multiple inheritance needed
- Duck typing / feature detection required
- COM compatibility desired
- Pure contracts without implementation

**When to Use Abstract Classes:**
- Shared implementation logic exists
- Constructor requirements needed
- State management required
- Template method pattern applies

**Example: Abstract Base Class for Storage**

```pascal
type
  // Abstract base with common functionality
  TAbstractTaskStorage = class(TInterfacedObject, ITaskStorage)
  protected
    FFileName: string;
    function ValidateTaskList(ATaskList: TTaskList): Boolean; virtual;
    procedure LogOperation(const AOperation: string); virtual;
  public
    constructor Create(const AFileName: string);
    
    // ITaskStorage - must be implemented by descendants
    function LoadTasks: TTaskList; virtual; abstract;
    procedure SaveTasks(ATaskList: TTaskList); virtual; abstract;
  end;
  
  // Concrete implementation
  TJSONTaskStorage = class(TAbstractTaskStorage)
  public
    function LoadTasks: TTaskList; override;
    procedure SaveTasks(ATaskList: TTaskList); override;
  end;
```

### 1.8 Error Handling Architecture

#### 1.8.1 Exception Hierarchy

The library defines a clear exception hierarchy rooted in a custom base exception:

```pascal
type
  // Base exception for all task manager errors
  ETaskManagerException = class(Exception)
  private
    FErrorCode: Integer;
    FContext: string;
  public
    constructor Create(const AMessage: string); overload;
    constructor Create(const AMessage: string; AErrorCode: Integer); overload;
    constructor Create(const AMessage, AContext: string; AErrorCode: Integer); overload;
    property ErrorCode: Integer read FErrorCode;
    property Context: string read FContext;
  end;

  // Data validation errors
  ETaskValidationException = class(ETaskManagerException)
  private
    FFieldName: string;
    FInvalidValue: string;
  public
    constructor Create(const AMessage, AFieldName, AInvalidValue: string);
    property FieldName: string read FFieldName;
    property InvalidValue: string read FInvalidValue;
  end;

  // Storage and persistence errors
  ETaskStorageException = class(ETaskManagerException)
  private
    FFileName: string;
  public
    constructor Create(const AMessage, AFileName: string);
    property FileName: string read FFileName;
  end;

  // Task not found errors
  ETaskNotFoundException = class(ETaskManagerException)
  private
    FTaskID: Integer;
  public
    constructor Create(ATaskID: Integer);
    property TaskID: Integer read FTaskID;
  end;

  // Concurrency and locking errors
  ETaskConcurrencyException = class(ETaskManagerException);
```

#### 1.8.2 Error Handling Strategy

**Defensive Programming at Boundaries:**

```pascal
function TTaskManager.GetTaskByID(AID: Integer): TTask;
begin
  // Validate input at public API boundary
  if AID <= 0 then
    raise ETaskManagerException.Create('Invalid task ID', 'GetTaskByID', ERR_INVALID_ID);
  
  Result := FTaskList.FindByID(AID);
  
  // Explicit error for not found
  if Result = nil then
    raise ETaskNotFoundException.Create(AID);
end;
```

**Resource Protection with Try-Finally:**

```pascal
procedure TTaskManager.SaveTasks;
var
  TempList: TTaskList;
begin
  if FStorage = nil then
    raise ETaskManagerException.Create('Storage not configured');
  
  TempList := nil;
  try
    // Create temporary copy for thread safety
    TempList := FTaskList.Clone;
    
    // Attempt save
    try
      FStorage.SaveTasks(TempList);
    except
      on E: Exception do
      begin
        // Wrap and re-raise with context
        raise ETaskStorageException.Create(
          'Failed to save tasks: ' + E.Message,
          FStorage.FileName
        );
      end;
    end;
  finally
    TempList.Free;
  end;
end;
```

**Validation with Result Objects (Alternative to Exceptions):**

```pascal
type
  TValidationResult = record
    IsValid: Boolean;
    ErrorMessage: string;
    ErrorCode: Integer;
    FieldName: string;
  end;

function TTaskValidator.ValidateTask(ATask: TTask): TValidationResult;
begin
  Result.IsValid := True;
  Result.ErrorMessage := '';
  
  // Non-exceptional validation
  if Trim(ATask.Title) = '' then
  begin
    Result.IsValid := False;
    Result.ErrorMessage := 'Title cannot be empty';
    Result.FieldName := 'Title';
    Result.ErrorCode := ERR_EMPTY_TITLE;
    Exit;
  end;
  
  if Length(ATask.Title) > 255 then
  begin
    Result.IsValid := False;
    Result.ErrorMessage := 'Title too long (max 255 characters)';
    Result.FieldName := 'Title';
    Result.ErrorCode := ERR_TITLE_TOO_LONG;
    Exit;
  end;
end;
```

#### 1.8.3 Error Recovery Patterns

**Graceful Degradation:**

```pascal
function TTaskManager.LoadTasksWithRecovery: Boolean;
begin
  Result := False;
  try
    FTaskList.Clear;
    FTaskList.Free;
    FTaskList := FStorage.LoadTasks;
    Result := True;
  except
    on E: ETaskStorageException do
    begin
      // Log error but continue with empty list
      LogError('Failed to load tasks: ' + E.Message);
      FTaskList := TTaskList.Create;
      Result := False;  // Indicate failure but don't crash
    end;
  end;
end;
```

**Retry Logic:**

```pascal
function TTaskManager.SaveTasksWithRetry(MaxRetries: Integer = 3): Boolean;
var
  Attempt: Integer;
  LastError: string;
begin
  Result := False;
  LastError := '';
  
  for Attempt := 1 to MaxRetries do
  begin
    try
      FStorage.SaveTasks(FTaskList);
      Result := True;
      Exit;  // Success
    except
      on E: Exception do
      begin
        LastError := E.Message;
        if Attempt < MaxRetries then
          Sleep(1000 * Attempt);  // Exponential backoff
      end;
    end;
  end;
  
  // All retries failed
  if not Result then
    raise ETaskStorageException.Create(
      Format('Save failed after %d attempts. Last error: %s', 
        [MaxRetries, LastError]),
      FStorage.FileName
    );
end;
```

### 1.9 Concurrency Model and Thread Safety

#### 1.9.1 Thread Safety Design

**Default Stance: Not Thread-Safe by Default**

The library is designed for single-threaded use by default to avoid performance overhead. Thread safety is opt-in through specific mechanisms.

**Rationale:**
- Most Pascal applications are single-threaded
- Synchronization overhead impacts performance
- Explicit opt-in makes threading model clear
- Simpler implementation and maintenance

#### 1.9.2 Thread-Safe Wrapper Pattern

For applications requiring thread-safe access, a wrapper class provides synchronized access:

```pascal
type
  TThreadSafeTaskManager = class
  private
    FManager: TTaskManager;
    FLock: TCriticalSection;
  public
    constructor Create(AStorage: ITaskStorage);
    destructor Destroy; override;
    
    // Thread-safe operations
    function AddTask(ATask: TTask): Integer;
    function GetTaskByID(AID: Integer): TTask;
    function UpdateTask(ATask: TTask): Boolean;
    function DeleteTask(AID: Integer): Boolean;
    procedure LoadTasks;
    procedure SaveTasks;
    
    // Batch operations with single lock
    procedure ExecuteBatch(ABatchProc: TTaskBatchProc);
  end;

implementation

constructor TThreadSafeTaskManager.Create(AStorage: ITaskStorage);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FManager := TTaskManager.Create(AStorage);
end;

destructor TThreadSafeTaskManager.Destroy;
begin
  FManager.Free;
  FLock.Free;
  inherited;
end;

function TThreadSafeTaskManager.AddTask(ATask: TTask): Integer;
begin
  FLock.Enter;
  try
    Result := FManager.AddTask(ATask);
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeTaskManager.ExecuteBatch(ABatchProc: TTaskBatchProc);
begin
  FLock.Enter;
  try
    ABatchProc(FManager);
  finally
    FLock.Leave;
  end;
end;
```

#### 1.9.3 Read-Write Lock Pattern for High Concurrency

For scenarios with many reads and few writes:

```pascal
type
  TTaskManagerRWLock = class
  private
    FManager: TTaskManager;
    FRWLock: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create(AStorage: ITaskStorage);
    destructor Destroy; override;
    
    // Read operations (multiple readers allowed)
    function GetTaskByID(AID: Integer): TTask;
    function GetTaskCount: Integer;
    function FilterTasks(ACriteria: TTaskFilterCriteria): TTaskList;
    
    // Write operations (exclusive access)
    function AddTask(ATask: TTask): Integer;
    function UpdateTask(ATask: TTask): Boolean;
    function DeleteTask(AID: Integer): Boolean;
  end;

implementation

function TTaskManagerRWLock.GetTaskByID(AID: Integer): TTask;
begin
  FRWLock.BeginRead;
  try
    Result := FManager.GetTaskByID(AID);
  finally
    FRWLock.EndRead;
  end;
end;

function TTaskManagerRWLock.AddTask(ATask: TTask): Integer;
begin
  FRWLock.BeginWrite;
  try
    Result := FManager.AddTask(ATask);
  finally
    FRWLock.EndWrite;
  end;
end;
```

#### 1.9.4 Immutable Task Pattern for Concurrent Access

For advanced scenarios, immutable tasks eliminate synchronization needs:

```pascal
type
  // Immutable task - all properties read-only after creation
  TImmutableTask = class
  private
    FID: Integer;
    FTitle: string;
    FDescription: string;
    FStatus: TTaskStatus;
    FPriority: TTaskPriority;
    FCreatedDate: TDateTime;
    // ... other fields
  public
    constructor Create(AID: Integer; const ATitle, ADescription: string;
      AStatus: TTaskStatus; APriority: TTaskPriority);
    
    // Factory method for modifications (returns new instance)
    function WithStatus(ANewStatus: TTaskStatus): TImmutableTask;
    function WithPriority(ANewPriority: TTaskPriority): TImmutableTask;
    
    // Read-only properties
    property ID: Integer read FID;
    property Title: string read FTitle;
    property Status: TTaskStatus read FStatus;
    // ... other read-only properties
  end;
```

#### 1.9.5 Concurrency Best Practices

**Guidelines for Library Users:**

1. **Single-Threaded Default**: Use `TTaskManager` directly in single-threaded applications
2. **Wrapper for Multi-Threading**: Use `TThreadSafeTaskManager` when multiple threads access the same manager
3. **Separate Instances**: Create separate `TTaskManager` instances per thread (with separate storage) when possible
4. **Batch Operations**: Use `ExecuteBatch` to perform multiple operations under a single lock
5. **Avoid Long-Running Operations**: Don't hold locks during I/O or complex calculations

**Example: Worker Thread Pattern**

```pascal
type
  TTaskProcessorThread = class(TThread)
  private
    FManager: TThreadSafeTaskManager;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TThreadSafeTaskManager);
  end;

procedure TTaskProcessorThread.Execute;
var
  Task: TTask;
  ProcessedIDs: array of Integer;
begin
  while not Terminated do
  begin
    // Get pending tasks (short lock)
    FManager.ExecuteBatch(
      procedure(M: TTaskManager)
      var
        I: Integer;
      begin
        SetLength(ProcessedIDs, 0);
        for I := 0 to M.TaskCount - 1 do
        begin
          if M.Tasks[I].Status = tsInProgress then
          begin
            SetLength(ProcessedIDs, Length(ProcessedIDs) + 1);
            ProcessedIDs[High(ProcessedIDs)] := M.Tasks[I].ID;
          end;
        end;
      end
    );
    
    // Process tasks (no lock held)
    for TaskID in ProcessedIDs do
    begin
      Task := FManager.GetTaskByID(TaskID);
      // ... do work ...
      FManager.UpdateTask(Task);
    end;
    
    Sleep(1000);
  end;
end;
```

### 1.10 Class Relationships and Dependencies

#### 1.10.1 Detailed Dependency Graph

```
┌─────────────────────────────────────────────────────────────────┐
│                         Client Code                              │
│                    (Application Layer)                           │
└────────────┬────────────────────────────────────────────────────┘
             │
             │ creates and uses
             ▼
┌─────────────────────────────────────────────────────────────────┐
│                      TTaskManager                                │
│  ┌──────────────────────────────────────────────────────┐      │
│  │ Dependencies (Constructor Injection):                 │      │
│  │  • ITaskStorage (required, owned)                     │      │
│  │                                                        │      │
│  │ Internal Components (Created and Owned):              │      │
│  │  • TTaskList (owned)                                  │      │
│  │  • TTaskValidator (optional, owned)                   │      │
│  │                                                        │      │
│  │ External References (Not Owned):                      │      │
│  │  • None - fully self-contained                        │      │
│  └──────────────────────────────────────────────────────┘      │
└────┬────────────────┬─────────────────┬────────────────────────┘
     │                │                 │
     │ uses           │ uses            │ uses
     ▼                ▼                 ▼
┌──────────┐    ┌─────────────┐  ┌──────────────┐
│ITaskStorage│   │  TTaskList  │  │TTaskValidator│
│(interface)│    │             │  │              │
└────┬─────┘    └──────┬──────┘  └──────────────┘
     │                 │
     │ implemented by  │ contains
     │                 │
     ▼                 ▼
┌──────────────┐  ┌────────┐
│Storage Impls │  │ TTask  │
│ • JSON       │  │        │
│ • XML        │  │        │
│ • CSV        │  │        │
└──────────────┘  └────────┘

┌─────────────────────────────────────────────────────────────────┐
│                      TTaskFilter                                 │
│  ┌──────────────────────────────────────────────────────┐      │
│  │ Dependencies (Method Parameters):                     │      │
│  │  • TTaskList (reference, not owned)                   │      │
│  │                                                        │      │
│  │ No Owned Components                                   │      │
│  │ Stateless utility class                               │      │
│  └──────────────────────────────────────────────────────┘      │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                    TTaskStatistics                               │
│  ┌──────────────────────────────────────────────────────┐      │
│  │ Dependencies (Method Parameters):                     │      │
│  │  • TTaskList (reference, not owned)                   │      │
│  │                                                        │      │
│  │ No Owned Components                                   │      │
│  │ Stateless utility class                               │      │
│  └──────────────────────────────────────────────────────┘      │
└─────────────────────────────────────────────────────────────────┘
```

#### 1.10.2 Dependency Injection Points

**Primary Injection Point: TTaskManager Constructor**

```pascal
// Dependency: ITaskStorage (required)
constructor TTaskManager.Create(AStorage: ITaskStorage);

// Factory methods for common configurations
class function TTaskManager.CreateWithJSONStorage(const AFileName: string): TTaskManager;
class function TTaskManager.CreateWithXMLStorage(const AFileName: string): TTaskManager;
class function TTaskManager.CreateWithCSVStorage(const AFileName: string): TTaskManager;
```

**Secondary Injection Point: Storage Implementations**

```pascal
// Each storage implementation requires a file path
constructor TJSONTaskStorage.Create(const AFileName: string);
constructor TXMLTaskStorage.Create(const AFileName: string);
constructor TCSVTaskStorage.Create(const AFileName: string);
```

#### 1.10.3 Compile-Time Dependency Order

Units must be compiled in this order to respect dependencies:

```
Level 1 (No dependencies):
  • TaskTypes.pas           - Type definitions
  • TaskExceptions.pas      - Exception classes

Level 2 (Depends on Level 1):
  • TaskModel.pas           - TTask class
  • TaskUtils.pas           - Utility functions

Level 3 (Depends on Levels 1-2):
  • TaskList.pas            - TTaskList class
  • TaskStorage.pas         - ITaskStorage interface

Level 4 (Depends on Levels 1-3):
  • TaskStorageJSON.pas     - JSON storage implementation
  • TaskStorageXML.pas      - XML storage implementation
  • TaskStorageCSV.pas      - CSV storage implementation
  • TaskValidator.pas       - Validation logic
  • TaskFilter.pas          - Filtering logic
  • TaskStatistics.pas      - Statistics logic

Level 5 (Depends on Levels 1-4):
  • TaskManager.pas         - Main manager class
```

#### 1.10.4 Runtime Object Graph Example

```
Application Start:
  │
  ├─ Create Storage: TJSONTaskStorage('tasks.json')
  │    │
  │    └─ Opens file handle (managed internally)
  │
  ├─ Create Manager: TTaskManager(storage)
  │    │
  │    ├─ Stores reference to storage (owned)
  │    │
  │    └─ Creates TTaskList (owned)
  │         │
  │         └─ Initially empty
  │
  ├─ Load Tasks: Manager.LoadTasks()
  │    │
  │    ├─ Calls Storage.LoadTasks()
  │    │    │
  │    │    └─ Returns new TTaskList with TTask instances
  │    │
  │    └─ Replaces internal task list
  │         │
  │         └─ Old list and tasks are freed
  │
  ├─ Add Task: Manager.AddTask(task)
  │    │
  │    ├─ Validates task
  │    │
  │    └─ Adds to internal TTaskList
  │         │
  │         └─ TTaskList now owns the task
  │
  ├─ Filter Tasks: TTaskFilter.FilterByStatus(Manager.TaskList, tsCompleted)
  │    │
  │    ├─ Receives reference to task list (not owned)
  │    │
  │    └─ Returns new TTaskList with matching tasks (caller owns result)
  │
  ├─ Save Tasks: Manager.SaveTasks()
  │    │
  │    └─ Calls Storage.SaveTasks(internal task list)
  │         │
  │         └─ Storage reads task list (doesn't own it)
  │
  └─ Cleanup: Manager.Free
       │
       ├─ Frees internal TTaskList
       │    │
       │    └─ TTaskList frees all TTask instances
       │
       └─ Frees Storage instance
            │
            └─ Storage closes file handles
```

### 1.11 Object Pascal Specific Patterns and Idioms

#### 1.11.1 Reference Counting with Interfaces

The library uses interfaces for the storage layer to enable automatic reference counting:

```pascal
procedure Example;
var
  Manager: TTaskManager;
begin
  // Storage is interface - automatically reference counted
  Manager := TTaskManager.Create(TJSONTaskStorage.Create('tasks.json'));
  try
    // Use manager
  finally
    Manager.Free;  // Frees manager, which releases storage interface
    // Storage is automatically freed when reference count reaches zero
  end;
end;
```

#### 1.11.2 Properties with Lazy Initialization

```pascal
type
  TTaskManager = class
  private
    FValidator: TTaskValidator;
    function GetValidator: TTaskValidator;
  public
    property Validator: TTaskValidator read GetValidator;
  end;

function TTaskManager.GetValidator: TTaskValidator;
begin
  if FValidator = nil then
    FValidator := TTaskValidator.Create;
  Result := FValidator;
end;
```

#### 1.11.3 Enumerator Pattern for Task Lists

```pascal
type
  TTaskList = class
  public
    function GetEnumerator: TTaskListEnumerator;
  end;
  
  TTaskListEnumerator = class
  private
    FList: TTaskList;
    FIndex: Integer;
  public
    constructor Create(AList: TTaskList);
    function MoveNext: Boolean;
    property Current: TTask read GetCurrent;
  end;

// Usage with for-in loop
var
  Task: TTask;
begin
  for Task in TaskList do
    WriteLn(Task.Title);
end;
```

#### 1.11.4 Class Helpers for Extension

```pascal
type
  TTaskHelper = class helper for TTask
  public
    function IsOverdue: Boolean;
    function DaysUntilDue: Integer;
    function ToJSON: string;
    procedure FromJSON(const AJSON: string);
  end;

implementation

function TTaskHelper.IsOverdue: Boolean;
begin
  Result := (DueDate < Now) and (Status <> tsCompleted);
end;
```

#### 1.11.5 Advanced Generic Patterns (FPC 3.2+)

```pascal
type
  // Generic task collection with type-safe filtering
  TGenericTaskList<T: TTask> = class(specialize TObjectList<T>)
  public
    function FilterByPredicate(APredicate: specialize TFunc<T, Boolean>): TGenericTaskList<T>;
  end;

function TGenericTaskList<T>.FilterByPredicate(APredicate: specialize TFunc<T, Boolean>): TGenericTaskList<T>;
var
  Item: T;
begin
  Result := TGenericTaskList<T>.Create(False);  // Don't own items
  for Item in Self do
    if APredicate(Item) then
      Result.Add(Item);
end;
```

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


### 2.12 Task Dependency Module (`TaskDependency.pas`)

#### Purpose
Manages relationships and dependencies between tasks, enabling complex task workflows where tasks can block or depend on other tasks.

#### Responsibilities
- Create and manage task dependencies
- Validate dependencies (detect circular dependencies)
- Query dependency graphs (get blockers, get blocked tasks)
- Provide dependency traversal methods
- Support cascade operations (optional)
- Calculate dependency paths and impact analysis

#### Key Classes
- **`TTaskDependency`**: Represents a single dependency relationship
- **`TTaskDependencyManager`**: Manages all dependencies for a task collection
- **`TDependencyGraph`**: Provides graph analysis and traversal operations

#### Dependencies
- Uses: `TaskModel.pas`, `TaskList.pas`, `TaskExceptions.pas`
- Used by: `TaskManager.pas`, `TaskStorage.pas` implementations

#### Design Considerations
- **Graph Structure**: Uses adjacency list for efficient traversal
- **Circular Detection**: Implements cycle detection using depth-first search
- **Performance**: O(V+E) for most graph operations where V=tasks, E=dependencies
- **Thread Safety**: Can be wrapped in thread-safe decorator if needed
- **Persistence**: Dependencies are serialized with task data

### 2.13 Task Recurrence Module (`TaskRecurrence.pas`)

**Purpose**: Manages recurring task patterns and generates task instances based on recurrence rules.

**Key Classes**:
- `TRecurrencePattern`: Enumeration of recurrence types
- `TRecurrenceRule`: Configuration for task recurrence
- `TRecurrenceEngine`: Generates task instances from patterns
- `TRecurrenceException`: Handles skipped occurrences

**Responsibilities**:
- Define recurrence patterns (daily, weekly, monthly, yearly, custom)
- Calculate next occurrence dates based on rules
- Handle exception dates (skip specific occurrences)
- Generate task instances for date ranges
- Validate recurrence rules
- Support complex recurrence patterns (e.g., "every 2nd Tuesday")
- Integrate with task validation and persistence layers

**Dependencies**:
- Uses: `TaskModel.pas`, `TaskList.pas`, `SysUtils`, `DateUtils`, `Classes`
- Used by: `TaskManager.pas`, `TaskFilter.pas`, `TaskStorage.pas` implementations

**Design Considerations**:
- **Immutability**: Recurrence rules are immutable once created for thread safety
- **Performance**: Efficient date calculation algorithms (O(1) for simple patterns)
- **Flexibility**: Support for complex recurrence patterns and exceptions
- **Validation**: Comprehensive validation of recurrence rules before use
- **Storage**: Recurrence rules serialize/deserialize with task data
- **Instance Generation**: Lazy generation strategy to avoid creating unnecessary instances
- **Template Pattern**: Uses template method pattern for different recurrence calculations
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


### 3.9 TDependencyType Enumeration

```pascal
type
  TDependencyType = (
    dtFinishToStart,    // Task B cannot start until Task A finishes (default)
    dtStartToStart,     // Task B cannot start until Task A starts
    dtFinishToFinish,   // Task B cannot finish until Task A finishes
    dtStartToFinish,    // Task B cannot finish until Task A starts (rare)
    dtRelatedTo         // Informational relationship, no blocking
  );
```

**Usage Notes:**
- `dtFinishToStart`: Most common dependency type (classical "blocks")
- `dtStartToStart`: Both tasks must begin together
- `dtFinishToFinish`: Both tasks must complete together
- `dtStartToFinish`: Task B must wait for Task A to start before finishing
- `dtRelatedTo`: Link tasks that are related but don't block each other

### 3.10 TTaskDependency Class

```pascal
type
  TTaskDependency = class
  private
    FSourceTaskID: Integer;      // The task that depends
    FTargetTaskID: Integer;      // The task being depended upon
    FDependencyType: TDependencyType;
    FCreatedAt: TDateTime;
    FDescription: string;        // Optional description of the dependency
    FLagTime: Integer;           // Lag time in hours (can be negative for lead time)
  public
    constructor Create(ASourceID, ATargetID: Integer; 
                      ADepType: TDependencyType = dtFinishToStart);
    destructor Destroy; override;
    
    // Properties
    property SourceTaskID: Integer read FSourceTaskID;
    property TargetTaskID: Integer read FTargetTaskID;
    property DependencyType: TDependencyType read FDependencyType write FDependencyType;
    property CreatedAt: TDateTime read FCreatedAt;
    property Description: string read FDescription write FDescription;
    property LagTime: Integer read FLagTime write FLagTime;
    
    // Methods
    function ToString: string;
    function Clone: TTaskDependency;
    function IsValid: Boolean;
  end;
```

**Key Features:**
- **Immutable IDs**: Source and Target IDs cannot change after creation
- **Lag Time**: Supports lag (positive) and lead (negative) time in hours
- **Description**: Optional human-readable explanation
- **Timestamp**: Tracks when dependency was created

### 3.11 TTaskDependencyManager Class

```pascal
type
  TTaskDependencyManager = class
  private
    FDependencies: TObjectList<TTaskDependency>;
    FTaskList: TTaskList;  // Reference to managed tasks
    
    function FindDependency(SourceID, TargetID: Integer): TTaskDependency;
    function HasCycleDFS(TaskID: Integer; Visited, RecStack: TList<Integer>): Boolean;
  public
    constructor Create(ATaskList: TTaskList);
    destructor Destroy; override;
    
    // Dependency CRUD operations
    function AddDependency(SourceID, TargetID: Integer; 
                          DepType: TDependencyType = dtFinishToStart;
                          const Description: string = ''): TTaskDependency;
    function RemoveDependency(SourceID, TargetID: Integer): Boolean;
    function GetDependency(SourceID, TargetID: Integer): TTaskDependency;
    function UpdateDependency(SourceID, TargetID: Integer; 
                             NewType: TDependencyType): Boolean;
    
    // Dependency queries
    function GetDependencies(TaskID: Integer; 
                           IncludeSource: Boolean = True;
                           IncludeTarget: Boolean = True): TList<TTaskDependency>;
    function GetBlockedBy(TaskID: Integer): TList<Integer>;  // Tasks blocking this one
    function GetBlocking(TaskID: Integer): TList<Integer>;   // Tasks this one blocks
    function GetAllDependencies: TList<TTaskDependency>;
    
    // Validation
    function WouldCreateCycle(SourceID, TargetID: Integer): Boolean;
    function HasCircularDependencies: Boolean;
    function ValidateDependencies: TStringList;  // Returns list of validation errors
    
    // Graph analysis
    function GetDependencyPath(FromTaskID, ToTaskID: Integer): TList<Integer>;
    function GetTaskDepth(TaskID: Integer): Integer;  // Depth in dependency tree
    function GetRootTasks: TList<Integer>;  // Tasks with no dependencies
    function GetLeafTasks: TList<Integer>;  // Tasks that don't block anything
    function CanTaskStart(TaskID: Integer): Boolean;  // Check if all dependencies met
    
    // Cascade operations
    procedure CascadeDelete(TaskID: Integer; DeleteDependents: Boolean);
    procedure CascadeComplete(TaskID: Integer; CompleteDependents: Boolean);
    
    // Utility
    function GetDependencyCount: Integer;
    function ExportToDOT: string;  // Export to GraphViz DOT format
    procedure Clear;
  end;
```

**Key Methods Explained:**

- **`AddDependency`**: Creates new dependency with cycle detection
- **`WouldCreateCycle`**: Checks if adding dependency would create circular reference
- **`GetDependencyPath`**: Finds path between two tasks (if exists)
- **`CanTaskStart`**: Determines if all blocking tasks are completed
- **`CascadeDelete`**: Optionally deletes dependent tasks when deleting a task
- **`ExportToDOT`**: Generates GraphViz visualization of dependency graph

### 3.12 TDependencyGraph Helper Class

```pascal
type
  TDependencyGraph = class
  private
    FAdjacencyList: TDictionary<Integer, TList<Integer>>;
    FReverseList: TDictionary<Integer, TList<Integer>>;
    
    procedure BuildGraph(Dependencies: TList<TTaskDependency>);
    function TopologicalSortUtil(TaskID: Integer; Visited: TDictionary<Integer, Boolean>;
                                 Stack: TStack<Integer>): Boolean;
  public
    constructor Create(Dependencies: TList<TTaskDependency>);
    destructor Destroy; override;
    
    // Graph algorithms
    function TopologicalSort: TList<Integer>;  // Returns ordered task list
    function DetectCycles: TList<TList<Integer>>;  // Returns all cycles found
    function GetCriticalPath: TList<Integer>;  // Longest path through graph
    function GetShortestPath(FromID, ToID: Integer): TList<Integer>;
    
    // Graph properties
    function GetInDegree(TaskID: Integer): Integer;
    function GetOutDegree(TaskID: Integer): Integer;
    function IsAcyclic: Boolean;
    function GetComponentCount: Integer;  // Number of disconnected subgraphs
  end;
```

### 3.13 TRecurrencePattern Enumeration

Defines the types of recurrence patterns supported by the task manager.

```pascal
type
  TRecurrencePattern = (
    rpNone,           // No recurrence (one-time task) - default
    rpDaily,          // Repeats every day(s)
    rpWeekly,         // Repeats every week(s) on specific days
    rpMonthly,        // Repeats every month(s) on specific day
    rpYearly,         // Repeats every year(s) on specific date
    rpCustom          // Custom pattern with specific interval
  );
```

**Usage Notes:**
- `rpNone`: Default for all tasks - indicates a one-time task
- `rpDaily`: Can be configured with interval (e.g., every 2 days)
- `rpWeekly`: Requires `DaysOfWeek` to be set (e.g., Monday and Friday)
- `rpMonthly`: Uses `DayOfMonth` (1-31, or 0 for last day of month)
- `rpYearly`: Uses `MonthOfYear` (1-12) and `DayOfMonth`
- `rpCustom`: For advanced patterns not covered by standard types

### 3.14 TDayOfWeek Set Type

Defines days of the week for weekly recurrence patterns.

```pascal
type
  TDayOfWeek = (
    dwSunday,
    dwMonday,
    dwTuesday,
    dwWednesday,
    dwThursday,
    dwFriday,
    dwSaturday
  );
  
  TDaysOfWeek = set of TDayOfWeek;
```

**Example Usage:**
```pascal
// Every weekday (Monday-Friday)
Rule.DaysOfWeek := [dwMonday, dwTuesday, dwWednesday, dwThursday, dwFriday];

// Every weekend
Rule.DaysOfWeek := [dwSaturday, dwSunday];

// Every Monday, Wednesday, and Friday
Rule.DaysOfWeek := [dwMonday, dwWednesday, dwFriday];
```

### 3.15 TRecurrenceRule Class

Encapsulates all configuration for a recurring task pattern.

```pascal
type
  TRecurrenceRule = class
  private
    FPattern: TRecurrencePattern;
    FInterval: Integer;              // Repeat every N days/weeks/months/years
    FStartDate: TDateTime;           // When recurrence begins
    FEndDate: TDateTime;             // When recurrence ends (0 = no end)
    FMaxOccurrences: Integer;        // Maximum number of occurrences (0 = unlimited)
    FDaysOfWeek: TDaysOfWeek;        // For weekly recurrence
    FDayOfMonth: Integer;            // For monthly/yearly (1-31, 0=last day)
    FMonthOfYear: Integer;           // For yearly recurrence (1-12)
    FExceptionDates: TList<TDateTime>; // Dates to skip
    FGeneratedCount: Integer;        // Internal: count of generated instances
    
    procedure ValidatePattern;
    procedure ValidateInterval;
    procedure ValidateDates;
  public
    constructor Create(APattern: TRecurrencePattern);
    destructor Destroy; override;
    
    // Properties
    property Pattern: TRecurrencePattern read FPattern write FPattern;
    property Interval: Integer read FInterval write FInterval;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property EndDate: TDateTime read FEndDate write FEndDate;
    property MaxOccurrences: Integer read FMaxOccurrences write FMaxOccurrences;
    property DaysOfWeek: TDaysOfWeek read FDaysOfWeek write FDaysOfWeek;
    property DayOfMonth: Integer read FDayOfMonth write FDayOfMonth;
    property MonthOfYear: Integer read FMonthOfYear write FMonthOfYear;
    property ExceptionDates: TList<TDateTime> read FExceptionDates;
    property GeneratedCount: Integer read FGeneratedCount write FGeneratedCount;
    
    // Validation
    function IsValid: Boolean;
    function GetValidationErrors: TStringList;
    
    // Exception date management
    procedure AddExceptionDate(const ADate: TDateTime);
    procedure RemoveExceptionDate(const ADate: TDateTime);
    function IsExceptionDate(const ADate: TDateTime): Boolean;
    procedure ClearExceptionDates;
    
    // Utility methods
    function Clone: TRecurrenceRule;
    function Equals(ARule: TRecurrenceRule): Boolean;
    function ToString: string; override;
    function ToHumanReadable: string;  // "Every 2 weeks on Monday and Friday"
    
    // Serialization support
    function ToJSON: string;
    procedure FromJSON(const AJSON: string);
  end;
```

**Key Features:**
- **Immutable Core**: Pattern type cannot change after creation
- **Flexible Configuration**: Supports simple and complex recurrence rules
- **Validation**: Comprehensive validation before use
- **Exception Handling**: Skip specific occurrences
- **Human Readable**: Convert rules to natural language
- **Serialization**: Full JSON support for persistence

**Validation Rules:**
- `Interval` must be >= 1
- `StartDate` must be set and valid
- `EndDate` (if set) must be after `StartDate`
- `MaxOccurrences` (if set) must be >= 1
- For weekly patterns, at least one day must be selected
- For monthly patterns, `DayOfMonth` must be 0-31
- For yearly patterns, `MonthOfYear` must be 1-12

### 3.16 TRecurrenceEngine Class

Calculates occurrence dates and generates task instances from recurrence rules.

```pascal
type
  TRecurrenceEngine = class
  private
    FRule: TRecurrenceRule;
    
    // Pattern-specific calculation methods
    function CalculateNextDaily(const AFromDate: TDateTime): TDateTime;
    function CalculateNextWeekly(const AFromDate: TDateTime): TDateTime;
    function CalculateNextMonthly(const AFromDate: TDateTime): TDateTime;
    function CalculateNextYearly(const AFromDate: TDateTime): TDateTime;
    function CalculateNextCustom(const AFromDate: TDateTime): TDateTime;
    
    // Helper methods
    function IsDateValid(const ADate: TDateTime): Boolean;
    function AdjustForExceptions(const ADate: TDateTime): TDateTime;
    function GetLastDayOfMonth(Year, Month: Word): Word;
    function GetNextWeekdayOccurrence(const AFromDate: TDateTime; 
                                     ADayOfWeek: TDayOfWeek): TDateTime;
  public
    constructor Create(ARule: TRecurrenceRule);
    destructor Destroy; override;
    
    // Core recurrence calculation
    function GetNextOccurrence(const AFromDate: TDateTime): TDateTime;
    function GetPreviousOccurrence(const AFromDate: TDateTime): TDateTime;
    function GetOccurrencesBetween(const AStartDate, AEndDate: TDateTime): TList<TDateTime>;
    function GetNextNOccurrences(const AFromDate: TDateTime; N: Integer): TList<TDateTime>;
    
    // Validation and queries
    function WillOccurOn(const ADate: TDateTime): Boolean;
    function GetOccurrenceCount(const AStartDate, AEndDate: TDateTime): Integer;
    function HasEnded(const AAsOfDate: TDateTime): Boolean;
    function GetEffectiveEndDate: TDateTime;  // Calculates end based on max occurrences
    
    // Task generation from template
    function GenerateTaskInstance(const ATemplate: TTask; 
                                  const AOccurrenceDate: TDateTime): TTask;
    function GenerateTaskInstances(const ATemplate: TTask;
                                   const AStartDate, AEndDate: TDateTime): TTaskList;
    
    // Statistics
    function GetAverageInterval: Double;  // Average days between occurrences
    function GetTotalOccurrences: Integer;  // Total if end date is set
    
    property Rule: TRecurrenceRule read FRule;
  end;
```

**Design Pattern**: Uses **Template Method Pattern** where `GetNextOccurrence` delegates to pattern-specific calculation methods.

**Performance Characteristics:**
- Daily/Weekly patterns: O(1) calculation
- Monthly/Yearly patterns: O(1) to O(12) depending on constraints
- Exception date checking: O(n) where n = number of exceptions
- Instance generation: O(k) where k = number of occurrences

**Thread Safety**: Read-only operations are thread-safe. Not safe for concurrent rule modification.

### 3.17 Enhanced TTask Class for Recurrence

Extensions to the existing `TTask` class to support recurring tasks.

**Additional Fields:**

```pascal
type
  TTask = class
  private
    // ... existing fields ...
    
    // Recurrence-related fields
    FIsRecurring: Boolean;
    FRecurrenceRule: TRecurrenceRule;
    FParentRecurringTaskID: Integer;  // 0 if template, >0 if instance
    FRecurrenceInstanceDate: TDateTime; // The occurrence date for this instance
    FRecurrenceSeriesID: string;      // Unique ID for the recurrence series
    
    procedure SetRecurrenceRule(ARule: TRecurrenceRule);
  public
    // ... existing methods ...
    
    // Recurrence methods
    function IsRecurringTask: Boolean;      // Is this a recurring template?
    function IsRecurrenceInstance: Boolean; // Is this a generated instance?
    procedure SetRecurrence(ARule: TRecurrenceRule);
    procedure ClearRecurrence;
    function GetRecurrenceRule: TRecurrenceRule;
    function GetNextOccurrence: TDateTime;
    function GetPreviousOccurrence: TDateTime;
    
    // Properties
    property IsRecurring: Boolean read FIsRecurring;
    property RecurrenceRule: TRecurrenceRule read FRecurrenceRule write SetRecurrenceRule;
    property ParentRecurringTaskID: Integer read FParentRecurringTaskID write FParentRecurringTaskID;
    property RecurrenceInstanceDate: TDateTime read FRecurrenceInstanceDate write FRecurrenceInstanceDate;
    property RecurrenceSeriesID: string read FRecurrenceSeriesID write FRecurrenceSeriesID;
  end;
```

**Usage Patterns:**

```pascal
// Creating a recurring task template
var
  Template: TTask;
  Rule: TRecurrenceRule;
begin
  Template := TTask.Create('Weekly team meeting');
  
  Rule := TRecurrenceRule.Create(rpWeekly);
  Rule.Interval := 1;
  Rule.DaysOfWeek := [dwMonday];
  Rule.StartDate := Date;
  
  Template.SetRecurrence(Rule);
  // Template.IsRecurring = True
  // Template.IsRecurrenceInstance = False
  // Template.ParentRecurringTaskID = 0
end;

// Checking if a task is a generated instance
if MyTask.IsRecurrenceInstance then
  WriteLn('This task is part of series: ', MyTask.RecurrenceSeriesID);
```

**Storage Considerations:**
- Template tasks store the full `TRecurrenceRule`
- Instance tasks store only `ParentRecurringTaskID`, `RecurrenceInstanceDate`, and `RecurrenceSeriesID`
- Instances can be edited independently of the template
- Deleting a template can optionally delete all instances
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


---


### 4.8 Task Dependency Management API

##### 4.8.1 Creating Dependencies

```pascal
uses
  TaskManager, TaskDependency, TaskModel;

var
  Manager: TTaskManager;
  DepManager: TTaskDependencyManager;
  Task1, Task2, Task3: TTask;
  Dependency: TTaskDependency;
begin
  Manager := TTaskManager.Create;
  try
    // Create tasks
    Task1 := Manager.CreateTask('Design Database Schema', tpHigh);
    Task2 := Manager.CreateTask('Implement Database Layer', tpHigh);
    Task3 := Manager.CreateTask('Write Unit Tests', tpMedium);
    
    // Create dependency manager
    DepManager := TTaskDependencyManager.Create(Manager.Tasks);
    try
      // Task2 depends on Task1 (Task1 must finish before Task2 starts)
      Dependency := DepManager.AddDependency(
        Task2.ID,           // Source: depends on
        Task1.ID,           // Target: dependency
        dtFinishToStart,    // Type
        'Database implementation requires completed schema'
      );
      
      // Task3 depends on Task2
      DepManager.AddDependency(Task3.ID, Task2.ID, dtFinishToStart);
      
      WriteLn('Dependencies created successfully');
      WriteLn(Format('Total dependencies: %d', [DepManager.GetDependencyCount]));
    finally
      DepManager.Free;
    end;
  finally
    Manager.Free;
  end;
end;
```

##### 4.8.2 Checking Dependencies Before Task Operations

```pascal
function CanCompleteTask(Manager: TTaskManager; DepManager: TTaskDependencyManager; 
                         TaskID: Integer): Boolean;
var
  BlockingTasks: TList<Integer>;
  BlockerID: Integer;
  BlockerTask: TTask;
begin
  Result := True;
  
  // Get all tasks that block this one
  BlockingTasks := DepManager.GetBlockedBy(TaskID);
  try
    for BlockerID in BlockingTasks do
    begin
      BlockerTask := Manager.GetTaskByID(BlockerID);
      if (BlockerTask <> nil) and (BlockerTask.Status <> tsCompleted) then
      begin
        WriteLn(Format('Task %d is blocked by incomplete task: %s', 
                      [TaskID, BlockerTask.Title]));
        Result := False;
      end;
    end;
  finally
    BlockingTasks.Free;
  end;
end;

// Usage
if CanCompleteTask(Manager, DepManager, MyTask.ID) then
  Manager.UpdateTaskStatus(MyTask.ID, tsCompleted)
else
  WriteLn('Cannot complete task - dependencies not met');
```

##### 4.8.3 Preventing Circular Dependencies

```pascal
procedure SafeAddDependency(DepManager: TTaskDependencyManager; 
                           SourceID, TargetID: Integer);
begin
  // Check for circular dependency before adding
  if DepManager.WouldCreateCycle(SourceID, TargetID) then
  begin
    raise ETaskDependencyException.CreateFmt(
      'Cannot add dependency: would create circular reference between tasks %d and %d',
      [SourceID, TargetID]
    );
  end;
  
  DepManager.AddDependency(SourceID, TargetID);
  WriteLn('Dependency added successfully');
end;
```

##### 4.8.4 Finding Dependency Paths

```pascal
procedure ShowDependencyPath(DepManager: TTaskDependencyManager; 
                            Manager: TTaskManager;
                            FromTaskID, ToTaskID: Integer);
var
  Path: TList<Integer>;
  TaskID: Integer;
  Task: TTask;
  I: Integer;
begin
  Path := DepManager.GetDependencyPath(FromTaskID, ToTaskID);
  try
    if Path.Count = 0 then
    begin
      WriteLn('No dependency path exists');
      Exit;
    end;
    
    WriteLn('Dependency path:');
    for I := 0 to Path.Count - 1 do
    begin
      TaskID := Path[I];
      Task := Manager.GetTaskByID(TaskID);
      if Task <> nil then
      begin
        if I > 0 then Write(' -> ');
        Write(Format('%s (ID: %d)', [Task.Title, TaskID]));
      end;
    end;
    WriteLn;
  finally
    Path.Free;
  end;
end;
```

##### 4.8.5 Getting Task Execution Order

```pascal
procedure ShowExecutionOrder(DepManager: TTaskDependencyManager; 
                            Manager: TTaskManager);
var
  Graph: TDependencyGraph;
  OrderedTasks: TList<Integer>;
  TaskID: Integer;
  Task: TTask;
  Dependencies: TList<TTaskDependency>;
begin
  Dependencies := DepManager.GetAllDependencies;
  try
    Graph := TDependencyGraph.Create(Dependencies);
    try
      OrderedTasks := Graph.TopologicalSort;
      try
        WriteLn('Recommended task execution order:');
        for TaskID in OrderedTasks do
        begin
          Task := Manager.GetTaskByID(TaskID);
          if Task <> nil then
            WriteLn(Format('%d. %s', [OrderedTasks.IndexOf(TaskID) + 1, Task.Title]));
        end;
      finally
        OrderedTasks.Free;
      end;
    finally
      Graph.Free;
    end;
  finally
    Dependencies.Free;
  end;
end;
```

##### 4.8.6 Visualizing Dependencies

```pascal
procedure ExportDependencyGraph(DepManager: TTaskDependencyManager; 
                               const Filename: string);
var
  DOTContent: string;
  F: TextFile;
begin
  DOTContent := DepManager.ExportToDOT;
  
  AssignFile(F, Filename);
  try
    Rewrite(F);
    WriteLn(F, DOTContent);
  finally
    CloseFile(F);
  end;
  
  WriteLn(Format('Dependency graph exported to: %s', [Filename]));
  WriteLn('Use: dot -Tpng dependencies.dot -o dependencies.png');
end;
```

### 4.9 Recurring Tasks API

The Recurring Tasks API provides comprehensive functionality for creating, managing, and generating instances from recurring task patterns.

#### 4.9.1 Creating a Simple Recurring Task

```pascal
uses
  TaskManager, TaskModel, TaskRecurrence;

var
  RecurringTask: TTask;
  Rule: TRecurrenceRule;
begin
  // Create the task template
  RecurringTask := TTask.Create('Daily standup meeting');
  RecurringTask.Description := 'Team sync meeting at 9:00 AM';
  RecurringTask.Priority := tpNormal;
  RecurringTask.Category := tcWork;
  RecurringTask.EstimatedMinutes := 15;
  
  // Create a daily recurrence rule
  Rule := TRecurrenceRule.Create(rpDaily);
  Rule.Interval := 1;  // Every day
  Rule.StartDate := Date;  // Start today
  Rule.EndDate := Date + 90;  // End in 90 days
  
  // Apply the rule to the task
  RecurringTask.SetRecurrence(Rule);
  
  // Add to task manager
  TaskMgr.AddTask(RecurringTask);
  
  WriteLn('Created recurring task: ', RecurringTask.Title);
  WriteLn('Pattern: ', Rule.ToHumanReadable);
end;
```

#### 4.9.2 Creating a Weekly Recurring Task

```pascal
var
  WeeklyTask: TTask;
  WeeklyRule: TRecurrenceRule;
begin
  WeeklyTask := TTask.Create('Team retrospective');
  WeeklyTask.Description := 'Weekly team retrospective and planning';
  WeeklyTask.Priority := tpHigh;
  WeeklyTask.EstimatedMinutes := 60;
  
  // Every Friday
  WeeklyRule := TRecurrenceRule.Create(rpWeekly);
  WeeklyRule.Interval := 1;
  WeeklyRule.DaysOfWeek := [dwFriday];
  WeeklyRule.StartDate := Date;
  WeeklyRule.MaxOccurrences := 52;  // One year (52 weeks)
  
  WeeklyTask.SetRecurrence(WeeklyRule);
  TaskMgr.AddTask(WeeklyTask);
end;
```

#### 4.9.3 Creating a Complex Weekly Pattern

```pascal
var
  Task: TTask;
  Rule: TRecurrenceRule;
begin
  // Every weekday (Monday through Friday)
  Task := TTask.Create('Check emails');
  
  Rule := TRecurrenceRule.Create(rpWeekly);
  Rule.Interval := 1;
  Rule.DaysOfWeek := [dwMonday, dwTuesday, dwWednesday, dwThursday, dwFriday];
  Rule.StartDate := Date;
  // No end date - continues indefinitely
  
  Task.SetRecurrence(Rule);
  TaskMgr.AddTask(Task);
end;
```

#### 4.9.4 Creating Monthly Recurring Tasks

```pascal
var
  MonthlyTask: TTask;
  MonthlyRule: TRecurrenceRule;
begin
  // First Monday of every month
  MonthlyTask := TTask.Create('Monthly status report');
  
  MonthlyRule := TRecurrenceRule.Create(rpMonthly);
  MonthlyRule.Interval := 1;  // Every month
  MonthlyRule.DayOfMonth := 1;  // 1st day of month
  MonthlyRule.StartDate := EncodeDate(2024, 1, 1);
  MonthlyRule.MaxOccurrences := 12;  // One year
  
  MonthlyTask.SetRecurrence(MonthlyRule);
  TaskMgr.AddTask(MonthlyTask);
  
  // Last day of every month
  MonthlyTask := TTask.Create('Month-end closing');
  MonthlyRule := TRecurrenceRule.Create(rpMonthly);
  MonthlyRule.Interval := 1;
  MonthlyRule.DayOfMonth := 0;  // 0 = last day of month
  MonthlyRule.StartDate := Date;
  
  MonthlyTask.SetRecurrence(MonthlyRule);
  TaskMgr.AddTask(MonthlyTask);
end;
```

#### 4.9.5 Generating Task Instances

```pascal
uses
  TaskRecurrence;

var
  Template: TTask;
  Engine: TRecurrenceEngine;
  Instances: TTaskList;
  Instance: TTask;
  StartDate, EndDate: TDateTime;
begin
  // Get the recurring task template
  Template := TaskMgr.GetTaskByID(123);
  
  if Template.IsRecurring then
  begin
    // Create recurrence engine
    Engine := TRecurrenceEngine.Create(Template.RecurrenceRule);
    try
      // Generate instances for the next 30 days
      StartDate := Date;
      EndDate := Date + 30;
      
      Instances := Engine.GenerateTaskInstances(Template, StartDate, EndDate);
      try
        WriteLn(Format('Generated %d task instances', [Instances.Count]));
        
        // Add each instance to the task manager
        for Instance in Instances do
        begin
          TaskMgr.AddTask(Instance);
          WriteLn(Format('  Instance for %s', [DateToStr(Instance.RecurrenceInstanceDate)]));
        end;
      finally
        Instances.Free;
      end;
    finally
      Engine.Free;
    end;
  end;
end;
```

#### 4.9.6 Working with Exception Dates

```pascal
var
  Task: TTask;
  Rule: TRecurrenceRule;
  HolidayDate, VacationStart, VacationEnd: TDateTime;
  CurrentDate: TDateTime;
begin
  Task := TaskMgr.GetTaskByID(456);
  
  if Task.IsRecurring then
  begin
    Rule := Task.RecurrenceRule;
    
    // Skip specific holidays
    HolidayDate := EncodeDate(2024, 12, 25);  // Christmas
    Rule.AddExceptionDate(HolidayDate);
    
    HolidayDate := EncodeDate(2024, 1, 1);  // New Year's Day
    Rule.AddExceptionDate(HolidayDate);
    
    // Skip a range of dates (vacation period)
    VacationStart := EncodeDate(2024, 7, 1);
    VacationEnd := EncodeDate(2024, 7, 14);
    CurrentDate := VacationStart;
    
    while CurrentDate <= VacationEnd do
    begin
      Rule.AddExceptionDate(CurrentDate);
      CurrentDate := CurrentDate + 1;
    end;
    
    // Update the task
    TaskMgr.UpdateTask(Task);
    
    WriteLn(Format('Added %d exception dates', [Rule.ExceptionDates.Count]));
  end;
end;
```

#### 4.9.7 Querying Recurrence Information

```pascal
var
  Task: TTask;
  Engine: TRecurrenceEngine;
  NextOccurrence: TDateTime;
  OccurrenceCount: Integer;
  Occurrences: TList<TDateTime>;
  OccDate: TDateTime;
begin
  Task := TaskMgr.GetTaskByID(789);
  
  if Task.IsRecurring then
  begin
    WriteLn('Task: ', Task.Title);
    WriteLn('Pattern: ', Task.RecurrenceRule.ToHumanReadable);
    
    // Get next occurrence
    NextOccurrence := Task.GetNextOccurrence;
    WriteLn('Next occurrence: ', DateTimeToStr(NextOccurrence));
    
    // Create engine for advanced queries
    Engine := TRecurrenceEngine.Create(Task.RecurrenceRule);
    try
      // Check if task will occur on a specific date
      if Engine.WillOccurOn(EncodeDate(2024, 12, 31)) then
        WriteLn('Task will occur on Dec 31, 2024');
      
      // Count occurrences in a date range
      OccurrenceCount := Engine.GetOccurrenceCount(Date, Date + 365);
      WriteLn(Format('Will occur %d times in the next year', [OccurrenceCount]));
      
      // Get next 10 occurrences
      Occurrences := Engine.GetNextNOccurrences(Date, 10);
      try
        WriteLn('Next 10 occurrences:');
        for OccDate in Occurrences do
          WriteLn('  - ', DateToStr(OccDate));
      finally
        Occurrences.Free;
      end;
      
      // Check if recurrence has ended
      if Engine.HasEnded(Date) then
        WriteLn('This recurrence has ended');
    finally
      Engine.Free;
    end;
  end;
end;
```

#### 4.9.8 Modifying Recurring Task Instances

```pascal
var
  Instance: TTask;
  Template: TTask;
begin
  // Get a specific instance
  Instance := TaskMgr.GetTaskByID(1001);
  
  if Instance.IsRecurrenceInstance then
  begin
    WriteLn('This is an instance of series: ', Instance.RecurrenceSeriesID);
    WriteLn('Instance date: ', DateToStr(Instance.RecurrenceInstanceDate));
    WriteLn('Parent template ID: ', Instance.ParentRecurringTaskID);
    
    // Modify this instance independently
    Instance.Status := tsCompleted;
    Instance.Notes := 'Completed early this week';
    TaskMgr.UpdateTask(Instance);
    
    // The template task remains unchanged
    Template := TaskMgr.GetTaskByID(Instance.ParentRecurringTaskID);
    WriteLn('Template status: ', GetEnumName(TypeInfo(TTaskStatus), Ord(Template.Status)));
  end;
end;
```

#### 4.9.9 Deleting Recurring Tasks

```pascal
var
  Template: TTask;
  Instances: TTaskList;
  Instance: TTask;
begin
  Template := TaskMgr.GetTaskByID(500);
  
  if Template.IsRecurring then
  begin
    // Option 1: Delete only the template (keep instances)
    TaskMgr.DeleteTask(Template.ID);
    
    // Option 2: Delete template and all future instances
    Template := TaskMgr.GetTaskByID(500);
    Instances := TaskMgr.GetTasksByFilter(
      function(T: TTask): Boolean
      begin
        Result := (T.ParentRecurringTaskID = Template.ID) and
                  (T.RecurrenceInstanceDate >= Date);
      end
    );
    try
      for Instance in Instances do
        TaskMgr.DeleteTask(Instance.ID);
      TaskMgr.DeleteTask(Template.ID);
    finally
      Instances.Free;
    end;
    
    WriteLn('Deleted recurring task and all future instances');
  end;
end;
```

#### 4.9.10 Converting Human-Readable Recurrence Descriptions

```pascal
var
  Rule: TRecurrenceRule;
  Description: string;
begin
  // Create various rules and get human-readable descriptions
  
  // Daily
  Rule := TRecurrenceRule.Create(rpDaily);
  Rule.Interval := 1;
  Description := Rule.ToHumanReadable;
  // Output: "Every day"
  
  Rule.Interval := 3;
  Description := Rule.ToHumanReadable;
  // Output: "Every 3 days"
  
  // Weekly
  Rule := TRecurrenceRule.Create(rpWeekly);
  Rule.Interval := 1;
  Rule.DaysOfWeek := [dwMonday, dwFriday];
  Description := Rule.ToHumanReadable;
  // Output: "Every week on Monday and Friday"
  
  Rule.Interval := 2;
  Rule.DaysOfWeek := [dwWednesday];
  Description := Rule.ToHumanReadable;
  // Output: "Every 2 weeks on Wednesday"
  
  // Monthly
  Rule := TRecurrenceRule.Create(rpMonthly);
  Rule.Interval := 1;
  Rule.DayOfMonth := 15;
  Description := Rule.ToHumanReadable;
  // Output: "Every month on day 15"
  
  Rule.DayOfMonth := 0;
  Description := Rule.ToHumanReadable;
  // Output: "Every month on the last day"
  
  // Yearly
  Rule := TRecurrenceRule.Create(rpYearly);
  Rule.MonthOfYear := 12;
  Rule.DayOfMonth := 25;
  Description := Rule.ToHumanReadable;
  // Output: "Every year on December 25"
  
  WriteLn('Recurrence pattern: ', Description);
end;
```

#### 4.9.11 Best Practices for Recurring Tasks

**1. Instance Generation Strategy:**
```pascal
// Generate instances for a rolling window (recommended)
procedure GenerateUpcomingInstances(const Template: TTask; DaysAhead: Integer = 90);
var
  Engine: TRecurrenceEngine;
  Instances: TTaskList;
begin
  Engine := TRecurrenceEngine.Create(Template.RecurrenceRule);
  try
    // Generate instances for the next N days
    Instances := Engine.GenerateTaskInstances(Template, Date, Date + DaysAhead);
    try
      // Add only new instances (check if they don't already exist)
      for Instance in Instances do
      begin
        if not TaskMgr.InstanceExists(Instance.RecurrenceSeriesID, 
                                      Instance.RecurrenceInstanceDate) then
          TaskMgr.AddTask(Instance);
      end;
    finally
      Instances.Free;
    end;
  finally
    Engine.Free;
  end;
end;
```

**2. Cleanup Old Instances:**
```pascal
// Delete completed instances older than 30 days
procedure CleanupOldInstances;
var
  AllTasks: TTaskList;
  Task: TTask;
  CutoffDate: TDateTime;
begin
  CutoffDate := Date - 30;
  AllTasks := TaskMgr.GetAllTasks;
  try
    for Task in AllTasks do
    begin
      if Task.IsRecurrenceInstance and
         (Task.Status = tsCompleted) and
         (Task.CompletedDate < CutoffDate) then
      begin
        TaskMgr.DeleteTask(Task.ID);
      end;
    end;
  finally
    AllTasks.Free;
  end;
end;
```

**3. Validation Before Creating:**
```pascal
function CreateValidatedRecurringTask(const Title: string; 
                                     Rule: TRecurrenceRule): TTask;
var
  Errors: TStringList;
begin
  Result := nil;
  
  // Validate the recurrence rule first
  if not Rule.IsValid then
  begin
    Errors := Rule.GetValidationErrors;
    try
      WriteLn('Invalid recurrence rule:');
      for Error in Errors do
        WriteLn('  - ', Error);
    finally
      Errors.Free;
    end;
    Exit;
  end;
  
  // Create the task
  Result := TTask.Create(Title);
  Result.SetRecurrence(Rule);
end;
```
## 5. User Interface Designs

### 5.1 Overview

This is a **library component** with no built-in user interface. The Free Pascal Task Manager Library is designed to be UI-agnostic and can be integrated into any type of application:

- Console applications
- GUI applications (Lazarus LCL, fpGUI, MSEgui, Custom VCL)
- Web services (fphttpapp, Brook Framework)
- Background services/daemons
- Mobile applications (when compiled with appropriate Free Pascal targets)

### 5.2 Integration Guidelines

When integrating this library into a UI application, developers should:

1. **Instantiate TTaskManager** in the application's main form or controller
2. **Bind task data** to UI components (grids, lists, trees) by retrieving task lists
3. **Call library methods** in response to user actions (buttons, menus)
4. **Handle events** (if implemented) to update the UI when tasks change
5. **Display validation results** from TTaskValidator in user-friendly messages

### 5.3 Example UI Integration Pattern

```pascal
// Example: Lazarus LCL integration
type
  TMainForm = class(TForm)
    TaskGrid: TStringGrid;
    BtnAddTask: TButton;
    BtnDeleteTask: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnAddTaskClick(Sender: TObject);
    procedure BtnDeleteTaskClick(Sender: TObject);
    procedure RefreshTaskGrid;
  private
    FTaskManager: TTaskManager;
  end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FTaskManager := TTaskManager.Create('tasks.json');
  FTaskManager.LoadTasks;
  RefreshTaskGrid;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FTaskManager.Free;
end;

procedure TMainForm.RefreshTaskGrid;
var
  Tasks: TTaskList;
  I: Integer;
  Task: TTask;
begin
  Tasks := FTaskManager.GetAllTasks;
  try
    TaskGrid.RowCount := Tasks.Count + 1;
    for I := 0 to Tasks.Count - 1 do
    begin
      Task := Tasks[I];
      TaskGrid.Cells[0, I + 1] := Task.Title;
      TaskGrid.Cells[1, I + 1] := Task.Description;
      TaskGrid.Cells[2, I + 1] := GetEnumName(TypeInfo(TTaskStatus), Ord(Task.Status));
      TaskGrid.Cells[3, I + 1] := GetEnumName(TypeInfo(TTaskPriority), Ord(Task.Priority));
    end;
  finally
    Tasks.Free;
  end;
end;

procedure TMainForm.BtnAddTaskClick(Sender: TObject);
var
  Task: TTask;
begin
  Task := FTaskManager.CreateTask('New Task');
  Task.Description := 'Task description';
  Task.Priority := tpMedium;
  FTaskManager.SaveTasks;
  RefreshTaskGrid;
end;

procedure TMainForm.BtnDeleteTaskClick(Sender: TObject);
var
  TaskID: string;
begin
  if TaskGrid.Row > 0 then
  begin
    TaskID := FTaskManager.GetAllTasks[TaskGrid.Row - 1].ID;
    if FTaskManager.DeleteTask(TaskID) then
    begin
      FTaskManager.SaveTasks;
      RefreshTaskGrid;
    end;
  end;
end;
```

---

## 6. Third-Party Libraries and Services

### 6.1 Overview

The Free Pascal Task Manager Library is designed to minimize external dependencies while leveraging the robust Free Pascal standard library and widely-available units.

### 6.2 Required Free Pascal Standard Units

The library relies exclusively on units included in the Free Pascal Compiler (FPC) distribution:

| Unit | Purpose | Availability |
|------|---------|--------------|
| `Classes` | Base classes (TObject, TList, TStringList, etc.) | FPC RTL |
| `SysUtils` | System utilities, exception handling, string functions | FPC RTL |
| `DateUtils` | Date/time manipulation | FPC RTL |
| `TypInfo` | Runtime type information (RTTI) | FPC RTL |
| `fpjson` | JSON parsing and generation | FPC packages |
| `jsonparser` | JSON parsing support | FPC packages |
| `DOM` | XML Document Object Model | FPC packages |
| `XMLRead` | XML file reading | FPC packages |
| `XMLWrite` | XML file writing | FPC packages |

### 6.3 Optional Dependencies

No optional third-party libraries are required. All functionality is implemented using FPC standard units.

### 6.4 Compiler Version Requirements

- **Minimum FPC Version**: 3.0.4
- **Recommended FPC Version**: 3.2.0 or later
- **Language Mode**: `{$mode objfpc}{$H+}` (Object Pascal with long strings)

### 6.5 Platform Compatibility

The library is cross-platform and has been designed to work on:

- **Windows**: Windows 7 and later (32-bit and 64-bit)
- **Linux**: All major distributions (x86, x86_64, ARM)
- **macOS**: macOS 10.10 and later (x86_64, Apple Silicon via Rosetta)
- **FreeBSD**: 11.x and later
- **Embedded**: When compiled with appropriate FPC cross-compilers

### 6.6 External Services

This library does **not** integrate with any external services by default. It is a self-contained, offline-capable task management solution. However, developers can extend it to integrate with:

- Cloud storage services (Dropbox, Google Drive) by implementing custom `ITaskStorage` implementations
- Databases (SQLite, PostgreSQL, MySQL) via custom storage backends
- REST APIs for task synchronization
- Message queues (RabbitMQ, Redis) for distributed task management

### 6.7 Future Considerations

Potential future extensions could include:

- **Optional SQLite backend**: Using the built-in FPC SQLite3 units
- **Optional encryption**: Using FPC's built-in cryptography units
- **Optional compression**: Using FPC's built-in zlib support for compressed storage formats

---


### 6.8 Task Dependency Feature

**No additional third-party libraries required** for the Task Dependency feature. All dependency management and graph algorithms are implemented using standard Free Pascal units:
- `System.Generics.Collections` (TList, TDictionary, TStack)
- `System.Classes` (TObjectList, TStringList)


## 7. Deployment and Scaling Strategies

### 7.1 Deployment Overview

As a library component, the Free Pascal Task Manager is deployed by including its compiled units (`.ppu`, `.o`, `.a`) in consuming applications. There are several deployment approaches depending on the target application type.

### 7.2 Static Linking (Recommended)

**Approach**: Compile the library units directly into the final executable.

**Advantages**:
- Single executable file
- No external dependencies
- Faster startup time
- Simpler deployment

**Implementation**:
```pascal
// In your project's main program file or Lazarus project options
// Simply add the library units to the uses clause
program MyTaskApp;

{$mode objfpc}{$H+}

uses
  TaskModel, TaskList, TaskManager, TaskFilter, 
  TaskValidator, TaskStorage, TaskStorageJSON;

begin
  // Your application code
end.
```

**Compilation**:
```bash
fpc -O3 -XX -CX MyTaskApp.pas
```

### 7.3 Dynamic Linking (Shared Library)

**Approach**: Compile the library as a shared library (.so, .dll, .dylib) for use by multiple applications.

**Advantages**:
- Shared code between multiple applications
- Smaller individual executable sizes
- Can update library without recompiling applications

**Implementation**:
```pascal
// TaskManagerLib.lpr - Shared library project
library TaskManagerLib;

{$mode objfpc}{$H+}

uses
  TaskModel, TaskList, TaskManager, TaskFilter, 
  TaskValidator, TaskStorage, TaskStorageJSON;

// Export functions for C-style API
exports
  CreateTaskManager,
  DestroyTaskManager,
  CreateTask,
  DeleteTask,
  SaveTasks,
  LoadTasks;

begin
end.
```

**Compilation**:
```bash
fpc -O3 TaskManagerLib.lpr
```

### 7.4 Package-Based Deployment (Lazarus)

**Approach**: Create a Lazarus package (.lpk) for easy integration into Lazarus IDE projects.

**Advantages**:
- IDE integration
- Dependency management
- Easy updates via package manager
- Compile-time checking

**Package Structure**:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <Package Version="5">
    <Name Value="TaskManagerLib"/>
    <Type Value="RunTimeOnly"/>
    <CompilerOptions>
      <Version Value="11"/>
      <SearchPaths>
        <UnitOutputDirectory Value="lib/$(TargetCPU)-$(TargetOS)"/>
      </SearchPaths>
    </CompilerOptions>
    <Files Count="11">
      <Item1>
        <Filename Value="TaskModel.pas"/>
        <UnitName Value="TaskModel"/>
      </Item1>
      <!-- Additional units... -->
    </Files>
  </Package>
</CONFIG>
```

### 7.5 File System Layout

**Development Layout**:
```
TaskManagerLib/
├── src/
│   ├── TaskModel.pas
│   ├── TaskList.pas
│   ├── TaskManager.pas
│   ├── TaskFilter.pas
│   ├── TaskValidator.pas
│   ├── TaskStorage.pas
│   ├── TaskStorageJSON.pas
│   ├── TaskStorageXML.pas
│   ├── TaskStorageCSV.pas
│   ├── TaskStatistics.pas
│   └── TaskUtils.pas
├── tests/
│   ├── TestTaskModel.pas
│   ├── TestTaskManager.pas
│   └── TestAll.pas
├── examples/
│   ├── console/
│   │   └── TaskConsoleDemo.pas
│   └── gui/
│       └── TaskGUIDemo.lpr
├── docs/
│   └── software-spec.md
├── lib/
│   └── (compiled units, automatically generated)
└── TaskManagerLib.lpk
```

**Distribution Layout**:
```
TaskManagerLib-1.0.0/
├── units/
│   ├── i386-win32/
│   │   └── *.ppu, *.o
│   ├── x86_64-linux/
│   │   └── *.ppu, *.o
│   └── x86_64-darwin/
│       └── *.ppu, *.o
├── include/
│   └── *.pas (source files for reference)
├── docs/
│   ├── software-spec.md
│   └── api-reference.html
├── examples/
│   └── (example projects)
└── README.md
```

### 7.6 Scaling Strategies

#### 7.6.1 Scaling for Large Task Collections

**Challenge**: Managing thousands or millions of tasks efficiently.

**Strategies**:

1. **Lazy Loading**:
```pascal
// Don't load all tasks at once
function TTaskManager.GetTasksByDateRange(StartDate, EndDate: TDateTime): TTaskList;
begin
  Result := TTaskList.Create(True);
  // Load only tasks in the specified range from storage
end;
```

2. **Pagination**:
```pascal
function TTaskManager.GetTasksPage(PageNumber, PageSize: Integer): TTaskList;
var
  AllTasks: TTaskList;
  StartIndex, EndIndex: Integer;
begin
  Result := TTaskList.Create(False); // Don't own objects
  AllTasks := GetAllTasks;
  try
    StartIndex := PageNumber * PageSize;
    EndIndex := Min(StartIndex + PageSize - 1, AllTasks.Count - 1);
    for I := StartIndex to EndIndex do
      Result.Add(AllTasks[I]);
  finally
    AllTasks.Free;
  end;
end;
```

3. **Indexing**:
```pascal
// Implement hash-based lookup for faster searching
type
  TTaskManager = class
  private
    FTasksByID: TFPHashObjectList; // Fast ID-based lookup
    FTasksByCategory: TDictionary<TTaskCategory, TTaskList>;
  end;
```

4. **Database Backend**:
```pascal
// Implement ITaskStorage with SQLite for large datasets
type
  TTaskStorageSQLite = class(TInterfacedObject, ITaskStorage)
  public
    procedure SaveTasks(Tasks: TTaskList); override;
    function LoadTasks: TTaskList; override;
  end;
```

#### 7.6.2 Scaling for Concurrent Access

**Challenge**: Multiple processes or threads accessing the same task data.

**Strategies**:

1. **File Locking**:
```pascal
// Use OS-level file locks when saving/loading
procedure TTaskStorageJSON.SaveTasks(Tasks: TTaskList);
var
  FileHandle: THandle;
begin
  FileHandle := FileOpen(FFileName, fmOpenWrite or fmShareDenyWrite);
  try
    // Save tasks with exclusive lock
  finally
    FileClose(FileHandle);
  end;
end;
```

2. **Client-Server Architecture**:
```pascal
// Implement a task server that manages a single TTaskManager instance
// Clients communicate via TCP/IP or HTTP
type
  TTaskServer = class
  private
    FTaskManager: TTaskManager;
    FServer: TFPHTTPServer;
  public
    procedure HandleCreateTask(Req: TRequest; Res: TResponse);
    procedure HandleGetTasks(Req: TRequest; Res: TResponse);
  end;
```

3. **Message Queue Integration**:
```pascal
// Use message queues for distributed task management
// Each worker process maintains its own TTaskManager
// Synchronization happens via message passing
```

#### 7.6.3 Scaling for Storage Size

**Challenge**: Task data growing beyond available memory.

**Strategies**:

1. **Streaming API**:
```pascal
// Process tasks one at a time without loading all into memory
procedure ProcessAllTasks(Callback: TTaskCallback);
var
  F: TextFile;
  Line: string;
  Task: TTask;
begin
  AssignFile(F, 'tasks.json');
  Reset(F);
  try
    while not EOF(F) do
    begin
      ReadLn(F, Line);
      Task := ParseJSONTask(Line);
      try
        Callback(Task);
      finally
        Task.Free;
      end;
    end;
  finally
    CloseFile(F);
  end;
end;
```

2. **Archive Old Tasks**:
```pascal
// Move completed/old tasks to separate archive files
procedure TTaskManager.ArchiveCompletedTasks(BeforeDate: TDateTime);
var
  ArchiveTasks: TTaskList;
begin
  ArchiveTasks := Filter.FilterByCompletedBefore(BeforeDate);
  try
    SaveToArchive(ArchiveTasks, FormatDateTime('yyyy-mm', BeforeDate));
    DeleteTasks(ArchiveTasks);
  finally
    ArchiveTasks.Free;
  end;
end;
```

3. **Compression**:
```pascal
// Compress stored task data
uses zstream;

procedure TTaskStorageJSON.SaveTasks(Tasks: TTaskList);
var
  FileStream: TFileStream;
  CompStream: TCompressionStream;
begin
  FileStream := TFileStream.Create(FFileName, fmCreate);
  try
    CompStream := TCompressionStream.Create(clMax, FileStream);
    try
      // Write compressed JSON data
    finally
      CompStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;
```

#### 7.6.4 Scaling for Recurring Task Instances

**Challenge**: Recurring tasks can generate large numbers of instances over time, potentially impacting performance and storage.

**Solutions**:

1. **Lazy Instance Generation**
   - Generate instances only for a rolling window (e.g., next 90 days)
   - Avoid pre-generating all instances for long-running or unlimited recurrences
   - Generate on-demand as users navigate forward in time

2. **Instance Cleanup Strategy**
   - Automatically delete completed instances older than a threshold (e.g., 30 days)
   - Archive important completed instances before deletion
   - Provide manual override for instances that should be kept

3. **Template-Based Storage**
   - Store only the template task and its recurrence rule
   - Generate instances dynamically at query time when possible
   - Persist only instances that have been modified from the template

4. **Efficient Query Patterns**
   ```pascal
   // Efficient: Generate instances for visible date range only
   procedure ShowTasksForMonth(Year, Month: Word);
   var
     StartDate, EndDate: TDateTime;
     RecurringTasks: TTaskList;
     Template: TTask;
     Engine: TRecurrenceEngine;
     Instances: TTaskList;
   begin
     StartDate := EncodeDate(Year, Month, 1);
     EndDate := EndOfMonth(EncodeDate(Year, Month, 1));
     
     // Get all recurring task templates
     RecurringTasks := TaskMgr.GetRecurringTasks;
     try
       for Template in RecurringTasks do
       begin
         Engine := TRecurrenceEngine.Create(Template.RecurrenceRule);
         try
           // Generate only for this month
           Instances := Engine.GenerateTaskInstances(Template, StartDate, EndDate);
           try
             DisplayInstances(Instances);
           finally
             Instances.Free;
           end;
         finally
           Engine.Free;
         end;
       end;
     finally
       RecurringTasks.Free;
     end;
   end;
   ```

5. **Performance Monitoring**
   - Track number of active recurring task templates
   - Monitor instance generation time
   - Alert when instance count exceeds thresholds
   - Log recurrence rule calculation performance

**Best Practices**:
- Set reasonable `MaxOccurrences` for recurring tasks
- Use `EndDate` to limit open-ended recurrences
- Implement background job for instance generation (don't block UI)
- Cache frequently accessed recurrence calculations
- Consider database storage for systems with many recurring tasks

**Performance Targets**:
- Instance generation: < 100ms for 100 occurrences
- Recurrence calculation: < 1ms per occurrence
- Storage overhead: < 500 bytes per recurring task template
- Maximum recommended active instances per template: 365 (one year)
### 7.7 Performance Optimization

**Key Metrics**:
- Task creation: < 1ms
- Task retrieval by ID: < 1ms
- Filtering 1000 tasks: < 10ms
- Saving 1000 tasks to JSON: < 100ms
- Loading 1000 tasks from JSON: < 100ms

**Optimization Techniques**:

1. **Object Pooling**: Reuse TTask objects instead of creating/destroying frequently
2. **String Builder**: Use TStringBuilder for string concatenation in serialization
3. **Memory Pre-allocation**: Pre-allocate list capacity when the size is known
4. **Avoid Unnecessary Copies**: Use references instead of cloning when possible

### 7.8 Deployment Checklist

- [ ] Compile with optimization flags (`-O3 -XX -CX`)
- [ ] Include all required units in the distribution
- [ ] Test on target platforms (Windows, Linux, macOS)
- [ ] Provide example code for integration
- [ ] Document minimum FPC version requirements
- [ ] Include software specification and API documentation
- [ ] Create installation/setup instructions
- [ ] Test with sample data sets of various sizes
- [ ] Verify memory usage is acceptable
- [ ] Ensure thread safety if targeting multi-threaded applications


---

## 8. Testing Strategies and Coverage

### 8.1 Testing Philosophy

The Free Pascal Task Manager Library follows a comprehensive testing strategy to ensure reliability, correctness, and maintainability. Testing is divided into multiple layers:

- **Unit Testing**: Testing individual components in isolation
- **Integration Testing**: Testing component interactions
- **System Testing**: Testing the complete library functionality
- **Performance Testing**: Ensuring scalability and efficiency
- **Regression Testing**: Preventing reintroduction of bugs

### 8.2 Testing Framework and Tools

#### 8.2.1 Recommended Testing Frameworks

**FPCUnit** (Primary Framework)
```pascal
uses
  fpcunit, testregistry, testutils;

type
  TTaskModelTest = class(TTestCase)
  published
    procedure TestTaskCreation;
    procedure TestTaskValidation;
    procedure TestTaskSerialization;
  end;
```

**Alternative Frameworks**:
- **DUnit2** - For Delphi compatibility
- **tiOPF Testing Framework** - For more complex testing scenarios
- **Custom Test Harness** - For specific requirements

#### 8.2.2 Code Coverage Tools

- **FPCov** - Free Pascal code coverage tool
- **lcov** - Line coverage visualization
- **gcov** - GNU coverage tool (with FPC)

#### 8.2.3 Continuous Integration

- **GitLab CI/CD** - Automated testing on commit
- **GitHub Actions** - Cross-platform testing
- **Jenkins** - Enterprise CI/CD
- **Travis CI** - Open source projects

### 8.3 Unit Testing Strategy

#### 8.3.1 Test Organization

```
tests/
├── unit/
│   ├── TaskModelTests.pas
│   ├── TaskListTests.pas
│   ├── TaskManagerTests.pas
│   ├── TaskFilterTests.pas
│   ├── TaskValidatorTests.pas
│   ├── TaskStorageJSONTests.pas
│   ├── TaskStorageXMLTests.pas
│   ├── TaskStorageCSVTests.pas
│   ├── TaskStatisticsTests.pas
│   └── TaskUtilsTests.pas
├── integration/
│   ├── EndToEndTests.pas
│   ├── StorageIntegrationTests.pas
│   └── PerformanceTests.pas
├── fixtures/
│   ├── sample_tasks.json
│   ├── sample_tasks.xml
│   └── sample_tasks.csv
└── TestRunner.pas
```

#### 8.3.2 TTask Unit Tests

```pascal
unit TaskModelTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TaskModel;

type
  TTaskModelTest = class(TTestCase)
  private
    FTask: TTask;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTaskCreation;
    procedure TestTaskIDGeneration;
    procedure TestTaskTitle;
    procedure TestTaskDescription;
    procedure TestTaskStatus;
    procedure TestTaskPriority;
    procedure TestTaskCategory;
    procedure TestTaskDates;
    procedure TestTaskTags;
    procedure TestTaskClone;
    procedure TestTaskEquality;
    procedure TestTaskSerialization;
  end;

implementation

procedure TTaskModelTest.SetUp;
begin
  FTask := TTask.Create;
end;

procedure TTaskModelTest.TearDown;
begin
  FTask.Free;
end;

procedure TTaskModelTest.TestTaskCreation;
begin
  AssertNotNull('Task should be created', FTask);
  AssertTrue('Task ID should be generated', FTask.ID <> '');
  AssertEquals('Default status should be Pending', 
    Ord(tsPending), Ord(FTask.Status));
  AssertEquals('Default priority should be Medium', 
    Ord(tpMedium), Ord(FTask.Priority));
end;

procedure TTaskModelTest.TestTaskTitle;
begin
  FTask.Title := 'Test Task';
  AssertEquals('Title should be set', 'Test Task', FTask.Title);
  
  // Test empty title
  FTask.Title := '';
  AssertEquals('Empty title should be allowed', '', FTask.Title);
end;

procedure TTaskModelTest.TestTaskDates;
var
  StartDate, DueDate: TDateTime;
begin
  StartDate := EncodeDate(2024, 12, 1);
  DueDate := EncodeDate(2024, 12, 31);
  
  FTask.StartDate := StartDate;
  FTask.DueDate := DueDate;
  
  AssertEquals('Start date should be set', StartDate, FTask.StartDate);
  AssertEquals('Due date should be set', DueDate, FTask.DueDate);
  AssertTrue('Due date should be after start date', 
    FTask.DueDate >= FTask.StartDate);
end;

procedure TTaskModelTest.TestTaskTags;
begin
  FTask.AddTag('urgent');
  FTask.AddTag('important');
  
  AssertEquals('Should have 2 tags', 2, FTask.Tags.Count);
  AssertTrue('Should contain urgent tag', FTask.HasTag('urgent'));
  AssertTrue('Should contain important tag', FTask.HasTag('important'));
  
  FTask.RemoveTag('urgent');
  AssertEquals('Should have 1 tag after removal', 1, FTask.Tags.Count);
  AssertFalse('Should not contain urgent tag', FTask.HasTag('urgent'));
end;

initialization
  RegisterTest(TTaskModelTest);
end.
```

#### 8.3.3 TTaskList Unit Tests

```pascal
unit TaskListTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TaskModel, TaskList;

type
  TTaskListTest = class(TTestCase)
  private
    FTaskList: TTaskList;
    FTask1, FTask2, FTask3: TTask;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddTask;
    procedure TestRemoveTask;
    procedure TestFindTask;
    procedure TestClearTasks;
    procedure TestTaskCount;
    procedure TestGetTaskByIndex;
    procedure TestSortByPriority;
    procedure TestSortByDueDate;
    procedure TestFilterByStatus;
  end;

implementation

procedure TTaskListTest.SetUp;
begin
  FTaskList := TTaskList.Create;
  
  FTask1 := TTask.Create;
  FTask1.Title := 'Task 1';
  FTask1.Priority := tpHigh;
  
  FTask2 := TTask.Create;
  FTask2.Title := 'Task 2';
  FTask2.Priority := tpMedium;
  
  FTask3 := TTask.Create;
  FTask3.Title := 'Task 3';
  FTask3.Priority := tpLow;
end;

procedure TTaskListTest.TearDown;
begin
  FTask1.Free;
  FTask2.Free;
  FTask3.Free;
  FTaskList.Free;
end;

procedure TTaskListTest.TestAddTask;
begin
  AssertEquals('List should be empty', 0, FTaskList.Count);
  
  FTaskList.Add(FTask1);
  AssertEquals('List should have 1 task', 1, FTaskList.Count);
  
  FTaskList.Add(FTask2);
  AssertEquals('List should have 2 tasks', 2, FTaskList.Count);
end;

procedure TTaskListTest.TestRemoveTask;
begin
  FTaskList.Add(FTask1);
  FTaskList.Add(FTask2);
  
  AssertTrue('Remove should succeed', FTaskList.Remove(FTask1.ID));
  AssertEquals('List should have 1 task', 1, FTaskList.Count);
  
  AssertFalse('Remove non-existent should fail', 
    FTaskList.Remove('non-existent-id'));
end;

procedure TTaskListTest.TestFilterByStatus;
var
  FilteredList: TTaskList;
begin
  FTask1.Status := tsPending;
  FTask2.Status := tsInProgress;
  FTask3.Status := tsCompleted;
  
  FTaskList.Add(FTask1);
  FTaskList.Add(FTask2);
  FTaskList.Add(FTask3);
  
  FilteredList := FTaskList.FilterByStatus(tsCompleted);
  try
    AssertEquals('Should have 1 completed task', 1, FilteredList.Count);
    AssertEquals('Should be Task 3', 'Task 3', FilteredList[0].Title);
  finally
    FilteredList.Free;
  end;
end;

initialization
  RegisterTest(TTaskListTest);
end.
```

#### 8.3.4 TTaskManager Unit Tests

```pascal
unit TaskManagerTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, 
  TaskModel, TaskManager, TaskStorage;

type
  TTaskManagerTest = class(TTestCase)
  private
    FManager: TTaskManager;
    FTestFile: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateTask;
    procedure TestUpdateTask;
    procedure TestDeleteTask;
    procedure TestGetAllTasks;
    procedure TestSearchTasks;
    procedure TestSaveAndLoad;
    procedure TestGetStatistics;
  end;

implementation

procedure TTaskManagerTest.SetUp;
begin
  FManager := TTaskManager.Create;
  FTestFile := 'test_tasks.json';
  // Clean up any existing test file
  if FileExists(FTestFile) then
    DeleteFile(FTestFile);
end;

procedure TTaskManagerTest.TearDown;
begin
  FManager.Free;
  if FileExists(FTestFile) then
    DeleteFile(FTestFile);
end;

procedure TTaskManagerTest.TestCreateTask;
var
  TaskID: string;
  Task: TTask;
begin
  TaskID := FManager.CreateTask('New Task', 'Description', tpHigh, tcWork);
  AssertTrue('Task ID should not be empty', TaskID <> '');
  
  Task := FManager.GetTask(TaskID);
  AssertNotNull('Task should exist', Task);
  AssertEquals('Title should match', 'New Task', Task.Title);
  AssertEquals('Priority should match', Ord(tpHigh), Ord(Task.Priority));
end;

procedure TTaskManagerTest.TestSaveAndLoad;
var
  TaskID: string;
  Manager2: TTaskManager;
  LoadedTask: TTask;
begin
  // Create and save
  TaskID := FManager.CreateTask('Persistent Task', 'Test', tpMedium, tcPersonal);
  FManager.SaveToFile(FTestFile);
  
  // Load in new manager
  Manager2 := TTaskManager.Create;
  try
    Manager2.LoadFromFile(FTestFile);
    LoadedTask := Manager2.GetTask(TaskID);
    
    AssertNotNull('Loaded task should exist', LoadedTask);
    AssertEquals('Loaded task title should match', 
      'Persistent Task', LoadedTask.Title);
  finally
    Manager2.Free;
  end;
end;

initialization
  RegisterTest(TTaskManagerTest);
end.
```

### 8.4 Integration Testing

#### 8.4.1 End-to-End Integration Tests

```pascal
unit EndToEndTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TaskModel, TaskManager, TaskFilter, TaskValidator;

type
  TEndToEndTest = class(TTestCase)
  published
    procedure TestCompleteWorkflow;
    procedure TestMultipleStorageFormats;
    procedure TestFilterAndStatistics;
  end;

implementation

procedure TEndToEndTest.TestCompleteWorkflow;
var
  Manager: TTaskManager;
  TaskID: string;
  Task: TTask;
  FilteredTasks: TTaskList;
begin
  Manager := TTaskManager.Create;
  try
    // Create tasks
    TaskID := Manager.CreateTask('Task 1', 'Desc 1', tpHigh, tcWork);
    Manager.CreateTask('Task 2', 'Desc 2', tpMedium, tcPersonal);
    Manager.CreateTask('Task 3', 'Desc 3', tpLow, tcWork);
    
    // Update task
    Task := Manager.GetTask(TaskID);
    Task.Status := tsInProgress;
    Manager.UpdateTask(Task);
    
    // Filter tasks
    FilteredTasks := Manager.FilterTasks(
      procedure(const Criteria: TTaskFilterCriteria)
      begin
        Criteria.Category := tcWork;
      end
    );
    
    try
      AssertEquals('Should have 2 work tasks', 2, FilteredTasks.Count);
    finally
      FilteredTasks.Free;
    end;
    
    // Save and reload
    Manager.SaveToFile('test_workflow.json');
    Manager.LoadFromFile('test_workflow.json');
    
    AssertEquals('Should still have 3 tasks', 3, Manager.TaskCount);
    
  finally
    Manager.Free;
    DeleteFile('test_workflow.json');
  end;
end;

initialization
  RegisterTest(TEndToEndTest);
end.
```

### 8.5 Test Coverage Requirements

#### 8.5.1 Coverage Targets

| Component | Minimum Coverage | Target Coverage |
|-----------|-----------------|-----------------|
| TaskModel | 95% | 100% |
| TaskList | 90% | 95% |
| TaskManager | 90% | 95% |
| TaskFilter | 85% | 90% |
| TaskValidator | 95% | 100% |
| TaskStorage* | 85% | 90% |
| TaskStatistics | 80% | 85% |
| TaskUtils | 85% | 90% |
| **Overall** | **90%** | **95%** |

#### 8.5.2 Coverage Measurement

```bash
# Compile with coverage support
fpc -g -gl -Criot TaskManager.lpr

# Run tests
./TaskManagerTests

# Generate coverage report
lcov --capture --directory . --output-file coverage.info
genhtml coverage.info --output-directory coverage_html
```

### 8.6 Performance Testing

#### 8.6.1 Performance Benchmarks

```pascal
unit PerformanceTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TaskManager;

type
  TPerformanceTest = class(TTestCase)
  published
    procedure TestLargeTaskCreation;
    procedure TestBulkOperations;
    procedure TestSearchPerformance;
    procedure TestStoragePerformance;
  end;

implementation

procedure TPerformanceTest.TestLargeTaskCreation;
var
  Manager: TTaskManager;
  StartTime: TDateTime;
  i: Integer;
const
  TASK_COUNT = 10000;
begin
  Manager := TTaskManager.Create;
  try
    StartTime := Now;
    
    for i := 1 to TASK_COUNT do
      Manager.CreateTask(
        Format('Task %d', [i]),
        Format('Description %d', [i]),
        tpMedium,
        tcWork
      );
    
    // Should complete in less than 5 seconds
    AssertTrue('Should create 10k tasks quickly',
      MilliSecondsBetween(Now, StartTime) < 5000);
    
    AssertEquals('Should have all tasks', TASK_COUNT, Manager.TaskCount);
  finally
    Manager.Free;
  end;
end;

procedure TPerformanceTest.TestSearchPerformance;
var
  Manager: TTaskManager;
  StartTime: TDateTime;
  Results: TTaskList;
  i: Integer;
begin
  Manager := TTaskManager.Create;
  try
    // Create 1000 tasks
    for i := 1 to 1000 do
      Manager.CreateTask(Format('Task %d', [i]), '', tpMedium, tcWork);
    
    StartTime := Now;
    Results := Manager.SearchTasks('Task 5');
    try
      // Search should be fast even with 1000 tasks
      AssertTrue('Search should be fast',
        MilliSecondsBetween(Now, StartTime) < 100);
    finally
      Results.Free;
    end;
  finally
    Manager.Free;
  end;
end;

initialization
  RegisterTest(TPerformanceTest);
end.
```

#### 8.6.2 Performance Targets

| Operation | Target Time | Maximum Time |
|-----------|-------------|--------------|
| Create single task | < 1ms | < 5ms |
| Create 1000 tasks | < 500ms | < 2s |
| Search 1000 tasks | < 50ms | < 200ms |
| Filter 1000 tasks | < 100ms | < 500ms |
| Save 1000 tasks (JSON) | < 200ms | < 1s |
| Load 1000 tasks (JSON) | < 300ms | < 1.5s |
| Sort 1000 tasks | < 50ms | < 200ms |

### 8.7 Regression Testing

#### 8.7.1 Regression Test Suite

Maintain a comprehensive regression test suite that:

1. **Runs automatically** on every commit
2. **Covers all fixed bugs** to prevent reintroduction
3. **Tests edge cases** discovered during development
4. **Validates backward compatibility** when making changes

```pascal
unit RegressionTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TaskModel;

type
  TRegressionTest = class(TTestCase)
  published
    // Bug fix: Task with empty title should be allowed
    procedure TestBug001_EmptyTitleAllowed;
    
    // Bug fix: Date comparison should handle time component
    procedure TestBug002_DateComparisonWithTime;
    
    // Bug fix: Tag removal should be case-insensitive
    procedure TestBug003_CaseInsensitiveTagRemoval;
  end;

implementation

procedure TRegressionTest.TestBug001_EmptyTitleAllowed;
var
  Task: TTask;
begin
  Task := TTask.Create;
  try
    Task.Title := '';
    AssertEquals('Empty title should be allowed', '', Task.Title);
  finally
    Task.Free;
  end;
end;

initialization
  RegisterTest(TRegressionTest);
end.
```

### 8.8 Test Execution

#### 8.8.1 Running All Tests

```pascal
program TestRunner;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry, testreport,
  // Unit tests
  TaskModelTests,
  TaskListTests,
  TaskManagerTests,
  TaskFilterTests,
  TaskValidatorTests,
  TaskStorageJSONTests,
  TaskStorageXMLTests,
  TaskStorageCSVTests,
  TaskStatisticsTests,
  TaskUtilsTests,
  // Integration tests
  EndToEndTests,
  StorageIntegrationTests,
  PerformanceTests,
  // Regression tests
  RegressionTests;

var
  TestResult: TTestResult;
  Reporter: TPlainResultsWriter;

begin
  TestResult := TTestResult.Create;
  Reporter := TPlainResultsWriter.Create;
  try
    TestResult.AddListener(Reporter);
    GetTestRegistry.Run(TestResult);
    Reporter.WriteResult(TestResult);
    
    // Exit with error code if tests failed
    if TestResult.NumberOfErrors > 0 then
      Halt(1);
    if TestResult.NumberOfFailures > 0 then
      Halt(2);
  finally
    Reporter.Free;
    TestResult.Free;
  end;
end.
```

#### 8.8.2 Continuous Integration Configuration

```yaml
# .gitlab-ci.yml
test:
  stage: test
  script:
    - fpc -B -g -gl tests/TestRunner.pas
    - ./tests/TestRunner
  coverage: '/Lines: (\d+\.\d+)%/'
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml
```

### 8.9 Testing Best Practices

1. **Test Independence**: Each test should be independent and not rely on other tests
2. **Clean State**: Use SetUp and TearDown to ensure clean test state
3. **Meaningful Names**: Test names should clearly describe what they test
4. **One Assertion Focus**: Each test should focus on one specific behavior
5. **Test Data Management**: Use fixtures for consistent test data
6. **Mock External Dependencies**: Mock file I/O and external services when needed
7. **Maintain Test Code**: Keep test code as clean as production code
8. **Document Complex Tests**: Add comments for non-obvious test scenarios

### 8.10 Test Documentation

Each test file should include:

```pascal
{
  Unit: TaskModelTests
  Purpose: Unit tests for TTask class
  Coverage:
    - Task creation and initialization
    - Property getters and setters
    - Task lifecycle methods
    - Serialization and deserialization
  Dependencies: fpcunit, TaskModel
  Author: Development Team
  Last Updated: 2024-12
}
```

### 8.11 Testing Checklist

- [ ] Unit tests written for all public methods
- [ ] Integration tests cover main workflows
- [ ] Performance tests validate scalability
- [ ] Code coverage meets minimum requirements (90%)
- [ ] All tests pass on all target platforms
- [ ] Regression tests added for all bug fixes
- [ ] Test documentation is complete
- [ ] CI/CD pipeline runs all tests automatically
- [ ] Memory leaks checked and resolved
- [ ] Edge cases and error conditions tested





### 8.12 Task Dependency Testing

##### 8.12.1 TTaskDependency Unit Tests

```pascal
procedure TestTaskDependencyCreation;
var
  Dep: TTaskDependency;
begin
  Dep := TTaskDependency.Create(1, 2, dtFinishToStart);
  try
    AssertEquals('Source ID', 1, Dep.SourceTaskID);
    AssertEquals('Target ID', 2, Dep.TargetTaskID);
    AssertEquals('Type', dtFinishToStart, Dep.DependencyType);
    AssertTrue('Valid dependency', Dep.IsValid);
  finally
    Dep.Free;
  end;
end;

procedure TestDependencyCloning;
var
  Original, Clone: TTaskDependency;
begin
  Original := TTaskDependency.Create(1, 2);
  try
    Original.Description := 'Test dependency';
    Original.LagTime := 24;
    
    Clone := Original.Clone;
    try
      AssertEquals('Cloned source', Original.SourceTaskID, Clone.SourceTaskID);
      AssertEquals('Cloned target', Original.TargetTaskID, Clone.TargetTaskID);
      AssertEquals('Cloned description', Original.Description, Clone.Description);
      AssertEquals('Cloned lag time', Original.LagTime, Clone.LagTime);
    finally
      Clone.Free;
    end;
  finally
    Original.Free;
  end;
end;
```

##### 8.12.2 TTaskDependencyManager Unit Tests

```pascal
procedure TestAddDependency;
var
  Manager: TTaskManager;
  DepManager: TTaskDependencyManager;
  Task1, Task2: TTask;
  Dep: TTaskDependency;
begin
  Manager := TTaskManager.Create;
  try
    Task1 := Manager.CreateTask('Task 1', tpHigh);
    Task2 := Manager.CreateTask('Task 2', tpMedium);
    
    DepManager := TTaskDependencyManager.Create(Manager.Tasks);
    try
      Dep := DepManager.AddDependency(Task2.ID, Task1.ID);
      AssertNotNull('Dependency created', Dep);
      AssertEquals('Dependency count', 1, DepManager.GetDependencyCount);
    finally
      DepManager.Free;
    end;
  finally
    Manager.Free;
  end;
end;

procedure TestCircularDependencyDetection;
var
  Manager: TTaskManager;
  DepManager: TTaskDependencyManager;
  T1, T2, T3: TTask;
begin
  Manager := TTaskManager.Create;
  try
    T1 := Manager.CreateTask('Task 1', tpHigh);
    T2 := Manager.CreateTask('Task 2', tpHigh);
    T3 := Manager.CreateTask('Task 3', tpHigh);
    
    DepManager := TTaskDependencyManager.Create(Manager.Tasks);
    try
      // Create chain: T1 -> T2 -> T3
      DepManager.AddDependency(T2.ID, T1.ID);
      DepManager.AddDependency(T3.ID, T2.ID);
      
      // Try to create cycle: T3 -> T1
      AssertTrue('Should detect cycle', 
                DepManager.WouldCreateCycle(T1.ID, T3.ID));
      
      // Should raise exception
      try
        DepManager.AddDependency(T1.ID, T3.ID);
        Fail('Should have raised exception for circular dependency');
      except
        on E: ETaskDependencyException do
          AssertTrue('Correct exception', True);
      end;
    finally
      DepManager.Free;
    end;
  finally
    Manager.Free;
  end;
end;

procedure TestDependencyPathFinding;
var
  Manager: TTaskManager;
  DepManager: TTaskDependencyManager;
  T1, T2, T3, T4: TTask;
  Path: TList<Integer>;
begin
  Manager := TTaskManager.Create;
  try
    T1 := Manager.CreateTask('Task 1', tpHigh);
    T2 := Manager.CreateTask('Task 2', tpHigh);
    T3 := Manager.CreateTask('Task 3', tpHigh);
    T4 := Manager.CreateTask('Task 4', tpHigh);
    
    DepManager := TTaskDependencyManager.Create(Manager.Tasks);
    try
      // Create path: T1 -> T2 -> T3 -> T4
      DepManager.AddDependency(T2.ID, T1.ID);
      DepManager.AddDependency(T3.ID, T2.ID);
      DepManager.AddDependency(T4.ID, T3.ID);
      
      Path := DepManager.GetDependencyPath(T1.ID, T4.ID);
      try
        AssertEquals('Path length', 4, Path.Count);
        AssertEquals('Path start', T1.ID, Path[0]);
        AssertEquals('Path end', T4.ID, Path[3]);
      finally
        Path.Free;
      end;
    finally
      DepManager.Free;
    end;
  finally
    Manager.Free;
  end;
end;
```

##### 8.12.3 TDependencyGraph Unit Tests

```pascal
procedure TestTopologicalSort;
var
  Manager: TTaskManager;
  DepManager: TTaskDependencyManager;
  Graph: TDependencyGraph;
  Dependencies: TList<TTaskDependency>;
  Sorted: TList<Integer>;
  T1, T2, T3: TTask;
begin
  Manager := TTaskManager.Create;
  try
    T1 := Manager.CreateTask('Foundation', tpHigh);
    T2 := Manager.CreateTask('Walls', tpHigh);
    T3 := Manager.CreateTask('Roof', tpHigh);
    
    DepManager := TTaskDependencyManager.Create(Manager.Tasks);
    try
      // Dependencies: Foundation -> Walls -> Roof
      DepManager.AddDependency(T2.ID, T1.ID);
      DepManager.AddDependency(T3.ID, T2.ID);
      
      Dependencies := DepManager.GetAllDependencies;
      try
        Graph := TDependencyGraph.Create(Dependencies);
        try
          Sorted := Graph.TopologicalSort;
          try
            AssertTrue('Valid sort', Sorted.Count = 3);
            // Foundation should come before Walls
            AssertTrue('Foundation before Walls', 
                      Sorted.IndexOf(T1.ID) < Sorted.IndexOf(T2.ID));
            // Walls should come before Roof
            AssertTrue('Walls before Roof', 
                      Sorted.IndexOf(T2.ID) < Sorted.IndexOf(T3.ID));
          finally
            Sorted.Free;
          end;
        finally
          Graph.Free;
        end;
      finally
        Dependencies.Free;
      end;
    finally
      DepManager.Free;
    end;
  finally
    Manager.Free;
  end;
end;
```

### 8.13 Recurring Tasks Testing

Comprehensive testing for the recurring tasks feature to ensure correct recurrence calculation, instance generation, and edge case handling.

#### 8.13.1 TRecurrenceRule Unit Tests

```pascal
unit RecurrenceRuleTests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, TaskRecurrence, SysUtils, DateUtils;

type
  TRecurrenceRuleTest = class(TTestCase)
  published
    procedure TestCreateDailyRule;
    procedure TestCreateWeeklyRule;
    procedure TestCreateMonthlyRule;
    procedure TestCreateYearlyRule;
    procedure TestRuleValidation;
    procedure TestIntervalValidation;
    procedure TestDateRangeValidation;
    procedure TestDaysOfWeekValidation;
    procedure TestExceptionDateManagement;
    procedure TestRuleCloning;
    procedure TestRuleEquality;
    procedure TestToHumanReadable;
    procedure TestRuleSerialization;
  end;

implementation

procedure TRecurrenceRuleTest.TestCreateDailyRule;
var
  Rule: TRecurrenceRule;
begin
  Rule := TRecurrenceRule.Create(rpDaily);
  try
    AssertEquals('Pattern', Ord(rpDaily), Ord(Rule.Pattern));
    AssertEquals('Default interval', 1, Rule.Interval);
    AssertTrue('Should be valid by default', Rule.IsValid);
  finally
    Rule.Free;
  end;
end;

procedure TRecurrenceRuleTest.TestCreateWeeklyRule;
var
  Rule: TRecurrenceRule;
begin
  Rule := TRecurrenceRule.Create(rpWeekly);
  try
    Rule.Interval := 1;
    Rule.DaysOfWeek := [dwMonday, dwFriday];
    Rule.StartDate := Date;
    
    AssertTrue('Should be valid', Rule.IsValid);
    AssertEquals('Interval', 1, Rule.Interval);
    AssertTrue('Has Monday', dwMonday in Rule.DaysOfWeek);
    AssertTrue('Has Friday', dwFriday in Rule.DaysOfWeek);
  finally
    Rule.Free;
  end;
end;

procedure TRecurrenceRuleTest.TestRuleValidation;
var
  Rule: TRecurrenceRule;
  Errors: TStringList;
begin
  Rule := TRecurrenceRule.Create(rpDaily);
  try
    // Test invalid interval
    Rule.Interval := 0;
    AssertFalse('Should be invalid with zero interval', Rule.IsValid);
    
    Errors := Rule.GetValidationErrors;
    try
      AssertTrue('Should have errors', Errors.Count > 0);
    finally
      Errors.Free;
    end;
    
    // Fix interval
    Rule.Interval := 1;
    Rule.StartDate := Date;
    AssertTrue('Should be valid after fixing', Rule.IsValid);
  finally
    Rule.Free;
  end;
end;

procedure TRecurrenceRuleTest.TestExceptionDateManagement;
var
  Rule: TRecurrenceRule;
  ExDate: TDateTime;
begin
  Rule := TRecurrenceRule.Create(rpDaily);
  try
    ExDate := EncodeDate(2024, 12, 25);
    
    // Add exception
    Rule.AddExceptionDate(ExDate);
    AssertEquals('Exception count', 1, Rule.ExceptionDates.Count);
    AssertTrue('Is exception', Rule.IsExceptionDate(ExDate));
    
    // Remove exception
    Rule.RemoveExceptionDate(ExDate);
    AssertEquals('Exception count after remove', 0, Rule.ExceptionDates.Count);
    AssertFalse('Not exception anymore', Rule.IsExceptionDate(ExDate));
  finally
    Rule.Free;
  end;
end;

procedure TRecurrenceRuleTest.TestRuleCloning;
var
  Original, Clone: TRecurrenceRule;
begin
  Original := TRecurrenceRule.Create(rpWeekly);
  try
    Original.Interval := 2;
    Original.DaysOfWeek := [dwMonday, dwWednesday];
    Original.StartDate := Date;
    Original.EndDate := Date + 365;
    Original.AddExceptionDate(Date + 10);
    
    Clone := Original.Clone;
    try
      AssertEquals('Cloned pattern', Ord(Original.Pattern), Ord(Clone.Pattern));
      AssertEquals('Cloned interval', Original.Interval, Clone.Interval);
      AssertEquals('Cloned days count', 2, Clone.DaysOfWeek.Count);
      AssertTrue('Cloned has Monday', dwMonday in Clone.DaysOfWeek);
      AssertEquals('Cloned exceptions', 1, Clone.ExceptionDates.Count);
    finally
      Clone.Free;
    end;
  finally
    Original.Free;
  end;
end;

procedure TRecurrenceRuleTest.TestToHumanReadable;
var
  Rule: TRecurrenceRule;
  Description: string;
begin
  // Test daily
  Rule := TRecurrenceRule.Create(rpDaily);
  try
    Rule.Interval := 1;
    Description := Rule.ToHumanReadable;
    AssertTrue('Contains "day"', Pos('day', LowerCase(Description)) > 0);
  finally
    Rule.Free;
  end;
  
  // Test weekly
  Rule := TRecurrenceRule.Create(rpWeekly);
  try
    Rule.Interval := 1;
    Rule.DaysOfWeek := [dwMonday, dwFriday];
    Description := Rule.ToHumanReadable;
    AssertTrue('Contains "week"', Pos('week', LowerCase(Description)) > 0);
    AssertTrue('Contains "Monday"', Pos('Monday', Description) > 0);
  finally
    Rule.Free;
  end;
end;
```

**Test Coverage Requirements:**
- [ ] Test all recurrence patterns (daily, weekly, monthly, yearly, custom)
- [ ] Test interval validation (positive integers only)
- [ ] Test date range validation (end after start)
- [ ] Test max occurrences validation
- [ ] Test days of week selection for weekly patterns
- [ ] Test day of month for monthly patterns
- [ ] Test exception date management (add, remove, check)
- [ ] Test rule cloning (deep copy verification)
- [ ] Test rule equality comparison
- [ ] Test human-readable description generation
- [ ] Test JSON serialization/deserialization
- [ ] Test edge cases (leap years, month-end dates)

#### 8.13.2 TRecurrenceEngine Unit Tests

```pascal
unit RecurrenceEngineTests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, TaskRecurrence, TaskModel, DateUtils, SysUtils;

type
  TRecurrenceEngineTest = class(TTestCase)
  private
    FRule: TRecurrenceRule;
    FEngine: TRecurrenceEngine;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDailyNextOccurrence;
    procedure TestWeeklyNextOccurrence;
    procedure TestMonthlyNextOccurrence;
    procedure TestYearlyNextOccurrence;
    procedure TestOccurrencesBetween;
    procedure TestNextNOccurrences;
    procedure TestExceptionDateSkipping;
    procedure TestEndDateEnforcement;
    procedure TestMaxOccurrencesEnforcement;
    procedure TestWillOccurOn;
    procedure TestOccurrenceCount;
    procedure TestHasEnded;
    procedure TestTaskInstanceGeneration;
    procedure TestPerformance;
  end;

implementation

procedure TRecurrenceEngineTest.SetUp;
begin
  FRule := nil;
  FEngine := nil;
end;

procedure TRecurrenceEngineTest.TearDown;
begin
  FEngine.Free;
  FRule.Free;
end;

procedure TRecurrenceEngineTest.TestDailyNextOccurrence;
var
  StartDate, NextDate: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 1);
  
  FRule := TRecurrenceRule.Create(rpDaily);
  FRule.Interval := 1;
  FRule.StartDate := StartDate;
  
  FEngine := TRecurrenceEngine.Create(FRule);
  
  NextDate := FEngine.GetNextOccurrence(StartDate);
  AssertEquals('Next day', 1, DaysBetween(StartDate, NextDate));
  
  // Test with interval of 3
  FRule.Interval := 3;
  NextDate := FEngine.GetNextOccurrence(StartDate);
  AssertEquals('Every 3 days', 3, DaysBetween(StartDate, NextDate));
end;

procedure TRecurrenceEngineTest.TestWeeklyNextOccurrence;
var
  StartDate, NextDate: TDateTime;
  Monday: TDateTime;
begin
  // Start on a Monday
  Monday := EncodeDate(2024, 1, 1);  // January 1, 2024 is a Monday
  
  FRule := TRecurrenceRule.Create(rpWeekly);
  FRule.Interval := 1;
  FRule.DaysOfWeek := [dwMonday, dwFriday];
  FRule.StartDate := Monday;
  
  FEngine := TRecurrenceEngine.Create(FRule);
  
  // From Monday, next should be Friday
  NextDate := FEngine.GetNextOccurrence(Monday);
  AssertEquals('Day of week', 5, DayOfWeek(NextDate));  // 5 = Friday
  
  // From Friday, next should be next Monday
  NextDate := FEngine.GetNextOccurrence(Monday + 4);
  AssertEquals('Next Monday', DayOfWeek(NextDate), 1);  // 1 = Monday
end;

procedure TRecurrenceEngineTest.TestOccurrencesBetween;
var
  StartDate, EndDate: TDateTime;
  Occurrences: TList<TDateTime>;
begin
  StartDate := EncodeDate(2024, 1, 1);
  EndDate := EncodeDate(2024, 1, 31);
  
  FRule := TRecurrenceRule.Create(rpDaily);
  FRule.Interval := 1;
  FRule.StartDate := StartDate;
  
  FEngine := TRecurrenceEngine.Create(FRule);
  
  Occurrences := FEngine.GetOccurrencesBetween(StartDate, EndDate);
  try
    AssertEquals('Days in January', 31, Occurrences.Count);
  finally
    Occurrences.Free;
  end;
end;

procedure TRecurrenceEngineTest.TestExceptionDateSkipping;
var
  StartDate, ExceptionDate: TDateTime;
  Occurrences: TList<TDateTime>;
begin
  StartDate := EncodeDate(2024, 1, 1);
  ExceptionDate := EncodeDate(2024, 1, 15);
  
  FRule := TRecurrenceRule.Create(rpDaily);
  FRule.Interval := 1;
  FRule.StartDate := StartDate;
  FRule.AddExceptionDate(ExceptionDate);
  
  FEngine := TRecurrenceEngine.Create(FRule);
  
  Occurrences := FEngine.GetOccurrencesBetween(StartDate, StartDate + 30);
  try
    // Should be 30 occurrences (31 days - 1 exception)
    AssertEquals('Occurrences with exception', 30, Occurrences.Count);
    
    // Verify exception date is not in list
    AssertFalse('Exception not in list', Occurrences.Contains(ExceptionDate));
  finally
    Occurrences.Free;
  end;
end;

procedure TRecurrenceEngineTest.TestTaskInstanceGeneration;
var
  Template: TTask;
  Instances: TTaskList;
  StartDate, EndDate: TDateTime;
begin
  Template := TTask.Create('Recurring Task');
  try
    Template.Description := 'This is a recurring task';
    Template.Priority := tpHigh;
    
    StartDate := Date;
    EndDate := Date + 7;
    
    FRule := TRecurrenceRule.Create(rpDaily);
    FRule.Interval := 1;
    FRule.StartDate := StartDate;
    
    FEngine := TRecurrenceEngine.Create(FRule);
    
    Instances := FEngine.GenerateTaskInstances(Template, StartDate, EndDate);
    try
      AssertEquals('Instance count', 8, Instances.Count);
      
      // Verify first instance
      AssertEquals('Title copied', Template.Title, Instances[0].Title);
      AssertEquals('Priority copied', Template.Priority, Instances[0].Priority);
      AssertTrue('Is instance', Instances[0].IsRecurrenceInstance);
      AssertEquals('Instance date', DateToStr(StartDate), DateToStr(Instances[0].RecurrenceInstanceDate));
    finally
      Instances.Free;
    end;
  finally
    Template.Free;
  end;
end;

procedure TRecurrenceEngineTest.TestPerformance;
var
  StartDate: TDateTime;
  Occurrences: TList<TDateTime>;
  StartTime, EndTime: TDateTime;
  ElapsedMs: Int64;
begin
  StartDate := Date;
  
  FRule := TRecurrenceRule.Create(rpDaily);
  FRule.Interval := 1;
  FRule.StartDate := StartDate;
  
  FEngine := TRecurrenceEngine.Create(FRule);
  
  StartTime := Now;
  
  // Generate 1000 occurrences
  Occurrences := FEngine.GetNextNOccurrences(StartDate, 1000);
  try
    EndTime := Now;
    ElapsedMs := MilliSecondsBetween(EndTime, StartTime);
    
    AssertEquals('Generated count', 1000, Occurrences.Count);
    AssertTrue('Performance target', ElapsedMs < 100);  // Should be < 100ms
    
    WriteLn(Format('Generated 1000 occurrences in %d ms', [ElapsedMs]));
  finally
    Occurrences.Free;
  end;
end;
```

**Test Coverage Requirements:**
- [ ] Test next occurrence calculation for all patterns
- [ ] Test previous occurrence calculation
- [ ] Test occurrence generation for date ranges
- [ ] Test N-occurrence generation
- [ ] Test exception date skipping during generation
- [ ] Test end date enforcement
- [ ] Test max occurrences limit
- [ ] Test WillOccurOn validation
- [ ] Test occurrence counting
- [ ] Test recurrence ended detection
- [ ] Test task instance generation from template
- [ ] Test performance (target: 1000 calculations < 100ms)

#### 8.13.3 Integration Tests for Recurring Tasks

```pascal
unit RecurringTaskIntegrationTests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, TaskManager, TaskModel, TaskRecurrence, 
  TaskStorageJSON, SysUtils, DateUtils;

type
  TRecurringTaskIntegrationTest = class(TTestCase)
  private
    FManager: TTaskManager;
    FTempFile: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateAndSaveRecurringTask;
    procedure TestLoadRecurringTask;
    procedure TestGenerateAndPersistInstances;
    procedure TestModifyRecurringTaskInstance;
    procedure TestDeleteRecurringTaskSeries;
    procedure TestFilterRecurringTasks;
    procedure TestRecurringTaskStatistics;
  end;

implementation

procedure TRecurringTaskIntegrationTest.SetUp;
begin
  FTempFile := GetTempDir + 'recurring_tasks_test.json';
  FManager := TTaskManager.Create(TJSONTaskStorage.Create(FTempFile));
end;

procedure TRecurringTaskIntegrationTest.TearDown;
begin
  FManager.Free;
  if FileExists(FTempFile) then
    DeleteFile(FTempFile);
end;

procedure TRecurringTaskIntegrationTest.TestCreateAndSaveRecurringTask;
var
  Task: TTask;
  Rule: TRecurrenceRule;
begin
  Task := TTask.Create('Daily Standup');
  Rule := TRecurrenceRule.Create(rpDaily);
  Rule.Interval := 1;
  Rule.StartDate := Date;
  Rule.EndDate := Date + 90;
  
  Task.SetRecurrence(Rule);
  FManager.AddTask(Task);
  
  AssertTrue('Manager has task', FManager.TaskCount > 0);
  AssertTrue('Task saved', FManager.SaveTasks);
  AssertTrue('File created', FileExists(FTempFile));
end;

procedure TRecurringTaskIntegrationTest.TestLoadRecurringTask;
var
  Task, LoadedTask: TTask;
  Rule: TRecurrenceRule;
  NewManager: TTaskManager;
begin
  // Create and save
  Task := TTask.Create('Weekly Meeting');
  Rule := TRecurrenceRule.Create(rpWeekly);
  Rule.Interval := 1;
  Rule.DaysOfWeek := [dwMonday];
  Rule.StartDate := Date;
  
  Task.SetRecurrence(Rule);
  FManager.AddTask(Task);
  FManager.SaveTasks;
  
  // Load in new manager
  NewManager := TTaskManager.Create(TJSONTaskStorage.Create(FTempFile));
  try
    AssertTrue('Loaded tasks', NewManager.LoadTasks);
    AssertEquals('Task count', 1, NewManager.TaskCount);
    
    LoadedTask := NewManager.GetTaskByID(Task.ID);
    AssertNotNull('Task loaded', LoadedTask);
    AssertTrue('Is recurring', LoadedTask.IsRecurring);
    AssertEquals('Pattern', Ord(rpWeekly), Ord(LoadedTask.RecurrenceRule.Pattern));
  finally
    NewManager.Free;
  end;
end;
```

**Integration Test Coverage:**
- [ ] Test creating recurring tasks with TTaskManager
- [ ] Test saving recurring tasks to storage
- [ ] Test loading recurring tasks from storage
- [ ] Test generating instances and persisting them
- [ ] Test modifying individual instances
- [ ] Test deleting recurring task series
- [ ] Test filtering recurring vs non-recurring tasks
- [ ] Test statistics for recurring tasks
- [ ] Test exception date persistence
- [ ] Test recurrence rule updates

#### 8.13.4 Edge Case Testing

**Critical Edge Cases:**
- [ ] Leap year handling (February 29th occurrences)
- [ ] Month-end dates (e.g., monthly on 31st for months with < 31 days)
- [ ] Daylight saving time transitions
- [ ] End of year to beginning of year transitions
- [ ] Weekly patterns spanning year boundaries
- [ ] Maximum occurrences reaching exactly on end date
- [ ] Zero-duration recurrences (start date = end date)
- [ ] Very large intervals (e.g., every 100 days)
- [ ] Multiple exception dates in sequence
- [ ] Concurrent access to recurrence engine

#### 8.13.5 Performance Benchmarks

**Target Performance Metrics:**
- Recurrence rule validation: < 1ms
- Next occurrence calculation (daily): < 0.1ms
- Next occurrence calculation (weekly): < 0.5ms
- Next occurrence calculation (monthly): < 1ms
- Generate 100 occurrences: < 10ms
- Generate 1000 occurrences: < 100ms
- Task instance creation: < 1ms per instance
- Exception date lookup: < 0.1ms (using hash set)

**Load Testing:**
- [ ] Test with 100 concurrent recurring task templates
- [ ] Test with 10,000 generated instances
- [ ] Test memory usage with long-running recurrences
- [ ] Test recurrence calculation accuracy over 10-year span
## 9. Class Diagrams and Methods/Properties

### 9.1 Overview

This section provides detailed class diagrams for all major components of the Free Pascal Task Manager Library. Each class diagram includes:

- All public properties with their types
- All public methods with parameters and return types
- Private/protected members where relevant for understanding
- Inheritance relationships
- Interface implementations
- Key dependencies between classes

### 9.2 Core Data Model Classes

#### 9.2.1 TTask Class Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                            TTask                                 │
├─────────────────────────────────────────────────────────────────┤
│ Private Fields:                                                  │
│   FID: string                                                    │
│   FTitle: string                                                 │
│   FDescription: string                                           │
│   FStatus: TTaskStatus                                           │
│   FPriority: TTaskPriority                                       │
│   FCategory: TTaskCategory                                       │
│   FCreatedAt: TDateTime                                          │
│   FUpdatedAt: TDateTime                                          │
│   FDueDate: TDateTime                                            │
│   FCompletedAt: TDateTime                                        │
│   FTags: TStringList                                             │
│   FNotes: string                                                 │
│   FEstimatedHours: Double                                        │
│   FActualHours: Double                                           │
│   FParentTaskID: string                                          │
│   FSubtasks: TStringList                                         │
├─────────────────────────────────────────────────────────────────┤
│ Public Properties:                                               │
│   property ID: string read FID write FID                         │
│   property Title: string read FTitle write SetTitle              │
│   property Description: string read FDescription write FDesc...  │
│   property Status: TTaskStatus read FStatus write SetStatus      │
│   property Priority: TTaskPriority read FPriority write SetPr... │
│   property Category: TTaskCategory read FCategory write SetCa... │
│   property CreatedAt: TDateTime read FCreatedAt write FCreat...  │
│   property UpdatedAt: TDateTime read FUpdatedAt write FUpdat...  │
│   property DueDate: TDateTime read FDueDate write SetDueDate    │
│   property CompletedAt: TDateTime read FCompletedAt write FC...  │
│   property Tags: TStringList read FTags                          │
│   property Notes: string read FNotes write SetNotes              │
│   property EstimatedHours: Double read FEstimatedHours write...  │
│   property ActualHours: Double read FActualHours write SetAc...  │
│   property ParentTaskID: string read FParentTaskID write FPa...  │
│   property Subtasks: TStringList read FSubtasks                  │
├─────────────────────────────────────────────────────────────────┤
│ Public Constructors:                                             │
│   + Create(): TTask                                              │
│   + CreateWithParams(                                            │
│       ATitle: string;                                            │
│       ADescription: string;                                      │
│       APriority: TTaskPriority;                                  │
│       ACategory: TTaskCategory                                   │
│     ): TTask                                                     │
├─────────────────────────────────────────────────────────────────┤
│ Public Destructor:                                               │
│   + Destroy(): void override                                     │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods:                                                  │
│   + AddTag(ATag: string): Boolean                                │
│   + RemoveTag(ATag: string): Boolean                             │
│   + HasTag(ATag: string): Boolean                                │
│   + ClearTags(): void                                            │
│   + AddSubtask(ATaskID: string): Boolean                         │
│   + RemoveSubtask(ATaskID: string): Boolean                      │
│   + HasSubtask(ATaskID: string): Boolean                         │
│   + ClearSubtasks(): void                                        │
│   + MarkAsCompleted(): void                                      │
│   + MarkAsInProgress(): void                                     │
│   + MarkAsPending(): void                                        │
│   + MarkAsCancelled(): void                                      │
│   + IsOverdue(): Boolean                                         │
│   + IsCompleted(): Boolean                                       │
│   + DaysUntilDue(): Integer                                      │
│   + DaysOverdue(): Integer                                       │
│   + Clone(): TTask                                               │
│   + CopyFrom(ATask: TTask): void                                 │
│   + ToJSON(): string                                             │
│   + FromJSON(const AJSON: string): Boolean                       │
│   + ToXML(): string                                              │
│   + FromXML(const AXML: string): Boolean                         │
│   + ToString(): string override                                  │
│   + Equals(ATask: TTask): Boolean                                │
│   + GetProgressPercentage(): Double                              │
│   + UpdateTimestamp(): void                                      │
├─────────────────────────────────────────────────────────────────┤
│ Private Methods:                                                 │
│   - SetTitle(const AValue: string): void                         │
│   - SetDescription(const AValue: string): void                   │
│   - SetStatus(const AValue: TTaskStatus): void                   │
│   - SetPriority(const AValue: TTaskPriority): void               │
│   - SetCategory(const AValue: TTaskCategory): void               │
│   - SetDueDate(const AValue: TDateTime): void                    │
│   - SetNotes(const AValue: string): void                         │
│   - SetEstimatedHours(const AValue: Double): void                │
│   - SetActualHours(const AValue: Double): void                   │
│   - GenerateID(): string                                         │
│   - ValidateData(): Boolean                                      │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- Inherits from: `TObject`
- Manages its own `TStringList` instances for Tags and Subtasks
- Automatically updates `UpdatedAt` timestamp on property changes
- Generates unique ID on creation using GUID
- Supports serialization to/from JSON and XML formats

#### 9.2.2 TTaskList Class Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                          TTaskList                               │
├─────────────────────────────────────────────────────────────────┤
│ Private Fields:                                                  │
│   FTasks: TFPList                                                │
│   FOwnsObjects: Boolean                                          │
│   FSorted: Boolean                                               │
│   FSortOrder: TSortOrder (ascending/descending)                  │
│   FSortField: TSortField (title/date/priority/status)            │
├─────────────────────────────────────────────────────────────────┤
│ Public Properties:                                               │
│   property Count: Integer read GetCount                          │
│   property Items[Index: Integer]: TTask read GetItem write...    │
│     default                                                      │
│   property OwnsObjects: Boolean read FOwnsObjects write FOw...   │
│   property Sorted: Boolean read FSorted write SetSorted          │
│   property SortOrder: TSortOrder read FSortOrder write SetSo...  │
│   property SortField: TSortField read FSortField write SetSo...  │
├─────────────────────────────────────────────────────────────────┤
│ Public Constructors:                                             │
│   + Create(): TTaskList                                          │
│   + Create(AOwnsObjects: Boolean): TTaskList                     │
├─────────────────────────────────────────────────────────────────┤
│ Public Destructor:                                               │
│   + Destroy(): void override                                     │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Collection Management:                          │
│   + Add(ATask: TTask): Integer                                   │
│   + Insert(Index: Integer; ATask: TTask): void                   │
│   + Remove(ATaskID: string): Boolean                             │
│   + Delete(Index: Integer): void                                 │
│   + Clear(): void                                                │
│   + IndexOf(ATaskID: string): Integer                            │
│   + FindByID(const AID: string): TTask                           │
│   + Contains(ATaskID: string): Boolean                           │
│   + Extract(ATask: TTask): TTask                                 │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Filtering:                                      │
│   + FilterByStatus(AStatus: TTaskStatus): TTaskList              │
│   + FilterByPriority(APriority: TTaskPriority): TTaskList        │
│   + FilterByCategory(ACategory: TTaskCategory): TTaskList        │
│   + FilterByTag(const ATag: string): TTaskList                   │
│   + FilterByDateRange(                                           │
│       AStartDate: TDateTime;                                     │
│       AEndDate: TDateTime                                        │
│     ): TTaskList                                                 │
│   + FilterOverdue(): TTaskList                                   │
│   + FilterCompleted(): TTaskList                                 │
│   + FilterActive(): TTaskList                                    │
│   + FilterByCustomCriteria(                                      │
│       ACriteria: TTaskFilterCriteria                             │
│     ): TTaskList                                                 │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Searching:                                      │
│   + Search(const AQuery: string): TTaskList                      │
│   + SearchInTitle(const AQuery: string): TTaskList               │
│   + SearchInDescription(const AQuery: string): TTaskList         │
│   + SearchInTags(const ATag: string): TTaskList                  │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Sorting:                                        │
│   + Sort(): void                                                 │
│   + SortByTitle(AOrder: TSortOrder): void                        │
│   + SortByPriority(AOrder: TSortOrder): void                     │
│   + SortByDueDate(AOrder: TSortOrder): void                      │
│   + SortByStatus(AOrder: TSortOrder): void                       │
│   + SortByCreatedDate(AOrder: TSortOrder): void                  │
│   + SortByCustomComparator(                                      │
│       AComparator: TTaskComparator                               │
│     ): void                                                      │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Iteration:                                      │
│   + First(): TTask                                               │
│   + Last(): TTask                                                │
│   + GetEnumerator(): TTaskListEnumerator                         │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Serialization:                                  │
│   + ToJSON(): string                                             │
│   + FromJSON(const AJSON: string): Boolean                       │
│   + ToXML(): string                                              │
│   + FromXML(const AXML: string): Boolean                         │
│   + ToCSV(): string                                              │
│   + FromCSV(const ACSV: string): Boolean                         │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Utilities:                                      │
│   + Clone(): TTaskList                                           │
│   + CopyFrom(AList: TTaskList): void                             │
│   + Merge(AList: TTaskList): void                                │
│   + GetStatistics(): TTaskStatisticsData                         │
│   + GetTasksByParentID(const AParentID: string): TTaskList       │
├─────────────────────────────────────────────────────────────────┤
│ Private Methods:                                                 │
│   - GetCount(): Integer                                          │
│   - GetItem(Index: Integer): TTask                               │
│   - SetItem(Index: Integer; AValue: TTask): void                 │
│   - SetSorted(AValue: Boolean): void                             │
│   - SetSortOrder(AValue: TSortOrder): void                       │
│   - SetSortField(AValue: TSortField): void                       │
│   - QuickSort(L, R: Integer): void                               │
│   - CompareTasksByField(                                         │
│       Task1, Task2: TTask;                                       │
│       Field: TSortField                                          │
│     ): Integer                                                   │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- Inherits from: `TObject`
- Uses `TFPList` internally for efficient storage
- Owns task objects by default (frees them on destruction)
- Supports custom sorting and filtering
- Implements enumerator for for-in loops
- All filter methods return new `TTaskList` instances

### 9.3 Business Logic Classes

#### 9.3.1 TTaskManager Class Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                        TTaskManager                              │
├─────────────────────────────────────────────────────────────────┤
│ Private Fields:                                                  │
│   FTaskList: TTaskList                                           │
│   FStorage: ITaskStorage                                         │
│   FValidator: TTaskValidator                                     │
│   FStatistics: TTaskStatistics                                   │
│   FAutoSave: Boolean                                             │
│   FDefaultStorageFile: string                                    │
│   FOnTaskAdded: TTaskEvent                                       │
│   FOnTaskUpdated: TTaskEvent                                     │
│   FOnTaskDeleted: TTaskEvent                                     │
│   FOnTaskCompleted: TTaskEvent                                   │
├─────────────────────────────────────────────────────────────────┤
│ Public Properties:                                               │
│   property TaskList: TTaskList read FTaskList                    │
│   property TaskCount: Integer read GetTaskCount                  │
│   property AutoSave: Boolean read FAutoSave write FAutoSave      │
│   property DefaultStorageFile: string read FDefaultStorage...    │
│     write FDefaultStorageFile                                    │
│   property OnTaskAdded: TTaskEvent read FOnTaskAdded write...    │
│   property OnTaskUpdated: TTaskEvent read FOnTaskUpdated w...    │
│   property OnTaskDeleted: TTaskEvent read FOnTaskDeleted w...    │
│   property OnTaskCompleted: TTaskEvent read FOnTaskComplet...    │
├─────────────────────────────────────────────────────────────────┤
│ Public Constructors:                                             │
│   + Create(): TTaskManager                                       │
│   + Create(AStorage: ITaskStorage): TTaskManager                 │
├─────────────────────────────────────────────────────────────────┤
│ Public Destructor:                                               │
│   + Destroy(): void override                                     │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - CRUD Operations:                                │
│   + CreateTask(                                                  │
│       const ATitle: string;                                      │
│       const ADescription: string;                                │
│       APriority: TTaskPriority;                                  │
│       ACategory: TTaskCategory                                   │
│     ): string {returns TaskID}                                   │
│   + CreateTaskFull(ATask: TTask): string                         │
│   + GetTask(const ATaskID: string): TTask                        │
│   + GetAllTasks(): TTaskList                                     │
│   + UpdateTask(ATask: TTask): Boolean                            │
│   + DeleteTask(const ATaskID: string): Boolean                   │
│   + TaskExists(const ATaskID: string): Boolean                   │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Task Operations:                                │
│   + CompleteTask(const ATaskID: string): Boolean                 │
│   + StartTask(const ATaskID: string): Boolean                    │
│   + CancelTask(const ATaskID: string): Boolean                   │
│   + ReopenTask(const ATaskID: string): Boolean                   │
│   + CloneTask(const ATaskID: string): string                     │
│   + MoveTask(                                                    │
│       const ATaskID: string;                                     │
│       ANewCategory: TTaskCategory                                │
│     ): Boolean                                                   │
│   + SetTaskPriority(                                             │
│       const ATaskID: string;                                     │
│       APriority: TTaskPriority                                   │
│     ): Boolean                                                   │
│   + SetTaskDueDate(                                              │
│       const ATaskID: string;                                     │
│       ADueDate: TDateTime                                        │
│     ): Boolean                                                   │
│   + AddTaskTag(                                                  │
│       const ATaskID: string;                                     │
│       const ATag: string                                         │
│     ): Boolean                                                   │
│   + RemoveTaskTag(                                               │
│       const ATaskID: string;                                     │
│       const ATag: string                                         │
│     ): Boolean                                                   │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Filtering and Searching:                        │
│   + FilterTasks(                                                 │
│       ACriteria: TTaskFilterCriteria                             │
│     ): TTaskList                                                 │
│   + SearchTasks(const AQuery: string): TTaskList                 │
│   + GetTasksByStatus(AStatus: TTaskStatus): TTaskList            │
│   + GetTasksByPriority(                                          │
│       APriority: TTaskPriority                                   │
│     ): TTaskList                                                 │
│   + GetTasksByCategory(                                          │
│       ACategory: TTaskCategory                                   │
│     ): TTaskList                                                 │
│   + GetTasksByTag(const ATag: string): TTaskList                 │
│   + GetOverdueTasks(): TTaskList                                 │
│   + GetUpcomingTasks(ADays: Integer): TTaskList                  │
│   + GetCompletedTasks(): TTaskList                               │
│   + GetActiveTasks(): TTaskList                                  │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Persistence:                                    │
│   + SaveToFile(const AFileName: string): Boolean                 │
│   + LoadFromFile(const AFileName: string): Boolean               │
│   + Save(): Boolean                                              │
│   + Load(): Boolean                                              │
│   + ImportFromFile(                                              │
│       const AFileName: string;                                   │
│       AFormat: TStorageFormat                                    │
│     ): Boolean                                                   │
│   + ExportToFile(                                                │
│       const AFileName: string;                                   │
│       AFormat: TStorageFormat                                    │
│     ): Boolean                                                   │
│   + SetStorage(AStorage: ITaskStorage): void                     │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Statistics and Analytics:                       │
│   + GetStatistics(): TTaskStatisticsData                         │
│   + GetCompletionRate(): Double                                  │
│   + GetAverageCompletionTime(): Double                           │
│   + GetProductivityScore(): Double                               │
│   + GetTaskDistribution(): TTaskDistribution                     │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Validation:                                     │
│   + ValidateTask(ATask: TTask): TValidationResult                │
│   + ValidateTaskData(                                            │
│       const ATitle: string;                                      │
│       const ADescription: string                                 │
│     ): TValidationResult                                         │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Batch Operations:                               │
│   + BulkDelete(ATaskIDs: TStringList): Integer                   │
│   + BulkUpdateStatus(                                            │
│       ATaskIDs: TStringList;                                     │
│       AStatus: TTaskStatus                                       │
│     ): Integer                                                   │
│   + BulkUpdatePriority(                                          │
│       ATaskIDs: TStringList;                                     │
│       APriority: TTaskPriority                                   │
│     ): Integer                                                   │
│   + BulkAddTag(                                                  │
│       ATaskIDs: TStringList;                                     │
│       const ATag: string                                         │
│     ): Integer                                                   │
│   + DeleteCompletedTasks(): Integer                              │
│   + ArchiveOldTasks(ADaysOld: Integer): Integer                  │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Utilities:                                      │
│   + Clear(): void                                                │
│   + GetUniqueTaskID(): string                                    │
│   + GetAllTags(): TStringList                                    │
│   + GetAllCategories(): TStringList                              │
├─────────────────────────────────────────────────────────────────┤
│ Private Methods:                                                 │
│   - GetTaskCount(): Integer                                      │
│   - DoAutoSave(): void                                           │
│   - NotifyTaskAdded(ATask: TTask): void                          │
│   - NotifyTaskUpdated(ATask: TTask): void                        │
│   - NotifyTaskDeleted(const ATaskID: string): void               │
│   - NotifyTaskCompleted(ATask: TTask): void                      │
│   - ValidateTaskID(const ATaskID: string): Boolean               │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- Inherits from: `TObject`
- Central facade for all task management operations
- Uses composition with `TTaskList`, `ITaskStorage`, `TTaskValidator`, `TTaskStatistics`
- Implements event system for task state changes
- Supports auto-save functionality
- Thread-safe for read operations (write operations should be synchronized externally)

#### 9.3.2 TTaskFilter Class Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                        TTaskFilter                               │
├─────────────────────────────────────────────────────────────────┤
│ Private Fields:                                                  │
│   FCriteria: TTaskFilterCriteria                                 │
│   FMatchCount: Integer                                           │
├─────────────────────────────────────────────────────────────────┤
│ Public Properties:                                               │
│   property Criteria: TTaskFilterCriteria read FCriteria          │
│     write FCriteria                                              │
│   property MatchCount: Integer read FMatchCount                  │
├─────────────────────────────────────────────────────────────────┤
│ Public Constructors:                                             │
│   + Create(): TTaskFilter                                        │
│   + Create(ACriteria: TTaskFilterCriteria): TTaskFilter          │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Filtering:                                      │
│   + Filter(ATaskList: TTaskList): TTaskList                      │
│   + Matches(ATask: TTask): Boolean                               │
│   + MatchesStatus(ATask: TTask): Boolean                         │
│   + MatchesPriority(ATask: TTask): Boolean                       │
│   + MatchesCategory(ATask: TTask): Boolean                       │
│   + MatchesDateRange(ATask: TTask): Boolean                      │
│   + MatchesTags(ATask: TTask): Boolean                           │
│   + MatchesSearchQuery(ATask: TTask): Boolean                    │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Criteria Building:                              │
│   + SetStatusFilter(AStatus: TTaskStatus): TTaskFilter           │
│   + SetPriorityFilter(                                           │
│       APriority: TTaskPriority                                   │
│     ): TTaskFilter                                               │
│   + SetCategoryFilter(                                           │
│       ACategory: TTaskCategory                                   │
│     ): TTaskFilter                                               │
│   + SetDateRangeFilter(                                          │
│       AStartDate, AEndDate: TDateTime                            │
│     ): TTaskFilter                                               │
│   + SetTagFilter(const ATag: string): TTaskFilter                │
│   + SetSearchQuery(const AQuery: string): TTaskFilter            │
│   + SetOverdueFilter(): TTaskFilter                              │
│   + SetCompletedFilter(): TTaskFilter                            │
│   + ClearFilters(): TTaskFilter                                  │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Advanced:                                       │
│   + AddCustomFilter(                                             │
│       AFilterFunc: TTaskFilterFunction                           │
│     ): TTaskFilter                                               │
│   + CombineWith(                                                 │
│       AOther: TTaskFilter;                                       │
│       AOperation: TFilterCombineOp                               │
│     ): TTaskFilter                                               │
│   + Clone(): TTaskFilter                                         │
├─────────────────────────────────────────────────────────────────┤
│ Private Methods:                                                 │
│   - ApplyFilters(ATask: TTask): Boolean                          │
│   - MatchesText(                                                 │
│       const AText, AQuery: string                                │
│     ): Boolean                                                   │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- Inherits from: `TObject`
- Fluent interface for building filter criteria
- Supports method chaining
- Can combine multiple filters with AND/OR logic
- Immutable filter operations (returns new instances)

#### 9.3.3 TTaskValidator Class Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                      TTaskValidator                              │
├─────────────────────────────────────────────────────────────────┤
│ Private Fields:                                                  │
│   FMinTitleLength: Integer                                       │
│   FMaxTitleLength: Integer                                       │
│   FMinDescriptionLength: Integer                                 │
│   FMaxDescriptionLength: Integer                                 │
│   FRequireDueDate: Boolean                                       │
│   FRequireCategory: Boolean                                      │
│   FAllowPastDueDates: Boolean                                    │
│   FCustomValidators: TList                                       │
├─────────────────────────────────────────────────────────────────┤
│ Public Properties:                                               │
│   property MinTitleLength: Integer read FMinTitleLength          │
│     write FMinTitleLength                                        │
│   property MaxTitleLength: Integer read FMaxTitleLength          │
│     write FMaxTitleLength                                        │
│   property RequireDueDate: Boolean read FRequireDueDate          │
│     write FRequireDueDate                                        │
│   property RequireCategory: Boolean read FRequireCategory        │
│     write FRequireCategory                                       │
│   property AllowPastDueDates: Boolean read FAllowPastDueD...     │
│     write FAllowPastDueDates                                     │
├─────────────────────────────────────────────────────────────────┤
│ Public Constructors:                                             │
│   + Create(): TTaskValidator                                     │
├─────────────────────────────────────────────────────────────────┤
│ Public Destructor:                                               │
│   + Destroy(): void override                                     │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Validation:                                     │
│   + Validate(ATask: TTask): TValidationResult                    │
│   + ValidateTitle(const ATitle: string): TValidationResult       │
│   + ValidateDescription(                                         │
│       const ADescription: string                                 │
│     ): TValidationResult                                         │
│   + ValidateDueDate(ADueDate: TDateTime): TValidationResult      │
│   + ValidatePriority(                                            │
│       APriority: TTaskPriority                                   │
│     ): TValidationResult                                         │
│   + ValidateCategory(                                            │
│       ACategory: TTaskCategory                                   │
│     ): TValidationResult                                         │
│   + ValidateStatus(AStatus: TTaskStatus): TValidationResult      │
│   + ValidateTags(ATags: TStringList): TValidationResult          │
│   + ValidateEstimatedHours(                                      │
│       AHours: Double                                             │
│     ): TValidationResult                                         │
│   + IsValid(ATask: TTask): Boolean                               │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Custom Validation:                              │
│   + AddCustomValidator(                                          │
│       AValidator: TCustomValidatorProc                           │
│     ): void                                                      │
│   + RemoveCustomValidator(                                       │
│       AValidator: TCustomValidatorProc                           │
│     ): void                                                      │
│   + ClearCustomValidators(): void                                │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Configuration:                                  │
│   + SetDefaultRules(): void                                      │
│   + SetStrictRules(): void                                       │
│   + SetLenientRules(): void                                      │
│   + LoadRulesFromConfig(const AFileName: string): Boolean        │
│   + SaveRulesToConfig(const AFileName: string): Boolean          │
├─────────────────────────────────────────────────────────────────┤
│ Private Methods:                                                 │
│   - ValidateLength(                                              │
│       const AText: string;                                       │
│       AMin, AMax: Integer;                                       │
│       const AFieldName: string                                   │
│     ): TValidationResult                                         │
│   - AddError(                                                    │
│       var AResult: TValidationResult;                            │
│       const AMessage: string                                     │
│     ): void                                                      │
│   - AddWarning(                                                  │
│       var AResult: TValidationResult;                            │
│       const AMessage: string                                     │
│     ): void                                                      │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- Inherits from: `TObject`
- Configurable validation rules
- Supports custom validators via procedural callbacks
- Returns detailed validation results with errors and warnings
- Can load/save validation configuration

#### 9.3.4 TTaskStatistics Class Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                     TTaskStatistics                              │
├─────────────────────────────────────────────────────────────────┤
│ Private Fields:                                                  │
│   FTaskList: TTaskList                                           │
│   FCachedData: TTaskStatisticsData                               │
│   FCacheValid: Boolean                                           │
├─────────────────────────────────────────────────────────────────┤
│ Public Properties:                                               │
│   property TaskList: TTaskList read FTaskList write SetTask...   │
├─────────────────────────────────────────────────────────────────┤
│ Public Constructors:                                             │
│   + Create(ATaskList: TTaskList): TTaskStatistics                │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Basic Statistics:                               │
│   + GetTotalTasks(): Integer                                     │
│   + GetCompletedTasks(): Integer                                 │
│   + GetPendingTasks(): Integer                                   │
│   + GetInProgressTasks(): Integer                                │
│   + GetCancelledTasks(): Integer                                 │
│   + GetOverdueTasks(): Integer                                   │
│   + GetCompletionRate(): Double                                  │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - By Priority:                                    │
│   + GetTasksByPriority(                                          │
│       APriority: TTaskPriority                                   │
│     ): Integer                                                   │
│   + GetHighPriorityTasks(): Integer                              │
│   + GetMediumPriorityTasks(): Integer                            │
│   + GetLowPriorityTasks(): Integer                               │
│   + GetPriorityDistribution(): TPriorityDistribution             │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - By Category:                                    │
│   + GetTasksByCategory(                                          │
│       ACategory: TTaskCategory                                   │
│     ): Integer                                                   │
│   + GetCategoryDistribution(): TCategoryDistribution             │
│   + GetMostUsedCategory(): TTaskCategory                         │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Time-Based:                                     │
│   + GetAverageCompletionTime(): Double                           │
│   + GetMedianCompletionTime(): Double                            │
│   + GetTasksCreatedToday(): Integer                              │
│   + GetTasksCreatedThisWeek(): Integer                           │
│   + GetTasksCreatedThisMonth(): Integer                          │
│   + GetTasksCompletedToday(): Integer                            │
│   + GetTasksCompletedThisWeek(): Integer                         │
│   + GetTasksCompletedThisMonth(): Integer                        │
│   + GetTasksDueThisWeek(): Integer                               │
│   + GetOldestTask(): TTask                                       │
│   + GetNewestTask(): TTask                                       │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Advanced Analytics:                             │
│   + GetProductivityScore(): Double                               │
│   + GetTaskVelocity(): Double                                    │
│   + GetAverageTasksPerDay(): Double                              │
│   + GetCompletionTrend(ADays: Integer): TArray<Double>           │
│   + GetBusiestDayOfWeek(): Integer                               │
│   + GetMostProductiveHour(): Integer                             │
│   + GetTagFrequency(): TStringIntegerMap                         │
│   + GetEstimatedVsActualHours(): TEstimateAccuracy               │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods - Reporting:                                      │
│   + GetFullStatistics(): TTaskStatisticsData                     │
│   + GenerateReport(                                              │
│       AFormat: TReportFormat                                     │
│     ): string                                                    │
│   + ExportStatistics(const AFileName: string): Boolean           │
├─────────────────────────────────────────────────────────────────┤
│ Private Methods:                                                 │
│   - SetTaskList(AValue: TTaskList): void                         │
│   - InvalidateCache(): void                                      │
│   - CalculateStatistics(): void                                  │
│   - GetTasksInDateRange(                                         │
│       AStart, AEnd: TDateTime                                    │
│     ): Integer                                                   │
│   - CalculateCompletionTime(ATask: TTask): Double                │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- Inherits from: `TObject`
- Provides comprehensive analytics on task data
- Implements caching for expensive calculations
- Supports various reporting formats
- Can analyze productivity trends over time

### 9.4 Storage Layer Classes

#### 9.4.1 ITaskStorage Interface Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                      ITaskStorage                                │
│                      <<interface>>                               │
├─────────────────────────────────────────────────────────────────┤
│ Methods:                                                         │
│   + SaveTasks(                                                   │
│       ATaskList: TTaskList;                                      │
│       const AFileName: string                                    │
│     ): Boolean                                                   │
│   + LoadTasks(                                                   │
│       ATaskList: TTaskList;                                      │
│       const AFileName: string                                    │
│     ): Boolean                                                   │
│   + GetFormatName(): string                                      │
│   + GetFileExtension(): string                                   │
│   + SupportsCompression(): Boolean                               │
│   + SetCompression(AEnabled: Boolean): void                      │
│   + Validate(const AFileName: string): Boolean                   │
│   + GetLastError(): string                                       │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- Interface (no implementation)
- Defines contract for all storage implementations
- Supports validation before loading
- Error reporting through `GetLastError()`

#### 9.4.2 TJSONTaskStorage Class Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    TJSONTaskStorage                              │
├─────────────────────────────────────────────────────────────────┤
│ Private Fields:                                                  │
│   FPrettyPrint: Boolean                                          │
│   FCompression: Boolean                                          │
│   FLastError: string                                             │
│   FEncoding: TEncoding                                           │
├─────────────────────────────────────────────────────────────────┤
│ Public Properties:                                               │
│   property PrettyPrint: Boolean read FPrettyPrint write FPr...   │
│   property Compression: Boolean read FCompression write FCo...   │
│   property Encoding: TEncoding read FEncoding write FEncoding    │
├─────────────────────────────────────────────────────────────────┤
│ Public Constructors:                                             │
│   + Create(): TJSONTaskStorage                                   │
├─────────────────────────────────────────────────────────────────┤
│ Interface Methods (ITaskStorage):                                │
│   + SaveTasks(                                                   │
│       ATaskList: TTaskList;                                      │
│       const AFileName: string                                    │
│     ): Boolean                                                   │
│   + LoadTasks(                                                   │
│       ATaskList: TTaskList;                                      │
│       const AFileName: string                                    │
│     ): Boolean                                                   │
│   + GetFormatName(): string                                      │
│   + GetFileExtension(): string                                   │
│   + SupportsCompression(): Boolean                               │
│   + SetCompression(AEnabled: Boolean): void                      │
│   + Validate(const AFileName: string): Boolean                   │
│   + GetLastError(): string                                       │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods:                                                  │
│   + TaskToJSON(ATask: TTask): TJSONObject                        │
│   + JSONToTask(AJSON: TJSONObject): TTask                        │
│   + TaskListToJSON(ATaskList: TTaskList): TJSONArray             │
│   + JSONToTaskList(                                              │
│       AJSON: TJSONArray;                                         │
│       ATaskList: TTaskList                                       │
│     ): Boolean                                                   │
├─────────────────────────────────────────────────────────────────┤
│ Private Methods:                                                 │
│   - SerializeTask(ATask: TTask): TJSONObject                     │
│   - DeserializeTask(AJSON: TJSONObject): TTask                   │
│   - CompressData(const AData: string): string                    │
│   - DecompressData(const AData: string): string                  │
│   - SetLastError(const AError: string): void                     │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- Inherits from: `TInterfacedObject`
- Implements: `ITaskStorage`
- Uses fpjson unit for JSON parsing
- Supports pretty-printing and compression
- UTF-8 encoding by default

#### 9.4.3 TXMLTaskStorage Class Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                     TXMLTaskStorage                              │
├─────────────────────────────────────────────────────────────────┤
│ Private Fields:                                                  │
│   FIndentation: Integer                                          │
│   FCompression: Boolean                                          │
│   FLastError: string                                             │
│   FValidateSchema: Boolean                                       │
│   FSchemaFile: string                                            │
├─────────────────────────────────────────────────────────────────┤
│ Public Properties:                                               │
│   property Indentation: Integer read FIndentation write FIn...   │
│   property ValidateSchema: Boolean read FValidateSchema w...     │
│   property SchemaFile: string read FSchemaFile write FSchem...   │
├─────────────────────────────────────────────────────────────────┤
│ Public Constructors:                                             │
│   + Create(): TXMLTaskStorage                                    │
├─────────────────────────────────────────────────────────────────┤
│ Interface Methods (ITaskStorage):                                │
│   + SaveTasks(                                                   │
│       ATaskList: TTaskList;                                      │
│       const AFileName: string                                    │
│     ): Boolean                                                   │
│   + LoadTasks(                                                   │
│       ATaskList: TTaskList;                                      │
│       const AFileName: string                                    │
│     ): Boolean                                                   │
│   + GetFormatName(): string                                      │
│   + GetFileExtension(): string                                   │
│   + SupportsCompression(): Boolean                               │
│   + SetCompression(AEnabled: Boolean): void                      │
│   + Validate(const AFileName: string): Boolean                   │
│   + GetLastError(): string                                       │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods:                                                  │
│   + TaskToXML(ATask: TTask): TDOMNode                            │
│   + XMLToTask(ANode: TDOMNode): TTask                            │
│   + CreateXMLDocument(                                           │
│       ATaskList: TTaskList                                       │
│     ): TXMLDocument                                              │
│   + ParseXMLDocument(                                            │
│       ADoc: TXMLDocument;                                        │
│       ATaskList: TTaskList                                       │
│     ): Boolean                                                   │
├─────────────────────────────────────────────────────────────────┤
│ Private Methods:                                                 │
│   - SerializeTask(                                               │
│       ATask: TTask;                                              │
│       ADoc: TXMLDocument                                         │
│     ): TDOMNode                                                  │
│   - DeserializeTask(ANode: TDOMNode): TTask                      │
│   - ValidateAgainstSchema(                                       │
│       const AFileName: string                                    │
│     ): Boolean                                                   │
│   - SetLastError(const AError: string): void                     │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- Inherits from: `TInterfacedObject`
- Implements: `ITaskStorage`
- Uses DOM (Document Object Model) for XML
- Optional XSD schema validation
- Supports configurable indentation

#### 9.4.4 TCSVTaskStorage Class Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                     TCSVTaskStorage                              │
├─────────────────────────────────────────────────────────────────┤
│ Private Fields:                                                  │
│   FDelimiter: Char                                               │
│   FQuoteChar: Char                                               │
│   FIncludeHeader: Boolean                                        │
│   FLastError: string                                             │
│   FCompression: Boolean                                          │
├─────────────────────────────────────────────────────────────────┤
│ Public Properties:                                               │
│   property Delimiter: Char read FDelimiter write FDelimiter      │
│   property QuoteChar: Char read FQuoteChar write FQuoteChar      │
│   property IncludeHeader: Boolean read FIncludeHeader write...   │
├─────────────────────────────────────────────────────────────────┤
│ Public Constructors:                                             │
│   + Create(): TCSVTaskStorage                                    │
├─────────────────────────────────────────────────────────────────┤
│ Interface Methods (ITaskStorage):                                │
│   + SaveTasks(                                                   │
│       ATaskList: TTaskList;                                      │
│       const AFileName: string                                    │
│     ): Boolean                                                   │
│   + LoadTasks(                                                   │
│       ATaskList: TTaskList;                                      │
│       const AFileName: string                                    │
│     ): Boolean                                                   │
│   + GetFormatName(): string                                      │
│   + GetFileExtension(): string                                   │
│   + SupportsCompression(): Boolean                               │
│   + SetCompression(AEnabled: Boolean): void                      │
│   + Validate(const AFileName: string): Boolean                   │
│   + GetLastError(): string                                       │
├─────────────────────────────────────────────────────────────────┤
│ Public Methods:                                                  │
│   + TaskToCSVRow(ATask: TTask): string                           │
│   + CSVRowToTask(const ARow: string): TTask                      │
│   + GetHeaderRow(): string                                       │
├─────────────────────────────────────────────────────────────────┤
│ Private Methods:                                                 │
│   - EscapeCSVField(const AField: string): string                 │
│   - UnescapeCSVField(const AField: string): string               │
│   - ParseCSVRow(const ARow: string): TStringArray                │
│   - SetLastError(const AError: string): void                     │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- Inherits from: `TInterfacedObject`
- Implements: `ITaskStorage`
- Configurable delimiter and quote characters
- RFC 4180 compliant CSV parsing
- Optional header row
- Note: Limited support for complex structures (tags, subtasks stored as delimited strings)

### 9.5 Utility Classes

#### 9.5.1 TTaskUtils Class Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                       TTaskUtils                                 │
│                    <<static class>>                              │
├─────────────────────────────────────────────────────────────────┤
│ Class Methods - ID Generation:                                   │
│   + class function GenerateGUID(): string; static;               │
│   + class function GenerateShortID(): string; static;            │
│   + class function GenerateSequentialID(                         │
│       APrefix: string                                            │
│     ): string; static;                                           │
├─────────────────────────────────────────────────────────────────┤
│ Class Methods - Date/Time:                                       │
│   + class function FormatDateTime(                               │
│       ADateTime: TDateTime                                       │
│     ): string; static;                                           │
│   + class function ParseDateTime(                                │
│       const ADateTimeStr: string                                 │
│     ): TDateTime; static;                                        │
│   + class function GetStartOfDay(                                │
│       ADate: TDateTime                                           │
│     ): TDateTime; static;                                        │
│   + class function GetEndOfDay(                                  │
│       ADate: TDateTime                                           │
│     ): TDateTime; static;                                        │
│   + class function GetStartOfWeek(                               │
│       ADate: TDateTime                                           │
│     ): TDateTime; static;                                        │
│   + class function GetEndOfWeek(                                 │
│       ADate: TDateTime                                           │
│     ): TDateTime; static;                                        │
│   + class function DaysBetween(                                  │
│       ADate1, ADate2: TDateTime                                  │
│     ): Integer; static;                                          │
│   + class function IsToday(ADate: TDateTime): Boolean; static;   │
│   + class function IsThisWeek(                                   │
│       ADate: TDateTime                                           │
│     ): Boolean; static;                                          │
├─────────────────────────────────────────────────────────────────┤
│ Class Methods - String Utilities:                                │
│   + class function TrimString(                                   │
│       const AStr: string                                         │
│     ): string; static;                                           │
│   + class function CompareStringsIgnoreCase(                     │
│       const AStr1, AStr2: string                                 │
│     ): Integer; static;                                          │
│   + class function ContainsText(                                 │
│       const AText, ASubText: string                              │
│     ): Boolean; static;                                          │
│   + class function SplitString(                                  │
│       const AStr: string;                                        │
│       ADelimiter: Char                                           │
│     ): TStringArray; static;                                     │
│   + class function JoinStrings(                                  │
│       AStrings: TStringArray;                                    │
│       const ASeparator: string                                   │
│     ): string; static;                                           │
├─────────────────────────────────────────────────────────────────┤
│ Class Methods - Enum Conversions:                                │
│   + class function StatusToString(                               │
│       AStatus: TTaskStatus                                       │
│     ): string; static;                                           │
│   + class function StringToStatus(                               │
│       const AStr: string                                         │
│     ): TTaskStatus; static;                                      │
│   + class function PriorityToString(                             │
│       APriority: TTaskPriority                                   │
│     ): string; static;                                           │
│   + class function StringToPriority(                             │
│       const AStr: string                                         │
│     ): TTaskPriority; static;                                    │
│   + class function CategoryToString(                             │
│       ACategory: TTaskCategory                                   │
│     ): string; static;                                           │
│   + class function StringToCategory(                             │
│       const AStr: string                                         │
│     ): TTaskCategory; static;                                    │
├─────────────────────────────────────────────────────────────────┤
│ Class Methods - Validation:                                      │
│   + class function IsValidEmail(                                 │
│       const AEmail: string                                       │
│     ): Boolean; static;                                          │
│   + class function IsValidURL(                                   │
│       const AURL: string                                         │
│     ): Boolean; static;                                          │
│   + class function IsValidGUID(                                  │
│       const AGUID: string                                        │
│     ): Boolean; static;                                          │
├─────────────────────────────────────────────────────────────────┤
│ Class Methods - File Utilities:                                  │
│   + class function GetFileExtension(                             │
│       const AFileName: string                                    │
│     ): string; static;                                           │
│   + class function ChangeFileExtension(                          │
│       const AFileName, ANewExt: string                           │
│     ): string; static;                                           │
│   + class function FileExists(                                   │
│       const AFileName: string                                    │
│     ): Boolean; static;                                          │
│   + class function CreateBackupFile(                             │
│       const AFileName: string                                    │
│     ): Boolean; static;                                          │
└─────────────────────────────────────────────────────────────────┘
```

**Key Characteristics:**
- All methods are class methods (static)
- No instance creation needed
- Provides utility functions used across the library
- Pure functions (no side effects)

### 9.6 Enumerations and Records

#### 9.6.1 Enumeration Type Definitions

```pascal
type
  // Task status enumeration
  TTaskStatus = (
    tsPending,      // Task created but not started
    tsInProgress,   // Task actively being worked on
    tsCompleted,    // Task finished successfully
    tsCancelled,    // Task cancelled/abandoned
    tsOnHold        // Task temporarily paused
  );

  // Task priority enumeration
  TTaskPriority = (
    tpLow,          // Low priority task
    tpMedium,       // Medium priority task
    tpHigh,         // High priority task
    tpCritical      // Critical/urgent task
  );

  // Task category enumeration
  TTaskCategory = (
    tcWork,         // Work-related task
    tcPersonal,     // Personal task
    tcShopping,     // Shopping/errands
    tcHealth,       // Health/fitness
    tcEducation,    // Learning/education
    tcHousehold,    // Household chores
    tcOther         // Miscellaneous
  );

  // Storage format enumeration
  TStorageFormat = (
    sfJSON,         // JSON format
    sfXML,          // XML format
    sfCSV           // CSV format
  );

  // Sort order enumeration
  TSortOrder = (
    soAscending,    // A-Z, 0-9, oldest-newest
    soDescending    // Z-A, 9-0, newest-oldest
  );

  // Sort field enumeration
  TSortField = (
    sfTitle,        // Sort by task title
    sfPriority,     // Sort by priority
    sfDueDate,      // Sort by due date
    sfStatus,       // Sort by status
    sfCreatedDate,  // Sort by creation date
    sfCategory      // Sort by category
  );
```

#### 9.6.2 Record Type Definitions

```pascal
type
  // Validation result record
  TValidationResult = record
    IsValid: Boolean;
    Errors: TStringList;
    Warnings: TStringList;
    
    procedure AddError(const AMessage: string);
    procedure AddWarning(const AMessage: string);
    procedure Clear;
    function HasErrors: Boolean;
    function HasWarnings: Boolean;
    function GetErrorCount: Integer;
    function GetWarningCount: Integer;
    function ToString: string;
  end;

  // Task filter criteria record
  TTaskFilterCriteria = record
    Status: TTaskStatus;
    Priority: TTaskPriority;
    Category: TTaskCategory;
    StartDate: TDateTime;
    EndDate: TDateTime;
    SearchQuery: string;
    Tags: TStringList;
    IncludeCompleted: Boolean;
    IncludeCancelled: Boolean;
    OnlyOverdue: Boolean;
    
    procedure Clear;
    function IsEmpty: Boolean;
  end;

  // Task statistics data record
  TTaskStatisticsData = record
    TotalTasks: Integer;
    CompletedTasks: Integer;
    PendingTasks: Integer;
    InProgressTasks: Integer;
    CancelledTasks: Integer;
    OverdueTasks: Integer;
    CompletionRate: Double;
    AverageCompletionTime: Double;
    HighPriorityCount: Integer;
    MediumPriorityCount: Integer;
    LowPriorityCount: Integer;
    TasksByCategory: array[TTaskCategory] of Integer;
    
    procedure Clear;
    function ToString: string;
  end;
```

### 9.7 Class Relationship Diagram

```
                    ┌──────────────┐
                    │ TTaskManager │
                    └──────┬───────┘
                           │
                           │ owns
                           ▼
        ┌──────────────────┴──────────────────┐
        │                                      │
        ▼                                      ▼
   ┌──────────┐                        ┌──────────────┐
   │ TTaskList│◄───────────────────────│ ITaskStorage │
   └────┬─────┘                        └──────┬───────┘
        │                                     │
        │ contains                            │ implements
        │ multiple                            │
        ▼                                     ▼
   ┌────────┐           ┌───────────────────────────────┐
   │ TTask  │           │  ┌──────────────────────┐     │
   └────────┘           │  │ TJSONTaskStorage     │     │
        │               │  ├──────────────────────┤     │
        │ uses          │  │ TXMLTaskStorage      │     │
        ▼               │  ├──────────────────────┤     │
   ┌──────────────┐    │  │ TCSVTaskStorage      │     │
   │ TTaskUtils   │    │  └──────────────────────┘     │
   │ (static)     │    └───────────────────────────────┘
   └──────────────┘
        ▲
        │ uses
        │
   ┌────┴─────────┐
   │              │
   ▼              ▼
┌────────────┐ ┌──────────────┐
│ TTaskFilter│ │TTaskValidator│
└────────────┘ └──────────────┘
   ▲              ▲
   │              │
   │ uses         │ uses
   │              │
   └──────┬───────┘
          │
          ▼
   ┌──────────────────┐
   │ TTaskStatistics  │
   └──────────────────┘
```

### 9.8 Inheritance Hierarchy

```
TObject (FPC RTL)
│
├─ TTask
│
├─ TTaskList
│
├─ TTaskManager
│
├─ TTaskFilter
│
├─ TTaskValidator
│
├─ TTaskStatistics
│
└─ TInterfacedObject
    │
    ├─ TJSONTaskStorage (implements ITaskStorage)
    │
    ├─ TXMLTaskStorage (implements ITaskStorage)
    │
    └─ TCSVTaskStorage (implements ITaskStorage)
```

### 9.9 Method Signature Details

#### 9.9.1 Key Type Definitions for Method Signatures

```pascal
type
  // Event handler types
  TTaskEvent = procedure(ATask: TTask) of object;
  TTaskFilterFunction = function(ATask: TTask): Boolean;
  TTaskComparator = function(Task1, Task2: TTask): Integer;
  TCustomValidatorProc = procedure(
    ATask: TTask; 
    var AResult: TValidationResult
  );

  // Array types
  TStringArray = array of string;
  TTaskArray = array of TTask;

  // Map types (simplified - could use generics)
  TStringIntegerMap = class
    // Maps string keys to integer values
    // Used for tag frequency, etc.
  end;

  // Distribution types
  TPriorityDistribution = record
    Low: Integer;
    Medium: Integer;
    High: Integer;
    Critical: Integer;
  end;

  TCategoryDistribution = array[TTaskCategory] of Integer;

  TTaskDistribution = record
    ByStatus: array[TTaskStatus] of Integer;
    ByPriority: TPriorityDistribution;
    ByCategory: TCategoryDistribution;
  end;

  TEstimateAccuracy = record
    TotalTasks: Integer;
    AverageEstimatedHours: Double;
    AverageActualHours: Double;
    AccuracyPercentage: Double;
  end;

  // Report format
  TReportFormat = (
    rfPlainText,
    rfJSON,
    rfXML,
    rfHTML,
    rfMarkdown
  );

  // Filter combination operation
  TFilterCombineOp = (
    fcoAnd,  // Both filters must match
    fcoOr,   // Either filter must match
    fcoXor   // Exactly one filter must match
  );

  // Encoding type
  TEncoding = (
    encUTF8,
    encUTF16,
    encASCII
  );
```

### 9.10 Property Access Methods

Most properties use private fields with getter/setter methods for:

1. **Validation**: Setters can validate data before assignment
2. **Side Effects**: Setters can trigger updates (e.g., updating `UpdatedAt` timestamp)
3. **Lazy Loading**: Getters can calculate or load data on-demand
4. **Encapsulation**: Prevent direct field access

**Example Pattern:**
```pascal
type
  TTask = class
  private
    FTitle: string;
    procedure SetTitle(const AValue: string);
  public
    property Title: string read FTitle write SetTitle;
  end;

procedure TTask.SetTitle(const AValue: string);
begin
  if FTitle <> AValue then
  begin
    FTitle := AValue;
    UpdateTimestamp;  // Side effect: update timestamp
  end;
end;
```

### 9.11 Memory Management Notes

1. **TTask**: Owns its `TStringList` instances (Tags, Subtasks) - frees them in destructor
2. **TTaskList**: By default owns contained `TTask` objects (controlled by `OwnsObjects` property)
3. **TTaskManager**: Owns its `TTaskList`, `TTaskValidator`, and `TTaskStatistics` instances
4. **Filter Results**: Methods like `FilterByStatus` return new `TTaskList` instances - caller must free
5. **Storage Implementations**: Implement reference counting via `TInterfacedObject`
6. **Validation Results**: `TValidationResult` owns its `TStringList` instances (Errors, Warnings)

**Best Practice:** Always use try-finally blocks when working with returned objects:

```pascal
var
  FilteredTasks: TTaskList;
begin
  FilteredTasks := TaskManager.FilterTasks(MyCriteria);
  try
    // Use FilteredTasks
  finally
    FilteredTasks.Free;
  end;
end;
```

---


### 9.12 Sequence Diagrams and Interaction Patterns

This section provides detailed sequence diagrams showing how different components interact during common operations. These diagrams illustrate the dynamic behavior of the system and help developers understand the flow of control and data between objects.

#### 9.12.1 Task Creation and Persistence Flow

This sequence diagram shows the complete flow of creating a task and persisting it to storage:

```
┌─────────┐  ┌──────────────┐  ┌──────────┐  ┌──────────────┐  ┌─────────────┐
│ Client  │  │ TTaskManager │  │  TTask   │  │ TTaskValidator│  │ITaskStorage │
└────┬────┘  └──────┬───────┘  └────┬─────┘  └──────┬───────┘  └──────┬──────┘
     │               │               │               │                  │
     │ CreateTask()  │               │               │                  │
     │──────────────>│               │               │                  │
     │               │               │               │                  │
     │               │ Create()      │               │                  │
     │               │──────────────>│               │                  │
     │               │               │               │                  │
     │               │<──────────────│               │                  │
     │               │   TTask obj   │               │                  │
     │               │               │               │                  │
     │               │ SetTitle()    │               │                  │
     │               │──────────────>│               │                  │
     │               │               │               │                  │
     │               │ SetPriority() │               │                  │
     │               │──────────────>│               │                  │
     │               │               │               │                  │
     │               │ ValidateTask()│               │                  │
     │               │───────────────────────────────>│                  │
     │               │               │               │                  │
     │               │               │               │ Validate rules   │
     │               │               │               │─┐                │
     │               │               │               │ │                │
     │               │               │               │<┘                │
     │               │               │               │                  │
     │               │<───────────────────────────────│                  │
     │               │ TValidationResult             │                  │
     │               │               │               │                  │
     │               │[Valid]        │               │                  │
     │               │ Add to list   │               │                  │
     │               │─┐             │               │                  │
     │               │ │             │               │                  │
     │               │<┘             │               │                  │
     │               │               │               │                  │
     │               │[AutoSave]     │               │                  │
     │               │ SaveTasks()   │               │                  │
     │               │───────────────────────────────────────────────────>│
     │               │               │               │                  │
     │               │               │               │   Serialize &    │
     │               │               │               │   Write to file  │
     │               │               │               │        ─┐        │
     │               │               │               │         │        │
     │               │               │               │        <┘        │
     │               │               │               │                  │
     │               │<───────────────────────────────────────────────────│
     │               │           Success             │                  │
     │               │               │               │                  │
     │<──────────────│               │               │                  │
     │   Task ID     │               │               │                  │
     │               │               │               │                  │
```

**Key Points**:
1. Client calls `CreateTask()` on `TTaskManager`
2. Manager creates a new `TTask` instance
3. Manager sets task properties (title, priority, etc.)
4. Manager validates the task using `TTaskValidator`
5. If valid, task is added to internal task list
6. If `AutoSave` is enabled, changes are persisted via `ITaskStorage`
7. Task ID is returned to client

#### 9.12.2 Task Filtering and Retrieval Flow

This diagram shows how filtering operations work across multiple components:

```
┌─────────┐  ┌──────────────┐  ┌─────────────┐  ┌──────────┐
│ Client  │  │ TTaskManager │  │ TTaskFilter │  │TTaskList │
└────┬────┘  └──────┬───────┘  └──────┬──────┘  └────┬─────┘
     │               │                 │              │
     │ GetTasksByStatus(tsPending)     │              │
     │──────────────>│                 │              │
     │               │                 │              │
     │               │ FilterByStatus()│              │
     │               │────────────────>│              │
     │               │                 │              │
     │               │                 │ GetAllTasks()│
     │               │                 │─────────────>│
     │               │                 │              │
     │               │                 │<─────────────│
     │               │                 │  Task list   │
     │               │                 │              │
     │               │                 │ Create result list
     │               │                 │─┐            │
     │               │                 │ │            │
     │               │                 │<┘            │
     │               │                 │              │
     │               │                 │ For each task│
     │               │                 │─┐            │
     │               │                 │ │ Check      │
     │               │                 │ │ status     │
     │               │                 │ │ If match,  │
     │               │                 │ │ add to     │
     │               │                 │ │ result     │
     │               │                 │<┘            │
     │               │                 │              │
     │               │<────────────────│              │
     │               │ Filtered TTaskList            │
     │               │                 │              │
     │<──────────────│                 │              │
     │ TTaskList     │                 │              │
     │               │                 │              │
```

**Key Points**:
1. Client requests tasks filtered by status
2. Manager delegates to `TTaskFilter`
3. Filter retrieves all tasks from manager's task list
4. Filter iterates through tasks, checking each against criteria
5. Matching tasks are added to a new result `TTaskList`
6. Result list is returned to client (client owns this object)

#### 9.12.3 Complex Multi-Criteria Filtering

This shows advanced filtering with multiple criteria:

```
┌─────────┐  ┌──────────────┐  ┌─────────────┐  ┌──────────┐
│ Client  │  │ TTaskManager │  │ TTaskFilter │  │TTaskList │
└────┬────┘  └──────┬───────┘  └──────┬──────┘  └────┬─────┘
     │               │                 │              │
     │ Build filter criteria           │              │
     │─┐             │                 │              │
     │ │ Criteria.Status := tsPending  │              │
     │ │ Criteria.Priority := tpHigh   │              │
     │ │ Criteria.DueDateFrom := Now   │              │
     │<┘             │                 │              │
     │               │                 │              │
     │ FilterTasks(Criteria)           │              │
     │──────────────>│                 │              │
     │               │                 │              │
     │               │ ApplyFilter()   │              │
     │               │────────────────>│              │
     │               │                 │              │
     │               │                 │ GetAllTasks()│
     │               │                 │─────────────>│
     │               │                 │              │
     │               │                 │<─────────────│
     │               │                 │              │
     │               │                 │ For each task│
     │               │                 │─┐            │
     │               │                 │ │ Check all  │
     │               │                 │ │ criteria:  │
     │               │                 │ │ - Status?  │
     │               │                 │ │ - Priority?│
     │               │                 │ │ - DueDate? │
     │               │                 │ │ Add if ALL │
     │               │                 │ │ match      │
     │               │                 │<┘            │
     │               │                 │              │
     │               │<────────────────│              │
     │               │ Filtered list   │              │
     │               │                 │              │
     │<──────────────│                 │              │
     │ TTaskList     │                 │              │
     │ (3 matches)   │                 │              │
     │               │                 │              │
```

**Key Points**:
1. Client builds `TTaskFilterCriteria` record with multiple conditions
2. All criteria must be satisfied (AND logic)
3. Filter applies each criterion sequentially
4. Only tasks matching ALL criteria are included in result

#### 9.12.4 Task Update with Validation Flow

This diagram shows the update process including validation and error handling:

```
┌─────────┐  ┌──────────────┐  ┌──────────┐  ┌──────────────┐  ┌─────────────┐
│ Client  │  │ TTaskManager │  │  TTask   │  │TTaskValidator│  │ITaskStorage │
└────┬────┘  └──────┬───────┘  └────┬─────┘  └──────┬───────┘  └──────┬──────┘
     │               │               │               │                  │
     │ GetTask(ID)   │               │               │                  │
     │──────────────>│               │               │                  │
     │               │               │               │                  │
     │               │ Find by ID    │               │                  │
     │               │─┐             │               │                  │
     │               │ │             │               │                  │
     │               │<┘             │               │                  │
     │               │               │               │                  │
     │<──────────────│               │               │                  │
     │   TTask ref   │               │               │                  │
     │               │               │               │                  │
     │ Modify task   │               │               │                  │
     │─┐             │               │               │                  │
     │ │ SetTitle()  │               │               │                  │
     │ │────────────────────────────>│               │                  │
     │ │ SetDueDate()│               │               │                  │
     │ │────────────────────────────>│               │                  │
     │<┘             │               │               │                  │
     │               │               │               │                  │
     │ UpdateTask()  │               │               │                  │
     │──────────────>│               │               │                  │
     │               │               │               │                  │
     │               │ ValidateTask()│               │                  │
     │               │───────────────────────────────>│                  │
     │               │               │               │                  │
     │               │               │               │ Validate all     │
     │               │               │               │ constraints      │
     │               │               │               │─┐                │
     │               │               │               │ │                │
     │               │               │               │<┘                │
     │               │               │               │                  │
     │               │<───────────────────────────────│                  │
     │               │ TValidationResult             │                  │
     │               │               │               │                  │
     │               │[If Valid]     │               │                  │
     │               │ Update timestamp              │                  │
     │               │───────────────>│               │                  │
     │               │               │               │                  │
     │               │[AutoSave]     │               │                  │
     │               │ SaveTasks()   │               │                  │
     │               │───────────────────────────────────────────────────>│
     │               │               │               │                  │
     │               │<───────────────────────────────────────────────────│
     │               │               │               │                  │
     │<──────────────│               │               │                  │
     │   Success     │               │               │                  │
     │               │               │               │                  │
     │               │               │               │                  │
     │[If Invalid]   │               │               │                  │
     │<──────────────│               │               │                  │
     │   False +     │               │               │                  │
     │   Errors      │               │               │                  │
     │               │               │               │                  │
```

**Key Points**:
1. Client retrieves task reference (not a copy)
2. Client modifies task properties directly
3. Client calls `UpdateTask()` to commit changes
4. Manager validates the modified task
5. If valid, timestamp is updated and task is saved
6. If invalid, changes remain but validation errors are returned
7. Client should handle validation errors appropriately

#### 9.12.5 Batch Operation Flow

This shows how batch operations are optimized:

```
┌─────────┐  ┌──────────────┐  ┌──────────┐  ┌─────────────┐
│ Client  │  │ TTaskManager │  │TTaskList │  │ITaskStorage │
└────┬────┘  └──────┬───────┘  └────┬─────┘  └──────┬──────┘
     │               │               │              │
     │ BulkUpdateStatus(IDs, tsCompleted)          │
     │──────────────>│               │              │
     │               │               │              │
     │               │ Disable AutoSave             │
     │               │─┐             │              │
     │               │ │             │              │
     │               │<┘             │              │
     │               │               │              │
     │               │ For each ID   │              │
     │               │─┐             │              │
     │               │ │ Find task   │              │
     │               │ │────────────>│              │
     │               │ │             │              │
     │               │ │<────────────│              │
     │               │ │             │              │
     │               │ │ Update status              │
     │               │ │ Increment counter          │
     │               │ │             │              │
     │               │<┘             │              │
     │               │               │              │
     │               │ SaveTasks()   │              │
     │               │ (Single save) │              │
     │               │───────────────────────────────>│
     │               │               │              │
     │               │<───────────────────────────────│
     │               │               │              │
     │               │ Re-enable AutoSave           │
     │               │─┐             │              │
     │               │ │             │              │
     │               │<┘             │              │
     │               │               │              │
     │<──────────────│               │              │
     │ Updated count │               │              │
     │               │               │              │
```

**Key Points**:
1. Batch operations temporarily disable `AutoSave`
2. All modifications are made in memory
3. Single save operation at the end (performance optimization)
4. Returns count of successfully updated tasks
5. `AutoSave` is re-enabled after completion

#### 9.12.6 Storage Backend Switching

This shows how to switch between different storage formats:

```
┌─────────┐  ┌──────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Client  │  │ TTaskManager │  │TJSONTaskStorage │  │ TXMLTaskStorage │
└────┬────┘  └──────┬───────┘  └────────┬────────┘  └────────┬────────┘
     │               │                   │                    │
     │ (Using JSON)  │                   │                    │
     │ LoadFromFile()│                   │                    │
     │──────────────>│                   │                    │
     │               │                   │                    │
     │               │ LoadTasks()       │                    │
     │               │──────────────────>│                    │
     │               │                   │                    │
     │               │                   │ Parse JSON         │
     │               │                   │ Create tasks       │
     │               │                   │─┐                  │
     │               │                   │ │                  │
     │               │                   │<┘                  │
     │               │                   │                    │
     │               │<──────────────────│                    │
     │               │  TTaskList        │                    │
     │               │                   │                    │
     │<──────────────│                   │                    │
     │               │                   │                    │
     │               │                   │                    │
     │ Switch to XML │                   │                    │
     │─┐             │                   │                    │
     │ │ Create TXMLTaskStorage          │                    │
     │<┘             │                   │                    │
     │               │                   │                    │
     │ SetStorage()  │                   │                    │
     │──────────────>│                   │                    │
     │               │                   │                    │
     │               │ Replace storage   │                    │
     │               │─┐                 │                    │
     │               │ │                 │                    │
     │               │<┘                 │                    │
     │               │                   │                    │
     │<──────────────│                   │                    │
     │               │                   │                    │
     │               │                   │                    │
     │ SaveToFile()  │                   │                    │
     │──────────────>│                   │                    │
     │               │                   │                    │
     │               │ SaveTasks()       │                    │
     │               │────────────────────────────────────────>│
     │               │                   │                    │
     │               │                   │     Generate XML   │
     │               │                   │     Write to file  │
     │               │                   │          ─┐        │
     │               │                   │           │        │
     │               │                   │          <┘        │
     │               │                   │                    │
     │               │<────────────────────────────────────────│
     │               │                   │                    │
     │<──────────────│                   │                    │
     │               │                   │                    │
```

**Key Points**:
1. Tasks loaded from JSON format
2. Client creates new storage instance (XML)
3. Client calls `SetStorage()` to switch backends
4. Next save operation uses XML format
5. Task data remains in memory; only storage format changes

#### 9.12.7 Error Handling and Recovery Flow

This diagram illustrates error handling during persistence operations:

```
┌─────────┐  ┌──────────────┐  ┌─────────────┐  ┌──────────────┐
│ Client  │  │ TTaskManager │  │ITaskStorage │  │  Exception   │
└────┬────┘  └──────┬───────┘  └──────┬──────┘  └──────┬───────┘
     │               │                 │                │
     │ SaveToFile()  │                 │                │
     │──────────────>│                 │                │
     │               │                 │                │
     │               │ Try SaveTasks() │                │
     │               │────────────────>│                │
     │               │                 │                │
     │               │                 │ [Disk Full]    │
     │               │                 │ Raise ETaskStorageException
     │               │                 │────────────────>│
     │               │                 │                │
     │               │<────────────────────────────────│
     │               │  Exception caught              │
     │               │                 │                │
     │               │ Log error       │                │
     │               │─┐               │                │
     │               │ │               │                │
     │               │<┘               │                │
     │               │                 │                │
     │               │ Try backup location             │
     │               │────────────────>│                │
     │               │                 │                │
     │               │                 │ [Success]      │
     │               │<────────────────│                │
     │               │                 │                │
     │               │ Return partial success          │
     │<──────────────│                 │                │
     │ False + Error │                 │                │
     │ Message       │                 │                │
     │               │                 │                │
     │ Handle error  │                 │                │
     │─┐             │                 │                │
     │ │ Display msg │                 │                │
     │ │ Retry?      │                 │                │
     │<┘             │                 │                │
     │               │                 │                │
```

**Key Points**:
1. Save operation may fail due to I/O errors
2. Manager catches `ETaskStorageException`
3. Manager attempts recovery (backup location, retry)
4. Error details are returned to client
5. Client decides how to handle the error
6. Tasks remain safely in memory even if save fails

#### 9.12.8 Task State Transition Flow

This shows valid state transitions for task status:

```
┌─────────┐  ┌──────────────┐  ┌──────────┐
│ Client  │  │ TTaskManager │  │  TTask   │
└────┬────┘  └──────┬───────┘  └────┬─────┘
     │               │               │
     │ CreateTask()  │               │
     │──────────────>│               │
     │               │               │
     │               │ Create()      │
     │               │──────────────>│
     │               │ Status=Pending│
     │               │               │
     │<──────────────│               │
     │               │               │
     │ StartTask(ID) │               │
     │──────────────>│               │
     │               │               │
     │               │ [Validate: Pending->InProgress]
     │               │─┐             │
     │               │ │ OK          │
     │               │<┘             │
     │               │               │
     │               │ SetStatus()   │
     │               │──────────────>│
     │               │ InProgress    │
     │               │               │
     │<──────────────│               │
     │               │               │
     │ CompleteTask()│               │
     │──────────────>│               │
     │               │               │
     │               │ [Validate: InProgress->Completed]
     │               │─┐             │
     │               │ │ OK          │
     │               │<┘             │
     │               │               │
     │               │ SetStatus()   │
     │               │──────────────>│
     │               │ Completed     │
     │               │ SetCompletedDate
     │               │──────────────>│
     │               │               │
     │<──────────────│               │
     │               │               │
     │               │               │
     │ [Invalid transition attempt]  │
     │ StartTask(ID) │               │
     │ (already done)│               │
     │──────────────>│               │
     │               │               │
     │               │ [Validate: Completed->InProgress]
     │               │─┐             │
     │               │ │ INVALID!    │
     │               │<┘             │
     │               │               │
     │<──────────────│               │
     │ False (error) │               │
     │               │               │
```

**Valid State Transitions**:
```
Pending ──────────> InProgress ──────────> Completed
   │                    │                      │
   │                    │                      │
   └────> Cancelled <───┴──────────────────────┘
   │                                           │
   │                                           │
   └───────────────> On Hold <─────────────────┘
```

**Invalid Transitions**:
- Completed → InProgress (use ReopenTask instead)
- Cancelled → InProgress (must go through Pending)
- Completed → Pending (use ReopenTask)

#### 9.12.9 Concurrent Access Pattern (Thread-Safe Operations)

This diagram shows how the library handles concurrent access when used in multi-threaded environments:

```
┌──────────┐  ┌──────────┐  ┌──────────────┐  ┌─────────┐
│ Thread 1 │  │ Thread 2 │  │ TTaskManager │  │  Lock   │
└────┬─────┘  └────┬─────┘  └──────┬───────┘  └────┬────┘
     │             │                │               │
     │ CreateTask()│                │               │
     │─────────────────────────────>│               │
     │             │                │               │
     │             │                │ Acquire Lock  │
     │             │                │──────────────>│
     │             │                │               │
     │             │                │<──────────────│
     │             │                │  Lock granted │
     │             │                │               │
     │             │ GetTask(ID)    │               │
     │             │───────────────>│               │
     │             │                │               │
     │             │                │ [Blocked]     │
     │             │                │ Waiting...    │
     │             │                │               │
     │             │                │ Add task      │
     │             │                │─┐             │
     │             │                │ │             │
     │             │                │<┘             │
     │             │                │               │
     │             │                │ Release Lock  │
     │             │                │──────────────>│
     │             │                │               │
     │<────────────────────────────│               │
     │   Task ID   │                │               │
     │             │                │               │
     │             │                │ Acquire Lock  │
     │             │                │──────────────>│
     │             │                │               │
     │             │                │<──────────────│
     │             │                │               │
     │             │                │ Find task     │
     │             │                │─┐             │
     │             │                │ │             │
     │             │                │<┘             │
     │             │                │               │
     │             │                │ Release Lock  │
     │             │                │──────────────>│
     │             │                │               │
     │             │<───────────────│               │
     │             │    TTask ref   │               │
     │             │                │               │
```

**Key Points**:
1. Critical sections are protected with synchronization primitives
2. Lock granularity is at the operation level
3. Read operations can be parallelized with read/write locks
4. Writer operations require exclusive access
5. Deadlock prevention through ordered lock acquisition

#### 9.12.10 Import/Export Data Flow

This diagram shows the complete import/export process with format conversion:

```
┌─────────┐  ┌──────────────┐  ┌────────────┐  ┌────────────┐  ┌─────────┐
│ Client  │  │ TTaskManager │  │TJSONStorage│  │TCSVStorage │  │File Sys │
└────┬────┘  └──────┬───────┘  └─────┬──────┘  └─────┬──────┘  └────┬────┘
     │               │                │               │              │
     │ ImportFromFile("data.csv", fmtCSV)            │              │
     │──────────────>│                │               │              │
     │               │                │               │              │
     │               │ Create CSV storage             │              │
     │               │────────────────────────────────>│              │
     │               │                │               │              │
     │               │                │               │ LoadTasks()  │
     │               │                │               │─────────────>│
     │               │                │               │              │
     │               │                │               │ Read CSV     │
     │               │                │               │<─────────────│
     │               │                │               │              │
     │               │                │               │ Parse rows   │
     │               │                │               │ Create tasks │
     │               │                │               │─┐            │
     │               │                │               │ │            │
     │               │                │               │<┘            │
     │               │                │               │              │
     │               │<────────────────────────────────│              │
     │               │           TTaskList            │              │
     │               │                │               │              │
     │               │ Merge with existing tasks      │              │
     │               │─┐              │               │              │
     │               │ │ Check duplicates             │              │
     │               │ │ Add new tasks                │              │
     │               │<┘              │               │              │
     │               │                │               │              │
     │<──────────────│                │               │              │
     │ Success + count                │               │              │
     │               │                │               │              │
     │               │                │               │              │
     │ ExportToFile("backup.json", fmtJSON)          │              │
     │──────────────>│                │               │              │
     │               │                │               │              │
     │               │ Create JSON storage            │              │
     │               │───────────────>│               │              │
     │               │                │               │              │
     │               │ SaveTasks()    │               │              │
     │               │───────────────>│               │              │
     │               │                │               │              │
     │               │                │ Serialize     │              │
     │               │                │ to JSON       │              │
     │               │                │─┐             │              │
     │               │                │ │             │              │
     │               │                │<┘             │              │
     │               │                │               │              │
     │               │                │ Write file    │              │
     │               │                │──────────────────────────────>│
     │               │                │               │              │
     │               │                │<──────────────────────────────│
     │               │                │               │              │
     │               │<───────────────│               │              │
     │               │                │               │              │
     │<──────────────│                │               │              │
     │   Success     │                │               │              │
     │               │                │               │              │
```

**Key Points**:
1. Import creates appropriate storage backend based on format
2. Imported tasks are merged with existing tasks
3. Duplicate detection by task ID
4. Export creates new file in specified format
5. All tasks are exported (no filtering during export)
6. Original storage format is preserved unless explicitly changed

---

**Summary of Interaction Patterns**:

These sequence diagrams illustrate the following key architectural patterns:

1. **Layered Architecture**: Clear separation between presentation (client), business logic (manager), and persistence (storage)
2. **Delegation Pattern**: Manager delegates to specialized components (validator, filter, storage)
3. **Validation-First**: All mutations go through validation before being committed
4. **Lazy Persistence**: Optional auto-save vs manual save for performance
5. **Object Ownership**: Clear rules about who owns and frees objects
6. **Error Recovery**: Graceful degradation with error reporting
7. **State Machine**: Well-defined state transitions with validation
8. **Synchronization**: Thread-safe operations for concurrent access
9. **Format Abstraction**: Storage format is interchangeable via interface

These patterns ensure the library is:
- **Predictable**: Behavior is consistent and well-documented
- **Maintainable**: Clear responsibilities for each component
- **Extensible**: New features can be added without breaking existing code
- **Reliable**: Errors are handled gracefully with recovery mechanisms
- **Performant**: Optimizations like batch operations and lazy persistence



### 9.13 Implementation Examples

This section provides concrete, compilable Free Pascal code snippets demonstrating how to implement key classes in the Task Manager Library. These examples follow Object Pascal best practices and illustrate the practical application of the class diagrams defined above.

#### 9.13.1 TTask Class - Complete Implementation Example

```pascal
unit TaskModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Task Status Enumeration }
  TTaskStatus = (
    tsNotStarted,
    tsInProgress,
    tsOnHold,
    tsCompleted,
    tsCancelled,
    tsDeferred
  );

  { Task Priority Enumeration }
  TTaskPriority = (
    tpLowest,
    tpLow,
    tpNormal,
    tpHigh,
    tpHighest,
    tpCritical
  );

  { Task Category Enumeration }
  TTaskCategory = (
    tcPersonal,
    tcWork,
    tcShopping,
    tcHealth,
    tcFinance,
    tcEducation,
    tcHome,
    tcSocial,
    tcOther
  );

  { TTask - Core task entity class }
  TTask = class(TObject)
  private
    FID: TGUID;
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
    
    { Core methods }
    function Clone: TTask;
    function IsEqual(ATask: TTask): Boolean;
    procedure MarkAsCompleted;
    procedure MarkAsStarted;
    procedure MarkAsOnHold;
    procedure MarkAsCancelled;
    function GetDaysUntilDue: Integer;
    function GetCompletionPercentage: Integer;
    
    { Properties }
    property ID: TGUID read FID write FID;
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

implementation

uses
  DateUtils;

{ TTask }

constructor TTask.Create;
begin
  inherited Create;
  CreateGUID(FID);
  FTitle := '';
  FDescription := '';
  FStatus := tsNotStarted;
  FPriority := tpNormal;
  FCategory := tcPersonal;
  FCreatedDate := Now;
  FDueDate := 0;
  FCompletedDate := 0;
  FEstimatedMinutes := 0;
  FActualMinutes := 0;
  FTags := TStringList.Create;
  FTags.Duplicates := dupIgnore;
  FTags.Sorted := True;
  FNotes := '';
end;

constructor TTask.Create(const ATitle: string);
begin
  Create;
  FTitle := ATitle;
end;

destructor TTask.Destroy;
begin
  FTags.Free;
  inherited Destroy;
end;

procedure TTask.SetTitle(const AValue: string);
begin
  if Trim(AValue) = '' then
    raise Exception.Create('Task title cannot be empty');
  FTitle := AValue;
end;

procedure TTask.SetDueDate(const AValue: TDateTime);
begin
  FDueDate := AValue;
end;

function TTask.GetIsOverdue: Boolean;
begin
  Result := (FDueDate > 0) and 
            (FStatus <> tsCompleted) and 
            (FStatus <> tsCancelled) and
            (Now > FDueDate);
end;

function TTask.GetIsCompleted: Boolean;
begin
  Result := (FStatus = tsCompleted);
end;

function TTask.Clone: TTask;
var
  I: Integer;
begin
  Result := TTask.Create;
  Result.FID := FID;
  Result.FTitle := FTitle;
  Result.FDescription := FDescription;
  Result.FStatus := FStatus;
  Result.FPriority := FPriority;
  Result.FCategory := FCategory;
  Result.FCreatedDate := FCreatedDate;
  Result.FDueDate := FDueDate;
  Result.FCompletedDate := FCompletedDate;
  Result.FEstimatedMinutes := FEstimatedMinutes;
  Result.FActualMinutes := FActualMinutes;
  Result.FNotes := FNotes;
  
  { Copy tags }
  for I := 0 to FTags.Count - 1 do
    Result.FTags.Add(FTags[I]);
end;

function TTask.IsEqual(ATask: TTask): Boolean;
begin
  if ATask = nil then
    Exit(False);
  
  Result := (GUIDToString(FID) = GUIDToString(ATask.FID)) and
            (FTitle = ATask.FTitle) and
            (FDescription = ATask.FDescription) and
            (FStatus = ATask.FStatus) and
            (FPriority = ATask.FPriority) and
            (FCategory = ATask.FCategory);
end;

procedure TTask.MarkAsCompleted;
begin
  FStatus := tsCompleted;
  FCompletedDate := Now;
end;

procedure TTask.MarkAsStarted;
begin
  if FStatus = tsNotStarted then
    FStatus := tsInProgress;
end;

procedure TTask.MarkAsOnHold;
begin
  FStatus := tsOnHold;
end;

procedure TTask.MarkAsCancelled;
begin
  FStatus := tsCancelled;
end;

function TTask.GetDaysUntilDue: Integer;
begin
  if FDueDate = 0 then
    Exit(-1);
  Result := DaysBetween(Now, FDueDate);
  if FDueDate < Now then
    Result := -Result;
end;

function TTask.GetCompletionPercentage: Integer;
begin
  case FStatus of
    tsNotStarted: Result := 0;
    tsInProgress: Result := 50;
    tsOnHold: Result := 50;
    tsCompleted: Result := 100;
    tsCancelled: Result := 0;
    tsDeferred: Result := 0;
  else
    Result := 0;
  end;
end;

end.
```

#### 9.13.2 TTaskList Class - Implementation Example

```pascal
unit TaskList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TaskModel;

type
  { Dynamic array of tasks }
  TTaskArray = array of TTask;

  { TTaskList - Collection class for managing tasks }
  TTaskList = class(TObject)
  private
    FItems: TTaskArray;
    FCount: Integer;
    FCapacity: Integer;
    FOwnsObjects: Boolean;
    
    function GetItem(Index: Integer): TTask;
    procedure SetItem(Index: Integer; const AValue: TTask);
    procedure Grow;
  public
    constructor Create(AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    
    { Core operations }
    function Add(ATask: TTask): Integer;
    procedure Insert(AIndex: Integer; ATask: TTask);
    procedure Delete(AIndex: Integer);
    procedure Clear;
    function Remove(ATask: TTask): Integer;
    
    { Search operations }
    function FindByID(const AID: TGUID): TTask;
    function IndexOf(ATask: TTask): Integer;
    function Contains(ATask: TTask): Boolean;
    
    { Utility operations }
    function ToArray: TTaskArray;
    procedure Sort(AComparer: TListSortCompare);
    
    { Properties }
    property Count: Integer read FCount;
    property Items[Index: Integer]: TTask read GetItem write SetItem; default;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

const
  DEFAULT_CAPACITY = 16;
  GROW_FACTOR = 2;

{ TTaskList }

constructor TTaskList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
  FCount := 0;
  FCapacity := DEFAULT_CAPACITY;
  SetLength(FItems, FCapacity);
end;

destructor TTaskList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTaskList.Grow;
begin
  if FCapacity = 0 then
    FCapacity := DEFAULT_CAPACITY
  else
    FCapacity := FCapacity * GROW_FACTOR;
  SetLength(FItems, FCapacity);
end;

function TTaskList.GetItem(Index: Integer): TTask;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('List index out of bounds (%d)', [Index]);
  Result := FItems[Index];
end;

procedure TTaskList.SetItem(Index: Integer; const AValue: TTask);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('List index out of bounds (%d)', [Index]);
  FItems[Index] := AValue;
end;

function TTaskList.Add(ATask: TTask): Integer;
begin
  if FCount >= FCapacity then
    Grow;
  FItems[FCount] := ATask;
  Result := FCount;
  Inc(FCount);
end;

procedure TTaskList.Insert(AIndex: Integer; ATask: TTask);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex > FCount) then
    raise Exception.CreateFmt('List index out of bounds (%d)', [AIndex]);
  
  if FCount >= FCapacity then
    Grow;
  
  { Shift elements to make room }
  for I := FCount downto AIndex + 1 do
    FItems[I] := FItems[I - 1];
  
  FItems[AIndex] := ATask;
  Inc(FCount);
end;

procedure TTaskList.Delete(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise Exception.CreateFmt('List index out of bounds (%d)', [AIndex]);
  
  { Free the object if we own it }
  if FOwnsObjects and Assigned(FItems[AIndex]) then
    FItems[AIndex].Free;
  
  { Shift elements }
  for I := AIndex to FCount - 2 do
    FItems[I] := FItems[I + 1];
  
  FItems[FCount - 1] := nil;
  Dec(FCount);
end;

procedure TTaskList.Clear;
var
  I: Integer;
begin
  if FOwnsObjects then
  begin
    for I := 0 to FCount - 1 do
      if Assigned(FItems[I]) then
        FItems[I].Free;
  end;
  
  FCount := 0;
  FCapacity := DEFAULT_CAPACITY;
  SetLength(FItems, FCapacity);
end;

function TTaskList.Remove(ATask: TTask): Integer;
begin
  Result := IndexOf(ATask);
  if Result >= 0 then
    Delete(Result);
end;

function TTaskList.FindByID(const AID: TGUID): TTask;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FCount - 1 do
  begin
    if GUIDToString(FItems[I].ID) = GUIDToString(AID) then
    begin
      Result := FItems[I];
      Break;
    end;
  end;
end;

function TTaskList.IndexOf(ATask: TTask): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
  begin
    if FItems[I] = ATask then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TTaskList.Contains(ATask: TTask): Boolean;
begin
  Result := IndexOf(ATask) >= 0;
end;

function TTaskList.ToArray: TTaskArray;
var
  I: Integer;
begin
  SetLength(Result, FCount);
  for I := 0 to FCount - 1 do
    Result[I] := FItems[I];
end;

procedure TTaskList.Sort(AComparer: TListSortCompare);
begin
  { Use QuickSort algorithm }
  if FCount > 1 then
    QuickSort(FItems, 0, FCount - 1, AComparer);
end;

end.
```

#### 9.13.3 TTaskManager Class - Implementation Example

```pascal
unit TaskManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TaskModel, TaskList, TaskStorage;

type
  { TTaskManager - Main entry point for task management }
  TTaskManager = class(TObject)
  private
    FTasks: TTaskList;
    FStorage: ITaskStorage;
    FModified: Boolean;
    
    function GetTaskCount: Integer;
  public
    constructor Create(AStorage: ITaskStorage);
    destructor Destroy; override;
    
    { CRUD operations }
    function CreateTask(const ATitle: string): TTask; overload;
    function CreateTask(const ATitle, ADescription: string): TTask; overload;
    function GetTask(const AID: TGUID): TTask;
    function UpdateTask(ATask: TTask): Boolean;
    function DeleteTask(const AID: TGUID): Boolean;
    function GetAllTasks: TTaskList;
    
    { Persistence operations }
    function LoadTasks(const AFileName: string): Boolean;
    function SaveTasks(const AFileName: string): Boolean;
    
    { Query operations }
    function GetTasksByStatus(AStatus: TTaskStatus): TTaskList;
    function GetTasksByPriority(APriority: TTaskPriority): TTaskList;
    function GetTasksByCategory(ACategory: TTaskCategory): TTaskList;
    function GetOverdueTasks: TTaskList;
    function GetTasksDueToday: TTaskList;
    function SearchTasks(const ASearchTerm: string): TTaskList;
    
    { Properties }
    property TaskCount: Integer read GetTaskCount;
    property Storage: ITaskStorage read FStorage;
    property Modified: Boolean read FModified;
  end;

implementation

uses
  DateUtils;

{ TTaskManager }

constructor TTaskManager.Create(AStorage: ITaskStorage);
begin
  inherited Create;
  FTasks := TTaskList.Create(True);
  FStorage := AStorage;
  FModified := False;
end;

destructor TTaskManager.Destroy;
begin
  FTasks.Free;
  inherited Destroy;
end;

function TTaskManager.GetTaskCount: Integer;
begin
  Result := FTasks.Count;
end;

function TTaskManager.CreateTask(const ATitle: string): TTask;
begin
  Result := TTask.Create(ATitle);
  FTasks.Add(Result);
  FModified := True;
end;

function TTaskManager.CreateTask(const ATitle, ADescription: string): TTask;
begin
  Result := CreateTask(ATitle);
  Result.Description := ADescription;
end;

function TTaskManager.GetTask(const AID: TGUID): TTask;
begin
  Result := FTasks.FindByID(AID);
end;

function TTaskManager.UpdateTask(ATask: TTask): Boolean;
var
  ExistingTask: TTask;
begin
  Result := False;
  ExistingTask := FTasks.FindByID(ATask.ID);
  if Assigned(ExistingTask) then
  begin
    { Update the existing task properties }
    ExistingTask.Title := ATask.Title;
    ExistingTask.Description := ATask.Description;
    ExistingTask.Status := ATask.Status;
    ExistingTask.Priority := ATask.Priority;
    ExistingTask.Category := ATask.Category;
    ExistingTask.DueDate := ATask.DueDate;
    ExistingTask.CompletedDate := ATask.CompletedDate;
    ExistingTask.EstimatedMinutes := ATask.EstimatedMinutes;
    ExistingTask.ActualMinutes := ATask.ActualMinutes;
    ExistingTask.Notes := ATask.Notes;
    FModified := True;
    Result := True;
  end;
end;

function TTaskManager.DeleteTask(const AID: TGUID): Boolean;
var
  TaskToDelete: TTask;
begin
  Result := False;
  TaskToDelete := FTasks.FindByID(AID);
  if Assigned(TaskToDelete) then
  begin
    FTasks.Remove(TaskToDelete);
    FModified := True;
    Result := True;
  end;
end;

function TTaskManager.GetAllTasks: TTaskList;
var
  I: Integer;
begin
  { Return a new list with references to all tasks }
  Result := TTaskList.Create(False); // Don't own the objects
  for I := 0 to FTasks.Count - 1 do
    Result.Add(FTasks[I]);
end;

function TTaskManager.LoadTasks(const AFileName: string): Boolean;
var
  LoadedTasks: TTaskList;
begin
  Result := False;
  try
    LoadedTasks := FStorage.LoadTasks(AFileName);
    if Assigned(LoadedTasks) then
    begin
      FTasks.Free;
      FTasks := LoadedTasks;
      FModified := False;
      Result := True;
    end;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TTaskManager.SaveTasks(const AFileName: string): Boolean;
begin
  Result := False;
  try
    Result := FStorage.SaveTasks(FTasks, AFileName);
    if Result then
      FModified := False;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TTaskManager.GetTasksByStatus(AStatus: TTaskStatus): TTaskList;
var
  I: Integer;
begin
  Result := TTaskList.Create(False);
  for I := 0 to FTasks.Count - 1 do
  begin
    if FTasks[I].Status = AStatus then
      Result.Add(FTasks[I]);
  end;
end;

function TTaskManager.GetTasksByPriority(APriority: TTaskPriority): TTaskList;
var
  I: Integer;
begin
  Result := TTaskList.Create(False);
  for I := 0 to FTasks.Count - 1 do
  begin
    if FTasks[I].Priority = APriority then
      Result.Add(FTasks[I]);
  end;
end;

function TTaskManager.GetTasksByCategory(ACategory: TTaskCategory): TTaskList;
var
  I: Integer;
begin
  Result := TTaskList.Create(False);
  for I := 0 to FTasks.Count - 1 do
  begin
    if FTasks[I].Category = ACategory then
      Result.Add(FTasks[I]);
  end;
end;

function TTaskManager.GetOverdueTasks: TTaskList;
var
  I: Integer;
begin
  Result := TTaskList.Create(False);
  for I := 0 to FTasks.Count - 1 do
  begin
    if FTasks[I].IsOverdue then
      Result.Add(FTasks[I]);
  end;
end;

function TTaskManager.GetTasksDueToday: TTaskList;
var
  I: Integer;
  Today: TDateTime;
begin
  Result := TTaskList.Create(False);
  Today := Date;
  for I := 0 to FTasks.Count - 1 do
  begin
    if (FTasks[I].DueDate > 0) and 
       (DateOf(FTasks[I].DueDate) = Today) then
      Result.Add(FTasks[I]);
  end;
end;

function TTaskManager.SearchTasks(const ASearchTerm: string): TTaskList;
var
  I: Integer;
  SearchLower: string;
begin
  Result := TTaskList.Create(False);
  SearchLower := LowerCase(ASearchTerm);
  
  for I := 0 to FTasks.Count - 1 do
  begin
    if (Pos(SearchLower, LowerCase(FTasks[I].Title)) > 0) or
       (Pos(SearchLower, LowerCase(FTasks[I].Description)) > 0) or
       (Pos(SearchLower, LowerCase(FTasks[I].Notes)) > 0) then
      Result.Add(FTasks[I]);
  end;
end;

end.
```

#### 9.13.4 Usage Example - Complete Workflow

```pascal
program TaskManagerExample;

{$mode objfpc}{$H+}

uses
  SysUtils, TaskModel, TaskList, TaskManager, TaskStorageJSON;

var
  Manager: TTaskManager;
  Storage: ITaskStorage;
  Task1, Task2: TTask;
  AllTasks: TTaskList;
  I: Integer;

begin
  { Create storage backend }
  Storage := TJSONTaskStorage.Create;
  
  { Create task manager }
  Manager := TTaskManager.Create(Storage);
  try
    { Create some tasks }
    Task1 := Manager.CreateTask('Buy groceries', 'Milk, bread, eggs');
    Task1.Priority := tpHigh;
    Task1.Category := tcShopping;
    Task1.DueDate := Date + 1; // Tomorrow
    Task1.Tags.Add('urgent');
    Task1.Tags.Add('home');
    
    Task2 := Manager.CreateTask('Finish project report');
    Task2.Priority := tpCritical;
    Task2.Category := tcWork;
    Task2.DueDate := Date + 3;
    Task2.EstimatedMinutes := 120;
    
    { Mark task as started }
    Task2.MarkAsStarted;
    
    { Save tasks to file }
    if Manager.SaveTasks('tasks.json') then
      WriteLn('Tasks saved successfully')
    else
      WriteLn('Failed to save tasks');
    
    { Query tasks }
    AllTasks := Manager.GetTasksByPriority(tpHigh);
    try
      WriteLn(Format('Found %d high priority tasks:', [AllTasks.Count]));
      for I := 0 to AllTasks.Count - 1 do
        WriteLn('  - ', AllTasks[I].Title);
    finally
      AllTasks.Free;
    end;
    
    { Search tasks }
    AllTasks := Manager.SearchTasks('project');
    try
      WriteLn(Format('Search results for "project": %d tasks', [AllTasks.Count]));
    finally
      AllTasks.Free;
    end;
    
  finally
    Manager.Free;
  end;
end.
```

#### 9.13.5 ITaskStorage Interface - Implementation Example

```pascal
unit TaskStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TaskList;

type
  { Interface for task storage implementations }
  ITaskStorage = interface
    ['{A1B2C3D4-E5F6-4A5B-8C9D-0E1F2A3B4C5D}']
    function LoadTasks(const AFileName: string): TTaskList;
    function SaveTasks(ATasks: TTaskList; const AFileName: string): Boolean;
    function GetFormatName: string;
    function GetFileExtension: string;
  end;

implementation

end.
```

These implementation examples demonstrate:

1. **Proper Object Pascal syntax** with `{$mode objfpc}{$H+}` directives
2. **Memory management** with constructors, destructors, and proper object ownership
3. **Error handling** with exceptions for invalid operations
4. **Type safety** with strong typing and GUID usage
5. **Encapsulation** with private fields and public properties
6. **Code reusability** through clean interfaces and modular design
7. **Best practices** such as sorted/duplicate-free string lists for tags
8. **Defensive programming** with bounds checking and nil checks

All code snippets are designed to compile with Free Pascal Compiler (FPC) version 3.0 or later and follow the specification's requirement of being UI-independent and reusable.

---
**End of Section 9: Class Diagrams and Methods/Properties**


## 10. Source Code Organization and File Structure

### 10.1 Overview

The Free Pascal Task Manager Library follows a modular architecture with clear separation of concerns. The source code is organized into distinct layers (data model, business logic, storage, utilities) with minimal coupling and high cohesion.

### 10.2 Directory Structure

```
TaskManagerLib/
├── src/                          # Source code directory
│   ├── core/                     # Core data models and types
│   │   ├── TaskModel.pas         # TTask class definition
│   │   ├── TaskTypes.pas         # Enumerations and type definitions
│   │   └── TaskExceptions.pas    # Custom exception classes
│   ├── collections/              # Collection management
│   │   ├── TaskList.pas          # TTaskList class
│   │   └── TaskCollection.pas    # Additional collection utilities
│   ├── business/                 # Business logic layer
│   │   ├── TaskManager.pas       # Main manager class
│   │   ├── TaskValidator.pas     # Validation logic
│   │   ├── TaskFilter.pas        # Filtering and search
│   │   └── TaskStatistics.pas    # Analytics and reporting
│   ├── storage/                  # Persistence layer
│   │   ├── TaskStorage.pas       # ITaskStorage interface
│   │   ├── TaskStorageJSON.pas   # JSON implementation
│   │   ├── TaskStorageXML.pas    # XML implementation
│   │   └── TaskStorageCSV.pas    # CSV implementation
│   ├── utils/                    # Utility functions
│   │   ├── TaskUtils.pas         # Helper functions
│   │   ├── DateTimeUtils.pas     # Date/time utilities
│   │   └── StringUtils.pas       # String manipulation
│   └── TaskManagerLib.lpk        # Lazarus package file (optional)
├── tests/                        # Unit and integration tests
│   ├── core/                     # Tests for core modules
│   │   ├── TestTaskModel.pas
│   │   └── TestTaskTypes.pas
│   ├── collections/              # Tests for collections
│   │   └── TestTaskList.pas
│   ├── business/                 # Tests for business logic
│   │   ├── TestTaskManager.pas
│   │   ├── TestTaskValidator.pas
│   │   ├── TestTaskFilter.pas
│   │   └── TestTaskStatistics.pas
│   ├── storage/                  # Tests for storage layer
│   │   ├── TestTaskStorageJSON.pas
│   │   ├── TestTaskStorageXML.pas
│   │   └── TestTaskStorageCSV.pas
│   ├── integration/              # Integration tests
│   │   ├── TestEndToEnd.pas
│   │   └── TestStorageRoundTrip.pas
│   └── AllTests.lpr              # Test runner project
├── examples/                     # Example usage code
│   ├── BasicUsage.lpr            # Simple CRUD operations
│   ├── FilteringExample.lpr      # Advanced filtering
│   ├── StatisticsExample.lpr     # Using statistics module
│   └── CustomStorageExample.lpr  # Implementing custom storage
├── docs/                         # Documentation
│   ├── software-spec.md          # This specification document
│   ├── API-Reference.md          # Detailed API documentation
│   └── diagrams/                 # UML and other diagrams
│       ├── class-diagram.png
│       └── architecture.png
├── bin/                          # Compiled binaries (gitignored)
├── lib/                          # Compiled units (gitignored)
├── data/                         # Sample data files
│   ├── sample-tasks.json
│   ├── sample-tasks.xml
│   └── sample-tasks.csv
└── README.md                     # Project overview
```

### 10.3 File Naming Conventions

#### 10.3.1 Pascal Unit Files

- **Pattern**: `{Prefix}{ComponentName}.pas`
- **Prefix**: `Task` for all task manager components
- **Examples**:
  - `TaskModel.pas` - Core task model
  - `TaskManager.pas` - Main manager
  - `TaskStorageJSON.pas` - JSON storage implementation

#### 10.3.2 Test Files

- **Pattern**: `Test{ComponentName}.pas`
- **Examples**:
  - `TestTaskModel.pas` - Tests for TTask class
  - `TestTaskManager.pas` - Tests for TTaskManager class

#### 10.3.3 Example Files

- **Pattern**: `{Feature}Example.lpr`
- **Examples**:
  - `BasicUsage.lpr` - Basic usage example
  - `FilteringExample.lpr` - Filtering example

### 10.4 Core Module Files

#### 10.4.1 TaskTypes.pas

**Purpose**: Defines all enumerations, constants, and type aliases used throughout the library.

**Location**: `src/core/TaskTypes.pas`

**Key Contents**:
```pascal
unit TaskTypes;

{$mode objfpc}{$H+}

interface

type
  // Enumerations
  TTaskStatus = (tsNotStarted, tsInProgress, tsCompleted, tsCancelled, tsOnHold);
  TTaskPriority = (tpLow, tpNormal, tpHigh, tpCritical);
  TTaskCategory = (tcPersonal, tcWork, tcShopping, tcHealth, tcEducation, tcOther);
  TReportFormat = (rfPlainText, rfJSON, rfXML, rfHTML, rfMarkdown);
  TEncoding = (encUTF8, encUTF16, encASCII);
  
  // Constants
const
  MAX_TITLE_LENGTH = 200;
  MAX_DESCRIPTION_LENGTH = 4000;
  DEFAULT_ESTIMATED_HOURS = 0.0;
  
  // Type aliases
type
  TTaskID = Integer;
  TTaskArray = array of TTask;

implementation

end.
```

**Dependencies**: None (base types only)

**Used By**: All other modules

#### 10.4.2 TaskModel.pas

**Purpose**: Defines the `TTask` class, the core data model for a single task.

**Location**: `src/core/TaskModel.pas`

**Key Contents**:
- `TTask` class with all properties and methods
- Private field declarations
- Public property accessors
- Constructor/destructor
- Serialization helper methods

**Dependencies**: 
- `Classes` (for TStringList)
- `SysUtils` (for exception handling)
- `TaskTypes` (for enumerations)

**Used By**: `TaskList.pas`, `TaskManager.pas`, storage modules

#### 10.4.3 TaskExceptions.pas

**Purpose**: Defines custom exception classes for the library.

**Location**: `src/core/TaskExceptions.pas`

**Key Contents**:
```pascal
unit TaskExceptions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  ETaskException = class(Exception);
  ETaskValidationException = class(ETaskException);
  ETaskNotFoundException = class(ETaskException);
  ETaskStorageException = class(ETaskException);
  ETaskDuplicateException = class(ETaskException);
  ETaskFilterException = class(ETaskException);

implementation

end.
```

**Dependencies**: `SysUtils`

**Used By**: All modules that throw exceptions

### 10.5 Collection Module Files

#### 10.5.1 TaskList.pas

**Purpose**: Implements `TTaskList` class for managing collections of tasks.

**Location**: `src/collections/TaskList.pas`

**Key Contents**:
- `TTaskList` class (inherits from `TObjectList<TTask>`)
- CRUD operations
- Search and filter methods
- Sorting capabilities
- Import/export helpers

**Dependencies**:
- `Classes`, `Contnrs`
- `TaskModel.pas`
- `TaskTypes.pas`

**Used By**: `TaskManager.pas`, `TaskFilter.pas`, `TaskStatistics.pas`

### 10.6 Business Logic Module Files

#### 10.6.1 TaskManager.pas

**Purpose**: Main entry point for the library, coordinates all operations.

**Location**: `src/business/TaskManager.pas`

**Key Contents**:
- `TTaskManager` class
- Task lifecycle management
- Integration with storage, validation, and statistics
- High-level API methods

**Dependencies**:
- `TaskModel.pas`, `TaskList.pas`
- `TaskStorage.pas`
- `TaskValidator.pas`
- `TaskStatistics.pas`
- `TaskFilter.pas`

**Used By**: Client applications

#### 10.6.2 TaskValidator.pas

**Purpose**: Implements validation rules for tasks.

**Location**: `src/business/TaskValidator.pas`

**Key Contents**:
- `TTaskValidator` class
- `TValidationResult` record
- Validation rule implementations
- Custom validation rule support

**Dependencies**:
- `TaskModel.pas`
- `TaskTypes.pas`

**Used By**: `TaskManager.pas`

#### 10.6.3 TaskFilter.pas

**Purpose**: Implements advanced filtering and searching capabilities.

**Location**: `src/business/TaskFilter.pas`

**Key Contents**:
- `TTaskFilter` class
- `TTaskFilterCriteria` record
- Filter predicates
- Composite filter support

**Dependencies**:
- `TaskModel.pas`, `TaskList.pas`
- `TaskTypes.pas`

**Used By**: `TaskManager.pas`, client applications

#### 10.6.4 TaskStatistics.pas

**Purpose**: Provides analytics and reporting on task collections.

**Location**: `src/business/TaskStatistics.pas`

**Key Contents**:
- `TTaskStatistics` class
- `TTaskStatisticsData` record
- Statistical calculations
- Report generation

**Dependencies**:
- `TaskModel.pas`, `TaskList.pas`
- `TaskTypes.pas`

**Used By**: `TaskManager.pas`, reporting applications

### 10.7 Storage Layer Module Files

#### 10.7.1 TaskStorage.pas

**Purpose**: Defines the `ITaskStorage` interface for persistence.

**Location**: `src/storage/TaskStorage.pas`

**Key Contents**:
```pascal
unit TaskStorage;

{$mode objfpc}{$H+}

interface

uses
  TaskList, TaskModel;

type
  ITaskStorage = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function LoadTasks: TTaskList;
    procedure SaveTasks(ATasks: TTaskList);
    function GetFilePath: string;
    procedure SetFilePath(const APath: string);
    property FilePath: string read GetFilePath write SetFilePath;
  end;

implementation

end.
```

**Dependencies**: `TaskList.pas`, `TaskModel.pas`

**Implemented By**: JSON, XML, CSV storage modules

#### 10.7.2 TaskStorageJSON.pas

**Purpose**: Implements JSON-based persistence.

**Location**: `src/storage/TaskStorageJSON.pas`

**Key Contents**:
- `TJSONTaskStorage` class
- JSON serialization/deserialization
- Error handling for malformed JSON

**Dependencies**:
- `fpjson`, `jsonparser` (Free Pascal JSON units)
- `TaskStorage.pas`, `TaskList.pas`, `TaskModel.pas`

**Used By**: `TaskManager.pas`, client applications

#### 10.7.3 TaskStorageXML.pas

**Purpose**: Implements XML-based persistence.

**Location**: `src/storage/TaskStorageXML.pas`

**Key Contents**:
- `TXMLTaskStorage` class
- XML serialization/deserialization using DOM
- Schema validation support

**Dependencies**:
- `DOM`, `XMLRead`, `XMLWrite` (Free Pascal XML units)
- `TaskStorage.pas`, `TaskList.pas`, `TaskModel.pas`

**Used By**: `TaskManager.pas`, client applications

#### 10.7.4 TaskStorageCSV.pas

**Purpose**: Implements CSV-based persistence (flat structure).

**Location**: `src/storage/TaskStorageCSV.pas`

**Key Contents**:
- `TCSVTaskStorage` class
- CSV parsing and generation
- Delimiter and encoding configuration

**Dependencies**:
- `Classes`, `SysUtils`
- `TaskStorage.pas`, `TaskList.pas`, `TaskModel.pas`

**Used By**: `TaskManager.pas`, data import/export tools

### 10.8 Utility Module Files

#### 10.8.1 TaskUtils.pas

**Purpose**: Provides utility functions used across the library.

**Location**: `src/utils/TaskUtils.pas`

**Key Contents**:
- Enum-to-string conversions
- String-to-enum conversions
- Date/time formatting helpers
- GUID generation

**Dependencies**: `TaskTypes.pas`, `SysUtils`, `DateUtils`

**Used By**: All modules

#### 10.8.2 DateTimeUtils.pas

**Purpose**: Date and time manipulation utilities specific to task management.

**Location**: `src/utils/DateTimeUtils.pas`

**Key Contents**:
```pascal
unit DateTimeUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils;

function IsOverdue(const ADueDate: TDateTime): Boolean;
function DaysUntilDue(const ADueDate: TDateTime): Integer;
function FormatTaskDateTime(const ADateTime: TDateTime): string;
function ParseTaskDateTime(const ADateTimeStr: string): TDateTime;
function GetWeekStartDate(const ADate: TDateTime): TDateTime;
function GetWeekEndDate(const ADate: TDateTime): TDateTime;

implementation

// Implementation details...

end.
```

**Dependencies**: `SysUtils`, `DateUtils`

**Used By**: `TaskModel.pas`, `TaskFilter.pas`, `TaskStatistics.pas`

#### 10.8.3 StringUtils.pas

**Purpose**: String manipulation utilities.

**Location**: `src/utils/StringUtils.pas`

**Key Contents**:
- String truncation
- Case-insensitive comparison
- Wildcard matching
- String sanitization for storage

**Dependencies**: `SysUtils`

**Used By**: `TaskValidator.pas`, `TaskFilter.pas`, storage modules

### 10.9 Compilation Units and Dependencies

#### 10.9.1 Dependency Graph

```
Layer 1 (Foundation - No dependencies):
  - TaskTypes.pas
  - TaskExceptions.pas

Layer 2 (Core Data Model):
  - TaskModel.pas → TaskTypes, TaskExceptions
  - DateTimeUtils.pas → (standard units)
  - StringUtils.pas → (standard units)

Layer 3 (Collections):
  - TaskList.pas → TaskModel, TaskTypes

Layer 4 (Storage Interface):
  - TaskStorage.pas → TaskList, TaskModel

Layer 5 (Storage Implementations):
  - TaskStorageJSON.pas → TaskStorage, TaskList, TaskModel
  - TaskStorageXML.pas → TaskStorage, TaskList, TaskModel
  - TaskStorageCSV.pas → TaskStorage, TaskList, TaskModel

Layer 6 (Business Logic):
  - TaskValidator.pas → TaskModel, TaskTypes
  - TaskFilter.pas → TaskModel, TaskList, TaskTypes
  - TaskStatistics.pas → TaskModel, TaskList, TaskTypes
  - TaskUtils.pas → TaskTypes, DateTimeUtils, StringUtils

Layer 7 (Facade):
  - TaskManager.pas → All of the above
```

#### 10.9.2 Compilation Order

When compiling manually or creating makefiles, respect this order:

1. `TaskTypes.pas`, `TaskExceptions.pas`
2. `TaskModel.pas`, `DateTimeUtils.pas`, `StringUtils.pas`
3. `TaskList.pas`
4. `TaskStorage.pas`
5. `TaskStorageJSON.pas`, `TaskStorageXML.pas`, `TaskStorageCSV.pas`
6. `TaskValidator.pas`, `TaskFilter.pas`, `TaskStatistics.pas`, `TaskUtils.pas`
7. `TaskManager.pas`

### 10.10 Package Files

#### 10.10.1 Lazarus Package (TaskManagerLib.lpk)

**Purpose**: Defines a Lazarus package for easy integration into Lazarus projects.

**Location**: `src/TaskManagerLib.lpk`

**Contents**:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <Package Version="5">
    <Name Value="TaskManagerLib"/>
    <Type Value="RunAndDesignTime"/>
    <CompilerOptions>
      <Version Value="11"/>
      <SearchPaths>
        <IncludeFiles Value="core;collections;business;storage;utils"/>
        <OtherUnitFiles Value="core;collections;business;storage;utils"/>
        <UnitOutputDirectory Value="lib/$(TargetCPU)-$(TargetOS)"/>
      </SearchPaths>
    </CompilerOptions>
    <Files Count="15">
      <Item1>
        <Filename Value="core/TaskTypes.pas"/>
        <UnitName Value="TaskTypes"/>
      </Item1>
      <Item2>
        <Filename Value="core/TaskExceptions.pas"/>
        <UnitName Value="TaskExceptions"/>
      </Item2>
      <Item3>
        <Filename Value="core/TaskModel.pas"/>
        <UnitName Value="TaskModel"/>
      </Item3>
      <!-- Additional items for all units -->
    </Files>
    <RequiredPkgs Count="1">
      <Item1>
        <PackageName Value="FCL"/>
      </Item1>
    </RequiredPkgs>
  </Package>
</CONFIG>
```

### 10.11 Build Configuration

#### 10.11.1 Free Pascal Compiler Options

Recommended FPC compiler options (`fpc.cfg` or command line):

```
# Mode and syntax
-Mobjfpc          # Object Pascal mode
-Sh               # Use ansistrings

# Optimization
-O2               # Level 2 optimization
-Xs               # Strip symbols (release builds)

# Warnings and errors
-vewn             # Verbose: errors, warnings, notes
-Sew              # Stop on warnings (strict mode)

# Output
-FU./lib          # Unit output directory
-FE./bin          # Executable output directory

# Search paths
-Fu./src/core
-Fu./src/collections
-Fu./src/business
-Fu./src/storage
-Fu./src/utils

# Include paths
-Fi./src/core
-Fi./src/collections
-Fi./src/business
-Fi./src/storage
-Fi./src/utils
```

#### 10.11.2 Makefile Structure

**Location**: `Makefile` (root directory)

```makefile
# Free Pascal Task Manager Library Makefile

FPC := fpc
FPCFLAGS := -Mobjfpc -Sh -O2 -vewn
SRCDIR := src
UNITDIRS := $(SRCDIR)/core $(SRCDIR)/collections $(SRCDIR)/business $(SRCDIR)/storage $(SRCDIR)/utils
UNITSEARCH := $(addprefix -Fu,$(UNITDIRS))
INCSEARCH := $(addprefix -Fi,$(UNITDIRS))
LIBDIR := lib
BINDIR := bin

# Core units (in compilation order)
CORE_UNITS := $(SRCDIR)/core/TaskTypes.pas               $(SRCDIR)/core/TaskExceptions.pas               $(SRCDIR)/core/TaskModel.pas

COLLECTION_UNITS := $(SRCDIR)/collections/TaskList.pas

STORAGE_UNITS := $(SRCDIR)/storage/TaskStorage.pas                  $(SRCDIR)/storage/TaskStorageJSON.pas                  $(SRCDIR)/storage/TaskStorageXML.pas                  $(SRCDIR)/storage/TaskStorageCSV.pas

BUSINESS_UNITS := $(SRCDIR)/business/TaskValidator.pas                   $(SRCDIR)/business/TaskFilter.pas                   $(SRCDIR)/business/TaskStatistics.pas                   $(SRCDIR)/business/TaskManager.pas

UTIL_UNITS := $(SRCDIR)/utils/DateTimeUtils.pas               $(SRCDIR)/utils/StringUtils.pas               $(SRCDIR)/utils/TaskUtils.pas

ALL_UNITS := $(CORE_UNITS) $(COLLECTION_UNITS) $(STORAGE_UNITS) $(BUSINESS_UNITS) $(UTIL_UNITS)

.PHONY: all clean tests examples

all: $(ALL_UNITS)
	@echo "Compiling all units..."
	$(FPC) $(FPCFLAGS) $(UNITSEARCH) $(INCSEARCH) -FU$(LIBDIR) $(SRCDIR)/business/TaskManager.pas

clean:
	rm -rf $(LIBDIR)/* $(BINDIR)/*
	find . -name "*.o" -delete
	find . -name "*.ppu" -delete

tests:
	@echo "Running tests..."
	$(FPC) $(FPCFLAGS) $(UNITSEARCH) $(INCSEARCH) -FU$(LIBDIR) -FE$(BINDIR) tests/AllTests.lpr
	./$(BINDIR)/AllTests

examples:
	@echo "Compiling examples..."
	$(FPC) $(FPCFLAGS) $(UNITSEARCH) $(INCSEARCH) -FU$(LIBDIR) -FE$(BINDIR) examples/BasicUsage.lpr
```

### 10.12 Version Control Structure

#### 10.12.1 .gitignore

**Location**: `.gitignore` (root directory)

```gitignore
# Compiled Units
*.o
*.ppu
*.compiled
*.rst
*.rsj
*.or

# Compiled binaries
bin/
lib/
backup/

# Lazarus IDE files
*.lps
*.lrt
*.bak

# OS-specific files
.DS_Store
Thumbs.db
desktop.ini

# Temporary files
*~
*.tmp
*.swp
*.swo

# Test output
test-results/
coverage/
```

### 10.13 Code Organization Best Practices

#### 10.13.1 Unit Structure Template

Every unit should follow this structure:

```pascal
unit UnitName;

{$mode objfpc}{$H+}

interface

uses
  // Standard units first
  Classes, SysUtils,
  // Then project units
  TaskTypes, TaskModel;

type
  // Type declarations

const
  // Constants

var
  // Global variables (avoid if possible)

// Function/procedure declarations

implementation

uses
  // Implementation-only units

// Function/procedure implementations

initialization
  // Initialization code (if needed)

finalization
  // Cleanup code (if needed)

end.
```

#### 10.13.2 Naming Conventions Summary

- **Classes**: `TClassName` (e.g., `TTaskManager`)
- **Interfaces**: `IInterfaceName` (e.g., `ITaskStorage`)
- **Enumerations**: `TEnumName` with values `enPrefix` (e.g., `TTaskStatus` with `tsCompleted`)
- **Records**: `TRecordName` (e.g., `TValidationResult`)
- **Methods**: `VerbNoun` (e.g., `GetTaskByID`, `ValidateTask`)
- **Properties**: `PropertyName` (e.g., `Title`, `CreatedAt`)
- **Private Fields**: `FFieldName` (e.g., `FTitle`, `FCreatedAt`)
- **Parameters**: `AParameterName` (e.g., `ATaskID`, `ATitle`)
- **Local Variables**: `LocalVarName` (e.g., `Task`, `Index`)

#### 10.13.3 Documentation Comments

Use XML-style documentation comments for all public interfaces:

```pascal
type
  /// <summary>
  /// Manages a collection of tasks with CRUD operations.
  /// </summary>
  TTaskManager = class
  public
    /// <summary>
    /// Creates a new task with the specified parameters.
    /// </summary>
    /// <param name="ATitle">The title of the task</param>
    /// <param name="ADescription">The detailed description</param>
    /// <returns>The newly created task instance</returns>
    /// <exception cref="ETaskValidationException">
    /// Raised when the title is empty or exceeds maximum length
    /// </exception>
    function CreateTask(const ATitle, ADescription: string): TTask;
  end;
```

### 10.14 File Size and Complexity Guidelines

- **Maximum lines per unit**: 1000 lines (recommendation)
- **Maximum methods per class**: 30 methods (recommendation)
- **Maximum cyclomatic complexity per method**: 10 (recommendation)

When units exceed these limits, consider refactoring into smaller, more focused units.

### 10.15 Integration with Build Systems

#### 10.15.1 Continuous Integration

The library structure supports CI systems like GitHub Actions, GitLab CI, or Jenkins:

**Example GitHub Actions Workflow** (`.github/workflows/build.yml`):

```yaml
name: Build and Test

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install FPC
        run: sudo apt-get install -y fpc
      - name: Compile Library
        run: make all
      - name: Run Tests
        run: make tests
      - name: Build Examples
        run: make examples
```

#### 10.15.2 Documentation Generation

The structure supports automated documentation generation using PasDoc:

```bash
pasdoc --format html        --output docs/api        --source src/core/*.pas        --source src/collections/*.pas        --source src/business/*.pas        --source src/storage/*.pas        --source src/utils/*.pas
```

---

**End of Section 10: Source Code Organization and File Structure**


---

## 11. Coding Task List

This section provides a comprehensive task list for implementing the Free Pascal Task Manager Library. Tasks are organized by module and component, following the architecture defined in previous sections. Use `[x]` to mark completed tasks and `[ ]` for pending tasks.

### 11.1 Foundation and Core Types

#### 11.1.1 TaskTypes.pas - Core Type Definitions
- [ ] Define `TTaskStatus` enumeration (Pending, InProgress, Completed, Cancelled, OnHold)
- [ ] Define `TTaskPriority` enumeration (Low, Normal, High, Critical)
- [ ] Define `TTaskCategory` enumeration (Work, Personal, Shopping, Health, Education, Other)
- [ ] Define `TValidationResult` record with `IsValid` and `ErrorMessages` fields
- [ ] Define `TTaskFilterCriteria` record for filtering parameters
- [ ] Define `TTaskStatisticsData` record for analytics data
- [ ] Define helper functions for enum-to-string and string-to-enum conversions
- [ ] Add comprehensive XML documentation comments to all types

#### 11.1.2 TaskExceptions.pas - Custom Exception Classes
- [ ] Implement `ETaskException` base exception class
- [ ] Implement `ETaskValidationException` for validation errors
- [ ] Implement `ETaskNotFoundException` for missing task errors
- [ ] Implement `ETaskStorageException` for storage-related errors
- [ ] Implement `ETaskDuplicateException` for duplicate task errors
- [ ] Add constructors with custom error messages
- [ ] Add XML documentation for all exception classes

### 11.2 Data Model Layer

#### 11.2.1 TaskModel.pas - TTask Class
- [ ] Implement `TTask` class with private fields (FID, FTitle, FDescription, etc.)
- [ ] Implement constructor `Create` with default values
- [ ] Implement destructor `Destroy` for cleanup
- [ ] Implement property `ID` (TGUID, read-only)
- [ ] Implement property `Title` (string, read/write with validation)
- [ ] Implement property `Description` (string, read/write)
- [ ] Implement property `Status` (TTaskStatus, read/write)
- [ ] Implement property `Priority` (TTaskPriority, read/write)
- [ ] Implement property `Category` (TTaskCategory, read/write)
- [ ] Implement property `DueDate` (TDateTime, read/write)
- [ ] Implement property `CreatedDate` (TDateTime, read-only)
- [ ] Implement property `ModifiedDate` (TDateTime, read-only)
- [ ] Implement property `CompletedDate` (TDateTime, read-only)
- [ ] Implement property `Tags` (TStringList, read-only)
- [ ] Implement method `Clone: TTask` for deep copying
- [ ] Implement method `IsOverdue: Boolean` to check due date
- [ ] Implement method `MarkAsCompleted` to update status and completion date
- [ ] Implement method `AddTag(const ATag: string)` for tag management
- [ ] Implement method `RemoveTag(const ATag: string)` for tag management
- [ ] Implement method `HasTag(const ATag: string): Boolean` for tag queries
- [ ] Add internal method `UpdateModifiedDate` called on any property change
- [ ] Add comprehensive unit tests for TTask class

### 11.3 Collection Layer

#### 11.3.1 TaskList.pas - TTaskList Class
- [ ] Implement `TTaskList` class using `TObjectList<TTask>` internally
- [ ] Implement constructor `Create` with ownership management
- [ ] Implement destructor `Destroy` for cleanup
- [ ] Implement method `Add(ATask: TTask): Integer` to add tasks
- [ ] Implement method `Remove(ATask: TTask): Boolean` to remove tasks
- [ ] Implement method `Delete(AIndex: Integer)` to delete by index
- [ ] Implement method `Clear` to remove all tasks
- [ ] Implement method `FindByID(const AID: TGUID): TTask` to find tasks
- [ ] Implement method `IndexOf(ATask: TTask): Integer` to get index
- [ ] Implement property `Count: Integer` (read-only)
- [ ] Implement property `Items[Index: Integer]: TTask` (default array property)
- [ ] Implement method `ToArray: TTaskArray` to convert to dynamic array
- [ ] Implement iterator support for `for..in` loops
- [ ] Add thread-safety considerations (critical sections if needed)
- [ ] Add comprehensive unit tests for TTaskList class

### 11.4 Business Logic Layer

#### 11.4.1 TaskManager.pas - TTaskManager Class
- [ ] Implement `TTaskManager` class as main entry point
- [ ] Implement constructor `Create(AStorage: ITaskStorage)` with storage injection
- [ ] Implement destructor `Destroy` for cleanup
- [ ] Implement method `CreateTask(const ATitle, ADescription: string): TTask`
- [ ] Implement method `UpdateTask(ATask: TTask): Boolean`
- [ ] Implement method `DeleteTask(const AID: TGUID): Boolean`
- [ ] Implement method `GetTask(const AID: TGUID): TTask`
- [ ] Implement method `GetAllTasks: TTaskList`
- [ ] Implement method `LoadTasks(const AFileName: string): Boolean`
- [ ] Implement method `SaveTasks(const AFileName: string): Boolean`
- [ ] Implement method `GetTasksByStatus(AStatus: TTaskStatus): TTaskList`
- [ ] Implement method `GetTasksByPriority(APriority: TTaskPriority): TTaskList`
- [ ] Implement method `GetTasksByCategory(ACategory: TTaskCategory): TTaskList`
- [ ] Implement method `GetOverdueTasks: TTaskList`
- [ ] Implement method `GetTasksDueToday: TTaskList`
- [ ] Implement method `SearchTasks(const ASearchTerm: string): TTaskList`
- [ ] Implement method `SortTasks(ASortBy: TTaskSortField; ADescending: Boolean)`
- [ ] Add property `TaskCount: Integer` (read-only)
- [ ] Add property `Storage: ITaskStorage` (read-only)
- [ ] Implement error handling for all operations
- [ ] Add comprehensive unit tests for TTaskManager class

#### 11.4.2 TaskValidator.pas - TTaskValidator Class
- [ ] Implement `TTaskValidator` class for validation logic
- [ ] Implement constructor `Create` with validation rules initialization
- [ ] Implement method `ValidateTask(ATask: TTask): TValidationResult`
- [ ] Implement method `ValidateTitle(const ATitle: string): TValidationResult`
- [ ] Implement method `ValidateDescription(const ADesc: string): TValidationResult`
- [ ] Implement method `ValidateDueDate(const ADueDate: TDateTime): TValidationResult`
- [ ] Implement validation rule: Title must not be empty
- [ ] Implement validation rule: Title length between 1-200 characters
- [ ] Implement validation rule: Description maximum 2000 characters
- [ ] Implement validation rule: Due date must be in the future (if set)
- [ ] Implement validation rule: Tag names must be valid (no special chars)
- [ ] Add configurable validation rules (min/max lengths, etc.)
- [ ] Add property `MaxTitleLength: Integer` (default 200)
- [ ] Add property `MaxDescriptionLength: Integer` (default 2000)
- [ ] Add comprehensive unit tests for TTaskValidator class

#### 11.4.3 TaskFilter.pas - TTaskFilter Class
- [ ] Implement `TTaskFilter` class for advanced filtering
- [ ] Implement constructor `Create`
- [ ] Implement method `Filter(ATaskList: TTaskList; ACriteria: TTaskFilterCriteria): TTaskList`
- [ ] Implement method `FilterByStatus(ATaskList: TTaskList; AStatus: TTaskStatus): TTaskList`
- [ ] Implement method `FilterByPriority(ATaskList: TTaskList; APriority: TTaskPriority): TTaskList`
- [ ] Implement method `FilterByCategory(ATaskList: TTaskList; ACategory: TTaskCategory): TTaskList`
- [ ] Implement method `FilterByDateRange(ATaskList: TTaskList; AStartDate, AEndDate: TDateTime): TTaskList`
- [ ] Implement method `FilterByTags(ATaskList: TTaskList; ATags: TStringArray): TTaskList`
- [ ] Implement method `FilterBySearchTerm(ATaskList: TTaskList; const ASearchTerm: string): TTaskList`
- [ ] Implement method `FilterOverdue(ATaskList: TTaskList): TTaskList`
- [ ] Implement method `FilterDueToday(ATaskList: TTaskList): TTaskList`
- [ ] Implement method `FilterDueThisWeek(ATaskList: TTaskList): TTaskList`
- [ ] Implement support for combining multiple filter criteria
- [ ] Implement case-insensitive search functionality
- [ ] Add comprehensive unit tests for TTaskFilter class

#### 11.4.4 TaskStatistics.pas - TTaskStatistics Class
- [ ] Implement `TTaskStatistics` class for analytics
- [ ] Implement constructor `Create`
- [ ] Implement method `CalculateStatistics(ATaskList: TTaskList): TTaskStatisticsData`
- [ ] Implement method `GetTotalTaskCount(ATaskList: TTaskList): Integer`
- [ ] Implement method `GetCompletedTaskCount(ATaskList: TTaskList): Integer`
- [ ] Implement method `GetPendingTaskCount(ATaskList: TTaskList): Integer`
- [ ] Implement method `GetOverdueTaskCount(ATaskList: TTaskList): Integer`
- [ ] Implement method `GetCompletionRate(ATaskList: TTaskList): Double`
- [ ] Implement method `GetTasksByStatusCount(ATaskList: TTaskList): TStatusCountArray`
- [ ] Implement method `GetTasksByPriorityCount(ATaskList: TTaskList): TPriorityCountArray`
- [ ] Implement method `GetTasksByCategoryCount(ATaskList: TTaskList): TCategoryCountArray`
- [ ] Implement method `GetAverageCompletionTime(ATaskList: TTaskList): Double`
- [ ] Implement method `GetMostUsedTags(ATaskList: TTaskList; ATopN: Integer): TStringArray`
- [ ] Implement method `GetProductivityTrend(ATaskList: TTaskList; ADays: Integer): TProductivityData`
- [ ] Add comprehensive unit tests for TTaskStatistics class

### 11.5 Storage Layer

#### 11.5.1 TaskStorage.pas - ITaskStorage Interface
- [ ] Define `ITaskStorage` interface with standard methods
- [ ] Define method `LoadFromFile(const AFileName: string): TTaskList`
- [ ] Define method `SaveToFile(ATaskList: TTaskList; const AFileName: string): Boolean`
- [ ] Define method `GetSupportedExtension: string`
- [ ] Define method `GetFormatName: string`
- [ ] Add XML documentation for interface and all methods

#### 11.5.2 TaskStorageJSON.pas - JSON Storage Implementation
- [ ] Implement `TJSONTaskStorage` class implementing `ITaskStorage`
- [ ] Implement constructor `Create`
- [ ] Implement method `LoadFromFile(const AFileName: string): TTaskList`
- [ ] Implement method `SaveToFile(ATaskList: TTaskList; const AFileName: string): Boolean`
- [ ] Implement method `GetSupportedExtension: string` (returns '.json')
- [ ] Implement method `GetFormatName: string` (returns 'JSON')
- [ ] Implement JSON serialization using `fpjson` unit
- [ ] Implement proper encoding/decoding of GUID fields
- [ ] Implement proper encoding/decoding of DateTime fields
- [ ] Implement proper encoding/decoding of Tags (string array)
- [ ] Implement proper encoding/decoding of enumerations
- [ ] Add error handling for malformed JSON files
- [ ] Add support for pretty-printing JSON output
- [ ] Add comprehensive unit tests for JSON storage

#### 11.5.3 TaskStorageXML.pas - XML Storage Implementation
- [ ] Implement `TXMLTaskStorage` class implementing `ITaskStorage`
- [ ] Implement constructor `Create`
- [ ] Implement method `LoadFromFile(const AFileName: string): TTaskList`
- [ ] Implement method `SaveToFile(ATaskList: TTaskList; const AFileName: string): Boolean`
- [ ] Implement method `GetSupportedExtension: string` (returns '.xml')
- [ ] Implement method `GetFormatName: string` (returns 'XML')
- [ ] Implement XML serialization using `DOM` and `XMLRead/XMLWrite` units
- [ ] Implement proper XML schema/structure for task data
- [ ] Implement proper encoding/decoding of special characters
- [ ] Implement proper encoding/decoding of DateTime as ISO 8601
- [ ] Add error handling for malformed XML files
- [ ] Add support for formatted XML output
- [ ] Add comprehensive unit tests for XML storage

#### 11.5.4 TaskStorageCSV.pas - CSV Storage Implementation
- [ ] Implement `TCSVTaskStorage` class implementing `ITaskStorage`
- [ ] Implement constructor `Create`
- [ ] Implement method `LoadFromFile(const AFileName: string): TTaskList`
- [ ] Implement method `SaveToFile(ATaskList: TTaskList; const AFileName: string): Boolean`
- [ ] Implement method `GetSupportedExtension: string` (returns '.csv')
- [ ] Implement method `GetFormatName: string` (returns 'CSV')
- [ ] Implement CSV parsing with proper quote and delimiter handling
- [ ] Implement CSV header row with field names
- [ ] Implement proper encoding/decoding of GUID fields
- [ ] Implement proper encoding/decoding of DateTime fields
- [ ] Implement proper encoding/decoding of Tags (semicolon-separated)
- [ ] Implement proper escaping of special characters (quotes, commas)
- [ ] Add error handling for malformed CSV files
- [ ] Add comprehensive unit tests for CSV storage

### 11.6 Utility Layer

#### 11.6.1 TaskUtils.pas - General Utilities
- [ ] Implement `TTaskUtils` class with class methods
- [ ] Implement method `GenerateTaskID: TGUID` for unique ID generation
- [ ] Implement method `StatusToString(AStatus: TTaskStatus): string`
- [ ] Implement method `StringToStatus(const AStr: string): TTaskStatus`
- [ ] Implement method `PriorityToString(APriority: TTaskPriority): string`
- [ ] Implement method `StringToPriority(const AStr: string): TTaskPriority`
- [ ] Implement method `CategoryToString(ACategory: TTaskCategory): string`
- [ ] Implement method `StringToCategory(const AStr: string): TTaskCategory`
- [ ] Implement method `FormatTaskSummary(ATask: TTask): string`
- [ ] Implement method `SanitizeInput(const AInput: string): string`
- [ ] Add comprehensive unit tests for TaskUtils

#### 11.6.2 DateTimeUtils.pas - Date/Time Utilities
- [ ] Implement `TDateTimeUtils` class with class methods
- [ ] Implement method `IsToday(ADate: TDateTime): Boolean`
- [ ] Implement method `IsTomorrow(ADate: TDateTime): Boolean`
- [ ] Implement method `IsThisWeek(ADate: TDateTime): Boolean`
- [ ] Implement method `IsOverdue(ADate: TDateTime): Boolean`
- [ ] Implement method `DaysBetween(ADate1, ADate2: TDateTime): Integer`
- [ ] Implement method `FormatDateTimeISO8601(ADateTime: TDateTime): string`
- [ ] Implement method `ParseDateTimeISO8601(const AStr: string): TDateTime`
- [ ] Implement method `GetStartOfDay(ADate: TDateTime): TDateTime`
- [ ] Implement method `GetEndOfDay(ADate: TDateTime): TDateTime`
- [ ] Implement method `GetStartOfWeek(ADate: TDateTime): TDateTime`
- [ ] Add comprehensive unit tests for DateTimeUtils

#### 11.6.3 StringUtils.pas - String Utilities
- [ ] Implement `TStringUtils` class with class methods
- [ ] Implement method `IsNullOrEmpty(const AStr: string): Boolean`
- [ ] Implement method `IsNullOrWhiteSpace(const AStr: string): Boolean`
- [ ] Implement method `Trim(const AStr: string): string`
- [ ] Implement method `SplitString(const AStr, ADelimiter: string): TStringArray`
- [ ] Implement method `JoinStrings(AStrings: TStringArray; const ADelimiter: string): string`
- [ ] Implement method `ContainsIgnoreCase(const AStr, ASubStr: string): Boolean`
- [ ] Implement method `StartsWithIgnoreCase(const AStr, APrefix: string): Boolean`
- [ ] Implement method `EndsWithIgnoreCase(const AStr, ASuffix: string): Boolean`
- [ ] Add comprehensive unit tests for StringUtils

### 11.7 Testing Infrastructure

#### 11.7.1 Test Framework Setup
- [ ] Set up FPCUnit testing framework
- [ ] Create `tests/` directory structure
- [ ] Create test project file `TaskManagerTests.lpi`
- [ ] Create test program `TaskManagerTestRunner.pas`
- [ ] Configure test compilation settings
- [ ] Set up test data fixtures directory

#### 11.7.2 Unit Tests - Core Types
- [ ] Create `TaskTypesTests.pas` test unit
- [ ] Test enumeration conversions (enum to string, string to enum)
- [ ] Test record types initialization and field access
- [ ] Test type validation and edge cases

#### 11.7.3 Unit Tests - TTask Class
- [ ] Create `TaskModelTests.pas` test unit
- [ ] Test task creation with valid data
- [ ] Test task property getters and setters
- [ ] Test task validation (empty title, title length, etc.)
- [ ] Test task cloning (deep copy verification)
- [ ] Test task status transitions
- [ ] Test task tag management (add, remove, has)
- [ ] Test task date/time properties
- [ ] Test overdue detection logic
- [ ] Test MarkAsCompleted functionality
- [ ] Achieve 100% code coverage for TTask class

#### 11.7.4 Unit Tests - TTaskList Class
- [ ] Create `TaskListTests.pas` test unit
- [ ] Test task list creation and destruction
- [ ] Test adding tasks to list
- [ ] Test removing tasks from list
- [ ] Test finding tasks by ID
- [ ] Test list iteration
- [ ] Test list clearing
- [ ] Test list indexing and bounds checking
- [ ] Achieve 95%+ code coverage for TTaskList class

#### 11.7.5 Unit Tests - TTaskManager Class
- [ ] Create `TaskManagerTests.pas` test unit
- [ ] Test task creation through manager
- [ ] Test task update operations
- [ ] Test task deletion operations
- [ ] Test task retrieval operations
- [ ] Test filtering operations (by status, priority, category)
- [ ] Test search functionality
- [ ] Test sorting functionality
- [ ] Test load/save operations with different storage formats
- [ ] Test error handling (invalid IDs, null references, etc.)
- [ ] Achieve 90%+ code coverage for TTaskManager class

#### 11.7.6 Unit Tests - TTaskValidator Class
- [ ] Create `TaskValidatorTests.pas` test unit
- [ ] Test title validation (empty, too long, valid)
- [ ] Test description validation (too long, valid)
- [ ] Test due date validation (past, future, null)
- [ ] Test tag validation (invalid characters, valid tags)
- [ ] Test complete task validation
- [ ] Achieve 100% code coverage for TTaskValidator class

#### 11.7.7 Unit Tests - TTaskFilter Class
- [ ] Create `TaskFilterTests.pas` test unit
- [ ] Test status filtering
- [ ] Test priority filtering
- [ ] Test category filtering
- [ ] Test date range filtering
- [ ] Test tag filtering
- [ ] Test search term filtering
- [ ] Test overdue filtering
- [ ] Test combined filter criteria
- [ ] Test edge cases (empty lists, no matches)
- [ ] Achieve 95%+ code coverage for TTaskFilter class

#### 11.7.8 Unit Tests - TTaskStatistics Class
- [ ] Create `TaskStatisticsTests.pas` test unit
- [ ] Test basic count calculations
- [ ] Test completion rate calculation
- [ ] Test status distribution calculation
- [ ] Test priority distribution calculation
- [ ] Test category distribution calculation
- [ ] Test average completion time calculation
- [ ] Test most used tags calculation
- [ ] Test productivity trend calculation
- [ ] Achieve 90%+ code coverage for TTaskStatistics class

#### 11.7.9 Unit Tests - Storage Implementations
- [ ] Create `TaskStorageJSONTests.pas` test unit
- [ ] Test JSON save and load operations
- [ ] Test JSON format correctness
- [ ] Test JSON error handling (malformed files)
- [ ] Create `TaskStorageXMLTests.pas` test unit
- [ ] Test XML save and load operations
- [ ] Test XML format correctness
- [ ] Test XML error handling (malformed files)
- [ ] Create `TaskStorageCSVTests.pas` test unit
- [ ] Test CSV save and load operations
- [ ] Test CSV format correctness
- [ ] Test CSV error handling (malformed files)
- [ ] Test special character escaping in all formats
- [ ] Test GUID and DateTime serialization in all formats
- [ ] Achieve 90%+ code coverage for all storage classes

#### 11.7.10 Integration Tests
- [ ] Create `IntegrationTests.pas` test unit
- [ ] Test complete CRUD workflow (Create, Read, Update, Delete)
- [ ] Test data persistence across save/load cycles
- [ ] Test switching between different storage formats
- [ ] Test large dataset handling (1000+ tasks)
- [ ] Test concurrent access scenarios (if applicable)
- [ ] Test error recovery scenarios

#### 11.7.11 Performance Tests
- [ ] Create `PerformanceTests.pas` test unit
- [ ] Benchmark task creation (target: 10,000 tasks/second)
- [ ] Benchmark task filtering (target: 1,000,000 tasks in < 1 second)
- [ ] Benchmark task search (target: 100,000 tasks in < 500ms)
- [ ] Benchmark JSON save/load (target: 10,000 tasks in < 2 seconds)
- [ ] Benchmark XML save/load (target: 10,000 tasks in < 3 seconds)
- [ ] Benchmark memory usage (target: < 1KB per task average)

### 11.8 Documentation and Examples

#### 11.8.1 Code Documentation
- [ ] Add XML documentation comments to all public classes
- [ ] Add XML documentation comments to all public methods
- [ ] Add XML documentation comments to all public properties
- [ ] Add XML documentation comments to all interfaces
- [ ] Add usage examples in documentation comments
- [ ] Document all exceptions that can be raised
- [ ] Document thread-safety considerations

#### 11.8.2 Example Programs
- [ ] Create `examples/` directory
- [ ] Create `BasicUsage.pas` - simple CRUD example
- [ ] Create `FilteringExample.pas` - filtering and search example
- [ ] Create `StorageExample.pas` - working with different storage formats
- [ ] Create `StatisticsExample.pas` - generating task statistics
- [ ] Create `ValidationExample.pas` - input validation example
- [ ] Create `BatchOperations.pas` - bulk task operations example
- [ ] Ensure all examples compile and run successfully
- [ ] Add comments explaining each example

#### 11.8.3 API Documentation
- [ ] Generate API documentation using PasDoc
- [ ] Create HTML documentation output
- [ ] Create searchable index
- [ ] Review generated documentation for completeness
- [ ] Host documentation (optional - GitHub Pages or similar)

#### 11.8.4 User Guide
- [ ] Create `docs/USER_GUIDE.md` with getting started guide
- [ ] Document installation instructions
- [ ] Document basic usage patterns
- [ ] Document advanced usage patterns
- [ ] Document troubleshooting common issues
- [ ] Document performance optimization tips

### 11.9 Build System and Deployment

#### 11.9.1 Build Configuration
- [ ] Create Makefile for command-line compilation
- [ ] Create Lazarus package file (`TaskManagerLib.lpk`)
- [ ] Create Free Pascal project file (`TaskManager.lpi`)
- [ ] Configure compiler options (optimization, debug symbols)
- [ ] Configure search paths for units
- [ ] Configure output directories (lib, bin, obj)

#### 11.9.2 Build Targets
- [ ] Implement `make all` - build library and examples
- [ ] Implement `make lib` - build library only
- [ ] Implement `make tests` - build and run tests
- [ ] Implement `make examples` - build example programs
- [ ] Implement `make docs` - generate documentation
- [ ] Implement `make clean` - remove build artifacts
- [ ] Implement `make install` - install library system-wide (optional)

#### 11.9.3 Continuous Integration
- [ ] Create `.github/workflows/build.yml` for GitHub Actions
- [ ] Configure CI to run on push and pull requests
- [ ] Add compilation step for Linux
- [ ] Add compilation step for Windows (optional)
- [ ] Add compilation step for macOS (optional)
- [ ] Add test execution step
- [ ] Add code coverage reporting step
- [ ] Add documentation generation step

#### 11.9.4 Version Control
- [ ] Create `.gitignore` file for Pascal projects
- [ ] Ignore compiled files (*.o, *.ppu, *.compiled)
- [ ] Ignore build directories (lib, bin, obj)
- [ ] Ignore backup files (*.bak, *~)
- [ ] Ignore IDE-specific files (*.lps, *.local)
- [ ] Set up proper line endings (LF for Unix, CRLF for Windows)

#### 11.9.5 Packaging and Distribution
- [ ] Create release package structure
- [ ] Include source code in release package
- [ ] Include compiled library in release package
- [ ] Include documentation in release package
- [ ] Include examples in release package
- [ ] Create README.md with installation instructions
- [ ] Create LICENSE file (choose appropriate license)
- [ ] Create CHANGELOG.md for version history
- [ ] Tag releases with semantic versioning (v1.0.0, v1.1.0, etc.)

### 11.10 Code Quality and Maintenance

#### 11.10.1 Code Review Checklist
- [ ] Verify all code follows Pascal naming conventions
- [ ] Verify all code has proper XML documentation
- [ ] Verify no memory leaks (all objects properly freed)
- [ ] Verify proper error handling in all methods
- [ ] Verify thread-safety where required
- [ ] Verify input validation on all public methods
- [ ] Run static code analysis tools
- [ ] Review code for potential optimizations

#### 11.10.2 Refactoring Tasks
- [ ] Review and refactor units exceeding 1000 lines
- [ ] Review and refactor methods with cyclomatic complexity > 10
- [ ] Extract common code into utility functions
- [ ] Remove duplicate code
- [ ] Improve variable and method naming clarity
- [ ] Optimize performance bottlenecks identified in profiling

#### 11.10.3 Technical Debt
- [ ] Document known issues and limitations
- [ ] Create GitHub issues for feature requests
- [ ] Create GitHub issues for bug reports
- [ ] Prioritize technical debt items
- [ ] Schedule refactoring sprints

### 11.11 Advanced Features (Future Enhancements)

#### 11.11.1 Optional Advanced Features
- [ ] Implement task dependencies (task A requires task B to be completed)
- [ ] Implement recurring tasks (daily, weekly, monthly patterns)
- [ ] Implement task templates for common task types
- [ ] Implement task notifications/reminders
- [ ] Implement task attachments (file references)
- [ ] Implement task comments/notes history
- [ ] Implement task assignment to users (multi-user support)
- [ ] Implement task time tracking (estimated vs actual time)
- [ ] Implement task subtasks (hierarchical task structure)
- [ ] Implement task import/export in additional formats (iCal, Outlook, etc.)

#### 11.11.2 Database Storage Backend
- [ ] Design database schema for task storage
- [ ] Implement `TDatabaseTaskStorage` class
- [ ] Support SQLite database backend
- [ ] Support PostgreSQL database backend (optional)
- [ ] Support MySQL database backend (optional)
- [ ] Implement connection pooling
- [ ] Implement transaction support
- [ ] Add database migration scripts

#### 11.11.3 REST API (Optional)
- [ ] Design REST API endpoints
- [ ] Implement HTTP server using Free Pascal HTTP server libraries
- [ ] Implement GET /api/tasks - retrieve all tasks
- [ ] Implement GET /api/tasks/{id} - retrieve specific task
- [ ] Implement POST /api/tasks - create new task
- [ ] Implement PUT /api/tasks/{id} - update task
- [ ] Implement DELETE /api/tasks/{id} - delete task
- [ ] Implement GET /api/tasks/filter - filter tasks with query parameters
- [ ] Implement authentication and authorization
- [ ] Document API using OpenAPI/Swagger

---

## Task Completion Summary

**Total Tasks**: To be counted as implementation progresses

**Completed Tasks**: 0

**Completion Percentage**: 0%

### Priority Guidelines

- **Critical (Must Have)**: Sections 11.1 - 11.6 (Foundation, Data Model, Business Logic, Storage, Utilities)
- **High (Should Have)**: Sections 11.7 - 11.9 (Testing, Documentation, Build System)
- **Medium (Nice to Have)**: Section 11.10 (Code Quality and Maintenance)
- **Low (Future)**: Section 11.11 (Advanced Features)

### Recommended Implementation Order

1. **Phase 1 - Foundation** (Week 1-2)
   - Complete sections 11.1 (Foundation and Core Types)
   - Complete sections 11.2 (Data Model Layer)
   - Complete sections 11.3 (Collection Layer)

2. **Phase 2 - Business Logic** (Week 3-4)
   - Complete sections 11.4 (Business Logic Layer)
   - Complete sections 11.6 (Utility Layer)

3. **Phase 3 - Storage** (Week 5)
   - Complete sections 11.5 (Storage Layer)

4. **Phase 4 - Testing** (Week 6-7)
   - Complete sections 11.7 (Testing Infrastructure)

5. **Phase 5 - Polish** (Week 8)
   - Complete sections 11.8 (Documentation and Examples)
   - Complete sections 11.9 (Build System and Deployment)
   - Complete sections 11.10 (Code Quality)

6. **Phase 6 - Future** (Optional)
   - Complete sections 11.11 (Advanced Features)

---

**End of Section 11: Coding Task List**
