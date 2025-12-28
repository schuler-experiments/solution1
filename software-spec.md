
# Free Pascal Task Manager - Software Specification

## Document Information
- **Project Name:** Free Pascal Task Manager
- **Version:** 1.0
- **Last Updated:** 2024
- **Framework:** Free Pascal / Object Pascal with mORMot Framework
- **Status:** Architecture & Implementation Specification

---

## Table of Contents
1. [Overview](#1-overview)
2. [Software Architecture](#2-software-architecture)
3. [Detailed Module Descriptions](#3-detailed-module-descriptions)
4. [Data Models and Structures](#4-data-models-and-structures)
5. [API Endpoints and Usage](#5-api-endpoints-and-usage)
6. [User Interface Designs](#6-user-interface-designs)
7. [Third-Party Libraries and Services](#7-third-party-libraries-and-services)
8. [Deployment and Scaling Strategies](#8-deployment-and-scaling-strategies)
9. [Testing Strategies and Coverage](#9-testing-strategies-and-coverage)
10. [Class Diagrams and Methods](#10-class-diagrams-and-methods)
11. [Source Code Organization](#11-source-code-organization)
12. [Coding Task List](#12-coding-task-list)

---

## 1. Overview

### 1.1 Project Purpose
The Free Pascal Task Manager is a comprehensive, modular task management system built with Free Pascal/Object Pascal and the mORMot framework. The system provides reusable components for task management, team collaboration, productivity tracking, and workflow automation without direct user interface dependencies (no ReadLn or console input).

### 1.2 Key Features
- **Core Task Management:** Create, update, delete, and organize tasks with priorities, deadlines, and statuses
- **Advanced Features:** Recurring tasks, templates, time tracking, resource allocation
- **Collaboration:** Team management, task assignments, comments, and notifications
- **Productivity:** Focus mode, gamification, smart suggestions, knowledge base integration
- **Organization:** Boards/Kanban, tags, search capabilities, meetings management
- **Wellness:** Well-being tracking and lifestyle management integration
- **Extensibility:** Modular architecture allowing feature extensions

### 1.3 Technology Stack
- **Language:** Free Pascal / Object Pascal (FPC 3.2+)
- **Framework:** mORMot 2.x (Model-View-Controller + ORM + REST)
- **Database:** SQLite3 (via mORMot ORM)
- **Architecture Pattern:** Service-Oriented Architecture with Clean Architecture principles
- **Design Patterns:** Repository, Service Layer, Dependency Injection, Observer

### 1.4 Target Platforms
- Linux (primary)
- Windows
- macOS
- FreeBSD

### 1.5 Design Principles
- **Modularity:** Each feature is a separate unit that can be included or excluded
- **Reusability:** Core components designed for integration into GUI, web, or console applications
- **No Direct I/O:** No ReadLn, WriteLn for user interaction (library approach)
- **Interface-Based:** Services defined through interfaces for testability and flexibility
- **Data-Driven:** Configuration and behavior controlled through data models
- **Type Safety:** Strong typing with Object Pascal's type system

---

## 2. Software Architecture

### 2.1 Architectural Overview

The system follows a **layered architecture** with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                        │
│  (Console Apps, GUI Apps, Web Services - Not Included)     │
└─────────────────────────────────────────────────────────────┘
                            ↓ ↑
┌─────────────────────────────────────────────────────────────┐
│                     Service Layer                           │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │ Task         │  │ Comment      │  │ Tag          │     │
│  │ Services     │  │ Services     │  │ Services     │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
│                                                             │
│  ┌────────────────────────────────────────────────────┐   │
│  │  Feature Services (Advanced, Boards, Team, etc.)   │   │
│  └────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                            ↓ ↑
┌─────────────────────────────────────────────────────────────┐
│                     Domain Layer                            │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │ Task         │  │ Comment      │  │ Tag          │     │
│  │ Models       │  │ Models       │  │ Models       │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
└─────────────────────────────────────────────────────────────┘
                            ↓ ↑
┌─────────────────────────────────────────────────────────────┐
│                  Data Access Layer (mORMot ORM)             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │         mORMot REST/ORM Infrastructure               │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
                            ↓ ↑
┌─────────────────────────────────────────────────────────────┐
│                   Persistence Layer                         │
│              SQLite3 Database (File-Based)                  │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 Architectural Layers

#### 2.2.1 Domain Layer (Models)
- **Responsibility:** Define business entities and domain logic
- **Components:**
  - `task_models`: Core task entity definitions
  - `comment_models`: Comment entity definitions  
  - `tag_models`: Tag entity definitions
- **Pattern:** Rich Domain Models with mORMot TSQLRecord inheritance
- **Dependencies:** mORMot.orm.core, mORMot.core.base

#### 2.2.2 Service Layer (Business Logic)
- **Responsibility:** Implement business operations and orchestrate domain objects
- **Components:**
  - Interface Definitions: `task_services`, `comment_services`, `tag_services`
  - Implementations: `task_services_impl`, `comment_services_impl`, `tag_services_impl`
- **Pattern:** Service Interface + Implementation (Dependency Injection ready)
- **Dependencies:** Domain Models, mORMot.orm.rest

#### 2.2.3 Feature Modules Layer
- **Responsibility:** Extended functionality built on core services
- **Components:** 20 feature modules (detailed in Section 3)
- **Pattern:** Plugin/Module architecture
- **Dependencies:** Core services, domain models

#### 2.2.4 Data Access Layer
- **Responsibility:** Database operations, persistence, querying
- **Components:** mORMot ORM infrastructure
- **Pattern:** Repository pattern (via mORMot REST)
- **Dependencies:** mORMot.orm.sqlite3, mORMot.db.raw.sqlite3

#### 2.2.5 Infrastructure Layer
- **Responsibility:** Cross-cutting concerns (logging, security, networking)
- **Components:** mORMot framework services
- **Pattern:** Framework-provided infrastructure

### 2.3 Core Architectural Patterns

#### 2.3.1 Service-Oriented Architecture (SOA)
Each major functionality is exposed through service interfaces:

```pascal
type
  ITaskService = interface(IInvokable)
    function CreateTask(const ATask: TTaskModel): Int64;
    function GetTask(ATaskID: Int64): TTaskModel;
    function UpdateTask(const ATask: TTaskModel): Boolean;
    function DeleteTask(ATaskID: Int64): Boolean;
    function ListTasks(const AFilter: TTaskFilter): TTaskModelArray;
  end;
```

#### 2.3.2 Repository Pattern (via mORMot ORM)
Data access abstracted through mORMot's REST ORM:
- Automatic CRUD operations
- Query builder interface
- Transaction management
- Connection pooling

#### 2.3.3 Dependency Injection
Services depend on interfaces, not concrete implementations:
- Constructor injection for service dependencies
- Interface-based design for testability
- Factory pattern for service creation

#### 2.3.4 Observer Pattern
Event-driven notifications:
- Task status changes
- Comment additions
- Deadline alerts
- Team notifications

### 2.4 Module Dependency Graph

```
Core Modules (Foundation):
  task_models ──┐
  task_services ├──> All Feature Modules depend on these
  tag_models    │
  tag_services  │
  comment_models│
  comment_services ┘

Feature Modules (Can be independently enabled/disabled):
  taskmanager (Base) ──> taskmanagerenhanced ──> taskmanagerext
                    │
                    ├──> taskmanageradvanced ──> taskmanagersmart
                    ├──> taskmanagerboards
                    ├──> taskmanagerteam
                    ├──> taskmanagertemplates ──> taskmanagerrecurring
                    ├──> taskmanagertimetracking
                    ├──> taskmanagersearch
                    ├──> taskmanagernotifications
                    ├──> taskmanagerfocus
                    ├──> taskmanagergamify
                    ├──> taskmanagerknowledge
                    ├──> taskmanagerlifestyle
                    ├──> taskmanagermeetings
                    ├──> taskmanagerresource
                    └──> taskmanagerwellbeing
```

### 2.5 Data Flow Architecture


**Typical Task Creation Flow:**

```
User Request (External)
    ↓
[Service Layer: ITaskService.CreateTask()]
    ↓
1. Validate input data (title, description, priority, etc.)
2. Apply business rules (default values, calculated fields)
3. Check permissions (if team-based)
    ↓
[Domain Layer: TTaskModel instance creation]
    ↓
4. Set task properties
5. Generate unique ID (if not auto-increment)
6. Set creation timestamp
    ↓
[Data Access Layer: mORMot ORM]
    ↓
7. Begin transaction
8. Insert record into SQLite database
9. Commit transaction
    ↓
[Event System: Observer Pattern]
    ↓
10. Trigger OnTaskCreated event
11. Notify subscribers (notifications, logging, etc.)
    ↓
Return Task ID to caller
```

**Query Flow Example (List Tasks with Filters):**

```
User Request: Get all high-priority tasks due this week
    ↓
[Service Layer: ITaskService.ListTasks(filter)]
    ↓
1. Parse filter criteria
2. Build mORMot ORM query
    ↓
[Data Access Layer: mORMot ORM Query Builder]
    ↓
3. Generate SQL: 
   SELECT * FROM Task 
   WHERE Priority = 'High' 
   AND DueDate BETWEEN ? AND ?
    ↓
[SQLite3 Database]
    ↓
4. Execute query
5. Return result set
    ↓
[Domain Layer: Hydrate TTaskModel objects]
    ↓
6. Convert database rows to domain objects
7. Apply post-query business logic
    ↓
Return TTaskModelArray to caller
```

**Update Flow with Event Propagation:**

```
Update Task Status: "In Progress" → "Completed"
    ↓
[Service Layer: ITaskService.UpdateTask()]
    ↓
1. Load existing task from database
2. Validate state transition
3. Update fields (Status, CompletedDate, etc.)
    ↓
[Data Access Layer: mORMot ORM Update]
    ↓
4. Begin transaction
5. UPDATE Task SET Status=?, CompletedDate=? WHERE ID=?
6. Commit transaction
    ↓
[Event System: Multiple observers notified]
    ↓
7. OnTaskStatusChanged event
    ├──> Notification Service → Send completion notification
    ├──> Gamification Service → Award points
    ├──> Time Tracking Service → Stop timer
    └──> Statistics Service → Update metrics
    ↓
Return success status
```

**Cross-Module Data Flow (Task → Comment → Notification):**

```
Add Comment to Task
    ↓
[Comment Service Layer]
    ↓
1. Validate comment data
2. Check task exists
3. Create TCommentModel
    ↓
[Data Access: Insert Comment]
    ↓
4. Save comment to database
5. Link to parent task (foreign key)
    ↓
[Event: OnCommentAdded]
    ↓
6. Trigger notification system
    ↓
[Notification Service]
    ↓
7. Determine recipients (task assignees, watchers)
8. Create notification records
9. Queue for delivery
    ↓
[Delivery Mechanisms (External)]
    ├──> Email gateway (if configured)
    ├──> Push notification service
    └──> In-app notification store
```

**Dependency Injection Data Flow:**

```
Application Startup
    ↓
[Service Container/Registry Initialization]
    ↓
1. Register interface implementations:
   - ITaskService → TTaskServiceImpl
   - ICommentService → TCommentServiceImpl
   - ITagService → TTagServiceImpl
    ↓
2. Configure dependencies:
   - Services receive IRestOrm instance
   - Services receive configuration objects
    ↓
[Runtime Service Resolution]
    ↓
3. Request: "Give me ITaskService"
4. Container returns: TTaskServiceImpl instance
5. Instance already has dependencies injected
    ↓
Service ready for use
```

---

## 3. Detailed Module Descriptions

This section provides comprehensive descriptions of all modules in the Free Pascal Task Manager system. Modules are organized by layer and functionality.

### 3.1 Core Domain Models

#### 3.1.1 Task Models (`task_models.pas`)

**Purpose:** Define the core Task entity and related types.

**Key Components:**

- **`TTaskModel`**: Main task entity inheriting from `TSQLRecord`
  - Properties: ID, Title, Description, Status, Priority, DueDate, CreatedDate, CompletedDate, EstimatedHours, ActualHours
  - Methods: `Validate()`, `IsOverdue()`, `CalculateProgress()`
  
- **`TTaskStatus`**: Enumeration for task states
  ```pascal
  type
    TTaskStatus = (
      tsNotStarted,
      tsInProgress,
      tsBlocked,
      tsCompleted,
      tsCancelled
    );
  ```

- **`TTaskPriority`**: Enumeration for priority levels
  ```pascal
  type
    TTaskPriority = (
      tpLow,
      tpMedium,
      tpHigh,
      tpCritical
    );
  ```

- **`TTaskFilter`**: Record for filtering task queries
  ```pascal
  type
    TTaskFilter = record
      Status: TTaskStatus;
      Priority: TTaskPriority;
      StartDate: TDateTime;
      EndDate: TDateTime;
      TagIDs: TInt64DynArray;
      AssignedUserID: Int64;
      SearchText: RawUTF8;
    end;
  ```

**Dependencies:** 
- mORMot.orm.core
- mORMot.core.base
- mORMot.core.data

**File Location:** `src/models/task_models.pas`

---

#### 3.1.2 Comment Models (`comment_models.pas`)

**Purpose:** Define comment entities for task discussions.

**Key Components:**

- **`TCommentModel`**: Comment entity
  - Properties: ID, TaskID (foreign key), AuthorID, Content, CreatedDate, UpdatedDate, IsEdited
  - Methods: `Validate()`, `MarkAsEdited()`

- **`TCommentFilter`**: Record for filtering comments
  ```pascal
  type
    TCommentFilter = record
      TaskID: Int64;
      AuthorID: Int64;
      StartDate: TDateTime;
      EndDate: TDateTime;
      SearchText: RawUTF8;
    end;
  ```

**Dependencies:**
- task_models (for TaskID reference)
- mORMot.orm.core

**File Location:** `src/models/comment_models.pas`

---

#### 3.1.3 Tag Models (`tag_models.pas`)

**Purpose:** Define tagging system for task categorization.

**Key Components:**

- **`TTagModel`**: Tag entity
  - Properties: ID, Name, Color, Description, CreatedDate
  - Methods: `Validate()`, `GetDisplayName()`

- **`TTaskTagLink`**: Many-to-many relationship between tasks and tags
  - Properties: ID, TaskID, TagID, CreatedDate

**Dependencies:**
- task_models
- mORMot.orm.core

**File Location:** `src/models/tag_models.pas`

---

### 3.2 Core Service Interfaces

#### 3.2.1 Task Service Interface (`task_services.pas`)

**Purpose:** Define the contract for task management operations.

**Key Interface:**

```pascal
type
  ITaskService = interface(IInvokable)
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    
    // CRUD Operations
    function CreateTask(const ATask: TTaskModel): Int64;
    function GetTask(ATaskID: Int64): TTaskModel;
    function UpdateTask(const ATask: TTaskModel): Boolean;
    function DeleteTask(ATaskID: Int64): Boolean;
    
    // Query Operations
    function ListTasks(const AFilter: TTaskFilter): TTaskModelArray;
    function SearchTasks(const ASearchText: RawUTF8): TTaskModelArray;
    function GetTasksByStatus(AStatus: TTaskStatus): TTaskModelArray;
    function GetTasksByPriority(APriority: TTaskPriority): TTaskModelArray;
    function GetOverdueTasks: TTaskModelArray;
    
    // Status Management
    function ChangeTaskStatus(ATaskID: Int64; ANewStatus: TTaskStatus): Boolean;
    function CompleteTask(ATaskID: Int64): Boolean;
    function CancelTask(ATaskID: Int64; const AReason: RawUTF8): Boolean;
    
    // Validation
    function ValidateTask(const ATask: TTaskModel): TValidationResult;
  end;
```

**Design Notes:**
- Interface uses `IInvokable` for potential SOA/RPC exposure
- All methods return values (no var/out parameters for simplicity)
- Validation separated from creation for flexibility

**Dependencies:**
- task_models

**File Location:** `src/services/task_services.pas`

---

#### 3.2.2 Comment Service Interface (`comment_services.pas`)

**Purpose:** Define the contract for comment management operations.

**Key Interface:**

```pascal
type
  ICommentService = interface(IInvokable)
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    
    // CRUD Operations
    function CreateComment(const AComment: TCommentModel): Int64;
    function GetComment(ACommentID: Int64): TCommentModel;
    function UpdateComment(const AComment: TCommentModel): Boolean;
    function DeleteComment(ACommentID: Int64): Boolean;
    
    // Query Operations
    function GetCommentsForTask(ATaskID: Int64): TCommentModelArray;
    function GetCommentsByAuthor(AAuthorID: Int64): TCommentModelArray;
    function SearchComments(const ASearchText: RawUTF8): TCommentModelArray;
    
    // Comment Management
    function EditComment(ACommentID: Int64; const ANewContent: RawUTF8): Boolean;
    function GetCommentCount(ATaskID: Int64): Integer;
  end;
```

**Dependencies:**
- comment_models
- task_models

**File Location:** `src/services/comment_services.pas`

---

#### 3.2.3 Tag Service Interface (`tag_services.pas`)

**Purpose:** Define the contract for tag management and task tagging.

**Key Interface:**

```pascal
type
  ITagService = interface(IInvokable)
    ['{C3D4E5F6-A7B8-9012-CDEF-123456789012}']
    
    // Tag CRUD
    function CreateTag(const ATag: TTagModel): Int64;
    function GetTag(ATagID: Int64): TTagModel;
    function UpdateTag(const ATag: TTagModel): Boolean;
    function DeleteTag(ATagID: Int64): Boolean;
    
    // Tag Queries
    function ListAllTags: TTagModelArray;
    function SearchTags(const ASearchText: RawUTF8): TTagModelArray;
    
    // Task-Tag Association
    function AddTagToTask(ATaskID, ATagID: Int64): Boolean;
    function RemoveTagFromTask(ATaskID, ATagID: Int64): Boolean;
    function GetTagsForTask(ATaskID: Int64): TTagModelArray;
    function GetTasksForTag(ATagID: Int64): TTaskModelArray;
    
    // Bulk Operations
    function AddMultipleTagsToTask(ATaskID: Int64; ATagIDs: TInt64DynArray): Boolean;
    function RemoveAllTagsFromTask(ATaskID: Int64): Boolean;
  end;
```

**Dependencies:**
- tag_models
- task_models

**File Location:** `src/services/tag_services.pas`

---

### 3.3 Core Service Implementations

#### 3.3.1 Task Service Implementation (`task_services_impl.pas`)

**Purpose:** Implement task management business logic.

**Key Class:**

```pascal
type
  TTaskServiceImpl = class(TInterfacedObject, ITaskService)
  private
    FOrm: IRestOrm;
    FEventDispatcher: IEventDispatcher;
    
    procedure ValidateTaskData(const ATask: TTaskModel);
    procedure TriggerTaskCreatedEvent(ATaskID: Int64);
    procedure TriggerTaskUpdatedEvent(ATaskID: Int64);
    procedure TriggerTaskDeletedEvent(ATaskID: Int64);
  public
    constructor Create(AOrm: IRestOrm; AEventDispatcher: IEventDispatcher);
    
    // ITaskService implementation
    function CreateTask(const ATask: TTaskModel): Int64;
    function GetTask(ATaskID: Int64): TTaskModel;
    function UpdateTask(const ATask: TTaskModel): Boolean;
    function DeleteTask(ATaskID: Int64): Boolean;
    function ListTasks(const AFilter: TTaskFilter): TTaskModelArray;
    // ... other methods
  end;
```

**Implementation Highlights:**

- **Transaction Management:** All write operations wrapped in transactions
- **Validation:** Input validation before database operations
- **Event Triggering:** Publishes events after successful operations
- **Error Handling:** Comprehensive exception handling with rollback
- **Logging:** Integration with logging infrastructure

**Example Implementation (CreateTask):**

```pascal
function TTaskServiceImpl.CreateTask(const ATask: TTaskModel): Int64;
var
  NewTask: TTaskModel;
begin
  ValidateTaskData(ATask);
  
  NewTask := TTaskModel.Create;
  try
    NewTask.Title := ATask.Title;
    NewTask.Description := ATask.Description;
    NewTask.Status := tsNotStarted;
    NewTask.Priority := ATask.Priority;
    NewTask.DueDate := ATask.DueDate;
    NewTask.CreatedDate := NowUTC;
    
    Result := FOrm.Add(NewTask, True);
    
    if Result > 0 then
      TriggerTaskCreatedEvent(Result);
  finally
    NewTask.Free;
  end;
end;
```

**Dependencies:**
- task_services (interface)
- task_models
- mORMot.orm.rest
- Event system (custom)

**File Location:** `src/services/impl/task_services_impl.pas`

---

#### 3.3.2 Comment Service Implementation (`comment_services_impl.pas`)

**Purpose:** Implement comment management business logic.

**Key Class:**

```pascal
type
  TCommentServiceImpl = class(TInterfacedObject, ICommentService)
  private
    FOrm: IRestOrm;
    FEventDispatcher: IEventDispatcher;
    FTaskService: ITaskService;
    
    procedure ValidateCommentData(const AComment: TCommentModel);
    function TaskExists(ATaskID: Int64): Boolean;
  public
    constructor Create(AOrm: IRestOrm; AEventDispatcher: IEventDispatcher;
                      ATaskService: ITaskService);
    
    // ICommentService implementation
    function CreateComment(const AComment: TCommentModel): Int64;
    // ... other methods
  end;
```

**Implementation Highlights:**

- **Reference Validation:** Ensures referenced tasks exist
- **Content Sanitization:** Basic XSS prevention for comment content
- **Cascade Operations:** Optionally delete comments when tasks are deleted
- **Edit Tracking:** Marks comments as edited and tracks edit timestamp

**Dependencies:**
- comment_services (interface)
- comment_models
- task_services (for validation)
- mORMot.orm.rest

**File Location:** `src/services/impl/comment_services_impl.pas`

---

#### 3.3.3 Tag Service Implementation (`tag_services_impl.pas`)

**Purpose:** Implement tag management and task-tag association logic.

**Key Class:**

```pascal
type
  TTagServiceImpl = class(TInterfacedObject, ITagService)
  private
    FOrm: IRestOrm;
    
    procedure ValidateTagData(const ATag: TTagModel);
    function TagNameExists(const AName: RawUTF8; AExcludeID: Int64): Boolean;
  public
    constructor Create(AOrm: IRestOrm);
    
    // ITagService implementation
    function CreateTag(const ATag: TTagModel): Int64;
    function AddTagToTask(ATaskID, ATagID: Int64): Boolean;
    // ... other methods
  end;
```

**Implementation Highlights:**

- **Unique Names:** Ensures tag names are unique (case-insensitive)
- **Color Validation:** Validates hex color codes
- **Duplicate Prevention:** Prevents duplicate task-tag associations
- **Cascade Deletion:** Removes task-tag links when tags are deleted

**Dependencies:**
- tag_services (interface)
- tag_models
- task_models
- mORMot.orm.rest

**File Location:** `src/services/impl/tag_services_impl.pas`

---

### 3.4 Feature Modules (Extended Functionality)

The system includes 17 feature modules that extend core functionality. Each module is designed to be independently enabled or disabled.

#### 3.4.1 Task Manager Base (`taskmanager.pas`)

**Purpose:** Foundational module that initializes core services and provides basic task management.

**Key Components:**

- **`TTaskManager`**: Main coordinator class
  ```pascal
  type
    TTaskManager = class
    private
      FTaskService: ITaskService;
      FCommentService: ICommentService;
      FTagService: ITagService;
      FOrm: IRestOrm;
    public
      constructor Create(const ADatabasePath: TFileName);
      destructor Destroy; override;
      
      property TaskService: ITaskService read FTaskService;
      property CommentService: ICommentService read FCommentService;
      property TagService: ITagService read FTagService;
    end;
  ```

**Responsibilities:**
- Initialize mORMot ORM
- Create service instances
- Manage database connection
- Provide service access points

**Dependencies:**
- All core services
- mORMot framework

**File Location:** `src/modules/taskmanager.pas`

---

#### 3.4.2 Enhanced Task Manager (`taskmanagerenhanced.pas`)

**Purpose:** Add enhanced filtering, sorting, and bulk operations.

**Key Features:**

- Advanced task filtering with multiple criteria
- Custom sorting options (by priority, due date, creation date)
- Bulk task operations (bulk update, bulk delete, bulk tag assignment)
- Task statistics (count by status, average completion time)

**Key Components:**

```pascal
type
  TTaskManagerEnhanced = class(TTaskManager)
  public
    function GetTasksWithAdvancedFilter(const AFilter: TAdvancedTaskFilter): TTaskModelArray;
    function BulkUpdateStatus(ATaskIDs: TInt64DynArray; ANewStatus: TTaskStatus): Boolean;
    function BulkAssignTags(ATaskIDs: TInt64DynArray; ATagIDs: TInt64DynArray): Boolean;
    function GetTaskStatistics: TTaskStatistics;
  end;
  
  TAdvancedTaskFilter = record
    BasicFilter: TTaskFilter;
    SortBy: TTaskSortField;
    SortOrder: TSortOrder;
    Limit: Integer;
    Offset: Integer;
  end;
```

**Dependencies:**
- taskmanager
- All core services

**File Location:** `src/modules/taskmanagerenhanced.pas`

---

#### 3.4.3 Task Manager Extended (`taskmanagerext.pas`)

**Purpose:** Provide import/export functionality and data interchange.

**Key Features:**

- Export tasks to JSON, CSV, XML
- Import tasks from JSON, CSV, XML
- Backup and restore functionality
- Data migration tools

**Key Components:**

```pascal
type
  TTaskManagerExt = class(TTaskManagerEnhanced)
  public
    function ExportTasksToJSON(const AFilter: TTaskFilter): RawJSON;
    function ExportTasksToCSV(const AFilter: TTaskFilter): RawUTF8;
    function ImportTasksFromJSON(const AJSONData: RawJSON): TImportResult;
    function CreateBackup(const ABackupPath: TFileName): Boolean;
    function RestoreBackup(const ABackupPath: TFileName): Boolean;
  end;
  
  TImportResult = record
    SuccessCount: Integer;
    FailureCount: Integer;
    ErrorMessages: TRawUTF8DynArray;
  end;
```

**Dependencies:**
- taskmanagerenhanced
- JSON/XML serialization libraries (mORMot)

**File Location:** `src/modules/taskmanagerext.pas`

---

#### 3.4.4 Advanced Task Manager (`taskmanageradvanced.pas`)

**Purpose:** Advanced features like dependencies, subtasks, and workflows.

**Key Features:**

- Task dependencies (prerequisite tasks)
- Subtask hierarchies
- Custom workflows
- Task checklists

**Key Components:**

```pascal
type
  TTaskDependency = record
    TaskID: Int64;
    DependsOnTaskID: Int64;
    DependencyType: TDependencyType; // FinishToStart, StartToStart, etc.
  end;
  
  TTaskManagerAdvanced = class(TTaskManagerExt)
  public
    function AddDependency(ATaskID, ADependsOnTaskID: Int64; 
                          AType: TDependencyType): Boolean;
    function GetDependencies(ATaskID: Int64): TTaskDependencyArray;
    function CreateSubtask(AParentTaskID: Int64; const ASubtask: TTaskModel): Int64;
    function GetSubtasks(AParentTaskID: Int64): TTaskModelArray;
    function CanCompleteTask(ATaskID: Int64): Boolean; // Checks dependencies
  end;
```

**Dependencies:**
- taskmanagerext
- Custom dependency models

**File Location:** `src/modules/taskmanageradvanced.pas`

---

#### 3.4.5 Boards/Kanban Module (`taskmanagerboards.pas`)

**Purpose:** Kanban board visualization and management.

**Key Features:**

- Create and manage boards
- Define custom columns/lanes
- Move tasks between columns
- Board templates

**Key Components:**

```pascal
type
  TBoardModel = class(TSQLRecord)
  private
    FName: RawUTF8;
    FDescription: RawUTF8;
    FCreatedDate: TDateTime;
  published
    property Name: RawUTF8 read FName write FName;
    property Description: RawUTF8 read FDescription write FDescription;
    property CreatedDate: TDateTime read FCreatedDate write FCreatedDate;
  end;
  
  TBoardColumn = class(TSQLRecord)
  private
    FBoardID: Int64;
    FName: RawUTF8;
    FPosition: Integer;
    FWIPLimit: Integer; // Work In Progress limit
  published
    property BoardID: Int64 read FBoardID write FBoardID;
    property Name: RawUTF8 read FName write FName;
    property Position: Integer read FPosition write FPosition;
    property WIPLimit: Integer read FWIPLimit write FWIPLimit;
  end;
```

**Dependencies:**
- taskmanager
- Board-specific models

**File Location:** `src/modules/taskmanagerboards.pas`

---

#### 3.4.6 Team Management Module (`taskmanagerteam.pas`)

**Purpose:** Team collaboration, user management, and permissions.

**Key Features:**

- User/team management
- Task assignment to users
- Role-based permissions
- Team statistics

**Key Components:**

```pascal
type
  TUserModel = class(TSQLRecord)
  private
    FUsername: RawUTF8;
    FEmail: RawUTF8;
    FFullName: RawUTF8;
    FRole: TUserRole;
  published
    property Username: RawUTF8 read FUsername write FUsername;
    property Email: RawUTF8 read FEmail write FEmail;
    property FullName: RawUTF8 read FFullName write FFullName;
    property Role: TUserRole read FRole write FRole;
  end;
  
  TUserRole = (urViewer, urContributor, urManager, urAdmin);
```

**Dependencies:**
- taskmanager
- User/permission models

**File Location:** `src/modules/taskmanagerteam.pas`

---

#### 3.4.7 Templates Module (`taskmanagertemplates.pas`)

**Purpose:** Task templates for repeatable workflows.

**Key Features:**

- Create task templates
- Apply templates to create new tasks
- Template categories
- Template variables

**Key Components:**

```pascal
type
  TTaskTemplate = class(TSQLRecord)
  private
    FName: RawUTF8;
    FDescription: RawUTF8;
    FTemplateData: RawJSON; // Serialized task structure
    FCategory: RawUTF8;
  published
    property Name: RawUTF8 read FName write FName;
    property Description: RawUTF8 read FDescription write FDescription;
    property TemplateData: RawJSON read FTemplateData write FTemplateData;
    property Category: RawUTF8 read FCategory write FCategory;
  end;
```

**Dependencies:**
- taskmanager
- Template models

**File Location:** `src/modules/taskmanagertemplates.pas`

---

#### 3.4.8 Recurring Tasks Module (`taskmanagerrecurring.pas`)

**Purpose:** Handle recurring/repeating tasks.

**Key Features:**

- Define recurrence patterns (daily, weekly, monthly, custom)
- Automatic task generation
- Recurrence management
- Skip/reschedule occurrences

**Key Components:**

```pascal
type
  TRecurrencePattern = record
    Frequency: TRecurrenceFrequency; // Daily, Weekly, Monthly, Yearly
    Interval: Integer; // Every N days/weeks/months
    DaysOfWeek: set of TDayOfWeek;
    DayOfMonth: Integer;
    EndDate: TDateTime;
    MaxOccurrences: Integer;
  end;
  
  TRecurrenceFrequency = (rfDaily, rfWeekly, rfMonthly, rfYearly, rfCustom);
```

**Dependencies:**
- taskmanagertemplates
- Recurrence models

**File Location:** `src/modules/taskmanagerrecurring.pas`

---

#### 3.4.9 Time Tracking Module (`taskmanagertimetracking.pas`)

**Purpose:** Track time spent on tasks.

**Key Features:**

- Start/stop timers for tasks
- Manual time entries
- Time reports
- Estimated vs. actual time tracking

**Key Components:**

```pascal
type
  TTimeEntry = class(TSQLRecord)
  private
    FTaskID: Int64;
    FUserID: Int64;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FDuration: Integer; // In minutes
    FDescription: RawUTF8;
  published
    property TaskID: Int64 read FTaskID write FTaskID;
    property UserID: Int64 read FUserID write FUserID;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;
    property Duration: Integer read FDuration write FDuration;
    property Description: RawUTF8 read FDescription write FDescription;
  end;
```

**Dependencies:**
- taskmanager
- taskmanagerteam
- Time tracking models

**File Location:** `src/modules/taskmanagertimetracking.pas`

---

#### 3.4.10 Search Module (`taskmanagersearch.pas`)

**Purpose:** Advanced search and indexing capabilities.

**Key Features:**

- Full-text search across tasks and comments
- Search filters and facets
- Search result ranking
- Saved searches

**Key Components:**

```pascal
type
  TSearchQuery = record
    SearchText: RawUTF8;
    SearchFields: TSearchFields; // Title, Description, Comments
    Filters: TTaskFilter;
    MaxResults: Integer;
  end;
  
  TSearchFields = set of (sfTitle, sfDescription, sfComments, sfTags);
```

**Dependencies:**
- taskmanager
- Full-text indexing (mORMot FTS)

**File Location:** `src/modules/taskmanagersearch.pas`

---

#### 3.4.11 Notifications Module (`taskmanagernotifications.pas`)

**Purpose:** Notification system for task events.

**Key Features:**

- Event-based notifications
- Notification preferences
- Multiple delivery channels (extensible)
- Notification history

**Key Components:**

```pascal
type
  TNotification = class(TSQLRecord)
  private
    FRecipientID: Int64;
    FType: TNotificationType;
    FTitle: RawUTF8;
    FMessage: RawUTF8;
    FRelatedTaskID: Int64;
    FCreatedDate: TDateTime;
    FIsRead: Boolean;
  published
    property RecipientID: Int64 read FRecipientID write FRecipientID;
    property Type_: TNotificationType read FType write FType;
    property Title: RawUTF8 read FTitle write FTitle;
    property Message: RawUTF8 read FMessage write FMessage;
    property RelatedTaskID: Int64 read FRelatedTaskID write FRelatedTaskID;
    property CreatedDate: TDateTime read FCreatedDate write FCreatedDate;
    property IsRead: Boolean read FIsRead write FIsRead;
  end;
  
  TNotificationType = (
    ntTaskAssigned, ntTaskCompleted, ntTaskOverdue,
    ntCommentAdded, ntMentioned, ntDeadlineApproaching
  );
```

**Dependencies:**
- taskmanager
- taskmanagerteam
- Notification models

**File Location:** `src/modules/taskmanagernotifications.pas`

---

#### 3.4.12 Focus Mode Module (`taskmanagerfocus.pas`)

**Purpose:** Productivity features like focus sessions and Pomodoro technique.

**Key Features:**

- Pomodoro timer integration
- Focus sessions
- Distraction blocking (data tracking)
- Productivity analytics

**Key Components:**

```pascal
type
  TFocusSession = class(TSQLRecord)
  private
    FTaskID: Int64;
    FUserID: Int64;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FPlannedDuration: Integer; // Minutes
    FActualDuration: Integer;
    FInterruptions: Integer;
  published
    property TaskID: Int64 read FTaskID write FTaskID;
    property UserID: Int64 read FUserID write FUserID;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;
    property PlannedDuration: Integer read FPlannedDuration write FPlannedDuration;
    property ActualDuration: Integer read FActualDuration write FActualDuration;
    property Interruptions: Integer read FInterruptions write FInterruptions;
  end;
```

**Dependencies:**
- taskmanager
- taskmanagerteam
- Focus session models

**File Location:** `src/modules/taskmanagerfocus.pas`

---

#### 3.4.13 Gamification Module (`taskmanagergamify.pas`)

**Purpose:** Gamification features to increase engagement.

**Key Features:**

- Points and achievements
- Leaderboards
- Badges and rewards
- Streak tracking

**Key Components:**

```pascal
type
  TUserPoints = class(TSQLRecord)
  private
    FUserID: Int64;
    FTotalPoints: Integer;
    FLevel: Integer;
    FCurrentStreak: Integer;
    FLongestStreak: Integer;
  published
    property UserID: Int64 read FUserID write FUserID;
    property TotalPoints: Integer read FTotalPoints write FTotalPoints;
    property Level: Integer read FLevel write FLevel;
    property CurrentStreak: Integer read FCurrentStreak write FCurrentStreak;
    property LongestStreak: Integer read FLongestStreak write FLongestStreak;
  end;
  
  TAchievement = class(TSQLRecord)
  private
    FName: RawUTF8;
    FDescription: RawUTF8;
    FBadgeIcon: RawUTF8;
    FPointsRequired: Integer;
  published
    property Name: RawUTF8 read FName write FName;
    property Description: RawUTF8 read FDescription write FDescription;
    property BadgeIcon: RawUTF8 read FBadgeIcon write FBadgeIcon;
    property PointsRequired: Integer read FPointsRequired write FPointsRequired;
  end;
```

**Dependencies:**
- taskmanager
- taskmanagerteam
- Gamification models

**File Location:** `src/modules/taskmanagergamify.pas`

---

#### 3.4.14 Knowledge Base Module (`taskmanagerknowledge.pas`)

**Purpose:** Integrate knowledge management with tasks.

**Key Features:**

- Link documentation to tasks
- Knowledge articles
- FAQ system
- Search knowledge base

**Key Components:**

```pascal
type
  TKnowledgeArticle = class(TSQLRecord)
  private
    FTitle: RawUTF8;
    FContent: RawUTF8;
    FCategory: RawUTF8;
    FTags: RawUTF8;
    FAuthorID: Int64;
    FCreatedDate: TDateTime;
    FUpdatedDate: TDateTime;
  published
    property Title: RawUTF8 read FTitle write FTitle;
    property Content: RawUTF8 read FContent write FContent;
    property Category: RawUTF8 read FCategory write FCategory;
    property Tags: RawUTF8 read FTags write FTags;
    property AuthorID: Int64 read FAuthorID write FAuthorID;
    property CreatedDate: TDateTime read FCreatedDate write FCreatedDate;
    property UpdatedDate: TDateTime read FUpdatedDate write FUpdatedDate;
  end;
```

**Dependencies:**
- taskmanager
- Knowledge base models

**File Location:** `src/modules/taskmanagerknowledge.pas`

---

#### 3.4.15 Lifestyle/Wellness Module (`taskmanagerlifestyle.pas`)

**Purpose:** Personal wellness and lifestyle task management.

**Key Features:**

- Habit tracking
- Health goals
- Wellness check-ins
- Mood tracking

**Key Components:**

```pascal
type
  THabit = class(TSQLRecord)
  private
    FUserID: Int64;
    FName: RawUTF8;
    FFrequency: TRecurrenceFrequency;
    FTargetDays: Integer;
    FCurrentStreak: Integer;
  published
    property UserID: Int64 read FUserID write FUserID;
    property Name: RawUTF8 read FName write FName;
    property Frequency: TRecurrenceFrequency read FFrequency write FFrequency;
    property TargetDays: Integer read FTargetDays write FTargetDays;
    property CurrentStreak: Integer read FCurrentStreak write FCurrentStreak;
  end;
```

**Dependencies:**
- taskmanager
- taskmanagerrecurring
- Wellness models

**File Location:** `src/modules/taskmanagerlifestyle.pas`

---

#### 3.4.16 Meetings Module (`taskmanagermeetings.pas`)

**Purpose:** Meeting management integrated with tasks.

**Key Features:**

- Schedule meetings
- Meeting agendas
- Action items from meetings
- Meeting minutes

**Key Components:**

```pascal
type
  TMeeting = class(TSQLRecord)
  private
    FTitle: RawUTF8;
    FDescription: RawUTF8;
    FScheduledTime: TDateTime;
    FDuration: Integer; // Minutes
    FOrganizerID: Int64;
    FLocation: RawUTF8;
  published
    property Title: RawUTF8 read FTitle write FTitle;
    property Description: RawUTF8 read FDescription write FDescription;
    property ScheduledTime: TDateTime read FScheduledTime write FScheduledTime;
    property Duration: Integer read FDuration write FDuration;
    property OrganizerID: Int64 read FOrganizerID write FOrganizerID;
    property Location: RawUTF8 read FLocation write FLocation;
  end;
```

**Dependencies:**
- taskmanager
- taskmanagerteam
- Meeting models

**File Location:** `src/modules/taskmanagermeetings.pas`

---

#### 3.4.17 Resource Allocation Module (`taskmanagerresource.pas`)

**Purpose:** Manage resource allocation and capacity planning.

**Key Features:**

- Resource definitions
- Task-resource assignments
- Capacity tracking
- Resource utilization reports

**Key Components:**

```pascal
type
  TResource = class(TSQLRecord)
  private
    FName: RawUTF8;
    FType: TResourceType;
    FCapacity: Double;
    FUnit: RawUTF8;
  published
    property Name: RawUTF8 read FName write FName;
    property Type_: TResourceType read FType write FType;
    property Capacity: Double read FCapacity write FCapacity;
    property Unit_: RawUTF8 read FUnit write FUnit;
  end;
  
  TResourceType = (rtHuman, rtEquipment, rtBudget, rtTime);
```

**Dependencies:**
- taskmanager
- Resource models

**File Location:** `src/modules/taskmanagerresource.pas`

---

#### 3.4.18 Wellbeing Module (`taskmanagerwellbeing.pas`)

**Purpose:** Work-life balance and mental health features.

**Key Features:**

- Break reminders
- Overwork detection
- Stress level tracking
- Work-life balance metrics

**Key Components:**

```pascal
type
  TWellbeingCheck = class(TSQLRecord)
  private
    FUserID: Int64;
    FCheckDate: TDateTime;
    FStressLevel: Integer; // 1-10
    FEnergyLevel: Integer; // 1-10
    FMoodRating: Integer; // 1-10
    FNotes: RawUTF8;
  published
    property UserID: Int64 read FUserID write FUserID;
    property CheckDate: TDateTime read FCheckDate write FCheckDate;
    property StressLevel: Integer read FStressLevel write FStressLevel;
    property EnergyLevel: Integer read FEnergyLevel write FEnergyLevel;
    property MoodRating: Integer read FMoodRating write FMoodRating;
    property Notes: RawUTF8 read FNotes write FNotes;
  end;
```

**Dependencies:**
- taskmanager
- taskmanagerteam
- Wellbeing models

**File Location:** `src/modules/taskmanagerwellbeing.pas`

---

#### 3.4.19 Smart Suggestions Module (`taskmanagersmart.pas`)

**Purpose:** AI/ML-powered suggestions and automation (rule-based initially).

**Key Features:**

- Smart task prioritization
- Due date suggestions
- Task estimation assistance
- Pattern recognition for recurring tasks
- Workload balancing recommendations

**Key Components:**

```pascal
type
  TSmartSuggestion = record
    SuggestionType: TSuggestionType;
    TaskID: Int64;
    Message: RawUTF8;
    Confidence: Double; // 0.0 to 1.0
    Data: RawJSON; // Structured suggestion data
  end;
  
  TSuggestionType = (
    stPriorityChange,
    stDueDateAdjustment,
    stTaskBreakdown,
    stResourceReallocation,
    stDelegation
  );
  
  TTaskManagerSmart = class(TTaskManagerAdvanced)
  public
    function GetSmartSuggestions(AUserID: Int64): TSmartSuggestionArray;
    function ApplySuggestion(const ASuggestion: TSmartSuggestion): Boolean;
    function AnalyzeWorkload(AUserID: Int64): TWorkloadAnalysis;
    function SuggestTaskEstimate(const ATask: TTaskModel): Integer; // Minutes
  end;
```

**Implementation Notes:**

- **Rule-Based Engine:** Initially uses heuristics and rules rather than ML
- **Pattern Matching:** Analyzes historical data for patterns
- **Configurable Rules:** Suggestion rules can be customized
- **Learning Capability:** Framework for future ML integration

**Dependencies:**
- taskmanageradvanced
- taskmanagertimetracking
- Smart suggestion models

**File Location:** `src/modules/taskmanagersmart.pas`

---

### 3.5 Supporting Infrastructure Modules

#### 3.5.1 Event System (`task_events.pas`)

**Purpose:** Centralized event publishing and subscription system.

**Key Components:**

```pascal
type
  IEventDispatcher = interface
    procedure Subscribe(AEventType: TEventType; AHandler: TEventHandler);
    procedure Unsubscribe(AEventType: TEventType; AHandler: TEventHandler);
    procedure Publish(AEventType: TEventType; const AData: IEventData);
  end;
  
  TEventType = (
    etTaskCreated, etTaskUpdated, etTaskDeleted, etTaskStatusChanged,
    etCommentAdded, etCommentUpdated, etCommentDeleted,
    etTagAdded, etTagRemoved
  );
  
  TEventHandler = procedure(const AData: IEventData) of object;
```

**File Location:** `src/infrastructure/task_events.pas`

---

#### 3.5.2 Validation Framework (`task_validation.pas`)

**Purpose:** Centralized validation logic and error handling.

**Key Components:**

```pascal
type
  TValidationResult = record
    IsValid: Boolean;
    Errors: TRawUTF8DynArray;
  end;
  
  IValidator<T> = interface
    function Validate(const AValue: T): TValidationResult;
  end;
```

**File Location:** `src/infrastructure/task_validation.pas`

---

This completes Section 3 with detailed descriptions of all 20+ modules in the system, including core services, feature modules, and infrastructure components.




## 4. Data Models and Structures

### 4.1 Overview

This section provides detailed specifications for all data models used in the Free Pascal Task Manager. Each model is documented with complete field definitions, data types, constraints, validation rules, and relationships to other entities.

### 4.2 Core Domain Models

#### 4.2.1 Task Model (`TTaskModel`)

**Purpose:** Represents a task/todo item in the system.

**Class Definition:**

```pascal
type
  TTaskStatus = (
    tsBacklog,      // Not yet started
    tsTodo,         // Ready to start
    tsInProgress,   // Currently being worked on
    tsInReview,     // Waiting for review
    tsBlocked,      // Blocked by dependencies
    tsCompleted,    // Successfully completed
    tsCancelled     // Cancelled/abandoned
  );

  TTaskPriority = (
    tpLow,
    tpMedium,
    tpHigh,
    tpCritical
  );

  TTaskModel = class(TSQLRecord)
  private
    FTitle: RawUTF8;
    FDescription: RawUTF8;
    FStatus: TTaskStatus;
    FPriority: TTaskPriority;
    FDueDate: TDateTime;
    FCreatedAt: TDateTime;
    FUpdatedAt: TDateTime;
    FCompletedAt: TDateTime;
    FEstimatedHours: Double;
    FActualHours: Double;
    FProgress: Integer;  // 0-100
    FParentTaskID: TID;
    FAssignedToUserID: TID;
    FCreatedByUserID: TID;
    FProjectID: TID;
    FBoardID: TID;
    FColumnID: TID;
    FRecurringTaskID: TID;
    FIsArchived: Boolean;
    FIsTemplate: Boolean;
    FPosition: Integer;  // For ordering within lists/boards
  published
    property Title: RawUTF8 read FTitle write FTitle;
    property Description: RawUTF8 read FDescription write FDescription;
    property Status: TTaskStatus read FStatus write FStatus;
    property Priority: TTaskPriority read FPriority write FPriority;
    property DueDate: TDateTime read FDueDate write FDueDate;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property UpdatedAt: TDateTime read FUpdatedAt write FUpdatedAt;
    property CompletedAt: TDateTime read FCompletedAt write FCompletedAt;
    property EstimatedHours: Double read FEstimatedHours write FEstimatedHours;
    property ActualHours: Double read FActualHours write FActualHours;
    property Progress: Integer read FProgress write FProgress;
    property ParentTaskID: TID read FParentTaskID write FParentTaskID;
    property AssignedToUserID: TID read FAssignedToUserID write FAssignedToUserID;
    property CreatedByUserID: TID read FCreatedByUserID write FCreatedByUserID;
    property ProjectID: TID read FProjectID write FProjectID;
    property BoardID: TID read FBoardID write FBoardID;
    property ColumnID: TID read FColumnID write FColumnID;
    property RecurringTaskID: TID read FRecurringTaskID write FRecurringTaskID;
    property IsArchived: Boolean read FIsArchived write FIsArchived;
    property IsTemplate: Boolean read FIsTemplate write FIsTemplate;
    property Position: Integer read FPosition write FPosition;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique task identifier |
| Title | RawUTF8 | No | - | MaxLength: 500, MinLength: 1 | Task title/summary |
| Description | RawUTF8 | Yes | '' | MaxLength: 10000 | Detailed task description |
| Status | TTaskStatus | No | tsBacklog | Enum | Current task status |
| Priority | TTaskPriority | No | tpMedium | Enum | Task priority level |
| DueDate | TDateTime | Yes | NULL | Must be >= CreatedAt | Target completion date |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |
| UpdatedAt | TDateTime | No | Now() | - | Timestamp of last update |
| CompletedAt | TDateTime | Yes | NULL | - | Timestamp of completion |
| EstimatedHours | Double | Yes | NULL | >= 0 | Estimated effort in hours |
| ActualHours | Double | Yes | NULL | >= 0 | Actual effort spent |
| Progress | Integer | No | 0 | 0-100 | Completion percentage |
| ParentTaskID | TID | Yes | NULL | Foreign Key | Parent task for subtasks |
| AssignedToUserID | TID | Yes | NULL | Foreign Key | Assigned user |
| CreatedByUserID | TID | No | - | Foreign Key | Creator user |
| ProjectID | TID | Yes | NULL | Foreign Key | Associated project |
| BoardID | TID | Yes | NULL | Foreign Key | Kanban board |
| ColumnID | TID | Yes | NULL | Foreign Key | Board column |
| RecurringTaskID | TID | Yes | NULL | Foreign Key | Source recurring task |
| IsArchived | Boolean | No | False | - | Archive status flag |
| IsTemplate | Boolean | No | False | - | Template flag |
| Position | Integer | No | 0 | >= 0 | Display order position |

**Business Rules:**

1. **Status Transitions:**
   - tsBacklog → tsTodo, tsCancelled
   - tsTodo → tsInProgress, tsCancelled
   - tsInProgress → tsInReview, tsBlocked, tsCancelled
   - tsInReview → tsCompleted, tsInProgress
   - tsBlocked → tsInProgress, tsCancelled
   - tsCompleted → (final state)
   - tsCancelled → (final state)

2. **Validation Rules:**
   - Title must not be empty or whitespace only
   - Progress must be 0-100
   - When Status = tsCompleted, CompletedAt must be set
   - When Status = tsCompleted, Progress should be 100
   - DueDate, if set, must be in the future when task is created
   - ParentTaskID must not create circular references
   - EstimatedHours and ActualHours must be non-negative

3. **Calculated Fields:**
   - IsOverdue: DueDate < Now() AND Status NOT IN (tsCompleted, tsCancelled)
   - TimeRemaining: DueDate - Now()
   - EfficiencyRatio: ActualHours / EstimatedHours (if both set)

**Relationships:**

- **One-to-Many with TCommentModel:** A task can have multiple comments
- **One-to-Many with TTaskTagModel:** A task can have multiple tags (junction)
- **One-to-Many with TTimeEntryModel:** A task can have multiple time entries
- **One-to-Many with TTaskModel (Self):** A task can have multiple subtasks
- **Many-to-One with TUserModel:** AssignedToUserID, CreatedByUserID
- **Many-to-One with TProjectModel:** ProjectID
- **Many-to-One with TBoardModel:** BoardID
- **Many-to-One with TBoardColumnModel:** ColumnID
- **Many-to-One with TRecurringTaskModel:** RecurringTaskID

**Database Indexes:**

```sql
CREATE INDEX idx_task_status ON Task(Status);
CREATE INDEX idx_task_priority ON Task(Priority);
CREATE INDEX idx_task_duedate ON Task(DueDate);
CREATE INDEX idx_task_assignedto ON Task(AssignedToUserID);
CREATE INDEX idx_task_createdby ON Task(CreatedByUserID);
CREATE INDEX idx_task_project ON Task(ProjectID);
CREATE INDEX idx_task_board ON Task(BoardID);
CREATE INDEX idx_task_parent ON Task(ParentTaskID);
CREATE INDEX idx_task_archived ON Task(IsArchived);
CREATE INDEX idx_task_createdat ON Task(CreatedAt);
CREATE INDEX idx_task_board_column ON Task(BoardID, ColumnID, Position);
```

---

#### 4.2.2 Comment Model (`TCommentModel`)

**Purpose:** Represents user comments on tasks.

**Class Definition:**

```pascal
type
  TCommentModel = class(TSQLRecord)
  private
    FTaskID: TID;
    FUserID: TID;
    FContent: RawUTF8;
    FCreatedAt: TDateTime;
    FUpdatedAt: TDateTime;
    FIsEdited: Boolean;
    FParentCommentID: TID;
  published
    property TaskID: TID read FTaskID write FTaskID;
    property UserID: TID read FUserID write FUserID;
    property Content: RawUTF8 read FContent write FContent;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property UpdatedAt: TDateTime read FUpdatedAt write FUpdatedAt;
    property IsEdited: Boolean read FIsEdited write FIsEdited;
    property ParentCommentID: TID read FParentCommentID write FParentCommentID;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique comment identifier |
| TaskID | TID | No | - | Foreign Key | Associated task |
| UserID | TID | No | - | Foreign Key | Comment author |
| Content | RawUTF8 | No | - | MaxLength: 5000, MinLength: 1 | Comment text |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |
| UpdatedAt | TDateTime | No | Now() | - | Timestamp of last update |
| IsEdited | Boolean | No | False | - | Edit status flag |
| ParentCommentID | TID | Yes | NULL | Foreign Key | Parent comment for threading |

**Business Rules:**

1. Content must not be empty or whitespace only
2. When updated, IsEdited must be set to True and UpdatedAt updated
3. ParentCommentID must reference a comment on the same task
4. ParentCommentID must not create circular references

**Relationships:**

- **Many-to-One with TTaskModel:** TaskID
- **Many-to-One with TUserModel:** UserID
- **One-to-Many with TCommentModel (Self):** For threaded comments

**Database Indexes:**

```sql
CREATE INDEX idx_comment_task ON Comment(TaskID, CreatedAt);
CREATE INDEX idx_comment_user ON Comment(UserID);
CREATE INDEX idx_comment_parent ON Comment(ParentCommentID);
```

---

#### 4.2.3 Tag Model (`TTagModel`)

**Purpose:** Represents labels/tags for categorizing tasks.

**Class Definition:**

```pascal
type
  TTagModel = class(TSQLRecord)
  private
    FName: RawUTF8;
    FColor: RawUTF8;
    FDescription: RawUTF8;
    FCreatedAt: TDateTime;
  published
    property Name: RawUTF8 read FName write FName;
    property Color: RawUTF8 read FColor write FColor;
    property Description: RawUTF8 read FDescription write FDescription;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique tag identifier |
| Name | RawUTF8 | No | - | MaxLength: 100, Unique | Tag name |
| Color | RawUTF8 | Yes | '#808080' | HEX color format | Display color |
| Description | RawUTF8 | Yes | '' | MaxLength: 500 | Tag description |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |

**Business Rules:**

1. Name must be unique (case-insensitive)
2. Name must not contain special characters (alphanumeric, spaces, hyphens only)
3. Color must be valid hex color format (#RRGGBB) if provided

**Relationships:**

- **Many-to-Many with TTaskModel:** Through TTaskTagModel junction table

**Database Indexes:**

```sql
CREATE UNIQUE INDEX idx_tag_name ON Tag(Name COLLATE NOCASE);
```

---

#### 4.2.4 Task-Tag Junction Model (`TTaskTagModel`)

**Purpose:** Links tasks to tags (many-to-many relationship).

**Class Definition:**

```pascal
type
  TTaskTagModel = class(TSQLRecord)
  private
    FTaskID: TID;
    FTagID: TID;
    FCreatedAt: TDateTime;
  published
    property TaskID: TID read FTaskID write FTaskID;
    property TagID: TID read FTagID write FTagID;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique identifier |
| TaskID | TID | No | - | Foreign Key | Associated task |
| TagID | TID | No | - | Foreign Key | Associated tag |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |

**Business Rules:**

1. (TaskID, TagID) combination must be unique
2. Both TaskID and TagID must reference existing records

**Database Indexes:**

```sql
CREATE UNIQUE INDEX idx_tasktag_unique ON TaskTag(TaskID, TagID);
CREATE INDEX idx_tasktag_tag ON TaskTag(TagID);
```

---

### 4.3 Extended Domain Models

#### 4.3.1 Project Model (`TProjectModel`)

**Purpose:** Represents a project containing multiple tasks.

**Class Definition:**

```pascal
type
  TProjectStatus = (
    psPlanning,
    psActive,
    psOnHold,
    psCompleted,
    psCancelled
  );

  TProjectModel = class(TSQLRecord)
  private
    FName: RawUTF8;
    FDescription: RawUTF8;
    FStatus: TProjectStatus;
    FStartDate: TDateTime;
    FEndDate: TDateTime;
    FCreatedAt: TDateTime;
    FUpdatedAt: TDateTime;
    FCreatedByUserID: TID;
    FIsArchived: Boolean;
  published
    property Name: RawUTF8 read FName write FName;
    property Description: RawUTF8 read FDescription write FDescription;
    property Status: TProjectStatus read FStatus write FStatus;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property EndDate: TDateTime read FEndDate write FEndDate;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property UpdatedAt: TDateTime read FUpdatedAt write FUpdatedAt;
    property CreatedByUserID: TID read FCreatedByUserID write FCreatedByUserID;
    property IsArchived: Boolean read FIsArchived write FIsArchived;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique project identifier |
| Name | RawUTF8 | No | - | MaxLength: 200, MinLength: 1 | Project name |
| Description | RawUTF8 | Yes | '' | MaxLength: 5000 | Project description |
| Status | TProjectStatus | No | psPlanning | Enum | Project status |
| StartDate | TDateTime | Yes | NULL | - | Project start date |
| EndDate | TDateTime | Yes | NULL | Must be > StartDate | Project end date |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |
| UpdatedAt | TDateTime | No | Now() | - | Timestamp of last update |
| CreatedByUserID | TID | No | - | Foreign Key | Creator user |
| IsArchived | Boolean | No | False | - | Archive status flag |

**Relationships:**

- **One-to-Many with TTaskModel:** A project can have multiple tasks
- **Many-to-One with TUserModel:** CreatedByUserID

**Database Indexes:**

```sql
CREATE INDEX idx_project_status ON Project(Status);
CREATE INDEX idx_project_archived ON Project(IsArchived);
CREATE INDEX idx_project_dates ON Project(StartDate, EndDate);
```

---

#### 4.3.2 Board Model (`TBoardModel`)

**Purpose:** Represents a Kanban board for visual task management.

**Class Definition:**

```pascal
type
  TBoardModel = class(TSQLRecord)
  private
    FName: RawUTF8;
    FDescription: RawUTF8;
    FProjectID: TID;
    FCreatedAt: TDateTime;
    FUpdatedAt: TDateTime;
    FCreatedByUserID: TID;
    FIsArchived: Boolean;
  published
    property Name: RawUTF8 read FName write FName;
    property Description: RawUTF8 read FDescription write FDescription;
    property ProjectID: TID read FProjectID write FProjectID;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property UpdatedAt: TDateTime read FUpdatedAt write FUpdatedAt;
    property CreatedByUserID: TID read FCreatedByUserID write FCreatedByUserID;
    property IsArchived: Boolean read FIsArchived write FIsArchived;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique board identifier |
| Name | RawUTF8 | No | - | MaxLength: 200 | Board name |
| Description | RawUTF8 | Yes | '' | MaxLength: 1000 | Board description |
| ProjectID | TID | Yes | NULL | Foreign Key | Associated project |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |
| UpdatedAt | TDateTime | No | Now() | - | Timestamp of last update |
| CreatedByUserID | TID | No | - | Foreign Key | Creator user |
| IsArchived | Boolean | No | False | - | Archive status flag |

**Relationships:**

- **One-to-Many with TBoardColumnModel:** A board has multiple columns
- **One-to-Many with TTaskModel:** A board can have multiple tasks
- **Many-to-One with TProjectModel:** ProjectID
- **Many-to-One with TUserModel:** CreatedByUserID

**Database Indexes:**

```sql
CREATE INDEX idx_board_project ON Board(ProjectID);
CREATE INDEX idx_board_archived ON Board(IsArchived);
```

---

#### 4.3.3 Board Column Model (`TBoardColumnModel`)

**Purpose:** Represents columns in a Kanban board.

**Class Definition:**

```pascal
type
  TBoardColumnModel = class(TSQLRecord)
  private
    FBoardID: TID;
    FName: RawUTF8;
    FPosition: Integer;
    FWIPLimit: Integer;
    FCreatedAt: TDateTime;
  published
    property BoardID: TID read FBoardID write FBoardID;
    property Name: RawUTF8 read FName write FName;
    property Position: Integer read FPosition write FPosition;
    property WIPLimit: Integer read FWIPLimit write FWIPLimit;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique column identifier |
| BoardID | TID | No | - | Foreign Key | Parent board |
| Name | RawUTF8 | No | - | MaxLength: 100 | Column name |
| Position | Integer | No | 0 | >= 0 | Display order |
| WIPLimit | Integer | Yes | NULL | > 0 | Work-in-progress limit |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |

**Business Rules:**

1. Position must be unique within a board
2. Default columns: "Backlog" (0), "To Do" (1), "In Progress" (2), "Review" (3), "Done" (4)

**Relationships:**

- **Many-to-One with TBoardModel:** BoardID
- **One-to-Many with TTaskModel:** A column can have multiple tasks

**Database Indexes:**

```sql
CREATE INDEX idx_boardcolumn_board ON BoardColumn(BoardID, Position);
```

---

#### 4.3.4 User Model (`TUserModel`)

**Purpose:** Represents system users.

**Class Definition:**

```pascal
type
  TUserRole = (
    urViewer,
    urMember,
    urAdmin
  );

  TUserModel = class(TSQLRecord)
  private
    FUsername: RawUTF8;
    FEmail: RawUTF8;
    FFullName: RawUTF8;
    FRole: TUserRole;
    FCreatedAt: TDateTime;
    FLastLoginAt: TDateTime;
    FIsActive: Boolean;
  published
    property Username: RawUTF8 read FUsername write FUsername;
    property Email: RawUTF8 read FEmail write FEmail;
    property FullName: RawUTF8 read FFullName write FFullName;
    property Role: TUserRole read FRole write FRole;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property LastLoginAt: TDateTime read FLastLoginAt write FLastLoginAt;
    property IsActive: Boolean read FIsActive write FIsActive;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique user identifier |
| Username | RawUTF8 | No | - | MaxLength: 50, Unique | Username |
| Email | RawUTF8 | No | - | MaxLength: 255, Unique, Valid email | Email address |
| FullName | RawUTF8 | Yes | '' | MaxLength: 200 | Full name |
| Role | TUserRole | No | urMember | Enum | User role |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |
| LastLoginAt | TDateTime | Yes | NULL | - | Last login timestamp |
| IsActive | Boolean | No | True | - | Active status flag |

**Database Indexes:**

```sql
CREATE UNIQUE INDEX idx_user_username ON User(Username COLLATE NOCASE);
CREATE UNIQUE INDEX idx_user_email ON User(Email COLLATE NOCASE);
CREATE INDEX idx_user_active ON User(IsActive);
```

---

#### 4.3.5 Time Entry Model (`TTimeEntryModel`)

**Purpose:** Tracks time spent on tasks.

**Class Definition:**

```pascal
type
  TTimeEntryModel = class(TSQLRecord)
  private
    FTaskID: TID;
    FUserID: TID;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FDuration: Double;  // Hours
    FDescription: RawUTF8;
    FCreatedAt: TDateTime;
  published
    property TaskID: TID read FTaskID write FTaskID;
    property UserID: TID read FUserID write FUserID;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;
    property Duration: Double read FDuration write FDuration;
    property Description: RawUTF8 read FDescription write FDescription;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique entry identifier |
| TaskID | TID | No | - | Foreign Key | Associated task |
| UserID | TID | No | - | Foreign Key | User who tracked time |
| StartTime | TDateTime | No | - | - | Start timestamp |
| EndTime | TDateTime | Yes | NULL | Must be > StartTime | End timestamp |
| Duration | Double | No | - | > 0 | Duration in hours |
| Description | RawUTF8 | Yes | '' | MaxLength: 500 | Entry description |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |

**Business Rules:**

1. If EndTime is set, Duration = (EndTime - StartTime) in hours
2. If EndTime is NULL, this represents an active timer
3. Only one active timer per user allowed

**Relationships:**

- **Many-to-One with TTaskModel:** TaskID
- **Many-to-One with TUserModel:** UserID

**Database Indexes:**

```sql
CREATE INDEX idx_timeentry_task ON TimeEntry(TaskID);
CREATE INDEX idx_timeentry_user ON TimeEntry(UserID, StartTime);
CREATE INDEX idx_timeentry_active ON TimeEntry(EndTime) WHERE EndTime IS NULL;
```

---

#### 4.3.6 Recurring Task Model (`TRecurringTaskModel`)

**Purpose:** Defines recurring task patterns.

**Class Definition:**

```pascal
type
  TRecurrenceType = (
    rtDaily,
    rtWeekly,
    rtMonthly,
    rtYearly,
    rtCustom
  );

  TRecurringTaskModel = class(TSQLRecord)
  private
    FTemplateTaskID: TID;
    FRecurrenceType: TRecurrenceType;
    FInterval: Integer;
    FDayOfWeek: Integer;  // 0-6 (Sunday-Saturday)
    FDayOfMonth: Integer; // 1-31
    FStartDate: TDateTime;
    FEndDate: TDateTime;
    FNextOccurrence: TDateTime;
    FIsActive: Boolean;
    FCreatedAt: TDateTime;
  published
    property TemplateTaskID: TID read FTemplateTaskID write FTemplateTaskID;
    property RecurrenceType: TRecurrenceType read FRecurrenceType write FRecurrenceType;
    property Interval: Integer read FInterval write FInterval;
    property DayOfWeek: Integer read FDayOfWeek write FDayOfWeek;
    property DayOfMonth: Integer read FDayOfMonth write FDayOfMonth;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property EndDate: TDateTime read FEndDate write FEndDate;
    property NextOccurrence: TDateTime read FNextOccurrence write FNextOccurrence;
    property IsActive: Boolean read FIsActive write FIsActive;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique identifier |
| TemplateTaskID | TID | No | - | Foreign Key | Template task to copy |
| RecurrenceType | TRecurrenceType | No | - | Enum | Recurrence pattern type |
| Interval | Integer | No | 1 | > 0 | Interval multiplier |
| DayOfWeek | Integer | Yes | NULL | 0-6 | Day of week for weekly |
| DayOfMonth | Integer | Yes | NULL | 1-31 | Day of month for monthly |
| StartDate | TDateTime | No | - | - | Recurrence start date |
| EndDate | TDateTime | Yes | NULL | Must be > StartDate | Recurrence end date |
| NextOccurrence | TDateTime | No | - | - | Next scheduled occurrence |
| IsActive | Boolean | No | True | - | Active status flag |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |

**Business Rules:**

1. TemplateTaskID must reference a task with IsTemplate = True
2. For weekly recurrence, DayOfWeek must be set
3. For monthly recurrence, DayOfMonth must be set
4. NextOccurrence is calculated based on RecurrenceType and Interval

**Relationships:**

- **Many-to-One with TTaskModel:** TemplateTaskID
- **One-to-Many with TTaskModel:** Generated tasks reference this via RecurringTaskID

**Database Indexes:**

```sql
CREATE INDEX idx_recurring_next ON RecurringTask(NextOccurrence, IsActive);
CREATE INDEX idx_recurring_template ON RecurringTask(TemplateTaskID);
```

---

#### 4.3.7 Notification Model (`TNotificationModel`)

**Purpose:** Stores user notifications.

**Class Definition:**

```pascal
type
  TNotificationType = (
    ntTaskAssigned,
    ntTaskDueSoon,
    ntTaskOverdue,
    ntTaskCompleted,
    ntCommentAdded,
    ntMentioned,
    ntCustom
  );

  TNotificationModel = class(TSQLRecord)
  private
    FUserID: TID;
    FType: TNotificationType;
    FTitle: RawUTF8;
    FMessage: RawUTF8;
    FTaskID: TID;
    FIsRead: Boolean;
    FCreatedAt: TDateTime;
    FReadAt: TDateTime;
  published
    property UserID: TID read FUserID write FUserID;
    property NotificationType: TNotificationType read FType write FType;
    property Title: RawUTF8 read FTitle write FTitle;
    property Message: RawUTF8 read FMessage write FMessage;
    property TaskID: TID read FTaskID write FTaskID;
    property IsRead: Boolean read FIsRead write FIsRead;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property ReadAt: TDateTime read FReadAt write FReadAt;
  end;
```

**Field Specifications:**

| Field | Type | Nullable | Default | Constraints | Description |
|-------|------|----------|---------|-------------|-------------|
| ID | TID (Int64) | No | Auto | Primary Key | Unique notification ID |
| UserID | TID | No | - | Foreign Key | Target user |
| NotificationType | TNotificationType | No | - | Enum | Notification type |
| Title | RawUTF8 | No | - | MaxLength: 200 | Notification title |
| Message | RawUTF8 | Yes | '' | MaxLength: 1000 | Notification message |
| TaskID | TID | Yes | NULL | Foreign Key | Related task (if any) |
| IsRead | Boolean | No | False | - | Read status flag |
| CreatedAt | TDateTime | No | Now() | - | Timestamp of creation |
| ReadAt | TDateTime | Yes | NULL | - | Timestamp when read |

**Relationships:**

- **Many-to-One with TUserModel:** UserID
- **Many-to-One with TTaskModel:** TaskID

**Database Indexes:**

```sql
CREATE INDEX idx_notification_user ON Notification(UserID, IsRead, CreatedAt);
CREATE INDEX idx_notification_task ON Notification(TaskID);
```

---

### 4.4 Entity Relationship Diagram (ERD)

```
┌─────────────────┐
│   TUserModel    │
│─────────────────│
│ + ID            │
│ + Username      │
│ + Email         │◄────────────┐
│ + FullName      │             │
│ + Role          │             │
│ + IsActive      │             │
└────────┬────────┘             │
         │                      │
         │ CreatedBy            │ AssignedTo
         │                      │
         ▼                      │
┌─────────────────────────┐    │
│     TTaskModel          │◄───┘
│─────────────────────────│
│ + ID                    │
│ + Title                 │
│ + Description           │◄─────────┐
│ + Status                │          │
│ + Priority              │          │
│ + DueDate               │          │
│ + ParentTaskID          │──┐       │
│ + AssignedToUserID      │  │       │
│ + ProjectID             │  │       │
│ + BoardID               │  │       │
│ + RecurringTaskID       │  │       │
└────┬──────┬─────┬────┬──┘  │       │
     │      │     │    │     │       │
     │      │     │    │     │       │ Many-to-Many
     │      │     │    │     │       │ via TaskTag
     │      │     │    │     │       │
     │      │     │    │     │  ┌────┴──────────┐
     │      │     │    │     │  │   TTagModel   │
     │      │     │    │     │  │───────────────│
     │      │     │    │     │  │ + ID          │
     │      │     │    │     │  │ + Name        │
     │      │     │    │     │  │ + Color       │
     │      │     │    │     │  └───────────────┘
     │      │     │    │     │
     │      │     │    │     └──Self-reference (Subtasks)
     │      │     │    │
     │      │     │    └────┐
     │      │     │         ▼
     │      │     │    ┌────────────────────┐
     │      │     │    │   TBoardModel      │
     │      │     │    │────────────────────│
     │      │     │    │ + ID               │
     │      │     │    │ + Name             │
     │      │     │    │ + ProjectID        │
     │      │     │    └─────────┬──────────┘
     │      │     │              │
     │      │     │              │ Has Columns
     │      │     │              ▼
     │      │     │    ┌────────────────────┐
     │      │     │    │ TBoardColumnModel  │
     │      │     │    │────────────────────│
     │      │     │    │ + ID               │
     │      │     │    │ + BoardID          │
     │      │     │    │ + Name             │
     │      │     │    │ + Position         │
     │      │     │    └────────────────────┘
     │      │     │
     │      │     └────┐
     │      │          ▼
     │      │     ┌─────────────────┐
     │      │     │  TProjectModel  │
     │      │     │─────────────────│
     │      │     │ + ID            │
     │      │     │ + Name          │
     │      │     │ + Status        │
     │      │     │ + StartDate     │
     │      │     │ + EndDate       │
     │      │     └─────────────────┘
     │      │
     │      └──────┐
     │             ▼
     │        ┌─────────────────────┐
     │        │  TCommentModel      │
     │        │─────────────────────│
     │        │ + ID                │
     │        │ + TaskID            │
     │        │ + UserID            │
     │        │ + Content           │
     │        │ + ParentCommentID   │──┐
     │        └─────────────────────┘  │
     │                                 │
     │                                 └─ Self-reference (Threading)
     │
     └──────┐
            ▼
       ┌──────────────────────┐
       │  TTimeEntryModel     │
       │──────────────────────│
       │ + ID                 │
       │ + TaskID             │
       │ + UserID             │
       │ + StartTime          │
       │ + EndTime            │
       │ + Duration           │
       └──────────────────────┘
```

---

### 4.5 Data Model Summary

**Total Models:** 12 core models

**Model Categories:**

1. **Core Domain:** Task, Comment, Tag, TaskTag
2. **Organization:** Project, Board, BoardColumn
3. **User Management:** User
4. **Time Tracking:** TimeEntry
5. **Automation:** RecurringTask
6. **Notifications:** Notification

**Key Relationships:**

- **One-to-Many:** User→Task, Task→Comment, Task→TimeEntry, Project→Task, Board→Task, Board→BoardColumn
- **Many-to-Many:** Task↔Tag (via TaskTag)
- **Self-Referencing:** Task (subtasks), Comment (threading)

**Database Statistics (Estimated):**

- **Total Tables:** 12
- **Total Indexes:** ~30
- **Foreign Keys:** ~20
- **Unique Constraints:** ~6

---

This completes Section 4 with comprehensive data model specifications including all field definitions, constraints, relationships, business rules, and database schema details.
