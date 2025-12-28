
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
