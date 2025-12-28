
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
