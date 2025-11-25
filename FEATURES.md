# Task Manager Features Reference

Complete listing of all features in the Pascal Task Manager with method signatures and usage notes.

## Core Task Operations

### Task Creation
```pascal
function addTask(const name, description, category: string;
                 priority: TTaskPriority; dueDate: TDateTime): integer;
```
Creates a new task and returns its ID. Returns -1 on failure with error message in getLastError().

### Task Retrieval
```pascal
function getTask(id: integer; var task: TTask): boolean;
function getAllTasks: TTaskArray;
function getTaskCount: integer;
```
Retrieve individual tasks by ID or get all tasks. getTaskCount returns the number of tasks in the system.

### Task Modification
```pascal
function updateTask(var task: TTask): boolean;
function deleteTask(id: integer): boolean;
```
Update task details or remove a task from the system.

## Status & Priority Management

### Status Operations
```pascal
function setTaskStatus(id: integer; newStatus: TTaskStatus): boolean;
function completeTask(id: integer): boolean;
function getTasksByStatus(status: TTaskStatus): TTaskArray;
```

**Task Status Values:**
- tsNew: New task, not started
- tsInProgress: Work has begun
- tsBlocked: Cannot proceed due to blockers
- tsCompleted: Task is finished
- tsCancelled: Task was cancelled

### Priority Operations
```pascal
function setTaskPriority(id: integer; newPriority: TTaskPriority): boolean;
function getTasksByPriority(priority: TTaskPriority): TTaskArray;
```

**Priority Levels:**
- tpLow: Can be done at any time
- tpMedium: Should be done soon
- tpHigh: Important task
- tpCritical: Urgent, do immediately

## Filtering & Organization

### Category Filtering
```pascal
function getTasksByCategory(const category: string): TTaskArray;
```
Get all tasks in a specific category.

### Overdue Tasks
```pascal
function getOverdueTaskCount: integer;
```
Count incomplete tasks past their due date.

## Sorting Features

### Sort by Due Date
```pascal
function getTasksSortedByDueDate: TTaskArray;
```
Returns tasks sorted by due date (earliest first).

### Sort by Priority
```pascal
function getTasksSortedByPriority: TTaskArray;
```
Returns tasks sorted by priority (highest first).

### Sort by Status
```pascal
function getTasksSortedByStatus: TTaskArray;
```
Returns tasks grouped and sorted by status.

## Search Functionality

### Name Search
```pascal
function searchTasksByName(const searchTerm: string): TTaskArray;
```
Case-insensitive search in task names.

### Description Search
```pascal
function searchTasksByDescription(const searchTerm: string): TTaskArray;
```
Case-insensitive search in task descriptions.

### General Search
```pascal
function searchTasks(const searchTerm: string): TTaskArray;
```
Search in both name and description fields.

## Tag Management

### Add/Remove Tags
```pascal
function addTaskTag(id: integer; const tag: string): boolean;
function removeTaskTag(id: integer; const tag: string): boolean;
```

### Query Tags
```pascal
function hasTaskTag(id: integer; const tag: string): boolean;
function getTaskTags(id: integer): TTagArray;
function getAllTags: TTagArray;
```

### Filter by Tag
```pascal
function getTasksByTag(const tag: string): TTaskArray;
```
Get all tasks with a specific tag.

**Constraints:**
- Maximum 10 tags per task
- Tags are case-sensitive strings

## Time Tracking

### Set Estimates
```pascal
function setTaskEstimatedHours(id: integer; hours: double): boolean;
```
Set the estimated number of hours for a task.

### Track Actual Time
```pascal
function addTaskActualHours(id: integer; hours: double): boolean;
```
Add actual hours worked (accumulative).

### Query Time Information
```pascal
function getTaskEstimatedHours(id: integer): double;
function getTaskActualHours(id: integer): double;
function getTaskTimeOverrun(id: integer): double;
```

### Time Analysis
```pascal
function getTasksOverTime: TTaskArray;
```
Get all tasks where actual hours exceed estimated hours.

## Task Dependencies

### Manage Dependencies
```pascal
function addTaskDependency(taskId, dependencyTaskId: integer): boolean;
function removeTaskDependency(taskId, dependencyTaskId: integer): boolean;
```

### Query Dependencies
```pascal
function getTaskDependencies(taskId: integer): TIntArray;
function getTasksDependingOn(taskId: integer): TTaskArray;
function canCompleteTask(taskId: integer): boolean;
```

**Usage Notes:**
- A task cannot depend on itself
- Dependencies are validated before completion
- canCompleteTask checks if all dependencies are completed

## Task Notes/Comments (Extended Features)

### Add/Remove Notes
```pascal
function addTaskNote(taskId: integer; const noteText: string; 
                     isImportant: boolean): boolean;
function removeTaskNote(taskId: integer; noteIndex: integer): boolean;
```

### Query Notes
```pascal
function getTaskNotes(taskId: integer): TNoteArray;
function getTaskNoteCount(taskId: integer): integer;
function getImportantNotes: TTaskArray;
```

**Constraints:**
- Maximum 20 notes per task
- Notes include creation timestamp
- Can flag notes as important

## Task Assignment (Extended Features)

### Assign Tasks
```pascal
function assignTaskTo(taskId: integer; const assignee: string): boolean;
```
Assign a task to a team member (by name or ID string).

### Query Assignments
```pascal
function getTaskAssignment(taskId: integer): string;
function getTasksAssignedTo(const assignee: string): TTaskArray;
```

## Milestones (Extended Features)

### Manage Milestones
```pascal
function addMilestone(const name, description: string; 
                      targetDate: TDateTime): integer;
function deleteMilestone(milestoneId: integer): boolean;
function setMilestoneStatus(milestoneId: integer; 
                           newStatus: TTaskStatus): boolean;
```

### Assign Tasks to Milestones
```pascal
function assignTaskToMilestone(taskId: integer; 
                              milestoneId: integer): boolean;
```

### Query Milestones
```pascal
function getTasksByMilestone(milestoneId: integer): TTaskArray;
function getMilestoneProgress(milestoneId: integer): TTaskStats;
function getAllMilestones: TMilestoneArray;
```

**Features:**
- Track progress with built-in statistics
- Set target dates for milestones
- Monitor completion across related tasks

## Task Templates (Extended Features)

### Create/Delete Templates
```pascal
function createTemplate(const name, description, category: string;
                       priority: TTaskPriority; 
                       estimatedHours: double): integer;
function deleteTemplate(templateId: integer): boolean;
```

### Query Templates
```pascal
function getTemplate(templateId: integer; 
                    var template: TTaskTemplate): boolean;
function getAllTemplates: TTemplateArray;
```

### Create Tasks from Templates
```pascal
function createTaskFromTemplate(templateId: integer; 
                               const taskName: string;
                               dueDate: TDateTime): integer;
```

### Manage Template Tags
```pascal
function addTagToTemplate(templateId: integer; 
                         const tag: string): boolean;
```

**Usage:**
- Templates pre-populate task fields
- Tags defined in templates are automatically added to new tasks
- Estimated hours from template are applied to new task
- Useful for recurring or similar tasks

## Statistics & Reporting

### Get Statistics
```pascal
function getStatistics: TTaskStats;
```

**TTaskStats Record:**
- totalTasks: integer
- newTasks: integer
- inProgressTasks: integer
- blockedTasks: integer
- completedTasks: integer
- cancelledTasks: integer
- highPriorityCount: integer
- criticalPriorityCount: integer
- tasksWithNotes: integer
- averageCompletionTime: double

### Overdue Analysis
```pascal
function getOverdueTaskCount: integer;
```

## Error Handling

### Error Management
```pascal
function getLastError: string;
procedure clearError;
```

All operations that can fail return a boolean and set an error message. Check getLastError() for details when operations return false.

## Type Definitions

### Task Status
```pascal
TTaskStatus = (tsNew, tsInProgress, tsBlocked, tsCompleted, tsCancelled);
```

### Task Priority
```pascal
TTaskPriority = (tpLow, tpMedium, tpHigh, tpCritical);
```

### Task Record
```pascal
TTask = record
  id: integer;
  name: string[255];
  description: string[255];
  status: TTaskStatus;
  priority: TTaskPriority;
  dueDate: TDateTime;
  createdDate: TDateTime;
  completedDate: TDateTime;
  category: string[100];
  tags: TTagArray;
  estimatedHours: double;
  actualHours: double;
  dependencyIds: array of integer;
  notes: TNoteArray;
  assignedTo: string[100];
  milestoneId: integer;
end;
```

### Task Note Record
```pascal
TTaskNote = record
  text: string[255];
  createdDate: TDateTime;
  isImportant: boolean;
end;
```

### Milestone Record
```pascal
TMilestone = record
  id: integer;
  name: string[255];
  description: string[255];
  targetDate: TDateTime;
  status: TTaskStatus;
  createdDate: TDateTime;
end;
```

### Template Record
```pascal
TTaskTemplate = record
  id: integer;
  name: string[255];
  description: string[255];
  category: string[100];
  priority: TTaskPriority;
  estimatedHours: double;
  tags: TTagArray;
end;
```

## Constants

- maxStringLength = 255
- maxTagsPerTask = 10
- maxNotesPerTask = 20
- maxMilestonesPerManager = 50
- defaultTaskFile = 'tasks.dat'

## Object-Oriented Design

### Class Hierarchy

```
TTaskManager (base class)
  ├─ addTask, deleteTask, updateTask
  ├─ getTask, getAllTasks, getTaskCount
  ├─ setTaskStatus, setTaskPriority, completeTask
  ├─ getTasksBy* (status, priority, category)
  ├─ getTasksSortedBy* (dueDate, priority, status)
  ├─ searchTasks* (name, description, general)
  ├─ Tag management (add, remove, get, filter)
  ├─ Time tracking (estimate, actual, overrun)
  ├─ Dependencies (add, remove, query, validate)
  └─ getStatistics, getOverdueTaskCount

    TTaskManagerExtended (derived class)
      ├─ Task notes management (add, remove, get, query)
      ├─ Task assignment (assign, query)
      ├─ Milestone management (create, delete, query, assign)
      └─ Template management (create, delete, query, create from template)
```

## Usage Pattern

```pascal
var
  manager: TTaskManagerExtended;
  taskId: integer;

begin
  manager := TTaskManagerExtended.Create;
  try
    // Create a task
    taskId := manager.addTask('My Task', 'Description', 'Category',
                             tpHigh, now + 7);
    
    // Add a note
    manager.addTaskNote(taskId, 'Important note', true);
    
    // Assign task
    manager.assignTaskTo(taskId, 'John Doe');
    
    // Create milestone
    milestoneId := manager.addMilestone('Release 1.0', 'First release', now + 30);
    
    // Assign task to milestone
    manager.assignTaskToMilestone(taskId, milestoneId);
    
    // Get statistics
    stats := manager.getStatistics;
    WriteLn('Total tasks: ', stats.totalTasks);
    
  finally
    manager.Free;
  end;
end;
```
