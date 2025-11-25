
# Advanced Task Manager Features

## Overview

This document describes the advanced features added to the Task Manager system, extending the already comprehensive base functionality with enterprise-grade task management capabilities.

## Table of Contents

1. [Task Recurrence](#task-recurrence)
2. [Story Points & Agile Estimation](#story-points--agile-estimation)
3. [Task Watchers](#task-watchers)
4. [Task References & Linking](#task-references--linking)
5. [Task Blocking](#task-blocking)
6. [Advanced Statistics](#advanced-statistics)

---

## Task Recurrence

### Purpose
Enable creation of recurring tasks that automatically generate next instances based on a defined pattern.

### Supported Patterns
- **rpNone** - No recurrence (default)
- **rpDaily** - Every day
- **rpWeekly** - Every 7 days
- **rpBiWeekly** - Every 14 days
- **rpMonthly** - Every 30 days
- **rpQuarterly** - Every 90 days
- **rpYearly** - Every 365 days

### Methods

#### setTaskRecurrence
```pascal
function setTaskRecurrence(taskId: integer; pattern: TRecurrencePattern;
                           endDate: TDateTime): boolean;
```
Set a recurrence pattern for a task with an end date.

**Example:**
```pascal
manager.setTaskRecurrence(taskId, rpWeekly, Now + 90);
// Task will recur weekly for 90 days
```

#### getTaskRecurrence
```pascal
function getTaskRecurrence(taskId: integer; var pattern: TRecurrencePattern;
                            var endDate: TDateTime): boolean;
```
Retrieve the recurrence pattern and end date of a task.

#### getRecurringTasks
```pascal
function getRecurringTasks: TTaskArray;
```
Get all tasks that have a recurrence pattern set.

#### generateNextRecurrence
```pascal
function generateNextRecurrence(taskId: integer): integer;
```
Generate the next instance of a recurring task based on its pattern. Returns the ID of the newly created task, or -1 if the recurrence end date has been reached.

### Use Cases
- Daily standup meetings
- Weekly code reviews
- Monthly reporting tasks
- Quarterly business reviews
- Recurring maintenance tasks

---

## Story Points & Agile Estimation

### Purpose
Support Agile/Scrum methodology by allowing tasks to be estimated using story points and tracking team velocity.

### Methods

#### setTaskStoryPoints
```pascal
function setTaskStoryPoints(taskId: integer; points: integer): boolean;
```
Assign story points to a task for Agile estimation.

**Example:**
```pascal
manager.setTaskStoryPoints(taskId, 8);  // 8 story points
```

#### getTaskStoryPoints
```pascal
function getTaskStoryPoints(taskId: integer): integer;
```
Retrieve the story points assigned to a task.

#### getTotalStoryPoints
```pascal
function getTotalStoryPoints: integer;
```
Get the total story points for all tasks (both completed and pending).

#### getCompletedStoryPoints
```pascal
function getCompletedStoryPoints: integer;
```
Get the total story points of completed tasks only.

#### getVelocity
```pascal
function getVelocity(const sprintDays: integer): double;
```
Calculate team velocity (completed story points per day).

**Example:**
```pascal
velocity := manager.getVelocity(14);  // Velocity per day in a 14-day sprint
```

### Use Cases
- Sprint planning
- Capacity planning
- Velocity tracking
- Burndown charts
- Release planning

---

## Task Watchers

### Purpose
Allow team members to monitor task progress without being assigned to the task.

### Methods

#### addTaskWatcher
```pascal
function addTaskWatcher(taskId: integer; const watcher: string): boolean;
```
Add a team member as a watcher for a task.

**Example:**
```pascal
manager.addTaskWatcher(taskId, 'alice@company.com');
```

#### removeTaskWatcher
```pascal
function removeTaskWatcher(taskId: integer; const watcher: string): boolean;
```
Remove a watcher from a task.

#### hasTaskWatcher
```pascal
function hasTaskWatcher(taskId: integer; const watcher: string): boolean;
```
Check if a specific person is watching a task.

#### getTaskWatchers
```pascal
function getTaskWatchers(taskId: integer): TWatcherArray;
```
Get all watchers for a specific task.

#### getTasksWatchedBy
```pascal
function getTasksWatchedBy(const watcher: string): TTaskArray;
```
Get all tasks being watched by a specific person.

### Limits
- Maximum 20 watchers per task (configurable via `maxWatchersPerTask`)

### Use Cases
- Cross-functional awareness
- Stakeholder notification
- Code review tracking
- Knowledge sharing
- Team collaboration

---

## Task References & Linking

### Purpose
Create relationships between tasks to show dependencies, duplicates, and related work.

### Relationship Types
- `"blocks"` - This task blocks another task
- `"blocked_by"` - This task is blocked by another task
- `"related_to"` - This task is related to another task
- `"duplicates"` - This task duplicates another task
- Custom types supported (up to 50 characters)

### Methods

#### addTaskReference
```pascal
function addTaskReference(taskId: integer; referenceTaskId: integer;
                          const relationshipType: string): boolean;
```
Add a reference link between two tasks.

**Example:**
```pascal
manager.addTaskReference(taskId1, taskId2, 'blocks');
// Task 1 blocks Task 2
```

#### removeTaskReference
```pascal
function removeTaskReference(taskId: integer; refIndex: integer): boolean;
```
Remove a specific reference from a task.

#### getTaskReferences
```pascal
function getTaskReferences(taskId: integer): TReferenceArray;
```
Get all references for a task.

#### getTasksReferencedBy
```pascal
function getTasksReferencedBy(taskId: integer): TTaskArray;
```
Get all tasks that are referenced by a specific task.

### Limits
- Maximum 10 references per task (configurable via `maxReferencesPerTask`)

### Use Cases
- Dependency management
- Duplicate tracking
- Change impact analysis
- Related work identification
- Workflow coordination

---

## Task Blocking

### Purpose
Track tasks that are blocked with reasons, enabling better visibility into impediments.

### Methods

#### blockTask
```pascal
function blockTask(taskId: integer; const reason: string): boolean;
```
Mark a task as blocked with a reason.

**Example:**
```pascal
manager.blockTask(taskId, 'Waiting for design approval');
```

#### unblockTask
```pascal
function unblockTask(taskId: integer): boolean;
```
Remove the blocked status from a task.

#### isTaskBlocked
```pascal
function isTaskBlocked(taskId: integer): boolean;
```
Check if a task is currently blocked.

#### getBlockedTasks
```pascal
function getBlockedTasks: TTaskArray;
```
Get all currently blocked tasks.

### Use Cases
- Impediment tracking
- Risk management
- Team communication
- Process bottleneck identification
- Progress monitoring

---

## Advanced Statistics

### Purpose
Provide advanced analytics and metrics for team and project management.

### Methods

#### getSprintBurndown
```pascal
function getSprintBurndown(const sprintName: string): TTaskArray;
```
Get all tasks in a specific sprint/category for burndown tracking.

#### getTeamWorkload
```pascal
function getTeamWorkload(const assignee: string): double;
```
Calculate current workload (estimated hours) for a team member based on in-progress tasks.

**Example:**
```pascal
workload := manager.getTeamWorkload('alice@company.com');
// Returns total estimated hours for in-progress tasks
```

#### getCriticalPath
```pascal
function getCriticalPath: TTaskArray;
```
Get all critical priority tasks that are not yet completed.

### Enhanced Statistics Record

The `TTaskStats` record now includes:
```pascal
totalStoryPoints: integer;        // Total story points for all tasks
completedStoryPoints: integer;    // Story points for completed tasks
```

### Use Cases
- Burndown chart generation
- Team capacity analysis
- Priority management
- Schedule risk assessment
- Performance metrics

---

## Integration Examples

### Complete Agile Sprint Setup

```pascal
procedure setupAgileSpirit;
var
  taskId, i: integer;
  manager: TTaskManagerAdvanced;
begin
  manager := TTaskManagerAdvanced.Create;
  try
    // Create sprint tasks
    for i := 1 to 5 do
      taskId := manager.addTask(
        'Sprint Task ' + IntToStr(i),
        'Feature implementation',
        'Sprint 1',
        tpHigh,
        Now + 14
      );

    // Set story points
    manager.setTaskStoryPoints(taskId, 5);

    // Assign and add watchers
    manager.assignTaskTo(taskId, 'alice@company.com');
    manager.addTaskWatcher(taskId, 'bob@company.com');

    // Track burndown
    WriteLn('Sprint Velocity: ' + FloatToStr(manager.getVelocity(14)));
    WriteLn('Workload: ' + FloatToStr(manager.getTeamWorkload('alice@company.com')));
  finally
    manager.Free;
  end;
end;
```

---

## Data Structures

### TRecurrencePattern
```pascal
type TRecurrencePattern = (
  rpNone,       // No recurrence
  rpDaily,      // Every day
  rpWeekly,     // Every 7 days
  rpBiWeekly,   // Every 14 days
  rpMonthly,    // Every 30 days
  rpQuarterly,  // Every 90 days
  rpYearly      // Every 365 days
);
```

### TTaskReference
```pascal
type TTaskReference = record
  referenceId: integer;              // ID of referenced task
  relationshipType: string[50];       // Type of relationship
end;
```

### TWatcherArray
```pascal
type TWatcherArray = array of string;
```

### TReferenceArray
```pascal
type TReferenceArray = array of TTaskReference;
```

---

## Constants

```pascal
const
  maxWatchersPerTask = 20;        // Maximum watchers per task
  maxReferencesPerTask = 10;      // Maximum references per task
```

---

## Performance Considerations

1. **Recurrence Generation**: Generating large numbers of recurring task instances may consume memory
2. **Watcher Queries**: Getting tasks watched by a person iterates through all tasks
3. **Reference Lookup**: Task reference resolution requires searching for referenced task IDs
4. **Workload Calculation**: Sums estimated hours, should be called sparingly in tight loops

---

## Future Enhancements

1. Recurring task auto-generation on schedule
2. Watcher notifications/hooks
3. Reference relationship validation
4. Blocking reason categorization
5. Velocity forecasting
6. Team capacity constraints
7. Automatic unblocking on dependencies completion
8. Story point estimation assistance

---

## Summary

The Advanced Features extend the Task Manager with:
- **Recurrence Management**: 7 recurrence patterns
- **Agile Support**: Story points and velocity tracking
- **Collaboration**: Task watchers and references
- **Risk Management**: Task blocking with reasons
- **Analytics**: Sprint burndown and team workload

These features enable the Task Manager to support:
- ✓ Scrum/Agile methodologies
- ✓ Enterprise project management
- ✓ Cross-functional team coordination
- ✓ Risk and impediment tracking
- ✓ Performance analytics
