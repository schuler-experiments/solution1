
# Task Manager - Complete Usage Guide

## Quick Start

### Compilation

```bash
fpc solution1/solution1.pas -obin/task_manager -O1 -Mobjfpc
```

### Running Tests

```bash
./bin/task_manager
```

Expected output: All 13 tests pass successfully ✓

---

## Architecture Overview

The Task Manager uses a three-tier class hierarchy for progressive feature enhancement:

```
TTaskManager (Base Class)
    ↓ extends
TTaskManagerExtended (Intermediate Layer)
    ↓ extends
TTaskManagerAdvanced (Enterprise Layer)
```

### Layer 1: TTaskManager (Core - 30+ methods)
- Task CRUD operations (Create, Read, Update, Delete)
- Task filtering and sorting
- Task search functionality
- Time tracking (estimated/actual hours)
- Tag/label management
- Task dependencies
- Statistics generation

### Layer 2: TTaskManagerExtended (Enhanced - 18+ methods)
- Task notes and comments
- Team member assignments
- Project milestones
- Reusable task templates

### Layer 3: TTaskManagerAdvanced (Advanced - 20+ methods)
- Task recurrence patterns
- Story points and velocity tracking
- Task watchers (observers)
- Task references and linking
- Task blocking with impediment tracking
- Advanced analytics

---

## Usage Patterns

### Pattern 1: Basic Task Management

```pascal
program BasicTaskManager;
uses TaskTypes, TaskManager;

var
  manager: TTaskManager;
  taskId: integer;
  task: TTask;
begin
  manager := TTaskManager.Create;
  try
    { Create a task }
    taskId := manager.addTask(
      'Implement login feature',
      'Add user authentication',
      'Authentication',
      tpHigh,
      Now + 7  { Due date: 7 days from now }
    );

    { Retrieve and modify }
    if manager.getTask(taskId, task) then
    begin
      task.status := tsInProgress;
      manager.updateTask(task);
    end;

    { Complete the task }
    manager.completeTask(taskId);

    { Get statistics }
    WriteLn('Total tasks: ', manager.getTaskCount);
  finally
    manager.Free;
  end;
end.
```

### Pattern 2: Team Collaboration with Extended Features

```pascal
program TeamCollaboration;
uses TaskTypes, TaskManager, TaskManagerExtended;

var
  manager: TTaskManagerExtended;
  taskId, milestoneId, templateId: integer;
begin
  manager := TTaskManagerExtended.Create;
  try
    { Create a milestone }
    milestoneId := manager.addMilestone(
      'Sprint 1',
      'Initial development sprint',
      Now + 14
    );

    { Create a template for recurring tasks }
    templateId := manager.createTemplate(
      'Code Review',
      'Review pull requests',
      'Development',
      tpMedium,
      2.0  { Estimated hours }
    );
    manager.addTagToTemplate(templateId, 'review');
    manager.addTagToTemplate(templateId, 'quality');

    { Create task from template }
    taskId := manager.createTaskFromTemplate(
      templateId,
      'Review PR #123',
      Now + 1
    );

    { Assign to team member }
    manager.assignTaskTo(taskId, 'alice@company.com');

    { Add notes }
    manager.addTaskNote(
      taskId,
      'Focus on error handling in login flow',
      true  { Mark as important }
    );

    { Assign to milestone }
    manager.assignTaskToMilestone(taskId, milestoneId);

    { Get milestone progress }
    WriteLn('Tasks in Sprint 1: ', length(manager.getTasksByMilestone(milestoneId)));
  finally
    manager.Free;
  end;
end.
```

### Pattern 3: Agile/Scrum with Advanced Features

```pascal
program AgileSprintManagement;
uses TaskTypes, TaskManager, TaskManagerExtended, TaskManagerAdvanced;

var
  manager: TTaskManagerAdvanced;
  taskId, i: integer;
  blockedTasks: TTaskArray;
  velocity: double;
  workload: double;
begin
  manager := TTaskManagerAdvanced.Create;
  try
    { Create sprint tasks with story points }
    for i := 1 to 5 do
    begin
      taskId := manager.addTask(
        'Feature ' + IntToStr(i),
        'Implementation task',
        'Sprint-42',
        tpHigh,
        Now + 14
      );

      { Assign story points (Agile estimation) }
      manager.setTaskStoryPoints(taskId, (i * 3) mod 13);

      { Assign to developer }
      manager.assignTaskTo(taskId, 'dev' + IntToStr((i mod 3) + 1) + '@company.com');

      { Add watchers for visibility }
      manager.addTaskWatcher(taskId, 'scrum-master@company.com');
      manager.addTaskWatcher(taskId, 'product-owner@company.com');
    end;

    { Track recurring daily standup }
    taskId := manager.addTask(
      'Daily Standup',
      'Team synchronization',
      'Meetings',
      tpMedium,
      Now + 1
    );
    manager.setTaskRecurrence(taskId, rpDaily, Now + 90);
    manager.addTaskWatcher(taskId, 'team@company.com');

    { Handle blockers }
    manager.blockTask(1, 'Waiting for design approval');
    blockedTasks := manager.getBlockedTasks;
    WriteLn('Currently blocked tasks: ', length(blockedTasks));

    { Get sprint metrics }
    velocity := manager.getVelocity(14);
    WriteLn('Sprint velocity: ', velocity:0:2, ' points/day');

    { Check team member workload }
    workload := manager.getTeamWorkload('dev1@company.com');
    WriteLn('Dev 1 workload: ', workload:0:2, ' hours');

    { Get critical path }
    WriteLn('Critical path size: ', length(manager.getCriticalPath));
  finally
    manager.Free;
  end;
end.
```

### Pattern 4: Enterprise Project Management

```pascal
program EnterpriseProjects;
uses TaskTypes, TaskManager, TaskManagerExtended, TaskManagerAdvanced;

var
  manager: TTaskManagerAdvanced;
  taskId1, taskId2, taskId3: integer;
begin
  manager := TTaskManagerAdvanced.Create;
  try
    { Create interdependent tasks }
    taskId1 := manager.addTask('Design System', 'Architecture design', 'Phase-1', tpCritical, Now + 7);
    taskId2 := manager.addTask('Implementation', 'Develop components', 'Phase-1', tpHigh, Now + 14);
    taskId3 := manager.addTask('Testing', 'QA and testing', 'Phase-1', tpHigh, Now + 21);

    { Create dependencies }
    manager.addTaskDependency(taskId2, taskId1);
    manager.addTaskDependency(taskId3, taskId2);

    { Create task references }
    manager.addTaskReference(taskId1, taskId2, 'blocks');
    manager.addTaskReference(taskId2, taskId3, 'blocks');

    { Assign to team leads }
    manager.assignTaskTo(taskId1, 'architect@company.com');
    manager.assignTaskTo(taskId2, 'tech-lead@company.com');
    manager.assignTaskTo(taskId3, 'qa-lead@company.com');

    { Add multiple watchers }
    manager.addTaskWatcher(taskId1, 'cto@company.com');
    manager.addTaskWatcher(taskId1, 'pm@company.com');

    { Add notes with timestamps }
    manager.addTaskNote(taskId1, 'Review with stakeholders before starting', true);
    manager.addTaskNote(taskId1, 'Architecture approved on 2024-01-15', false);

    { Set time estimates }
    manager.setTaskEstimatedHours(taskId1, 40.0);
    manager.setTaskEstimatedHours(taskId2, 80.0);
    manager.setTaskEstimatedHours(taskId3, 30.0);

    { Log actual hours }
    manager.addTaskActualHours(taskId1, 42.0);

    { Check for overruns }
    WriteLn('Design overrun: ', manager.getTaskTimeOverrun(taskId1):0:2, ' hours');

    { Get statistics }
    WriteLn('Project size: ', manager.getTotalStoryPoints, ' story points');
    WriteLn('Critical tasks: ', length(manager.getCriticalPath));
  finally
    manager.Free;
  end;
end.
```

---

## Key Features by Use Case

### For Agile Teams

| Feature | Method | Use Case |
|---------|--------|----------|
| Story Points | `setTaskStoryPoints()` | Sprint estimation |
| Velocity | `getVelocity()` | Capacity planning |
| Sprint Burndown | `getSprintBurndown()` | Progress tracking |
| Team Workload | `getTeamWorkload()` | Load balancing |
| Critical Path | `getCriticalPath()` | Risk identification |

### For Team Collaboration

| Feature | Method | Use Case |
|---------|--------|----------|
| Task Notes | `addTaskNote()` | Comments and discussions |
| Task Watchers | `addTaskWatcher()` | Awareness and notifications |
| Assignments | `assignTaskTo()` | Task ownership |
| Milestones | `addMilestone()` | Project phases |
| Templates | `createTemplate()` | Standardization |

### For Project Management

| Feature | Method | Use Case |
|---------|--------|----------|
| Dependencies | `addTaskDependency()` | Task sequences |
| References | `addTaskReference()` | Relationship mapping |
| Blocking | `blockTask()` | Impediment tracking |
| Time Tracking | `addTaskActualHours()` | Resource planning |
| Statistics | `getStatistics()` | Progress reporting |

### For Operations

| Feature | Method | Use Case |
|---------|--------|----------|
| Recurrence | `setTaskRecurrence()` | Recurring tasks |
| Tags | `addTaskTag()` | Organization |
| Status | `setTaskStatus()` | Workflow tracking |
| Priority | `setTaskPriority()` | Work prioritization |
| Search | `searchTasks()` | Task discovery |

---

## Data Types Reference

### Task Status
```pascal
type TTaskStatus = (
  tsNew,          { Newly created }
  tsInProgress,   { Currently being worked on }
  tsBlocked,      { Waiting for something }
  tsCompleted,    { Done }
  tsCancelled     { Cancelled or rejected }
);
```

### Task Priority
```pascal
type TTaskPriority = (
  tpLow,          { Can wait }
  tpMedium,       { Normal priority }
  tpHigh,         { Needs attention }
  tpCritical      { Urgent, do first }
);
```

### Recurrence Patterns
```pascal
type TRecurrencePattern = (
  rpNone,         { No recurrence }
  rpDaily,        { Every day }
  rpWeekly,       { Every 7 days }
  rpBiWeekly,     { Every 14 days }
  rpMonthly,      { Every 30 days }
  rpQuarterly,    { Every 90 days }
  rpYearly        { Every 365 days }
);
```

---

## Common Queries

### Get all tasks for a person
```pascal
var tasks: TTaskArray;
begin
  tasks := manager.getTasksAssignedTo('alice@company.com');
end;
```

### Find overdue tasks
```pascal
var tasks: TTaskArray;
    i: integer;
begin
  tasks := manager.getTasksByStatus(tsInProgress);
  for i := 0 to high(tasks) do
  begin
    if tasks[i].dueDate < Now then
      WriteLn('Overdue: ', tasks[i].name);
  end;
end;
```

### Get blocked tasks with reasons
```pascal
var blockedTasks: TTaskArray;
    i: integer;
begin
  blockedTasks := manager.getBlockedTasks;
  for i := 0 to high(blockedTasks) do
    WriteLn('Blocked: ', blockedTasks[i].name, ' - ', blockedTasks[i].blockReason);
end;
```

### Calculate team capacity
```pascal
var assignees: array of string;
    totalCapacity: double;
    i: integer;
begin
  assignees := ['alice@company.com', 'bob@company.com', 'charlie@company.com'];
  totalCapacity := 0;
  for i := 0 to high(assignees) do
    totalCapacity := totalCapacity + manager.getTeamWorkload(assignees[i]);
  WriteLn('Total team workload: ', totalCapacity:0:2, ' hours');
end;
```

### Get sprint statistics
```pascal
var sprintTasks: TTaskArray;
    stats: TTaskStats;
begin
  sprintTasks := manager.getSprintBurndown('Sprint-42');
  stats := manager.getStatistics;
  WriteLn('Sprint tasks: ', length(sprintTasks));
  WriteLn('Completed: ', stats.completedTasks);
  WriteLn('In progress: ', stats.inProgressTasks);
end;
```

---

## Integration Examples

### With GUI Framework (Lazarus)

The Task Manager can be used as a backend for a Lazarus application by:
1. Creating a manager instance in the main form's `OnCreate` event
2. Binding UI controls to manager methods
3. Using the task arrays returned by queries to populate list views
4. Handling events like button clicks by calling manager methods

### With Web Service

The Task Manager can be wrapped in a REST API by:
1. Creating a manager instance in your web service
2. Exposing methods as HTTP endpoints
3. Converting task data to JSON for responses
4. Accepting task operations via POST/PUT requests

### With Database

Tasks can be persisted by:
1. Creating a persistence layer that reads/writes to database
2. Using `getAllTasks()` to export data
3. Importing tasks via `addTask()` in bulk operations
4. Implementing custom save/load methods

---

## Performance Tips

1. **Batch Operations**: Create multiple tasks in a loop rather than one at a time
2. **Query Caching**: Store frequently accessed arrays locally
3. **Avoid Deep Searches**: Use filtered queries before searching
4. **Limit Array Copies**: Pass arrays as `var` parameters when possible
5. **Clean Up**: Always call `Free` on the manager in a try/finally block

---

## Troubleshooting

### Compilation Issues

If compilation fails, check:
- All unit dependencies are in the same folder
- File paths use forward slashes
- No circular unit dependencies

### Runtime Issues

If tests fail, verify:
- Enough memory available
- No infinite loops in custom code
- Proper cleanup with `Free`
- Valid date values

### Performance Issues

If the system is slow:
- Reduce number of watchers per task
- Limit tags per task to necessary ones
- Use specific queries instead of `getAllTasks()`
- Avoid large loops over task arrays

---

## Summary

The Task Manager provides:
- ✓ **Core**: Basic task management with full CRUD
- ✓ **Extended**: Team collaboration and milestones
- ✓ **Advanced**: Agile features and analytics

Choose the class that fits your needs:
- `TTaskManager` for simple task tracking
- `TTaskManagerExtended` for team projects
- `TTaskManagerAdvanced` for enterprise/Agile

All classes maintain full backward compatibility through inheritance.
