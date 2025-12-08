
# Advanced Task Manager Features

This document describes the advanced features added to the task management system beyond the basic and extended functionality.

## Overview

The `taskmanageradvanced.pas` unit extends `TExtendedTaskManager` with four major feature categories:

1. **Work Session Tracking** (Pomodoro-style time management)
2. **Task Notes & Comments** (Timestamped annotations)
3. **Task Dependencies** (Blocking/blocked-by relationships)
4. **Task Templates** (Reusable workflow patterns)

## 1. Work Session Tracking

Track focused work sessions on tasks, similar to the Pomodoro Technique.

### Key Features
- Start and end work sessions with notes
- Track session duration automatically
- Mark sessions as completed or interrupted
- View all sessions for a specific task
- Calculate total work time per task
- Get session statistics and averages

### Usage Example
```pascal
var
  Manager: TAdvancedTaskManager;
  SessionID: Integer;
begin
  Manager := TAdvancedTaskManager.Create;
  try
    // Start a work session
    SessionID := Manager.StartWorkSession(TaskID, 'Debugging login issue');
    
    // ... do work ...
    
    // End the session (true = completed, false = interrupted)
    Manager.EndWorkSession(SessionID, True);
    
    // Get total work time for a task
    TotalMinutes := Manager.GetTotalWorkTime(TaskID);
    
    // View session statistics
    WriteLn(Manager.GetSessionStats);
  finally
    Manager.Free;
  end;
end;
```

### Session Data Structure
```pascal
TWorkSession = record
  SessionID: Integer;
  TaskID: Integer;
  StartTime: TDateTime;
  EndTime: TDateTime;
  DurationMinutes: Integer;
  Notes: string;
  WasCompleted: Boolean;
end;
```

## 2. Task Notes & Comments

Add timestamped notes to tasks for tracking progress, decisions, and blockers.

### Key Features
- Add notes with author, content, and type
- Support different note types: comment, update, decision, blocker
- Timestamp automatically recorded
- Retrieve all notes for a task
- Delete individual notes
- Format notes as readable strings

### Usage Example
```pascal
var
  NoteID: Integer;
begin
  // Add different types of notes
  NoteID := Manager.AddNote(TaskID, 'Developer', 
    'Found the root cause', 'decision');
    
  NoteID := Manager.AddNote(TaskID, 'QA Team', 
    'This is blocking the release!', 'blocker');
    
  // Get all notes for a task
  Notes := Manager.GetNotes(TaskID);
  
  // Display formatted notes
  WriteLn(Manager.NotesToString(TaskID));
end;
```

### Note Types
- **comment**: General comments or observations
- **update**: Progress updates
- **decision**: Important decisions made
- **blocker**: Issues blocking progress

## 3. Task Dependencies

Define relationships between tasks where one task depends on another being completed first.

### Key Features
- Add dependencies with different relationship types
- Validate task completion based on dependencies
- View dependency chains
- See which tasks are blocking others
- See which tasks are blocked by others
- Support for lag/lead time between dependent tasks

### Dependency Types
```pascal
TDependencyType = (
  dtFinishToStart,   // Task B starts when Task A finishes (most common)
  dtStartToStart,    // Task B starts when Task A starts
  dtFinishToFinish,  // Task B finishes when Task A finishes
  dtStartToFinish    // Task B finishes when Task A starts
);
```

### Usage Example
```pascal
var
  DepID: Integer;
begin
  // Task 2 can only start when Task 1 is finished
  DepID := Manager.AddDependency(
    Task2ID,        // Dependent task
    Task1ID,        // Depends on this task
    dtFinishToStart, // Dependency type
    0               // Lag days (0 = immediate)
  );
  
  // Check if a task can be completed
  if Manager.ValidateTaskCompletion(Task2ID) then
    WriteLn('All dependencies met!')
  else
    WriteLn('Cannot complete - dependencies not satisfied');
    
  // View dependency chain
  WriteLn(Manager.GetDependencyChain(Task2ID));
  
  // See what tasks are blocked by this one
  BlockingTasks := Manager.GetBlocking(Task1ID);
end;
```

### Lag Time
The `LagDays` parameter allows specifying a delay:
- Positive values: delay after the predecessor task
- Negative values: lead time (can start before predecessor finishes)

## 4. Task Templates

Create reusable templates for common workflows and quickly generate tasks from them.

### Key Features
- Define templates with default values
- Include checklists in templates
- Create tasks from templates with custom dates
- Manage template library
- Support for categories, priorities, and time estimates

### Usage Example
```pascal
var
  TemplateID, TaskID: Integer;
begin
  // Create a template
  TemplateID := Manager.CreateTemplate(
    'Bug Fix Template',           // Name
    'Standard template for bugs', // Description
    'Development',                // Category
    tpHigh,                        // Default priority
    2.0,                          // Estimated hours
    3                             // Default due in 3 days
  );
  
  // Add checklist items
  Manager.AddChecklistToTemplate(TemplateID, 'Reproduce the bug');
  Manager.AddChecklistToTemplate(TemplateID, 'Identify root cause');
  Manager.AddChecklistToTemplate(TemplateID, 'Implement fix');
  Manager.AddChecklistToTemplate(TemplateID, 'Test the fix');
  
  // Create a task from the template
  TaskID := Manager.CreateTaskFromTemplate(
    TemplateID,
    'Fix login authentication bug',
    EncodeDate(2024, 2, 15)
  );
  
  // View all templates
  Templates := Manager.GetAllTemplates;
end;
```

### Template Benefits
- **Consistency**: Ensure all similar tasks follow the same structure
- **Efficiency**: Quickly create tasks without entering all details
- **Best Practices**: Encode organizational best practices in templates
- **Checklists**: Include standard checklists for complex workflows

## Advanced Analytics

The advanced task manager includes additional analytics functions:

### Work Session Analytics
```pascal
// Average session duration across all tasks
AvgMinutes := Manager.GetAverageSessionDuration;

// Session statistics summary
WriteLn(Manager.GetSessionStats);
```

### Available Methods
- `GetProductivityByTimeOfDay`: Analyze when you're most productive (planned)
- `GetMostProductiveDays`: Identify most productive days of the week (planned)
- `GetTaskCompletionForecast`: Predict task completion based on work patterns (planned)

## File Operations

Save and load all advanced features:

```pascal
// Save everything (tasks, sessions, notes, dependencies, templates)
Manager.SaveAdvancedToFile('tasks_advanced.dat');

// Load from file
Manager.LoadAdvancedFromFile('tasks_advanced.dat');

// Export to CSV (includes all data)
CSVData := Manager.ExportAdvancedToCSV;
```

## Integration with Extended Features

All extended features from `TExtendedTaskManager` remain available:
- Recurring tasks
- Subtasks and hierarchies
- Batch operations
- Priority scoring
- Productivity reports
- Time management analysis

## Complete Workflow Example

```pascal
program TaskWorkflow;
uses
  SysUtils, DateUtils, taskmanager, taskmanagerext, taskmanageradvanced;

var
  Manager: TAdvancedTaskManager;
  Template, Task1, Task2, SessionID: Integer;
  
begin
  Manager := TAdvancedTaskManager.Create;
  try
    // 1. Create a template for common task type
    Template := Manager.CreateTemplate(
      'Code Review', 'Review code changes',
      'Development', tpMedium, 1.5, 2);
      
    // 2. Create tasks from template
    Task1 := Manager.CreateTaskFromTemplate(Template,
      'Review PR #123', Now + 1);
    Task2 := Manager.CreateTaskFromTemplate(Template,
      'Review PR #124', Now + 2);
      
    // 3. Add dependency (Task2 depends on Task1)
    Manager.AddDependency(Task2, Task1, dtFinishToStart, 0);
    
    // 4. Add notes
    Manager.AddNote(Task1, 'Lead Dev', 
      'Please review the authentication changes', 'comment');
      
    // 5. Start work session
    SessionID := Manager.StartWorkSession(Task1, 'Reviewing code');
    
    // ... do work ...
    
    // 6. End session
    Manager.EndWorkSession(SessionID, True);
    
    // 7. Complete task and verify dependencies
    Manager.UpdateTaskStatus(Task1, tsCompleted);
    
    if Manager.ValidateTaskCompletion(Task2) then
      WriteLn('Task 2 is now ready to start!');
      
    // 8. Save all data
    Manager.SaveAdvancedToFile('tasks.dat');
    
  finally
    Manager.Free;
  end;
end.
```

## Best Practices

1. **Work Sessions**: Use 25-minute Pomodoro sessions for focused work
2. **Notes**: Add notes for important decisions and blockers
3. **Dependencies**: Map out dependencies before starting complex projects
4. **Templates**: Create templates for recurring task patterns
5. **Regular Commits**: Save your task data regularly to avoid data loss

## Future Enhancements

Planned features for future versions:
- Circular dependency detection
- Gantt chart data export
- Resource allocation tracking
- Custom field support
- Notification system
- Calendar integration
- Team collaboration features

---

*For basic features, see README.md*  
*For extended features, see README_EXTENDED.md*
