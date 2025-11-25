
# Subtasks and History Features

## Overview

Version 4 of the Task Manager introduces two major new capabilities:
- **Subtask Support** - Breaking down complex tasks into smaller, manageable work items
- **Task History/Audit Logging** - Complete audit trail of all task changes

## Subtask Management

Subtasks allow you to break down a complex task into smaller, more manageable units of work.

### Features

1. **Add Subtask**
   - Create a subtask with title and estimated hours
   - Returns subtask ID for future reference
   - Limited to 50 subtasks per task

2. **Manage Subtasks**
   - Update subtask details
   - Remove individual subtasks
   - Assign subtasks to team members
   - Track completion status independently

3. **Progress Tracking**
   - Get completion percentage for a task based on subtasks
   - Query if all subtasks are complete
   - Count subtasks for a task
   - View subtasks for a specific assignee

### Example Usage

```pascal
// Create a main task
mainTaskId := manager.addTask('Implement API', 'REST API for user management',
                              'Development', tpHigh, now + 14);

// Add subtasks
manager.addSubtask(mainTaskId, 'Design endpoints', 3.0);
manager.addSubtask(mainTaskId, 'Implement authentication', 5.0);
manager.addSubtask(mainTaskId, 'Write unit tests', 4.0);
manager.addSubtask(mainTaskId, 'Integration testing', 3.0);

// Assign subtasks to team members
manager.assignSubtaskTo(mainTaskId, 0, 'alice');   // Design
manager.assignSubtaskTo(mainTaskId, 1, 'bob');     // Authentication
manager.assignSubtaskTo(mainTaskId, 2, 'charlie'); // Tests
manager.assignSubtaskTo(mainTaskId, 3, 'alice');   // Integration

// Track progress
WriteLn('Completion: ' + FloatToStr(
  manager.getSubtaskCompletionPercentage(mainTaskId)) + '%');

// Complete subtasks as they're done
manager.completeSubtask(mainTaskId, 0); // Design done
manager.completeSubtask(mainTaskId, 1); // Auth done

// Check progress again
WriteLn('Completion: ' + FloatToStr(
  manager.getSubtaskCompletionPercentage(mainTaskId)) + '%'); // 50%

// View all subtasks for a person
subtasks := manager.getSubtasksForAssignee('alice');
WriteLn('Alice has ' + intToStr(length(subtasks)) + ' subtask(s)');
```

## Task History and Audit Logging

The audit logging system tracks every change made to a task, providing complete visibility into task evolution.

### Features

1. **Record Changes**
   - Log field changes with old and new values
   - Track who made the change and when
   - Limited to 100 history entries per task (oldest removed when exceeded)

2. **Query History**
   - Get complete history for a task
   - Filter history by time period
   - View all changes made by a specific person
   - Get last modification time

3. **Audit Trail**
   - Generate human-readable audit trail for a task
   - Shows all changes in chronological order
   - Useful for compliance and debugging

### Example Usage

```pascal
// Record changes as they happen
taskId := manager.addTask('Sprint Planning', 'Plan sprint tasks', 'Planning',
                         tpMedium, now + 3);

// Log the change
manager.recordTaskChange(taskId, 'status', '', 'new', 'system');
manager.recordTaskChange(taskId, 'assignedTo', '', 'alice', 'alice');
manager.recordTaskChange(taskId, 'priority', 'medium', 'high', 'alice');

// View history
history := manager.getTaskHistory(taskId);
WriteLn('Task has ' + intToStr(length(history)) + ' history entries');

// Get audit trail (human-readable)
auditTrail := manager.getTaskAuditTrail(taskId);
WriteLn(auditTrail);
{ Output:
  Task ID: X (Sprint Planning)
  Created: 25-11-25 09:11:21

  Audit Trail:
  25-11-25 09:11:21 - system changed status from "" to "new"
  25-11-25 09:11:21 - alice changed assignedTo from "" to "alice"
  25-11-25 09:11:21 - alice changed priority from "medium" to "high"
}

// Query history since a specific time
recentHistory := manager.getHistorySince(taskId, now - 1); // Last 24 hours
WriteLn('Changes in last 24 hours: ' + intToStr(length(recentHistory)));

// View all changes by a person
aliceChanges := manager.getChangesBy('alice');
WriteLn('Alice has made ' + intToStr(length(aliceChanges)) + ' changes');

// Get last modification time
lastMod := manager.getLastModificationTime(taskId);
WriteLn('Last modified: ' + DateTimeToStr(lastMod));
```

## Architecture

### TaskManagerSubtasks Class
- Extends TTaskManagerAdvanced
- Provides all subtask-related operations
- Integrates with existing task management

### TaskManagerHistory Class
- Extends TTaskManagerSubtasks
- Provides audit logging capabilities
- Tracks all changes with timestamps

### TaskManagerEnhanced Class
- Extends TaskManagerHistory
- Combines all previous capabilities
- Adds advanced features (see below)

## Limits

- Maximum 50 subtasks per task
- Maximum 100 history entries per task (oldest automatically removed)
- String fields limited to 255 characters

## Integration with GUI

These features are designed for GUI integration:
- No user input required
- All operations return success/failure indicators
- History can be displayed in audit log UI
- Subtask progress can drive visual progress bars
- Audit trail can be exported to reports

