# Task Manager - Complete API Reference

## Overview

The Task Manager system is a comprehensive task management framework built in Object Pascal (Free Pascal). It provides a complete suite of classes for managing tasks, workflows, automation, persistence, analytics, and more.

## Core Task Management

### TTaskManagerClass

Primary interface for task management operations.

**AddTask** - Creates a new task. Returns task ID.
```pascal
function AddTask(title, description: string; priority: TTaskPriority;
                 dueDate: TDateTime): integer;
```
- Parameters: title (non-empty), description, priority (enum), dueDate (must be future)
- Returns: Positive integer task ID
- Behavior: Task status initialized to tsNotStarted

**GetTask** - Retrieves a task by ID
**UpdateTask** - Modifies an existing task
**DeleteTask** - Removes a task from the system
**GetTaskCount** - Returns total number of tasks
**SetTaskStatus** - Changes task status
**CompleteTask** - Marks task as completed
**GetAllTasks** - Returns all tasks as array

### TTaskQueryClass

Advanced querying and filtering capabilities.

**FilterByKeyword** - Full-text search across titles and descriptions
**FilterByDateRange** - Returns tasks within date range (inclusive)
**GetTasksByStatus** - Gets all tasks with specific status
**GetTasksByPriority** - Gets all tasks with specific priority
**GetOverdueTasks** - Returns overdue tasks not yet completed
**GetStats** - Returns comprehensive task statistics
**FilterByCategory** - Returns tasks in specified category
**FilterByAssignee** - Returns tasks assigned to person

## Workflow Management

### TTaskWorkflowManagerClass

Manages workflow states, transitions, and approval workflows.

**AddRule** - Creates workflow transition rule
```pascal
function AddRule(fromState, toState: TWorkflowState; action: TWorkflowAction;
                 requiresApproval, requiresComment: boolean;
                 description: string): integer;
```
- requiresApproval: Transition needs approval before completion
- requiresComment: Transition requires explanatory comment
- Returns: Rule ID

**IsValidTransition** - Validates if transition is allowed

**PerformTransition** - Executes state transition
```pascal
function PerformTransition(taskId: integer; action: TWorkflowAction;
                          performedBy, comment: string): integer;
```
- Returns: Transition ID if successful, -1 if invalid

**GetTaskTransitions** - Returns complete transition history for task
**ApproveTransition** - Approves a transition requiring approval
**RejectTransition** - Rejects a transition, rolling back state
**GetAverageTimeInState** - Calculates average time in state (identifies bottlenecks)
**GetStateDistribution** - Returns report of task distribution across states

## Automation

### TTaskAutomationManagerClass

Manages automated rules triggered by task events.

**CreateRule** - Creates automation rule
```pascal
function CreateRule(name, description: string; trigger: TAutomationTrigger;
                   triggerCondition: string; action: TAutomationAction;
                   actionParameter: string): integer;
```
- trigger: Event triggering rule (atTaskCompleted, atTaskOverdue, etc.)
- triggerCondition: Additional refinement condition
- action: Action to execute (aaCreateTask, aaUpdateStatus, aaEscalate, etc.)
- actionParameter: Parameter for action
- Returns: Rule ID

**UpdateRule** - Modifies existing automation rule
**DeleteRule** - Removes automation rule
**EnableRule** - Enables disabled rule
**DisableRule** - Disables rule temporarily
**ExecuteRule** - Manually executes specific rule
**ExecuteRulesByTrigger** - Executes all rules matching trigger event
**GetRulesByTrigger** - Returns rules associated with trigger type
**GetExecutionLog** - Returns complete execution history
**GetRuleExecutionCount** - Returns execution count for rule
**GetLastExecutionDate** - Returns when rule was last executed

## Task Templates

### TTaskTemplateManagerClass

Manages reusable task templates and template instances.

**CreateTemplate** - Creates task template with default values
```pascal
function CreateTemplate(name, description: string; priority: TTaskPriority;
                       assignee: string; estimatedHours: double;
                       category: string): integer;
```
- Returns: Template ID

**GetTemplate** - Retrieves template by ID
**GetAllTemplates** - Returns all defined templates
**DeleteTemplate** - Removes template (existing instances unaffected)
**UpdateTemplate** - Modifies existing template
**GetTemplatesByCategory** - Returns templates in category
**SearchTemplates** - Searches templates by keyword

**AddFieldToTemplate** - Adds custom field to template
```pascal
function AddFieldToTemplate(templateId: integer; fieldName, fieldType: string;
                           defaultValue: string; isRequired: boolean): boolean;
```
- fieldType: Type of field (string, integer, boolean, date, etc.)
- isRequired: Field must be populated when creating instances

**RemoveFieldFromTemplate** - Removes custom field from template
**GetTemplateFields** - Returns custom fields for template

**CreateTaskFromTemplate** - Creates task from template
```pascal
function CreateTaskFromTemplate(templateId: integer; title: string;
                               dueDate: TDateTime; taskId: integer): integer;
```
- Returns: ID of created task

**GetInstancesForTemplate** - Returns all tasks created from template
**GetAllInstances** - Returns all template instances
**GetTemplateUsageCount** - Returns times template used
**GetMostUsedTemplate** - Returns most frequently used template
**GetTemplateStatistics** - Returns usage statistics
**CloneTemplate** - Creates copy of template with new name

## Time Tracking

### TTaskTimeTrackingClass

Manages time entries and productivity analysis.

**StartTimeTracking** - Starts time tracking session
```pascal
function StartTimeTracking(taskId: integer; description: string): integer;
```
- description: Description of work being performed
- Returns: Entry ID

**StopTimeTracking** - Stops tracking session, calculates elapsed time
**GetTimeEntry** - Retrieves time entry details
**GetTaskTimeEntries** - Returns all entries for task
**GetAllTimeEntries** - Returns all time entries
**DeleteTimeEntry** - Removes time entry

**GetTaskTimeSummary** - Calculates task hours and variance
**GetTotalTimeSummary** - Calculates aggregate time metrics
**GetDailyTimeSummary** - Returns daily summary (hours, tasks completed)
**GetWeeklyTimeSummary** - Calculates week metrics
**GetMonthlyTimeSummary** - Calculates month metrics

**GetAverageTimePerTask** - Returns mean hours per task
**GetMostTimeSpentTask** - Returns task with most time invested
**GetTimeVariance** - Calculates variance from estimate
**GetProductivityTrend** - Returns tracking data for N days
**GetTotalTimeTracked** - Returns total hours tracked
**GetTimeEntryCount** - Returns total entries
**GetAverageTaskDuration** - Returns mean duration of completed tasks

## Validation

### TTaskValidatorClass

Provides data validation and business rule enforcement.

**ValidateTaskTitle** - Validates title (non-empty, length, characters)
**ValidateTaskDescription** - Validates description format
**ValidateDueDate** - Validates date is after creation date
**ValidateTaskData** - Comprehensive validation of all fields
**ValidatePriority** - Validates priority is valid enum
**ValidateAssignee** - Validates assignee exists
**ValidateTimeEstimate** - Validates hours are positive and reasonable
**ValidateCategory** - Validates category is recognized

All validation methods return TValidationResult with isValid flag and error message.

**CanDeleteTask** - Determines if task can be deleted
**CanChangeStatus** - Validates status transition is allowed

## Data Persistence

### TTaskPersistenceClass

Handles file I/O and backup operations.

**SaveAsCSV** - Exports tasks to CSV format with headers
**SaveAsText** - Exports tasks to human-readable text
**CreateBackup** - Creates timestamped backup of data
**LoadFromCSV** - Imports tasks from CSV file
**LoadFromFile** - Loads tasks from specified file

## Advanced Search

### TTaskSearchClass

Sophisticated search with criteria and saved searches.

**SearchTasks** - Searches using advanced criteria
```pascal
function SearchTasks(tasks: TTaskArray; criteria: TSearchCriteria): TTaskArray;
```
- Supports keyword, date range, status, priority filters
- Returns: Array of matching tasks

**FullTextSearch** - Quick full-text search in titles and descriptions
**SaveSearch** - Saves search configuration for reuse
**ExecuteSavedSearch** - Executes previously saved search
**FindDuplicates** - Identifies similar tasks by title/description

## Multi-Format Export

### TTaskExporterClass

Exports tasks to multiple formats for integration.

**ExportTasksAsJSON** - Exports to JSON (web services/APIs)
**ExportTasksAsXML** - Exports to XML (enterprise integration)
**ExportTasksAsCSV** - Exports to CSV (spreadsheet apps)
**ExportTasksAsHTML** - Exports to HTML (web viewing)
**ExportToFormat** - Generic export supporting all formats

## Analytics and Metrics

### TTaskAnalyticsClass

Provides analytics, metrics, and predictive analysis.

**CalculateProductivity** - Calculates tasks per day
```pascal
function CalculateProductivity(totalTasks, completedTasks: integer;
                               days: integer): double;
```
- Formula: completedTasks / days

**CalculateVelocity** - Calculates velocity (tasks/sprint period)
**GetProductivityMetrics** - Generates detailed metrics from daily counts
**EstimateCompletionDays** - Predicts days to complete remaining tasks
**PredictDeadlineMiss** - Predicts if deadline will be missed

**CalculateRiskScore** - Calculates risk score (0.0 to 1.0)
```pascal
function CalculateRiskScore(overdueCount: integer;
                            blockedCount: integer;
                            totalCount: integer): double;
```
- Formula: (overdueCount + blockedCount) / totalCount
- Returns: 0.0 (no risk) to 1.0 (high risk)

**CalculateTaskComplexity** - Calculates complexity from estimation accuracy
**GetAccuracyScore** - Scores estimation accuracy
**CalculateTeamCapacity** - Calculates total team capacity
**AnalyzeTrend** - Analyzes metric trends (increasing/decreasing/stable)
**CalculateBurndownData** - Generates burndown chart data
**CalculateStandardDeviation** - Calculates statistical standard deviation

## Audit and Compliance

### TTaskAuditManagerClass

Maintains complete audit trail for compliance.

**LogAction** - Logs task action with timestamp
```pascal
function LogAction(taskId: integer; action: TAuditActionType;
                  changedBy: string; description: string): integer;
```
- action: Created, Modified, Deleted, StatusChanged, etc.
- Returns: Audit entry ID

**LogChange** - Logs field change with before/after values

**GetTaskAuditLog** - Returns audit history for task
**GetAuditEntriesByType** - Returns entries of specific action type
**GetAuditEntriesByUser** - Returns entries created by user
**GetAuditEntriesInDateRange** - Returns entries in date range
**GetAllAuditEntries** - Returns complete audit log
**GetAuditCount** - Returns total audit entries
**GetAuditCountForTask** - Returns entries for task
**ClearAuditLog** - Removes all entries (use with caution)
**DeleteTaskAuditLog** - Removes entries for task
**DeleteOldEntries** - Removes entries before date

### TTaskAuditAnalysisClass

Analyzes audit data for reporting.

**GetLastModificationDate** - When task was last modified
**GetLastModificationBy** - Who last modified task
**GetModificationCount** - Total modifications for task
**GetMostActiveTasks** - Returns task IDs with most activity
**GetMostActiveUsers** - Returns users with most system activity
**GetAuditSummary** - Returns formatted audit history summary

## Reminders and Notifications

### TTaskReminderManagerClass

Manages task reminders and notifications.

**AddReminder** - Creates reminder for task
```pascal
function AddReminder(taskId: integer; reminderType: TReminderType;
                    timing: TReminderTiming; dueDate: TDateTime): integer;
```
- reminderType: Email, Notification, SMS, Popup
- timing: Before5Min, Before15Min, Before1Hour, Before1Day, Before1Week, Custom
- Returns: Reminder ID

**GetPendingReminders** - Returns unsent/unacknowledged reminders
**AcknowledgeReminder** - Marks reminder as acknowledged
**GetReminderStats** - Returns reminder statistics

## Team and Resource Management

### TTaskTeamManagerClass

Manages team members, skills, and workload.

**AddTeamMember** - Registers team member with skills
```pascal
function AddTeamMember(name: string; skills: array of string; 
                      available: boolean): integer;
```
- Returns: Member ID

**GetTeamMember** - Retrieves member information
**RemoveTeamMember** - Removes member from system
**GetAllTeamMembers** - Returns all team members
**UpdateMemberAvailability** - Updates availability status

**AssignTaskToMember** - Assigns task with allocation
```pascal
function AssignTaskToMember(taskId, memberId: integer; 
                           hoursAllocated: double; hourlyRate: double): integer;
```
- Returns: Assignment ID

**GetMemberAssignments** - Returns tasks assigned to member
**GetTaskAssignment** - Returns task assignment details
**RemoveAssignment** - Removes task assignment

**GetMemberWorkload** - Calculates member allocated hours
**IsMemberAvailable** - Checks if member available for work
**GetTeamCapacity** - Calculates total team capacity
**GetTeamUtilization** - Calculates utilization percentage
**GetMemberSkillMatch** - Returns skill match count
**FindBestMemberForTask** - Returns best qualified member for skills

## Type Definitions

### Enumerations

**TTaskStatus**: tsNotStarted, tsInProgress, tsCompleted, tsOnHold
**TTaskPriority**: tpLow, tpMedium, tpHigh, tpCritical
**TWorkflowState**: wsCreated, wsAssigned, wsInProgress, wsBlocked, wsReview, wsCompleted, wsCancelled
**TWorkflowAction**: waStart, waProgress, waBlock, waUnblock, waRequestReview, waApprove, waReject, waCancel, waReopen
**TAutomationTrigger**: atTaskCreated, atTaskCompleted, atTaskOverdue, atDateReached, atStatusChanged, atPriorityChanged, atAssigneeChanged, atCustom
**TAutomationAction**: aaCreateTask, aaUpdateStatus, aaUpdatePriority, aaAddNote, aaAssignTo, aaSetDueDate, aaSendNotification, aaEscalate, aaAddTag
**TExportFormat**: efJSON, efXML, efCSV, efHTML

### Records

**TTask**: id, title, description, priority, status, dueDate, createdDate, completedDate, category, assignee, estimatedHours, tags
**TTaskStats**: totalTasks, completedTasks, inProgressTasks, overdueTasks, completionRate
**TSearchCriteria**: keyword, searchTitle, searchDescription, statusFilter, priorityFilter, startDate, endDate, useDateRange, caseSensitive
**TValidationResult**: isValid, errorMessage, errorCode
**TReminderStats**: totalReminders, pendingReminders, acknowledgedReminders, overdueReminders, emailReminders, notificationReminders

## Error Handling

- **Boolean returns**: Check value before proceeding
- **Validation methods**: Return TValidationResult with isValid flag
- **Array returns**: Empty arrays indicate no matches
- **ID returns**: -1 or 0 indicate failure

Implement appropriate error handling rather than relying on exceptions.

## Implementation Notes

1. **Thread Safety**: Not thread-safe; add locking for multi-threaded use
2. **Persistence**: Save work regularly using TTaskPersistenceClass
3. **Audit Trail**: Enable logging for compliance operations
4. **Validation**: Always validate input with TTaskValidatorClass
5. **Performance**: Use indexed queries for large task lists (>1000 tasks)
6. **Workflow**: Define rules before transitions to prevent invalid states
