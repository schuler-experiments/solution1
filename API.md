# Task Manager - API Reference

## Core Classes

### TTaskManagerClass

Main interface for task management operations.

#### Methods

**AddTask**
```pascal
function AddTask(title, description: string; priority: TTaskPriority;
                 dueDate: TDateTime): integer;
```
Creates a new task and returns its ID.

**GetTask**
```pascal
function GetTask(id: integer): TTask;
```
Retrieves a task by ID.

**UpdateTask**
```pascal
procedure UpdateTask(id: integer; newTask: TTask);
```
Updates an existing task.

**DeleteTask**
```pascal
procedure DeleteTask(id: integer);
```
Removes a task from the system.

**GetTaskCount**
```pascal
function GetTaskCount(): integer;
```
Returns the total number of tasks.

**SetTaskStatus**
```pascal
procedure SetTaskStatus(id: integer; newStatus: TTaskStatus);
```
Changes the status of a task.

**CompleteTask**
```pascal
procedure CompleteTask(id: integer);
```
Marks a task as completed.

### TTaskQueryClass

Advanced querying and filtering.

#### Methods

**FilterByKeyword**
```pascal
function FilterByKeyword(keyword: string): TTaskArray;
```
Full-text search across all tasks.

**FilterByDateRange**
```pascal
function FilterByDateRange(startDate, endDate: TDateTime): TTaskArray;
```
Get tasks within a date range.

**GetTasksByStatus**
```pascal
function GetTasksByStatus(status: TTaskStatus): TTaskArray;
```
Get all tasks with a specific status.

**GetOverdueTasks**
```pascal
function GetOverdueTasks(): TTaskArray;
```
Get all overdue tasks.

**GetStats**
```pascal
function GetStats(): TTaskStats;
```
Get comprehensive statistics about all tasks.

### TTaskPersistenceClass

File I/O and persistence.

#### Methods

**SaveAsCSV**
```pascal
procedure SaveAsCSV(mgr: TTaskManagerClass);
```
Export tasks to CSV format.

**LoadFromCSV**
```pascal
procedure LoadFromCSV(mgr: TTaskManagerClass);
```
Import tasks from CSV format.

**SaveAsText**
```pascal
procedure SaveAsText(mgr: TTaskManagerClass);
```
Export tasks to text format.

**CreateBackup**
```pascal
procedure CreateBackup();
```
Create a backup of the current data file.

### TTaskSearchClass

Advanced search capabilities.

#### Methods

**SearchTasks**
```pascal
function SearchTasks(tasks: TTaskArray; criteria: TSearchCriteria): TTaskArray;
```
Search with advanced criteria.

**FullTextSearch**
```pascal
function FullTextSearch(tasks: TTaskArray; keyword: string): TTaskArray;
```
Quick full-text search.

**SaveSearch**
```pascal
function SaveSearch(name: string; criteria: TSearchCriteria): integer;
```
Save a search for reuse.

**FindDuplicates**
```pascal
function FindDuplicates(tasks: TTaskArray): TTaskArray;
```
Find duplicate tasks.

### TTaskExporterClass

Multi-format export.

#### Methods

**ExportTasksAsJSON**
```pascal
function ExportTasksAsJSON(tasks: TTaskArray; filename: string): boolean;
```
Export to JSON format.

**ExportTasksAsXML**
```pascal
function ExportTasksAsXML(tasks: TTaskArray; filename: string): boolean;
```
Export to XML format.

**ExportTasksAsCSV**
```pascal
function ExportTasksAsCSV(tasks: TTaskArray; filename: string): boolean;
```
Export to CSV format.

**ExportTasksAsHTML**
```pascal
function ExportTasksAsHTML(tasks: TTaskArray; filename: string): boolean;
```
Export to HTML format.

**ExportToFormat**
```pascal
function ExportToFormat(tasks: TTaskArray; filename: string;
                       format: TExportFormat): boolean;
```
Export to specified format.

### TTaskAnalyticsClass

Analytics and metrics.

#### Methods

**CalculateProductivity**
```pascal
function CalculateProductivity(totalTasks, completedTasks: integer;
                               days: integer): double;
```
Calculate tasks per day.

**CalculateVelocity**
```pascal
function CalculateVelocity(completedCount: integer;
                           sprintDays: integer): double;
```
Calculate velocity metrics.

**EstimateCompletionDays**
```pascal
function EstimateCompletionDays(remainingTasks: integer;
                                currentVelocity: double): integer;
```
Predict project completion date.

**PredictDeadlineMiss**
```pascal
function PredictDeadlineMiss(tasksRemaining: integer;
                             daysRemaining: integer;
                             velocity: double): boolean;
```
Predict if deadline will be missed.

**CalculateRiskScore**
```pascal
function CalculateRiskScore(overdueCount: integer;
                            blockedCount: integer;
                            totalCount: integer): double;
```
Calculate project risk score.

### TTaskReminderManagerClass

Notification and reminders.

#### Methods

**AddReminder**
```pascal
function AddReminder(taskId: integer; reminderType: TReminderType;
                    timing: TReminderTiming; dueDate: TDateTime): integer;
```
Add a reminder for a task.

**GetPendingReminders**
```pascal
function GetPendingReminders(): TReminderArray;
```
Get all pending reminders.

**AcknowledgeReminder**
```pascal
function AcknowledgeReminder(id: integer): boolean;
```
Mark a reminder as acknowledged.

**GetReminderStats**
```pascal
function GetReminderStats(): TReminderStats;
```
Get reminder statistics.

## Usage Example

```pascal
program Example;
uses TaskManager, TaskTypes, TaskQuery, TaskExporter;

var
  mgr: TTaskManagerClass;
  query: TTaskQueryClass;
  exporter: TTaskExporterClass;
  taskId: integer;
  stats: TTaskStats;

begin
  { Initialize managers }
  mgr := TTaskManagerClass.Create();
  query := TTaskQueryClass.Create(mgr);
  exporter := TTaskExporterClass.Create();
  
  try
    { Add tasks }
    taskId := mgr.AddTask('Important Task', 'Description', tpHigh, Date + 7);
    mgr.AddTask('Another Task', 'More work', tpMedium, Date + 14);
    
    { Query tasks }
    stats := query.GetStats();
    WriteLn('Total tasks: ', stats.totalTasks);
    
    { Export data }
    exporter.ExportTasksAsJSON(mgr.GetAllTasks(), 'tasks.json');
    
  finally
    mgr.Free();
    query.Free();
    exporter.Free();
  end;
end.
```

## Type Reference

### Enumerations

**TTaskStatus**
- tsNotStarted
- tsInProgress
- tsCompleted
- tsOnHold

**TTaskPriority**
- tpLow
- tpMedium
- tpHigh
- tpCritical

**TExportFormat**
- efJSON
- efXML
- efCSV
- efHTML

**TReminderType**
- rtEmail
- rtNotification
- rtPopup
- rtSMS

**TReminderTiming**
- rtBefore5Min
- rtBefore15Min
- rtBefore1Hour
- rtBefore1Day
- rtBefore1Week
- rtCustom

### Records

**TTask**
- id: integer
- title: string
- description: string
- priority: TTaskPriority
- status: TTaskStatus
- dueDate: TDateTime
- createdDate: TDateTime
- completedDate: TDateTime
- category: string
- assignee: string
- estimatedHours: double
- tags: array of string

**TTaskStats**
- totalTasks: integer
- completedTasks: integer
- inProgressTasks: integer
- overdueTasks: integer
- completionRate: double

**TSearchCriteria**
- keyword: string
- searchTitle: boolean
- searchDescription: boolean
- statusFilter: TTaskStatus
- priorityFilter: TTaskPriority
- startDate: TDateTime
- endDate: TDateTime
- useDateRange: boolean
- caseSensitive: boolean

**TExportOptions**
- includeNotes: boolean
- includeDependencies: boolean
- includeHistory: boolean
- includeComments: boolean
- compressOutput: boolean
- prettyPrint: boolean

**TReminderStats**
- totalReminders: integer
- pendingReminders: integer
- acknowledgedReminders: integer
- overdueReminders: integer
- emailReminders: integer
- notificationReminders: integer
