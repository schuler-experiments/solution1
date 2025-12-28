
# Task Manager - Comprehensive Free Pascal Task Management System


## Project Overview

A feature-rich, enterprise-grade task management system implemented entirely in Free Pascal (FPC). This project demonstrates advanced software engineering concepts including object-oriented programming, class inheritance, modular design, and complex business logic.

**Project Statistics:**
- **Total Source Code Lines:** 29,431 lines across 47 source files
- **Core Units:** 22 specialized task manager modules
- **Demo Programs:** 22 test/demonstration programs (solution1.pas - solution22.pas)
- **Include Files:** 2 modular include files for intelligence features
- **Programming Language:** Free Pascal (Object Pascal mode)
- **Architecture:** Object-oriented with inheritance hierarchy
- **Design Pattern:** Modular, extensible class-based architecture

**File Breakdown:**
- **Main Modules:** 22 taskmanager*.pas files (22,742 lines)
- **Demo Programs:** 22 solution*.pas files (4,408 lines)
- **Core Base:** 1 taskmanager.pas in src/ folder (987 lines)
- **Include Files:** 2 taskmanagerintelligence_*.inc files (656 lines)
- **Dual Files:** Note that taskmanager.pas exists in both root (899 lines) and src/ (987 lines)


## Complete Module Reference Guide

This task management system includes 22 specialized modules, each providing specific functionality:

| Module | Class Name | Primary Purpose |
|--------|-----------|-----------------|
| taskmanager.pas | TTaskManager | Core task management with CRUD operations, filtering, sorting |
| taskmanagerext.pas | TExtendedTaskManager | Recurring tasks, subtasks, hierarchical relationships |
| taskmanageradvanced.pas | TAdvancedTaskManager | Analytics, pattern detection, predictive insights |
| taskmanagerenhanced.pas | TEnhancedTaskManager | Reminders, audit trails, archiving, attachments |
| taskmanagerteam.pas | TTeamTaskManager | Team collaboration, roles, permissions, analytics |
| taskmanagerboards.pas | TBoardTaskManager | Kanban/Scrum boards, WIP limits, sprint planning |
| taskmanagercomments.pas | TCommentedTaskManager | Comments, discussions, mentions, collaboration |
| taskmanagertimetracking.pas | TTimeTrackingTaskManager | Time tracking, Pomodoro, timesheets, productivity metrics |
| taskmanagerwellbeing.pas | TWellbeingTaskManager | Wellness, stress management, burnout detection, breaks |
| taskmanagerknowledge.pas | TKnowledgeTaskManager | Knowledge base, documentation, article management |
| taskmanagernotifications.pas | TNotificationTaskManager | Multi-channel notifications, escalations, tracking |
| taskmanagertemplates.pas | TTemplateTaskManager | Task templates, instantiation, categories |
| taskmanagergamify.pas | TGamifiedTaskManager | Gamification, points, achievements, leaderboards |
| taskmanagersearch.pas | TSearchTaskManager | Full-text search, advanced filtering, faceting |
| taskmanagerfocus.pas | TFocusTaskManager | Focus sessions, deep work, distraction management |
| taskmanagerrecurring.pas | TRecurringTaskManager | Recurring tasks, scheduling patterns |
| taskmanagermeetings.pas | TMeetingTaskManager | Meeting scheduling, attendees, action items |
| taskmanagerlifestyle.pas | TLifestyleTaskManager | Habit tracking, lifestyle integration |
| taskmanagerresource.pas | TResourceTaskManager | Resource allocation, capacity planning |
| taskmanagersmart.pas | TSmartTaskManager | AI-powered suggestions, smart scheduling |
| taskmanagerintelligence.pas | TIntelligenceTaskManager | Advanced AI, machine learning integration |

Each module builds upon the previous one through class inheritance, creating a powerful, extensible system.

## Quick Start

### Prerequisites

Install Free Pascal Compiler (FPC):

```bash
# Ubuntu/Debian
sudo apt-get install fpc

# macOS
brew install fpc

# Windows
# Download from: https://www.freepascal.org/download.html
```

### Compilation

#### Using the Compilation Script

The project includes a convenient compilation script:

```bash
cd solution1/bin
./compile.sh
```

The `compile.sh` script:
- Automatically navigates to the correct directory
- Compiles solution1.pas with debugging symbols (`-gl`)
- Places the executable in the `bin/` folder
- Reports compilation success or failure

#### Manual Compilation

Compile any demo program manually:

```bash
cd solution1

# Compile core module
fpc taskmanager.pas -O2 -Mobjfpc

# Compile and run a specific demo
fpc solution1.pas -obin/demo1 -O2 -Mobjfpc
./bin/demo1

# Compile with debugging symbols
fpc solution1.pas -gl -obin/demo1 -Mobjfpc
```

**Compiler flags explained:**
- `-Mobjfpc` - Object Pascal mode (required for OOP features)
- `-O2` - Optimization level 2
- `-gl` - Generate debugging information
- `-o<filename>` - Specify output executable name
- `-H+` - Use AnsiStrings (long strings)

## Demo Programs Reference

Each solution file demonstrates specific features and capabilities. Run them in order to learn the system progressively.

| File | Program Name | Modules Used | Features Demonstrated |
|------|-------------|--------------|----------------------|
| **solution1.pas** | TaskManagerDemo | taskmanager | **Core Features**: CRUD operations, categories, tags, priorities, filtering, sorting, basic statistics, CSV export |
| **solution2.pas** | TaskManagerExtendedDemo | taskmanager, taskmanagerext | **Extended Features**: Recurring tasks, subtasks, parent-child relationships, priority scoring, batch operations |
| **solution3.pas** | TaskManagerAdvancedDemo | taskmanager, taskmanagerext, taskmanageradvanced | **Advanced Features**: Analytics, smart suggestions, pattern detection, predictive insights |
| **solution4.pas** | solution4 | taskmanager, taskmanagerext, taskmanageradvanced, taskmanagerenhanced | **Enhanced Features**: Reminders, audit trail, archiving, file attachments, change tracking |
| **solution5.pas** | solution5 | taskmanagerteam (+ dependencies) | **Team Collaboration**: Team members, task assignments, permissions, workload management |
| **solution6.pas** | solution6 | taskmanagerteam, taskmanagergamify | **Gamification**: Points, badges, achievements, leaderboards, productivity rewards |
| **solution7.pas** | solution7 | taskmanagerteam, taskmanagerrecurring | **Recurring Tasks**: Daily/weekly/monthly patterns, recurrence rules, automated task generation |
| **solution8.pas** | TaskManagerResourceDemo | taskmanagerresource | **Resource Management**: Resource allocation, capacity planning, conflict detection |
| **solution9.pas** | TaskManagerIntelligenceDemo | taskmanagerintelligence | **AI Intelligence**: Pattern recognition, predictive analytics, smart scheduling recommendations |
| **solution10.pas** | solution10 | taskmanagersmart | **Smart Features**: Auto-categorization, intelligent prioritization, time estimation |
| **solution11.pas** | solution11 | Multiple modules | **Integration Demo**: Combined features from multiple modules working together |
| **solution12.pas** | solution12 | taskmanagerlifestyle | **Lifestyle Integration**: Health tracking, work-life balance, wellness goals |
| **solution13.pas** | solution13 | taskmanagerlifestyle, taskmanagerwellbeing | **Wellbeing**: Stress management, burnout prevention, mental health support |
| **solution14.pas** | solution14 | taskmanagerfocus | **Focus & Productivity**: Pomodoro technique, distraction blocking, deep work sessions |
| **solution15.pas** | BoardTaskManagerDemo | taskmanagerboards | **Kanban Boards**: Board management, columns, WIP limits, sprint planning, agile workflows |
| **solution16.pas** | SearchEngineDemo | taskmanagersearch | **Search Engine**: Full-text search, filters, advanced queries, search indexing |
| **solution17.pas** | solution17 | taskmanagermeetings | **Meeting Management**: Meeting scheduling, attendees, agendas, minutes, action items |
| **solution18.pas** | solution18 | taskmanagercomments | **Comments & Discussion**: Threaded comments, mentions, notifications, collaboration |
| **solution19.pas** | solution19 | taskmanagertemplates | **Templates**: Task templates, project templates, reusable workflows |
| **solution20.pas** | solution20 | taskmanagernotifications | **Notifications**: Multi-channel alerts, email/SMS, escalation rules, notification preferences |
| **solution21.pas** | TimeTrackingDemo | taskmanagertimetracking | **Time Tracking**: Time logging, timesheets, billable hours, productivity analysis |
| **solution22.pas** | KnowledgeBaseDemo | taskmanagerknowledge | **Knowledge Base**: Documentation, wiki, knowledge articles, search, versioning |

### Running Demo Programs

```bash
# Compile and run in one step
fpc solution1.pas -obin/demo1 && ./bin/demo1

# Compile multiple demos
for i in {1..22}; do
  fpc solution$i.pas -obin/demo$i -O2 -Mobjfpc
done

# Run a specific demo
./bin/demo5  # Team collaboration demo
```

## Architecture Overview

### Class Inheritance Hierarchy

```
TTaskManager (base class - taskmanager.pas)
│
├── TExtendedTaskManager (taskmanagerext.pas)
│   └── TAdvancedTaskManager (taskmanageradvanced.pas)
│       └── TEnhancedTaskManager (taskmanagerenhanced.pas)
│           ├── TTeamTaskManager (taskmanagerteam.pas)
│           │   └── TLifestyleTaskManager (taskmanagerlifestyle.pas)
│           ├── TFocusTaskManager (taskmanagerfocus.pas)
│           └── TGamifiedTaskManager (taskmanagergamify.pas)
│
├── TRecurringTaskManager (taskmanagerrecurring.pas)
├── TResourceTaskManager (taskmanagerresource.pas)
├── TSmartTaskManager (taskmanagersmart.pas)
├── TIntelligenceTaskManager (taskmanagerintelligence.pas)
├── TBoardTaskManager (taskmanagerboards.pas)
├── TNotificationTaskManager (taskmanagernotifications.pas)
├── TSearchTaskManager (taskmanagersearch.pas)
├── TKnowledgeTaskManager (taskmanagerknowledge.pas)
├── TTemplateTaskManager (taskmanagertemplates.pas)
├── TTimeTrackingTaskManager (taskmanagertimetracking.pas)
├── TMeetingTaskManager (taskmanagermeetings.pas)
├── TCommentTaskManager (taskmanagercomments.pas)
└── TWellbeingTaskManager (taskmanagerwellbeing.pas)
```

### Project Structure

```
solution1/
├── bin/
│   └── compile.sh          # Compilation helper script
├── src/
│   └── taskmanager.pas     # Alternative/backup version of core module
├── taskmanager.pas         # Core base class (use this one)
├── taskmanager*.pas        # 20+ feature modules
├── solution*.pas           # 22 demo programs
└── README.md              # This documentation
```

**Note:** There are two `taskmanager.pas` files:
- **Root `taskmanager.pas`** (23,918 bytes) - The main version to use
- **src/taskmanager.pas** (24,654 bytes) - Alternative/development version

For new projects, use the root `taskmanager.pas` as it's the stable release version.

### Module Organization

The system is organized into specialized modules, each providing distinct functionality:

| Module | Unit File | Primary Features | Lines of Code |
|--------|-----------|------------------|---------------|
| **Core** | taskmanager.pas | Basic CRUD, filtering, sorting, categories, tags | 899 |
| **Extended** | taskmanagerext.pas | Recurring tasks, subtasks, priority scoring | 958 |
| **Advanced** | taskmanageradvanced.pas | Batch operations, analytics, smart suggestions | 701 |
| **Enhanced** | taskmanagerenhanced.pas | Reminders, audit trail, archiving, attachments | 1,024 |
| **Team** | taskmanagerteam.pas | Team collaboration, assignments, permissions | 1,051 |
| **Boards** | taskmanagerboards.pas | Kanban boards, sprint planning, agile workflows | 962 |
| **Notifications** | taskmanagernotifications.pas | Multi-channel alerts, templates, escalation | 1,538 |
| **Intelligence** | taskmanagerintelligence.pas | AI insights, pattern detection, predictions | 713 |
| **Knowledge** | taskmanagerknowledge.pas | Knowledge base, documentation, wiki | 1,284 |
| **Lifestyle** | taskmanagerlifestyle.pas | Health integration, work-life balance | 1,410 |
| **Focus** | taskmanagerfocus.pas | Pomodoro, deep work, distraction management | 1,370 |
| **Gamify** | taskmanagergamify.pas | Points, badges, achievements, leaderboards | 844 |
| **Recurring** | taskmanagerrecurring.pas | Recurring task patterns and automation | 1,043 |
| **Resource** | taskmanagerresource.pas | Resource allocation and capacity planning | 1,000 |
| **Smart** | taskmanagersmart.pas | Auto-categorization, intelligent features | 719 |
| **Search** | taskmanagersearch.pas | Full-text search and advanced queries | 942 |
| **Templates** | taskmanagertemplates.pas | Task and project templates | 1,147 |
| **Time Tracking** | taskmanagertimetracking.pas | Time logs, timesheets, billable hours | 1,544 |
| **Meetings** | taskmanagermeetings.pas | Meeting management and scheduling | 1,084 |
| **Comments** | taskmanagercomments.pas | Threaded discussions and collaboration | 1,502 |
| **Wellbeing** | taskmanagerwellbeing.pas | Stress management, burnout prevention | 1,099 |

**Total:** 30,842 lines across 48 source files



## File Structure and Organization

Understanding the project's file organization is crucial for navigation and development.

### Directory Layout

```
solution1/
├── bin/                          # Compiled executables and build scripts
│   └── compile.sh                # Automated compilation script
├── src/                          # Alternative source location
│   └── taskmanager.pas           # Core module (987 lines, newer version)
├── taskmanager*.pas              # Task manager modules (root level)
├── solution*.pas                 # Demo programs (solution1 - solution22)
└── README.md                     # This documentation
```

### Important: Dual taskmanager.pas Files

⚠️ **Note:** There are **two different** `taskmanager.pas` files in this project:

1. **Root level:** `solution1/taskmanager.pas` (899 lines)
2. **Src folder:** `solution1/src/taskmanager.pas` (987 lines)

**Key Differences:**
- The **src/taskmanager.pas** version is newer and more feature-complete (88 more lines)
- When compiling, Free Pascal will use the file in the **current directory first**
- Demo programs (solution*.pas) expect to use the **root level** taskmanager.pas
- For development, consider which version to standardize on

**Recommendation:** 
```bash
# To use the src/ version consistently, compile from within src/
cd solution1/src
fpc ../solution1.pas -obin/demo1

# Or copy src/taskmanager.pas to root (after backing up)
cp src/taskmanager.pas taskmanager.pas.backup
cp src/taskmanager.pas .
```

### Module Files

All task manager modules are in the root `solution1/` directory:

| File Pattern | Purpose | Example |
|--------------|---------|---------|
| `taskmanager.pas` | Core base class | Base TTaskManager |
| `taskmanagerext.pas` | Extended features | TExtendedTaskManager |
| `taskmanager*.pas` | Specialized modules | taskmanagerfocus.pas, taskmanagerteam.pas |
| `taskmanagerintelligence_*.inc` | Include files | Code split for organization |

### Include Files (.inc)

Some modules use include files to organize code:

```pascal
// In taskmanagerintelligence.pas
{$I taskmanagerintelligence_analytics.inc}
{$I taskmanagerintelligence_export.inc}
```

**Include Files:**
- `taskmanagerintelligence_analytics.inc` (304 lines) - Analytics implementation
- `taskmanagerintelligence_export.inc` (352 lines) - Export functionality

**Purpose:** Breaking large modules into logical sections while maintaining single unit compilation.

### Demo Programs (solution*.pas)

22 demonstration programs showcase different features:

**Organization Pattern:**
```pascal
program solution<N>;        // Program declaration
{$mode objfpc}{$H+}        // Compiler directives
uses                        // Module dependencies
  SysUtils, taskmanager, taskmanagerXXX;
var
  Manager: TXXXTaskManager; // Specific manager type
procedure SelfTest;         // Main test procedure
begin
  Manager := TXXXTaskManager.Create;
  try
    Manager.SelfTest;       // Most demos use built-in SelfTest
  finally
    Manager.Free;
  end;
end;
begin
  SelfTest;
end.
```

**Categories:**
- **solution1-5:** Core progression (basic → team)
- **solution6-10:** Specialized features (focus, gamification, lifestyle)
- **solution11-15:** Advanced features (recurring, resources, smart)
- **solution16-20:** Intelligence and boards
- **solution21-22:** Time tracking and knowledge base

**Note:** Solutions 18-20 are minimal (28 lines each) and simply invoke the module's built-in `SelfTest` method.

### The bin/ Folder

**Purpose:** Contains compiled executables and build automation.

**Contents:**
- `compile.sh` - Automated compilation script for solution1.pas
- Compiled executables (after running compile.sh or manual compilation)

**compile.sh Functionality:**
```bash
#!/bin/bash
cd "$(dirname "$0")/.."     # Navigate to solution1/ directory
fpc -gl -o./bin/solution1 ./solution1.pas  # Compile with debug symbols
# Check for errors and report
```

**Usage:**
```bash
cd solution1/bin
./compile.sh               # Compiles solution1.pas → bin/solution1
./solution1                # Run the compiled program
```

### Source Code Organization Strategy

**Why Multiple Files?**
1. **Modularity:** Each feature in its own unit
2. **Inheritance:** Clear class hierarchy through separate files
3. **Reusability:** Mix and match modules as needed
4. **Maintainability:** Easier to locate and modify specific features

**Dependency Pattern:**
```
solution1.pas (demo)
    ↓ uses
taskmanager.pas (base)
    ↓ inherited by
taskmanagerext.pas
    ↓ inherited by
taskmanageradvanced.pas
    ↓ used by
solution1.pas
```

### Working with the Source

**To add a new feature:**

1. Create new unit: `taskmanagermyfeature.pas`
2. Inherit from appropriate base class
3. Add interface and implementation
4. Create demo: `solution23.pas`
5. Update this README

**To modify existing code:**

1. Identify the correct module file
2. Be aware of the dual taskmanager.pas situation
3. Update dependent modules if interfaces change
4. Test with relevant solution*.pas demos
5. Recompile all affected demos



## Core Features

### Basic Task Management (TTaskManager)

The foundation class provides essential task management:

```pascal
uses taskmanager;

var
  TM: TTaskManager;
  TaskID: Integer;

begin
  TM := TTaskManager.Create;
  try
    // Create a task
    TaskID := TM.AddTask(
      'Implement login feature',
      'Create user authentication',
      'Backend',
      tpHigh,
      EncodeDate(2024, 12, 31),
      8.0  // 8 hours estimated
    );
    
    // Update task
    TM.UpdateTask(TaskID, 'Implement OAuth login', '', '', tpHigh, 0, 0);
    
    // Mark complete
    TM.CompleteTask(TaskID);
    
    // Export to CSV
    TM.ExportToCSV('tasks.csv');
  finally
    TM.Free;
  end;
end.
```

**Key Methods:**

```pascal
// CRUD Operations
function AddTask(const ATitle, ADescription, ACategory: string;
                 APriority: TTaskPriority; ADueDate: TDateTime;
                 AEstimatedHours: Double): Integer;
function UpdateTask(AID: Integer; const ATitle, ADescription, 
                   ACategory: string; APriority: TTaskPriority;
                   ADueDate: TDateTime; AEstimatedHours: Double): Boolean;
function DeleteTask(AID: Integer): Boolean;
function CompleteTask(AID: Integer): Boolean;

// Filtering & Retrieval
function GetAllTasks: TTaskArray;
function GetTasksByCategory(const ACategory: string): TTaskArray;
function GetTasksByPriority(APriority: TTaskPriority): TTaskArray;
function GetOverdueTasks: TTaskArray;
function GetUpcomingTasks(ADays: Integer): TTaskArray;

// Sorting
procedure SortTasksByPriority;
procedure SortTasksByDueDate;
procedure SortTasksByTitle;

// Statistics
function GetCompletionRate: Double;
function GetAverageCompletionTime: Double;
function GetCategoryStatistics: string;

// Export
function ExportToCSV(const AFilename: string): Boolean;
procedure SaveToFile(const AFilename: string);
procedure LoadFromFile(const AFilename: string);
```

### Data Types

```pascal
type
  TTaskPriority = (tpLow, tpMedium, tpHigh, tpUrgent);
  
  TTask = record
    ID: Integer;
    Title: string;
    Description: string;
    Category: string;
    Priority: TTaskPriority;
    DueDate: TDateTime;
    EstimatedHours: Double;
    ActualHours: Double;
    CreatedDate: TDateTime;
    CompletedDate: TDateTime;
    IsCompleted: Boolean;
    Tags: array of string;
  end;
  
  TTaskArray = array of TTask;
```


## API Reference

This section provides detailed documentation for the core classes and their methods. The task manager uses an object-oriented architecture with three main classes working together.

### Class Architecture

```
TTaskManager
  └── manages → TTaskList (root tasks)
                  └── contains → TTask (individual tasks)
                                   └── may have → TTaskList (subtasks)
```

### TTask Class

The `TTask` class represents an individual task with properties and methods for task manipulation.

#### Constructor

```pascal
constructor Create(const AName: string; 
                  const ADescription: string; 
                  APriority: integer = 0);
```

**Parameters:**
- `AName`: The task name/title (required)
- `ADescription`: Detailed description of the task (required)
- `APriority`: Priority level as integer (default: 0, higher = more important)

**Example:**
```pascal
var
  Task: TTask;
begin
  Task := TTask.Create('Implement authentication', 
                       'Add OAuth2 login system', 
                       10);
  try
    // Use the task
    WriteLn('Task: ', Task.Name);
    WriteLn('Priority: ', Task.Priority);
  finally
    Task.Free;
  end;
end;
```

#### Properties

| Property | Type | Access | Description |
|----------|------|--------|-------------|
| `Name` | string | Read-only | Task name/title |
| `Description` | string | Read-only | Task description |
| `Priority` | integer | Read/Write | Priority level (higher = more important) |
| `Completed` | boolean | Read/Write | Completion status |
| `Parent` | TTask | Read-only | Parent task (nil if root task) |
| `Subtasks` | TTaskList | Read-only | List of subtasks |

**Example:**
```pascal
Task.Priority := 15;
Task.Completed := True;

if Task.Completed then
  WriteLn('Task "', Task.Name, '" is complete');
```

#### Methods

##### Execute

```pascal
procedure Execute;
```

Executes the task's associated procedure (if set via `SetExecuteProc`).

**Example:**
```pascal
procedure MyTaskAction;
begin
  WriteLn('Executing custom task logic');
end;

Task.SetExecuteProc(@MyTaskAction);
Task.Execute;  // Calls MyTaskAction
```

##### AddSubtask

```pascal
procedure AddSubtask(ATask: TTask);
```

Adds a subtask to this task, creating a parent-child relationship.

**Parameters:**
- `ATask`: The task to add as a subtask

**Example:**
```pascal
var
  ParentTask, SubTask: TTask;
begin
  ParentTask := TTask.Create('Develop feature', 'Main feature', 10);
  SubTask := TTask.Create('Write tests', 'Unit tests', 5);
  
  ParentTask.AddSubtask(SubTask);  // SubTask is now child of ParentTask
  
  // ParentTask now owns SubTask - it will be freed with ParentTask
end;
```

##### RemoveSubtask

```pascal
procedure RemoveSubtask(ATask: TTask);
```

Removes a subtask from this task's subtask list.

**Parameters:**
- `ATask`: The subtask to remove

##### HasSubtasks

```pascal
function HasSubtasks: boolean;
```

Returns `True` if the task has any subtasks.

**Example:**
```pascal
if Task.HasSubtasks then
  WriteLn('This task has ', Task.Subtasks.Count, ' subtasks');
```

##### GetSubtasks

```pascal
function GetSubtasks: TTaskList;
```

Returns the list of subtasks. Same as accessing the `Subtasks` property.

---

### TTaskList Class

The `TTaskList` class manages a collection of tasks with filtering, sorting, and execution capabilities.

#### Constructor

```pascal
constructor Create(AOwner: TTask = nil);
```

**Parameters:**
- `AOwner`: Optional owner task (for subtask lists). Pass `nil` for independent lists.

**Example:**
```pascal
var
  TaskList: TTaskList;
begin
  TaskList := TTaskList.Create(nil);
  try
    // Use the task list
  finally
    TaskList.Free;
  end;
end;
```

#### Methods

##### Add

```pascal
procedure Add(ATask: TTask);
```

Adds a task to the list.

**Example:**
```pascal
var
  TaskList: TTaskList;
  Task1, Task2: TTask;
begin
  TaskList := TTaskList.Create(nil);
  Task1 := TTask.Create('Task 1', 'Description 1', 5);
  Task2 := TTask.Create('Task 2', 'Description 2', 10);
  
  TaskList.Add(Task1);
  TaskList.Add(Task2);
  
  WriteLn('Task count: ', TaskList.Count);  // Output: 2
end;
```

##### Remove

```pascal
procedure Remove(ATask: TTask);
```

Removes a task from the list (does not free the task object).

##### Clear

```pascal
procedure Clear;
```

Removes all tasks from the list and frees them.

**Warning:** Only call this if the list owns the tasks.

##### Count

```pascal
function Count: integer;
```

Returns the number of tasks in the list.

##### GetTask

```pascal
function GetTask(Index: integer): TTask;
```

Returns the task at the specified index (0-based).

**Example:**
```pascal
var
  i: integer;
  Task: TTask;
begin
  for i := 0 to TaskList.Count - 1 do
  begin
    Task := TaskList.GetTask(i);
    WriteLn(i, ': ', Task.Name);
  end;
end;
```

##### GetTaskByName

```pascal
function GetTaskByName(const AName: string): TTask;
```

Finds and returns the first task with the specified name. Returns `nil` if not found.

**Example:**
```pascal
var
  Task: TTask;
begin
  Task := TaskList.GetTaskByName('Implement authentication');
  if Task <> nil then
    WriteLn('Found: ', Task.Description)
  else
    WriteLn('Task not found');
end;
```

##### GetTaskByPriority

```pascal
function GetTaskByPriority(APriority: integer): TTask;
```

Finds and returns the first task with the specified priority. Returns `nil` if not found.

##### ExecuteAll

```pascal
procedure ExecuteAll;
```

Executes all tasks in the list that have an execution procedure set.

##### ExecuteCompletedTasks

```pascal
procedure ExecuteCompletedTasks;
```

Executes only tasks marked as completed.

##### ExecuteIncompleteTasks

```pascal
procedure ExecuteIncompleteTasks;
```

Executes only tasks not marked as completed.

##### ExecuteHighPriorityTasks

```pascal
procedure ExecuteHighPriorityTasks;
```

Executes tasks with priority ≥ 5.

##### ExecuteLowPriorityTasks

```pascal
procedure ExecuteLowPriorityTasks;
```

Executes tasks with priority < 5.

---

### TTaskManager Class

The `TTaskManager` class is the main interface for managing tasks, providing high-level operations.

#### Constructor

```pascal
constructor Create;
```

Creates a new task manager instance.

**Example:**
```pascal
var
  Manager: TTaskManager;
begin
  Manager := TTaskManager.Create;
  try
    // Use the manager
  finally
    Manager.Free;
  end;
end;
```

#### Task Management Methods

##### AddTask

```pascal
procedure AddTask(ATask: TTask);
```

Adds a root-level task to the manager.

**Example:**
```pascal
var
  Manager: TTaskManager;
  Task: TTask;
begin
  Manager := TTaskManager.Create;
  Task := TTask.Create('New feature', 'Implement new feature', 10);
  
  Manager.AddTask(Task);
  // Manager now owns Task - it will be freed with Manager
end;
```

##### RemoveTask

```pascal
procedure RemoveTask(ATask: TTask);
```

Removes a root-level task from the manager.

##### Clear

```pascal
procedure Clear;
```

Removes and frees all tasks from the manager.

##### Count

```pascal
function Count: integer;
```

Returns the total number of root-level tasks.

##### GetTask

```pascal
function GetTask(Index: integer): TTask;
```

Returns the root-level task at the specified index.

##### GetTaskByName

```pascal
function GetTaskByName(const AName: string): TTask;
```

Finds a task by name (searches root tasks only).

##### GetTaskByPriority

```pascal
function GetTaskByPriority(APriority: integer): TTask;
```

Finds the first task with the specified priority (searches root tasks only).

#### Execution Methods

##### ExecuteAllTasks

```pascal
procedure ExecuteAllTasks;
```

Executes all root-level tasks.

##### ExecuteTaskByName

```pascal
procedure ExecuteTaskByName(const AName: string);
```

Finds and executes a specific task by name.

**Example:**
```pascal
Manager.ExecuteTaskByName('Backup database');
```

##### ExecuteTaskByPriority

```pascal
procedure ExecuteTaskByPriority(APriority: integer);
```

Executes the first task found with the specified priority.

##### ExecuteHighPriorityTasks

```pascal
procedure ExecuteHighPriorityTasks;
```

Executes all high-priority tasks (priority ≥ 5).

##### ExecuteLowPriorityTasks

```pascal
procedure ExecuteLowPriorityTasks;
```

Executes all low-priority tasks (priority < 5).

##### ExecuteCompletedTasks

```pascal
procedure ExecuteCompletedTasks;
```

Executes all completed tasks.

##### ExecuteIncompleteTasks

```pascal
procedure ExecuteIncompleteTasks;
```

Executes all incomplete tasks.

##### ExecuteTaskRecursively

```pascal
procedure ExecuteTaskRecursively(ATask: TTask);
```

Executes a task and all its subtasks recursively.

**Example:**
```pascal
var
  Task: TTask;
begin
  Task := Manager.GetTaskByName('Project A');
  if Task <> nil then
    Manager.ExecuteTaskRecursively(Task);  // Executes task and all subtasks
end;
```

##### ExecuteTaskList

```pascal
procedure ExecuteTaskList(ATaskList: TTaskList);
```

Executes all tasks in the provided task list.

#### Search and Filter Methods

##### FindTaskByName

```pascal
function FindTaskByName(const AName: string): TTask;
```

Searches all tasks (including subtasks) for a task with the specified name.

**Example:**
```pascal
var
  Task: TTask;
begin
  Task := Manager.FindTaskByName('Write documentation');
  if Task <> nil then
  begin
    WriteLn('Found task: ', Task.Description);
    WriteLn('Parent: ', Task.Parent.Name);
  end;
end;
```

##### FindTaskByDescription

```pascal
function FindTaskByDescription(const ADescription: string): TTask;
```

Searches all tasks for one containing the specified description text.

##### SearchTasks

```pascal
function SearchTasks(const AKeyword: string): TTaskList;
```

Searches for tasks containing the keyword in name or description. Returns a new `TTaskList` with matching tasks.

**Example:**
```pascal
var
  Results: TTaskList;
  i: integer;
begin
  Results := Manager.SearchTasks('authentication');
  try
    WriteLn('Found ', Results.Count, ' tasks:');
    for i := 0 to Results.Count - 1 do
      WriteLn('  - ', Results.GetTask(i).Name);
  finally
    Results.Free;
  end;
end;
```

##### GetCompletedTasks

```pascal
function GetCompletedTasks: TTaskList;
```

Returns a new list containing only completed tasks.

**Note:** You must free the returned list when done.

##### GetIncompleteTasks

```pascal
function GetIncompleteTasks: TTaskList;
```

Returns a new list containing only incomplete tasks.

##### GetHighPriorityTasks

```pascal
function GetHighPriorityTasks: TTaskList;
```

Returns a new list containing only high-priority tasks (priority ≥ 5).

##### GetLowPriorityTasks

```pascal
function GetLowPriorityTasks: TTaskList;
```

Returns a new list containing only low-priority tasks (priority < 5).

##### GetTasksByPriority

```pascal
function GetTasksByPriority(APriority: integer): TTaskList;
```

Returns a new list containing only tasks with the exact priority specified.

##### GetLeafTasks

```pascal
function GetLeafTasks: TTaskList;
```

Returns a new list containing only leaf tasks (tasks with no subtasks).

##### GetRootTasks

```pascal
function GetRootTasks: TTaskList;
```

Returns the internal list of root-level tasks.

**Warning:** Do not free this list as it's owned by the manager.

#### Filtering and Sorting Methods

##### FilterTasksByPriority

```pascal
procedure FilterTasksByPriority(ATaskList: TTaskList; APriority: integer);
```

Filters the provided task list in-place, removing tasks that don't match the priority.

**Example:**
```pascal
var
  Tasks: TTaskList;
begin
  Tasks := Manager.GetRootTasks;
  Manager.FilterTasksByPriority(Tasks, 10);
  // Tasks now contains only priority-10 tasks
end;
```

##### FilterTasksByCompletion

```pascal
procedure FilterTasksByCompletion(ATaskList: TTaskList; ACompleted: boolean);
```

Filters the provided task list in-place, keeping only tasks matching the completion status.

##### SortTasksByPriority

```pascal
procedure SortTasksByPriority(ATaskList: TTaskList);
```

Sorts the provided task list by priority (highest first).

**Example:**
```pascal
var
  Tasks: TTaskList;
  i: integer;
begin
  Tasks := TTaskList.Create(nil);
  try
    // Add tasks...
    Manager.SortTasksByPriority(Tasks);
    
    for i := 0 to Tasks.Count - 1 do
      WriteLn(Tasks.GetTask(i).Priority, ': ', Tasks.GetTask(i).Name);
  finally
    Tasks.Free;
  end;
end;
```

##### SortTasksByName

```pascal
procedure SortTasksByName(ATaskList: TTaskList);
```

Sorts the provided task list alphabetically by name.

#### Properties

##### CurrentTask

```pascal
property CurrentTask: TTask read FCurrentTask write FCurrentTask;
```

Gets or sets the currently active task.

**Example:**
```pascal
Manager.CurrentTask := Manager.GetTaskByName('Active feature');
if Manager.CurrentTask <> nil then
  WriteLn('Working on: ', Manager.CurrentTask.Name);
```

##### RootTasks

```pascal
property RootTasks: TTaskList read GetRootTasks;
```

Returns the list of root-level tasks (same as calling `GetRootTasks`).

---

### Complete Usage Example

Here's a comprehensive example demonstrating the core API:

```pascal
program TaskManagerExample;

{$mode objfpc}{$H+}

uses
  SysUtils, taskmanager;

var
  Manager: TTaskManager;
  ProjectTask, DesignTask, CodeTask, TestTask: TTask;
  HighPriorityTasks, SearchResults: TTaskList;
  i: integer;

begin
  // Create the task manager
  Manager := TTaskManager.Create;
  try
    WriteLn('=== Task Manager Example ===');
    WriteLn;
    
    // Create a project with subtasks
    ProjectTask := TTask.Create('Website Redesign', 
                                'Complete website overhaul', 
                                10);
    
    DesignTask := TTask.Create('Design mockups', 
                              'Create UI/UX designs', 
                              8);
    CodeTask := TTask.Create('Implement design', 
                            'Code the new design', 
                            9);
    TestTask := TTask.Create('Testing', 
                            'QA and user testing', 
                            7);
    
    // Build the hierarchy
    ProjectTask.AddSubtask(DesignTask);
    ProjectTask.AddSubtask(CodeTask);
    ProjectTask.AddSubtask(TestTask);
    
    // Add to manager
    Manager.AddTask(ProjectTask);
    
    // Add more root tasks
    Manager.AddTask(TTask.Create('Fix bug #123', 
                                 'Memory leak in payment module', 
                                 15));
    Manager.AddTask(TTask.Create('Update documentation', 
                                 'Document new API endpoints', 
                                 5));
    
    WriteLn('Total root tasks: ', Manager.Count);
    WriteLn;
    
    // Mark some tasks complete
    DesignTask.Completed := True;
    WriteLn('Marked "', DesignTask.Name, '" as completed');
    WriteLn;
    
    // Search for tasks
    SearchResults := Manager.SearchTasks('design');
    try
      WriteLn('Tasks containing "design": ', SearchResults.Count);
      for i := 0 to SearchResults.Count - 1 do
        WriteLn('  - ', SearchResults.GetTask(i).Name);
      WriteLn;
    finally
      SearchResults.Free;
    end;
    
    // Get high priority tasks
    HighPriorityTasks := Manager.GetHighPriorityTasks;
    try
      WriteLn('High priority tasks:');
      Manager.SortTasksByPriority(HighPriorityTasks);
      for i := 0 to HighPriorityTasks.Count - 1 do
        WriteLn('  Priority ', HighPriorityTasks.GetTask(i).Priority, 
                ': ', HighPriorityTasks.GetTask(i).Name);
      WriteLn;
    finally
      HighPriorityTasks.Free;
    end;
    
    // Execute incomplete high-priority tasks
    WriteLn('Executing incomplete high-priority tasks...');
    Manager.ExecuteHighPriorityTasks;
    
    WriteLn;
    WriteLn('=== Example completed ===');
    
  finally
    Manager.Free;  // Frees all tasks automatically
  end;
end.
```

### Memory Management Best Practices

**Important Guidelines:**

1. **TTaskManager owns root tasks**: When you add a task to the manager, it takes ownership and will free it when the manager is destroyed.

2. **Parent tasks own subtasks**: When you add a subtask to a task, the parent takes ownership.

3. **Returned TTaskList objects**: Methods like `GetCompletedTasks()`, `SearchTasks()`, etc. return NEW `TTaskList` objects that you must free.

4. **Returned TTask references**: Methods like `FindTaskByName()` return references to existing tasks - do NOT free these.

**Safe Pattern:**
```pascal
var
  Manager: TTaskManager;
  Task: TTask;
  TaskList: TTaskList;
begin
  Manager := TTaskManager.Create;
  try
    // Create and add task - Manager owns it now
    Task := TTask.Create('My task', 'Description', 5);
    Manager.AddTask(Task);  // Manager takes ownership
    
    // Get a reference - don't free this
    Task := Manager.FindTaskByName('My task');
    if Task <> nil then
      Task.Priority := 10;  // OK to modify
    
    // Get a new list - must free this
    TaskList := Manager.GetCompletedTasks;
    try
      // Use TaskList
    finally
      TaskList.Free;  // Must free it
    end;
    
  finally
    Manager.Free;  // Frees all owned tasks automatically
  end;
end;
```


## Extended Features (TExtendedTaskManager)

Adds recurring tasks, subtasks, and advanced operations:

```pascal
uses taskmanager, taskmanagerext;

var
  Manager: TExtendedTaskManager;
  ParentID, ChildID: Integer;

begin
  Manager := TExtendedTaskManager.Create;
  try
    // Create recurring task (daily)
    ParentID := Manager.AddRecurringTask(
      'Daily standup',
      'Team sync meeting',
      rtDaily,
      EncodeDate(2024, 1, 1),
      EncodeDate(2024, 12, 31)
    );
    
    // Add subtask
    ChildID := Manager.AddSubtask(ParentID, 'Prepare agenda', 'List topics');
    
    // Get all subtasks
    Subtasks := Manager.GetSubtasks(ParentID);
    
    // Priority scoring
    Score := Manager.CalculatePriorityScore(ParentID);
  finally
    Manager.Free;
  end;
end.
```

**Recurring Task Types:**

```pascal
type
  TRecurrenceType = (rtDaily, rtWeekly, rtMonthly, rtYearly, rtCustom);
```

**Key Methods:**

```pascal
// Recurring Tasks
function AddRecurringTask(const ATitle, ADescription: string;
                         ARecurrence: TRecurrenceType;
                         AStartDate, AEndDate: TDateTime): Integer;
function GenerateRecurringInstances(ATaskID: Integer): Integer;

// Subtasks
function AddSubtask(AParentID: Integer; const ATitle, ADescription: string): Integer;
function GetSubtasks(AParentID: Integer): TExtendedTaskArray;
function GetParentTask(ATaskID: Integer): TExtendedTask;

// Priority Scoring
function CalculatePriorityScore(ATaskID: Integer): Double;
function GetTopPriorityTasks(ACount: Integer): TExtendedTaskArray;

// Batch Operations
type
  TBatchOperationResult = record
    SuccessCount: Integer;
    FailureCount: Integer;
    Errors: array of string;
  end;

function BatchUpdateCategory(const ATaskIDs: array of Integer;
                            const ANewCategory: string): TBatchOperationResult;
function BatchComplete(const ATaskIDs: array of Integer): TBatchOperationResult;
function BatchDelete(const ATaskIDs: array of Integer): TBatchOperationResult;
```

## Advanced Analytics (TAdvancedTaskManager)

Provides business intelligence and insights:

```pascal
uses taskmanager, taskmanagerext, taskmanageradvanced;

var
  Manager: TAdvancedTaskManager;
  Suggestions: TTaskSuggestionArray;
  Report: TProductivityReport;

begin
  Manager := TAdvancedTaskManager.Create;
  try
    // Get smart suggestions
    Suggestions := Manager.GetSmartSuggestions;
    
    // Generate productivity report
    Report := Manager.GetProductivityReport(
      EncodeDate(2024, 1, 1),
      EncodeDate(2024, 12, 31)
    );
    
    WriteLn('Tasks Completed: ', Report.TasksCompleted);
    WriteLn('Completion Rate: ', Report.CompletionRate:0:2, '%');
    WriteLn('Average Time: ', Report.AverageCompletionTime:0:2, ' hours');
  finally
    Manager.Free;
  end;
end.
```

**Analytics Types:**

```pascal
type
  TProductivityReport = record
    TasksCompleted: Integer;
    TasksCreated: Integer;
    CompletionRate: Double;
    AverageCompletionTime: Double;
    TotalHoursLogged: Double;
    ProductivityScore: Double;
  end;
  
  TTaskSuggestion = record
    TaskID: Integer;
    SuggestionType: string;
    Reason: string;
    Priority: Integer;
  end;
```

**Key Methods:**

```pascal
// Analytics
function GetProductivityReport(AStartDate, AEndDate: TDateTime): TProductivityReport;
function GetCategoryAnalytics: TCategoryAnalyticsArray;
function GetTimelineAnalysis: TTimelineAnalysisArray;

// Smart Suggestions
function GetSmartSuggestions: TTaskSuggestionArray;
function GetTasksNeedingAttention: TExtendedTaskArray;
function PredictCompletionDate(ATaskID: Integer): TDateTime;

// Pattern Detection
function DetectProductivityPatterns: TProductivityPatternArray;
function GetOptimalWorkTimes: TTimeRangeArray;
```

## Enhanced Features (TEnhancedTaskManager)

Adds reminders, audit trails, archiving, and attachments:

```pascal
uses taskmanagerenhanced;

var
  Manager: TEnhancedTaskManager;
  ReminderID, AttachmentID: Integer;

begin
  Manager := TEnhancedTaskManager.Create;
  try
    // Add reminder
    ReminderID := Manager.AddReminder(
      TaskID,
      EncodeDate(2024, 12, 25) + EncodeTime(9, 0, 0, 0),
      'Start working on this task'
    );
    
    // Attach file
    AttachmentID := Manager.AttachFile(
      TaskID,
      '/path/to/document.pdf',
      'Requirements document'
    );
    
    // Get audit log
    AuditLog := Manager.GetAuditLog(TaskID);
    
    // Archive old tasks
    ArchivedCount := Manager.ArchiveOldTasks(90); // 90 days old
  finally
    Manager.Free;
  end;
end.
```

**Key Methods:**

```pascal
// Reminders
function AddReminder(ATaskID: Integer; AReminderTime: TDateTime;
                    const AMessage: string): Integer;
function GetUpcomingReminders(AHours: Integer): TReminderArray;
function DismissReminder(AReminderID: Integer): Boolean;

// Audit Trail
function GetAuditLog(ATaskID: Integer): TAuditEntryArray;
function GetAllAuditLogs: TAuditEntryArray;
function GetAuditLogByUser(const AUserName: string): TAuditEntryArray;
function GetAuditLogByDateRange(AStart, AEnd: TDateTime): TAuditEntryArray;

// Task Archiving
function ArchiveTask(ATaskID: Integer): Boolean;
function ArchiveOldTasks(ADaysOld: Integer): Integer;
function RestoreTask(ATaskID: Integer): Boolean;
function GetArchivedTasks: TTaskArray;

// File Attachments
function AttachFile(ATaskID: Integer; const AFilePath, ADescription: string): Integer;
function GetAttachments(ATaskID: Integer): TAttachmentArray;
function RemoveAttachment(AAttachmentID: Integer): Boolean;
```

## Team Collaboration (TTeamTaskManager)

Enables multi-user collaboration:

```pascal
uses taskmanagerteam;

var
  Manager: TTeamTaskManager;
  MemberID: Integer;

begin
  Manager := TTeamTaskManager.Create;
  try
    // Add team member
    MemberID := Manager.AddTeamMember(
      'John Doe',
      'john@example.com',
      'Developer'
    );
    
    // Assign task
    Manager.AssignTask(TaskID, MemberID);
    
    // Set permissions
    Manager.SetTaskPermission(TaskID, MemberID, plEdit);
    
    // Get workload
    Workload := Manager.GetMemberWorkload(MemberID);
  finally
    Manager.Free;
  end;
end.
```

**Permission Levels:**

```pascal
type
  TPermissionLevel = (plNone, plView, plEdit, plAdmin);
```

**Key Methods:**

```pascal
// User Management
function AddTeamMember(const AName, AEmail, ARole: string): Integer;
function GetAllTeamMembers: TTeamMemberArray;
function UpdateTeamMember(AMemberID: Integer; const AName, AEmail, ARole: string): Boolean;
function RemoveTeamMember(AMemberID: Integer): Boolean;

// Task Assignment
function AssignTask(ATaskID, AMemberID: Integer): Boolean;
function UnassignTask(ATaskID: Integer): Boolean;
function GetAssignedTasks(AMemberID: Integer): TTaskArray;
function GetUnassignedTasks: TTaskArray;

// Permissions & Access Control
function SetTaskPermission(ATaskID, AMemberID: Integer; ALevel: TPermissionLevel): Boolean;
function GetTaskPermission(ATaskID, AMemberID: Integer): TPermissionLevel;
function CanUserModifyTask(ATaskID, AMemberID: Integer): Boolean;

// Team Analytics
function GetTeamProductivity: TTeamProductivityReport;
function GetMemberWorkload(AMemberID: Integer): TWorkloadReport;
function GetTeamCapacity: TCapacityReport;
```

## Kanban Boards (TBoardTaskManager)

Implements agile board management:

```pascal
uses taskmanagerboards;

var
  Manager: TBoardTaskManager;
  BoardID, ColumnID: Integer;

begin
  Manager := TBoardTaskManager.Create;
  try
    // Create Kanban board
    BoardID := Manager.CreateBoard('Development Sprint', 'Q1 2024', btKanban);
    
    // Add columns
    ColumnID := Manager.AddColumn(BoardID, 'To Do', ctToDo, 5);  // WIP limit: 5
    
    // Move task to column
    Manager.MoveTaskToColumn(TaskID, ColumnID);
    
    // Check WIP limit
    if Manager.IsColumnAtWIPLimit(ColumnID) then
      WriteLn('Column is at WIP limit!');
  finally
    Manager.Free;
  end;
end.
```

**Board Types:**

```pascal
type
  TBoardType = (btKanban, btScrum, btCustom);
  TColumnType = (ctBacklog, ctToDo, ctInProgress, ctReview, ctDone, ctCustom);
```

**Key Methods:**

```pascal
// Board Management
function CreateBoard(const AName, ADescription: string; ABoardType: TBoardType): Integer;
function GetAllBoards: TBoardArray;
function UpdateBoard(ABoardID: Integer; const AName, ADescription: string): Boolean;
function ArchiveBoard(ABoardID: Integer): Boolean;

// Column Management
function AddColumn(ABoardID: Integer; const AName: string;
                  AColumnType: TColumnType; AWIPLimit: Integer): Integer;
function UpdateColumn(AColumnID: Integer; const AName: string;
                     AWIPLimit: Integer): Boolean;
function MoveColumn(AColumnID: Integer; ANewPosition: Integer): Boolean;
function DeleteColumn(AColumnID: Integer): Boolean;

// Task Card Management
function MoveTaskToColumn(ATaskID, AColumnID: Integer): Boolean;
function GetTasksInColumn(AColumnID: Integer): TTaskArray;
function GetTaskColumn(ATaskID: Integer): Integer;

// WIP Limits
function IsColumnAtWIPLimit(AColumnID: Integer): Boolean;
function GetColumnWIPStatus(AColumnID: Integer): TWIPStatus;

// Sprint Planning
function CreateSprint(ABoardID: Integer; const AName: string;
                     AStartDate, AEndDate: TDateTime): Integer;
function GetActiveSprints(ABoardID: Integer): TSprintArray;
function GetSprintVelocity(ASprintID: Integer): Double;
```

## Additional Modules

### Search Engine (TSearchTaskManager)

```pascal
// Full-text search
Results := SearchManager.SearchTasks('login authentication');

// Advanced filtering
Results := SearchManager.SearchWithFilters(
  'feature',
  tpHigh,
  'Backend',
  EncodeDate(2024, 1, 1),
  EncodeDate(2024, 12, 31)
);
```

### Time Tracking (TTimeTrackingTaskManager)

```pascal
// Start time tracking
SessionID := TimeManager.StartTimeTracking(TaskID, 'Working on implementation');

// Stop tracking
TimeManager.StopTimeTracking(SessionID);

// Get timesheet
Timesheet := TimeManager.GetTimesheet(
  MemberID,
  EncodeDate(2024, 12, 1),
  EncodeDate(2024, 12, 31)
);
```

### Knowledge Base (TKnowledgeTaskManager)

```pascal
// Create article
ArticleID := KnowledgeManager.CreateArticle(
  'How to Deploy',
  'Deployment guide...',
  'Documentation'
);

// Link to task
KnowledgeManager.LinkArticleToTask(ArticleID, TaskID);

// Search knowledge base
Articles := KnowledgeManager.SearchArticles('deployment');
```

### Notifications (TNotificationTaskManager)

```pascal
// Send notification
NotificationManager.SendNotification(
  TaskID,
  [MemberID1, MemberID2],
  'Task assigned to you',
  ntEmail
);

// Configure escalation
NotificationManager.SetEscalationRule(
  TaskID,
  24,  // hours
  ManagerID
);
```

### Templates (TTemplateTaskManager)

```pascal
// Create template
TemplateID := TemplateManager.CreateTaskTemplate(
  'Bug Fix Template',
  'Standard bug fixing workflow'
);

// Add template steps
TemplateManager.AddTemplateStep(TemplateID, 'Reproduce bug', 1);
TemplateManager.AddTemplateStep(TemplateID, 'Fix code', 2);
TemplateManager.AddTemplateStep(TemplateID, 'Test fix', 3);

// Create task from template
TaskID := TemplateManager.CreateTaskFromTemplate(TemplateID);
```


### Gamification (TGamifiedTaskManager)

**Module:** `taskmanagergamify.pas`  
**Inherits from:** TTeamTaskManager  
**Key Features:** Points system, achievements, levels, leaderboards, productivity metrics

Transform task management into an engaging game with the Gamification module. This system motivates users through points, achievements, levels, and streaks.

```pascal
// Award points for task completion
Points := GamificationManager.CompleteTaskWithRewards(TaskID);
WriteLn('Earned ', Points, ' points!');

// Check achievements
Achievements := GamificationManager.GetUnlockedAchievements;
for Achievement in Achievements do
  WriteLn('Achievement: ', Achievement.Title);

// Get user level and progress
UserLevel := GamificationManager.GetCurrentLevel;
WriteLn('Level: ', UserLevel.Level, ' - ', UserLevel.Title);
WriteLn('XP: ', UserLevel.CurrentXP, '/', UserLevel.XPForNextLevel);

// View productivity metrics
Metrics := GamificationManager.GetProductivityMetrics;
WriteLn('Completion Rate: ', Metrics.ProductivityScore:0:1, '%');
WriteLn('Current Streak: ', Metrics.CurrentStreak, ' days');

// Get motivational message
Message := GamificationManager.GetMotivationalMessage;
WriteLn(Message);
```

**Key Features:**

1. **Achievement System** (16 types):
   - First Task, 10/50/100 Tasks Completed
   - Perfect Week, Early Bird, Speed Demon
   - Marathoner (4+ hour session), Streaks (7/30 days)
   - Team Player, Multitasker, Priority Master
   - Organizer, Mentor, Time Wizard

2. **Points System**:
   - Points for task completion
   - Bonus points for high priority and early completion
   - Streak multipliers
   - Session and note bonuses

3. **Levels and Experience**:
   - User levels based on accumulated XP
   - Level titles (Beginner → Master)
   - Dynamic XP requirements per level

4. **Productivity Metrics**:
   - Daily/weekly/monthly completion tracking
   - Streak monitoring (consecutive work days)
   - Productivity score (0-100)
   - Focus score based on work sessions
   - Velocity trends

5. **Daily Activity Tracking**:
   - Tasks completed per day
   - Hours worked
   - Points earned
   - Productivity threshold monitoring

6. **Leaderboard Features**:
   - Personal bests tracking
   - Achievement progress visualization
   - Activity calendar view
   - Streak information

**Main Methods:**

- `CompleteTaskWithRewards(TaskID)` - Complete task and earn rewards
- `GetAllAchievements` - Retrieve all achievements with progress
- `GetCurrentLevel` - Get current user level info
- `GetProductivityMetrics` - Full productivity statistics
- `GetMotivationalMessage` - Personalized motivation based on performance
- `AwardPoints(Points)` - Manually award bonus points


### AI Intelligence & Analytics (TIntelligenceTaskManager)

**Module:** `taskmanagerintelligence_final.pas`  
**Inherits from:** TResourceTaskManager  
**Key Features:** Natural Language Processing, Advanced Analytics, Backup/Versioning, Bulk Operations, Smart Notifications, Multi-format Export

The Intelligence Task Manager provides enterprise-grade features for advanced task management, including AI-powered natural language processing, comprehensive analytics, automated backups, and bulk operations.

#### Natural Language Processing (NLP)

Create tasks using natural language input:

```pascal
uses taskmanagerintelligence;

var
  Manager: TIntelligenceTaskManager;
  ParsedTask: TParsedTask;
  TaskID: Integer;
  TaskIDs: TIntArray;
begin
  Manager := TIntelligenceTaskManager.Create;
  try
    // Parse natural language to task structure
    ParsedTask := Manager.ParseNaturalLanguageTask(
      'Create high priority feature for user authentication due next Friday'
    );
    
    WriteLn('Parsed: ', ParsedTask.Title);
    WriteLn('Priority: ', Ord(ParsedTask.Priority));
    WriteLn('Confidence: ', ParsedTask.Confidence:0:2);
    
    // Create task directly from natural language
    TaskID := Manager.CreateTaskFromNL(
      'Fix login bug high priority due tomorrow'
    );
    
    // Bulk create from natural language
    TaskIDs := Manager.BulkCreateFromNL([
      'Review pull request for authentication',
      'Update documentation for API endpoints',
      'Test deployment pipeline critical'
    ]);
    
    WriteLn('Created ', Length(TaskIDs), ' tasks from natural language');
  finally
    Manager.Free;
  end;
end.
```

**NLP Types:**

```pascal
type
  TNLPToken = record
    TokenType: string;      // 'action', 'priority', 'date', 'category'
    Value: string;          // Extracted value
    Confidence: Double;     // 0.0 to 1.0
  end;
  
  TParsedTask = record
    Title: string;
    Description: string;
    Category: string;
    Priority: TTaskPriority;
    DueDate: TDateTime;
    EstimatedHours: Double;
    Tags: array of string;
    Confidence: Double;         // Overall parsing confidence
    ParsedSuccessfully: Boolean;
  end;
```

#### Backup & Versioning

Comprehensive backup and restore capabilities:

```pascal
var
  Manager: TIntelligenceTaskManager;
  VersionID, PointID: Integer;
  Versions: TBackupVersionArray;
  RestorePoints: TRestorePointArray;
begin
  Manager := TIntelligenceTaskManager.Create;
  try
    // Create manual backup
    VersionID := Manager.CreateBackupVersion('Before major changes');
    WriteLn('Backup created: Version ', VersionID);
    
    // Create restore point
    PointID := Manager.CreateRestorePoint('Project milestone reached');
    
    // Enable automatic backups every 24 hours
    Manager.EnableAutoBackup(True, 24);
    
    // ... make changes ...
    
    // Restore from version
    if Manager.RestoreFromVersion(VersionID) then
      WriteLn('Successfully restored from version ', VersionID);
    
    // Restore to restore point
    if Manager.RestoreToPoint(PointID) then
      WriteLn('Successfully restored to restore point');
    
    // List all backups
    Versions := Manager.GetBackupVersions;
    WriteLn('Available backups: ', Length(Versions));
    for i := 0 to High(Versions) do
      WriteLn('  Version ', Versions[i].VersionID, ': ', 
              Versions[i].Description, ' (', 
              DateTimeToStr(Versions[i].Timestamp), ')');
    
    // Delete old backup
    Manager.DeleteBackupVersion(VersionID);
  finally
    Manager.Free;
  end;
end.
```

**Backup Types:**

```pascal
type
  TBackupVersion = record
    VersionID: Integer;
    Timestamp: TDateTime;
    Description: string;
    FilePath: string;
    FileSize: Int64;
    TaskCount: Integer;
    Checksum: string;       // Integrity verification
  end;
  
  TRestorePoint = record
    PointID: Integer;
    Created: TDateTime;
    Label_: string;
    AutoCreated: Boolean;   // Auto vs manual
    DataSnapshot: string;
  end;
```

#### Bulk Operations

Perform operations on multiple tasks efficiently:

```pascal
var
  Manager: TIntelligenceTaskManager;
  TaskIDs: array of Integer;
  UpdatedCount: Integer;
  History: TBulkOperationArray;
begin
  Manager := TIntelligenceTaskManager.Create;
  try
    // Prepare task IDs
    SetLength(TaskIDs, 3);
    TaskIDs[0] := 1;
    TaskIDs[1] := 2;
    TaskIDs[2] := 3;
    
    // Bulk update status
    UpdatedCount := Manager.BulkUpdateStatus(TaskIDs, tsInProgress);
    WriteLn('Updated status for ', UpdatedCount, ' tasks');
    
    // Bulk update priority
    UpdatedCount := Manager.BulkUpdatePriority(TaskIDs, tpHigh);
    
    // Bulk update category
    UpdatedCount := Manager.BulkUpdateCategory(TaskIDs, 'Sprint 1');
    
    // Bulk add tag
    UpdatedCount := Manager.BulkAddTag(TaskIDs, 'urgent');
    
    // Bulk archive with reason
    UpdatedCount := Manager.BulkArchive(TaskIDs, 'Sprint completed');
    WriteLn('Archived ', UpdatedCount, ' tasks');
    
    // View bulk operation history
    History := Manager.GetBulkOperationHistory;
    WriteLn('Total bulk operations: ', Length(History));
    for i := 0 to High(History) do
      WriteLn('  Op ', History[i].OperationID, ': ',
              'Success=', History[i].SuccessCount,
              ', Failed=', History[i].FailureCount);
  finally
    Manager.Free;
  end;
end.
```

**Bulk Operation Types:**

```pascal
type
  TBulkOperationType = (
    boUpdateStatus,     // Change status for multiple tasks
    boUpdatePriority,   // Change priority
    boUpdateCategory,   // Change category
    boAddTag,          // Add tag to multiple tasks
    boRemoveTag,       // Remove tag
    boDelete,          // Delete multiple tasks
    boArchive,         // Archive multiple tasks
    boAssignMember,    // Assign team member
    boSetDueDate,      // Set due date
    boAddToGoal        // Add to goal/milestone
  );
  
  TBulkOperation = record
    OperationID: Integer;
    OpType: TBulkOperationType;
    TargetTaskIDs: array of Integer;
    Parameters: string;
    ExecutedAt: TDateTime;
    ExecutedBy: string;
    SuccessCount: Integer;
    FailureCount: Integer;
    ResultLog: string;
  end;
```

#### Advanced Analytics

Generate comprehensive analytics and predictions:

```pascal
var
  Manager: TIntelligenceTaskManager;
  CompletionTrend: TTrendArray;
  VelocityReport: TAnalyticsReport;
  Heatmap, Analysis: string;
begin
  Manager := TIntelligenceTaskManager.Create;
  try
    // Generate 30-day completion trend
    CompletionTrend := Manager.GenerateCompletionTrend(30);
    WriteLn('Completion trend points: ', Length(CompletionTrend));
    
    // Category-specific trend
    CompletionTrend := Manager.GenerateCategoryTrend('Development', 30);
    
    // Priority distribution analysis
    CompletionTrend := Manager.GeneratePriorityDistribution;
    for i := 0 to High(CompletionTrend) do
      WriteLn(CompletionTrend[i].Label_, ': ', CompletionTrend[i].Value:0:0, ' tasks');
    
    // Productivity heatmap (visual representation)
    Heatmap := Manager.GenerateProductivityHeatmap;
    WriteLn(Heatmap);
    
    // Team velocity report
    VelocityReport := Manager.GenerateVelocityReport(4);  // 4 weeks
    WriteLn('Velocity Report: ', VelocityReport.Summary);
    
    // Burndown chart for sprint
    CompletionTrend := Manager.GenerateBurndownChart('Sprint 1');
    
    // Predictive analytics
    CompletionTrend := Manager.PredictTaskCompletionTrend(7);  // Next 7 days
    WriteLn('Predicted completions for next 7 days:');
    for i := 0 to High(CompletionTrend) do
      WriteLn('  ', DateToStr(CompletionTrend[i].Date), ': ', 
              CompletionTrend[i].Value:0:1, ' tasks');
    
    // Top performing categories
    Analysis := Manager.GetTopPerformingCategories(5);
    WriteLn('Top Categories:', Analysis);
    
    // Bottleneck analysis
    Analysis := Manager.GetBottleneckAnalysis;
    WriteLn('Bottlenecks:', Analysis);
  finally
    Manager.Free;
  end;
end.
```

**Analytics Types:**

```pascal
type
  TTrendPoint = record
    Date: TDateTime;
    Value: Double;
    Label_: string;
  end;
  
  TAnalyticsReport = record
    ReportID: Integer;
    ReportType: string;
    Generated: TDateTime;
    TimeRange: string;
    DataPoints: TTrendArray;
    Summary: string;
    Insights: array of string;
  end;
```

#### Smart Notifications

Multi-channel notification system:

```pascal
var
  Manager: TIntelligenceTaskManager;
  NotificationID: Integer;
  Pending: TSmartNotificationArray;
  SentCount: Integer;
begin
  Manager := TIntelligenceTaskManager.Create;
  try
    // Create notification
    NotificationID := Manager.CreateNotification(
      ncEmail,                    // Channel: console, file, email, webhook
      npHigh,                     // Priority: low, normal, high, critical
      'Task Overdue',             // Title
      'Task #123 is overdue',     // Message
      123                         // Task ID
    );
    
    // Send specific notification
    if Manager.SendNotification(NotificationID) then
      WriteLn('Notification sent successfully');
    
    // Get pending notifications
    Pending := Manager.GetPendingNotifications;
    WriteLn('Pending notifications: ', Length(Pending));
    
    // Send all pending
    SentCount := Manager.SendAllPendingNotifications;
    WriteLn('Sent ', SentCount, ' notifications');
    
    // Check and create smart notifications automatically
    Manager.CheckAndCreateSmartNotifications;
  finally
    Manager.Free;
  end;
end.
```

**Notification Types:**

```pascal
type
  TNotificationChannel = (
    ncConsole,    // Console output
    ncFile,       // File logging
    ncEmail,      // Email notification
    ncWebhook     // HTTP webhook
  );
  
  TNotificationPriority = (
    npLow,        // Low priority
    npNormal,     // Normal priority
    npHigh,       // High priority
    npCritical    // Critical - immediate attention
  );
  
  TSmartNotification = record
    NotificationID: Integer;
    Channel: TNotificationChannel;
    Priority: TNotificationPriority;
    Title: string;
    Message: string;
    TaskID: Integer;
    CreatedAt: TDateTime;
    SentAt: TDateTime;
    IsSent: Boolean;
    Context: string;
  end;
```

#### Multi-Format Export

Export tasks to various formats:

```pascal
var
  Manager: TIntelligenceTaskManager;
  Result: TExportResult;
begin
  Manager := TIntelligenceTaskManager.Create;
  try
    // Export to JSON
    Result := Manager.ExportToJSON;
    if Result.Success then
      WriteLn('JSON exported: ', Result.FileSize, ' bytes');
    
    // Export to XML
    Result := Manager.ExportToXML;
    
    // Export to iCalendar format
    Result := Manager.ExportToICalendar;
    
    // Export to Markdown
    Result := Manager.ExportToMarkdown;
    
    // Export to HTML
    Result := Manager.ExportToHTML;
    
    // Generic export with format selection
    Result := Manager.ExportWithFormat(efJSON);
    if Result.Success then
      WriteLn('Export successful: ', Result.Content)
    else
      WriteLn('Export failed: ', Result.ErrorMessage);
  finally
    Manager.Free;
  end;
end.
```

**Export Types:**

```pascal
type
  TExportFormat = (
    efJSON,        // JSON format
    efXML,         // XML format
    efICalendar,   // iCalendar (.ics) format
    efMarkdown,    // Markdown format
    efHTML,        // HTML format
    efCSV          // CSV format
  );
  
  TExportResult = record
    Success: Boolean;
    Format: TExportFormat;
    Content: string;
    FileSize: Integer;
    ExportedAt: TDateTime;
    ErrorMessage: string;
  end;
```

**Key Methods Summary:**

```pascal
// NLP
function ParseNaturalLanguageTask(const AInput: string): TParsedTask;
function CreateTaskFromNL(const AInput: string): Integer;
function BulkCreateFromNL(const AInputs: array of string): TIntArray;

// Backup & Versioning
function CreateBackupVersion(const ADescription: string): Integer;
function RestoreFromVersion(AVersionID: Integer): Boolean;
function CreateRestorePoint(const ALabel: string): Integer;
procedure EnableAutoBackup(AEnabled: Boolean; AIntervalHours: Integer);

// Bulk Operations
function BulkUpdateStatus(const ATaskIDs: array of Integer; ANewStatus: TTaskStatus): Integer;
function BulkUpdatePriority(const ATaskIDs: array of Integer; ANewPriority: TTaskPriority): Integer;
function BulkArchive(const ATaskIDs: array of Integer; const AReason: string): Integer;

// Analytics
function GenerateCompletionTrend(ADays: Integer): TTrendArray;
function GenerateVelocityReport(AWeeks: Integer): TAnalyticsReport;
function PredictTaskCompletionTrend(ADaysAhead: Integer): TTrendArray;
function GetBottleneckAnalysis: string;

// Notifications
function CreateNotification(AChannel: TNotificationChannel; APriority: TNotificationPriority; 
  const ATitle, AMessage: string; ATaskID: Integer): Integer;
function SendAllPendingNotifications: Integer;

// Export
function ExportToJSON: TExportResult;
function ExportToXML: TExportResult;
function ExportToICalendar: TExportResult;
function ExportWithFormat(AFormat: TExportFormat): TExportResult;
```



### Focus & Deep Work (TFocusTaskManager)

**Module:** `taskmanagerfocus.pas`  
**Inherits from:** TAdvancedTaskManager  
**Lines of Code:** 1,370  
**Key Features:** Pomodoro Technique, Deep Work Blocks, Flow State Analysis, Distraction Tracking, Context Switch Management

The Focus Task Manager is a comprehensive productivity enhancement system that helps manage attention, track focus quality, and optimize deep work sessions. It implements proven productivity techniques including the Pomodoro Technique, distraction logging, and flow state analysis.

#### Pomodoro Technique

Implement the proven Pomodoro time management method:

```pascal
uses taskmanagerfocus;

var
  Manager: TFocusTaskManager;
  TaskID, PomodoroID: Integer;
begin
  Manager := TFocusTaskManager.Create;
  try
    // Configure Pomodoro settings
    // Parameters: work minutes, short break, long break, cycles before long break
    Manager.SetPomodoroSettings(25, 5, 15, 4);
    
    // Create a task
    TaskID := Manager.AddTask(
      'Write Documentation',
      'Complete API documentation',
      tpHigh,
      IncDay(Now, 3)
    );
    
    // Start a Pomodoro session
    PomodoroID := Manager.StartPomodoro(TaskID, 25);
    WriteLn('Pomodoro session started. Focus for 25 minutes!');
    
    // ... work happens ...
    
    // Complete the Pomodoro with quality rating
    Manager.CompletePomodoro(
      PomodoroID,
      fqExcellent,  // Quality: fqPoor, fqFair, fqGood, fqExcellent
      'Great focus, completed 2 sections'
    );
    
    // Or abandon if interrupted
    // Manager.AbandonPomodoro(PomodoroID, 'Emergency meeting');
    
    // Get Pomodoro statistics
    WriteLn(Manager.GetPomodoroStats(7));  // Last 7 days
  finally
    Manager.Free;
  end;
end.
```

**Pomodoro Types:**

```pascal
type
  TFocusQuality = (fqPoor, fqFair, fqGood, fqExcellent);
  
  TPomodoroSession = record
    ID: Integer;
    TaskID: Integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
    PlannedMinutes: Integer;
    ActualMinutes: Integer;
    Quality: TFocusQuality;
    Completed: Boolean;
    Abandoned: Boolean;
    AbandonReason: string;
    Notes: string;
  end;
```

#### Focus Sessions & Flow State

Track deep work sessions and analyze flow states:

```pascal
var
  Manager: TFocusTaskManager;
  TaskID, SessionID: Integer;
  Stats: TFocusStats;
begin
  Manager := TFocusTaskManager.Create;
  try
    // Set flow threshold (minimum minutes for flow state)
    Manager.SetFlowThreshold(20);
    
    TaskID := Manager.AddTask('Complex Algorithm', 'Implement sorting', tpHigh, Now);
    
    // Start a focus session
    SessionID := Manager.StartFocusSession(
      TaskID,
      ftDeepWork,     // ftShallowWork, ftDeepWork, ftCreative, ftAnalytical
      8               // Energy level before (1-10)
    );
    
    // ... focused work happens ...
    
    // End session with metrics
    Manager.EndFocusSession(
      SessionID,
      9,              // Productivity rating (1-10)
      6,              // Energy level after (1-10)
      'Achieved flow state, made excellent progress'
    );
    
    // Get focus statistics
    Stats := Manager.GetFocusStats(30);  // Last 30 days
    WriteLn('Total Focus Time: ', Stats.TotalFocusMinutes, ' minutes');
    WriteLn('Flow Sessions: ', Stats.FlowSessions);
    WriteLn('Average Session: ', Stats.AverageFocusMinutes:0:1, ' min');
    WriteLn('Deep Work Ratio: ', Stats.DeepWorkRatio:0:1, '%');
  finally
    Manager.Free;
  end;
end.
```

**Focus Session Types:**

```pascal
type
  TFocusType = (ftShallowWork, ftDeepWork, ftCreative, ftAnalytical);
  TFlowState = (fsNoFlow, fsPartialFlow, fsFullFlow, fsSuperFlow);
  
  TFocusSession = record
    ID: Integer;
    TaskID: Integer;
    FocusType: TFocusType;
    StartTime: TDateTime;
    EndTime: TDateTime;
    FocusMinutes: Integer;
    Productivity: Integer;        // 1-10
    EnergyBefore: Integer;        // 1-10
    EnergyAfter: Integer;         // 1-10
    FlowState: TFlowState;
    FlowScore: Integer;           // Calculated flow quality
    Distractions: Integer;
    ContextSwitches: Integer;
    Notes: string;
  end;
  
  TFocusStats = record
    TotalSessions: Integer;
    TotalFocusMinutes: Integer;
    AverageFocusMinutes: Double;
    FlowSessions: Integer;
    DeepWorkRatio: Double;        // Percentage of deep work
    AverageProductivity: Double;
    AverageEnergyChange: Double;
    TotalDistractions: Integer;
    TotalContextSwitches: Integer;
  end;
```

#### Distraction Tracking

Log and analyze distractions to improve focus:

```pascal
var
  Manager: TFocusTaskManager;
  SessionID, DistractionID: Integer;
begin
  Manager := TFocusTaskManager.Create;
  try
    SessionID := Manager.StartFocusSession(TaskID, ftDeepWork, 8);
    
    // Log a distraction
    DistractionID := Manager.LogDistraction(
      SessionID,
      TaskID,
      dtExternal,       // dtInternal, dtExternal, dtDigital, dtEnvironmental
      'Colleague question',
      7,                // Impact: 1-10
      True              // Was it avoidable?
    );
    
    // Analyze distraction patterns
    WriteLn(Manager.GetDistractionStats(30));
    WriteLn('Most common: ', Manager.GetMostCommonDistractions);
    WriteLn('Avoidable rate: ', 
      Manager.GetAvoidableDistractionRate(30):0:1, '%');
  finally
    Manager.Free;
  end;
end.
```

**Distraction Types:**

```pascal
type
  TDistractionType = (
    dtInternal,        // Self-generated (thoughts, hunger, etc.)
    dtExternal,        // People, phone calls
    dtDigital,         // Emails, notifications, social media
    dtEnvironmental    // Noise, temperature, lighting
  );
  
  TDistraction = record
    ID: Integer;
    SessionID: Integer;
    TaskID: Integer;
    DistractionType: TDistractionType;
    Source: string;
    OccurredAt: TDateTime;
    Impact: Integer;           // 1-10
    Avoidable: Boolean;
    RecoveryMinutes: Integer;
  end;
```

#### Context Switching Analysis

Track task switching costs and patterns:

```pascal
var
  Manager: TFocusTaskManager;
  FromTask, ToTask, SwitchID: Integer;
  AverageCost: Double;
begin
  Manager := TFocusTaskManager.Create;
  try
    FromTask := Manager.AddTask('Feature A', 'Development', tpHigh, Now);
    ToTask := Manager.AddTask('Bug Fix', 'Urgent fix', tpCritical, Now);
    
    // Log a context switch
    SwitchID := Manager.LogContextSwitch(
      FromTask,
      ToTask,
      'Critical bug reported',
      False          // Was this switch planned?
    );
    
    // Update with recovery cost
    Manager.UpdateSwitchRecovery(
      SwitchID,
      15,            // Recovery time in minutes
      8              // Productivity cost (1-10)
    );
    
    // Analyze switching impact
    WriteLn(Manager.GetSwitchingCost(30));
    AverageCost := Manager.GetAverageSwitchCost;
    WriteLn('Average switch cost: ', AverageCost:0:1, ' minutes');
  finally
    Manager.Free;
  end;
end.
```

**Context Switch Types:**

```pascal
type
  TContextSwitch = record
    ID: Integer;
    FromTaskID: Integer;
    ToTaskID: Integer;
    SwitchedAt: TDateTime;
    Reason: string;
    Planned: Boolean;
    RecoveryMinutes: Integer;
    ProductivityCost: Integer;    // 1-10
  end;
```

#### Deep Work Blocks

Schedule and protect dedicated deep work time:

```pascal
var
  Manager: TFocusTaskManager;
  BlockID, TaskID: Integer;
  StartTime, EndTime: TDateTime;
begin
  Manager := TFocusTaskManager.Create;
  try
    // Schedule a deep work block
    StartTime := EncodeDate(2024, 12, 25) + EncodeTime(9, 0, 0, 0);
    EndTime := EncodeDate(2024, 12, 25) + EncodeTime(12, 0, 0, 0);
    
    BlockID := Manager.ScheduleDeepWorkBlock(
      'Morning Deep Work',
      StartTime,
      EndTime,
      3              // Protection level: 1-3 (higher = stricter)
    );
    
    // Add tasks to the block
    TaskID := Manager.AddTask('Algorithm Design', 'Core logic', tpHigh, Now);
    Manager.AddTaskToBlock(BlockID, TaskID);
    
    // Start the block
    Manager.StartDeepWorkBlock(BlockID);
    WriteLn('Deep work block started - minimize interruptions!');
    
    // ... deep work happens ...
    
    // End the block
    Manager.EndDeepWorkBlock(BlockID, 'Completed algorithm design');
    
    // Analyze effectiveness
    WriteLn(Manager.GetBlockEffectiveness);
  finally
    Manager.Free;
  end;
end.
```

**Deep Work Block Types:**

```pascal
type
  TDeepWorkBlock = record
    ID: Integer;
    Title: string;
    StartTime: TDateTime;
    EndTime: TDateTime;
    PlannedMinutes: Integer;
    ActualMinutes: Integer;
    ProtectionLevel: Integer;     // 1-3
    TaskIDs: array of Integer;
    Started: Boolean;
    Completed: Boolean;
    Productivity: Integer;        // 1-10
    Notes: string;
  end;
```

#### Flow Pattern Analysis

Identify optimal conditions for deep work:

```pascal
var
  Manager: TFocusTaskManager;
  Patterns: TFlowPatternArray;
  BestHour: Integer;
  FlowPotential: Double;
begin
  Manager := TFocusTaskManager.Create;
  try
    // Identify when you achieve flow most easily
    Patterns := Manager.IdentifyFlowPatterns;
    for Pattern in Patterns do
      WriteLn('Pattern: ', Pattern.Description, 
              ' (confidence: ', Pattern.Confidence:0:1, '%)');
    
    // Find your best time for deep work
    BestHour := Manager.GetBestTimeForDeepWork;
    WriteLn('Best deep work time: ', BestHour, ':00');
    
    // Get personalized recommendations
    WriteLn(Manager.GetFlowStateRecommendations);
    
    // Predict flow potential for a task at specific time
    FlowPotential := Manager.PredictFlowPotential(TaskID, 9);  // 9 AM
    WriteLn('Flow potential at 9 AM: ', FlowPotential:0:1, '%');
  finally
    Manager.Free;
  end;
end.
```

**Flow Pattern Types:**

```pascal
type
  TFlowPattern = record
    Description: string;
    TimeOfDay: Integer;           // Hour 0-23
    AverageFlowScore: Integer;
    SessionCount: Integer;
    Confidence: Double;           // 0-100%
  end;
```

#### Productivity Analytics

Generate comprehensive focus and productivity reports:

```pascal
var
  Manager: TFocusTaskManager;
  Efficiency, DeepWorkRatio: Double;
begin
  Manager := TFocusTaskManager.Create;
  try
    // Overall focus efficiency
    Efficiency := Manager.GetFocusEfficiency(30);
    WriteLn('Focus efficiency: ', Efficiency:0:1, '%');
    
    // Deep work vs shallow work ratio
    DeepWorkRatio := Manager.GetDeepWorkRatio(30);
    WriteLn('Deep work ratio: ', DeepWorkRatio:0:1, '%');
    
    // Interruption impact analysis
    WriteLn(Manager.GetInterruptionImpact(30));
    
    // Get improvement suggestions
    WriteLn(Manager.SuggestFocusImprovements);
    
    // Comprehensive focus report
    WriteLn(Manager.GenerateFocusReport(30));
    
    // Productivity by time of day
    WriteLn(Manager.GetProductivityByTimeOfDay);
    
    // Energy-productivity correlation
    WriteLn(Manager.GetEnergyCorrelation);
  finally
    Manager.Free;
  end;
end.
```

#### Data Persistence

Save and load focus data:

```pascal
var
  Manager: TFocusTaskManager;
begin
  Manager := TFocusTaskManager.Create;
  try
    // Save all focus data
    if Manager.SaveFocusDataToFile('focus_data.json') then
      WriteLn('Focus data saved successfully');
    
    // Load previous data
    if Manager.LoadFocusDataFromFile('focus_data.json') then
      WriteLn('Focus data loaded successfully');
  finally
    Manager.Free;
  end;
end.
```

**Key Methods Summary:**

```pascal
// Pomodoro
function StartPomodoro(ATaskID, AMinutes: Integer): Integer;
function CompletePomodoro(APomodoroID: Integer; AQuality: TFocusQuality; 
  const ANotes: string): Boolean;
function AbandonPomodoro(APomodoroID: Integer; const AReason: string): Boolean;
function GetPomodoroStats(ADays: Integer): string;

// Focus Sessions
function StartFocusSession(ATaskID: Integer; AFocusType: TFocusType; 
  AEnergyBefore: Integer): Integer;
function EndFocusSession(ASessionID, AProductivity, AEnergyAfter: Integer; 
  const ANotes: string): Boolean;
function GetFocusStats(ADays: Integer): TFocusStats;

// Distraction Management
function LogDistraction(ASessionID, ATaskID: Integer; AType: TDistractionType;
  const ASource: string; AImpact: Integer; AAvoidable: Boolean): Integer;
function GetDistractionStats(ADays: Integer): string;
function GetMostCommonDistractions: string;
function GetAvoidableDistractionRate(ADays: Integer): Double;

// Context Switching
function LogContextSwitch(AFromTask, AToTask: Integer; 
  const AReason: string; APlanned: Boolean): Integer;
function UpdateSwitchRecovery(ASwitchID, AMinutes, ACost: Integer): Boolean;
function GetSwitchingCost(ADays: Integer): string;
function GetAverageSwitchCost: Double;

// Deep Work Blocks
function ScheduleDeepWorkBlock(const ATitle: string; AStart, AEnd: TDateTime;
  AProtectionLevel: Integer): Integer;
function AddTaskToBlock(ABlockID, ATaskID: Integer): Boolean;
function StartDeepWorkBlock(ABlockID: Integer): Boolean;
function EndDeepWorkBlock(ABlockID: Integer; const ANotes: string): Boolean;
function GetBlockEffectiveness: string;

// Flow Analysis
function IdentifyFlowPatterns: TFlowPatternArray;
function GetBestTimeForDeepWork: Integer;
function GetFlowStateRecommendations: string;
function PredictFlowPotential(ATaskID, AHour: Integer): Double;

// Analytics
function GetFocusEfficiency(ADays: Integer): Double;
function GetDeepWorkRatio(ADays: Integer): Double;
function GetInterruptionImpact(ADays: Integer): string;
function SuggestFocusImprovements: string;
function GenerateFocusReport(ADays: Integer): string;

// Configuration
procedure SetPomodoroSettings(AWorkMinutes, AShortBreak, ALongBreak, ACycleCount: Integer);
procedure SetFlowThreshold(AMinutes: Integer);

// Persistence
function SaveFocusDataToFile(const AFilename: string): Boolean;
function LoadFocusDataFromFile(const AFilename: string): Boolean;
```

**Use Cases:**

- **Time Management:** Implement Pomodoro technique for focused work sessions
- **Deep Work:** Schedule and protect blocks of uninterrupted time
- **Productivity Analysis:** Track and improve focus quality over time
- **Distraction Management:** Identify and eliminate common interruptions
- **Flow State Optimization:** Discover optimal conditions for peak productivity
- **Context Switch Reduction:** Measure and minimize task switching costs
- **Energy Management:** Correlate energy levels with productivity
- **Personal Analytics:** Generate insights from focus patterns and trends


## Development History

### Version 3.0 - December 2024
- Consolidated all 20+ modules into unified system
- Added comprehensive documentation
- Implemented 22 demonstration programs
- Reached 30,842 lines of production code
- Added compilation helper script

### Version 2.0 - November 2024
- Added extended features (recurring tasks, subtasks)
- Implemented advanced analytics
- Added team collaboration features
- Introduced Kanban boards

### Version 1.0 - October 2024
- Initial release with core CRUD operations
- Basic filtering and sorting
- CSV export and file persistence
- Statistics and analytics

## Integration Guide

### Basic Integration

```pascal
program MyTaskApp;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  taskmanager,  // Core module
  taskmanagerext,  // Extended features
  taskmanagerteam;  // Team features

var
  Manager: TTeamTaskManager;
  TaskID, MemberID: Integer;

begin
  Manager := TTeamTaskManager.Create;
  try
    // Create task
    TaskID := Manager.AddTask(
      'My First Task',
      'Task description',
      'Work',
      tpHigh,
      Now + 7,
      4.0
    );
    
    // Add team member
    MemberID := Manager.AddTeamMember(
      'Alice',
      'alice@example.com',
      'Developer'
    );
    
    // Assign task
    Manager.AssignTask(TaskID, MemberID);
    
    // Save to file
    Manager.SaveToFile('tasks.dat');
    
    WriteLn('Task created and assigned successfully!');
  finally
    Manager.Free;
  end;
end.
```

### Advanced Integration

```pascal
program AdvancedTaskApp;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  taskmanager, taskmanagerext, taskmanageradvanced,
  taskmanagerenhanced, taskmanagerteam, taskmanagerboards,
  taskmanagernotifications;

var
  Manager: TTeamTaskManager;
  NotifManager: TNotificationTaskManager;
  BoardManager: TBoardTaskManager;

begin
  // Initialize managers
  Manager := TTeamTaskManager.Create;
  NotifManager := TNotificationTaskManager.Create;
  BoardManager := TBoardTaskManager.Create;
  
  try
    // Your application logic here
    // Combine features from multiple modules
  finally
    Manager.Free;
    NotifManager.Free;
    BoardManager.Free;
  end;
end.
```

## Best Practices

### Memory Management

Always use try-finally blocks:

```pascal
var
  Manager: TTaskManager;
begin
  Manager := TTaskManager.Create;
  try
    // Your code here
  finally
    Manager.Free;  // Always free
  end;
end.
```

### Error Handling

Check return values:

```pascal
if Manager.AddTask(...) > 0 then
  WriteLn('Task created successfully')
else
  WriteLn('Error creating task');
```

### Performance Tips

1. **Batch operations**: Use batch methods for multiple updates
2. **Filtering**: Filter at the database/manager level, not in code
3. **Indexing**: Use appropriate data structures for large datasets
4. **Caching**: Cache frequently accessed data

### Module Selection

Choose modules based on needs:

- **Small projects**: `taskmanager.pas` only
- **Medium projects**: Add `taskmanagerext.pas` and `taskmanageradvanced.pas`
- **Enterprise**: Include team, boards, notifications modules
- **Full-featured**: Use all modules as needed

## Testing

Each demo program includes comprehensive self-tests:

```bash
# Run all demos sequentially
for i in {1..22}; do
  echo "Running demo $i..."
  ./bin/demo$i
done

# Run specific feature test
./bin/demo5  # Team collaboration
./bin/demo15 # Kanban boards
./bin/demo21 # Time tracking
```

## Troubleshooting

### Compilation Errors

**Error:** "Unknown identifier TTaskManager"
```bash
# Solution: Ensure taskmanager.pas is in the same directory or use -Fu flag
fpc -Futaskmanager.pas solution1.pas
```

**Error:** "Can't open include file"
```bash
# Solution: Include files should be in same directory
# Check taskmanagerintelligence_*.inc files are present
```

### Runtime Errors

**Error:** "Access violation"
- Check that all objects are created before use
- Ensure proper try-finally blocks
- Verify array bounds

**Error:** "File not found"
- Use absolute paths or ensure working directory is correct
- Check file permissions

## Contributing

To extend the system:

1. **Create new module**: Inherit from appropriate base class
2. **Follow naming**: Use `TYourFeatureTaskManager` pattern
3. **Document**: Add demo program (solution*.pas)
4. **Test**: Include comprehensive self-tests

Example new module:

```pascal
unit taskmanagercustom;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, taskmanager;

type
  TCustomTaskManager = class(TTaskManager)
  private
    // Your private fields
  public
    constructor Create;
    destructor Destroy; override;
    // Your methods
  end;

implementation

constructor TCustomTaskManager.Create;
begin
  inherited Create;
  // Your initialization
end;

destructor TCustomTaskManager.Destroy;
begin
  // Your cleanup
  inherited Destroy;
end;

end.
```

## Support & Resources

- **Documentation**: This README and inline code comments
- **Examples**: 22 complete demo programs (solution1.pas - solution22.pas)
- **Source Code**: All modules fully documented with Pascal doc comments
- **Architecture**: See class hierarchy diagram above

## License

This is a demonstration project for educational purposes.

---

**Last Updated**: December 2024  
**Document Version**: 4.0  
**Total Lines of Code**: 30,842  
**Compiler Required**: Free Pascal 3.0+  
**Platform**: Cross-platform (Linux, macOS, Windows)

**Happy Task Managing!** 🚀
