
## 6. User Interface Designs

### 6.1 Overview

The Free Pascal Task Manager is designed as a **reusable library component** without direct user interface implementation. This architectural decision aligns with the core design principle of creating modular, reusable code that can be integrated into various application types.

### 6.2 Design Philosophy

**Library-First Approach:**
- No direct console I/O (no `ReadLn`, `WriteLn` for user interaction)
- No embedded GUI components
- All functionality exposed through clean API interfaces
- UI-agnostic design allowing integration with any presentation layer

### 6.3 Intended UI Integration Patterns

The library is designed to support multiple UI paradigms:

#### 6.3.1 Desktop GUI Applications

**Supported Frameworks:**
- Lazarus / Free Pascal Component Library (FCL)
- fpGUI
- MSEide+MSEgui
- Cross-platform LCL applications

**Integration Pattern:**

```pascal
// Example: GUI application using the task manager library
type
  TMainForm = class(TForm)
  private
    FTaskService: ITaskService;
    FTaskList: TTaskModelList;
    procedure LoadTasksToGrid;
    procedure OnCreateTaskButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  // Initialize the task service
  FTaskService := TTaskServiceImpl.Create(DatabaseConnection);
end;

procedure TMainForm.LoadTasksToGrid;
var
  Task: TTaskModel;
begin
  FTaskList := FTaskService.GetAllTasks;
  try
    TaskGrid.RowCount := FTaskList.Count + 1; // +1 for header
    for Task in FTaskList do
    begin
      TaskGrid.Cells[0, Task.Index] := Task.Title;
      TaskGrid.Cells[1, Task.Index] := TaskStatusToString(Task.Status);
      TaskGrid.Cells[2, Task.Index] := DateToStr(Task.DueDate);
    end;
  finally
    FTaskList.Free;
  end;
end;

procedure TMainForm.OnCreateTaskButtonClick(Sender: TObject);
var
  NewTask: TTaskModel;
begin
  NewTask := TTaskModel.Create;
  try
    NewTask.Title := EditTitle.Text;
    NewTask.Description := MemoDescription.Text;
    NewTask.DueDate := DatePicker.Date;
    NewTask.Priority := TPriority(ComboBoxPriority.ItemIndex);
    
    FTaskService.CreateTask(NewTask);
    LoadTasksToGrid; // Refresh display
  finally
    NewTask.Free;
  end;
end;
```

#### 6.3.2 Web Applications

**Supported Frameworks:**
- Brook Framework
- mORMot Web MVC
- Pascal Server Pages (PSP)
- FastPlaz

**Integration Pattern:**

```pascal
// Example: RESTful web service endpoint
type
  TTaskWebController = class(TBrookController)
  private
    FTaskService: ITaskService;
  public
    procedure GetTasks; // GET /tasks
    procedure CreateTask; // POST /tasks
    procedure UpdateTask; // PUT /tasks/:id
    procedure DeleteTask; // DELETE /tasks/:id
  end;

procedure TTaskWebController.GetTasks;
var
  Tasks: TTaskModelList;
  JSONArray: TJSONArray;
begin
  Tasks := FTaskService.GetAllTasks;
  try
    JSONArray := TaskListToJSON(Tasks);
    Render(JSONArray.AsJSON);
  finally
    Tasks.Free;
    JSONArray.Free;
  end;
end;

procedure TTaskWebController.CreateTask;
var
  NewTask: TTaskModel;
  TaskData: TJSONObject;
begin
  TaskData := TJSONObject(GetJSON(Request.Body));
  try
    NewTask := JSONToTask(TaskData);
    try
      FTaskService.CreateTask(NewTask);
      Render(Format('{"id": %d, "status": "created"}', [NewTask.ID]));
    finally
      NewTask.Free;
    end;
  finally
    TaskData.Free;
  end;
end;
```

#### 6.3.3 Console/Terminal Applications

**Use Case:** CLI tools, system administration, automation scripts

**Integration Pattern:**

```pascal
// Example: Command-line task manager
program TaskManagerCLI;

uses
  task_services, task_models, task_services_impl;

var
  TaskService: ITaskService;
  Tasks: TTaskModelList;
  Task: TTaskModel;
  Command: string;

procedure DisplayTasks;
begin
  Tasks := TaskService.GetAllTasks;
  try
    WriteLn('=== Task List ===');
    for Task in Tasks do
      WriteLn(Format('[%d] %s - %s', [Task.ID, Task.Title, TaskStatusToString(Task.Status)]));
  finally
    Tasks.Free;
  end;
end;

begin
  TaskService := TTaskServiceImpl.Create('tasks.db');
  
  // Read command from command-line arguments (not ReadLn)
  if ParamCount > 0 then
    Command := ParamStr(1);
    
  case Command of
    'list': DisplayTasks;
    'add': CreateTaskFromParams;
    'complete': CompleteTaskFromParams;
  end;
end.
```

#### 6.3.4 Mobile Applications

**Supported Frameworks:**
- Delphi FireMonkey (FMX)
- Castle Game Engine (with Pascal)

**Note:** Mobile UI would follow similar patterns to desktop GUI integration.

### 6.4 UI Component Recommendations

While the library doesn't include UI components, we recommend the following for implementers:

#### 6.4.1 Essential UI Components for Task Management

1. **Task List View:**
   - Grid/List component showing tasks
   - Sortable columns (title, status, priority, due date)
   - Filtering capabilities
   - Selection and multi-selection support

2. **Task Detail Form:**
   - Text input for title
   - Multi-line text area for description
   - Date/time picker for due dates
   - Priority selector (dropdown/radio buttons)
   - Tag selection (multi-select or tag input)
   - Status selector

3. **Calendar View:**
   - Monthly/weekly calendar component
   - Task visualization on dates
   - Drag-and-drop support for rescheduling

4. **Kanban Board (if using board module):**
   - Column-based layout
   - Draggable task cards
   - Status change on drag-and-drop

5. **Search and Filter Panel:**
   - Text search input
   - Filter dropdowns (status, priority, tags)
   - Date range pickers

6. **Notifications/Alerts:**
   - Toast/notification components
   - Alert dialogs for confirmations
   - Reminder pop-ups

### 6.5 Data Binding Patterns

**Recommended Approach:**

```pascal
// Example: Data-aware component binding
type
  TTaskViewModel = class
  private
    FTask: TTaskModel;
    FTaskService: ITaskService;
    FOnPropertyChanged: TNotifyEvent;
    procedure SetTitle(const Value: string);
    function GetTitle: string;
  public
    constructor Create(ATaskService: ITaskService; ATaskID: Int64);
    destructor Destroy; override;
    
    procedure Save;
    procedure Refresh;
    
    property Title: string read GetTitle write SetTitle;
    property Description: string read FTask.Description write FTask.Description;
    property DueDate: TDateTime read FTask.DueDate write FTask.DueDate;
    property OnPropertyChanged: TNotifyEvent read FOnPropertyChanged write FOnPropertyChanged;
  end;

procedure TTaskViewModel.SetTitle(const Value: string);
begin
  if FTask.Title <> Value then
  begin
    FTask.Title := Value;
    if Assigned(FOnPropertyChanged) then
      FOnPropertyChanged(Self);
  end;
end;

procedure TTaskViewModel.Save;
begin
  FTaskService.UpdateTask(FTask);
end;
```

### 6.6 Accessibility Considerations

UI implementers should consider:

1. **Keyboard Navigation:** All task operations accessible via keyboard
2. **Screen Reader Support:** Proper labels and ARIA attributes (web)
3. **High Contrast Mode:** Support for visual impairments
4. **Internationalization:** Library returns plain data; UI layer handles localization

### 6.7 UI Testing Recommendations

While this library focuses on backend logic, UI implementers should:

1. **Separation of Concerns:** Keep UI logic separate from business logic tests
2. **Mock Services:** Use interface mocking for UI tests
3. **UI Frameworks:** Utilize framework-specific testing tools (e.g., FPCUnit for LCL)

### 6.8 Example UI Mockups (Conceptual)

**Note:** These are conceptual descriptions, not implemented components.

#### 6.8.1 Main Task List View (Desktop)

```
┌─────────────────────────────────────────────────────────────────┐
│ Task Manager                                          [_][□][X] │
├─────────────────────────────────────────────────────────────────┤
│ File  Edit  View  Help                                          │
├─────────────────────────────────────────────────────────────────┤
│ [New Task] [Delete] [Filter ▼]              Search: [________] │
├─────────────────────────────────────────────────────────────────┤
│ ☐ Title               │ Priority │ Due Date   │ Status          │
├─────────────────────────────────────────────────────────────────┤
│ ☐ Complete project    │ High     │ 2024-12-15 │ In Progress     │
│ ☐ Review pull request │ Medium   │ 2024-12-10 │ Todo            │
│ ☑ Write documentation │ Low      │ 2024-12-05 │ Completed       │
└─────────────────────────────────────────────────────────────────┘
```

#### 6.8.2 Kanban Board View (Web)

```
┌──────────────┬──────────────┬──────────────┬──────────────┐
│   TODO       │ IN PROGRESS  │   REVIEW     │   DONE       │
├──────────────┼──────────────┼──────────────┼──────────────┤
│ ┌──────────┐ │ ┌──────────┐ │ ┌──────────┐ │ ┌──────────┐ │
│ │Task A    │ │ │Task D    │ │ │Task G    │ │ │Task J    │ │
│ │High      │ │ │Medium    │ │ │Low       │ │ │Completed │ │
│ └──────────┘ │ └──────────┘ │ └──────────┘ │ └──────────┘ │
│ ┌──────────┐ │ ┌──────────┐ │              │              │
│ │Task B    │ │ │Task E    │ │              │              │
│ └──────────┘ │ └──────────┘ │              │              │
└──────────────┴──────────────┴──────────────┴──────────────┘
```

### 6.9 Summary

This section clarifies that **user interface design is intentionally excluded** from the library specification, as the Free Pascal Task Manager is designed as a **backend library component**. UI implementation is left to the consumer applications, with the library providing clean, well-documented APIs for integration into any UI framework or application type.

**Key Takeaways:**
- Library provides UI-agnostic business logic and data access
- Supports integration with desktop, web, console, and mobile UIs
- UI implementers have full flexibility in presentation layer choices
- Clean separation of concerns enables better testability and maintainability

---
