
unit TaskManager;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes;

type
  TTaskManager = class
  private
    tasks: TTaskArray;
    nextTaskId: integer;
    lastError: string;

    function findTaskIndex(id: integer): integer;

  public
    constructor Create;
    destructor Destroy; override;

    // Core CRUD operations
    function addTask(const name, description, category: string;
                     priority: TTaskPriority; dueDate: TDateTime): integer;
    function getTask(id: integer; var task: TTask): boolean;
    function updateTask(var task: TTask): boolean;
    function deleteTask(id: integer): boolean;
    function getTaskCount: integer;

    // Task retrieval
    function getAllTasks: TTaskArray;
    function getTasksByStatus(status: TTaskStatus): TTaskArray;
    function getTasksByPriority(priority: TTaskPriority): TTaskArray;
    function getTasksByCategory(const category: string): TTaskArray;

    // Task operations
    function setTaskStatus(id: integer; newStatus: TTaskStatus): boolean;
    function setTaskPriority(id: integer; newPriority: TTaskPriority): boolean;
    function completeTask(id: integer): boolean;

    // Sorting
    function getTasksSortedByDueDate: TTaskArray;
    function getTasksSortedByPriority: TTaskArray;
    function getTasksSortedByStatus: TTaskArray;

    // Search
    function searchTasksByName(const searchTerm: string): TTaskArray;
    function searchTasksByDescription(const searchTerm: string): TTaskArray;
    function searchTasks(const searchTerm: string): TTaskArray;



    // Time tracking
    function setTaskEstimatedHours(id: integer; hours: double): boolean;
    function addTaskActualHours(id: integer; hours: double): boolean;
    function getTaskEstimatedHours(id: integer): double;
    function getTaskActualHours(id: integer): double;
    function getTaskTimeOverrun(id: integer): double;
    function getTasksOverTime: TTaskArray;
    // Tag management
    function addTaskTag(id: integer; const tag: string): boolean;
    function removeTaskTag(id: integer; const tag: string): boolean;
    function hasTaskTag(id: integer; const tag: string): boolean;
    function getTaskTags(id: integer): TTagArray;
    function getTasksByTag(const tag: string): TTaskArray;
    function getAllTags: TTagArray;
    // Persistence - overloaded versions
    function saveTasks: boolean; overload;
    function saveTasks(const filename: string): boolean; overload;
    function loadTasks: boolean; overload;
    function loadTasks(const filename: string): boolean; overload;

    // Statistics
    function getStatistics: TTaskStats;
    function getOverdueTaskCount: integer;

    // Error handling
    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskManager.Create;
begin
  inherited Create;
  setLength(tasks, 0);
  nextTaskId := 1;
  lastError := '';
end;

destructor TTaskManager.Destroy;
begin
  setLength(tasks, 0);
  inherited Destroy;
end;

function TTaskManager.findTaskIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to length(tasks) - 1 do
    if tasks[i].id = id then
      begin
        result := i;
        exit;
      end;
end;

function TTaskManager.addTask(const name, description, category: string;
                               priority: TTaskPriority; dueDate: TDateTime): integer;
var
  newTask: TTask;
begin
  clearError;

  if length(name) = 0 then
    begin
      lastError := 'Task name cannot be empty';
      result := -1;
      exit;
    end;

  newTask.id := nextTaskId;
  inc(nextTaskId);
  newTask.name := name;
  newTask.description := description;
  newTask.category := category;
  newTask.status := tsNew;
  newTask.priority := priority;
  newTask.dueDate := dueDate;
  newTask.createdDate := now;
  newTask.completedDate := 0;
  setLength(newTask.tags, 0);
  newTask.estimatedHours := 0;
  newTask.actualHours := 0;

  setLength(tasks, length(tasks) + 1);
  tasks[length(tasks) - 1] := newTask;

  result := newTask.id;
end;

function TTaskManager.getTask(id: integer; var task: TTask): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      task := tasks[idx];
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
    end;
end;

function TTaskManager.updateTask(var task: TTask): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(task.id);
  if idx >= 0 then
    begin
      tasks[idx] := task;
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(task.id);
      result := false;
    end;
end;

function TTaskManager.deleteTask(id: integer): boolean;
var
  idx, i: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      for i := idx to length(tasks) - 2 do
        tasks[i] := tasks[i + 1];
      setLength(tasks, length(tasks) - 1);
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
    end;
end;

function TTaskManager.getTaskCount: integer;
begin
  result := length(tasks);
end;

function TTaskManager.getAllTasks: TTaskArray;
begin
  result := copy(tasks, 0, length(tasks));
end;

function TTaskManager.getTasksByStatus(status: TTaskStatus): TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to length(tasks) - 1 do
    if tasks[i].status = status then
      begin
        setLength(result, count + 1);
        result[count] := tasks[i];
        inc(count);
      end;
end;

function TTaskManager.getTasksByPriority(priority: TTaskPriority): TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to length(tasks) - 1 do
    if tasks[i].priority = priority then
      begin
        setLength(result, count + 1);
        result[count] := tasks[i];
        inc(count);
      end;
end;

function TTaskManager.getTasksByCategory(const category: string): TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;
  for i := 0 to length(tasks) - 1 do
    if tasks[i].category = category then
      begin
        setLength(result, count + 1);
        result[count] := tasks[i];
        inc(count);
      end;
end;

function TTaskManager.setTaskStatus(id: integer; newStatus: TTaskStatus): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      tasks[idx].status := newStatus;
      if newStatus = tsCompleted then
        tasks[idx].completedDate := now;
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
    end;
end;

function TTaskManager.setTaskPriority(id: integer; newPriority: TTaskPriority): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      tasks[idx].priority := newPriority;
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
    end;
end;

function TTaskManager.completeTask(id: integer): boolean;
begin
  result := setTaskStatus(id, tsCompleted);
end;

function TTaskManager.getTasksSortedByDueDate: TTaskArray;
var
  sorted: TTaskArray;
  i, j: integer;
  temp: TTask;
begin
  sorted := copy(tasks, 0, length(tasks));

  for i := 0 to length(sorted) - 1 do
    for j := 0 to length(sorted) - 2 - i do
      if sorted[j].dueDate > sorted[j + 1].dueDate then
        begin
          temp := sorted[j];
          sorted[j] := sorted[j + 1];
          sorted[j + 1] := temp;
        end;

  result := sorted;
end;

function TTaskManager.getTasksSortedByPriority: TTaskArray;
var
  sorted: TTaskArray;
  i, j: integer;
  temp: TTask;
begin
  sorted := copy(tasks, 0, length(tasks));

  for i := 0 to length(sorted) - 1 do
    for j := 0 to length(sorted) - 2 - i do
      if integer(sorted[j].priority) < integer(sorted[j + 1].priority) then
        begin
          temp := sorted[j];
          sorted[j] := sorted[j + 1];
          sorted[j + 1] := temp;
        end;

  result := sorted;
end;

function TTaskManager.getTasksSortedByStatus: TTaskArray;
var
  sorted: TTaskArray;
  i, j: integer;
  temp: TTask;
begin
  sorted := copy(tasks, 0, length(tasks));

  for i := 0 to length(sorted) - 1 do
    for j := 0 to length(sorted) - 2 - i do
      if integer(sorted[j].status) > integer(sorted[j + 1].status) then
        begin
          temp := sorted[j];
          sorted[j] := sorted[j + 1];
          sorted[j + 1] := temp;
        end;

  result := sorted;
end;

function TTaskManager.searchTasksByName(const searchTerm: string): TTaskArray;
var
  i, count: integer;
  lowerSearchTerm, lowerName: string;
begin
  setLength(result, 0);
  count := 0;
  lowerSearchTerm := lowercase(searchTerm);

  for i := 0 to length(tasks) - 1 do
    begin
      lowerName := lowercase(tasks[i].name);
      if pos(lowerSearchTerm, lowerName) > 0 then
        begin
          setLength(result, count + 1);
          result[count] := tasks[i];
          inc(count);
        end;
    end;
end;

function TTaskManager.searchTasksByDescription(const searchTerm: string): TTaskArray;
var
  i, count: integer;
  lowerSearchTerm, lowerDesc: string;
begin
  setLength(result, 0);
  count := 0;
  lowerSearchTerm := lowercase(searchTerm);

  for i := 0 to length(tasks) - 1 do
    begin
      lowerDesc := lowercase(tasks[i].description);
      if pos(lowerSearchTerm, lowerDesc) > 0 then
        begin
          setLength(result, count + 1);
          result[count] := tasks[i];
          inc(count);
        end;
    end;
end;

function TTaskManager.searchTasks(const searchTerm: string): TTaskArray;
var
  i, count: integer;
  lowerSearchTerm, lowerName, lowerDesc: string;
begin
  setLength(result, 0);
  count := 0;
  lowerSearchTerm := lowercase(searchTerm);

  for i := 0 to length(tasks) - 1 do
    begin
      lowerName := lowercase(tasks[i].name);
      lowerDesc := lowercase(tasks[i].description);
      if (pos(lowerSearchTerm, lowerName) > 0) or
         (pos(lowerSearchTerm, lowerDesc) > 0) then
        begin
          setLength(result, count + 1);
          result[count] := tasks[i];
          inc(count);
        end;
    end;
end;

function TTaskManager.saveTasks: boolean;
begin
  result := saveTasks(defaultTaskFile);
end;

function TTaskManager.saveTasks(const filename: string): boolean;
begin
  clearError;
  lastError := 'File persistence disabled: dynamic arrays in TTask';
  result := false;
end;

function TTaskManager.loadTasks: boolean;
begin
  result := loadTasks(defaultTaskFile);
end;

function TTaskManager.loadTasks(const filename: string): boolean;
begin
  clearError;
  lastError := 'File persistence disabled: dynamic arrays in TTask';
  result := false;
end;


function TTaskManager.setTaskEstimatedHours(id: integer; hours: double): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      tasks[idx].estimatedHours := hours;
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
    end;
end;

function TTaskManager.addTaskActualHours(id: integer; hours: double): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      tasks[idx].actualHours := tasks[idx].actualHours + hours;
      result := true;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
    end;
end;

function TTaskManager.getTaskEstimatedHours(id: integer): double;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    result := tasks[idx].estimatedHours
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := 0;
    end;
end;

function TTaskManager.getTaskActualHours(id: integer): double;
var
  idx: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    result := tasks[idx].actualHours
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := 0;
    end;
end;

function TTaskManager.getTaskTimeOverrun(id: integer): double;
var
  idx: integer;
  overrun: double;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx >= 0 then
    begin
      overrun := tasks[idx].actualHours - tasks[idx].estimatedHours;
      if overrun < 0 then
        result := 0
      else
        result := overrun;
    end
  else
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := 0;
    end;
end;

function TTaskManager.getTasksOverTime: TTaskArray;
var
  i, count: integer;
begin
  clearError;
  setLength(result, 0);
  count := 0;

  for i := 0 to length(tasks) - 1 do
    if tasks[i].actualHours > tasks[i].estimatedHours then
      begin
        setLength(result, count + 1);
        result[count] := tasks[i];
        inc(count);
      end;
end;


function TTaskManager.addTaskTag(id: integer; const tag: string): boolean;
var
  idx: integer;
  tagCount: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx < 0 then
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
      exit;
    end;

  if length(tag) = 0 then
    begin
      lastError := 'Tag cannot be empty';
      result := false;
      exit;
    end;

  if hasTaskTag(id, tag) then
    begin
      lastError := 'Tag already exists: ' + tag;
      result := false;
      exit;
    end;

  tagCount := length(tasks[idx].tags);
  if tagCount >= maxTagsPerTask then
    begin
      lastError := 'Maximum tags reached for task';
      result := false;
      exit;
    end;

  setLength(tasks[idx].tags, tagCount + 1);
  tasks[idx].tags[tagCount] := tag;
  result := true;
end;

function TTaskManager.removeTaskTag(id: integer; const tag: string): boolean;
var
  idx, i, j: integer;
  found: boolean;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx < 0 then
    begin
      lastError := 'Task not found: ' + intToStr(id);
      result := false;
      exit;
    end;

  found := false;
  for i := 0 to length(tasks[idx].tags) - 1 do
    if tasks[idx].tags[i] = tag then
      begin
        found := true;
        for j := i to length(tasks[idx].tags) - 2 do
          tasks[idx].tags[j] := tasks[idx].tags[j + 1];
        setLength(tasks[idx].tags, length(tasks[idx].tags) - 1);
        result := true;
        exit;
      end;

  if not found then
    begin
      lastError := 'Tag not found: ' + tag;
      result := false;
    end;
end;

function TTaskManager.hasTaskTag(id: integer; const tag: string): boolean;
var
  idx, i: integer;
begin
  clearError;
  idx := findTaskIndex(id);
  if idx < 0 then
    begin
      result := false;
      exit;
    end;

  result := false;
  for i := 0 to length(tasks[idx].tags) - 1 do
    if tasks[idx].tags[i] = tag then
      begin
        result := true;
        exit;
      end;
end;

function TTaskManager.getTaskTags(id: integer): TTagArray;
var
  idx: integer;
begin
  clearError;
  setLength(result, 0);
  idx := findTaskIndex(id);
  if idx >= 0 then
    result := copy(tasks[idx].tags, 0, length(tasks[idx].tags))
  else
    lastError := 'Task not found: ' + intToStr(id);
end;

function TTaskManager.getTasksByTag(const tag: string): TTaskArray;
var
  i, j, count: integer;
begin
  clearError;
  setLength(result, 0);
  count := 0;

  for i := 0 to length(tasks) - 1 do
    for j := 0 to length(tasks[i].tags) - 1 do
      if tasks[i].tags[j] = tag then
        begin
          setLength(result, count + 1);
          result[count] := tasks[i];
          inc(count);
          break;
        end;
end;

function TTaskManager.getAllTags: TTagArray;
var
  i, j, k, count: integer;
  tagExists: boolean;
begin
  setLength(result, 0);
  count := 0;

  for i := 0 to length(tasks) - 1 do
    for j := 0 to length(tasks[i].tags) - 1 do
      begin
        tagExists := false;
        for k := 0 to count - 1 do
          if result[k] = tasks[i].tags[j] then
            begin
              tagExists := true;
              break;
            end;
        if not tagExists then
          begin
            setLength(result, count + 1);
            result[count] := tasks[i].tags[j];
            inc(count);
          end;
      end;
end;

function TTaskManager.getStatistics: TTaskStats;
var
  i: integer;
begin
  fillChar(result, sizeof(result), 0);
  result.totalTasks := length(tasks);

  for i := 0 to length(tasks) - 1 do
    begin
      case tasks[i].status of
        tsNew: inc(result.newTasks);
        tsInProgress: inc(result.inProgressTasks);
        tsBlocked: inc(result.blockedTasks);
        tsCompleted: inc(result.completedTasks);
        tsCancelled: inc(result.cancelledTasks);
      end;

      case tasks[i].priority of
        tpHigh: inc(result.highPriorityCount);
        tpCritical: inc(result.criticalPriorityCount);
        else;
      end;
    end;
end;

function TTaskManager.getOverdueTaskCount: integer;
var
  i, count: integer;
  currentTime: TDateTime;
begin
  count := 0;
  currentTime := now;
  for i := 0 to length(tasks) - 1 do
    if (tasks[i].dueDate < currentTime) and
       (tasks[i].status <> tsCompleted) and
       (tasks[i].status <> tsCancelled) then
      inc(count);
  result := count;
end;

function TTaskManager.getLastError: string;
begin
  result := lastError;
end;

procedure TTaskManager.clearError;
begin
  lastError := '';
end;

end.
