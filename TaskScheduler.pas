
unit TaskScheduler;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager,
  TaskManagerExtended,
  TaskManagerAdvanced,
  TaskManagerSubtasks,
  TaskManagerHistory,
  TaskManagerEnhanced;

type
  { Scheduling related types }
  TScheduleEntry = record
    taskId: integer;
    taskName: string;
    assignee: string;
    scheduledStart: TDateTime;
    scheduledEnd: TDateTime;
    estimatedHours: double;
    actualHours: double;
    status: TTaskStatus;
    priority: TTaskPriority;
  end;

  TScheduleEntryArray = array of TScheduleEntry;

  TGanttEntry = record
    taskId: integer;
    taskName: string;
    startDate: string; { Format: YYYY-MM-DD }
    endDate: string;
    assignee: string;
    progress: integer; { 0-100 }
    duration: integer; { days }
    priority: string;
    dependencies: string; { comma-separated task IDs }
  end;

  TGanttEntryArray = array of TGanttEntry;

  TCapacityEntry = record
    assignee: string;
    totalCapacityHours: double;
    allocatedHours: double;
    availableHours: double;
    utilizationPercent: double;
    taskCount: integer;
  end;

  TCapacityEntryArray = array of TCapacityEntry;

  TCriticalPathEntry = record
    taskId: integer;
    taskName: string;
    slack: double; { days }
    isCritical: boolean;
  end;

  TCriticalPathArray = array of TCriticalPathEntry;

  { Task Scheduler class }
  TTaskScheduler = class
  private
    manager: TTaskManagerEnhanced;
    lastError: string;
    workHoursPerDay: double;

    function findCapacityIndex(const assignee: string; var capacities: TCapacityEntryArray): integer;
    function calculateTaskProgress(taskId: integer): integer;
    function getDependencyString(taskId: integer): string;
    function dateToString(d: TDateTime): string;
  public
    constructor Create(aManager: TTaskManagerEnhanced);
    destructor Destroy; override;

    { Schedule tasks based on dependencies and workload }
    function scheduleAll(var schedule: TScheduleEntryArray): boolean;
    function scheduleSingleTask(taskId: integer; var entry: TScheduleEntry): boolean;

    { Gantt chart generation }
    function generateGanttChart(var ganttChart: TGanttEntryArray): boolean;
    function exportGanttChartAsText: string;

    { Capacity planning }
    function calculateTeamCapacity(var capacities: TCapacityEntryArray): boolean;
    function findOverallocatedAssignees: TStringArray;
    function suggestAssignmentForTask(taskId: integer): string;

    { Critical path analysis }
    function analyzeCriticalPath(var criticalPath: TCriticalPathArray): boolean;
    function getCriticalPathLength: double;
    function getCriticalTasks: TTaskArray;

    { Resource optimization }
    function optimizeSchedule(var schedule: TScheduleEntryArray): boolean;
    function balanceTeamWorkload: boolean;

    { Configuration }
    procedure setWorkHoursPerDay(hours: double);
    function getWorkHoursPerDay: double;

    { Error handling }
    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskScheduler.Create(aManager: TTaskManagerEnhanced);
begin
  inherited Create;
  manager := aManager;
  lastError := '';
  workHoursPerDay := 8.0;
end;

destructor TTaskScheduler.Destroy;
begin
  inherited Destroy;
end;

function TTaskScheduler.findCapacityIndex(const assignee: string; var capacities: TCapacityEntryArray): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to length(capacities) - 1 do
  begin
    if capacities[i].assignee = assignee then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskScheduler.calculateTaskProgress(taskId: integer): integer;
var
  task: TTask;
  progress: integer;
begin
  result := 0;
  if manager.getTask(taskId, task) then
  begin
    case task.status of
      tsCompleted: progress := 100;
      tsInProgress: progress := 50;
      tsBlocked: progress := 25;
      tsNew: progress := 0;
      tsCancelled: progress := 0;
    else
      progress := 0;
    end;
    result := progress;
  end;
end;

function TTaskScheduler.getDependencyString(taskId: integer): string;
var
  deps: TIntArray;
  i: integer;
  depStr: string;
begin
  depStr := '';
  deps := manager.getTaskDependencies(taskId);
  
  for i := 0 to length(deps) - 1 do
  begin
    if i > 0 then
      depStr := depStr + ',';
    depStr := depStr + intToStr(deps[i]);
  end;
  
  result := depStr;
end;

function TTaskScheduler.dateToString(d: TDateTime): string;
begin
  result := FormatDateTime('YYYY-MM-DD', d);
end;

function TTaskScheduler.scheduleAll(var schedule: TScheduleEntryArray): boolean;
var
  allTasks: TTaskArray;
  i: integer;
  entry: TScheduleEntry;
  currentDate: TDateTime;
  hoursToAdd: double;
begin
  result := false;
  try
    allTasks := manager.getAllTasks;
    setLength(schedule, 0);

    currentDate := now;

    for i := 0 to length(allTasks) - 1 do
    begin
      entry.taskId := allTasks[i].id;
      entry.taskName := allTasks[i].name;
      entry.assignee := allTasks[i].assignedTo;
      entry.status := allTasks[i].status;
      entry.priority := allTasks[i].priority;
      entry.actualHours := allTasks[i].actualHours;
      entry.estimatedHours := allTasks[i].estimatedHours;

      { Calculate scheduled start and end }
      if allTasks[i].status = tsCompleted then
      begin
        entry.scheduledStart := allTasks[i].createdDate;
        entry.scheduledEnd := allTasks[i].completedDate;
      end
      else
      begin
        entry.scheduledStart := currentDate;
        hoursToAdd := allTasks[i].estimatedHours;
        if hoursToAdd = 0 then
          hoursToAdd := 4.0;
        entry.scheduledEnd := currentDate + (hoursToAdd / workHoursPerDay);
      end;

      setLength(schedule, length(schedule) + 1);
      schedule[length(schedule) - 1] := entry;

      currentDate := entry.scheduledEnd + 1;
    end;

    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error scheduling tasks: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskScheduler.scheduleSingleTask(taskId: integer; var entry: TScheduleEntry): boolean;
var
  task: TTask;
begin
  result := false;
  try
    if manager.getTask(taskId, task) then
    begin
      entry.taskId := task.id;
      entry.taskName := task.name;
      entry.assignee := task.assignedTo;
      entry.status := task.status;
      entry.priority := task.priority;
      entry.actualHours := task.actualHours;
      entry.estimatedHours := task.estimatedHours;

      if task.status = tsCompleted then
      begin
        entry.scheduledStart := task.createdDate;
        entry.scheduledEnd := task.completedDate;
      end
      else
      begin
        entry.scheduledStart := now;
        entry.scheduledEnd := now + (task.estimatedHours / workHoursPerDay);
      end;

      result := true;
    end
    else
      lastError := 'Task not found: ' + intToStr(taskId);
  except
    on e: Exception do
      lastError := 'Error scheduling task: ' + e.Message;
  end;
end;

function TTaskScheduler.generateGanttChart(var ganttChart: TGanttEntryArray): boolean;
var
  allTasks: TTaskArray;
  i: integer;
  entry: TGanttEntry;
  progress: integer;
  daysDiff: integer;
begin
  result := false;
  try
    allTasks := manager.getAllTasks;
    setLength(ganttChart, 0);

    for i := 0 to length(allTasks) - 1 do
    begin
      entry.taskId := allTasks[i].id;
      entry.taskName := allTasks[i].name;
      entry.assignee := allTasks[i].assignedTo;
      entry.priority := 'High';

      case allTasks[i].priority of
        tpCritical: entry.priority := 'Critical';
        tpHigh: entry.priority := 'High';
        tpMedium: entry.priority := 'Medium';
        tpLow: entry.priority := 'Low';
      end;

      if allTasks[i].status = tsCompleted then
      begin
        entry.startDate := dateToString(allTasks[i].createdDate);
        entry.endDate := dateToString(allTasks[i].completedDate);
      end
      else
      begin
        entry.startDate := dateToString(now);
        entry.endDate := dateToString(allTasks[i].dueDate);
      end;

      progress := calculateTaskProgress(allTasks[i].id);
      entry.progress := progress;

      daysDiff := daysBetween(allTasks[i].dueDate, now);
      if daysDiff < 0 then
        daysDiff := abs(daysDiff);
      entry.duration := daysDiff;

      entry.dependencies := getDependencyString(allTasks[i].id);

      setLength(ganttChart, length(ganttChart) + 1);
      ganttChart[length(ganttChart) - 1] := entry;
    end;

    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error generating Gantt chart: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskScheduler.exportGanttChartAsText: string;
var
  ganttChart: TGanttEntryArray;
  output: string;
  i: integer;
begin
  output := '';
  
  if generateGanttChart(ganttChart) then
  begin
    output := output + 'GANTT CHART DATA' + #13#10;
    output := output + '================' + #13#10;
    output := output + 'TaskID,TaskName,Start,End,Assignee,Progress,Duration,Priority,Dependencies' + #13#10;

    for i := 0 to length(ganttChart) - 1 do
    begin
      with ganttChart[i] do
      begin
        output := output + intToStr(taskId) + ',' +
                  taskName + ',' +
                  startDate + ',' +
                  endDate + ',' +
                  assignee + ',' +
                  intToStr(progress) + ',' +
                  intToStr(duration) + ',' +
                  priority + ',' +
                  dependencies + #13#10;
      end;
    end;
  end;

  result := output;
end;

function TTaskScheduler.calculateTeamCapacity(var capacities: TCapacityEntryArray): boolean;
var
  allTasks: TTaskArray;
  allAssignees: TStringArray;
  i, j, capIndex: integer;
  assigneeExists: boolean;
  assigneeCount: integer;
begin
  result := false;
  try
    allTasks := manager.getAllTasks;
    setLength(capacities, 0);
    setLength(allAssignees, 0);
    assigneeCount := 0;

    { Collect all unique assignees }
    for i := 0 to length(allTasks) - 1 do
    begin
      if allTasks[i].assignedTo <> '' then
      begin
        assigneeExists := false;
        for j := 0 to assigneeCount - 1 do
        begin
          if allAssignees[j] = allTasks[i].assignedTo then
          begin
            assigneeExists := true;
            break;
          end;
        end;

        if not assigneeExists then
        begin
          setLength(allAssignees, assigneeCount + 1);
          allAssignees[assigneeCount] := allTasks[i].assignedTo;
          inc(assigneeCount);
        end;
      end;
    end;

    { Calculate capacity for each assignee }
    setLength(capacities, assigneeCount);
    for i := 0 to assigneeCount - 1 do
    begin
      capacities[i].assignee := allAssignees[i];
      capacities[i].totalCapacityHours := 40.0; { Assuming 40 hour work week }
      capacities[i].allocatedHours := 0;
      capacities[i].taskCount := 0;

      { Sum up allocated hours for this assignee }
      for j := 0 to length(allTasks) - 1 do
      begin
        if allTasks[j].assignedTo = allAssignees[i] then
        begin
          if allTasks[j].status <> tsCompleted then
          begin
            capacities[i].allocatedHours := capacities[i].allocatedHours + allTasks[j].estimatedHours;
            inc(capacities[i].taskCount);
          end;
        end;
      end;

      capacities[i].availableHours := capacities[i].totalCapacityHours - capacities[i].allocatedHours;
      capacities[i].utilizationPercent := (capacities[i].allocatedHours / capacities[i].totalCapacityHours) * 100;
    end;

    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error calculating team capacity: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskScheduler.findOverallocatedAssignees: TStringArray;
var
  capacities: TCapacityEntryArray;
  i: integer;
  count: integer;
begin
  setLength(result, 0);
  count := 0;

  if calculateTeamCapacity(capacities) then
  begin
    for i := 0 to length(capacities) - 1 do
    begin
      if capacities[i].utilizationPercent > 100 then
      begin
        setLength(result, count + 1);
        result[count] := capacities[i].assignee;
        inc(count);
      end;
    end;
  end;
end;

function TTaskScheduler.suggestAssignmentForTask(taskId: integer): string;
var
  capacities: TCapacityEntryArray;
  bestAssignee: string;
  bestCapacity: double;
  i: integer;
begin
  result := '';
  bestAssignee := '';
  bestCapacity := -1;

  if calculateTeamCapacity(capacities) then
  begin
    for i := 0 to length(capacities) - 1 do
    begin
      if capacities[i].availableHours > bestCapacity then
      begin
        bestCapacity := capacities[i].availableHours;
        bestAssignee := capacities[i].assignee;
      end;
    end;
  end;

  result := bestAssignee;
end;

function TTaskScheduler.analyzeCriticalPath(var criticalPath: TCriticalPathArray): boolean;
var
  allTasks: TTaskArray;
  i: integer;
  entry: TCriticalPathEntry;
  hasBlockingDependencies: boolean;
  deps: TIntArray;
  j: integer;
begin
  result := false;
  try
    allTasks := manager.getAllTasks;
    setLength(criticalPath, 0);

    for i := 0 to length(allTasks) - 1 do
    begin
      entry.taskId := allTasks[i].id;
      entry.taskName := allTasks[i].name;
      entry.slack := daysBetween(allTasks[i].dueDate, now);

      { Critical if: high priority, approaching deadline, or blocking other tasks }
      hasBlockingDependencies := false;
      deps := manager.getTaskDependencies(allTasks[i].id);
      if length(deps) > 0 then
        hasBlockingDependencies := true;

      entry.isCritical := (allTasks[i].priority = tpCritical) or
                         (entry.slack < 2) or
                         (hasBlockingDependencies and (allTasks[i].status <> tsCompleted));

      setLength(criticalPath, length(criticalPath) + 1);
      criticalPath[length(criticalPath) - 1] := entry;
    end;

    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error analyzing critical path: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskScheduler.getCriticalPathLength: double;
var
  criticalPath: TCriticalPathArray;
  i: integer;
  maxSlack: double;
begin
  result := 0;
  if analyzeCriticalPath(criticalPath) then
  begin
    maxSlack := 0;
    for i := 0 to length(criticalPath) - 1 do
    begin
      if criticalPath[i].slack > maxSlack then
        maxSlack := criticalPath[i].slack;
    end;
    result := maxSlack;
  end;
end;

function TTaskScheduler.getCriticalTasks: TTaskArray;
var
  criticalPath: TCriticalPathArray;
  i: integer;
  count: integer;
  task: TTask;
begin
  setLength(result, 0);
  count := 0;

  if analyzeCriticalPath(criticalPath) then
  begin
    for i := 0 to length(criticalPath) - 1 do
    begin
      if criticalPath[i].isCritical then
      begin
        if manager.getTask(criticalPath[i].taskId, task) then
        begin
          setLength(result, count + 1);
          result[count] := task;
          inc(count);
        end;
      end;
    end;
  end;
end;

function TTaskScheduler.optimizeSchedule(var schedule: TScheduleEntryArray): boolean;
var
  i, j: integer;
  temp: TScheduleEntry;
begin
  result := false;
  try
    { Sort by priority and due date }
    for i := 0 to length(schedule) - 2 do
    begin
      for j := i + 1 to length(schedule) - 1 do
      begin
        if (ord(schedule[j].priority) > ord(schedule[i].priority)) or
           ((schedule[j].priority = schedule[i].priority) and
            (schedule[j].scheduledEnd < schedule[i].scheduledEnd)) then
        begin
          temp := schedule[i];
          schedule[i] := schedule[j];
          schedule[j] := temp;
        end;
      end;
    end;
    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error optimizing schedule: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskScheduler.balanceTeamWorkload: boolean;
var
  capacities: TCapacityEntryArray;
begin
  result := false;
  try
    if calculateTeamCapacity(capacities) then
    begin
      { In a real scenario, we would reassign tasks here }
      result := true;
    end;
  except
    on e: Exception do
    begin
      lastError := 'Error balancing workload: ' + e.Message;
      result := false;
    end;
  end;
end;

procedure TTaskScheduler.setWorkHoursPerDay(hours: double);
begin
  if hours > 0 then
    workHoursPerDay := hours;
end;

function TTaskScheduler.getWorkHoursPerDay: double;
begin
  result := workHoursPerDay;
end;

function TTaskScheduler.getLastError: string;
begin
  result := lastError;
end;

procedure TTaskScheduler.clearError;
begin
  lastError := '';
end;

end.
