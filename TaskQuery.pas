
unit TaskQuery;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, TaskTypes, TaskManager;

type
  TTaskQueryClass = class
  private
    manager: TTaskManagerClass;
    function CountTasksInList(tasks: TTaskArray; status: TTaskStatus): integer;
  public
    constructor Create(amanager: TTaskManagerClass);
    
    { Filtering operations }
    function FilterByKeyword(keyword: string): TTaskArray;
    function FilterByDateRange(startDate, endDate: TDateTime): TTaskArray;
    
    { Time-based filtering }
    function GetTasksDueToday(): TTaskArray;
    function GetTasksDueTomorrow(): TTaskArray;
    function GetTasksDueThisWeek(): TTaskArray;
    function GetTasksDueThisMonth(): TTaskArray;
    
    { Statistics }
    function GetStats(): TTaskStats;
    function GetCompletionRate(): double;
    function GetAverageTasksPerDay(): double;
    
    { Priority analysis }
    function GetHighPriorityCount(): integer;
    function GetMediumPriorityCount(): integer;
    function GetLowPriorityCount(): integer;
    
    { Time-based analysis }
    function GetTasksCreatedToday(): TTaskArray;
    function GetTasksCompletedToday(): TTaskArray;
    function GetTasksCompletedThisWeek(): TTaskArray;
    
    { Advanced filtering }
    function FilterByPriorityRange(minPriority, maxPriority: TTaskPriority): TTaskArray;
    function GetTasksWithoutDueDate(): TTaskArray;
    function GetCompletedTasksThisMonth(): TTaskArray;
    function GetInProgressTasks(): TTaskArray;
    function GetOnHoldTasks(): TTaskArray;
    
    { Sorting utilities }
    procedure SortByCompletionStatus(var tasks: TTaskArray);
    procedure SortByCreationDate(var tasks: TTaskArray);
    procedure SortByCompletionDate(var tasks: TTaskArray);
    
    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskQueryClass.Create(amanager: TTaskManagerClass);
begin
  inherited Create();
  manager := amanager;
end;

function TTaskQueryClass.CountTasksInList(tasks: TTaskArray; status: TTaskStatus): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to length(tasks) - 1 do
  begin
    if tasks[i].status = status then
      inc(count);
  end;
  Result := count;
end;

function TTaskQueryClass.FilterByKeyword(keyword: string): TTaskArray;
var
  i, count: integer;
  allTasks: TTaskArray;
  arr: TTaskArray;
  lowerKeyword: string;
begin
  allTasks := manager.GetAllTasks();
  lowerKeyword := LowerCase(keyword);
  count := 0;
  SetLength(arr, 0);
  
  for i := 0 to length(allTasks) - 1 do
  begin
    if (Pos(lowerKeyword, LowerCase(allTasks[i].title)) > 0) or
       (Pos(lowerKeyword, LowerCase(allTasks[i].description)) > 0) then
    begin
      SetLength(arr, count + 1);
      arr[count] := allTasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

function TTaskQueryClass.FilterByDateRange(startDate, endDate: TDateTime): TTaskArray;
var
  i, count: integer;
  allTasks: TTaskArray;
  arr: TTaskArray;
begin
  allTasks := manager.GetAllTasks();
  count := 0;
  SetLength(arr, 0);
  
  for i := 0 to length(allTasks) - 1 do
  begin
    if (allTasks[i].dueDate >= startDate) and (allTasks[i].dueDate <= endDate) then
    begin
      SetLength(arr, count + 1);
      arr[count] := allTasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

function TTaskQueryClass.GetTasksDueToday(): TTaskArray;
var
  today: TDateTime;
begin
  today := Trunc(Now());
  Result := FilterByDateRange(today, today + 1);
end;

function TTaskQueryClass.GetTasksDueTomorrow(): TTaskArray;
var
  tomorrow: TDateTime;
begin
  tomorrow := Trunc(Now()) + 1;
  Result := FilterByDateRange(tomorrow, tomorrow + 1);
end;

function TTaskQueryClass.GetTasksDueThisWeek(): TTaskArray;
var
  today, endOfWeek: TDateTime;
begin
  today := Trunc(Now());
  endOfWeek := today + (7 - DayOfWeek(today));
  Result := FilterByDateRange(today, endOfWeek);
end;

function TTaskQueryClass.GetTasksDueThisMonth(): TTaskArray;
var
  today: TDateTime;
  startOfMonth, endOfMonth: TDateTime;
begin
  today := Now();
  startOfMonth := Trunc(today) - (DayOfTheMonth(today) - 1);
  endOfMonth := EndOfTheMonth(today);
  Result := FilterByDateRange(startOfMonth, endOfMonth);
end;

function TTaskQueryClass.GetStats(): TTaskStats;
var
  allTasks: TTaskArray;
begin
  allTasks := manager.GetAllTasks();
  Result.totalTasks := length(allTasks);
  Result.completedTasks := CountTasksInList(allTasks, tsCompleted);
  Result.inProgressTasks := CountTasksInList(allTasks, tsInProgress);
  Result.onHoldTasks := CountTasksInList(allTasks, tsOnHold);
  Result.notStartedTasks := CountTasksInList(allTasks, tsNotStarted);
  Result.overdueTasks := length(manager.GetOverdueTasks());
end;

function TTaskQueryClass.GetCompletionRate(): double;
var
  stats: TTaskStats;
begin
  stats := GetStats();
  if stats.totalTasks = 0 then
    Result := 0.0
  else
    Result := (stats.completedTasks / stats.totalTasks) * 100.0;
end;

function TTaskQueryClass.GetAverageTasksPerDay(): double;
var
  allTasks: TTaskArray;
  daysDiff: integer;
  i: integer;
  firstDate, lastDate: TDateTime;
begin
  allTasks := manager.GetAllTasks();
  if length(allTasks) = 0 then
  begin
    Result := 0.0;
    Exit;
  end;
  
  firstDate := allTasks[0].createdDate;
  lastDate := allTasks[0].createdDate;
  
  for i := 0 to length(allTasks) - 1 do
  begin
    if allTasks[i].createdDate < firstDate then
      firstDate := allTasks[i].createdDate;
    if allTasks[i].createdDate > lastDate then
      lastDate := allTasks[i].createdDate;
  end;
  
  daysDiff := Trunc(lastDate - firstDate) + 1;
  if daysDiff < 1 then
    daysDiff := 1;
  
  Result := length(allTasks) / daysDiff;
end;

function TTaskQueryClass.GetHighPriorityCount(): integer;
var
  tasks: TTaskArray;
begin
  tasks := manager.GetTasksByPriority(tpHigh);
  Result := length(tasks);
end;

function TTaskQueryClass.GetMediumPriorityCount(): integer;
var
  tasks: TTaskArray;
begin
  tasks := manager.GetTasksByPriority(tpMedium);
  Result := length(tasks);
end;

function TTaskQueryClass.GetLowPriorityCount(): integer;
var
  tasks: TTaskArray;
begin
  tasks := manager.GetTasksByPriority(tpLow);
  Result := length(tasks);
end;

function TTaskQueryClass.GetTasksCreatedToday(): TTaskArray;
var
  i, count: integer;
  allTasks: TTaskArray;
  arr: TTaskArray;
  today: TDateTime;
begin
  allTasks := manager.GetAllTasks();
  today := Trunc(Now());
  count := 0;
  SetLength(arr, 0);
  
  for i := 0 to length(allTasks) - 1 do
  begin
    if Trunc(allTasks[i].createdDate) = today then
    begin
      SetLength(arr, count + 1);
      arr[count] := allTasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

function TTaskQueryClass.GetTasksCompletedToday(): TTaskArray;
var
  i, count: integer;
  allTasks: TTaskArray;
  arr: TTaskArray;
  today: TDateTime;
begin
  allTasks := manager.GetAllTasks();
  today := Trunc(Now());
  count := 0;
  SetLength(arr, 0);
  
  for i := 0 to length(allTasks) - 1 do
  begin
    if (allTasks[i].status = tsCompleted) and (Trunc(allTasks[i].completedDate) = today) then
    begin
      SetLength(arr, count + 1);
      arr[count] := allTasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

function TTaskQueryClass.GetTasksCompletedThisWeek(): TTaskArray;
var
  i, count: integer;
  allTasks: TTaskArray;
  arr: TTaskArray;
  today, weekStart: TDateTime;
begin
  allTasks := manager.GetAllTasks();
  today := Trunc(Now());
  weekStart := today - (DayOfWeek(today) - 1);
  count := 0;
  SetLength(arr, 0);
  
  for i := 0 to length(allTasks) - 1 do
  begin
    if (allTasks[i].status = tsCompleted) and 
       (Trunc(allTasks[i].completedDate) >= weekStart) and
       (Trunc(allTasks[i].completedDate) <= today) then
    begin
      SetLength(arr, count + 1);
      arr[count] := allTasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

function TTaskQueryClass.FilterByPriorityRange(minPriority, maxPriority: TTaskPriority): TTaskArray;
var
  i, count: integer;
  allTasks: TTaskArray;
  arr: TTaskArray;
begin
  allTasks := manager.GetAllTasks();
  count := 0;
  SetLength(arr, 0);
  
  for i := 0 to length(allTasks) - 1 do
  begin
    if (ord(allTasks[i].priority) >= ord(minPriority)) and 
       (ord(allTasks[i].priority) <= ord(maxPriority)) then
    begin
      SetLength(arr, count + 1);
      arr[count] := allTasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

function TTaskQueryClass.GetTasksWithoutDueDate(): TTaskArray;
var
  i, count: integer;
  allTasks: TTaskArray;
  arr: TTaskArray;
begin
  allTasks := manager.GetAllTasks();
  count := 0;
  SetLength(arr, 0);
  
  for i := 0 to length(allTasks) - 1 do
  begin
    if allTasks[i].dueDate = 0 then
    begin
      SetLength(arr, count + 1);
      arr[count] := allTasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

function TTaskQueryClass.GetCompletedTasksThisMonth(): TTaskArray;
var
  i, count: integer;
  allTasks: TTaskArray;
  arr: TTaskArray;
  today: TDateTime;
  startOfMonth, endOfMonth: TDateTime;
begin
  allTasks := manager.GetAllTasks();
  today := Now();
  startOfMonth := Trunc(today) - (DayOfTheMonth(today) - 1);
  endOfMonth := EndOfTheMonth(today);
  count := 0;
  SetLength(arr, 0);
  
  for i := 0 to length(allTasks) - 1 do
  begin
    if (allTasks[i].status = tsCompleted) and
       (Trunc(allTasks[i].completedDate) >= startOfMonth) and
       (Trunc(allTasks[i].completedDate) <= endOfMonth) then
    begin
      SetLength(arr, count + 1);
      arr[count] := allTasks[i];
      inc(count);
    end;
  end;
  Result := arr;
end;

function TTaskQueryClass.GetInProgressTasks(): TTaskArray;
begin
  Result := manager.GetTasksByStatus(tsInProgress);
end;

function TTaskQueryClass.GetOnHoldTasks(): TTaskArray;
begin
  Result := manager.GetTasksByStatus(tsOnHold);
end;

procedure TTaskQueryClass.SortByCompletionStatus(var tasks: TTaskArray);
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to length(tasks) - 2 do
    for j := i + 1 to length(tasks) - 1 do
    begin
      if ord(tasks[j].status) < ord(tasks[i].status) then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
end;

procedure TTaskQueryClass.SortByCreationDate(var tasks: TTaskArray);
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to length(tasks) - 2 do
    for j := i + 1 to length(tasks) - 1 do
    begin
      if tasks[j].createdDate < tasks[i].createdDate then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
end;

procedure TTaskQueryClass.SortByCompletionDate(var tasks: TTaskArray);
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to length(tasks) - 2 do
    for j := i + 1 to length(tasks) - 1 do
    begin
      if tasks[j].completedDate < tasks[i].completedDate then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
end;

procedure TTaskQueryClass.SelfTest();
var
  allTasks, filtered, completed: TTaskArray;
  stats: TTaskStats;
begin
  WriteLn('=== TaskQuery Self Test ===');
  
  WriteLn('Adding test tasks with various dates...');
  manager.AddTask('Task 1', 'Test task', tpHigh, Now() + 1);
  manager.AddTask('Task 2', 'Test task', tpMedium, Now() + 3);
  manager.AddTask('Task 3', 'Completed test', tpLow, Now() + 5);
  manager.AddTask('Task 4', 'No due date', tpMedium, 0);
  
  WriteLn('Testing keyword search...');
  filtered := FilterByKeyword('test');
  WriteLn('  Found ', length(filtered), ' tasks matching "test"');
  
  WriteLn('Testing date range filtering...');
  filtered := GetTasksDueThisWeek();
  WriteLn('  Tasks due this week: ', length(filtered));
  
  WriteLn('Getting task statistics...');
  stats := GetStats();
  WriteLn('  Total tasks: ', stats.totalTasks);
  WriteLn('  Completed: ', stats.completedTasks);
  WriteLn('  In progress: ', stats.inProgressTasks);
  WriteLn('  Completion rate: ', GetCompletionRate():5:1, '%');
  
  WriteLn('Testing priority filtering...');
  filtered := FilterByPriorityRange(tpMedium, tpHigh);
  WriteLn('  Medium to High priority tasks: ', length(filtered));
  
  WriteLn('Testing advanced filters...');
  filtered := GetTasksWithoutDueDate();
  WriteLn('  Tasks without due date: ', length(filtered));
  
  filtered := GetInProgressTasks();
  WriteLn('  In progress tasks: ', length(filtered));
  
  WriteLn('=== TaskQuery Self Test Complete ===');
  WriteLn('');
end;

end.
