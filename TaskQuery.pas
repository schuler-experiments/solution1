
unit TaskQuery;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, TaskTypes, TaskManager, Math;

type
  { Query statistics result }
  TTaskStats = record
    totalTasks: integer;
    completedTasks: integer;
    pendingTasks: integer;
    inProgressTasks: integer;
    onHoldTasks: integer;
    overdueTasks: integer;
    completionPercentage: double;
  end;

  { Advanced query and reporting class }
  TTaskQueryClass = class
  private
    manager: TTaskManagerClass;
  public
    constructor Create(amanager: TTaskManagerClass);
    
    { Advanced filtering }
    function FilterByKeyword(keyword: string): TTaskArray;
    function FilterByDateRange(startDate, endDate: TDateTime): TTaskArray;
    function GetTasksDueToday(): TTaskArray;
    function GetTasksDueTomorrow(): TTaskArray;
    function GetTasksDueThisWeek(): TTaskArray;
    function GetTasksDueThisMonth(): TTaskArray;
    
    { Status reporting }
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

function TTaskQueryClass.FilterByKeyword(keyword: string): TTaskArray;
var
  tasks: TTaskArray;
  i, count: integer;
  searchTerm: string;
begin
  count := 0;
  SetLength(result, 0);
  searchTerm := LowerCase(keyword);
  tasks := manager.GetAllTasks();
  
  for i := 0 to Length(tasks) - 1 do
  begin
    if (Pos(searchTerm, LowerCase(tasks[i].title)) > 0) or
       (Pos(searchTerm, LowerCase(tasks[i].description)) > 0) then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      Inc(count);
    end;
  end;
end;

function TTaskQueryClass.FilterByDateRange(startDate, endDate: TDateTime): TTaskArray;
var
  tasks: TTaskArray;
  i, count: integer;
begin
  count := 0;
  SetLength(result, 0);
  tasks := manager.GetAllTasks();
  
  for i := 0 to Length(tasks) - 1 do
  begin
    if (tasks[i].dueDate >= startDate) and (tasks[i].dueDate <= endDate) then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      Inc(count);
    end;
  end;
end;

function TTaskQueryClass.GetTasksDueToday(): TTaskArray;
var
  today: TDateTime;
begin
  today := Trunc(Now());
  result := FilterByDateRange(today, today + 1);
end;

function TTaskQueryClass.GetTasksDueTomorrow(): TTaskArray;
var
  tomorrow: TDateTime;
begin
  tomorrow := Trunc(Now()) + 1;
  result := FilterByDateRange(tomorrow, tomorrow + 1);
end;

function TTaskQueryClass.GetTasksDueThisWeek(): TTaskArray;
var
  today, weekEnd: TDateTime;
begin
  today := Trunc(Now());
  weekEnd := today + (7 - DayOfWeek(today));
  result := FilterByDateRange(today, weekEnd);
end;

function TTaskQueryClass.GetTasksDueThisMonth(): TTaskArray;
var
  today, monthEnd: TDateTime;
begin
  today := Trunc(Now());
  monthEnd := EndOfTheMonth(today);
  result := FilterByDateRange(today, monthEnd);
end;

function TTaskQueryClass.GetStats(): TTaskStats;
var
  tasks: TTaskArray;
  i: integer;
begin
  tasks := manager.GetAllTasks();
  
  result.totalTasks := Length(tasks);
  result.completedTasks := 0;
  result.pendingTasks := 0;
  result.inProgressTasks := 0;
  result.onHoldTasks := 0;
  result.overdueTasks := 0;
  
  for i := 0 to Length(tasks) - 1 do
  begin
    case tasks[i].status of
      tsCompleted: Inc(result.completedTasks);
      tsNotStarted: Inc(result.pendingTasks);
      tsInProgress: Inc(result.inProgressTasks);
      tsOnHold: Inc(result.onHoldTasks);
    end;
    
    if (tasks[i].status <> tsCompleted) and (tasks[i].dueDate > 0) and
       (tasks[i].dueDate < Now()) then
      Inc(result.overdueTasks);
  end;
  
  if result.totalTasks > 0 then
    result.completionPercentage := (result.completedTasks / result.totalTasks) * 100
  else
    result.completionPercentage := 0;
end;

function TTaskQueryClass.GetCompletionRate(): double;
var
  stats: TTaskStats;
begin
  stats := GetStats();
  result := stats.completionPercentage;
end;

function TTaskQueryClass.GetAverageTasksPerDay(): double;
var
  stats: TTaskStats;
  tasks: TTaskArray;
  i: integer;
  totalDays: integer;
  oldestDate: TDateTime;
begin
  stats := GetStats();
  tasks := manager.GetAllTasks();
  
  if stats.totalTasks <= 1 then
  begin
    result := 0;
    exit;
  end;
  
  oldestDate := MaxDouble;
  for i := 0 to Length(tasks) - 1 do
  begin
    if (tasks[i].createdDate > 0) and (tasks[i].createdDate < oldestDate) then
      oldestDate := tasks[i].createdDate;
  end;
  
  if oldestDate < MaxDouble then
  begin
    totalDays := Trunc(Now() - oldestDate) + 1;
    if totalDays > 0 then
      result := stats.totalTasks / totalDays
    else
      result := 0;
  end
  else
    result := 0;
end;

function TTaskQueryClass.GetHighPriorityCount(): integer;
begin
  result := Length(manager.GetTasksByPriority(tpHigh));
end;

function TTaskQueryClass.GetMediumPriorityCount(): integer;
begin
  result := Length(manager.GetTasksByPriority(tpMedium));
end;

function TTaskQueryClass.GetLowPriorityCount(): integer;
begin
  result := Length(manager.GetTasksByPriority(tpLow));
end;

function TTaskQueryClass.GetTasksCreatedToday(): TTaskArray;
var
  tasks: TTaskArray;
  i, count: integer;
  today: TDateTime;
begin
  count := 0;
  SetLength(result, 0);
  today := Trunc(Now());
  tasks := manager.GetAllTasks();
  
  for i := 0 to Length(tasks) - 1 do
  begin
    if Trunc(tasks[i].createdDate) = today then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      Inc(count);
    end;
  end;
end;

function TTaskQueryClass.GetTasksCompletedToday(): TTaskArray;
var
  tasks: TTaskArray;
  i, count: integer;
  today: TDateTime;
begin
  count := 0;
  SetLength(result, 0);
  today := Trunc(Now());
  tasks := manager.GetAllTasks();
  
  for i := 0 to Length(tasks) - 1 do
  begin
    if (tasks[i].status = tsCompleted) and
       (Trunc(tasks[i].completedDate) = today) then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      Inc(count);
    end;
  end;
end;

function TTaskQueryClass.GetTasksCompletedThisWeek(): TTaskArray;
var
  tasks: TTaskArray;
  i, count: integer;
  weekStart: TDateTime;
begin
  count := 0;
  SetLength(result, 0);
  weekStart := Trunc(Now()) - (DayOfWeek(Now()) - 1);
  tasks := manager.GetAllTasks();
  
  for i := 0 to Length(tasks) - 1 do
  begin
    if (tasks[i].status = tsCompleted) and
       (Trunc(tasks[i].completedDate) >= weekStart) then
    begin
      SetLength(result, count + 1);
      result[count] := tasks[i];
      Inc(count);
    end;
  end;
end;

procedure TTaskQueryClass.SortByCompletionStatus(var tasks: TTaskArray);
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to Length(tasks) - 2 do
  begin
    for j := i + 1 to Length(tasks) - 1 do
    begin
      if (tasks[j].status = tsCompleted) and (tasks[i].status <> tsCompleted) then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
  end;
end;

procedure TTaskQueryClass.SortByCreationDate(var tasks: TTaskArray);
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to Length(tasks) - 2 do
  begin
    for j := i + 1 to Length(tasks) - 1 do
    begin
      if tasks[j].createdDate < tasks[i].createdDate then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
  end;
end;

procedure TTaskQueryClass.SortByCompletionDate(var tasks: TTaskArray);
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to Length(tasks) - 2 do
  begin
    for j := i + 1 to Length(tasks) - 1 do
    begin
      if (tasks[j].completedDate > 0) and
         ((tasks[i].completedDate = 0) or (tasks[j].completedDate < tasks[i].completedDate)) then
      begin
        temp := tasks[i];
        tasks[i] := tasks[j];
        tasks[j] := temp;
      end;
    end;
  end;
end;

procedure TTaskQueryClass.SelfTest();
var
  testMgr: TTaskManagerClass;
  testQuery: TTaskQueryClass;
  stats: TTaskStats;
  keywordResults: TTaskArray;
  thisWeekTasks: TTaskArray;
  i: integer;
begin
  WriteLn('=== TaskQuery Self Test ===');
  
  testMgr := TTaskManagerClass.Create();
  testQuery := TTaskQueryClass.Create(testMgr);
  
  try
    { Add test data }
    WriteLn('Adding test tasks with various dates...');
    testMgr.AddTask('Urgent bug fix', 'Fix critical issue', tpHigh, IncDay(Now(), -1));
    testMgr.AddTask('Feature implementation', 'New user dashboard', tpHigh, IncDay(Now(), 2));
    testMgr.AddTask('Code review', 'Review PR #42', tpMedium, IncDay(Now(), 1));
    testMgr.AddTask('Update tests', 'Add test coverage', tpLow, IncDay(Now(), 5));
    testMgr.CompleteTask(testMgr.GetAllTasks()[0].id);
    
    { Test keyword filtering }
    WriteLn('Testing keyword search...');
    keywordResults := testQuery.FilterByKeyword('review');
    WriteLn(Format('  Found %d tasks matching "review"', [Length(keywordResults)]));
    
    { Test date range filtering }
    WriteLn('Testing date range filtering...');
    thisWeekTasks := testQuery.GetTasksDueThisWeek();
    WriteLn(Format('  Tasks due this week: %d', [Length(thisWeekTasks)]));
    
    { Test statistics }
    WriteLn('Getting task statistics...');
    stats := testQuery.GetStats();
    WriteLn(Format('  Total tasks: %d', [stats.totalTasks]));
    WriteLn(Format('  Completed: %d', [stats.completedTasks]));
    WriteLn(Format('  In progress: %d', [stats.inProgressTasks]));
    WriteLn(Format('  Overdue: %d', [stats.overdueTasks]));
    WriteLn(Format('  Completion rate: %.1f%%', [stats.completionPercentage]));
    
    { Test priority counts }
    WriteLn('Priority breakdown:');
    WriteLn(Format('  High: %d', [testQuery.GetHighPriorityCount()]));
    WriteLn(Format('  Medium: %d', [testQuery.GetMediumPriorityCount()]));
    WriteLn(Format('  Low: %d', [testQuery.GetLowPriorityCount()]));
    
    WriteLn('=== TaskQuery Self Test Complete ===');
    
  finally
    testMgr.Free();
    testQuery.Free();
  end;
end;

end.
