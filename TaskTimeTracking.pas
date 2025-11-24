

unit TaskTimeTracking;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  { Time tracking entry }
  TTimeEntry = record
    id: integer;
    taskId: integer;
    startTime: TDateTime;
    endTime: TDateTime;
    description: string;
    createdDate: TDateTime;
  end;

  TTimeEntryArray = array of TTimeEntry;

  { Time summary }
  TTimeSummary = record
    totalHours: double;
    estimatedHours: double;
    variance: double;
    taskCount: integer;
  end;

  { Daily time summary }
  TDailyTimeSummary = record
    date: TDateTime;
    hoursWorked: double;
    tasksCompleted: integer;
    averageHoursPerTask: double;
  end;

  TDailyTimeSummaryArray = array of TDailyTimeSummary;

  { Task time tracking manager }
  TTaskTimeTrackingClass = class
  private
    timeEntries: TTimeEntryArray;
    nextEntryId: integer;
    
    function FindEntryIndex(id: integer): integer;
    function CalculateDurationHours(startTime, endTime: TDateTime): double;
  public
    constructor Create();
    destructor Destroy(); override;
    
    { Time entry operations }
    function StartTimeTracking(taskId: integer; description: string): integer;
    function StopTimeTracking(entryId: integer): boolean;
    function GetTimeEntry(id: integer): TTimeEntry;
    function GetTaskTimeEntries(taskId: integer): TTimeEntryArray;
    function GetAllTimeEntries(): TTimeEntryArray;
    function DeleteTimeEntry(id: integer): boolean;
    
    { Time summaries }
    function GetTaskTimeSummary(taskId: integer): TTimeSummary;
    function GetTotalTimeSummary(): TTimeSummary;
    function GetDailyTimeSummary(date: TDateTime): TDailyTimeSummary;
    function GetWeeklyTimeSummary(startDate: TDateTime): TTimeSummary;
    function GetMonthlyTimeSummary(year, month: integer): TTimeSummary;
    
    { Analysis }
    function GetAverageTimePerTask(): double;
    function GetMostTimeSpentTask(): integer;
    function GetTimeVariance(taskId: integer): double;
    function GetProductivityTrend(days: integer): TDailyTimeSummaryArray;
    
    { Statistics }
    function GetTotalTimeTracked(): double;
    function GetTimeEntryCount(): integer;
    function GetAverageTaskDuration(): double;
    
    { Self test }
    procedure SelfTest();
  end;

implementation

constructor TTaskTimeTrackingClass.Create();
begin
  SetLength(timeEntries, 0);
  nextEntryId := 1;
end;

destructor TTaskTimeTrackingClass.Destroy();
begin
  SetLength(timeEntries, 0);
  inherited;
end;

function TTaskTimeTrackingClass.FindEntryIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(timeEntries) - 1 do
  begin
    if timeEntries[i].id = id then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TTaskTimeTrackingClass.CalculateDurationHours(startTime, endTime: TDateTime): double;
begin
  if endTime > startTime then
    result := (endTime - startTime) * 24.0
  else
    result := 0.0;
end;

function TTaskTimeTrackingClass.StartTimeTracking(taskId: integer; description: string): integer;
var
  entry: TTimeEntry;
begin
  entry.id := nextEntryId;
  entry.taskId := taskId;
  entry.startTime := Now();
  entry.endTime := 0;
  entry.description := description;
  entry.createdDate := Now();
  
  SetLength(timeEntries, Length(timeEntries) + 1);
  timeEntries[Length(timeEntries) - 1] := entry;
  
  result := nextEntryId;
  Inc(nextEntryId);
end;

function TTaskTimeTrackingClass.StopTimeTracking(entryId: integer): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindEntryIndex(entryId);
  if idx >= 0 then
  begin
    timeEntries[idx].endTime := Now();
    result := true;
  end;
end;

function TTaskTimeTrackingClass.GetTimeEntry(id: integer): TTimeEntry;
var
  idx: integer;
  emptyEntry: TTimeEntry;
begin
  FillChar(emptyEntry, SizeOf(emptyEntry), 0);
  idx := FindEntryIndex(id);
  if idx >= 0 then
    result := timeEntries[idx]
  else
    result := emptyEntry;
end;

function TTaskTimeTrackingClass.GetTaskTimeEntries(taskId: integer): TTimeEntryArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  count := 0;
  
  for i := 0 to Length(timeEntries) - 1 do
  begin
    if timeEntries[i].taskId = taskId then
    begin
      SetLength(result, count + 1);
      result[count] := timeEntries[i];
      Inc(count);
    end;
  end;
end;

function TTaskTimeTrackingClass.GetAllTimeEntries(): TTimeEntryArray;
begin
  result := timeEntries;
end;

function TTaskTimeTrackingClass.DeleteTimeEntry(id: integer): boolean;
var
  idx, i: integer;
begin
  result := false;
  idx := FindEntryIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(timeEntries) - 2 do
      timeEntries[i] := timeEntries[i + 1];
    SetLength(timeEntries, Length(timeEntries) - 1);
    result := true;
  end;
end;

function TTaskTimeTrackingClass.GetTaskTimeSummary(taskId: integer): TTimeSummary;
var
  entries: TTimeEntryArray;
  i: integer;
  totalHours: double;
begin
  FillChar(result, SizeOf(result), 0);
  entries := GetTaskTimeEntries(taskId);
  totalHours := 0;
  
  for i := 0 to Length(entries) - 1 do
  begin
    if entries[i].endTime > 0 then
      totalHours := totalHours + CalculateDurationHours(entries[i].startTime, entries[i].endTime);
  end;
  
  result.totalHours := totalHours;
  result.taskCount := Length(entries);
  SetLength(entries, 0);
end;

function TTaskTimeTrackingClass.GetTotalTimeSummary(): TTimeSummary;
var
  i: integer;
  totalHours: double;
begin
  FillChar(result, SizeOf(result), 0);
  totalHours := 0;
  
  for i := 0 to Length(timeEntries) - 1 do
  begin
    if timeEntries[i].endTime > 0 then
      totalHours := totalHours + CalculateDurationHours(timeEntries[i].startTime, timeEntries[i].endTime);
  end;
  
  result.totalHours := totalHours;
  result.taskCount := Length(timeEntries);
end;

function TTaskTimeTrackingClass.GetDailyTimeSummary(date: TDateTime): TDailyTimeSummary;
var
  i: integer;
  hoursWorked: double;
  tasksCount: integer;
  targetDate: TDateTime;
begin
  FillChar(result, SizeOf(result), 0);
  targetDate := Int(date);
  hoursWorked := 0;
  tasksCount := 0;
  
  for i := 0 to Length(timeEntries) - 1 do
  begin
    if (timeEntries[i].endTime > 0) and (Int(timeEntries[i].startTime) = targetDate) then
    begin
      hoursWorked := hoursWorked + CalculateDurationHours(timeEntries[i].startTime, timeEntries[i].endTime);
      Inc(tasksCount);
    end;
  end;
  
  result.date := targetDate;
  result.hoursWorked := hoursWorked;
  result.tasksCompleted := tasksCount;
  if tasksCount > 0 then
    result.averageHoursPerTask := hoursWorked / tasksCount
  else
    result.averageHoursPerTask := 0;
end;

function TTaskTimeTrackingClass.GetWeeklyTimeSummary(startDate: TDateTime): TTimeSummary;
var
  i: integer;
  totalHours: double;
  endDate: TDateTime;
begin
  FillChar(result, SizeOf(result), 0);
  endDate := startDate + 7;
  totalHours := 0;
  
  for i := 0 to Length(timeEntries) - 1 do
  begin
    if (timeEntries[i].endTime > 0) and 
       (timeEntries[i].startTime >= startDate) and 
       (timeEntries[i].startTime < endDate) then
    begin
      totalHours := totalHours + CalculateDurationHours(timeEntries[i].startTime, timeEntries[i].endTime);
    end;
  end;
  
  result.totalHours := totalHours;
end;

function TTaskTimeTrackingClass.GetMonthlyTimeSummary(year, month: integer): TTimeSummary;
var
  i: integer;
  totalHours: double;
  startDate, endDate: TDateTime;
begin
  FillChar(result, SizeOf(result), 0);
  startDate := EncodeDate(year, month, 1);
  
  if month = 12 then
    endDate := EncodeDate(year + 1, 1, 1)
  else
    endDate := EncodeDate(year, month + 1, 1);
  
  totalHours := 0;
  
  for i := 0 to Length(timeEntries) - 1 do
  begin
    if (timeEntries[i].endTime > 0) and 
       (timeEntries[i].startTime >= startDate) and 
       (timeEntries[i].startTime < endDate) then
    begin
      totalHours := totalHours + CalculateDurationHours(timeEntries[i].startTime, timeEntries[i].endTime);
    end;
  end;
  
  result.totalHours := totalHours;
end;

function TTaskTimeTrackingClass.GetAverageTimePerTask(): double;
begin
  if Length(timeEntries) > 0 then
    result := GetTotalTimeSummary().totalHours / Length(timeEntries)
  else
    result := 0;
end;

function TTaskTimeTrackingClass.GetMostTimeSpentTask(): integer;
var
  i: integer;
  maxHours: double;
  maxTaskId: integer;
begin
  maxHours := 0;
  maxTaskId := -1;
  
  for i := 0 to Length(timeEntries) - 1 do
  begin
    if timeEntries[i].endTime > 0 then
    begin
      if CalculateDurationHours(timeEntries[i].startTime, timeEntries[i].endTime) > maxHours then
      begin
        maxHours := CalculateDurationHours(timeEntries[i].startTime, timeEntries[i].endTime);
        maxTaskId := timeEntries[i].taskId;
      end;
    end;
  end;
  
  result := maxTaskId;
end;

function TTaskTimeTrackingClass.GetTimeVariance(taskId: integer): double;
var
  summary: TTimeSummary;
begin
  summary := GetTaskTimeSummary(taskId);
  if summary.estimatedHours > 0 then
    result := ((summary.totalHours - summary.estimatedHours) / summary.estimatedHours) * 100.0
  else
    result := 0;
end;

function TTaskTimeTrackingClass.GetProductivityTrend(days: integer): TDailyTimeSummaryArray;
var
  i, count: integer;
  currentDate: TDateTime;
begin
  SetLength(result, 0);
  count := 0;
  currentDate := Now();
  
  for i := 0 to days - 1 do
  begin
    SetLength(result, count + 1);
    result[count] := GetDailyTimeSummary(currentDate - i);
    Inc(count);
  end;
end;

function TTaskTimeTrackingClass.GetTotalTimeTracked(): double;
begin
  result := GetTotalTimeSummary().totalHours;
end;

function TTaskTimeTrackingClass.GetTimeEntryCount(): integer;
begin
  result := Length(timeEntries);
end;

function TTaskTimeTrackingClass.GetAverageTaskDuration(): double;
begin
  result := GetAverageTimePerTask();
end;

procedure TTaskTimeTrackingClass.SelfTest();
var
  trackingMgr: TTaskTimeTrackingClass;
  entryId1, entryId2: integer;
  summary: TTimeSummary;
begin
  trackingMgr := TTaskTimeTrackingClass.Create();
  try
    WriteLn('');
    WriteLn('=== Task Time Tracking Self Test ===');
    
    { Test time entry creation }
    entryId1 := trackingMgr.StartTimeTracking(1, 'Working on task 1');
    Sleep(100);
    trackingMgr.StopTimeTracking(entryId1);
    
    entryId2 := trackingMgr.StartTimeTracking(2, 'Working on task 2');
    Sleep(100);
    trackingMgr.StopTimeTracking(entryId2);
    
    WriteLn(Format('Created %d time entries', [trackingMgr.GetTimeEntryCount()]));
    
    { Test summary }
    summary := trackingMgr.GetTotalTimeSummary();
    WriteLn(Format('Total hours tracked: %.2f', [summary.totalHours]));
    WriteLn(Format('Average time per task: %.4f hours', [trackingMgr.GetAverageTimePerTask()]));
    
    { Test daily summary }
    WriteLn(Format('Most time spent on task ID: %d', [trackingMgr.GetMostTimeSpentTask()]));
    
    WriteLn('=== Task Time Tracking Self Test Complete ===');
  finally
    trackingMgr.Free();
  end;
end;

end.
