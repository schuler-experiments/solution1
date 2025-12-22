
unit TaskTypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils;

const
  MAX_TASKS = 1000;
  DATA_FILE = 'tasks.dat';
  VERSION = '1.0.0';

type
  // Task priority levels
  TTaskPriority = (tpLow, tpMedium, tpHigh, tpCritical);
  
  // Task status
  TTaskStatus = (tsNew, tsPending, tsInProgress, tsCompleted, tsCancelled);
  
  // Individual task record
  TTask = record
    ID: integer;
    Title: string;
    Description: string;
    Priority: TTaskPriority;
    Status: TTaskStatus;
    CreatedDate: TDateTime;
    DueDate: TDateTime;
    CompletedDate: TDateTime;
    Tags: string; // Comma-separated tags
    IsActive: boolean; // For soft delete
  end;
  
  // Dynamic array of tasks
  TTaskArray = array of TTask;
  
  // Search criteria for filtering tasks
  TSearchCriteria = record
    SearchTitle: string;
    SearchDescription: string;
    FilterStatus: TTaskStatus;
    FilterPriority: TTaskPriority;
    UseStatusFilter: boolean;
    UsePriorityFilter: boolean;
    SearchTags: string;
  end;
  
  // Statistics about tasks
  TTaskStatistics = record
    TotalTasks: integer;
    ActiveTasks: integer;
    CompletedTasks: integer;
    CancelledTasks: integer;
    HighPriorityTasks: integer;
    OverdueTasks: integer;
  end;

// Helper functions for type conversions and display
function PriorityToString(aPriority: TTaskPriority): string;
function StringToPriority(const aStr: string): TTaskPriority;
function StatusToString(aStatus: TTaskStatus): string;
function StringToStatus(const aStr: string): TTaskStatus;
function DateTimeToStr(aDateTime: TDateTime): string;
function StrToDateTimeDef(const aStr: string; aDefault: TDateTime): TDateTime;

implementation

function PriorityToString(aPriority: TTaskPriority): string;
begin
  case aPriority of
    tpLow: Result := 'Low';
    tpMedium: Result := 'Medium';
    tpHigh: Result := 'High';
    tpCritical: Result := 'Critical';
  else
    Result := 'Unknown';
  end;
end;

function StringToPriority(const aStr: string): TTaskPriority;
var
  s: string;
begin
  s := LowerCase(Trim(aStr));
  if s = 'low' then
    Result := tpLow
  else if s = 'medium' then
    Result := tpMedium
  else if s = 'high' then
    Result := tpHigh
  else if s = 'critical' then
    Result := tpCritical
  else
    Result := tpMedium; // Default
end;

function StatusToString(aStatus: TTaskStatus): string;
begin
  case aStatus of
    tsNew: Result := 'New';
    tsPending: Result := 'Pending';
    tsInProgress: Result := 'In Progress';
    tsCompleted: Result := 'Completed';
    tsCancelled: Result := 'Cancelled';
  else
    Result := 'Unknown';
  end;
end;

function StringToStatus(const aStr: string): TTaskStatus;
var
  s: string;
begin
  s := LowerCase(Trim(aStr));
  if s = 'new' then
    Result := tsNew
  else if s = 'pending' then
    Result := tsPending
  else if (s = 'inprogress') or (s = 'in progress') then
    Result := tsInProgress
  else if s = 'completed' then
    Result := tsCompleted
  else if s = 'cancelled' then
    Result := tsCancelled
  else
    Result := tsNew; // Default
end;

function DateTimeToStr(aDateTime: TDateTime): string;
begin
  if aDateTime = 0 then
    Result := ''
  else
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', aDateTime);
end;

function StrToDateTimeDef(const aStr: string; aDefault: TDateTime): TDateTime;
begin
  try
    if Trim(aStr) = '' then
      Result := aDefault
    else
      Result := ScanDateTime('yyyy-mm-dd hh:nn:ss', aStr);
  except
    Result := aDefault;
  end;
end;

end.
