
unit TaskTypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils;

const
  MAX_TASKS = 1000;
  DATA_FILE = 'tasks.dat';
  VERSION = '2.0.0'; // Updated version with new features

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
    Category: string; // NEW: Category/Project name for organizing tasks
    DependsOnIDs: string; // NEW: Comma-separated IDs of prerequisite tasks
    EstimatedHours: double; // NEW: Estimated time to complete (in hours)
    ActualHours: double; // NEW: Actual time spent (in hours)
    LastModifiedDate: TDateTime; // NEW: Track when task was last modified
  end;
  
  // Dynamic array of tasks
  TTaskArray = array of TTask;
  
  // Helper dynamic arrays
  TIntegerArray = array of integer;
  TStringArray = array of string;
  
  // Search criteria for filtering tasks
  TSearchCriteria = record
    SearchTitle: string;
    SearchDescription: string;
    FilterStatus: TTaskStatus;
    FilterPriority: TTaskPriority;
    UseStatusFilter: boolean;
    UsePriorityFilter: boolean;
    SearchTags: string;
    SearchCategory: string; // NEW: Filter by category
  end;
  
  // Statistics about tasks
  TTaskStatistics = record
    TotalTasks: integer;
    ActiveTasks: integer;
    CompletedTasks: integer;
    CancelledTasks: integer;
    HighPriorityTasks: integer;
    OverdueTasks: integer;
    TotalEstimatedHours: double; // NEW: Total estimated hours
    TotalActualHours: double; // NEW: Total actual hours spent
    TasksWithDependencies: integer; // NEW: Tasks that have dependencies
  end;
  
  // NEW: Category statistics
  TCategoryStatistics = record
    CategoryName: string;
    TotalTasks: integer;
    CompletedTasks: integer;
    ActiveTasks: integer;
    EstimatedHours: double;
    ActualHours: double;
  end;
  
  // NEW: Array of category statistics
  TCategoryStatisticsArray = array of TCategoryStatistics;
  
  // NEW: History entry for audit trail
  THistoryEntry = record
    TaskID: integer;
    ChangeDate: TDateTime;
    FieldName: string;
    OldValue: string;
    NewValue: string;
    ChangeDescription: string;
  end;
  
  // NEW: Array of history entries
  THistoryArray = array of THistoryEntry;

// Helper functions for type conversions and display
function PriorityToString(aPriority: TTaskPriority): string;
function StringToPriority(const aStr: string): TTaskPriority;
function StatusToString(aStatus: TTaskStatus): string;
function StringToStatus(const aStr: string): TTaskStatus;
function DateTimeToStr(aDateTime: TDateTime): string;
function StrToDateTimeDef(const aStr: string; aDefault: TDateTime): TDateTime;

// NEW: Helper functions for dependencies
function HasDependencies(const aTask: TTask): boolean;
function GetDependencyIDs(const aTask: TTask): TIntegerArray;
function AddDependency(var aTask: TTask; aDependencyID: integer): boolean;
function RemoveDependency(var aTask: TTask; aDependencyID: integer): boolean;

// NEW: Helper functions for categories
function GetUniqueCategories(const aTasks: TTaskArray): TStringArray;

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

// NEW: Check if task has dependencies
function HasDependencies(const aTask: TTask): boolean;
begin
  Result := Trim(aTask.DependsOnIDs) <> '';
end;

// NEW: Get array of dependency IDs
function GetDependencyIDs(const aTask: TTask): TIntegerArray;
var
  depStr: string;
  parts: array of string;
  i, depID: integer;
  resultList: array of integer;
begin
  SetLength(resultList, 0);
  depStr := Trim(aTask.DependsOnIDs);
  
  if depStr = '' then
  begin
    Result := resultList;
    Exit;
  end;
  
  // Split by comma
  SetLength(parts, 0);
  while Pos(',', depStr) > 0 do
  begin
    SetLength(parts, Length(parts) + 1);
    parts[High(parts)] := Trim(Copy(depStr, 1, Pos(',', depStr) - 1));
    Delete(depStr, 1, Pos(',', depStr));
  end;
  
  if Trim(depStr) <> '' then
  begin
    SetLength(parts, Length(parts) + 1);
    parts[High(parts)] := Trim(depStr);
  end;
  
  // Convert to integers
  for i := 0 to High(parts) do
  begin
    if TryStrToInt(parts[i], depID) then
    begin
      SetLength(resultList, Length(resultList) + 1);
      resultList[High(resultList)] := depID;
    end;
  end;
  
  Result := resultList;
end;

// NEW: Add a dependency to a task
function AddDependency(var aTask: TTask; aDependencyID: integer): boolean;
var
  deps: array of integer;
  i: integer;
  depStr: string;
begin
  Result := false;
  
  // Check if dependency already exists
  deps := GetDependencyIDs(aTask);
  for i := 0 to High(deps) do
  begin
    if deps[i] = aDependencyID then
      Exit; // Already exists
  end;
  
  // Add new dependency
  if Trim(aTask.DependsOnIDs) = '' then
    aTask.DependsOnIDs := IntToStr(aDependencyID)
  else
    aTask.DependsOnIDs := aTask.DependsOnIDs + ',' + IntToStr(aDependencyID);
  
  Result := true;
end;

// NEW: Remove a dependency from a task
function RemoveDependency(var aTask: TTask; aDependencyID: integer): boolean;
var
  deps: array of integer;
  i: integer;
  newDepStr: string;
begin
  Result := false;
  newDepStr := '';
  
  deps := GetDependencyIDs(aTask);
  for i := 0 to High(deps) do
  begin
    if deps[i] <> aDependencyID then
    begin
      if newDepStr <> '' then
        newDepStr := newDepStr + ',';
      newDepStr := newDepStr + IntToStr(deps[i]);
    end
    else
      Result := true; // Found and removed
  end;
  
  aTask.DependsOnIDs := newDepStr;
end;

// NEW: Get unique categories from task array
function GetUniqueCategories(const aTasks: TTaskArray): TStringArray;
var
  i, j: integer;
  cat: string;
  found: boolean;
  categories: array of string;
begin
  SetLength(categories, 0);
  
  for i := 0 to High(aTasks) do
  begin
    cat := Trim(aTasks[i].Category);
    if cat = '' then
      Continue;
    
    // Check if category already in list
    found := false;
    for j := 0 to High(categories) do
    begin
      if categories[j] = cat then
      begin
        found := true;
        Break;
      end;
    end;
    
    // Add if not found
    if not found then
    begin
      SetLength(categories, Length(categories) + 1);
      categories[High(categories)] := cat;
    end;
  end;
  
  Result := categories;
end;

end.
