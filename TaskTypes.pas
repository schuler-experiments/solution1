
unit TaskTypes;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils;

type
  TTaskStatus = (tsNotStarted, tsInProgress, tsCompleted, tsOnHold);
  TTaskPriority = (tpLow, tpMedium, tpHigh);

  { Category/Tag type }
  TStringArray = array of string;

  TTask = record
    id: integer;
    title: string;
    description: string;
    status: TTaskStatus;
    priority: TTaskPriority;
    dueDate: TDateTime;
    createdDate: TDateTime;
    completedDate: TDateTime;
    category: string;        { Task category/project }
    tags: TStringArray;       { Multiple tags for this task }
    assignee: string;         { Person assigned to task }
    estimatedHours: double;   { Estimated time to complete }
    actualHours: double;      { Actual time spent }
  end;

  TTaskArray = array of TTask;

  { Task statistics record }
  TTaskStats = record
    totalTasks: integer;
    completedTasks: integer;
    inProgressTasks: integer;
    onHoldTasks: integer;
    notStartedTasks: integer;
    overdueTasks: integer;
  end;
  TTaskArrayRef = ^TTaskArray;

  { Helper functions for converting enums to strings }
  function StatusToString(status: TTaskStatus): string;
  function PriorityToString(priority: TTaskPriority): string;
  function StringToStatus(s: string): TTaskStatus;
  function StringToPriority(s: string): TTaskPriority;
  
  { Task creation with extended fields }
  function CreateTask(id: integer; title, description: string; 
                      priority: TTaskPriority; dueDate: TDateTime): TTask;
  function CreateTaskEx(id: integer; title, description: string; 
                        priority: TTaskPriority; dueDate: TDateTime;
                        category, assignee: string; estHours: double): TTask;
  
  { Tag management utilities }
  function AddTagToTask(var task: TTask; tag: string): boolean;
  function HasTag(task: TTask; tag: string): boolean;
  function RemoveTag(var task: TTask; tag: string): boolean;
  function GetTagsAsString(task: TTask): string;

implementation

function StatusToString(status: TTaskStatus): string;
begin
  case status of
    tsNotStarted: result := 'Not Started';
    tsInProgress: result := 'In Progress';
    tsCompleted: result := 'Completed';
    tsOnHold: result := 'On Hold';
  end;
end;

function PriorityToString(priority: TTaskPriority): string;
begin
  case priority of
    tpLow: result := 'Low';
    tpMedium: result := 'Medium';
    tpHigh: result := 'High';
  end;
end;

function StringToStatus(s: string): TTaskStatus;
begin
  s := LowerCase(Trim(s));
  if s = 'not started' then
    result := tsNotStarted
  else if s = 'in progress' then
    result := tsInProgress
  else if s = 'completed' then
    result := tsCompleted
  else if s = 'on hold' then
    result := tsOnHold
  else
    result := tsNotStarted;
end;

function StringToPriority(s: string): TTaskPriority;
begin
  s := LowerCase(Trim(s));
  if s = 'high' then
    result := tpHigh
  else if s = 'medium' then
    result := tpMedium
  else if s = 'low' then
    result := tpLow
  else
    result := tpMedium;
end;

function CreateTask(id: integer; title, description: string; 
                    priority: TTaskPriority; dueDate: TDateTime): TTask;
begin
  result.id := id;
  result.title := title;
  result.description := description;
  result.status := tsNotStarted;
  result.priority := priority;
  result.dueDate := dueDate;
  result.createdDate := Now();
  result.completedDate := 0;
  result.category := '';
  SetLength(result.tags, 0);
  result.assignee := '';
  result.estimatedHours := 0;
  result.actualHours := 0;
end;

function CreateTaskEx(id: integer; title, description: string; 
                      priority: TTaskPriority; dueDate: TDateTime;
                      category, assignee: string; estHours: double): TTask;
begin
  result := CreateTask(id, title, description, priority, dueDate);
  result.category := category;
  result.assignee := assignee;
  result.estimatedHours := estHours;
end;

function AddTagToTask(var task: TTask; tag: string): boolean;
var
  len: integer;
begin
  { Check if tag already exists }
  if HasTag(task, tag) then
  begin
    result := false;
    exit;
  end;
  
  len := Length(task.tags);
  SetLength(task.tags, len + 1);
  task.tags[len] := tag;
  result := true;
end;

function HasTag(task: TTask; tag: string): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Length(task.tags) - 1 do
  begin
    if task.tags[i] = tag then
    begin
      result := true;
      exit;
    end;
  end;
end;

function RemoveTag(var task: TTask; tag: string): boolean;
var
  i, j: integer;
  found: boolean;
begin
  found := false;
  for i := 0 to Length(task.tags) - 1 do
  begin
    if task.tags[i] = tag then
    begin
      { Shift remaining tags }
      for j := i to Length(task.tags) - 2 do
        task.tags[j] := task.tags[j + 1];
      SetLength(task.tags, Length(task.tags) - 1);
      found := true;
      break;
    end;
  end;
  result := found;
end;

function GetTagsAsString(task: TTask): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to Length(task.tags) - 1 do
  begin
    if i > 0 then
      result := result + ', ';
    result := result + task.tags[i];
  end;
  if Length(result) = 0 then
    result := '(none)';
end;

end.
