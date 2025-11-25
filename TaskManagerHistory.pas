
unit TaskManagerHistory;

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
  TaskManagerSubtasks;

type
  TTaskManagerHistory = class(TTaskManagerSubtasks)
  public
    function recordTaskChange(taskId: integer; const fieldName, oldValue, newValue, changedBy: string): boolean;
    function getTaskHistory(taskId: integer): THistoryArray;
    function getHistoryEntryCount(taskId: integer): integer;
    function getHistorySince(taskId: integer; const since: TDateTime): THistoryArray;
    function getChangesBy(const changedBy: string): THistoryArray;
    function getLastModificationTime(taskId: integer): TDateTime;
    function getTaskAuditTrail(taskId: integer): string;
    procedure clearHistory(taskId: integer);
  end;

implementation

function TTaskManagerHistory.recordTaskChange(taskId: integer; const fieldName, oldValue, newValue, changedBy: string): boolean;
var
  task: TTask;
  idx: integer;
  entry: TTaskHistoryEntry;
begin
  result := false;
  if not getTask(taskId, task) then
    exit;

  if length(task.history) >= maxHistoryEntriesPerTask then
  begin
    lastError := 'Maximum history entries exceeded for this task';
    exit;
  end;

  idx := length(task.history);
  setlength(task.history, idx + 1);

  entry.timestamp := now;
  entry.fieldName := fieldName;
  entry.oldValue := oldValue;
  entry.newValue := newValue;
  entry.changedBy := changedBy;

  task.history[idx] := entry;
  result := updateTask(task);
end;

function TTaskManagerHistory.getTaskHistory(taskId: integer): THistoryArray;
var
  task: TTask;
begin
  setlength(result, 0);
  if getTask(taskId, task) then
    result := task.history;
end;

function TTaskManagerHistory.getHistoryEntryCount(taskId: integer): integer;
var
  task: TTask;
begin
  result := 0;
  if getTask(taskId, task) then
    result := length(task.history);
end;

function TTaskManagerHistory.getHistorySince(taskId: integer; const since: TDateTime): THistoryArray;
var
  task: TTask;
  i: integer;
  count: integer;
begin
  setlength(result, 0);
  if not getTask(taskId, task) then
    exit;

  count := 0;
  for i := 0 to length(task.history) - 1 do
  begin
    if task.history[i].timestamp >= since then
    begin
      setlength(result, count + 1);
      result[count] := task.history[i];
      inc(count);
    end;
  end;
end;

function TTaskManagerHistory.getChangesBy(const changedBy: string): THistoryArray;
var
  allTasks: TTaskArray;
  i, j: integer;
  count: integer;
begin
  setlength(result, 0);
  allTasks := getAllTasks;
  count := 0;

  for i := 0 to length(allTasks) - 1 do
  begin
    for j := 0 to length(allTasks[i].history) - 1 do
    begin
      if allTasks[i].history[j].changedBy = changedBy then
      begin
        setlength(result, count + 1);
        result[count] := allTasks[i].history[j];
        inc(count);
      end;
    end;
  end;
end;

function TTaskManagerHistory.getLastModificationTime(taskId: integer): TDateTime;
var
  task: TTask;
  historyLen: integer;
begin
  result := 0;
  if not getTask(taskId, task) then
    exit;

  historyLen := length(task.history);
  if historyLen > 0 then
    result := task.history[historyLen - 1].timestamp
  else
    result := task.createdDate;
end;

function TTaskManagerHistory.getTaskAuditTrail(taskId: integer): string;
var
  task: TTask;
  i: integer;
  trail: string;
begin
  trail := '';
  if not getTask(taskId, task) then
  begin
    result := 'Task not found';
    exit;
  end;

  trail := 'Task ID: ' + intToStr(taskId) + ' (' + task.name + ')' + sLineBreak;
  trail := trail + 'Created: ' + DateTimeToStr(task.createdDate) + sLineBreak;
  trail := trail + sLineBreak + 'Audit Trail:' + sLineBreak;

  if length(task.history) = 0 then
    trail := trail + '(No changes recorded)'
  else
  begin
    for i := 0 to length(task.history) - 1 do
    begin
      trail := trail + DateTimeToStr(task.history[i].timestamp) + ' - ';
      trail := trail + task.history[i].changedBy + ' changed ' + task.history[i].fieldName;
      trail := trail + ' from "' + task.history[i].oldValue + '" to "' + task.history[i].newValue + '"' + sLineBreak;
    end;
  end;

  result := trail;
end;

procedure TTaskManagerHistory.clearHistory(taskId: integer);
var
  task: TTask;
begin
  if getTask(taskId, task) then
  begin
    setlength(task.history, 0);
    updateTask(task);
  end;
end;

end.
