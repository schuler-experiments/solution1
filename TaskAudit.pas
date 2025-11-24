
unit TaskAudit;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils;

type
  { Audit action types }
  TAuditActionType = (aaCreated, aaModified, aaStatusChanged, aaAssigned, 
                       aaDeleted, aaCompleted, aaPriorityChanged, aaDueDateChanged);

  { Audit log entry }
  TAuditLogEntry = record
    id: integer;
    taskId: integer;
    action: TAuditActionType;
    timestamp: TDateTime;
    changedBy: string;
    oldValue: string;
    newValue: string;
    description: string;
  end;

  { Dynamic array type for audit entries }
  TAuditEntryArray = array of TAuditLogEntry;

  { Dynamic array types for results }
  TIntegerArray = array of integer;
  TStringArray = array of string;

  { Audit log manager class }
  TTaskAuditManagerClass = class
  private
    auditLog: TAuditEntryArray;
    nextId: integer;
    function FindEntryIndex(id: integer): integer;
    function ActionTypeToString(action: TAuditActionType): string;
    function StringToActionType(s: string): TAuditActionType;
  public
    constructor Create();
    destructor Destroy(); override;
    
    { Core audit operations }
    function LogAction(taskId: integer; action: TAuditActionType; 
                      changedBy: string; description: string): integer;
    function LogChange(taskId: integer; changedBy: string; 
                      fieldName, oldValue, newValue: string): integer;
    function GetAuditEntry(id: integer): TAuditLogEntry;
    function GetTaskAuditLog(taskId: integer): TAuditEntryArray;
    
    { Query operations }
    function GetAuditCount(): integer;
    function GetAuditCountForTask(taskId: integer): integer;
    function GetAuditEntriesByType(action: TAuditActionType): TAuditEntryArray;
    function GetAuditEntriesByUser(userName: string): TAuditEntryArray;
    function GetAuditEntriesInDateRange(startDate, endDate: TDateTime): TAuditEntryArray;
    
    { Data access for analysis }
    function GetAllAuditEntries(): TAuditEntryArray;
    
    { Cleanup operations }
    procedure ClearAuditLog();
    procedure DeleteTaskAuditLog(taskId: integer);
    procedure DeleteOldEntries(beforeDate: TDateTime);
    
    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskAuditManagerClass.Create();
begin
  inherited Create();
  SetLength(auditLog, 0);
  nextId := 1;
end;

destructor TTaskAuditManagerClass.Destroy();
begin
  SetLength(auditLog, 0);
  inherited Destroy();
end;

function TTaskAuditManagerClass.FindEntryIndex(id: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].id = id then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTaskAuditManagerClass.ActionTypeToString(action: TAuditActionType): string;
begin
  case action of
    aaCreated: Result := 'CREATED';
    aaModified: Result := 'MODIFIED';
    aaStatusChanged: Result := 'STATUS_CHANGED';
    aaAssigned: Result := 'ASSIGNED';
    aaDeleted: Result := 'DELETED';
    aaCompleted: Result := 'COMPLETED';
    aaPriorityChanged: Result := 'PRIORITY_CHANGED';
    aaDueDateChanged: Result := 'DUE_DATE_CHANGED';
  else
    Result := 'UNKNOWN';
  end;
end;

function TTaskAuditManagerClass.StringToActionType(s: string): TAuditActionType;
begin
  s := UpperCase(s);
  if s = 'CREATED' then Result := aaCreated
  else if s = 'MODIFIED' then Result := aaModified
  else if s = 'STATUS_CHANGED' then Result := aaStatusChanged
  else if s = 'ASSIGNED' then Result := aaAssigned
  else if s = 'DELETED' then Result := aaDeleted
  else if s = 'COMPLETED' then Result := aaCompleted
  else if s = 'PRIORITY_CHANGED' then Result := aaPriorityChanged
  else if s = 'DUE_DATE_CHANGED' then Result := aaDueDateChanged
  else Result := aaModified;
end;

function TTaskAuditManagerClass.LogAction(taskId: integer; action: TAuditActionType;
                                         changedBy: string; description: string): integer;
begin
  SetLength(auditLog, Length(auditLog) + 1);
  with auditLog[Length(auditLog) - 1] do
  begin
    id := nextId;
    taskId := taskId;
    action := action;
    timestamp := Now();
    changedBy := changedBy;
    oldValue := '';
    newValue := '';
    description := description;
  end;
  Result := nextId;
  Inc(nextId);
end;

function TTaskAuditManagerClass.LogChange(taskId: integer; changedBy: string;
                                         fieldName, oldValue, newValue: string): integer;
begin
  SetLength(auditLog, Length(auditLog) + 1);
  with auditLog[Length(auditLog) - 1] do
  begin
    id := nextId;
    taskId := taskId;
    action := aaModified;
    timestamp := Now();
    changedBy := changedBy;
    description := fieldName;
    oldValue := oldValue;
    newValue := newValue;
  end;
  Result := nextId;
  Inc(nextId);
end;

function TTaskAuditManagerClass.GetAuditEntry(id: integer): TAuditLogEntry;
var
  idx: integer;
begin
  idx := FindEntryIndex(id);
  if idx >= 0 then
    Result := auditLog[idx]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TTaskAuditManagerClass.GetTaskAuditLog(taskId: integer): TAuditEntryArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].taskId = taskId then
    begin
      SetLength(Result, count + 1);
      Result[count] := auditLog[i];
      Inc(count);
    end;
  end;
end;

function TTaskAuditManagerClass.GetAuditCount(): integer;
begin
  Result := Length(auditLog);
end;

function TTaskAuditManagerClass.GetAuditCountForTask(taskId: integer): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].taskId = taskId then
      Inc(count);
  end;
  Result := count;
end;

function TTaskAuditManagerClass.GetAuditEntriesByType(action: TAuditActionType): TAuditEntryArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].action = action then
    begin
      SetLength(Result, count + 1);
      Result[count] := auditLog[i];
      Inc(count);
    end;
  end;
end;

function TTaskAuditManagerClass.GetAuditEntriesByUser(userName: string): TAuditEntryArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].changedBy = userName then
    begin
      SetLength(Result, count + 1);
      Result[count] := auditLog[i];
      Inc(count);
    end;
  end;
end;

function TTaskAuditManagerClass.GetAuditEntriesInDateRange(startDate, endDate: TDateTime): TAuditEntryArray;
var
  i, count: integer;
begin
  SetLength(Result, 0);
  count := 0;
  for i := 0 to Length(auditLog) - 1 do
  begin
    if (auditLog[i].timestamp >= startDate) and (auditLog[i].timestamp <= endDate) then
    begin
      SetLength(Result, count + 1);
      Result[count] := auditLog[i];
      Inc(count);
    end;
  end;
end;

function TTaskAuditManagerClass.GetAllAuditEntries(): TAuditEntryArray;
begin
  SetLength(Result, Length(auditLog));
  if Length(auditLog) > 0 then
    Move(auditLog[0], Result[0], Length(auditLog) * SizeOf(TAuditLogEntry));
end;

procedure TTaskAuditManagerClass.ClearAuditLog();
begin
  SetLength(auditLog, 0);
  nextId := 1;
end;

procedure TTaskAuditManagerClass.DeleteTaskAuditLog(taskId: integer);
var
  i, j: integer;
  newLog: TAuditEntryArray;
begin
  SetLength(newLog, 0);
  j := 0;
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].taskId <> taskId then
    begin
      SetLength(newLog, j + 1);
      newLog[j] := auditLog[i];
      Inc(j);
    end;
  end;
  SetLength(auditLog, 0);
  auditLog := newLog;
end;

procedure TTaskAuditManagerClass.DeleteOldEntries(beforeDate: TDateTime);
var
  i, j: integer;
  newLog: TAuditEntryArray;
begin
  SetLength(newLog, 0);
  j := 0;
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].timestamp >= beforeDate then
    begin
      SetLength(newLog, j + 1);
      newLog[j] := auditLog[i];
      Inc(j);
    end;
  end;
  SetLength(auditLog, 0);
  auditLog := newLog;
end;

procedure TTaskAuditManagerClass.SelfTest();
var
  entryId: integer;
  entries: TAuditEntryArray;
begin
  WriteLn('Testing Audit Trail Manager...');
  
  entryId := LogAction(1, aaCreated, 'admin', 'Task created');
  WriteLn('Logged action, ID: ', entryId);
  
  LogChange(1, 'admin', 'title', 'Old Title', 'New Title');
  LogChange(1, 'user1', 'status', 'pending', 'active');
  LogAction(1, aaCompleted, 'user1', 'Task completed');
  
  LogAction(2, aaCreated, 'admin', 'Task created');
  LogChange(2, 'user2', 'priority', 'low', 'high');
  
  WriteLn('Audit log count: ', GetAuditCount());
  WriteLn('Audit log for task 1: ', GetAuditCountForTask(1), ' entries');
  WriteLn('Audit log for task 2: ', GetAuditCountForTask(2), ' entries');
  
  entries := GetTaskAuditLog(1);
  WriteLn('Retrieved ', Length(entries), ' entries for task 1');
  SetLength(entries, 0);
  
  WriteLn('Audit Trail Manager test completed!');
end;

end.
