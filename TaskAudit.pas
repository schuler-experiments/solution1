
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

  { Helper record for counting tasks }
  TTaskCountRecord = record
    taskId: integer;
    count: integer;
  end;

  { Helper record for counting users }
  TUserCountRecord = record
    userName: string;
    count: integer;
  end;

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
    
    { Analysis operations }
    function GetLastModificationDate(taskId: integer): TDateTime;
    function GetLastModificationBy(taskId: integer): string;
    function GetModificationCount(taskId: integer): integer;
    function GetMostActiveTasks(limit: integer): TIntegerArray;
    function GetMostActiveUsers(): TStringArray;
    function GetAuditSummary(taskId: integer): string;
    
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
var
  newEntry: TAuditLogEntry;
begin
  newEntry.id := nextId;
  newEntry.taskId := taskId;
  newEntry.action := action;
  newEntry.timestamp := Now();
  newEntry.changedBy := changedBy;
  newEntry.oldValue := '';
  newEntry.newValue := '';
  newEntry.description := description;
  
  SetLength(auditLog, Length(auditLog) + 1);
  auditLog[Length(auditLog) - 1] := newEntry;
  
  Result := nextId;
  Inc(nextId);
end;

function TTaskAuditManagerClass.LogChange(taskId: integer; changedBy: string;
                                         fieldName, oldValue, newValue: string): integer;
var
  newEntry: TAuditLogEntry;
begin
  newEntry.id := nextId;
  newEntry.taskId := taskId;
  newEntry.action := aaModified;
  newEntry.timestamp := Now();
  newEntry.changedBy := changedBy;
  newEntry.oldValue := fieldName + ': ' + oldValue;
  newEntry.newValue := fieldName + ': ' + newValue;
  newEntry.description := 'Field changed: ' + fieldName;
  
  SetLength(auditLog, Length(auditLog) + 1);
  auditLog[Length(auditLog) - 1] := newEntry;
  
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
  begin
    FillByte(Result, SizeOf(Result), 0);
    Result.id := -1;
  end;
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
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].taskId = taskId then
      Inc(Result);
  end;
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

function TTaskAuditManagerClass.GetLastModificationDate(taskId: integer): TDateTime;
var
  i: integer;
  lastDate: TDateTime;
begin
  lastDate := 0;
  
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].taskId = taskId then
    begin
      if auditLog[i].timestamp > lastDate then
        lastDate := auditLog[i].timestamp;
    end;
  end;
  
  Result := lastDate;
end;

function TTaskAuditManagerClass.GetLastModificationBy(taskId: integer): string;
var
  i: integer;
  lastDate: TDateTime;
  lastUser: string;
begin
  lastDate := 0;
  lastUser := '';
  
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].taskId = taskId then
    begin
      if auditLog[i].timestamp > lastDate then
      begin
        lastDate := auditLog[i].timestamp;
        lastUser := auditLog[i].changedBy;
      end;
    end;
  end;
  
  Result := lastUser;
end;

function TTaskAuditManagerClass.GetModificationCount(taskId: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(auditLog) - 1 do
  begin
    if auditLog[i].taskId = taskId then
      Inc(Result);
  end;
end;

function TTaskAuditManagerClass.GetMostActiveTasks(limit: integer): TIntegerArray;
var
  taskCounts: array of TTaskCountRecord;
  i, j, resultCount: integer;
  found: boolean;
  temp: TTaskCountRecord;
begin
  SetLength(taskCounts, 0);
  SetLength(Result, 0);
  
  { Count occurrences of each task }
  for i := 0 to Length(auditLog) - 1 do
  begin
    found := False;
    for j := 0 to Length(taskCounts) - 1 do
    begin
      if taskCounts[j].taskId = auditLog[i].taskId then
      begin
        Inc(taskCounts[j].count);
        found := True;
        Break;
      end;
    end;
    
    if not found then
    begin
      SetLength(taskCounts, Length(taskCounts) + 1);
      taskCounts[Length(taskCounts) - 1].taskId := auditLog[i].taskId;
      taskCounts[Length(taskCounts) - 1].count := 1;
    end;
  end;
  
  { Sort by count descending }
  for i := 0 to Length(taskCounts) - 2 do
  begin
    for j := i + 1 to Length(taskCounts) - 1 do
    begin
      if taskCounts[j].count > taskCounts[i].count then
      begin
        temp := taskCounts[i];
        taskCounts[i] := taskCounts[j];
        taskCounts[j] := temp;
      end;
    end;
  end;
  
  { Return limited results }
  resultCount := Length(taskCounts);
  if resultCount > limit then
    resultCount := limit;
  
  SetLength(Result, resultCount);
  for i := 0 to resultCount - 1 do
    Result[i] := taskCounts[i].taskId;
  
  SetLength(taskCounts, 0);
end;

function TTaskAuditManagerClass.GetMostActiveUsers(): TStringArray;
var
  userCounts: array of TUserCountRecord;
  i, j, resultCount: integer;
  found: boolean;
  temp: TUserCountRecord;
begin
  SetLength(userCounts, 0);
  SetLength(Result, 0);
  
  { Count occurrences of each user }
  for i := 0 to Length(auditLog) - 1 do
  begin
    found := False;
    for j := 0 to Length(userCounts) - 1 do
    begin
      if userCounts[j].userName = auditLog[i].changedBy then
      begin
        Inc(userCounts[j].count);
        found := True;
        Break;
      end;
    end;
    
    if not found then
    begin
      SetLength(userCounts, Length(userCounts) + 1);
      userCounts[Length(userCounts) - 1].userName := auditLog[i].changedBy;
      userCounts[Length(userCounts) - 1].count := 1;
    end;
  end;
  
  { Sort by count descending }
  for i := 0 to Length(userCounts) - 2 do
  begin
    for j := i + 1 to Length(userCounts) - 1 do
    begin
      if userCounts[j].count > userCounts[i].count then
      begin
        temp := userCounts[i];
        userCounts[i] := userCounts[j];
        userCounts[j] := temp;
      end;
    end;
  end;
  
  SetLength(Result, Length(userCounts));
  for i := 0 to Length(userCounts) - 1 do
    Result[i] := userCounts[i].userName;
  
  SetLength(userCounts, 0);
end;

function TTaskAuditManagerClass.GetAuditSummary(taskId: integer): string;
var
  entries: TAuditEntryArray;
  i: integer;
begin
  Result := '';
  entries := GetTaskAuditLog(taskId);
  
  if Length(entries) = 0 then
  begin
    Result := 'No audit entries for task ' + IntToStr(taskId);
  end
  else
  begin
    Result := 'Audit Summary for Task ' + IntToStr(taskId) + ':' + #10;
    Result += 'Total Actions: ' + IntToStr(Length(entries)) + #10;
    
    for i := 0 to Length(entries) - 1 do
    begin
      Result += '[' + DateTimeToStr(entries[i].timestamp) + '] ';
      Result += entries[i].changedBy + ' - ';
      Result += ActionTypeToString(entries[i].action) + #10;
    end;
  end;
  
  SetLength(entries, 0);
end;

procedure TTaskAuditManagerClass.ClearAuditLog();
begin
  SetLength(auditLog, 0);
  nextId := 1;
end;

procedure TTaskAuditManagerClass.DeleteTaskAuditLog(taskId: integer);
var
  i, j: integer;
begin
  for i := Length(auditLog) - 1 downto 0 do
  begin
    if auditLog[i].taskId = taskId then
    begin
      for j := i to Length(auditLog) - 2 do
        auditLog[j] := auditLog[j + 1];
      SetLength(auditLog, Length(auditLog) - 1);
    end;
  end;
end;

procedure TTaskAuditManagerClass.DeleteOldEntries(beforeDate: TDateTime);
var
  i, j: integer;
begin
  for i := Length(auditLog) - 1 downto 0 do
  begin
    if auditLog[i].timestamp < beforeDate then
    begin
      for j := i to Length(auditLog) - 2 do
        auditLog[j] := auditLog[j + 1];
      SetLength(auditLog, Length(auditLog) - 1);
    end;
  end;
end;

procedure TTaskAuditManagerClass.SelfTest();
var
  entryId: integer;
  entries: TAuditEntryArray;
  users: TStringArray;
  dates: TIntegerArray;
  i: integer;
begin
  WriteLn('Testing Audit Trail Manager...');
  
  { Test logging actions }
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
  
  { Test retrieving audit entries }
  entries := GetTaskAuditLog(1);
  WriteLn('Retrieved ', Length(entries), ' entries for task 1');
  SetLength(entries, 0);
  
  { Test modification tracking }
  WriteLn('Last modified by: ', GetLastModificationBy(1));
  WriteLn('Modification count: ', GetModificationCount(1));
  
  { Test user tracking }
  users := GetMostActiveUsers();
  WriteLn('Most active users: ');
  for i := 0 to Length(users) - 1 do
    WriteLn('  - ', users[i]);
  SetLength(users, 0);
  
  { Test task activity }
  dates := GetMostActiveTasks(2);
  WriteLn('Most active tasks: ');
  for i := 0 to Length(dates) - 1 do
    WriteLn('  - Task ', dates[i]);
  SetLength(dates, 0);
  
  WriteLn('Audit Trail Manager test completed!');
end;

end.
