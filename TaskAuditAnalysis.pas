unit TaskAuditAnalysis;

{$mode objfpc}

interface

uses
  SysUtils,
  TaskAudit;

type
  TTaskAuditAnalysisClass = class
  private
    auditMgr: TTaskAuditManagerClass;
  public
    constructor Create(mgr: TTaskAuditManagerClass);
    destructor Destroy();
    function GetLastModificationDate(taskId: integer): TDateTime;
    function GetLastModificationBy(taskId: integer): string;
    function GetModificationCount(taskId: integer): integer;
    function GetMostActiveTasks(limit: integer): TIntegerArray;
    function GetMostActiveUsers(): TStringArray;
    function GetAuditSummary(taskId: integer): string;
    procedure SelfTest();
  end;

implementation

type
  TTaskCountRecord = record
    taskId: integer;
    count: integer;
  end;

  TUserCountRecord = record
    userName: string;
    count: integer;
  end;

constructor TTaskAuditAnalysisClass.Create(mgr: TTaskAuditManagerClass);
begin
  auditMgr := mgr;
end;

destructor TTaskAuditAnalysisClass.Destroy();
begin
  inherited Destroy();
end;

function TTaskAuditAnalysisClass.GetLastModificationDate(taskId: integer): TDateTime;
var
  entries: TAuditEntryArray;
begin
  entries := auditMgr.GetTaskAuditLog(taskId);
  if Length(entries) > 0 then
    Result := entries[Length(entries) - 1].timestamp
  else
    Result := 0;
  SetLength(entries, 0);
end;

function TTaskAuditAnalysisClass.GetLastModificationBy(taskId: integer): string;
var
  entries: TAuditEntryArray;
begin
  entries := auditMgr.GetTaskAuditLog(taskId);
  if Length(entries) > 0 then
    Result := entries[Length(entries) - 1].changedBy
  else
    Result := '';
  SetLength(entries, 0);
end;

function TTaskAuditAnalysisClass.GetModificationCount(taskId: integer): integer;
var
  entries: TAuditEntryArray;
begin
  entries := auditMgr.GetTaskAuditLog(taskId);
  Result := Length(entries);
  SetLength(entries, 0);
end;

function TTaskAuditAnalysisClass.GetMostActiveTasks(limit: integer): TIntegerArray;
var
  counts: array of TTaskCountRecord;
  i, j: integer;
  entries: TAuditEntryArray;
  newCount: boolean;
  temp: TTaskCountRecord;
begin
  SetLength(counts, 0);
  entries := auditMgr.GetAllAuditEntries();
  
  { Count occurrences of each task }
  for i := 0 to Length(entries) - 1 do
  begin
    newCount := true;
    for j := 0 to Length(counts) - 1 do
    begin
      if counts[j].taskId = entries[i].taskId then
      begin
        Inc(counts[j].count);
        newCount := false;
        Break;
      end;
    end;
    
    if newCount then
    begin
      SetLength(counts, Length(counts) + 1);
      counts[Length(counts) - 1].taskId := entries[i].taskId;
      counts[Length(counts) - 1].count := 1;
    end;
  end;
  
  { Sort by count descending }
  for i := 0 to Length(counts) - 2 do
  begin
    for j := i + 1 to Length(counts) - 1 do
    begin
      if counts[j].count > counts[i].count then
      begin
        { Swap }
        temp := counts[i];
        counts[i] := counts[j];
        counts[j] := temp;
      end;
    end;
  end;
  
  { Return top N task IDs }
  if limit > Length(counts) then
    limit := Length(counts);
  
  SetLength(Result, limit);
  for i := 0 to limit - 1 do
    Result[i] := counts[i].taskId;
  
  SetLength(counts, 0);
  SetLength(entries, 0);
end;

function TTaskAuditAnalysisClass.GetMostActiveUsers(): TStringArray;
var
  counts: array of TUserCountRecord;
  i, j: integer;
  entries: TAuditEntryArray;
  newUser: boolean;
  temp: TUserCountRecord;
begin
  SetLength(counts, 0);
  entries := auditMgr.GetAllAuditEntries();
  
  { Count occurrences of each user }
  for i := 0 to Length(entries) - 1 do
  begin
    newUser := true;
    for j := 0 to Length(counts) - 1 do
    begin
      if counts[j].userName = entries[i].changedBy then
      begin
        Inc(counts[j].count);
        newUser := false;
        Break;
      end;
    end;
    
    if newUser then
    begin
      SetLength(counts, Length(counts) + 1);
      counts[Length(counts) - 1].userName := entries[i].changedBy;
      counts[Length(counts) - 1].count := 1;
    end;
  end;
  
  { Sort by count descending }
  for i := 0 to Length(counts) - 2 do
  begin
    for j := i + 1 to Length(counts) - 1 do
    begin
      if counts[j].count > counts[i].count then
      begin
        temp := counts[i];
        counts[i] := counts[j];
        counts[j] := temp;
      end;
    end;
  end;
  
  { Return user names }
  SetLength(Result, Length(counts));
  for i := 0 to Length(counts) - 1 do
    Result[i] := counts[i].userName;
  
  SetLength(counts, 0);
  SetLength(entries, 0);
end;

function TTaskAuditAnalysisClass.GetAuditSummary(taskId: integer): string;
var
  count: integer;
  lastDate: TDateTime;
  lastUser: string;
begin
  count := GetModificationCount(taskId);
  lastDate := GetLastModificationDate(taskId);
  lastUser := GetLastModificationBy(taskId);
  
  Result := 'Task ID: ' + IntToStr(taskId) + ', ' +
            'Modifications: ' + IntToStr(count) + ', ' +
            'Last Modified: ' + DateTimeToStr(lastDate) + ', ' +
            'Last Modified By: ' + lastUser;
end;

procedure TTaskAuditAnalysisClass.SelfTest();
var
  mgr: TTaskAuditManagerClass;
  analysis: TTaskAuditAnalysisClass;
begin
  mgr := TTaskAuditManagerClass.Create();
  analysis := TTaskAuditAnalysisClass.Create(mgr);
  
  { Add some test audit entries }
  mgr.LogAction(1, aaCreated, 'TestUser', 'Task created');
  mgr.LogChange(1, 'TestUser', 'title', 'Old Title', 'New Title');
  mgr.LogAction(1, aaCompleted, 'TestUser', 'Task completed');
  
  WriteLn('Audit Summary: ', analysis.GetAuditSummary(1));
  WriteLn('Test completed successfully');
  
  analysis.Free();
  mgr.Free();
end;

end.
