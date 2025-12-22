
unit TaskHistory;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Classes, TaskTypes;

type
  // Task history manager class
  TTaskHistory = class
  private
    FHistory: THistoryArray;
    FHistoryFile: string;
    FMaxEntries: integer;
    
    procedure TrimOldEntries;
    procedure WriteString(aStream: TStream; const aStr: string);
    function ReadString(aStream: TStream): string;
  public
    constructor Create(const aHistoryFile: string; aMaxEntries: integer = 1000);
    destructor Destroy; override;
    
    // Add history entry
    procedure LogChange(aTaskID: integer; const aFieldName, aOldValue, aNewValue: string);
    procedure LogTaskCreated(aTaskID: integer; const aTitle: string);
    procedure LogTaskDeleted(aTaskID: integer; const aTitle: string);
    procedure LogStatusChange(aTaskID: integer; aOldStatus, aNewStatus: TTaskStatus);
    procedure LogPriorityChange(aTaskID: integer; aOldPriority, aNewPriority: TTaskPriority);
    
    // Query history
    function GetTaskHistory(aTaskID: integer): THistoryArray;
    function GetRecentHistory(aCount: integer): THistoryArray;
    function GetAllHistory: THistoryArray;
    
    // Persistence
    function SaveHistory: boolean;
    function LoadHistory: boolean;
    
    // Clear history
    procedure ClearHistory;
    procedure ClearTaskHistory(aTaskID: integer);
  end;

implementation

constructor TTaskHistory.Create(const aHistoryFile: string; aMaxEntries: integer);
begin
  inherited Create;
  FHistoryFile := aHistoryFile;
  FMaxEntries := aMaxEntries;
  SetLength(FHistory, 0);
  LoadHistory;
end;

destructor TTaskHistory.Destroy;
begin
  SaveHistory;
  SetLength(FHistory, 0);
  inherited Destroy;
end;

procedure TTaskHistory.TrimOldEntries;
var
  keepCount: integer;
  i: integer;
begin
  if Length(FHistory) <= FMaxEntries then
    Exit;
  
  keepCount := FMaxEntries;
  for i := 0 to keepCount - 1 do
  begin
    FHistory[i] := FHistory[Length(FHistory) - keepCount + i];
  end;
  
  SetLength(FHistory, keepCount);
end;

procedure TTaskHistory.WriteString(aStream: TStream; const aStr: string);
var
  len: integer;
begin
  len := Length(aStr);
  aStream.WriteBuffer(len, SizeOf(len));
  if len > 0 then
    aStream.WriteBuffer(aStr[1], len);
end;

function TTaskHistory.ReadString(aStream: TStream): string;
var
  len: integer;
begin
  aStream.ReadBuffer(len, SizeOf(len));
  SetLength(Result, len);
  if len > 0 then
    aStream.ReadBuffer(Result[1], len);
end;

procedure TTaskHistory.LogChange(aTaskID: integer; const aFieldName, aOldValue, aNewValue: string);
var
  entry: THistoryEntry;
begin
  entry.TaskID := aTaskID;
  entry.ChangeDate := Now;
  entry.FieldName := aFieldName;
  entry.OldValue := aOldValue;
  entry.NewValue := aNewValue;
  entry.ChangeDescription := Format('Changed %s from "%s" to "%s"', [aFieldName, aOldValue, aNewValue]);
  
  SetLength(FHistory, Length(FHistory) + 1);
  FHistory[High(FHistory)] := entry;
  
  TrimOldEntries;
end;

procedure TTaskHistory.LogTaskCreated(aTaskID: integer; const aTitle: string);
var
  entry: THistoryEntry;
begin
  entry.TaskID := aTaskID;
  entry.ChangeDate := Now;
  entry.FieldName := 'Task';
  entry.OldValue := '';
  entry.NewValue := aTitle;
  entry.ChangeDescription := Format('Task created: "%s"', [aTitle]);
  
  SetLength(FHistory, Length(FHistory) + 1);
  FHistory[High(FHistory)] := entry;
  
  TrimOldEntries;
end;

procedure TTaskHistory.LogTaskDeleted(aTaskID: integer; const aTitle: string);
var
  entry: THistoryEntry;
begin
  entry.TaskID := aTaskID;
  entry.ChangeDate := Now;
  entry.FieldName := 'Task';
  entry.OldValue := aTitle;
  entry.NewValue := '';
  entry.ChangeDescription := Format('Task deleted: "%s"', [aTitle]);
  
  SetLength(FHistory, Length(FHistory) + 1);
  FHistory[High(FHistory)] := entry;
  
  TrimOldEntries;
end;

procedure TTaskHistory.LogStatusChange(aTaskID: integer; aOldStatus, aNewStatus: TTaskStatus);
begin
  LogChange(aTaskID, 'Status', StatusToString(aOldStatus), StatusToString(aNewStatus));
end;

procedure TTaskHistory.LogPriorityChange(aTaskID: integer; aOldPriority, aNewPriority: TTaskPriority);
begin
  LogChange(aTaskID, 'Priority', PriorityToString(aOldPriority), PriorityToString(aNewPriority));
end;

function TTaskHistory.GetTaskHistory(aTaskID: integer): THistoryArray;
var
  i: integer;
  taskHistory: THistoryArray;
begin
  SetLength(taskHistory, 0);
  
  for i := 0 to High(FHistory) do
  begin
    if FHistory[i].TaskID = aTaskID then
    begin
      SetLength(taskHistory, Length(taskHistory) + 1);
      taskHistory[High(taskHistory)] := FHistory[i];
    end;
  end;
  
  Result := taskHistory;
end;

function TTaskHistory.GetRecentHistory(aCount: integer): THistoryArray;
var
  i, startIdx: integer;
  recentHistory: THistoryArray;
begin
  SetLength(recentHistory, 0);
  
  if Length(FHistory) = 0 then
  begin
    Result := recentHistory;
    Exit;
  end;
  
  if aCount > Length(FHistory) then
    startIdx := 0
  else
    startIdx := Length(FHistory) - aCount;
  
  for i := startIdx to High(FHistory) do
  begin
    SetLength(recentHistory, Length(recentHistory) + 1);
    recentHistory[High(recentHistory)] := FHistory[i];
  end;
  
  Result := recentHistory;
end;

function TTaskHistory.GetAllHistory: THistoryArray;
begin
  Result := FHistory;
end;

function TTaskHistory.SaveHistory: boolean;
var
  fs: TFileStream;
  i, count: integer;
begin
  Result := false;
  
  try
    fs := TFileStream.Create(FHistoryFile, fmCreate);
    try
      count := Length(FHistory);
      fs.WriteBuffer(count, SizeOf(count));
      
      for i := 0 to High(FHistory) do
      begin
        fs.WriteBuffer(FHistory[i].TaskID, SizeOf(FHistory[i].TaskID));
        fs.WriteBuffer(FHistory[i].ChangeDate, SizeOf(FHistory[i].ChangeDate));
        WriteString(fs, FHistory[i].FieldName);
        WriteString(fs, FHistory[i].OldValue);
        WriteString(fs, FHistory[i].NewValue);
        WriteString(fs, FHistory[i].ChangeDescription);
      end;
      
      Result := true;
    finally
      fs.Free;
    end;
  except
    on E: Exception do
    begin
      // Silently fail - history is not critical
    end;
  end;
end;

function TTaskHistory.LoadHistory: boolean;
var
  fs: TFileStream;
  i, count: integer;
  entry: THistoryEntry;
begin
  Result := false;
  
  if not FileExists(FHistoryFile) then
    Exit;
  
  try
    fs := TFileStream.Create(FHistoryFile, fmOpenRead);
    try
      fs.ReadBuffer(count, SizeOf(count));
      
      SetLength(FHistory, count);
      
      for i := 0 to count - 1 do
      begin
        fs.ReadBuffer(entry.TaskID, SizeOf(entry.TaskID));
        fs.ReadBuffer(entry.ChangeDate, SizeOf(entry.ChangeDate));
        entry.FieldName := ReadString(fs);
        entry.OldValue := ReadString(fs);
        entry.NewValue := ReadString(fs);
        entry.ChangeDescription := ReadString(fs);
        
        FHistory[i] := entry;
      end;
      
      Result := true;
    finally
      fs.Free;
    end;
  except
    on E: Exception do
    begin
      SetLength(FHistory, 0);
    end;
  end;
end;

procedure TTaskHistory.ClearHistory;
begin
  SetLength(FHistory, 0);
  SaveHistory;
end;

procedure TTaskHistory.ClearTaskHistory(aTaskID: integer);
var
  i: integer;
  newHistory: THistoryArray;
begin
  SetLength(newHistory, 0);
  
  for i := 0 to High(FHistory) do
  begin
    if FHistory[i].TaskID <> aTaskID then
    begin
      SetLength(newHistory, Length(newHistory) + 1);
      newHistory[High(newHistory)] := FHistory[i];
    end;
  end;
  
  FHistory := newHistory;
  SaveHistory;
end;

end.
