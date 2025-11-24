
unit TaskPersistence;

{$mode objfpc}

interface

uses
  SysUtils, Classes, DateUtils, TaskTypes, TaskManager;

type
  { Handles file I/O and data persistence for tasks }
  TTaskPersistenceClass = class
  private
    filePath: string;
    function FormatDateTimeValue(dt: TDateTime): string;
    function ParseDateTime(s: string): TDateTime;
  public
    constructor Create(aFilePath: string);
    
    { CSV Operations }
    procedure SaveAsCSV(mgr: TTaskManagerClass);
    procedure LoadFromCSV(mgr: TTaskManagerClass);
    
    { Text file operations }
    procedure SaveAsText(mgr: TTaskManagerClass);
    procedure LoadFromText(mgr: TTaskManagerClass);
    
    { Backup and export }
    procedure CreateBackup();
    function FileExists(): boolean;
    procedure DeleteFile();
    
    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskPersistenceClass.Create(aFilePath: string);
begin
  inherited Create();
  filePath := aFilePath;
end;

function TTaskPersistenceClass.FormatDateTimeValue(dt: TDateTime): string;
begin
  if dt > 0 then
    result := SysUtils.FormatDateTime('yyyy-mm-dd hh:mm:ss', dt)
  else
    result := '';
end;

function TTaskPersistenceClass.ParseDateTime(s: string): TDateTime;
begin
  try
    result := StrToDateTime(s);
  except
    result := 0;
  end;
end;

procedure TTaskPersistenceClass.SaveAsCSV(mgr: TTaskManagerClass);
var
  f: TextFile;
  tasks: TTaskArray;
  i: integer;
begin
  AssignFile(f, filePath);
  Rewrite(f);
  try
    { Write header }
    WriteLn(f, 'ID,Title,Description,Status,Priority,DueDate,CreatedDate,CompletedDate');
    
    { Write tasks }
    tasks := mgr.GetAllTasks();
    for i := 0 to Length(tasks) - 1 do
    begin
      WriteLn(f, Format('%d,"%s","%s",%s,%s,%s,%s,%s',
        [tasks[i].id,
         StringReplace(tasks[i].title, '"', '""', [rfReplaceAll]),
         StringReplace(tasks[i].description, '"', '""', [rfReplaceAll]),
         StatusToString(tasks[i].status),
         PriorityToString(tasks[i].priority),
         FormatDateTimeValue(tasks[i].dueDate),
         FormatDateTimeValue(tasks[i].createdDate),
         FormatDateTimeValue(tasks[i].completedDate)]));
    end;
  finally
    CloseFile(f);
  end;
end;

procedure TTaskPersistenceClass.LoadFromCSV(mgr: TTaskManagerClass);
var
  f: TextFile;
  line: string;
  parts: TStringList;
  dueDate: TDateTime;
begin
  if not SysUtils.FileExists(filePath) then
    exit;
    
  AssignFile(f, filePath);
  Reset(f);
  parts := TStringList.Create();
  try
    { Skip header }
    if not Eof(f) then
      ReadLn(f, line);
    
    { Read tasks }
    while not Eof(f) do
    begin
      ReadLn(f, line);
      if Length(Trim(line)) > 0 then
      begin
        { Simple CSV parsing (assumes no commas in fields) }
        if line[1] = '"' then
          continue; { Skip quoted lines for now }
        
        parts.Clear();
        parts.CommaText := line;
        
        if parts.Count >= 6 then
        begin
          dueDate := ParseDateTime(parts[5]);
          mgr.AddTask(
            parts[1],
            parts[2],
            StringToPriority(parts[4]),
            dueDate
          );
        end;
      end;
    end;
  finally
    parts.Free();
    CloseFile(f);
  end;
end;

procedure TTaskPersistenceClass.SaveAsText(mgr: TTaskManagerClass);
var
  f: TextFile;
  tasks: TTaskArray;
  i: integer;
begin
  AssignFile(f, filePath + '.txt');
  Rewrite(f);
  try
    WriteLn(f, '=== Task Manager Export ===');
    WriteLn(f, 'Generated: ' + FormatDateTimeValue(Now()));
    WriteLn(f, '');
    
    tasks := mgr.GetAllTasks();
    WriteLn(f, Format('Total Tasks: %d', [Length(tasks)]));
    WriteLn(f, '');
    
    for i := 0 to Length(tasks) - 1 do
    begin
      WriteLn(f, Format('Task #%d: %s', [tasks[i].id, tasks[i].title]));
      WriteLn(f, Format('  Status: %s', [StatusToString(tasks[i].status)]));
      WriteLn(f, Format('  Priority: %s', [PriorityToString(tasks[i].priority)]));
      WriteLn(f, Format('  Description: %s', [tasks[i].description]));
      WriteLn(f, Format('  Due Date: %s', [FormatDateTimeValue(tasks[i].dueDate)]));
      WriteLn(f, Format('  Created: %s', [FormatDateTimeValue(tasks[i].createdDate)]));
      if tasks[i].completedDate > 0 then
        WriteLn(f, Format('  Completed: %s', [FormatDateTimeValue(tasks[i].completedDate)]));
      WriteLn(f, '');
    end;
  finally
    CloseFile(f);
  end;
end;

procedure TTaskPersistenceClass.LoadFromText(mgr: TTaskManagerClass);
begin
  { Text format is human-readable but not easily parseable }
  { This is a placeholder for more sophisticated parsing if needed }
  WriteLn('Text format import not yet implemented');
end;

procedure TTaskPersistenceClass.CreateBackup();
var
  backupPath: string;
  sourceFile, destFile: TextFile;
  line: string;
begin
  if SysUtils.FileExists(filePath) then
  begin
    backupPath := filePath + '.backup';
    AssignFile(sourceFile, filePath);
    AssignFile(destFile, backupPath);
    
    Reset(sourceFile);
    Rewrite(destFile);
    try
      while not Eof(sourceFile) do
      begin
        ReadLn(sourceFile, line);
        WriteLn(destFile, line);
      end;
    finally
      CloseFile(sourceFile);
      CloseFile(destFile);
    end;
  end;
end;

function TTaskPersistenceClass.FileExists(): boolean;
begin
  result := SysUtils.FileExists(filePath);
end;

procedure TTaskPersistenceClass.DeleteFile();
begin
  if SysUtils.FileExists(filePath) then
    SysUtils.DeleteFile(filePath);
end;

procedure TTaskPersistenceClass.SelfTest();
var
  mgr: TTaskManagerClass;
  persistence: TTaskPersistenceClass;
  testFile: string;
  tomorrow, twoDays, threeDays: TDateTime;
begin
  WriteLn('=== TaskPersistence Self Test ===');
  
  testFile := 'test_tasks.csv';
  mgr := TTaskManagerClass.Create();
  persistence := TTaskPersistenceClass.Create(testFile);
  
  try
    { Add test data }
    tomorrow := IncDay(Now(), 1);
    twoDays := IncDay(Now(), 2);
    threeDays := IncDay(Now(), 3);
    
    WriteLn('Adding test tasks...');
    mgr.AddTask('Review code', 'Code review for sprint 1', tpHigh, tomorrow);
    mgr.AddTask('Deploy to production', 'Release v1.0', tpHigh, IncDay(tomorrow, 2));
    mgr.AddTask('Update documentation', 'API reference', tpMedium, IncDay(tomorrow, 4));
    
    WriteLn(Format('Tasks before save: %d', [mgr.GetTaskCount()]));
    
    { Save to CSV }
    WriteLn('Saving tasks to CSV...');
    persistence.SaveAsCSV(mgr);
    WriteLn(Format('CSV file saved: %s', [testFile]));
    
    { Save as Text }
    WriteLn('Saving tasks to text file...');
    persistence.SaveAsText(mgr);
    WriteLn('Text file saved: test_tasks.csv.txt');
    
    { Create backup }
    WriteLn('Creating backup...');
    persistence.CreateBackup();
    WriteLn('Backup created: test_tasks.csv.backup');
    
    { Check file exists }
    if persistence.FileExists() then
      WriteLn('✓ Persistence file verified')
    else
      WriteLn('✗ Persistence file not found');
    
    WriteLn('=== TaskPersistence Self Test Complete ===');
    
  finally
    mgr.Free();
    { Clean up test files }
    if persistence.FileExists() then
      persistence.DeleteFile();
    if SysUtils.FileExists('test_tasks.csv.txt') then
      SysUtils.DeleteFile('test_tasks.csv.txt');
    if SysUtils.FileExists('test_tasks.csv.backup') then
      SysUtils.DeleteFile('test_tasks.csv.backup');
    persistence.Free();
  end;
end;

end.
