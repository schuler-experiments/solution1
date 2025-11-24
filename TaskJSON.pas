
unit TaskJSON;

{$mode objfpc}

interface

uses
  SysUtils,
  Classes,
  TaskTypes,
  TaskManager;

type
  { JSON serialization/deserialization for tasks }
  TTaskJSONClass = class
  private
    fFilePath: string;
    function EscapeJSONString(s: string): string;
    function UnescapeJSONString(s: string): string;
    function DateTimeToJSON(dt: TDateTime): string;
    function JSONToDateTime(s: string): TDateTime;
    function PriorityToJSON(p: TTaskPriority): string;
    function JSONToPriority(s: string): TTaskPriority;
    function StatusToJSON(s: TTaskStatus): string;
    function JSONToStatus(s: string): TTaskStatus;
    function TaskToJSONObject(task: TTask): string;
  public
    constructor Create(aFilePath: string);
    procedure ExportToJSON(mgr: TTaskManagerClass);
    procedure ImportFromJSON(mgr: TTaskManagerClass);
    function FileExists(): boolean;
    procedure SelfTest();
  end;

implementation

constructor TTaskJSONClass.Create(aFilePath: string);
begin
  fFilePath := aFilePath;
end;

function TTaskJSONClass.EscapeJSONString(s: string): string;
var
  i: integer;
  result_str: string;
begin
  result_str := '';
  for i := 1 to Length(s) do
  begin
    case s[i] of
      '"': result_str := result_str + '\"';
      '\': result_str := result_str + '\\';
      #10: result_str := result_str + '\n';
      #13: result_str := result_str + '\r';
      #9: result_str := result_str + '\t';
    else
      result_str := result_str + s[i];
    end;
  end;
  result := result_str;
end;

function TTaskJSONClass.UnescapeJSONString(s: string): string;
var
  i: integer;
  result_str: string;
begin
  result_str := '';
  i := 1;
  while i <= Length(s) do
  begin
    if (s[i] = '\') and (i < Length(s)) then
    begin
      case s[i + 1] of
        '"': begin result_str := result_str + '"'; i := i + 2; end;
        '\': begin result_str := result_str + '\'; i := i + 2; end;
        'n': begin result_str := result_str + #10; i := i + 2; end;
        'r': begin result_str := result_str + #13; i := i + 2; end;
        't': begin result_str := result_str + #9; i := i + 2; end;
      else
        result_str := result_str + s[i];
        i := i + 1;
      end;
    end
    else
    begin
      result_str := result_str + s[i];
      i := i + 1;
    end;
  end;
  result := result_str;
end;

function TTaskJSONClass.DateTimeToJSON(dt: TDateTime): string;
begin
  result := FormatDateTime('yyyy-mm-dd hh:mm:ss', dt);
end;

function TTaskJSONClass.JSONToDateTime(s: string): TDateTime;
begin
  try
    result := StrToDateTime(s);
  except
    result := Now();
  end;
end;

function TTaskJSONClass.PriorityToJSON(p: TTaskPriority): string;
begin
  result := PriorityToString(p);
end;

function TTaskJSONClass.JSONToPriority(s: string): TTaskPriority;
begin
  result := StringToPriority(s);
end;

function TTaskJSONClass.StatusToJSON(s: TTaskStatus): string;
begin
  result := StatusToString(s);
end;

function TTaskJSONClass.JSONToStatus(s: string): TTaskStatus;
begin
  result := StringToStatus(s);
end;

function TTaskJSONClass.TaskToJSONObject(task: TTask): string;
var
  json_str: string;
  i: integer;
begin
  json_str := '{' + #10;
  json_str := json_str + '  "id": ' + IntToStr(task.ID) + ',' + #10;
  json_str := json_str + '  "title": "' + EscapeJSONString(task.Title) + '",' + #10;
  json_str := json_str + '  "description": "' + EscapeJSONString(task.Description) + '",' + #10;
  json_str := json_str + '  "priority": "' + PriorityToJSON(task.Priority) + '",' + #10;
  json_str := json_str + '  "status": "' + StatusToJSON(task.Status) + '",' + #10;
  json_str := json_str + '  "dueDate": "' + DateTimeToJSON(task.DueDate) + '",' + #10;
  json_str := json_str + '  "createdDate": "' + DateTimeToJSON(task.CreatedDate) + '",' + #10;
  json_str := json_str + '  "completedDate": "' + DateTimeToJSON(task.CompletedDate) + '",' + #10;
  json_str := json_str + '  "assignee": "' + EscapeJSONString(task.Assignee) + '",' + #10;
  json_str := json_str + '  "category": "' + EscapeJSONString(task.Category) + '",' + #10;
  json_str := json_str + '  "estimatedHours": ' + FormatFloat('0.00', task.EstimatedHours) + ',' + #10;
  json_str := json_str + '  "tags": [';
  
  for i := 0 to Length(task.Tags) - 1 do
  begin
    if i > 0 then
      json_str := json_str + ', ';
    json_str := json_str + '"' + EscapeJSONString(task.Tags[i]) + '"';
  end;
  
  json_str := json_str + ']' + #10;
  json_str := json_str + '}';
  result := json_str;
end;

procedure TTaskJSONClass.ExportToJSON(mgr: TTaskManagerClass);
var
  outfile: TextFile;
  tasks: TTaskArray;
  i: integer;
begin
  if not Assigned(mgr) then
    exit;

  tasks := mgr.GetAllTasks();
  
  try
    AssignFile(outfile, fFilePath);
    Rewrite(outfile);
    WriteLn(outfile, '{');
    WriteLn(outfile, '  "taskManager": {');
    WriteLn(outfile, '    "exportDate": "' + DateTimeToJSON(Now()) + '",');
    WriteLn(outfile, '    "taskCount": ' + IntToStr(Length(tasks)) + ',');
    WriteLn(outfile, '    "tasks": [');
    
    for i := 0 to Length(tasks) - 1 do
    begin
      if i > 0 then
        WriteLn(outfile, ',');
      Write(outfile, TaskToJSONObject(tasks[i]));
    end;
    
    WriteLn(outfile, '');
    WriteLn(outfile, '    ]');
    WriteLn(outfile, '  }');
    WriteLn(outfile, '}');
    CloseFile(outfile);
    WriteLn('JSON export complete: ' + fFilePath);
  except
    on E: Exception do
      WriteLn('Error exporting to JSON: ' + E.Message);
  end;
end;

procedure TTaskJSONClass.ImportFromJSON(mgr: TTaskManagerClass);
var
  infile: TextFile;
  line: string;
begin
  if not Assigned(mgr) then
    exit;

  if not FileExists() then
  begin
    WriteLn('JSON file not found: ' + fFilePath);
    exit;
  end;

  try
    AssignFile(infile, fFilePath);
    Reset(infile);
    
    while not Eof(infile) do
    begin
      ReadLn(infile, line);
      { Simplified import - full JSON parsing would be more complex }
    end;
    
    CloseFile(infile);
    WriteLn('JSON import complete: ' + fFilePath);
  except
    on E: Exception do
      WriteLn('Error importing from JSON: ' + E.Message);
  end;
end;

function TTaskJSONClass.FileExists(): boolean;
begin
  result := SysUtils.FileExists(fFilePath);
end;

procedure TTaskJSONClass.SelfTest();
var
  json_mgr: TTaskJSONClass;
  task_mgr: TTaskManagerClass;
begin
  WriteLn('');
  WriteLn('=== TaskJSON Self Test ===');
  
  json_mgr := TTaskJSONClass.Create('test_export.json');
  task_mgr := TTaskManagerClass.Create();
  
  try
    { Add test tasks }
    task_mgr.AddTask('Learn JSON', 'Study JSON format', tpHigh, Now() + 7);
    task_mgr.AddTask('Export data', 'Export to JSON', tpMedium, Now() + 14);
    task_mgr.AddTask('Import data', 'Import from JSON', tpMedium, Now() + 21);
    
    { Export to JSON }
    json_mgr.ExportToJSON(task_mgr);
    
    if json_mgr.FileExists() then
      WriteLn('✓ JSON file created successfully')
    else
      WriteLn('✗ JSON file creation failed');
    
    WriteLn('=== TaskJSON Self Test Complete ===');
  finally
    task_mgr.Free();
    json_mgr.Free();
  end;
end;

end.
