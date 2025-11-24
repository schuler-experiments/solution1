unit TaskExport;

{$mode objfpc}

interface

uses
  SysUtils,
  Classes,
  TaskTypes;

type
  TExportFormat = (efJSON, efXML, efCSV, efHTML);
  
  TExportOptions = record
    includeNotes: boolean;
    includeDependencies: boolean;
    includeHistory: boolean;
    includeComments: boolean;
    compressOutput: boolean;
    prettyPrint: boolean;
  end;

  TImportResult = record
    success: boolean;
    tasksImported: integer;
    errorsFound: integer;
    errorMessage: string;
  end;

  TExportMetadata = record
    exportDate: TDateTime;
    exportVersion: string;
    taskCount: integer;
    exportFormat: TExportFormat;
    sourceSystem: string;
  end;

  TTaskExporterClass = class
  private
    exportOptions: TExportOptions;
    metadata: TExportMetadata;
    function FormatJSONValue(value: string): string;
    function FormatXMLValue(value: string): string;
    function EscapeXML(s: string): string;
    function TaskToJSONLine(task: TTask): string;
    function TaskToXMLElement(task: TTask): string;
    function TaskToHTMLRow(task: TTask): string;
  public
    constructor Create();
    destructor Destroy();
    function ExportTasksAsJSON(tasks: TTaskArray; filename: string): boolean;
    function ExportTasksAsXML(tasks: TTaskArray; filename: string): boolean;
    function ExportTasksAsCSV(tasks: TTaskArray; filename: string): boolean;
    function ExportTasksAsHTML(tasks: TTaskArray; filename: string): boolean;
    function ExportToFormat(tasks: TTaskArray; filename: string;
                           format: TExportFormat): boolean;
    procedure SetExportOptions(options: TExportOptions);
    function GetExportOptions(): TExportOptions;
    function ValidateExportFile(filename: string): boolean;
    function GetFileSize(filename: string): int64;
    function CreateBackupBeforeExport(sourceFile: string): boolean;
    procedure SelfTest();
  end;

implementation

constructor TTaskExporterClass.Create();
begin
  inherited Create();
  FillChar(exportOptions, SizeOf(TExportOptions), 0);
  exportOptions.prettyPrint := true;
  FillChar(metadata, SizeOf(TExportMetadata), 0);
  metadata.exportVersion := '1.0';
  metadata.sourceSystem := 'FreePascal Task Manager';
end;

destructor TTaskExporterClass.Destroy();
begin
  inherited Destroy();
end;

function TTaskExporterClass.FormatJSONValue(value: string): string;
begin
  result := '"' + StringReplace(value, '"', '\"', [rfReplaceAll]) + '"';
end;

function TTaskExporterClass.EscapeXML(s: string): string;
begin
  result := s;
  result := StringReplace(result, '&', '&amp;', [rfReplaceAll]);
  result := StringReplace(result, '<', '&lt;', [rfReplaceAll]);
  result := StringReplace(result, '>', '&gt;', [rfReplaceAll]);
  result := StringReplace(result, '"', '&quot;', [rfReplaceAll]);
end;

function TTaskExporterClass.FormatXMLValue(value: string): string;
begin
  result := EscapeXML(value);
end;

function TTaskExporterClass.TaskToJSONLine(task: TTask): string;
var
  statusStr, priorityStr: string;
begin
  statusStr := IntToStr(Ord(task.status));
  priorityStr := IntToStr(Ord(task.priority));

  result := '{' +
    '"id":' + IntToStr(task.id) + ',' +
    '"title":' + FormatJSONValue(task.title) + ',' +
    '"status":' + statusStr + ',' +
    '"priority":' + priorityStr +
    '}';
end;

function TTaskExporterClass.TaskToXMLElement(task: TTask): string;
begin
  result := '<task>' +
    '<id>' + IntToStr(task.id) + '</id>' +
    '<title>' + FormatXMLValue(task.title) + '</title>' +
    '<description>' + FormatXMLValue(task.description) + '</description>' +
    '<status>' + IntToStr(Ord(task.status)) + '</status>' +
    '<priority>' + IntToStr(Ord(task.priority)) + '</priority>' +
    '</task>';
end;

function TTaskExporterClass.TaskToHTMLRow(task: TTask): string;
begin
  result := '<tr><td>' + IntToStr(task.id) + '</td>' +
    '<td>' + task.title + '</td>' +
    '<td>' + task.description + '</td></tr>';
end;

function TTaskExporterClass.ExportTasksAsJSON(tasks: TTaskArray;
                                               filename: string): boolean;
var
  i: integer;
  content: string;
begin
  content := '{' + sLineBreak + '  "tasks": [' + sLineBreak;

  for i := 0 to Length(tasks) - 1 do
  begin
    content := content + '    ' + TaskToJSONLine(tasks[i]);
    if i < Length(tasks) - 1 then
      content := content + ',';
    content := content + sLineBreak;
  end;

  content := content + '  ]' + sLineBreak + '}';

  try
    with TFileStream.Create(filename, fmCreate) do
    try
      WriteBuffer(content[1], Length(content));
    finally
      Free();
    end;
    result := true;
  except
    result := false;
  end;
end;

function TTaskExporterClass.ExportTasksAsXML(tasks: TTaskArray;
                                              filename: string): boolean;
var
  i: integer;
  content: string;
begin
  content := '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
    '<tasks>' + sLineBreak;

  for i := 0 to Length(tasks) - 1 do
    content := content + '  ' + TaskToXMLElement(tasks[i]) + sLineBreak;

  content := content + '</tasks>';

  try
    with TFileStream.Create(filename, fmCreate) do
    try
      WriteBuffer(content[1], Length(content));
    finally
      Free();
    end;
    result := true;
  except
    result := false;
  end;
end;

function TTaskExporterClass.ExportTasksAsCSV(tasks: TTaskArray;
                                              filename: string): boolean;
var
  i: integer;
  content: string;
begin
  content := 'ID,Title,Description,Status,Priority' + sLineBreak;

  for i := 0 to Length(tasks) - 1 do
  begin
    content := content + IntToStr(tasks[i].id) + ',' +
      tasks[i].title + ',' +
      tasks[i].description + ',' +
      IntToStr(Ord(tasks[i].status)) + ',' +
      IntToStr(Ord(tasks[i].priority)) + sLineBreak;
  end;

  try
    with TFileStream.Create(filename, fmCreate) do
    try
      WriteBuffer(content[1], Length(content));
    finally
      Free();
    end;
    result := true;
  except
    result := false;
  end;
end;

function TTaskExporterClass.ExportTasksAsHTML(tasks: TTaskArray;
                                               filename: string): boolean;
var
  i: integer;
  content: string;
begin
  content := '<!DOCTYPE html><html><head><title>Tasks</title>' +
    '<style>table{border-collapse:collapse}td{border:1px solid}</style>' +
    '</head><body><table><tr><th>ID</th><th>Title</th></tr>';

  for i := 0 to Length(tasks) - 1 do
    content := content + TaskToHTMLRow(tasks[i]);

  content := content + '</table></body></html>';

  try
    with TFileStream.Create(filename, fmCreate) do
    try
      WriteBuffer(content[1], Length(content));
    finally
      Free();
    end;
    result := true;
  except
    result := false;
  end;
end;

function TTaskExporterClass.ExportToFormat(tasks: TTaskArray; filename: string;
                                            format: TExportFormat): boolean;
begin
  case format of
    efJSON:
      result := ExportTasksAsJSON(tasks, filename);
    efXML:
      result := ExportTasksAsXML(tasks, filename);
    efCSV:
      result := ExportTasksAsCSV(tasks, filename);
    efHTML:
      result := ExportTasksAsHTML(tasks, filename);
  else
    result := false;
  end;
end;

procedure TTaskExporterClass.SetExportOptions(options: TExportOptions);
begin
  exportOptions := options;
end;

function TTaskExporterClass.GetExportOptions(): TExportOptions;
begin
  result := exportOptions;
end;

function TTaskExporterClass.ValidateExportFile(filename: string): boolean;
begin
  result := FileExists(filename) and (GetFileSize(filename) > 0);
end;

function TTaskExporterClass.GetFileSize(filename: string): int64;
var
  info: TSearchRec;
begin
  if FindFirst(filename, faAnyFile, info) = 0 then
  begin
    result := info.Size;
    FindClose(info);
  end
  else
    result := 0;
end;

function TTaskExporterClass.CreateBackupBeforeExport(sourceFile: string): boolean;
var
  backupName: string;
begin
  if not FileExists(sourceFile) then
  begin
    result := true;
    exit;
  end;

  backupName := sourceFile + '.backup';
  try
    with TFileStream.Create(sourceFile, fmOpenRead) do
    try
      with TFileStream.Create(backupName, fmCreate) do
      try
        CopyFrom(TFileStream(Self), 0);
      finally
        Free();
      end;
    finally
      Free();
    end;
    result := true;
  except
    result := false;
  end;
end;

procedure TTaskExporterClass.SelfTest();
var
  testTasks: TTaskArray;
  i: integer;
  exported: boolean;
begin
  WriteLn('');
  WriteLn('=== TaskExport Self Test ===');

  SetLength(testTasks, 3);
  for i := 0 to 2 do
  begin
    testTasks[i].id := i + 1;
    testTasks[i].title := 'Test Task ' + IntToStr(i + 1);
    testTasks[i].description := 'Test Description';
    testTasks[i].status := tsNotStarted;
    testTasks[i].priority := tpMedium;
  end;

  exported := ExportTasksAsJSON(testTasks, 'test_export.json');
  WriteLn('JSON export: ' + BoolToStr(exported));

  exported := ExportTasksAsXML(testTasks, 'test_export.xml');
  WriteLn('XML export: ' + BoolToStr(exported));

  exported := ExportTasksAsCSV(testTasks, 'test_export.csv');
  WriteLn('CSV export: ' + BoolToStr(exported));

  exported := ExportTasksAsHTML(testTasks, 'test_export.html');
  WriteLn('HTML export: ' + BoolToStr(exported));

  WriteLn('Validating exported files...');
  if ValidateExportFile('test_export.json') then
    WriteLn('JSON file valid: true');

  WriteLn('=== TaskExport Self Test Complete ===');

  SetLength(testTasks, 0);
end;

end.
