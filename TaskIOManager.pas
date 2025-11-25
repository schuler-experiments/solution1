
unit TaskIOManager;

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
  TaskManagerSubtasks,
  TaskManagerHistory,
  TaskManagerEnhanced;

type
  { Import/Export types }
  TExportFormat = (efCSV, efJSON, efXML, efPlainText);

  TImportResult = record
    successCount: integer;
    failureCount: integer;
    skippedCount: integer;
    lastError: string;
  end;

  { Task IO Manager class }
  TTaskIOManager = class
  private
    manager: TTaskManagerEnhanced;
    lastError: string;

    function taskStatusToString(status: TTaskStatus): string;
    function stringToTaskStatus(const statusStr: string): TTaskStatus;
    function taskPriorityToString(priority: TTaskPriority): string;
    function stringToTaskPriority(const priorityStr: string): TTaskPriority;
    function formatCSVField(const field: string): string;
  public
    constructor Create(aManager: TTaskManagerEnhanced);
    destructor Destroy; override;

    { Export operations }
    function exportToCSV(const filename: string): boolean;
    function getCSVData: string;

    function exportToJSON(const filename: string): boolean;
    function getJSONData: string;

    function exportToPlainText(const filename: string): boolean;
    function getPlainTextData: string;

    { Bulk operations }
    function bulkUpdateStatus(const taskIds: array of integer; newStatus: TTaskStatus): integer;
    function bulkUpdatePriority(const taskIds: array of integer; newPriority: TTaskPriority): integer;
    function bulkUpdateCategory(const taskIds: array of integer; const newCategory: string): integer;
    function bulkAssignTasks(const taskIds: array of integer; const assignee: string): integer;

    { Data validation }
    function validateTaskData: string;
    function detectOrphanedTasks: TTaskArray;

    { Backup }
    function createBackup(const backupName: string): boolean;

    { Error handling }
    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskIOManager.Create(aManager: TTaskManagerEnhanced);
begin
  inherited Create;
  manager := aManager;
  lastError := '';
end;

destructor TTaskIOManager.Destroy;
begin
  inherited Destroy;
end;

function TTaskIOManager.taskStatusToString(status: TTaskStatus): string;
begin
  case status of
    tsNew: result := 'New';
    tsInProgress: result := 'InProgress';
    tsBlocked: result := 'Blocked';
    tsCompleted: result := 'Completed';
    tsCancelled: result := 'Cancelled';
  else
    result := 'Unknown';
  end;
end;

function TTaskIOManager.stringToTaskStatus(const statusStr: string): TTaskStatus;
begin
  if statusStr = 'New' then
    result := tsNew
  else if statusStr = 'InProgress' then
    result := tsInProgress
  else if statusStr = 'Blocked' then
    result := tsBlocked
  else if statusStr = 'Completed' then
    result := tsCompleted
  else if statusStr = 'Cancelled' then
    result := tsCancelled
  else
    result := tsNew;
end;

function TTaskIOManager.taskPriorityToString(priority: TTaskPriority): string;
begin
  case priority of
    tpLow: result := 'Low';
    tpMedium: result := 'Medium';
    tpHigh: result := 'High';
    tpCritical: result := 'Critical';
  else
    result := 'Unknown';
  end;
end;

function TTaskIOManager.stringToTaskPriority(const priorityStr: string): TTaskPriority;
begin
  if priorityStr = 'Low' then
    result := tpLow
  else if priorityStr = 'Medium' then
    result := tpMedium
  else if priorityStr = 'High' then
    result := tpHigh
  else if priorityStr = 'Critical' then
    result := tpCritical
  else
    result := tpMedium;
end;

function TTaskIOManager.formatCSVField(const field: string): string;
var
  needsQuotes: boolean;
  i: integer;
begin
  needsQuotes := false;

  for i := 1 to length(field) do
  begin
    if (field[i] = ',') or (field[i] = '"') or (field[i] = #10) or (field[i] = #13) then
    begin
      needsQuotes := true;
      break;
    end;
  end;

  if needsQuotes then
    result := '"' + StringReplace(field, '"', '""', [rfReplaceAll]) + '"'
  else
    result := field;
end;

function TTaskIOManager.getCSVData: string;
var
  allTasks: TTaskArray;
  output: string;
  i: integer;
  task: TTask;
begin
  output := 'ID,Name,Description,Status,Priority,Category,DueDate,CreatedDate,CompletedDate,EstimatedHours,ActualHours,AssignedTo' + #13#10;

  allTasks := manager.getAllTasks;
  for i := 0 to length(allTasks) - 1 do
  begin
    task := allTasks[i];
    output := output +
      intToStr(task.id) + ',' +
      formatCSVField(task.name) + ',' +
      formatCSVField(task.description) + ',' +
      taskStatusToString(task.status) + ',' +
      taskPriorityToString(task.priority) + ',' +
      formatCSVField(task.category) + ',' +
      FormatDateTime('YYYY-MM-DD HH:MM:SS', task.dueDate) + ',' +
      FormatDateTime('YYYY-MM-DD HH:MM:SS', task.createdDate) + ',' +
      FormatDateTime('YYYY-MM-DD HH:MM:SS', task.completedDate) + ',' +
      FloatToStr(task.estimatedHours) + ',' +
      FloatToStr(task.actualHours) + ',' +
      formatCSVField(task.assignedTo) + #13#10;
  end;

  result := output;
end;

function TTaskIOManager.exportToCSV(const filename: string): boolean;
var
  csvFile: Text;
  csvData: string;
  i: integer;
begin
  try
    Assign(csvFile, filename);
    Rewrite(csvFile);
    csvData := getCSVData;
    
    { Write string line by line }
    for i := 1 to length(csvData) do
      write(csvFile, csvData[i]);
    
    Close(csvFile);
    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error exporting to CSV: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskIOManager.getJSONData: string;
var
  allTasks: TTaskArray;
  output: string;
  i: integer;
  task: TTask;
begin
  output := '{"tasks": [' + #10;

  allTasks := manager.getAllTasks;
  for i := 0 to length(allTasks) - 1 do
  begin
    task := allTasks[i];
    output := output +
      '  {' + #10 +
      '    "id": ' + intToStr(task.id) + ',' + #10 +
      '    "name": "' + StringReplace(task.name, '"', '\"', [rfReplaceAll]) + '",' + #10 +
      '    "description": "' + StringReplace(task.description, '"', '\"', [rfReplaceAll]) + '",' + #10 +
      '    "status": "' + taskStatusToString(task.status) + '",' + #10 +
      '    "priority": "' + taskPriorityToString(task.priority) + '",' + #10 +
      '    "category": "' + task.category + '",' + #10 +
      '    "dueDate": "' + FormatDateTime('YYYY-MM-DD', task.dueDate) + '",' + #10 +
      '    "assignedTo": "' + task.assignedTo + '",' + #10 +
      '    "estimatedHours": ' + FloatToStr(task.estimatedHours) + ',' + #10 +
      '    "actualHours": ' + FloatToStr(task.actualHours) + #10 +
      '  }';
    if i < length(allTasks) - 1 then
      output := output + ',';
    output := output + #10;
  end;

  output := output + ']}' + #10;
  result := output;
end;

function TTaskIOManager.exportToJSON(const filename: string): boolean;
var
  jsonFile: Text;
  jsonData: string;
  i: integer;
begin
  try
    Assign(jsonFile, filename);
    Rewrite(jsonFile);
    jsonData := getJSONData;
    
    for i := 1 to length(jsonData) do
      write(jsonFile, jsonData[i]);
    
    Close(jsonFile);
    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error exporting to JSON: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskIOManager.getPlainTextData: string;
var
  allTasks: TTaskArray;
  output: string;
  i: integer;
  task: TTask;
begin
  output := '===== TASK MANAGER EXPORT =====' + #13#10 + #13#10;

  allTasks := manager.getAllTasks;
  for i := 0 to length(allTasks) - 1 do
  begin
    task := allTasks[i];
    output := output +
      'Task #' + intToStr(task.id) + #13#10 +
      'Name: ' + task.name + #13#10 +
      'Description: ' + task.description + #13#10 +
      'Status: ' + taskStatusToString(task.status) + #13#10 +
      'Priority: ' + taskPriorityToString(task.priority) + #13#10 +
      'Category: ' + task.category + #13#10 +
      'Due Date: ' + FormatDateTime('YYYY-MM-DD HH:MM:SS', task.dueDate) + #13#10 +
      'Assigned To: ' + task.assignedTo + #13#10 +
      'Estimated Hours: ' + FloatToStr(task.estimatedHours) + #13#10 +
      'Actual Hours: ' + FloatToStr(task.actualHours) + #13#10 +
      '---' + #13#10 + #13#10;
  end;

  result := output;
end;

function TTaskIOManager.exportToPlainText(const filename: string): boolean;
var
  textFile: Text;
  textData: string;
  i: integer;
begin
  try
    Assign(textFile, filename);
    Rewrite(textFile);
    textData := getPlainTextData;
    
    for i := 1 to length(textData) do
      write(textFile, textData[i]);
    
    Close(textFile);
    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error exporting to plain text: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskIOManager.bulkUpdateStatus(const taskIds: array of integer; newStatus: TTaskStatus): integer;
var
  i: integer;
  count: integer;
begin
  count := 0;
  for i := 0 to length(taskIds) - 1 do
  begin
    if manager.setTaskStatus(taskIds[i], newStatus) then
      inc(count);
  end;
  result := count;
end;

function TTaskIOManager.bulkUpdatePriority(const taskIds: array of integer; newPriority: TTaskPriority): integer;
var
  i: integer;
  count: integer;
begin
  count := 0;
  for i := 0 to length(taskIds) - 1 do
  begin
    if manager.setTaskPriority(taskIds[i], newPriority) then
      inc(count);
  end;
  result := count;
end;

function TTaskIOManager.bulkUpdateCategory(const taskIds: array of integer; const newCategory: string): integer;
var
  i: integer;
  count: integer;
  task: TTask;
begin
  count := 0;
  for i := 0 to length(taskIds) - 1 do
  begin
    if manager.getTask(taskIds[i], task) then
    begin
      task.category := newCategory;
      if manager.updateTask(task) then
        inc(count);
    end;
  end;
  result := count;
end;

function TTaskIOManager.bulkAssignTasks(const taskIds: array of integer; const assignee: string): integer;
var
  i: integer;
  count: integer;
begin
  count := 0;
  for i := 0 to length(taskIds) - 1 do
  begin
    if manager.assignTaskTo(taskIds[i], assignee) then
      inc(count);
  end;
  result := count;
end;

function TTaskIOManager.validateTaskData: string;
var
  allTasks: TTaskArray;
  output: string;
  i: integer;
  task: TTask;
  issues: integer;
begin
  output := 'TASK DATA VALIDATION REPORT' + #13#10;
  output := output + '===========================' + #13#10 + #13#10;

  allTasks := manager.getAllTasks;
  issues := 0;

  for i := 0 to length(allTasks) - 1 do
  begin
    task := allTasks[i];

    if task.name = '' then
    begin
      output := output + 'ERROR: Task ' + intToStr(task.id) + ' has empty name' + #13#10;
      inc(issues);
    end;

    if (task.status = tsCompleted) and (task.completedDate = 0) then
    begin
      output := output + 'WARNING: Task ' + intToStr(task.id) + ' is marked complete but has no completed date' + #13#10;
      inc(issues);
    end;

    if task.estimatedHours < 0 then
    begin
      output := output + 'ERROR: Task ' + intToStr(task.id) + ' has negative estimated hours' + #13#10;
      inc(issues);
    end;

    if task.actualHours < 0 then
    begin
      output := output + 'ERROR: Task ' + intToStr(task.id) + ' has negative actual hours' + #13#10;
      inc(issues);
    end;
  end;

  output := output + #13#10 + 'Total tasks: ' + intToStr(length(allTasks)) + #13#10;
  output := output + 'Total issues found: ' + intToStr(issues) + #13#10;

  result := output;
end;

function TTaskIOManager.detectOrphanedTasks: TTaskArray;
var
  allTasks: TTaskArray;
  i: integer;
  count: integer;
begin
  setLength(result, 0);
  count := 0;

  allTasks := manager.getAllTasks;
  for i := 0 to length(allTasks) - 1 do
  begin
    if (allTasks[i].category = '') and (allTasks[i].assignedTo = '') then
    begin
      setLength(result, count + 1);
      result[count] := allTasks[i];
      inc(count);
    end;
  end;
end;

function TTaskIOManager.createBackup(const backupName: string): boolean;
var
  backupFile: Text;
  backupData: string;
  fileName: string;
  i: integer;
begin
  try
    fileName := 'backup_' + backupName + '.txt';
    Assign(backupFile, fileName);
    Rewrite(backupFile);
    
    backupData := '=== TASK MANAGER BACKUP ===' + #13#10;
    backupData := backupData + 'Backup Name: ' + backupName + #13#10;
    backupData := backupData + 'Created: ' + FormatDateTime('YYYY-MM-DD HH:MM:SS', now) + #13#10;
    backupData := backupData + #13#10;
    backupData := backupData + 'CSV DATA:' + #13#10;
    backupData := backupData + getCSVData;
    
    for i := 1 to length(backupData) do
      write(backupFile, backupData[i]);
    
    Close(backupFile);
    result := true;
  except
    on e: Exception do
    begin
      lastError := 'Error creating backup: ' + e.Message;
      result := false;
    end;
  end;
end;

function TTaskIOManager.getLastError: string;
begin
  result := lastError;
end;

procedure TTaskIOManager.clearError;
begin
  lastError := '';
end;

end.
