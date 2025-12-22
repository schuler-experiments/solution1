
unit TaskExport;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, TaskTypes;

type
  // Export format options
  TExportFormat = (efJSON, efHTML, efCSV, efText, efMarkdown);
  
  // Task exporter class
  TTaskExporter = class
  private
    function EscapeJSON(const aStr: string): string;
    function EscapeHTML(const aStr: string): string;
    function TaskToJSON(const aTask: TTask; aIndent: integer): string;
    function TasksToJSON(const aTasks: TTaskArray): string;
    function TasksToHTML(const aTasks: TTaskArray; const aTitle: string): string;
    function TasksToMarkdown(const aTasks: TTaskArray; const aTitle: string): string;
    function CategoryStatisticsToHTML(const aStats: TCategoryStatisticsArray): string;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Export functions
    function ExportToJSON(const aTasks: TTaskArray; const aFileName: string): boolean;
    function ExportToHTML(const aTasks: TTaskArray; const aFileName, aTitle: string): boolean;
    function ExportToMarkdown(const aTasks: TTaskArray; const aFileName, aTitle: string): boolean;
    
    // Generate reports
    function GenerateHTMLReport(const aTasks: TTaskArray; const aStats: TTaskStatistics;
      const aCategoryStats: TCategoryStatisticsArray; const aFileName: string): boolean;
    
    // Get export as string (without saving to file)
    function GetJSONString(const aTasks: TTaskArray): string;
    function GetHTMLString(const aTasks: TTaskArray; const aTitle: string): string;
  end;

implementation

constructor TTaskExporter.Create;
begin
  inherited Create;
end;

destructor TTaskExporter.Destroy;
begin
  inherited Destroy;
end;

function TTaskExporter.EscapeJSON(const aStr: string): string;
var
  i: integer;
  c: char;
begin
  Result := '';
  for i := 1 to Length(aStr) do
  begin
    c := aStr[i];
    case c of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      '/': Result := Result + '\/';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      Result := Result + c;
    end;
  end;
end;

function TTaskExporter.EscapeHTML(const aStr: string): string;
var
  i: integer;
  c: char;
begin
  Result := '';
  for i := 1 to Length(aStr) do
  begin
    c := aStr[i];
    case c of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&#39;';
    else
      Result := Result + c;
    end;
  end;
end;

function TTaskExporter.TaskToJSON(const aTask: TTask; aIndent: integer): string;
var
  indent: string;
  json: string;
begin
  indent := StringOfChar(' ', aIndent);
  
  json := indent + '{' + LineEnding;
  json := json + indent + '  "id": ' + IntToStr(aTask.ID) + ',' + LineEnding;
  json := json + indent + '  "title": "' + EscapeJSON(aTask.Title) + '",' + LineEnding;
  json := json + indent + '  "description": "' + EscapeJSON(aTask.Description) + '",' + LineEnding;
  json := json + indent + '  "priority": "' + PriorityToString(aTask.Priority) + '",' + LineEnding;
  json := json + indent + '  "status": "' + StatusToString(aTask.Status) + '",' + LineEnding;
  json := json + indent + '  "category": "' + EscapeJSON(aTask.Category) + '",' + LineEnding;
  json := json + indent + '  "tags": "' + EscapeJSON(aTask.Tags) + '",' + LineEnding;
  json := json + indent + '  "dependsOnIDs": "' + EscapeJSON(aTask.DependsOnIDs) + '",' + LineEnding;
  json := json + indent + '  "createdDate": "' + DateTimeToStr(aTask.CreatedDate) + '",' + LineEnding;
  json := json + indent + '  "dueDate": "' + DateTimeToStr(aTask.DueDate) + '",' + LineEnding;
  json := json + indent + '  "completedDate": "' + DateTimeToStr(aTask.CompletedDate) + '",' + LineEnding;
  json := json + indent + '  "lastModifiedDate": "' + DateTimeToStr(aTask.LastModifiedDate) + '",' + LineEnding;
  json := json + indent + '  "estimatedHours": ' + FloatToStr(aTask.EstimatedHours) + ',' + LineEnding;
  json := json + indent + '  "actualHours": ' + FloatToStr(aTask.ActualHours) + ',' + LineEnding;
  json := json + indent + '  "isActive": ' + LowerCase(BoolToStr(aTask.IsActive, true)) + LineEnding;
  json := json + indent + '}';
  
  Result := json;
end;

function TTaskExporter.TasksToJSON(const aTasks: TTaskArray): string;
var
  i: integer;
  json: string;
begin
  json := '{' + LineEnding;
  json := json + '  "version": "' + VERSION + '",' + LineEnding;
  json := json + '  "exportDate": "' + DateTimeToStr(Now) + '",' + LineEnding;
  json := json + '  "taskCount": ' + IntToStr(Length(aTasks)) + ',' + LineEnding;
  json := json + '  "tasks": [' + LineEnding;
  
  for i := 0 to High(aTasks) do
  begin
    json := json + TaskToJSON(aTasks[i], 4);
    if i < High(aTasks) then
      json := json + ',';
    json := json + LineEnding;
  end;
  
  json := json + '  ]' + LineEnding;
  json := json + '}' + LineEnding;
  
  Result := json;
end;

function TTaskExporter.TasksToHTML(const aTasks: TTaskArray; const aTitle: string): string;
var
  i: integer;
  html: string;
  statusClass: string;
begin
  html := '<!DOCTYPE html>' + LineEnding;
  html := html + '<html lang="en">' + LineEnding;
  html := html + '<head>' + LineEnding;
  html := html + '  <meta charset="UTF-8">' + LineEnding;
  html := html + '  <meta name="viewport" content="width=device-width, initial-scale=1.0">' + LineEnding;
  html := html + '  <title>' + EscapeHTML(aTitle) + '</title>' + LineEnding;
  html := html + '  <style>' + LineEnding;
  html := html + '    body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }' + LineEnding;
  html := html + '    h1 { color: #333; }' + LineEnding;
  html := html + '    table { width: 100%; border-collapse: collapse; background: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }' + LineEnding;
  html := html + '    th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }' + LineEnding;
  html := html + '    th { background: #4CAF50; color: white; font-weight: bold; }' + LineEnding;
  html := html + '    tr:hover { background: #f5f5f5; }' + LineEnding;
  html := html + '    .priority-low { color: #4CAF50; }' + LineEnding;
  html := html + '    .priority-medium { color: #FF9800; }' + LineEnding;
  html := html + '    .priority-high { color: #FF5722; }' + LineEnding;
  html := html + '    .priority-critical { color: #F44336; font-weight: bold; }' + LineEnding;
  html := html + '    .status-completed { color: #4CAF50; }' + LineEnding;
  html := html + '    .status-cancelled { color: #999; text-decoration: line-through; }' + LineEnding;
  html := html + '    .status-inprogress { color: #2196F3; }' + LineEnding;
  html := html + '    .category { background: #E3F2FD; padding: 2px 8px; border-radius: 4px; font-size: 0.9em; }' + LineEnding;
  html := html + '    .footer { margin-top: 20px; color: #666; font-size: 0.9em; }' + LineEnding;
  html := html + '  </style>' + LineEnding;
  html := html + '</head>' + LineEnding;
  html := html + '<body>' + LineEnding;
  html := html + '  <h1>' + EscapeHTML(aTitle) + '</h1>' + LineEnding;
  html := html + '  <p>Generated: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '</p>' + LineEnding;
  html := html + '  <p>Total Tasks: ' + IntToStr(Length(aTasks)) + '</p>' + LineEnding;
  html := html + '  <table>' + LineEnding;
  html := html + '    <thead>' + LineEnding;
  html := html + '      <tr>' + LineEnding;
  html := html + '        <th>ID</th><th>Title</th><th>Category</th><th>Priority</th><th>Status</th>' + LineEnding;
  html := html + '        <th>Due Date</th><th>Est. Hours</th><th>Actual Hours</th>' + LineEnding;
  html := html + '      </tr>' + LineEnding;
  html := html + '    </thead>' + LineEnding;
  html := html + '    <tbody>' + LineEnding;
  
  for i := 0 to High(aTasks) do
  begin
    case aTasks[i].Status of
      tsCompleted: statusClass := 'status-completed';
      tsCancelled: statusClass := 'status-cancelled';
      tsInProgress: statusClass := 'status-inprogress';
    else
      statusClass := '';
    end;
    
    html := html + '      <tr>' + LineEnding;
    html := html + '        <td>' + IntToStr(aTasks[i].ID) + '</td>' + LineEnding;
    html := html + '        <td class="' + statusClass + '">' + EscapeHTML(aTasks[i].Title) + '</td>' + LineEnding;
    html := html + '        <td><span class="category">' + EscapeHTML(aTasks[i].Category) + '</span></td>' + LineEnding;
    html := html + '        <td class="priority-' + LowerCase(PriorityToString(aTasks[i].Priority)) + '">' + 
            PriorityToString(aTasks[i].Priority) + '</td>' + LineEnding;
    html := html + '        <td>' + StatusToString(aTasks[i].Status) + '</td>' + LineEnding;
    html := html + '        <td>' + DateTimeToStr(aTasks[i].DueDate) + '</td>' + LineEnding;
    html := html + '        <td>' + FloatToStrF(aTasks[i].EstimatedHours, ffFixed, 10, 1) + '</td>' + LineEnding;
    html := html + '        <td>' + FloatToStrF(aTasks[i].ActualHours, ffFixed, 10, 1) + '</td>' + LineEnding;
    html := html + '      </tr>' + LineEnding;
  end;
  
  html := html + '    </tbody>' + LineEnding;
  html := html + '  </table>' + LineEnding;
  html := html + '  <div class="footer">' + LineEnding;
  html := html + '    <p>Task Manager v' + VERSION + '</p>' + LineEnding;
  html := html + '  </div>' + LineEnding;
  html := html + '</body>' + LineEnding;
  html := html + '</html>' + LineEnding;
  
  Result := html;
end;

function TTaskExporter.TasksToMarkdown(const aTasks: TTaskArray; const aTitle: string): string;
var
  i: integer;
  md: string;
begin
  md := '# ' + aTitle + LineEnding + LineEnding;
  md := md + 'Generated: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + LineEnding + LineEnding;
  md := md + 'Total Tasks: ' + IntToStr(Length(aTasks)) + LineEnding + LineEnding;
  md := md + '| ID | Title | Category | Priority | Status | Due Date | Est. Hours | Actual Hours |' + LineEnding;
  md := md + '|----|-------|----------|----------|--------|----------|------------|--------------|' + LineEnding;
  
  for i := 0 to High(aTasks) do
  begin
    md := md + '| ' + IntToStr(aTasks[i].ID) + ' ';
    md := md + '| ' + aTasks[i].Title + ' ';
    md := md + '| ' + aTasks[i].Category + ' ';
    md := md + '| ' + PriorityToString(aTasks[i].Priority) + ' ';
    md := md + '| ' + StatusToString(aTasks[i].Status) + ' ';
    md := md + '| ' + DateTimeToStr(aTasks[i].DueDate) + ' ';
    md := md + '| ' + FloatToStrF(aTasks[i].EstimatedHours, ffFixed, 10, 1) + ' ';
    md := md + '| ' + FloatToStrF(aTasks[i].ActualHours, ffFixed, 10, 1) + ' |';
    md := md + LineEnding;
  end;
  
  Result := md;
end;

function TTaskExporter.CategoryStatisticsToHTML(const aStats: TCategoryStatisticsArray): string;
var
  i: integer;
  html: string;
begin
  html := '  <h2>Category Statistics</h2>' + LineEnding;
  html := html + '  <table>' + LineEnding;
  html := html + '    <thead>' + LineEnding;
  html := html + '      <tr><th>Category</th><th>Total</th><th>Active</th><th>Completed</th><th>Est. Hours</th><th>Actual Hours</th></tr>' + LineEnding;
  html := html + '    </thead>' + LineEnding;
  html := html + '    <tbody>' + LineEnding;
  
  for i := 0 to High(aStats) do
  begin
    html := html + '      <tr>' + LineEnding;
    html := html + '        <td><span class="category">' + EscapeHTML(aStats[i].CategoryName) + '</span></td>' + LineEnding;
    html := html + '        <td>' + IntToStr(aStats[i].TotalTasks) + '</td>' + LineEnding;
    html := html + '        <td>' + IntToStr(aStats[i].ActiveTasks) + '</td>' + LineEnding;
    html := html + '        <td>' + IntToStr(aStats[i].CompletedTasks) + '</td>' + LineEnding;
    html := html + '        <td>' + FloatToStrF(aStats[i].EstimatedHours, ffFixed, 10, 1) + '</td>' + LineEnding;
    html := html + '        <td>' + FloatToStrF(aStats[i].ActualHours, ffFixed, 10, 1) + '</td>' + LineEnding;
    html := html + '      </tr>' + LineEnding;
  end;
  
  html := html + '    </tbody>' + LineEnding;
  html := html + '  </table>' + LineEnding;
  
  Result := html;
end;

function TTaskExporter.GenerateHTMLReport(const aTasks: TTaskArray; const aStats: TTaskStatistics;
  const aCategoryStats: TCategoryStatisticsArray; const aFileName: string): boolean;
var
  html: string;
  f: TextFile;
begin
  Result := false;
  
  try
    // Generate comprehensive HTML report
    html := TasksToHTML(aTasks, 'Task Manager - Comprehensive Report');
    
    // Insert statistics section before the table
    html := StringReplace(html, '<table>', 
      '<h2>Overall Statistics</h2>' + LineEnding +
      '<table style="width: auto; margin-bottom: 20px;">' + LineEnding +
      '  <tr><td><strong>Total Tasks:</strong></td><td>' + IntToStr(aStats.TotalTasks) + '</td></tr>' + LineEnding +
      '  <tr><td><strong>Active Tasks:</strong></td><td>' + IntToStr(aStats.ActiveTasks) + '</td></tr>' + LineEnding +
      '  <tr><td><strong>Completed Tasks:</strong></td><td>' + IntToStr(aStats.CompletedTasks) + '</td></tr>' + LineEnding +
      '  <tr><td><strong>Cancelled Tasks:</strong></td><td>' + IntToStr(aStats.CancelledTasks) + '</td></tr>' + LineEnding +
      '  <tr><td><strong>High Priority Tasks:</strong></td><td>' + IntToStr(aStats.HighPriorityTasks) + '</td></tr>' + LineEnding +
      '  <tr><td><strong>Overdue Tasks:</strong></td><td>' + IntToStr(aStats.OverdueTasks) + '</td></tr>' + LineEnding +
      '  <tr><td><strong>Total Estimated Hours:</strong></td><td>' + FloatToStrF(aStats.TotalEstimatedHours, ffFixed, 10, 1) + '</td></tr>' + LineEnding +
      '  <tr><td><strong>Total Actual Hours:</strong></td><td>' + FloatToStrF(aStats.TotalActualHours, ffFixed, 10, 1) + '</td></tr>' + LineEnding +
      '</table>' + LineEnding +
      CategoryStatisticsToHTML(aCategoryStats) + LineEnding +
      '<h2>All Tasks</h2>' + LineEnding +
      '<table>', []);
    
    AssignFile(f, aFileName);
    Rewrite(f);
    Write(f, html);
    CloseFile(f);
    
    Result := true;
  except
    on E: Exception do
    begin
      if IOResult <> 0 then
        CloseFile(f);
    end;
  end;
end;

function TTaskExporter.ExportToJSON(const aTasks: TTaskArray; const aFileName: string): boolean;
var
  f: TextFile;
  json: string;
begin
  Result := false;
  
  try
    json := TasksToJSON(aTasks);
    
    AssignFile(f, aFileName);
    Rewrite(f);
    Write(f, json);
    CloseFile(f);
    
    Result := true;
  except
    on E: Exception do
    begin
      if IOResult <> 0 then
        CloseFile(f);
    end;
  end;
end;

function TTaskExporter.ExportToHTML(const aTasks: TTaskArray; const aFileName, aTitle: string): boolean;
var
  f: TextFile;
  html: string;
begin
  Result := false;
  
  try
    html := TasksToHTML(aTasks, aTitle);
    
    AssignFile(f, aFileName);
    Rewrite(f);
    Write(f, html);
    CloseFile(f);
    
    Result := true;
  except
    on E: Exception do
    begin
      if IOResult <> 0 then
        CloseFile(f);
    end;
  end;
end;

function TTaskExporter.ExportToMarkdown(const aTasks: TTaskArray; const aFileName, aTitle: string): boolean;
var
  f: TextFile;
  md: string;
begin
  Result := false;
  
  try
    md := TasksToMarkdown(aTasks, aTitle);
    
    AssignFile(f, aFileName);
    Rewrite(f);
    Write(f, md);
    CloseFile(f);
    
    Result := true;
  except
    on E: Exception do
    begin
      if IOResult <> 0 then
        CloseFile(f);
    end;
  end;
end;

function TTaskExporter.GetJSONString(const aTasks: TTaskArray): string;
begin
  Result := TasksToJSON(aTasks);
end;

function TTaskExporter.GetHTMLString(const aTasks: TTaskArray; const aTitle: string): string;
begin
  Result := TasksToHTML(aTasks, aTitle);
end;

end.
