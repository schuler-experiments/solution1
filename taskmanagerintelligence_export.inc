
{ Export Hub Implementation }

function TIntelligenceTaskManager.ExportToJSON: TExportResult;
var
  sb: TStringList;
  tasks: TTaskArray;
  i: Integer;
begin
  Result.Format := efJSON;
  Result.ExportedAt := Now;
  Result.Success := True;
  Result.ErrorMessage := '';
  
  sb := TStringList.Create;
  try
    sb.Add('{');
    sb.Add('  "task_manager_export": {');
    sb.Add(Format('    "exported_at": "%s",', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]));
    sb.Add('    "version": "1.0",');
    sb.Add(Format('    "total_tasks": %d,', [GetInheritedTaskCount]));
    sb.Add('    "tasks": [');
    
    tasks := GetInheritedAllTasks;
    for i := 0 to High(tasks) do
    begin
      sb.Add('      {');
      sb.Add(Format('        "id": %d,', [tasks[i].ID]));
      sb.Add(Format('        "title": "%s",', [tasks[i].Title]));
      sb.Add(Format('        "description": "%s",', [tasks[i].Description]));
      sb.Add(Format('        "category": "%s",', [tasks[i].Category]));
      sb.Add(Format('        "priority": "%s",', [GetInheritedTaskPriorityToString(tasks[i].Priority)]));
      sb.Add(Format('        "status": "%s",', [GetInheritedTaskStatusToString(tasks[i].Status)]));
      sb.Add(Format('        "due_date": "%s",', [FormatDateTime('yyyy-mm-dd', tasks[i].DueDate)]));
      sb.Add(Format('        "created_date": "%s"', [FormatDateTime('yyyy-mm-dd', tasks[i].CreatedDate)]));
      if i < High(tasks) then
        sb.Add('      },')
      else
        sb.Add('      }');
    end;
    
    sb.Add('    ]');
    sb.Add('  }');
    sb.Add('}');
    
    Result.Content := sb.Text;
    Result.FileSize := Length(Result.Content);
  finally
    sb.Free;
  end;
end;

function TIntelligenceTaskManager.ExportToXML: TExportResult;
var
  sb: TStringList;
  tasks: TTaskArray;
  i: Integer;
begin
  Result.Format := efXML;
  Result.ExportedAt := Now;
  Result.Success := True;
  Result.ErrorMessage := '';
  
  sb := TStringList.Create;
  try
    sb.Add('<?xml version="1.0" encoding="UTF-8"?>');
    sb.Add('<TaskManagerExport>');
    sb.Add(Format('  <ExportedAt>%s</ExportedAt>', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]));
    sb.Add(Format('  <TotalTasks>%d</TotalTasks>', [GetInheritedTaskCount]));
    sb.Add('  <Tasks>');
    
    tasks := GetInheritedAllTasks;
    for i := 0 to High(tasks) do
    begin
      sb.Add('    <Task>');
      sb.Add(Format('      <ID>%d</ID>', [tasks[i].ID]));
      sb.Add(Format('      <Title>%s</Title>', [tasks[i].Title]));
      sb.Add(Format('      <Description>%s</Description>', [tasks[i].Description]));
      sb.Add(Format('      <Category>%s</Category>', [tasks[i].Category]));
      sb.Add(Format('      <Priority>%s</Priority>', [GetInheritedTaskPriorityToString(tasks[i].Priority)]));
      sb.Add(Format('      <Status>%s</Status>', [GetInheritedTaskStatusToString(tasks[i].Status)]));
      sb.Add(Format('      <DueDate>%s</DueDate>', [FormatDateTime('yyyy-mm-dd', tasks[i].DueDate)]));
      sb.Add('    </Task>');
    end;
    
    sb.Add('  </Tasks>');
    sb.Add('</TaskManagerExport>');
    
    Result.Content := sb.Text;
    Result.FileSize := Length(Result.Content);
  finally
    sb.Free;
  end;
end;

function TIntelligenceTaskManager.ExportToICalendar: TExportResult;
var
  sb: TStringList;
  tasks: TTaskArray;
  i: Integer;
  uid: string;
begin
  Result.Format := efICalendar;
  Result.ExportedAt := Now;
  Result.Success := True;
  Result.ErrorMessage := '';
  
  sb := TStringList.Create;
  try
    sb.Add('BEGIN:VCALENDAR');
    sb.Add('VERSION:2.0');
    sb.Add('PRODID:-//Task Manager//Intelligence Layer//EN');
    sb.Add('CALSCALE:GREGORIAN');
    
    tasks := GetInheritedAllTasks;
    for i := 0 to High(tasks) do
    begin
      uid := Format('%d@taskmanager.local', [tasks[i].ID]);
      sb.Add('BEGIN:VTODO');
      sb.Add(Format('UID:%s', [uid]));
      sb.Add(Format('DTSTAMP:%s', [FormatDateTime('yyyymmdd"T"hhnnss"Z"', Now)]));
      sb.Add(Format('SUMMARY:%s', [tasks[i].Title]));
      sb.Add(Format('DESCRIPTION:%s', [tasks[i].Description]));
      sb.Add(Format('DUE:%s', [FormatDateTime('yyyymmdd', tasks[i].DueDate)]));
      
      case tasks[i].Priority of
        tpHigh: sb.Add('PRIORITY:1');
        tpMedium: sb.Add('PRIORITY:5');
        tpLow: sb.Add('PRIORITY:9');
      end;
      
      if tasks[i].Status = tsCompleted then
        sb.Add('STATUS:COMPLETED')
      else if tasks[i].Status = tsInProgress then
        sb.Add('STATUS:IN-PROCESS')
      else
        sb.Add('STATUS:NEEDS-ACTION');
      
      sb.Add('END:VTODO');
    end;
    
    sb.Add('END:VCALENDAR');
    
    Result.Content := sb.Text;
    Result.FileSize := Length(Result.Content);
  finally
    sb.Free;
  end;
end;

function TIntelligenceTaskManager.ExportToMarkdown: TExportResult;
var
  sb: TStringList;
  tasks: TTaskArray;
  i: Integer;
  statusSymbol: string;
begin
  Result.Format := efMarkdown;
  Result.ExportedAt := Now;
  Result.Success := True;
  Result.ErrorMessage := '';
  
  sb := TStringList.Create;
  try
    sb.Add('# Task Manager Export');
    sb.Add('');
    sb.Add(Format('**Exported:** %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]));
    sb.Add(Format('**Total Tasks:** %d', [GetInheritedTaskCount]));
    sb.Add(Format('**Completed:** %d', [GetInheritedCompletedCount]));
    sb.Add(Format('**Pending:** %d', [GetInheritedPendingCount]));
    sb.Add('');
    sb.Add('## Task List');
    sb.Add('');
    
    tasks := GetInheritedAllTasks;
    for i := 0 to High(tasks) do
    begin
      if tasks[i].Status = tsCompleted then
        statusSymbol := '✓'
      else if tasks[i].Status = tsInProgress then
        statusSymbol := '▶'
      else
        statusSymbol := '○';
      
      sb.Add(Format('### %s %s', [statusSymbol, tasks[i].Title]));
      sb.Add('');
      sb.Add(Format('- **Priority:** %s', [GetInheritedTaskPriorityToString(tasks[i].Priority)]));
      sb.Add(Format('- **Status:** %s', [GetInheritedTaskStatusToString(tasks[i].Status)]));
      sb.Add(Format('- **Category:** %s', [tasks[i].Category]));
      sb.Add(Format('- **Due Date:** %s', [FormatDateTime('yyyy-mm-dd', tasks[i].DueDate)]));
      if tasks[i].Description <> '' then
        sb.Add(Format('- **Description:** %s', [tasks[i].Description]));
      sb.Add('');
    end;
    
    Result.Content := sb.Text;
    Result.FileSize := Length(Result.Content);
  finally
    sb.Free;
  end;
end;

function TIntelligenceTaskManager.ExportToHTML: TExportResult;
var
  sb: TStringList;
  tasks: TTaskArray;
  i: Integer;
  statusClass, priorityClass: string;
begin
  Result.Format := efHTML;
  Result.ExportedAt := Now;
  Result.Success := True;
  Result.ErrorMessage := '';
  
  sb := TStringList.Create;
  try
    sb.Add('<!DOCTYPE html>');
    sb.Add('<html>');
    sb.Add('<head>');
    sb.Add('  <meta charset="UTF-8">');
    sb.Add('  <title>Task Manager Export</title>');
    sb.Add('  <style>');
    sb.Add('    body { font-family: Arial, sans-serif; margin: 20px; }');
    sb.Add('    .task { border: 1px solid #ddd; margin: 10px 0; padding: 15px; border-radius: 5px; }');
    sb.Add('    .high { border-left: 5px solid #f44336; }');
    sb.Add('    .medium { border-left: 5px solid #ff9800; }');
    sb.Add('    .low { border-left: 5px solid #4caf50; }');
    sb.Add('    .completed { background-color: #e8f5e9; }');
    sb.Add('    .in-progress { background-color: #fff3e0; }');
    sb.Add('    h1 { color: #333; }');
    sb.Add('  </style>');
    sb.Add('</head>');
    sb.Add('<body>');
    sb.Add('  <h1>Task Manager Export</h1>');
    sb.Add(Format('  <p><strong>Exported:</strong> %s</p>', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]));
    sb.Add(Format('  <p><strong>Total Tasks:</strong> %d</p>', [GetInheritedTaskCount]));
    
    tasks := GetInheritedAllTasks;
    for i := 0 to High(tasks) do
    begin
      case tasks[i].Priority of
        tpHigh: priorityClass := 'high';
        tpMedium: priorityClass := 'medium';
        tpLow: priorityClass := 'low';
      end;
      
      if tasks[i].Status = tsCompleted then
        statusClass := 'completed'
      else if tasks[i].Status = tsInProgress then
        statusClass := 'in-progress'
      else
        statusClass := '';
      
      sb.Add(Format('  <div class="task %s %s">', [priorityClass, statusClass]));
      sb.Add(Format('    <h3>%s</h3>', [tasks[i].Title]));
      sb.Add(Format('    <p><strong>Priority:</strong> %s</p>', [GetInheritedTaskPriorityToString(tasks[i].Priority)]));
      sb.Add(Format('    <p><strong>Status:</strong> %s</p>', [GetInheritedTaskStatusToString(tasks[i].Status)]));
      sb.Add(Format('    <p><strong>Category:</strong> %s</p>', [tasks[i].Category]));
      sb.Add(Format('    <p><strong>Due Date:</strong> %s</p>', [FormatDateTime('yyyy-mm-dd', tasks[i].DueDate)]));
      if tasks[i].Description <> '' then
        sb.Add(Format('    <p>%s</p>', [tasks[i].Description]));
      sb.Add('  </div>');
    end;
    
    sb.Add('</body>');
    sb.Add('</html>');
    
    Result.Content := sb.Text;
    Result.FileSize := Length(Result.Content);
  finally
    sb.Free;
  end;
end;

function TIntelligenceTaskManager.ExportWithFormat(AFormat: TExportFormat): TExportResult;
begin
  case AFormat of
    efJSON: Result := ExportToJSON;
    efXML: Result := ExportToXML;
    efICalendar: Result := ExportToICalendar;
    efMarkdown: Result := ExportToMarkdown;
    efHTML: Result := ExportToHTML;
    efCSV: begin
      Result.Format := efCSV;
      Result.Success := True;
      Result.Content := GetInheritedCSVExport;
      Result.FileSize := Length(Result.Content);
      Result.ExportedAt := Now;
    end;
  end;
end;

{ Utility Functions }

function TIntelligenceTaskManager.BulkOperationTypeToString(
  AType: TBulkOperationType): string;
begin
  case AType of
    boUpdateStatus: Result := 'Update Status';
    boUpdatePriority: Result := 'Update Priority';
    boUpdateCategory: Result := 'Update Category';
    boAddTag: Result := 'Add Tag';
    boRemoveTag: Result := 'Remove Tag';
    boDelete: Result := 'Delete';
    boArchive: Result := 'Archive';
    boAssignMember: Result := 'Assign Member';
    boSetDueDate: Result := 'Set Due Date';
    boAddToGoal: Result := 'Add to Goal';
  else
    Result := 'Unknown';
  end;
end;

function TIntelligenceTaskManager.NotificationChannelToString(
  AChannel: TNotificationChannel): string;
begin
  case AChannel of
    ncConsole: Result := 'Console';
    ncFile: Result := 'File';
    ncEmail: Result := 'Email';
    ncWebhook: Result := 'Webhook';
  else
    Result := 'Unknown';
  end;
end;

function TIntelligenceTaskManager.NotificationPriorityToString(
  APriority: TNotificationPriority): string;
begin
  case APriority of
    npLow: Result := 'Low';
    npNormal: Result := 'Normal';
    npHigh: Result := 'High';
    npCritical: Result := 'Critical';
  else
    Result := 'Unknown';
  end;
end;

function TIntelligenceTaskManager.ExportFormatToString(AFormat: TExportFormat): string;
begin
  case AFormat of
    efJSON: Result := 'JSON';
    efXML: Result := 'XML';
    efICalendar: Result := 'iCalendar';
    efMarkdown: Result := 'Markdown';
    efHTML: Result := 'HTML';
    efCSV: Result := 'CSV';
  else
    Result := 'Unknown';
  end;
end;
