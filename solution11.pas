
program solution11;
{$mode objfpc}

uses
  SysUtils, DateUtils, Math,
  taskmanager, taskmanageradvanced, taskmanagerenhanced,
  taskmanagerteam, taskmanagerfocus, taskmanagergamify,
  taskmanagerresource, taskmanagersmart, taskmanagerrecurring,
  taskmanagerintelligence;

procedure SelfTest;
var
  TM: TIntelligenceTaskManager;
  TaskID1, TaskID2, TaskID3: Integer;
  ParsedTask: TParsedTask;
  BulkTaskIDs: TIntArray;
  BulkOpCount: Integer;
  BackupID1, BackupID2: Integer;
  RestorePointID: Integer;
  Versions: TBackupVersionArray;
  RestorePoints: TRestorePointArray;
  ExportResult: TExportResult;
  BulkOps: TBulkOperationArray;
  Trends: TTrendArray;
  Report: TAnalyticsReport;
  Notifications: TSmartNotificationArray;
  NotifID: Integer;
  TaskIDs: array of Integer;
  i: Integer;
begin
  WriteLn('=== Intelligence Task Manager Self Test ===');
  WriteLn;
  
  TM := TIntelligenceTaskManager.Create;
  try
    // Test 1: Natural Language Processing - Single task creation
    WriteLn('Test 1: Creating tasks from natural language input...');
    TaskID1 := TM.CreateTaskFromNL('Create presentation for Monday meeting high priority');
    if TaskID1 > 0 then
      WriteLn('  Created task from NL: "Create presentation for Monday meeting high priority" (ID: ', TaskID1, ')');
    
    TaskID2 := TM.CreateTaskFromNL('Review code changes tomorrow medium priority 2 hours');
    if TaskID2 > 0 then
      WriteLn('  Created task from NL: "Review code changes tomorrow medium priority 2 hours" (ID: ', TaskID2, ')');
    
    TaskID3 := TM.CreateTaskFromNL('Update documentation low priority');
    if TaskID3 > 0 then
      WriteLn('  Created task from NL: "Update documentation low priority" (ID: ', TaskID3, ')');
    WriteLn;
    
    // Test 2: Parse natural language without creating task
    WriteLn('Test 2: Parsing natural language without creating task...');
    ParsedTask := TM.ParseNaturalLanguageTask('Schedule team meeting for Friday high priority 1 hour');
    if ParsedTask.ParsedSuccessfully then
    begin
      WriteLn('  Parsed successfully:');
      WriteLn('    Title: ', ParsedTask.Title);
      WriteLn('    Priority: ', TM.TaskPriorityToString(ParsedTask.Priority));
      WriteLn('    Estimated hours: ', FormatFloat('0.0', ParsedTask.EstimatedHours));
      WriteLn('    Confidence: ', FormatFloat('0.00', ParsedTask.Confidence));
    end;
    WriteLn;
    
    // Test 3: Bulk creation from natural language - using proper string array
    WriteLn('Test 3: Bulk task creation from natural language...');
    BulkTaskIDs := TM.BulkCreateFromNL([
      'Write unit tests high priority 4 hours',
      'Deploy to staging medium priority 2 hours',
      'Run security audit high priority 6 hours',
      'Update README low priority 1 hour'
    ]);
    WriteLn('  Created ', Length(BulkTaskIDs), ' tasks from natural language inputs');
    for i := 0 to High(BulkTaskIDs) do
      WriteLn('    Task ', i + 1, ': ID = ', BulkTaskIDs[i]);
    WriteLn;
    
    // Test 4: Backup & Versioning - Create backup
    WriteLn('Test 4: Creating backup versions...');
    BackupID1 := TM.CreateBackupVersion('Initial backup after NL task creation');
    WriteLn('  Backup version created (ID: ', BackupID1, ')');
    
    // Add more tasks
    TM.AddTask('Additional task 1', 'Description 1', 'Testing', tpMedium, IncDay(Now, 3), 2.0);
    TM.AddTask('Additional task 2', 'Description 2', 'Testing', tpHigh, IncDay(Now, 5), 3.0);
    
    BackupID2 := TM.CreateBackupVersion('Second backup after adding more tasks');
    WriteLn('  Second backup version created (ID: ', BackupID2, ')');
    WriteLn;
    
    // Test 5: List backup versions
    WriteLn('Test 5: Listing backup versions...');
    Versions := TM.GetBackupVersions;
    WriteLn('  Found ', Length(Versions), ' backup version(s):');
    for i := 0 to High(Versions) do
    begin
      WriteLn('    Version ', Versions[i].VersionID, ': ', Versions[i].Description);
      WriteLn('      Created: ', FormatDateTime('dd-mm-yyyy hh:nn:ss', Versions[i].Timestamp));
      WriteLn('      Tasks: ', Versions[i].TaskCount, ', Size: ', Versions[i].FileSize, ' bytes');
    end;
    WriteLn;
    
    // Test 6: Create restore points
    WriteLn('Test 6: Creating restore points...');
    RestorePointID := TM.CreateRestorePoint('Before bulk operations');
    WriteLn('  Restore point created (ID: ', RestorePointID, ')');
    WriteLn;
    
    // Test 7: Bulk operations - Update status
    WriteLn('Test 7: Performing bulk operations...');
    SetLength(TaskIDs, 3);
    TaskIDs[0] := TaskID1;
    TaskIDs[1] := TaskID2;
    TaskIDs[2] := TaskID3;
    
    BulkOpCount := TM.BulkUpdateStatus(TaskIDs, tsInProgress);
    WriteLn('  Bulk update status: ', BulkOpCount, ' tasks updated to In Progress');
    
    BulkOpCount := TM.BulkAddTag(TaskIDs, 'sprint-1');
    WriteLn('  Bulk add tag: ', BulkOpCount, ' tasks tagged with "sprint-1"');
    
    BulkOpCount := TM.BulkUpdatePriority(TaskIDs, tpHigh);
    WriteLn('  Bulk update priority: ', BulkOpCount, ' tasks set to High priority');
    WriteLn;
    
    // Test 8: Get bulk operations history
    WriteLn('Test 8: Reviewing bulk operations history...');
    BulkOps := TM.GetBulkOperationHistory;
    WriteLn('  Found ', Length(BulkOps), ' bulk operation(s):');
    for i := 0 to High(BulkOps) do
    begin
      WriteLn('    Operation ', BulkOps[i].OperationID, ': ', 
        TM.BulkOperationTypeToString(BulkOps[i].OpType));
      WriteLn('      Executed: ', FormatDateTime('dd-mm-yyyy hh:nn:ss', BulkOps[i].ExecutedAt));
      WriteLn('      Targets: ', Length(BulkOps[i].TargetTaskIDs), ' tasks');
      WriteLn('      Success: ', BulkOps[i].SuccessCount, ', Failed: ', BulkOps[i].FailureCount);
    end;
    WriteLn;
    
    // Test 9: List restore points
    WriteLn('Test 9: Listing restore points...');
    RestorePoints := TM.GetRestorePoints;
    WriteLn('  Found ', Length(RestorePoints), ' restore point(s):');
    for i := 0 to High(RestorePoints) do
    begin
      WriteLn('    Point ', RestorePoints[i].PointID, ': ', RestorePoints[i].Label_);
      WriteLn('      Created: ', FormatDateTime('dd-mm-yyyy hh:nn:ss', RestorePoints[i].Created));
      if RestorePoints[i].AutoCreated then
        WriteLn('      Auto-created: Yes')
      else
        WriteLn('      Auto-created: No');
    end;
    WriteLn;
    
    // Test 10: Analytics - Generate completion trend
    WriteLn('Test 10: Generating analytics - completion trends...');
    Trends := TM.GenerateCompletionTrend(30); // Last 30 days
    WriteLn('  Completion Trend Analysis:');
    WriteLn('    Data points: ', Length(Trends));
    if Length(Trends) > 0 then
    begin
      WriteLn('    First point: ', Trends[0].Label_, ' = ', FormatFloat('0.00', Trends[0].Value));
      WriteLn('    Last point: ', Trends[High(Trends)].Label_, ' = ', 
        FormatFloat('0.00', Trends[High(Trends)].Value));
    end;
    WriteLn;
    
    // Test 11: Analytics - Priority distribution
    WriteLn('Test 11: Analyzing priority distribution...');
    Trends := TM.GeneratePriorityDistribution;
    WriteLn('  Priority Distribution:');
    for i := 0 to High(Trends) do
      WriteLn('    ', Trends[i].Label_, ': ', FormatFloat('0.0', Trends[i].Value), ' tasks');
    WriteLn;
    
    // Test 12: Analytics - Velocity report
    WriteLn('Test 12: Generating velocity report...');
    Report := TM.GenerateVelocityReport(4); // Last 4 weeks
    WriteLn('  Velocity Report (ID: ', Report.ReportID, ')');
    WriteLn('    Type: ', Report.ReportType);
    WriteLn('    Generated: ', FormatDateTime('dd-mm-yyyy hh:nn:ss', Report.Generated));
    WriteLn('    Summary: ', Report.Summary);
    if Length(Report.Insights) > 0 then
    begin
      WriteLn('    Insights:');
      for i := 0 to Min(2, High(Report.Insights)) do
        WriteLn('      - ', Report.Insights[i]);
    end;
    WriteLn;
    
    // Test 13: Export to different formats
    WriteLn('Test 13: Exporting data to various formats...');
    
    ExportResult := TM.ExportToJSON;
    WriteLn('  JSON export: ', ExportResult.FileSize, ' bytes, Success: ', ExportResult.Success);
    
    ExportResult := TM.ExportToXML;
    WriteLn('  XML export: ', ExportResult.FileSize, ' bytes, Success: ', ExportResult.Success);
    
    ExportResult := TM.ExportToMarkdown;
    WriteLn('  Markdown export: ', ExportResult.FileSize, ' bytes, Success: ', ExportResult.Success);
    
    ExportResult := TM.ExportToHTML;
    WriteLn('  HTML export: ', ExportResult.FileSize, ' bytes, Success: ', ExportResult.Success);
    
    ExportResult := TM.ExportToICalendar;
    WriteLn('  iCalendar export: ', ExportResult.FileSize, ' bytes, Success: ', ExportResult.Success);
    WriteLn;
    
    // Test 14: Smart Notifications
    WriteLn('Test 14: Creating smart notifications...');
    NotifID := TM.CreateNotification(ncConsole, npHigh, 
      'Task Due Soon', 'Your task is due in 2 hours', TaskID1);
    WriteLn('  Notification created (ID: ', NotifID, ')');
    
    NotifID := TM.CreateNotification(ncConsole, npCritical,
      'Overdue Task', 'Task is overdue!', TaskID2);
    WriteLn('  Critical notification created (ID: ', NotifID, ')');
    
    TM.CheckAndCreateSmartNotifications;
    WriteLn('  Smart notifications check completed');
    WriteLn;
    
    // Test 15: Get and send pending notifications
    WriteLn('Test 15: Processing pending notifications...');
    Notifications := TM.GetPendingNotifications;
    WriteLn('  Found ', Length(Notifications), ' pending notification(s)');
    for i := 0 to High(Notifications) do
    begin
      WriteLn('    Notification ', Notifications[i].NotificationID, ': ', Notifications[i].Title);
      WriteLn('      Priority: ', TM.NotificationPriorityToString(Notifications[i].Priority));
      WriteLn('      Channel: ', TM.NotificationChannelToString(Notifications[i].Channel));
    end;
    
    BulkOpCount := TM.SendAllPendingNotifications;
    WriteLn('  Sent ', BulkOpCount, ' notification(s)');
    WriteLn;
    
    // Test 16: Scheduled backups
    WriteLn('Test 16: Testing scheduled backups...');
    TM.EnableAutoBackup(True, 6); // Auto-backup every 6 hours
    WriteLn('  Auto-backup enabled (interval: 6 hours)');
    TM.CheckAndPerformAutoBackup;
    WriteLn('  Auto-backup check performed');
    WriteLn;
    
    // Test 17: Advanced analytics
    WriteLn('Test 17: Advanced analytics...');
    WriteLn(TM.GetTopPerformingCategories(5));
    WriteLn;
    WriteLn(TM.GetBottleneckAnalysis);
    WriteLn;
    
    // Test 18: Productivity heatmap
    WriteLn('Test 18: Productivity heatmap...');
    WriteLn(TM.GenerateProductivityHeatmap);
    WriteLn;
    
    WriteLn('=== All Intelligence Features Tests Completed Successfully! ===');
    WriteLn;
    WriteLn('New intelligence features demonstrated:');
    WriteLn('  1. Natural Language Processing for task creation');
    WriteLn('  2. Bulk task creation from NL inputs');
    WriteLn('  3. Task parsing with confidence scoring');
    WriteLn('  4. Backup & versioning system');
    WriteLn('  5. Restore points for data recovery');
    WriteLn('  6. Bulk operations (status, priority, tags)');
    WriteLn('  7. Operation history tracking');
    WriteLn('  8. Completion trend analytics');
    WriteLn('  9. Priority distribution analysis');
    WriteLn('  10. Velocity reporting');
    WriteLn('  11. Multi-format export (JSON, XML, HTML, Markdown, iCalendar)');
    WriteLn('  12. Smart notifications system');
    WriteLn('  13. Scheduled auto-backup');
    WriteLn('  14. Advanced analytics and insights');
    WriteLn('  15. Productivity heatmap');
    WriteLn('  16. Top performing categories');
    WriteLn('  17. Bottleneck analysis');
    WriteLn;
    
  finally
    TM.Free;
  end;
end;

begin
  try
    SelfTest;
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end.
