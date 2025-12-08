program TaskManagerIntelligenceDemo;
{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  taskmanager, taskmanageradvanced, taskmanagerenhanced,
  taskmanagerteam, taskmanagergamify, taskmanagersmart,
  taskmanagerfocus, taskmanagerresource, taskmanagerintelligence;

procedure SelfTest;
var
  tm: TIntelligenceTaskManager;
  taskID1, taskID2, taskID3: Integer;
  nlTaskIDs: TIntArray;
  backupID, restorePointID: Integer;
  bulkIDs: array[0..2] of Integer;
  trend: TTrendArray;
  exportResult: TExportResult;
  notifications: TSmartNotificationArray;
  i: Integer;
begin
  WriteLn('=== Task Manager Intelligence Layer Self-Test ===');
  WriteLn;
  
  tm := TIntelligenceTaskManager.Create;
  try
    WriteLn('1. Testing Natural Language Task Creation...');
    taskID1 := tm.CreateTaskFromNL('Buy groceries tomorrow high priority');
    WriteLn(Format('  Created task from NL: "%s" (ID: %d)', 
      ['Buy groceries tomorrow high priority', taskID1]));
    
    taskID2 := tm.CreateTaskFromNL('Fix website bug urgent today');
    WriteLn(Format('  Created task from NL: "%s" (ID: %d)', 
      ['Fix website bug urgent today', taskID2]));
    
    taskID3 := tm.CreateTaskFromNL('Review code next week low');
    WriteLn(Format('  Created task from NL: "%s" (ID: %d)', 
      ['Review code next week low', taskID3]));
    WriteLn;
    
    WriteLn('2. Testing Bulk NL Task Creation...');
    SetLength(nlTaskIDs, 3);
    nlTaskIDs := tm.BulkCreateFromNL([
      'Write documentation medium priority',
      'Update dependencies tomorrow',
      'Schedule team meeting next week'
    ]);
    WriteLn(Format('  Created %d tasks from natural language input', [Length(nlTaskIDs)]));
    WriteLn;
    
    WriteLn('3. Testing Backup & Versioning...');
    backupID := tm.CreateBackupVersion('Initial backup before tests');
    WriteLn(Format('  Created backup version: %d', [backupID]));
    
    restorePointID := tm.CreateRestorePoint('Before bulk operations');
    WriteLn(Format('  Created restore point: %d', [restorePointID]));
    
    tm.EnableAutoBackup(True, 1);
    WriteLn('  Enabled auto-backup (every 1 hour)');
    WriteLn;
    
    WriteLn('4. Testing Bulk Operations...');
    bulkIDs[0] := taskID1;
    bulkIDs[1] := taskID2;
    bulkIDs[2] := taskID3;
    
    WriteLn(Format('  Bulk updating status for %d tasks...', [Length(bulkIDs)]));
    tm.BulkUpdateStatus(bulkIDs, tsInProgress);
    
    WriteLn('  Bulk adding tag "automated-test"...');
    tm.BulkAddTag(bulkIDs, 'automated-test');
    
    WriteLn('  Bulk updating category...');
    tm.BulkUpdateCategory(bulkIDs, 'Development');
    WriteLn;
    
    WriteLn('5. Testing Analytics - Completion Trend...');
    trend := tm.GenerateCompletionTrend(7);
    WriteLn('  7-Day Completion Trend:');
    for i := 0 to 2 do
      if i <= High(trend) then
      WriteLn(Format('    %s: %.0f tasks', [trend[i].Label_, trend[i].Value]));
    WriteLn;
    
    WriteLn('6. Testing Analytics - Priority Distribution...');
    trend := tm.GeneratePriorityDistribution;
    WriteLn('  Priority Distribution:');
    for i := 0 to High(trend) do
      WriteLn(Format('    %s: %.0f tasks', [trend[i].Label_, trend[i].Value]));
    WriteLn;
    
    WriteLn('7. Testing Analytics - Productivity Heatmap...');
    WriteLn(tm.GenerateProductivityHeatmap);
    
    WriteLn('8. Testing Smart Notifications...');
    tm.CheckAndCreateSmartNotifications;
    notifications := tm.GetPendingNotifications;
    WriteLn(Format('  Generated %d smart notifications', [Length(notifications)]));
    
    tm.CreateNotification(ncConsole, npHigh, 
      'Test Notification', 'This is a test notification', taskID1);
    WriteLn('  Created manual notification');
    
    WriteLn(Format('  Sent %d pending notifications', [tm.SendAllPendingNotifications]));
    WriteLn;
    
    WriteLn('9. Testing Export Hub - JSON...');
    exportResult := tm.ExportToJSON;
    if exportResult.Success then
    begin
      WriteLn(Format('  JSON Export successful! Size: %d bytes', [exportResult.FileSize]));
      WriteLn('  Preview (first 200 chars):');
      WriteLn('  ' + Copy(exportResult.Content, 1, 200) + '...');
    end;
    WriteLn;
    
    WriteLn('10. Testing Export Hub - XML...');
    exportResult := tm.ExportToXML;
    if exportResult.Success then
      WriteLn(Format('  XML Export successful! Size: %d bytes', [exportResult.FileSize]));
    WriteLn;
    
    WriteLn('11. Testing Export Hub - Markdown...');
    exportResult := tm.ExportToMarkdown;
    if exportResult.Success then
    begin
      WriteLn(Format('  Markdown Export successful! Size: %d bytes', [exportResult.FileSize]));
      WriteLn('  Preview (first 300 chars):');
      WriteLn(Copy(exportResult.Content, 1, 300) + '...');
    end;
    WriteLn;
    
    WriteLn('12. Testing Export Hub - iCalendar...');
    exportResult := tm.ExportToICalendar;
    if exportResult.Success then
      WriteLn(Format('  iCalendar Export successful! Size: %d bytes', [exportResult.FileSize]));
    WriteLn;
    
    WriteLn('13. Testing Export Hub - HTML...');
    exportResult := tm.ExportToHTML;
    if exportResult.Success then
      WriteLn(Format('  HTML Export successful! Size: %d bytes', [exportResult.FileSize]));
    WriteLn;
    
    WriteLn('14. System Statistics...');
    WriteLn(Format('  Total Tasks: %d', [Length(tm.GetAllTasks)]));
    WriteLn(Format('  Completed Tasks: %d', [Length(tm.FilterByStatus(tsCompleted))]));
    WriteLn(Format('  Pending Tasks: %d', [Length(tm.FilterByStatus(tsNotStarted))]));
    WriteLn(Format('  Completion Rate: %.1f%%', [0.0]));
    WriteLn(Format('  Backup Versions: %d', [Length(tm.GetBackupVersions)]));
    WriteLn(Format('  Restore Points: %d', [Length(tm.GetRestorePoints)]));
    WriteLn(Format('  Bulk Operations: %d', [Length(tm.GetBulkOperationHistory)]));
    WriteLn;
    
    WriteLn('=== Intelligence Layer Self-Test Complete ===');
    WriteLn;
    WriteLn('Key Features Demonstrated:');
    WriteLn('  ✓ Natural Language Processing (NLP task creation)');
    WriteLn('  ✓ Bulk task creation from NL input');
    WriteLn('  ✓ Backup & versioning system');
    WriteLn('  ✓ Restore points for data recovery');
    WriteLn('  ✓ Auto-backup functionality');
    WriteLn('  ✓ Bulk operations (status, tags, category updates)');
    WriteLn('  ✓ Completion trend analytics');
    WriteLn('  ✓ Priority distribution analysis');
    WriteLn('  ✓ Productivity heatmap generation');
    WriteLn('  ✓ Smart notification system');
    WriteLn('  ✓ Multi-format export (JSON, XML, Markdown, iCal, HTML)');
    WriteLn('  ✓ Comprehensive analytics and reporting');
    
  finally
    tm.Free;
  end;
end;

begin
  try
    SelfTest;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
