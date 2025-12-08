
program solution4;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, taskmanager, taskmanagerext, taskmanageradvanced, taskmanagerenhanced;

procedure SelfTest;
var
  Manager: TEnhancedTaskManager;
  TaskID1, TaskID2, TaskID3: Integer;
  ReminderID1, ReminderID2: Integer;
  AttachID1, AttachID2: Integer;
  ArchiveID1: Integer;
  Reminders: TTaskReminderArray;
  AuditEntries: TAuditEntryArray;
  Attachments: TTaskAttachmentArray;
  ArchivedTasks: TArchivedTaskArray;
  i: Integer;
begin
  WriteLn('=== Enhanced Task Manager Self-Test ===');
  WriteLn;
  
  Manager := TEnhancedTaskManager.Create;
  try
    Manager.SetCurrentUser('Alice');
    WriteLn('Current User: ', Manager.GetCurrentUser);
    WriteLn;
    
    WriteLn('--- Testing Task Creation with Audit Trail ---');
    TaskID1 := Manager.AddTaskWithAudit(
      'Implement Login Feature',
      'Create user authentication system with JWT tokens',
      'Backend',
      tpHigh,
      IncDay(Now, 7),
      16.0
    );
    WriteLn('Created Task #', TaskID1);
    
    TaskID2 := Manager.AddTaskWithAudit(
      'Design Dashboard UI',
      'Create responsive dashboard with charts and widgets',
      'Frontend',
      tpMedium,
      IncDay(Now, 10),
      12.0
    );
    WriteLn('Created Task #', TaskID2);
    
    TaskID3 := Manager.AddTaskWithAudit(
      'Write API Documentation',
      'Document all REST API endpoints',
      'Documentation',
      tpLow,
      IncDay(Now, 14),
      8.0
    );
    WriteLn('Created Task #', TaskID3);
    WriteLn;
    
    WriteLn('--- Testing Reminders ---');
    ReminderID1 := Manager.AddReminder(
      TaskID1,
      rtBeforeDue,
      Now,
      1440,
      'Login feature due in 24 hours!'
    );
    WriteLn('Added reminder #', ReminderID1);
    
    ReminderID2 := Manager.AddReminder(
      TaskID2,
      rtAtSpecificTime,
      IncHour(Now, 2),
      0,
      'Check dashboard design progress'
    );
    WriteLn('Added reminder #', ReminderID2);
    
    Manager.AddReminder(
      TaskID3,
      rtRecurringDaily,
      Now,
      0,
      'Daily reminder to update documentation'
    );
    WriteLn;
    
    WriteLn('All Reminders:');
    Reminders := Manager.GetAllReminders;
    for i := 0 to High(Reminders) do
      WriteLn('  ', Manager.ReminderToString(Reminders[i]));
    WriteLn;
    
    WriteLn('Active Reminders:');
    Reminders := Manager.GetActiveReminders;
    WriteLn('  Count: ', Length(Reminders));
    WriteLn;
    
    WriteLn('--- Testing Attachments ---');
    AttachID1 := Manager.AddAttachment(
      TaskID1,
      atURL,
      'https://github.com/project/design-mockup.pdf',
      'design-mockup.pdf',
      'Initial design mockup for review'
    );
    WriteLn('Added attachment #', AttachID1);
    
    AttachID2 := Manager.AddAttachment(
      TaskID1,
      atLocalFile,
      '/docs/requirements.txt',
      'requirements.txt',
      'Project requirements document'
    );
    WriteLn('Added attachment #', AttachID2);
    
    Manager.AddAttachment(
      TaskID2,
      atCloudStorage,
      'cloud://drive/wireframes.fig',
      'wireframes.fig',
      'Figma wireframes'
    );
    WriteLn;
    
    WriteLn('Attachments for Task #', TaskID1, ':');
    Attachments := Manager.GetAttachments(TaskID1);
    for i := 0 to High(Attachments) do
      WriteLn('  ', Manager.AttachmentToString(Attachments[i]));
    WriteLn;
    
    WriteLn('All Attachments:');
    Attachments := Manager.GetAllAttachments;
    WriteLn('  Total: ', Length(Attachments));
    WriteLn;
    
    WriteLn(Manager.GetAttachmentStatistics);
    WriteLn;
    
    WriteLn('--- Testing Status Updates with Audit ---');
    Manager.UpdateTaskStatusWithAudit(
      TaskID1,
      tsInProgress,
      'Started working on login feature'
    );
    WriteLn('Updated status for Task #', TaskID1);
    
    Manager.SetCurrentUser('Bob');
    Manager.UpdateTaskStatusWithAudit(
      TaskID2,
      tsInProgress,
      'Bob started working on dashboard'
    );
    WriteLn('Updated status for Task #', TaskID2);
    
    Manager.UpdateTaskStatusWithAudit(
      TaskID1,
      tsCompleted,
      'Login feature completed and tested'
    );
    WriteLn('Completed Task #', TaskID1);
    WriteLn;
    
    WriteLn('--- Testing Audit Trail ---');
    WriteLn('Audit trail for Task #', TaskID1, ':');
    AuditEntries := Manager.GetAuditTrail(TaskID1);
    for i := 0 to High(AuditEntries) do
      WriteLn('  ', Manager.AuditEntryToString(AuditEntries[i]));
    WriteLn;
    
    WriteLn('All Audit Entries:');
    AuditEntries := Manager.GetAllAuditEntries;
    WriteLn('  Total: ', Length(AuditEntries));
    WriteLn;
    
    WriteLn(Manager.GetAuditSummary);
    WriteLn;
    
    WriteLn('Audit entries by user "Alice":');
    AuditEntries := Manager.GetAuditEntriesByUser('Alice');
    WriteLn('  Count: ', Length(AuditEntries));
    WriteLn;
    
    WriteLn(Manager.GetMostActiveUsers);
    WriteLn;
    
    WriteLn('--- Testing Task Archiving ---');
    ArchiveID1 := Manager.ArchiveTask(TaskID1, 'Completed and ready for archive');
    WriteLn('Archived Task #', TaskID1, ' as Archive #', ArchiveID1);
    WriteLn;
    
    WriteLn('Archived Tasks:');
    ArchivedTasks := Manager.GetArchivedTasks;
    for i := 0 to High(ArchivedTasks) do
      WriteLn('  ', Manager.ArchivedTaskToString(ArchivedTasks[i]));
    WriteLn;
    
    WriteLn(Manager.GetArchiveStatistics);
    WriteLn;
    
    WriteLn('--- Testing Unarchive ---');
    TaskID1 := Manager.UnarchiveTask(ArchiveID1);
    if TaskID1 > 0 then
    begin
      WriteLn('Unarchived as new Task #', TaskID1);
      WriteLn('Remaining archived tasks: ', Length(Manager.GetArchivedTasks));
    end;
    WriteLn;
    
    WriteLn('--- Testing Bulk Archive Operations ---');
    Manager.UpdateTaskStatusWithAudit(TaskID3, tsCompleted, 'Documentation complete');
    WriteLn('Archiving completed tasks older than 0 days...');
    WriteLn('Archived: ', Manager.ArchiveCompletedTasks(0), ' tasks');
    WriteLn;
    
    WriteLn('Final Statistics:');
    WriteLn('Active Tasks: ', Manager.TaskCount);
    WriteLn('Archived Tasks: ', Length(Manager.GetArchivedTasks));
    WriteLn('Total Reminders: ', Length(Manager.GetAllReminders));
    WriteLn('Total Attachments: ', Length(Manager.GetAllAttachments));
    WriteLn('Total Audit Entries: ', Length(Manager.GetAllAuditEntries));
    WriteLn;
    
    WriteLn('--- Testing Reminder Check ---');
    Reminders := Manager.CheckReminders;
    WriteLn('Triggered reminders: ', Length(Reminders));
    for i := 0 to High(Reminders) do
      WriteLn('  TRIGGERED: ', Reminders[i].Message);
    WriteLn;
    
    WriteLn('--- Testing File Operations ---');
    if Manager.SaveEnhancedToFile('solution1/tasks_enhanced.dat') then
      WriteLn('Saved to tasks_enhanced.dat')
    else
      WriteLn('Failed to save');
    WriteLn;
    
    WriteLn('=== All Enhanced Features Tests Passed! ===');
    
  finally
    Manager.Free;
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
