
unit taskmanagerenhanced;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Classes, Math, taskmanager, taskmanagerext, taskmanageradvanced;

type
  // Reminder types
  TReminderType = (rtBeforeDue, rtAtSpecificTime, rtRecurringDaily, rtRecurringWeekly);
  
  // Task reminder
  TTaskReminder = record
    ReminderID: Integer;
    TaskID: Integer;
    ReminderType: TReminderType;
    ReminderTime: TDateTime;
    MinutesBeforeDue: Integer;  // For rtBeforeDue type
    Message: string;
    IsActive: Boolean;
    LastTriggered: TDateTime;
    CreatedDate: TDateTime;
  end;
  TTaskReminderArray = array of TTaskReminder;
  
  // Audit trail entry types
  TAuditAction = (aaCreate, aaUpdate, aaDelete, aaStatusChange, aaComplete, 
                  aaArchive, aaRestore, aaAssign, aaPriorityChange);
  
  // Audit trail entry
  TAuditEntry = record
    AuditID: Integer;
    TaskID: Integer;
    Action: TAuditAction;
    Timestamp: TDateTime;
    UserName: string;
    FieldChanged: string;
    OldValue: string;
    NewValue: string;
    Description: string;
  end;
  TAuditEntryArray = array of TAuditEntry;
  
  // File attachment/reference
  TAttachmentType = (atLocalFile, atURL, atNetworkPath, atCloudStorage);
  
  TTaskAttachment = record
    AttachmentID: Integer;
    TaskID: Integer;
    AttachmentType: TAttachmentType;
    FilePath: string;
    FileName: string;
    FileSize: Int64;  // in bytes
    MimeType: string;
    Description: string;
    AddedDate: TDateTime;
    AddedBy: string;
  end;
  TTaskAttachmentArray = array of TTaskAttachment;
  
  // Archived task (includes original task data plus archive metadata)
  TArchivedTask = record
    ArchiveID: Integer;
    OriginalTask: TExtendedTask;
    ArchivedDate: TDateTime;
    ArchivedBy: string;
    ArchiveReason: string;
    OriginalTaskID: Integer;
  end;
  TArchivedTaskArray = array of TArchivedTask;
  
  // Enhanced task manager with reminders, audit trail, archiving, attachments
  TEnhancedTaskManager = class(TAdvancedTaskManager)
  private
    FReminders: TTaskReminderArray;
    FAuditTrail: TAuditEntryArray;
    FAttachments: TTaskAttachmentArray;
    FArchivedTasks: TArchivedTaskArray;
    FNextReminderID: Integer;
    FNextAuditID: Integer;
    FNextAttachmentID: Integer;
    FNextArchiveID: Integer;
    FCurrentUser: string;
    
    procedure LogAudit(ATaskID: Integer; AAction: TAuditAction; 
                      const AFieldChanged, AOldValue, ANewValue, ADescription: string);
    function GetRemindersForTask(ATaskID: Integer): TTaskReminderArray;
    function GetAttachmentsForTask(ATaskID: Integer): TTaskAttachmentArray;
    function GetFileSizeByPath(const AFilePath: string): Int64;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // User management
    procedure SetCurrentUser(const AUserName: string);
    function GetCurrentUser: string;
    
    // Reminder management
    function AddReminder(ATaskID: Integer; AReminderType: TReminderType;
                        AReminderTime: TDateTime; AMinutesBeforeDue: Integer;
                        const AMessage: string): Integer;
    function DeleteReminder(AReminderID: Integer): Boolean;
    function GetReminders(ATaskID: Integer): TTaskReminderArray;
    function GetAllReminders: TTaskReminderArray;
    function GetActiveReminders: TTaskReminderArray;
    function CheckReminders: TTaskReminderArray; // Returns triggered reminders
    function SnoozeReminder(AReminderID: Integer; AMinutes: Integer): Boolean;
    function ReminderTypeToString(AReminderType: TReminderType): string;
    function ReminderToString(const AReminder: TTaskReminder): string;
    
    // Audit trail
    function GetAuditTrail(ATaskID: Integer): TAuditEntryArray;
    function GetAllAuditEntries: TAuditEntryArray;
    function GetAuditEntriesByDate(AStartDate, AEndDate: TDateTime): TAuditEntryArray;
    function GetAuditEntriesByUser(const AUserName: string): TAuditEntryArray;
    function AuditActionToString(AAction: TAuditAction): string;
    function AuditEntryToString(const AEntry: TAuditEntry): string;
    function GetAuditSummary: string;
    
    // Attachment management
    function AddAttachment(ATaskID: Integer; AType: TAttachmentType;
                          const AFilePath, AFileName, ADescription: string): Integer;
    function DeleteAttachment(AAttachmentID: Integer): Boolean;
    function GetAttachments(ATaskID: Integer): TTaskAttachmentArray;
    function GetAllAttachments: TTaskAttachmentArray;
    function AttachmentTypeToString(AType: TAttachmentType): string;
    function AttachmentToString(const AAttachment: TTaskAttachment): string;
    function GetTotalAttachmentSize: Int64;
    
    // Archive management
    function ArchiveTask(ATaskID: Integer; const AReason: string): Integer;
    function UnarchiveTask(AArchiveID: Integer): Integer; // Returns new task ID
    function DeleteArchivedTask(AArchiveID: Integer): Boolean;
    function GetArchivedTasks: TArchivedTaskArray;
    function GetArchivedTasksByDate(AStartDate, AEndDate: TDateTime): TArchivedTaskArray;
    function SearchArchivedTasks(const ASearchTerm: string): TArchivedTaskArray;
    function ArchivedTaskToString(const AArchived: TArchivedTask): string;
    function GetArchiveStatistics: string;
    
    // Enhanced operations with audit logging
    function AddTaskWithAudit(const ATitle, ADescription, ACategory: string;
                             APriority: TTaskPriority; ADueDate: TDateTime;
                             AEstimatedHours: Double): Integer;
    function DeleteTaskWithAudit(ATaskID: Integer; const AReason: string): Boolean;
    function UpdateTaskStatusWithAudit(ATaskID: Integer; ANewStatus: TTaskStatus;
                                      const AReason: string): Boolean;
    
    // Bulk archive operations
    function ArchiveCompletedTasks(AOlderThanDays: Integer): Integer;
    function ArchiveCancelledTasks(AOlderThanDays: Integer): Integer;
    function ArchiveOldTasks(AOlderThanDays: Integer): Integer;
    
    // Export/Import with enhanced data
    function ExportEnhancedToCSV: string;
    function SaveEnhancedToFile(const AFilename: string): Boolean;
    function LoadEnhancedFromFile(const AFilename: string): Boolean;
    
    // Statistics
    function GetReminderStatistics: string;
    function GetAttachmentStatistics: string;
    function GetMostActiveUsers: string;
  end;

implementation

{ TEnhancedTaskManager }

constructor TEnhancedTaskManager.Create;
begin
  inherited Create;
  SetLength(FReminders, 0);
  SetLength(FAuditTrail, 0);
  SetLength(FAttachments, 0);
  SetLength(FArchivedTasks, 0);
  FNextReminderID := 1;
  FNextAuditID := 1;
  FNextAttachmentID := 1;
  FNextArchiveID := 1;
  FCurrentUser := 'System';
end;

destructor TEnhancedTaskManager.Destroy;
begin
  SetLength(FReminders, 0);
  SetLength(FAuditTrail, 0);
  SetLength(FAttachments, 0);
  SetLength(FArchivedTasks, 0);
  inherited Destroy;
end;

procedure TEnhancedTaskManager.SetCurrentUser(const AUserName: string);
begin
  FCurrentUser := AUserName;
end;

function TEnhancedTaskManager.GetCurrentUser: string;
begin
  Result := FCurrentUser;
end;

function TEnhancedTaskManager.GetFileSizeByPath(const AFilePath: string): Int64;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(AFilePath, faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    FindClose(SearchRec);
  end;
end;

procedure TEnhancedTaskManager.LogAudit(ATaskID: Integer; AAction: TAuditAction;
  const AFieldChanged, AOldValue, ANewValue, ADescription: string);
var
  Entry: TAuditEntry;
begin
  Entry.AuditID := FNextAuditID;
  Inc(FNextAuditID);
  Entry.TaskID := ATaskID;
  Entry.Action := AAction;
  Entry.Timestamp := Now;
  Entry.UserName := FCurrentUser;
  Entry.FieldChanged := AFieldChanged;
  Entry.OldValue := AOldValue;
  Entry.NewValue := ANewValue;
  Entry.Description := ADescription;
  
  SetLength(FAuditTrail, Length(FAuditTrail) + 1);
  FAuditTrail[High(FAuditTrail)] := Entry;
end;

function TEnhancedTaskManager.AddReminder(ATaskID: Integer; AReminderType: TReminderType;
  AReminderTime: TDateTime; AMinutesBeforeDue: Integer; const AMessage: string): Integer;
var
  Reminder: TTaskReminder;
begin
  Reminder.ReminderID := FNextReminderID;
  Inc(FNextReminderID);
  Reminder.TaskID := ATaskID;
  Reminder.ReminderType := AReminderType;
  Reminder.ReminderTime := AReminderTime;
  Reminder.MinutesBeforeDue := AMinutesBeforeDue;
  Reminder.Message := AMessage;
  Reminder.IsActive := True;
  Reminder.LastTriggered := 0;
  Reminder.CreatedDate := Now;
  
  SetLength(FReminders, Length(FReminders) + 1);
  FReminders[High(FReminders)] := Reminder;
  
  LogAudit(ATaskID, aaUpdate, 'Reminder', '', 'Added', 
           'Reminder added: ' + AMessage);
  
  Result := Reminder.ReminderID;
end;

function TEnhancedTaskManager.DeleteReminder(AReminderID: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FReminders) do
  begin
    if FReminders[i].ReminderID = AReminderID then
    begin
      LogAudit(FReminders[i].TaskID, aaUpdate, 'Reminder', 'Active', 'Deleted',
               'Reminder deleted');
      
      if i < High(FReminders) then
        FReminders[i] := FReminders[High(FReminders)];
      SetLength(FReminders, Length(FReminders) - 1);
      Result := True;
      Break;
    end;
  end;
end;

function TEnhancedTaskManager.GetRemindersForTask(ATaskID: Integer): TTaskReminderArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FReminders) do
  begin
    if FReminders[i].TaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FReminders[i];
      Inc(Count);
    end;
  end;
end;

function TEnhancedTaskManager.GetReminders(ATaskID: Integer): TTaskReminderArray;
begin
  Result := GetRemindersForTask(ATaskID);
end;

function TEnhancedTaskManager.GetAllReminders: TTaskReminderArray;
begin
  Result := Copy(FReminders, 0, Length(FReminders));
end;

function TEnhancedTaskManager.GetActiveReminders: TTaskReminderArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FReminders) do
  begin
    if FReminders[i].IsActive then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FReminders[i];
      Inc(Count);
    end;
  end;
end;

function TEnhancedTaskManager.CheckReminders: TTaskReminderArray;
var
  i, Count: Integer;
  CurrentTime: TDateTime;
  TaskIdx: Integer;
  Tasks: TExtendedTaskArray;
  ShouldTrigger: Boolean;
begin
  SetLength(Result, 0);
  Count := 0;
  CurrentTime := Now;
  Tasks := GetAllExtendedTasks;
  
  for i := 0 to High(FReminders) do
  begin
    if not FReminders[i].IsActive then
      Continue;
      
    ShouldTrigger := False;
    
    case FReminders[i].ReminderType of
      rtBeforeDue:
        begin
          TaskIdx := GetTaskByID(FReminders[i].TaskID);
          if TaskIdx >= 0 then
          begin
            if MinutesBetween(CurrentTime, Tasks[TaskIdx].BaseTask.DueDate) <= FReminders[i].MinutesBeforeDue then
              ShouldTrigger := True;
          end;
        end;
      rtAtSpecificTime:
        begin
          if CurrentTime >= FReminders[i].ReminderTime then
            ShouldTrigger := True;
        end;
      rtRecurringDaily:
        begin
          if (DaysBetween(CurrentTime, FReminders[i].LastTriggered) >= 1) or 
             (FReminders[i].LastTriggered = 0) then
            ShouldTrigger := True;
        end;
      rtRecurringWeekly:
        begin
          if (WeeksBetween(CurrentTime, FReminders[i].LastTriggered) >= 1) or
             (FReminders[i].LastTriggered = 0) then
            ShouldTrigger := True;
        end;
    end;
    
    if ShouldTrigger then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FReminders[i];
      Inc(Count);
      
      FReminders[i].LastTriggered := CurrentTime;
      
      if FReminders[i].ReminderType = rtAtSpecificTime then
        FReminders[i].IsActive := False;
    end;
  end;
end;

function TEnhancedTaskManager.SnoozeReminder(AReminderID: Integer; AMinutes: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FReminders) do
  begin
    if FReminders[i].ReminderID = AReminderID then
    begin
      FReminders[i].ReminderTime := IncMinute(Now, AMinutes);
      FReminders[i].IsActive := True;
      Result := True;
      Break;
    end;
  end;
end;

function TEnhancedTaskManager.ReminderTypeToString(AReminderType: TReminderType): string;
begin
  case AReminderType of
    rtBeforeDue: Result := 'Before Due Date';
    rtAtSpecificTime: Result := 'At Specific Time';
    rtRecurringDaily: Result := 'Daily';
    rtRecurringWeekly: Result := 'Weekly';
  else
    Result := 'Unknown';
  end;
end;

function TEnhancedTaskManager.ReminderToString(const AReminder: TTaskReminder): string;
begin
  Result := Format('Reminder #%d: %s - %s - %s (Active: %s)',
    [AReminder.ReminderID,
     AReminder.Message,
     ReminderTypeToString(AReminder.ReminderType),
     DateTimeToStr(AReminder.ReminderTime),
     BoolToStr(AReminder.IsActive, True)]);
end;

function TEnhancedTaskManager.GetAuditTrail(ATaskID: Integer): TAuditEntryArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FAuditTrail) do
  begin
    if FAuditTrail[i].TaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FAuditTrail[i];
      Inc(Count);
    end;
  end;
end;

function TEnhancedTaskManager.GetAllAuditEntries: TAuditEntryArray;
begin
  Result := Copy(FAuditTrail, 0, Length(FAuditTrail));
end;

function TEnhancedTaskManager.GetAuditEntriesByDate(AStartDate, AEndDate: TDateTime): TAuditEntryArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FAuditTrail) do
  begin
    if (FAuditTrail[i].Timestamp >= AStartDate) and 
       (FAuditTrail[i].Timestamp <= AEndDate) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FAuditTrail[i];
      Inc(Count);
    end;
  end;
end;

function TEnhancedTaskManager.GetAuditEntriesByUser(const AUserName: string): TAuditEntryArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FAuditTrail) do
  begin
    if FAuditTrail[i].UserName = AUserName then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FAuditTrail[i];
      Inc(Count);
    end;
  end;
end;

function TEnhancedTaskManager.AuditActionToString(AAction: TAuditAction): string;
begin
  case AAction of
    aaCreate: Result := 'Created';
    aaUpdate: Result := 'Updated';
    aaDelete: Result := 'Deleted';
    aaStatusChange: Result := 'Status Changed';
    aaComplete: Result := 'Completed';
    aaArchive: Result := 'Archived';
    aaRestore: Result := 'Restored';
    aaAssign: Result := 'Assigned';
    aaPriorityChange: Result := 'Priority Changed';
  else
    Result := 'Unknown';
  end;
end;

function TEnhancedTaskManager.AuditEntryToString(const AEntry: TAuditEntry): string;
begin
  Result := Format('[%s] %s by %s: %s "%s" -> "%s" (%s)',
    [DateTimeToStr(AEntry.Timestamp),
     AuditActionToString(AEntry.Action),
     AEntry.UserName,
     AEntry.FieldChanged,
     AEntry.OldValue,
     AEntry.NewValue,
     AEntry.Description]);
end;

function TEnhancedTaskManager.GetAuditSummary: string;
var
  i: Integer;
  UserCounts: TStringList;
  ActionCounts: array[TAuditAction] of Integer;
  Action: TAuditAction;
begin
  UserCounts := TStringList.Create;
  try
    for Action := Low(TAuditAction) to High(TAuditAction) do
      ActionCounts[Action] := 0;
      
    for i := 0 to High(FAuditTrail) do
    begin
      Inc(ActionCounts[FAuditTrail[i].Action]);
      
      if UserCounts.IndexOf(FAuditTrail[i].UserName) = -1 then
        UserCounts.Add(FAuditTrail[i].UserName);
    end;
    
    Result := Format('Audit Summary:%s', [sLineBreak]);
    Result := Result + Format('Total Entries: %d%s', [Length(FAuditTrail), sLineBreak]);
    Result := Result + Format('Unique Users: %d%s', [UserCounts.Count, sLineBreak]);
    Result := Result + Format('%sAction Breakdown:%s', [sLineBreak, sLineBreak]);
    
    for Action := Low(TAuditAction) to High(TAuditAction) do
    begin
      if ActionCounts[Action] > 0 then
        Result := Result + Format('  %s: %d%s', 
          [AuditActionToString(Action), ActionCounts[Action], sLineBreak]);
    end;
  finally
    UserCounts.Free;
  end;
end;

function TEnhancedTaskManager.AddAttachment(ATaskID: Integer; AType: TAttachmentType;
  const AFilePath, AFileName, ADescription: string): Integer;
var
  Attachment: TTaskAttachment;
begin
  Attachment.AttachmentID := FNextAttachmentID;
  Inc(FNextAttachmentID);
  Attachment.TaskID := ATaskID;
  Attachment.AttachmentType := AType;
  Attachment.FilePath := AFilePath;
  Attachment.FileName := AFileName;
  Attachment.FileSize := 0;
  Attachment.MimeType := '';
  Attachment.Description := ADescription;
  Attachment.AddedDate := Now;
  Attachment.AddedBy := FCurrentUser;
  
  if (AType = atLocalFile) and FileExists(AFilePath) then
  begin
    Attachment.FileSize := GetFileSizeByPath(AFilePath);
  end;
  
  SetLength(FAttachments, Length(FAttachments) + 1);
  FAttachments[High(FAttachments)] := Attachment;
  
  LogAudit(ATaskID, aaUpdate, 'Attachment', '', 'Added', 
           'Attachment added: ' + AFileName);
  
  Result := Attachment.AttachmentID;
end;

function TEnhancedTaskManager.DeleteAttachment(AAttachmentID: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FAttachments) do
  begin
    if FAttachments[i].AttachmentID = AAttachmentID then
    begin
      LogAudit(FAttachments[i].TaskID, aaUpdate, 'Attachment', 
               FAttachments[i].FileName, 'Deleted', 'Attachment removed');
               
      if i < High(FAttachments) then
        FAttachments[i] := FAttachments[High(FAttachments)];
      SetLength(FAttachments, Length(FAttachments) - 1);
      Result := True;
      Break;
    end;
  end;
end;

function TEnhancedTaskManager.GetAttachmentsForTask(ATaskID: Integer): TTaskAttachmentArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FAttachments) do
  begin
    if FAttachments[i].TaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FAttachments[i];
      Inc(Count);
    end;
  end;
end;

function TEnhancedTaskManager.GetAttachments(ATaskID: Integer): TTaskAttachmentArray;
begin
  Result := GetAttachmentsForTask(ATaskID);
end;

function TEnhancedTaskManager.GetAllAttachments: TTaskAttachmentArray;
begin
  Result := Copy(FAttachments, 0, Length(FAttachments));
end;

function TEnhancedTaskManager.AttachmentTypeToString(AType: TAttachmentType): string;
begin
  case AType of
    atLocalFile: Result := 'Local File';
    atURL: Result := 'URL';
    atNetworkPath: Result := 'Network Path';
    atCloudStorage: Result := 'Cloud Storage';
  else
    Result := 'Unknown';
  end;
end;

function TEnhancedTaskManager.AttachmentToString(const AAttachment: TTaskAttachment): string;
begin
  Result := Format('Attachment #%d: %s (%s) - %s - Added by %s on %s',
    [AAttachment.AttachmentID,
     AAttachment.FileName,
     AttachmentTypeToString(AAttachment.AttachmentType),
     AAttachment.Description,
     AAttachment.AddedBy,
     DateTimeToStr(AAttachment.AddedDate)]);
end;

function TEnhancedTaskManager.GetTotalAttachmentSize: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FAttachments) do
    Result := Result + FAttachments[i].FileSize;
end;

function TEnhancedTaskManager.ArchiveTask(ATaskID: Integer; const AReason: string): Integer;
var
  TaskIdx: Integer;
  Tasks: TExtendedTaskArray;
  Archived: TArchivedTask;
begin
  Result := -1;
  TaskIdx := GetTaskByID(ATaskID);
  if TaskIdx < 0 then
    Exit;
    
  Tasks := GetAllExtendedTasks;
  
  Archived.ArchiveID := FNextArchiveID;
  Inc(FNextArchiveID);
  Archived.OriginalTask := Tasks[TaskIdx];
  Archived.ArchivedDate := Now;
  Archived.ArchivedBy := FCurrentUser;
  Archived.ArchiveReason := AReason;
  Archived.OriginalTaskID := ATaskID;
  
  SetLength(FArchivedTasks, Length(FArchivedTasks) + 1);
  FArchivedTasks[High(FArchivedTasks)] := Archived;
  
  LogAudit(ATaskID, aaArchive, 'Status', 'Active', 'Archived', AReason);
  
  DeleteTask(ATaskID);
  
  Result := Archived.ArchiveID;
end;

function TEnhancedTaskManager.UnarchiveTask(AArchiveID: Integer): Integer;
var
  i: Integer;
  NewTaskID: Integer;
begin
  Result := -1;
  
  for i := 0 to High(FArchivedTasks) do
  begin
    if FArchivedTasks[i].ArchiveID = AArchiveID then
    begin
      NewTaskID := AddExtendedTask(
        FArchivedTasks[i].OriginalTask.BaseTask.Title,
        FArchivedTasks[i].OriginalTask.BaseTask.Description,
        FArchivedTasks[i].OriginalTask.BaseTask.Category,
        FArchivedTasks[i].OriginalTask.BaseTask.Priority,
        FArchivedTasks[i].OriginalTask.BaseTask.DueDate,
        FArchivedTasks[i].OriginalTask.BaseTask.EstimatedHours,
        FArchivedTasks[i].OriginalTask.RecurrencePattern
      );
      
      LogAudit(NewTaskID, aaRestore, 'Status', 'Archived', 'Active',
               'Restored from archive #' + IntToStr(AArchiveID));
      
      if i < High(FArchivedTasks) then
        FArchivedTasks[i] := FArchivedTasks[High(FArchivedTasks)];
      SetLength(FArchivedTasks, Length(FArchivedTasks) - 1);
      
      Result := NewTaskID;
      Break;
    end;
  end;
end;

function TEnhancedTaskManager.DeleteArchivedTask(AArchiveID: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FArchivedTasks) do
  begin
    if FArchivedTasks[i].ArchiveID = AArchiveID then
    begin
      if i < High(FArchivedTasks) then
        FArchivedTasks[i] := FArchivedTasks[High(FArchivedTasks)];
      SetLength(FArchivedTasks, Length(FArchivedTasks) - 1);
      Result := True;
      Break;
    end;
  end;
end;

function TEnhancedTaskManager.GetArchivedTasks: TArchivedTaskArray;
begin
  Result := Copy(FArchivedTasks, 0, Length(FArchivedTasks));
end;

function TEnhancedTaskManager.GetArchivedTasksByDate(AStartDate, AEndDate: TDateTime): TArchivedTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to High(FArchivedTasks) do
  begin
    if (FArchivedTasks[i].ArchivedDate >= AStartDate) and
       (FArchivedTasks[i].ArchivedDate <= AEndDate) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FArchivedTasks[i];
      Inc(Count);
    end;
  end;
end;

function TEnhancedTaskManager.SearchArchivedTasks(const ASearchTerm: string): TArchivedTaskArray;
var
  i, Count: Integer;
  SearchLower: string;
begin
  SetLength(Result, 0);
  Count := 0;
  SearchLower := LowerCase(ASearchTerm);
  
  for i := 0 to High(FArchivedTasks) do
  begin
    if (Pos(SearchLower, LowerCase(FArchivedTasks[i].OriginalTask.BaseTask.Title)) > 0) or
       (Pos(SearchLower, LowerCase(FArchivedTasks[i].OriginalTask.BaseTask.Description)) > 0) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FArchivedTasks[i];
      Inc(Count);
    end;
  end;
end;

function TEnhancedTaskManager.ArchivedTaskToString(const AArchived: TArchivedTask): string;
begin
  Result := Format('Archive #%d: %s - Archived on %s by %s - Reason: %s',
    [AArchived.ArchiveID,
     AArchived.OriginalTask.BaseTask.Title,
     DateTimeToStr(AArchived.ArchivedDate),
     AArchived.ArchivedBy,
     AArchived.ArchiveReason]);
end;

function TEnhancedTaskManager.GetArchiveStatistics: string;
var
  i: Integer;
  CompletedCount, CancelledCount, OtherCount: Integer;
begin
  CompletedCount := 0;
  CancelledCount := 0;
  OtherCount := 0;
  
  for i := 0 to High(FArchivedTasks) do
  begin
    case FArchivedTasks[i].OriginalTask.BaseTask.Status of
      tsCompleted: Inc(CompletedCount);
      tsCancelled: Inc(CancelledCount);
    else
      Inc(OtherCount);
    end;
  end;
  
  Result := Format('Archive Statistics:%s', [sLineBreak]);
  Result := Result + Format('Total Archived: %d%s', [Length(FArchivedTasks), sLineBreak]);
  Result := Result + Format('Completed: %d%s', [CompletedCount, sLineBreak]);
  Result := Result + Format('Cancelled: %d%s', [CancelledCount, sLineBreak]);
  Result := Result + Format('Other: %d', [OtherCount]);
end;

function TEnhancedTaskManager.AddTaskWithAudit(const ATitle, ADescription, ACategory: string;
  APriority: TTaskPriority; ADueDate: TDateTime; AEstimatedHours: Double): Integer;
begin
  Result := AddExtendedTask(ATitle, ADescription, ACategory, APriority, ADueDate,
                           AEstimatedHours, rpNone);
  LogAudit(Result, aaCreate, 'Task', '', ATitle, 'Task created');
end;

function TEnhancedTaskManager.DeleteTaskWithAudit(ATaskID: Integer; const AReason: string): Boolean;
var
  TaskIdx: Integer;
  Tasks: TTaskArray;
begin
  TaskIdx := GetTaskByID(ATaskID);
  if TaskIdx >= 0 then
  begin
    Tasks := GetAllTasks;
    LogAudit(ATaskID, aaDelete, 'Task', Tasks[TaskIdx].Title, '', AReason);
  end;
  Result := DeleteTask(ATaskID);
end;

function TEnhancedTaskManager.UpdateTaskStatusWithAudit(ATaskID: Integer; 
  ANewStatus: TTaskStatus; const AReason: string): Boolean;
var
  TaskIdx: Integer;
  Tasks: TTaskArray;
  OldStatus: TTaskStatus;
begin
  TaskIdx := GetTaskByID(ATaskID);
  if TaskIdx >= 0 then
  begin
    Tasks := GetAllTasks;
    OldStatus := Tasks[TaskIdx].Status;
    Result := UpdateTaskStatus(ATaskID, ANewStatus);
    if Result then
    begin
      LogAudit(ATaskID, aaStatusChange, 'Status', 
               TaskStatusToString(OldStatus), 
               TaskStatusToString(ANewStatus), AReason);
    end;
  end
  else
    Result := False;
end;

function TEnhancedTaskManager.ArchiveCompletedTasks(AOlderThanDays: Integer): Integer;
var
  i: Integer;
  Tasks: TExtendedTaskArray;
  CutoffDate: TDateTime;
begin
  Result := 0;
  Tasks := GetAllExtendedTasks;
  CutoffDate := IncDay(Now, -AOlderThanDays);
  
  for i := High(Tasks) downto 0 do
  begin
    if (Tasks[i].BaseTask.Status = tsCompleted) and 
       (Tasks[i].BaseTask.CompletedDate < CutoffDate) and
       (Tasks[i].BaseTask.CompletedDate > 0) then
    begin
      if ArchiveTask(Tasks[i].BaseTask.ID, 'Auto-archived: Completed > ' + 
                     IntToStr(AOlderThanDays) + ' days ago') >= 0 then
        Inc(Result);
    end;
  end;
end;

function TEnhancedTaskManager.ArchiveCancelledTasks(AOlderThanDays: Integer): Integer;
var
  i: Integer;
  Tasks: TExtendedTaskArray;
  CutoffDate: TDateTime;
begin
  Result := 0;
  Tasks := GetAllExtendedTasks;
  CutoffDate := IncDay(Now, -AOlderThanDays);
  
  for i := High(Tasks) downto 0 do
  begin
    if (Tasks[i].BaseTask.Status = tsCancelled) and 
       (Tasks[i].BaseTask.CreatedDate < CutoffDate) then
    begin
      if ArchiveTask(Tasks[i].BaseTask.ID, 'Auto-archived: Cancelled > ' + 
                     IntToStr(AOlderThanDays) + ' days ago') >= 0 then
        Inc(Result);
    end;
  end;
end;

function TEnhancedTaskManager.ArchiveOldTasks(AOlderThanDays: Integer): Integer;
begin
  Result := ArchiveCompletedTasks(AOlderThanDays);
  Result := Result + ArchiveCancelledTasks(AOlderThanDays);
end;

function TEnhancedTaskManager.ExportEnhancedToCSV: string;
begin
  Result := ExportExtendedToCSV;
end;

function TEnhancedTaskManager.SaveEnhancedToFile(const AFilename: string): Boolean;
begin
  Result := SaveExtendedToFile(AFilename);
end;

function TEnhancedTaskManager.LoadEnhancedFromFile(const AFilename: string): Boolean;
begin
  Result := LoadExtendedFromFile(AFilename);
end;

function TEnhancedTaskManager.GetReminderStatistics: string;
var
  ActiveCount, InactiveCount: Integer;
  i: Integer;
begin
  ActiveCount := 0;
  InactiveCount := 0;
  
  for i := 0 to High(FReminders) do
  begin
    if FReminders[i].IsActive then
      Inc(ActiveCount)
    else
      Inc(InactiveCount);
  end;
  
  Result := Format('Reminder Statistics:%s', [sLineBreak]);
  Result := Result + Format('Total: %d%s', [Length(FReminders), sLineBreak]);
  Result := Result + Format('Active: %d%s', [ActiveCount, sLineBreak]);
  Result := Result + Format('Inactive: %d', [InactiveCount]);
end;

function TEnhancedTaskManager.GetAttachmentStatistics: string;
var
  TotalSize: Int64;
begin
  TotalSize := GetTotalAttachmentSize;
  
  Result := Format('Attachment Statistics:%s', [sLineBreak]);
  Result := Result + Format('Total Attachments: %d%s', [Length(FAttachments), sLineBreak]);
  Result := Result + Format('Total Size: %d bytes (%.2f MB)', 
    [TotalSize, TotalSize / (1024 * 1024)]);
end;

function TEnhancedTaskManager.GetMostActiveUsers: string;
var
  UserCounts: TStringList;
  i, j: Integer;
  MaxCount, Count: Integer;
  MostActiveUser: string;
begin
  UserCounts := TStringList.Create;
  try
    for i := 0 to High(FAuditTrail) do
    begin
      j := UserCounts.IndexOf(FAuditTrail[i].UserName);
      if j = -1 then
        UserCounts.AddObject(FAuditTrail[i].UserName, TObject(PtrInt(1)))
      else
        UserCounts.Objects[j] := TObject(PtrInt(Integer(PtrInt(UserCounts.Objects[j])) + 1));
    end;
    
    MaxCount := 0;
    MostActiveUser := '';
    
    for i := 0 to UserCounts.Count - 1 do
    begin
      Count := Integer(PtrInt(UserCounts.Objects[i]));
      if Count > MaxCount then
      begin
        MaxCount := Count;
        MostActiveUser := UserCounts[i];
      end;
    end;
    
    Result := Format('Most Active Users:%s', [sLineBreak]);
    for i := 0 to UserCounts.Count - 1 do
    begin
      Result := Result + Format('%s: %d actions%s',
        [UserCounts[i], Integer(PtrInt(UserCounts.Objects[i])), sLineBreak]);
    end;
    
    if MostActiveUser <> '' then
      Result := Result + Format('%sMost Active: %s (%d actions)',
        [sLineBreak, MostActiveUser, MaxCount]);
  finally
    UserCounts.Free;
  end;
end;

end.
