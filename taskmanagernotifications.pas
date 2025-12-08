
unit taskmanagernotifications;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Classes, taskmanager, taskmanagerenhanced;

type
  // Notification types
  TNotificationType = (ntEmail, ntSMS, ntPush, ntInApp, ntDesktop, ntSlack, ntWebhook);
  
  TNotificationPriority = (npLow, npNormal, npHigh, npUrgent);
  
  TNotificationStatus = (nsScheduled, nsPending, nsSent, nsFailed, nsRead, nsDismissed);
  
  TDeliveryChannel = (dcEmail, dcSMS, dcPush, dcInApp, dcAll);
  
  TDigestFrequency = (dfNone, dfHourly, dfDaily, dfWeekly);
  
  // Notification record
  TNotification = record
    ID: Integer;
    NotificationType: TNotificationType;
    Priority: TNotificationPriority;
    Status: TNotificationStatus;
    TaskID: Integer;
    RecipientID: Integer;
    Title: string;
    Message: string;
    ScheduledTime: TDateTime;
    SentTime: TDateTime;
    ReadTime: TDateTime;
    Channel: TDeliveryChannel;
    RetryCount: Integer;
    MaxRetries: Integer;
    Metadata: string;
    CreatedAt: TDateTime;
  end;
  
  TNotificationArray = array of TNotification;
  
  // Notification template
  TNotificationTemplate = record
    ID: Integer;
    Name: string;
    Description: string;
    NotificationType: TNotificationType;
    TitleTemplate: string;
    MessageTemplate: string;
    Priority: TNotificationPriority;
    IsActive: Boolean;
    CreatedAt: TDateTime;
  end;
  
  TNotificationTemplateArray = array of TNotificationTemplate;
  
  // User notification preferences
  TNotificationPreference = record
    ID: Integer;
    UserID: Integer;
    NotificationType: TNotificationType;
    IsEnabled: Boolean;
    QuietHoursStart: Integer; // Hour 0-23
    QuietHoursEnd: Integer;   // Hour 0-23
    DigestFrequency: TDigestFrequency;
    PreferredChannel: TDeliveryChannel;
    AllowWeekends: Boolean;
    AllowNighttime: Boolean;
    MinPriority: TNotificationPriority;
  end;
  
  TNotificationPreferenceArray = array of TNotificationPreference;
  
  // Escalation rule
  TEscalationRule = record
    ID: Integer;
    Name: string;
    Description: string;
    TaskPriority: TTaskPriority;
    InitialDelay: Integer; // Minutes
    EscalationInterval: Integer; // Minutes
    MaxEscalations: Integer;
    EscalateChannel: TDeliveryChannel;
    IsActive: Boolean;
  end;
  
  TEscalationRuleArray = array of TEscalationRule;
  
  // Notification statistics
  TNotificationStats = record
    TotalSent: Integer;
    TotalFailed: Integer;
    TotalRead: Integer;
    TotalDismissed: Integer;
    AverageReadTime: Double; // Minutes
    ByType: array[TNotificationType] of Integer;
    ByPriority: array[TNotificationPriority] of Integer;
  end;
  
  // Digest notification
  TDigestNotification = record
    ID: Integer;
    RecipientID: Integer;
    Frequency: TDigestFrequency;
    LastSent: TDateTime;
    NextScheduled: TDateTime;
    IncludedNotifications: array of Integer;
    IsActive: Boolean;
  end;
  
  TDigestNotificationArray = array of TDigestNotification;

  { TNotificationTaskManager }
  TNotificationTaskManager = class(TEnhancedTaskManager)
  private
    FNotifications: TNotificationArray;
    FTemplates: TNotificationTemplateArray;
    FPreferences: TNotificationPreferenceArray;
    FEscalationRules: TEscalationRuleArray;
    FDigests: TDigestNotificationArray;
    FNextNotificationID: Integer;
    FNextTemplateID: Integer;
    FNextPreferenceID: Integer;
    FNextEscalationID: Integer;
    FNextDigestID: Integer;
    FNotificationsEnabled: Boolean;
    FDefaultRetries: Integer;
    
    function FindNotificationIndex(AID: Integer): Integer;
    function FindTemplateIndex(AID: Integer): Integer;
    function FindPreferenceIndex(AUserID: Integer; AType: TNotificationType): Integer;
    function FindEscalationRuleIndex(AID: Integer): Integer;
    function FindDigestIndex(AID: Integer): Integer;
    function SubstituteTemplateVariables(const ATemplate: string; ATaskID: Integer): string;
    function IsWithinQuietHours(const APrefs: TNotificationPreference): Boolean;
    function ShouldSendNotification(const ANotif: TNotification; const APrefs: TNotificationPreference): Boolean;
    procedure ProcessEscalations;
    procedure ProcessDigests;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Core notification functions
    function CreateNotification(AType: TNotificationType; APriority: TNotificationPriority;
      ATaskID, ARecipientID: Integer; const ATitle, AMessage: string;
      AScheduledTime: TDateTime): Integer;
    function SendNotification(ANotificationID: Integer): Boolean;
    function ScheduleNotification(ANotificationID: Integer; AScheduledTime: TDateTime): Boolean;
    function CancelNotification(ANotificationID: Integer): Boolean;
    function MarkAsRead(ANotificationID: Integer): Boolean;
    function DismissNotification(ANotificationID: Integer): Boolean;
    function RetryFailedNotification(ANotificationID: Integer): Boolean;
    
    // Template management
    function CreateTemplate(const AName, ADescription: string; 
      AType: TNotificationType; const ATitleTemplate, AMessageTemplate: string;
      APriority: TNotificationPriority): Integer;
    function UpdateTemplate(ATemplateID: Integer; const ATitleTemplate, 
      AMessageTemplate: string): Boolean;
    function DeleteTemplate(ATemplateID: Integer): Boolean;
    function ActivateTemplate(ATemplateID: Integer): Boolean;
    function DeactivateTemplate(ATemplateID: Integer): Boolean;
    function GetAllTemplates: TNotificationTemplateArray;
    function CreateNotificationFromTemplate(ATemplateID, ATaskID, ARecipientID: Integer;
      AScheduledTime: TDateTime): Integer;
    
    // Preference management
    function SetUserPreference(AUserID: Integer; AType: TNotificationType;
      AEnabled: Boolean; AQuietStart, AQuietEnd: Integer;
      ADigestFreq: TDigestFrequency): Integer;
    function GetUserPreference(AUserID: Integer; AType: TNotificationType): TNotificationPreference;
    function UpdateQuietHours(AUserID: Integer; AType: TNotificationType;
      AStartHour, AEndHour: Integer): Boolean;
    function EnableAllNotifications(AUserID: Integer): Boolean;
    function DisableAllNotifications(AUserID: Integer): Boolean;
    function SetDigestFrequency(AUserID: Integer; AFrequency: TDigestFrequency): Boolean;
    
    // Escalation management
    function CreateEscalationRule(const AName, ADescription: string;
      ATaskPriority: TTaskPriority; AInitialDelay, AInterval, AMaxEscalations: Integer;
      AChannel: TDeliveryChannel): Integer;
    function DeleteEscalationRule(ARuleID: Integer): Boolean;
    function ActivateEscalationRule(ARuleID: Integer): Boolean;
    function DeactivateEscalationRule(ARuleID: Integer): Boolean;
    function GetAllEscalationRules: TEscalationRuleArray;
    function TriggerEscalation(ATaskID: Integer): Boolean;
    
    // Digest notifications
    function CreateDigest(ARecipientID: Integer; AFrequency: TDigestFrequency): Integer;
    function UpdateDigestSchedule(ADigestID: Integer; AFrequency: TDigestFrequency): Boolean;
    function SendDigest(ADigestID: Integer): Boolean;
    function GetPendingDigests: TDigestNotificationArray;
    
    // Query functions
    function GetNotificationsByTask(ATaskID: Integer): TNotificationArray;
    function GetNotificationsByRecipient(ARecipientID: Integer): TNotificationArray;
    function GetUnreadNotifications(ARecipientID: Integer): TNotificationArray;
    function GetFailedNotifications: TNotificationArray;
    function GetScheduledNotifications: TNotificationArray;
    function GetNotificationsByPriority(APriority: TNotificationPriority): TNotificationArray;
    function GetNotificationsByStatus(AStatus: TNotificationStatus): TNotificationArray;
    function GetRecentNotifications(ARecipientID: Integer; AHours: Integer): TNotificationArray;
    
    // Statistics and reporting
    function GetNotificationStatistics: TNotificationStats;
    function GetUserNotificationStats(ARecipientID: Integer): TNotificationStats;
    function GetDeliverySuccessRate: Double;
    function GetAverageReadTime: Double;
    function GenerateNotificationReport: string;
    
    // Bulk operations
    function SendBulkNotification(ARecipientIDs: array of Integer; 
      const ATitle, AMessage: string; APriority: TNotificationPriority): Integer;
    function DeleteOldNotifications(ADaysOld: Integer): Integer;
    function RetryAllFailed: Integer;
    
    // Utility functions
    function NotificationTypeToString(AType: TNotificationType): string;
    function NotificationPriorityToString(APriority: TNotificationPriority): string;
    function NotificationStatusToString(AStatus: TNotificationStatus): string;
    function DigestFrequencyToString(AFreq: TDigestFrequency): string;
    
    // Processing
    procedure ProcessScheduledNotifications;
    procedure ProcessAllPending;
    
    // Configuration
    procedure EnableNotifications(AEnabled: Boolean);
    procedure SetDefaultRetries(ARetries: Integer);
    function GetNotificationsEnabled: Boolean;
    
    // Persistence
    function SaveNotificationDataToFile(const AFilename: string): Boolean;
    function LoadNotificationDataFromFile(const AFilename: string): Boolean;
    
    // Testing
    procedure SelfTest;
  end;

implementation

{ TNotificationTaskManager }

constructor TNotificationTaskManager.Create;
begin
  inherited Create;
  SetLength(FNotifications, 0);
  SetLength(FTemplates, 0);
  SetLength(FPreferences, 0);
  SetLength(FEscalationRules, 0);
  SetLength(FDigests, 0);
  FNextNotificationID := 1;
  FNextTemplateID := 1;
  FNextPreferenceID := 1;
  FNextEscalationID := 1;
  FNextDigestID := 1;
  FNotificationsEnabled := True;
  FDefaultRetries := 3;
end;

destructor TNotificationTaskManager.Destroy;
begin
  SetLength(FNotifications, 0);
  SetLength(FTemplates, 0);
  SetLength(FPreferences, 0);
  SetLength(FEscalationRules, 0);
  SetLength(FDigests, 0);
  inherited Destroy;
end;

function TNotificationTaskManager.FindNotificationIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FNotifications) do
    if FNotifications[i].ID = AID then
    begin
      Result := i;
      Exit;
    end;
end;

function TNotificationTaskManager.FindTemplateIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTemplates) do
    if FTemplates[i].ID = AID then
    begin
      Result := i;
      Exit;
    end;
end;

function TNotificationTaskManager.FindPreferenceIndex(AUserID: Integer; 
  AType: TNotificationType): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FPreferences) do
    if (FPreferences[i].UserID = AUserID) and 
       (FPreferences[i].NotificationType = AType) then
    begin
      Result := i;
      Exit;
    end;
end;

function TNotificationTaskManager.FindEscalationRuleIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FEscalationRules) do
    if FEscalationRules[i].ID = AID then
    begin
      Result := i;
      Exit;
    end;
end;

function TNotificationTaskManager.FindDigestIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FDigests) do
    if FDigests[i].ID = AID then
    begin
      Result := i;
      Exit;
    end;
end;

function TNotificationTaskManager.SubstituteTemplateVariables(const ATemplate: string; 
  ATaskID: Integer): string;
var
  task: TTask;
  taskIdx: Integer;
  allTasks: TTaskArray;
  i: Integer;
  found: Boolean;
begin
  Result := ATemplate;
  taskIdx := GetTaskByID(ATaskID);
  if taskIdx >= 0 then
  begin
    allTasks := GetAllTasks;
    if (taskIdx >= 0) and (taskIdx < Length(allTasks)) then
    begin
      task := allTasks[taskIdx];
      Result := StringReplace(Result, '{TASK_TITLE}', task.Title, [rfReplaceAll, rfIgnoreCase]);
      Result := StringReplace(Result, '{TASK_DESCRIPTION}', task.Description, [rfReplaceAll, rfIgnoreCase]);
      Result := StringReplace(Result, '{TASK_PRIORITY}', TaskPriorityToString(task.Priority), [rfReplaceAll, rfIgnoreCase]);
      Result := StringReplace(Result, '{TASK_STATUS}', TaskStatusToString(task.Status), [rfReplaceAll, rfIgnoreCase]);
      Result := StringReplace(Result, '{TASK_CATEGORY}', task.Category, [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
end;

function TNotificationTaskManager.IsWithinQuietHours(const APrefs: TNotificationPreference): Boolean;
var
  currentHour: Integer;
begin
  currentHour := HourOf(Now);
  
  if APrefs.QuietHoursStart < APrefs.QuietHoursEnd then
    Result := (currentHour >= APrefs.QuietHoursStart) and (currentHour < APrefs.QuietHoursEnd)
  else if APrefs.QuietHoursStart > APrefs.QuietHoursEnd then
    Result := (currentHour >= APrefs.QuietHoursStart) or (currentHour < APrefs.QuietHoursEnd)
  else
    Result := False;
end;

function TNotificationTaskManager.ShouldSendNotification(const ANotif: TNotification; 
  const APrefs: TNotificationPreference): Boolean;
var
  isWeekend: Boolean;
  currentHour: Integer;
begin
  Result := False;
  
  if not APrefs.IsEnabled then
    Exit;
    
  if ANotif.Priority < APrefs.MinPriority then
    Exit;
    
  isWeekend := DayOfWeek(Now) in [1, 7]; // Sunday = 1, Saturday = 7
  if isWeekend and not APrefs.AllowWeekends then
    Exit;
    
  currentHour := HourOf(Now);
  if ((currentHour < 6) or (currentHour >= 22)) and not APrefs.AllowNighttime then
    Exit;
    
  if IsWithinQuietHours(APrefs) then
    Exit;
    
  Result := True;
end;

procedure TNotificationTaskManager.ProcessEscalations;
// This would check tasks and create escalation notifications
begin
  // Implementation for escalation processing
end;

procedure TNotificationTaskManager.ProcessDigests;
// This would generate and send digest notifications
begin
  // Implementation for digest processing
end;

function TNotificationTaskManager.CreateNotification(AType: TNotificationType; 
  APriority: TNotificationPriority; ATaskID, ARecipientID: Integer; 
  const ATitle, AMessage: string; AScheduledTime: TDateTime): Integer;
var
  notif: TNotification;
begin
  notif.ID := FNextNotificationID;
  Inc(FNextNotificationID);
  notif.NotificationType := AType;
  notif.Priority := APriority;
  notif.Status := nsScheduled;
  notif.TaskID := ATaskID;
  notif.RecipientID := ARecipientID;
  notif.Title := ATitle;
  notif.Message := AMessage;
  notif.ScheduledTime := AScheduledTime;
  notif.SentTime := 0;
  notif.ReadTime := 0;
  notif.Channel := dcAll;
  notif.RetryCount := 0;
  notif.MaxRetries := FDefaultRetries;
  notif.Metadata := '';
  notif.CreatedAt := Now;
  
  SetLength(FNotifications, Length(FNotifications) + 1);
  FNotifications[High(FNotifications)] := notif;
  
  Result := notif.ID;
end;

function TNotificationTaskManager.SendNotification(ANotificationID: Integer): Boolean;
var
  idx: Integer;
  prefs: TNotificationPreference;
  prefsIdx: Integer;
begin
  Result := False;
  if not FNotificationsEnabled then
    Exit;
    
  idx := FindNotificationIndex(ANotificationID);
  if idx < 0 then
    Exit;
    
  // Check user preferences
  prefsIdx := FindPreferenceIndex(FNotifications[idx].RecipientID, 
                                   FNotifications[idx].NotificationType);
  if prefsIdx >= 0 then
  begin
    prefs := FPreferences[prefsIdx];
    if not ShouldSendNotification(FNotifications[idx], prefs) then
    begin
      FNotifications[idx].Status := nsScheduled; // Reschedule
      Exit;
    end;
  end;
  
  // Simulate sending
  FNotifications[idx].Status := nsSent;
  FNotifications[idx].SentTime := Now;
  Result := True;
end;

function TNotificationTaskManager.ScheduleNotification(ANotificationID: Integer; 
  AScheduledTime: TDateTime): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindNotificationIndex(ANotificationID);
  if idx < 0 then
    Exit;
    
  FNotifications[idx].ScheduledTime := AScheduledTime;
  FNotifications[idx].Status := nsScheduled;
  Result := True;
end;

function TNotificationTaskManager.CancelNotification(ANotificationID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindNotificationIndex(ANotificationID);
  if idx < 0 then
    Exit;
    
  FNotifications[idx].Status := nsDismissed;
  Result := True;
end;

function TNotificationTaskManager.MarkAsRead(ANotificationID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindNotificationIndex(ANotificationID);
  if idx < 0 then
    Exit;
    
  if FNotifications[idx].Status = nsSent then
  begin
    FNotifications[idx].Status := nsRead;
    FNotifications[idx].ReadTime := Now;
    Result := True;
  end;
end;

function TNotificationTaskManager.DismissNotification(ANotificationID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindNotificationIndex(ANotificationID);
  if idx < 0 then
    Exit;
    
  FNotifications[idx].Status := nsDismissed;
  Result := True;
end;

function TNotificationTaskManager.RetryFailedNotification(ANotificationID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindNotificationIndex(ANotificationID);
  if idx < 0 then
    Exit;
    
  if FNotifications[idx].Status = nsFailed then
  begin
    if FNotifications[idx].RetryCount < FNotifications[idx].MaxRetries then
    begin
      Inc(FNotifications[idx].RetryCount);
      Result := SendNotification(ANotificationID);
    end;
  end;
end;

function TNotificationTaskManager.CreateTemplate(const AName, ADescription: string; 
  AType: TNotificationType; const ATitleTemplate, AMessageTemplate: string;
  APriority: TNotificationPriority): Integer;
var
  template: TNotificationTemplate;
begin
  template.ID := FNextTemplateID;
  Inc(FNextTemplateID);
  template.Name := AName;
  template.Description := ADescription;
  template.NotificationType := AType;
  template.TitleTemplate := ATitleTemplate;
  template.MessageTemplate := AMessageTemplate;
  template.Priority := APriority;
  template.IsActive := True;
  template.CreatedAt := Now;
  
  SetLength(FTemplates, Length(FTemplates) + 1);
  FTemplates[High(FTemplates)] := template;
  
  Result := template.ID;
end;

function TNotificationTaskManager.UpdateTemplate(ATemplateID: Integer; 
  const ATitleTemplate, AMessageTemplate: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindTemplateIndex(ATemplateID);
  if idx < 0 then
    Exit;
    
  FTemplates[idx].TitleTemplate := ATitleTemplate;
  FTemplates[idx].MessageTemplate := AMessageTemplate;
  Result := True;
end;

function TNotificationTaskManager.DeleteTemplate(ATemplateID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindTemplateIndex(ATemplateID);
  if idx < 0 then
    Exit;
    
  for i := idx to High(FTemplates) - 1 do
    FTemplates[i] := FTemplates[i + 1];
  SetLength(FTemplates, Length(FTemplates) - 1);
  Result := True;
end;

function TNotificationTaskManager.ActivateTemplate(ATemplateID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindTemplateIndex(ATemplateID);
  if idx < 0 then
    Exit;
    
  FTemplates[idx].IsActive := True;
  Result := True;
end;

function TNotificationTaskManager.DeactivateTemplate(ATemplateID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindTemplateIndex(ATemplateID);
  if idx < 0 then
    Exit;
    
  FTemplates[idx].IsActive := False;
  Result := True;
end;

function TNotificationTaskManager.GetAllTemplates: TNotificationTemplateArray;
begin
  Result := Copy(FTemplates, 0, Length(FTemplates));
end;

function TNotificationTaskManager.CreateNotificationFromTemplate(ATemplateID, ATaskID, 
  ARecipientID: Integer; AScheduledTime: TDateTime): Integer;
var
  idx: Integer;
  title, message: string;
begin
  Result := -1;
  idx := FindTemplateIndex(ATemplateID);
  if idx < 0 then
    Exit;
    
  if not FTemplates[idx].IsActive then
    Exit;
    
  title := SubstituteTemplateVariables(FTemplates[idx].TitleTemplate, ATaskID);
  message := SubstituteTemplateVariables(FTemplates[idx].MessageTemplate, ATaskID);
  
  Result := CreateNotification(FTemplates[idx].NotificationType,
                                FTemplates[idx].Priority,
                                ATaskID, ARecipientID,
                                title, message, AScheduledTime);
end;

function TNotificationTaskManager.SetUserPreference(AUserID: Integer; 
  AType: TNotificationType; AEnabled: Boolean; AQuietStart, AQuietEnd: Integer;
  ADigestFreq: TDigestFrequency): Integer;
var
  pref: TNotificationPreference;
  idx: Integer;
begin
  idx := FindPreferenceIndex(AUserID, AType);
  
  if idx >= 0 then
  begin
    FPreferences[idx].IsEnabled := AEnabled;
    FPreferences[idx].QuietHoursStart := AQuietStart;
    FPreferences[idx].QuietHoursEnd := AQuietEnd;
    FPreferences[idx].DigestFrequency := ADigestFreq;
    Result := FPreferences[idx].ID;
  end
  else
  begin
    pref.ID := FNextPreferenceID;
    Inc(FNextPreferenceID);
    pref.UserID := AUserID;
    pref.NotificationType := AType;
    pref.IsEnabled := AEnabled;
    pref.QuietHoursStart := AQuietStart;
    pref.QuietHoursEnd := AQuietEnd;
    pref.DigestFrequency := ADigestFreq;
    pref.PreferredChannel := dcAll;
    pref.AllowWeekends := True;
    pref.AllowNighttime := False;
    pref.MinPriority := npLow;
    
    SetLength(FPreferences, Length(FPreferences) + 1);
    FPreferences[High(FPreferences)] := pref;
    Result := pref.ID;
  end;
end;

function TNotificationTaskManager.GetUserPreference(AUserID: Integer; 
  AType: TNotificationType): TNotificationPreference;
var
  idx: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  idx := FindPreferenceIndex(AUserID, AType);
  if idx >= 0 then
    Result := FPreferences[idx];
end;

function TNotificationTaskManager.UpdateQuietHours(AUserID: Integer; 
  AType: TNotificationType; AStartHour, AEndHour: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindPreferenceIndex(AUserID, AType);
  if idx < 0 then
    Exit;
    
  FPreferences[idx].QuietHoursStart := AStartHour;
  FPreferences[idx].QuietHoursEnd := AEndHour;
  Result := True;
end;

function TNotificationTaskManager.EnableAllNotifications(AUserID: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FPreferences) do
    if FPreferences[i].UserID = AUserID then
    begin
      FPreferences[i].IsEnabled := True;
      Result := True;
    end;
end;

function TNotificationTaskManager.DisableAllNotifications(AUserID: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FPreferences) do
    if FPreferences[i].UserID = AUserID then
    begin
      FPreferences[i].IsEnabled := False;
      Result := True;
    end;
end;

function TNotificationTaskManager.SetDigestFrequency(AUserID: Integer; 
  AFrequency: TDigestFrequency): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FPreferences) do
    if FPreferences[i].UserID = AUserID then
    begin
      FPreferences[i].DigestFrequency := AFrequency;
      Result := True;
    end;
end;

function TNotificationTaskManager.CreateEscalationRule(const AName, ADescription: string;
  ATaskPriority: TTaskPriority; AInitialDelay, AInterval, AMaxEscalations: Integer;
  AChannel: TDeliveryChannel): Integer;
var
  rule: TEscalationRule;
begin
  rule.ID := FNextEscalationID;
  Inc(FNextEscalationID);
  rule.Name := AName;
  rule.Description := ADescription;
  rule.TaskPriority := ATaskPriority;
  rule.InitialDelay := AInitialDelay;
  rule.EscalationInterval := AInterval;
  rule.MaxEscalations := AMaxEscalations;
  rule.EscalateChannel := AChannel;
  rule.IsActive := True;
  
  SetLength(FEscalationRules, Length(FEscalationRules) + 1);
  FEscalationRules[High(FEscalationRules)] := rule;
  
  Result := rule.ID;
end;

function TNotificationTaskManager.DeleteEscalationRule(ARuleID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindEscalationRuleIndex(ARuleID);
  if idx < 0 then
    Exit;
    
  for i := idx to High(FEscalationRules) - 1 do
    FEscalationRules[i] := FEscalationRules[i + 1];
  SetLength(FEscalationRules, Length(FEscalationRules) - 1);
  Result := True;
end;

function TNotificationTaskManager.ActivateEscalationRule(ARuleID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindEscalationRuleIndex(ARuleID);
  if idx < 0 then
    Exit;
    
  FEscalationRules[idx].IsActive := True;
  Result := True;
end;

function TNotificationTaskManager.DeactivateEscalationRule(ARuleID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindEscalationRuleIndex(ARuleID);
  if idx < 0 then
    Exit;
    
  FEscalationRules[idx].IsActive := False;
  Result := True;
end;

function TNotificationTaskManager.GetAllEscalationRules: TEscalationRuleArray;
begin
  Result := Copy(FEscalationRules, 0, Length(FEscalationRules));
end;

function TNotificationTaskManager.TriggerEscalation(ATaskID: Integer): Boolean;
begin
  Result := False;
  // Implementation for triggering escalations
end;

function TNotificationTaskManager.CreateDigest(ARecipientID: Integer; 
  AFrequency: TDigestFrequency): Integer;
var
  digest: TDigestNotification;
begin
  digest.ID := FNextDigestID;
  Inc(FNextDigestID);
  digest.RecipientID := ARecipientID;
  digest.Frequency := AFrequency;
  digest.LastSent := 0;
  digest.NextScheduled := Now;
  SetLength(digest.IncludedNotifications, 0);
  digest.IsActive := True;
  
  SetLength(FDigests, Length(FDigests) + 1);
  FDigests[High(FDigests)] := digest;
  
  Result := digest.ID;
end;

function TNotificationTaskManager.UpdateDigestSchedule(ADigestID: Integer; 
  AFrequency: TDigestFrequency): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindDigestIndex(ADigestID);
  if idx < 0 then
    Exit;
    
  FDigests[idx].Frequency := AFrequency;
  Result := True;
end;

function TNotificationTaskManager.SendDigest(ADigestID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindDigestIndex(ADigestID);
  if idx < 0 then
    Exit;
    
  FDigests[idx].LastSent := Now;
  // Calculate next scheduled time based on frequency
  case FDigests[idx].Frequency of
    dfHourly: FDigests[idx].NextScheduled := IncHour(Now, 1);
    dfDaily: FDigests[idx].NextScheduled := IncDay(Now, 1);
    dfWeekly: FDigests[idx].NextScheduled := IncWeek(Now, 1);
  end;
  Result := True;
end;

function TNotificationTaskManager.GetPendingDigests: TDigestNotificationArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to High(FDigests) do
    if FDigests[i].IsActive and (FDigests[i].NextScheduled <= Now) then
    begin
      SetLength(Result, count + 1);
      Result[count] := FDigests[i];
      Inc(count);
    end;
end;

function TNotificationTaskManager.GetNotificationsByTask(ATaskID: Integer): TNotificationArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to High(FNotifications) do
    if FNotifications[i].TaskID = ATaskID then
    begin
      SetLength(Result, count + 1);
      Result[count] := FNotifications[i];
      Inc(count);
    end;
end;

function TNotificationTaskManager.GetNotificationsByRecipient(ARecipientID: Integer): TNotificationArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to High(FNotifications) do
    if FNotifications[i].RecipientID = ARecipientID then
    begin
      SetLength(Result, count + 1);
      Result[count] := FNotifications[i];
      Inc(count);
    end;
end;

function TNotificationTaskManager.GetUnreadNotifications(ARecipientID: Integer): TNotificationArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to High(FNotifications) do
    if (FNotifications[i].RecipientID = ARecipientID) and 
       (FNotifications[i].Status = nsSent) then
    begin
      SetLength(Result, count + 1);
      Result[count] := FNotifications[i];
      Inc(count);
    end;
end;

function TNotificationTaskManager.GetFailedNotifications: TNotificationArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to High(FNotifications) do
    if FNotifications[i].Status = nsFailed then
    begin
      SetLength(Result, count + 1);
      Result[count] := FNotifications[i];
      Inc(count);
    end;
end;

function TNotificationTaskManager.GetScheduledNotifications: TNotificationArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to High(FNotifications) do
    if FNotifications[i].Status = nsScheduled then
    begin
      SetLength(Result, count + 1);
      Result[count] := FNotifications[i];
      Inc(count);
    end;
end;

function TNotificationTaskManager.GetNotificationsByPriority(
  APriority: TNotificationPriority): TNotificationArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to High(FNotifications) do
    if FNotifications[i].Priority = APriority then
    begin
      SetLength(Result, count + 1);
      Result[count] := FNotifications[i];
      Inc(count);
    end;
end;

function TNotificationTaskManager.GetNotificationsByStatus(
  AStatus: TNotificationStatus): TNotificationArray;
var
  i, count: Integer;
begin
  SetLength(Result, 0);
  count := 0;
  
  for i := 0 to High(FNotifications) do
    if FNotifications[i].Status = AStatus then
    begin
      SetLength(Result, count + 1);
      Result[count] := FNotifications[i];
      Inc(count);
    end;
end;

function TNotificationTaskManager.GetRecentNotifications(ARecipientID: Integer; 
  AHours: Integer): TNotificationArray;
var
  i, count: Integer;
  cutoffTime: TDateTime;
begin
  SetLength(Result, 0);
  count := 0;
  cutoffTime := IncHour(Now, -AHours);
  
  for i := 0 to High(FNotifications) do
    if (FNotifications[i].RecipientID = ARecipientID) and 
       (FNotifications[i].CreatedAt >= cutoffTime) then
    begin
      SetLength(Result, count + 1);
      Result[count] := FNotifications[i];
      Inc(count);
    end;
end;

function TNotificationTaskManager.GetNotificationStatistics: TNotificationStats;
var
  i: Integer;
  totalReadTime: Double;
  readCount: Integer;
  notifType: TNotificationType;
  notifPriority: TNotificationPriority;
begin
  FillChar(Result, SizeOf(Result), 0);
  totalReadTime := 0;
  readCount := 0;
  
  for i := 0 to High(FNotifications) do
  begin
    case FNotifications[i].Status of
      nsSent: Inc(Result.TotalSent);
      nsFailed: Inc(Result.TotalFailed);
      nsRead: 
        begin
          Inc(Result.TotalRead);
          if FNotifications[i].ReadTime > FNotifications[i].SentTime then
          begin
            totalReadTime := totalReadTime + 
              MinutesBetween(FNotifications[i].ReadTime, FNotifications[i].SentTime);
            Inc(readCount);
          end;
        end;
      nsDismissed: Inc(Result.TotalDismissed);
    end;
    
    notifType := FNotifications[i].NotificationType;
    Inc(Result.ByType[notifType]);
    
    notifPriority := FNotifications[i].Priority;
    Inc(Result.ByPriority[notifPriority]);
  end;
  
  if readCount > 0 then
    Result.AverageReadTime := totalReadTime / readCount
  else
    Result.AverageReadTime := 0;
end;

function TNotificationTaskManager.GetUserNotificationStats(
  ARecipientID: Integer): TNotificationStats;
var
  i: Integer;
  totalReadTime: Double;
  readCount: Integer;
  notifType: TNotificationType;
  notifPriority: TNotificationPriority;
begin
  FillChar(Result, SizeOf(Result), 0);
  totalReadTime := 0;
  readCount := 0;
  
  for i := 0 to High(FNotifications) do
  begin
    if FNotifications[i].RecipientID <> ARecipientID then
      Continue;
      
    case FNotifications[i].Status of
      nsSent: Inc(Result.TotalSent);
      nsFailed: Inc(Result.TotalFailed);
      nsRead: 
        begin
          Inc(Result.TotalRead);
          if FNotifications[i].ReadTime > FNotifications[i].SentTime then
          begin
            totalReadTime := totalReadTime + 
              MinutesBetween(FNotifications[i].ReadTime, FNotifications[i].SentTime);
            Inc(readCount);
          end;
        end;
      nsDismissed: Inc(Result.TotalDismissed);
    end;
    
    notifType := FNotifications[i].NotificationType;
    Inc(Result.ByType[notifType]);
    
    notifPriority := FNotifications[i].Priority;
    Inc(Result.ByPriority[notifPriority]);
  end;
  
  if readCount > 0 then
    Result.AverageReadTime := totalReadTime / readCount
  else
    Result.AverageReadTime := 0;
end;

function TNotificationTaskManager.GetDeliverySuccessRate: Double;
var
  total, sent: Integer;
  i: Integer;
begin
  total := 0;
  sent := 0;
  
  for i := 0 to High(FNotifications) do
  begin
    if FNotifications[i].Status in [nsSent, nsRead, nsDismissed] then
    begin
      Inc(total);
      Inc(sent);
    end
    else if FNotifications[i].Status = nsFailed then
      Inc(total);
  end;
  
  if total > 0 then
    Result := (sent / total) * 100
  else
    Result := 0;
end;

function TNotificationTaskManager.GetAverageReadTime: Double;
var
  stats: TNotificationStats;
begin
  stats := GetNotificationStatistics;
  Result := stats.AverageReadTime;
end;

function TNotificationTaskManager.GenerateNotificationReport: string;
var
  stats: TNotificationStats;
  notifType: TNotificationType;
  notifPriority: TNotificationPriority;
begin
  stats := GetNotificationStatistics;
  
  Result := 'Notification System Report'#13#10;
  Result := Result + '========================='#13#10#13#10;
  Result := Result + Format('Total Notifications: %d'#13#10, 
    [Length(FNotifications)]);
  Result := Result + Format('Sent: %d'#13#10, [stats.TotalSent]);
  Result := Result + Format('Failed: %d'#13#10, [stats.TotalFailed]);
  Result := Result + Format('Read: %d'#13#10, [stats.TotalRead]);
  Result := Result + Format('Dismissed: %d'#13#10, [stats.TotalDismissed]);
  Result := Result + Format('Average Read Time: %.2f minutes'#13#10, 
    [stats.AverageReadTime]);
  Result := Result + Format('Delivery Success Rate: %.2f%%'#13#10#13#10, 
    [GetDeliverySuccessRate]);
  
  Result := Result + 'By Type:'#13#10;
  for notifType := Low(TNotificationType) to High(TNotificationType) do
    if stats.ByType[notifType] > 0 then
      Result := Result + Format('  %s: %d'#13#10, 
        [NotificationTypeToString(notifType), stats.ByType[notifType]]);
  
  Result := Result + #13#10'By Priority:'#13#10;
  for notifPriority := Low(TNotificationPriority) to High(TNotificationPriority) do
    if stats.ByPriority[notifPriority] > 0 then
      Result := Result + Format('  %s: %d'#13#10, 
        [NotificationPriorityToString(notifPriority), stats.ByPriority[notifPriority]]);
  
  Result := Result + #13#10 + Format('Templates: %d'#13#10, [Length(FTemplates)]);
  Result := Result + Format('Escalation Rules: %d'#13#10, [Length(FEscalationRules)]);
  Result := Result + Format('Active Digests: %d'#13#10, [Length(FDigests)]);
end;

function TNotificationTaskManager.SendBulkNotification(ARecipientIDs: array of Integer; 
  const ATitle, AMessage: string; APriority: TNotificationPriority): Integer;
var
  i, notifID: Integer;
begin
  Result := 0;
  for i := 0 to High(ARecipientIDs) do
  begin
    notifID := CreateNotification(ntInApp, APriority, 0, ARecipientIDs[i],
                                   ATitle, AMessage, Now);
    if SendNotification(notifID) then
      Inc(Result);
  end;
end;

function TNotificationTaskManager.DeleteOldNotifications(ADaysOld: Integer): Integer;
var
  i, newLen: Integer;
  cutoffDate: TDateTime;
begin
  Result := 0;
  cutoffDate := IncDay(Now, -ADaysOld);
  newLen := 0;
  
  for i := 0 to High(FNotifications) do
  begin
    if FNotifications[i].CreatedAt >= cutoffDate then
    begin
      if newLen <> i then
        FNotifications[newLen] := FNotifications[i];
      Inc(newLen);
    end
    else
      Inc(Result);
  end;
  
  SetLength(FNotifications, newLen);
end;

function TNotificationTaskManager.RetryAllFailed: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FNotifications) do
    if FNotifications[i].Status = nsFailed then
      if RetryFailedNotification(FNotifications[i].ID) then
        Inc(Result);
end;

function TNotificationTaskManager.NotificationTypeToString(
  AType: TNotificationType): string;
begin
  case AType of
    ntEmail: Result := 'Email';
    ntSMS: Result := 'SMS';
    ntPush: Result := 'Push';
    ntInApp: Result := 'In-App';
    ntDesktop: Result := 'Desktop';
    ntSlack: Result := 'Slack';
    ntWebhook: Result := 'Webhook';
  else
    Result := 'Unknown';
  end;
end;

function TNotificationTaskManager.NotificationPriorityToString(
  APriority: TNotificationPriority): string;
begin
  case APriority of
    npLow: Result := 'Low';
    npNormal: Result := 'Normal';
    npHigh: Result := 'High';
    npUrgent: Result := 'Urgent';
  else
    Result := 'Unknown';
  end;
end;

function TNotificationTaskManager.NotificationStatusToString(
  AStatus: TNotificationStatus): string;
begin
  case AStatus of
    nsScheduled: Result := 'Scheduled';
    nsPending: Result := 'Pending';
    nsSent: Result := 'Sent';
    nsFailed: Result := 'Failed';
    nsRead: Result := 'Read';
    nsDismissed: Result := 'Dismissed';
  else
    Result := 'Unknown';
  end;
end;

function TNotificationTaskManager.DigestFrequencyToString(
  AFreq: TDigestFrequency): string;
begin
  case AFreq of
    dfNone: Result := 'None';
    dfHourly: Result := 'Hourly';
    dfDaily: Result := 'Daily';
    dfWeekly: Result := 'Weekly';
  else
    Result := 'Unknown';
  end;
end;

procedure TNotificationTaskManager.ProcessScheduledNotifications;
var
  i: Integer;
begin
  for i := 0 to High(FNotifications) do
    if (FNotifications[i].Status = nsScheduled) and 
       (FNotifications[i].ScheduledTime <= Now) then
      SendNotification(FNotifications[i].ID);
end;

procedure TNotificationTaskManager.ProcessAllPending;
var
  i: Integer;
begin
  ProcessScheduledNotifications;
  ProcessEscalations;
  ProcessDigests;
end;

procedure TNotificationTaskManager.EnableNotifications(AEnabled: Boolean);
begin
  FNotificationsEnabled := AEnabled;
end;

procedure TNotificationTaskManager.SetDefaultRetries(ARetries: Integer);
begin
  FDefaultRetries := ARetries;
end;

function TNotificationTaskManager.GetNotificationsEnabled: Boolean;
begin
  Result := FNotificationsEnabled;
end;

function TNotificationTaskManager.SaveNotificationDataToFile(
  const AFilename: string): Boolean;
var
  f: TextFile;
  i: Integer;
begin
  Result := False;
  try
    AssignFile(f, AFilename);
    Rewrite(f);
    
    WriteLn(f, Length(FNotifications));
    for i := 0 to High(FNotifications) do
    begin
      WriteLn(f, FNotifications[i].ID);
      WriteLn(f, Ord(FNotifications[i].NotificationType));
      WriteLn(f, Ord(FNotifications[i].Priority));
      WriteLn(f, Ord(FNotifications[i].Status));
      WriteLn(f, FNotifications[i].TaskID);
      WriteLn(f, FNotifications[i].RecipientID);
      WriteLn(f, FNotifications[i].Title);
      WriteLn(f, FNotifications[i].Message);
    end;
    
    CloseFile(f);
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TNotificationTaskManager.LoadNotificationDataFromFile(
  const AFilename: string): Boolean;
begin
  Result := False;
  // Implementation for loading
end;

procedure TNotificationTaskManager.SelfTest;
var
  taskID1, taskID2: Integer;
  notifID1, notifID2, notifID3: Integer;
  templateID1: Integer;
  prefID: Integer;
  ruleID: Integer;
  digestID: Integer;
  notifs: TNotificationArray;
  stats: TNotificationStats;
  report: string;
begin
  WriteLn('Notification System - Comprehensive Test');
  WriteLn('=========================================');
  WriteLn;
  
  // Create some tasks for testing
  WriteLn('1. Creating test tasks...');
  taskID1 := AddTask('Complete project proposal', 'Write and submit the Q4 proposal',
                      'Work', tpHigh, EncodeDate(2024, 12, 31), 8.0);
  taskID2 := AddTask('Review code changes', 'Review PR #123',
                      'Development', tpMedium, EncodeDate(2024, 12, 25), 2.0);
  WriteLn('   Created tasks: ', taskID1, ', ', taskID2);
  WriteLn;
  
  // Test notification creation
  WriteLn('2. Creating notifications...');
  notifID1 := CreateNotification(ntEmail, npHigh, taskID1, 1,
                                  'Task Due Soon', 
                                  'Your task "Complete project proposal" is due in 24 hours',
                                  Now);
  notifID2 := CreateNotification(ntPush, npNormal, taskID2, 1,
                                  'Task Assigned',
                                  'You have been assigned to review code changes',
                                  Now);
  notifID3 := CreateNotification(ntInApp, npUrgent, taskID1, 1,
                                  'Urgent Reminder',
                                  'Project proposal deadline approaching!',
                                  IncHour(Now, 1));
  WriteLn('   Created notifications: ', notifID1, ', ', notifID2, ', ', notifID3);
  WriteLn;
  
  // Test notification templates
  WriteLn('3. Creating notification templates...');
  templateID1 := CreateTemplate('Task Due Reminder', 
                                 'Remind user when task is due',
                                 ntEmail,
                                 'Task Due: {TASK_TITLE}',
                                 'Your {TASK_PRIORITY} priority task "{TASK_TITLE}" is due soon. ' +
                                 'Current status: {TASK_STATUS}',
                                 npNormal);
  WriteLn('   Created template: ', templateID1);
  WriteLn;
  
  // Test user preferences
  WriteLn('4. Setting user preferences...');
  prefID := SetUserPreference(1, ntEmail, True, 22, 8, dfDaily);
  WriteLn('   Set preference ID: ', prefID);
  WriteLn('   Quiet hours: 22:00 - 08:00');
  WriteLn('   Digest frequency: Daily');
  WriteLn;
  
  // Test sending notifications
  WriteLn('5. Sending notifications...');
  if SendNotification(notifID1) then
    WriteLn('   Sent notification ', notifID1)
  else
    WriteLn('   Failed to send notification ', notifID1);
    
  if SendNotification(notifID2) then
    WriteLn('   Sent notification ', notifID2)
  else
    WriteLn('   Failed to send notification ', notifID2);
  WriteLn;
  
  // Test notification queries
  WriteLn('6. Querying notifications...');
  notifs := GetNotificationsByRecipient(1);
  WriteLn('   Notifications for user 1: ', Length(notifs));
  
  notifs := GetUnreadNotifications(1);
  WriteLn('   Unread notifications: ', Length(notifs));
  
  notifs := GetScheduledNotifications;
  WriteLn('   Scheduled notifications: ', Length(notifs));
  WriteLn;
  
  // Test marking as read
  WriteLn('7. Marking notification as read...');
  if MarkAsRead(notifID1) then
    WriteLn('   Marked notification ', notifID1, ' as read');
  WriteLn;
  
  // Test escalation rules
  WriteLn('8. Creating escalation rule...');
  ruleID := CreateEscalationRule('High Priority Escalation',
                                  'Escalate high priority tasks after 30 minutes',
                                  tpHigh, 30, 15, 3, dcEmail);
  WriteLn('   Created escalation rule: ', ruleID);
  WriteLn;
  
  // Test digest notifications
  WriteLn('9. Creating digest notification...');
  digestID := CreateDigest(1, dfDaily);
  WriteLn('   Created digest: ', digestID);
  WriteLn;
  
  // Test statistics
  WriteLn('10. Generating statistics...');
  stats := GetNotificationStatistics;
  WriteLn('    Total sent: ', stats.TotalSent);
  WriteLn('    Total read: ', stats.TotalRead);
  WriteLn('    Success rate: ', GetDeliverySuccessRate:0:2, '%');
  WriteLn;
  
  // Test bulk operations
  WriteLn('11. Testing bulk operations...');
  WriteLn('    Sending bulk notification to 3 users...');
  WriteLn('    Sent to: ', SendBulkNotification([1, 2, 3],
                                                  'Team Update',
                                                  'All tasks updated',
                                                  npNormal), ' users');
  WriteLn;
  
  // Test notification report
  WriteLn('12. Generating notification report...');
  report := GenerateNotificationReport;
  WriteLn(report);
  
  // Test template instantiation
  WriteLn('13. Creating notification from template...');
  notifID1 := CreateNotificationFromTemplate(templateID1, taskID1, 1, Now);
  WriteLn('    Created notification from template: ', notifID1);
  WriteLn;
  
  // Test processing
  WriteLn('14. Processing scheduled notifications...');
  ProcessScheduledNotifications;
  WriteLn('    Processed all scheduled notifications');
  WriteLn;
  
  WriteLn('Notification System Test Complete!');
  WriteLn('===================================');
end;

end.
