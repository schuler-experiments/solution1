
unit TaskNotifications;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TNotificationType = (ntTaskDue, ntTaskOverdue, ntTaskCompleted, ntTaskAssigned,
                       ntTaskStatusChanged, ntTaskCommented, ntTaskDependencyReady,
                       ntReminderAlert, ntCustom);

  TNotificationChannel = (ncEmail, ncSystem, ncSMS, ncSlack, ncWebhook);

  TNotificationPriority = (npLow, npNormal, npHigh, npCritical);

  TNotification = record
    ID: string;
    TaskID: string;
    NotificationType: TNotificationType;
    Priority: TNotificationPriority;
    Channel: TNotificationChannel;
    RecipientEmail: string;
    Title: string;
    Message: string;
    CreatedTime: TDateTime;
    ScheduledTime: TDateTime;
    SentTime: TDateTime;
    IsRead: boolean;
    IsSent: boolean;
    DeliveryAttempts: integer;
  end;

  TNotificationArray = array of TNotification;

  TTaskNotificationManagerClass = class
  private
    FNotifications: TNotificationArray;
    FNotificationCounter: integer;
    FMaxRetries: integer;
    function GenerateNotificationID(): string;
    function FindNotificationIndex(aNotificationID: string): integer;
  public
    constructor Create();
    destructor Destroy(); override;
    function CreateNotification(aTaskID: string; aType: TNotificationType;
                               aPriority: TNotificationPriority;
                               aChannel: TNotificationChannel;
                               aRecipient: string; aTitle, aMessage: string): string;
    function GetNotification(aNotificationID: string): TNotification;
    function GetTaskNotifications(aTaskID: string): TNotificationArray;
    function GetUnreadNotifications(): TNotificationArray;
    function GetPendingNotifications(): TNotificationArray;
    procedure MarkAsRead(aNotificationID: string);
    procedure MarkAsSent(aNotificationID: string);
    procedure UpdateDeliveryStatus(aNotificationID: string; aSent: boolean);
    function GetNotificationsByChannel(aChannel: TNotificationChannel): TNotificationArray;
    function GetNotificationsByPriority(aPriority: TNotificationPriority): TNotificationArray;
    function GetNotificationCountByType(aType: TNotificationType): integer;
    function GetDeliverySuccessRate(): double;
    procedure ClearReadNotifications();
    procedure DeleteNotification(aNotificationID: string);
    function GetAllNotifications(): TNotificationArray;
    procedure SelfTest();
  end;

implementation

constructor TTaskNotificationManagerClass.Create();
begin
  inherited Create();
  SetLength(FNotifications, 0);
  FNotificationCounter := 0;
  FMaxRetries := 5;
end;

destructor TTaskNotificationManagerClass.Destroy();
begin
  SetLength(FNotifications, 0);
  inherited Destroy();
end;

function TTaskNotificationManagerClass.GenerateNotificationID(): string;
begin
  inc(FNotificationCounter);
  GenerateNotificationID := 'NOTIF-' + IntToStr(FNotificationCounter);
end;

function TTaskNotificationManagerClass.FindNotificationIndex(aNotificationID: string): integer;
var
  i: integer;
begin
  FindNotificationIndex := -1;
  for i := 0 to Length(FNotifications) - 1 do
  begin
    if FNotifications[i].ID = aNotificationID then
    begin
      FindNotificationIndex := i;
      Exit;
    end;
  end;
end;

function TTaskNotificationManagerClass.CreateNotification(aTaskID: string;
  aType: TNotificationType; aPriority: TNotificationPriority;
  aChannel: TNotificationChannel; aRecipient: string; aTitle, aMessage: string): string;
var
  newNotif: TNotification;
  idx: integer;
begin
  newNotif.ID := GenerateNotificationID();
  newNotif.TaskID := aTaskID;
  newNotif.NotificationType := aType;
  newNotif.Priority := aPriority;
  newNotif.Channel := aChannel;
  newNotif.RecipientEmail := aRecipient;
  newNotif.Title := aTitle;
  newNotif.Message := aMessage;
  newNotif.CreatedTime := Now();
  newNotif.ScheduledTime := Now();
  newNotif.SentTime := 0;
  newNotif.IsRead := false;
  newNotif.IsSent := false;
  newNotif.DeliveryAttempts := 0;

  idx := Length(FNotifications);
  SetLength(FNotifications, idx + 1);
  FNotifications[idx] := newNotif;

  CreateNotification := newNotif.ID;
end;

function TTaskNotificationManagerClass.GetNotification(aNotificationID: string): TNotification;
var
  idx: integer;
  emptyNotif: TNotification;
begin
  FillChar(emptyNotif, SizeOf(emptyNotif), 0);
  idx := FindNotificationIndex(aNotificationID);
  if idx >= 0 then
  begin
    GetNotification := FNotifications[idx];
  end
  else
  begin
    GetNotification := emptyNotif;
  end;
end;

function TTaskNotificationManagerClass.GetTaskNotifications(aTaskID: string): TNotificationArray;
var
  i, count: integer;
  notifArray: TNotificationArray;
begin
  count := 0;
  SetLength(notifArray, 0);
  for i := 0 to Length(FNotifications) - 1 do
  begin
    if FNotifications[i].TaskID = aTaskID then
    begin
      SetLength(notifArray, count + 1);
      notifArray[count] := FNotifications[i];
      inc(count);
    end;
  end;
  GetTaskNotifications := notifArray;
end;

function TTaskNotificationManagerClass.GetUnreadNotifications(): TNotificationArray;
var
  i, count: integer;
  notifArray: TNotificationArray;
begin
  count := 0;
  SetLength(notifArray, 0);
  for i := 0 to Length(FNotifications) - 1 do
  begin
    if not FNotifications[i].IsRead then
    begin
      SetLength(notifArray, count + 1);
      notifArray[count] := FNotifications[i];
      inc(count);
    end;
  end;
  GetUnreadNotifications := notifArray;
end;

function TTaskNotificationManagerClass.GetPendingNotifications(): TNotificationArray;
var
  i, count: integer;
  notifArray: TNotificationArray;
begin
  count := 0;
  SetLength(notifArray, 0);
  for i := 0 to Length(FNotifications) - 1 do
  begin
    if not FNotifications[i].IsSent then
    begin
      SetLength(notifArray, count + 1);
      notifArray[count] := FNotifications[i];
      inc(count);
    end;
  end;
  GetPendingNotifications := notifArray;
end;

procedure TTaskNotificationManagerClass.MarkAsRead(aNotificationID: string);
var
  idx: integer;
begin
  idx := FindNotificationIndex(aNotificationID);
  if idx >= 0 then
    FNotifications[idx].IsRead := true;
end;

procedure TTaskNotificationManagerClass.MarkAsSent(aNotificationID: string);
var
  idx: integer;
begin
  idx := FindNotificationIndex(aNotificationID);
  if idx >= 0 then
  begin
    FNotifications[idx].IsSent := true;
    FNotifications[idx].SentTime := Now();
  end;
end;

procedure TTaskNotificationManagerClass.UpdateDeliveryStatus(aNotificationID: string; aSent: boolean);
var
  idx: integer;
begin
  idx := FindNotificationIndex(aNotificationID);
  if idx >= 0 then
  begin
    FNotifications[idx].IsSent := aSent;
    inc(FNotifications[idx].DeliveryAttempts);
    if aSent then
      FNotifications[idx].SentTime := Now();
  end;
end;

function TTaskNotificationManagerClass.GetNotificationsByChannel(aChannel: TNotificationChannel): TNotificationArray;
var
  i, count: integer;
  notifArray: TNotificationArray;
begin
  count := 0;
  SetLength(notifArray, 0);
  for i := 0 to Length(FNotifications) - 1 do
  begin
    if FNotifications[i].Channel = aChannel then
    begin
      SetLength(notifArray, count + 1);
      notifArray[count] := FNotifications[i];
      inc(count);
    end;
  end;
  GetNotificationsByChannel := notifArray;
end;

function TTaskNotificationManagerClass.GetNotificationsByPriority(aPriority: TNotificationPriority): TNotificationArray;
var
  i, count: integer;
  notifArray: TNotificationArray;
begin
  count := 0;
  SetLength(notifArray, 0);
  for i := 0 to Length(FNotifications) - 1 do
  begin
    if FNotifications[i].Priority = aPriority then
    begin
      SetLength(notifArray, count + 1);
      notifArray[count] := FNotifications[i];
      inc(count);
    end;
  end;
  GetNotificationsByPriority := notifArray;
end;

function TTaskNotificationManagerClass.GetNotificationCountByType(aType: TNotificationType): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(FNotifications) - 1 do
  begin
    if FNotifications[i].NotificationType = aType then
      inc(count);
  end;
  GetNotificationCountByType := count;
end;

function TTaskNotificationManagerClass.GetDeliverySuccessRate(): double;
var
  i, totalSent, totalCreated: integer;
begin
  totalCreated := Length(FNotifications);
  if totalCreated = 0 then
  begin
    GetDeliverySuccessRate := 0.0;
    Exit;
  end;

  totalSent := 0;
  for i := 0 to Length(FNotifications) - 1 do
  begin
    if FNotifications[i].IsSent then
      inc(totalSent);
  end;

  GetDeliverySuccessRate := (totalSent / totalCreated) * 100.0;
end;

procedure TTaskNotificationManagerClass.ClearReadNotifications();
var
  i, writeIdx: integer;
begin
  writeIdx := 0;
  for i := 0 to Length(FNotifications) - 1 do
  begin
    if not FNotifications[i].IsRead then
    begin
      FNotifications[writeIdx] := FNotifications[i];
      inc(writeIdx);
    end;
  end;
  SetLength(FNotifications, writeIdx);
end;

procedure TTaskNotificationManagerClass.DeleteNotification(aNotificationID: string);
var
  idx, i: integer;
begin
  idx := FindNotificationIndex(aNotificationID);
  if idx >= 0 then
  begin
    for i := idx to Length(FNotifications) - 2 do
      FNotifications[i] := FNotifications[i + 1];
    SetLength(FNotifications, Length(FNotifications) - 1);
  end;
end;

function TTaskNotificationManagerClass.GetAllNotifications(): TNotificationArray;
begin
  GetAllNotifications := FNotifications;
end;

procedure TTaskNotificationManagerClass.SelfTest();
var
  notifID1: string;
  notifs: TNotificationArray;
  emailNotifs: TNotificationArray;
  unreadNotifs: TNotificationArray;
  successRate: double;
begin
  WriteLn('--- Task Notifications Manager Self Test ---');

  { Test notification creation }
  notifID1 := CreateNotification('TASK-001', ntTaskDue, npHigh, ncEmail,
    'user@example.com', 'Task Due Soon', 'Your task is due in 2 hours');
  CreateNotification('TASK-001', ntTaskOverdue, npCritical, ncSystem,
    'admin@example.com', 'Task Overdue!', 'Critical task overdue');
  CreateNotification('TASK-002', ntTaskAssigned, npNormal, ncSlack,
    'team@slack.com', 'New Task Assigned', 'You have been assigned a new task');

  WriteLn('✓ Created 3 notifications');

  { Test retrieval }
  notifs := GetTaskNotifications('TASK-001');
  if Length(notifs) = 2 then
    WriteLn('✓ Task notifications retrieval works')
  else
    WriteLn('✗ Task notifications retrieval failed: got ', Length(notifs), ' expected 2');

  { Test unread notifications }
  unreadNotifs := GetUnreadNotifications();
  if Length(unreadNotifs) = 3 then
    WriteLn('✓ Unread notifications: ', Length(unreadNotifs))
  else
    WriteLn('✗ Unread count mismatch');

  { Test marking as read }
  MarkAsRead(notifID1);
  unreadNotifs := GetUnreadNotifications();
  if Length(unreadNotifs) = 2 then
    WriteLn('✓ Mark as read works')
  else
    WriteLn('✗ Mark as read failed');

  { Test channel filtering }
  emailNotifs := GetNotificationsByChannel(ncEmail);
  if Length(emailNotifs) = 1 then
    WriteLn('✓ Channel filtering works')
  else
    WriteLn('✗ Channel filtering failed');

  { Test delivery status }
  MarkAsSent(notifID1);
  successRate := GetDeliverySuccessRate();
  WriteLn('✓ Delivery success rate: ', FormatFloat('0.00', successRate), '%');

  { Test count by type }
  if GetNotificationCountByType(ntTaskDue) = 1 then
    WriteLn('✓ Notification count by type works')
  else
    WriteLn('✗ Notification count failed');

  WriteLn('--- Notification Manager Tests Complete ---');
  WriteLn('');
end;

end.
