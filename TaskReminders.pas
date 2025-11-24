unit TaskReminders;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TReminderType = (rtEmail, rtNotification, rtPopup, rtSMS);
  TReminderTiming = (rtBefore5Min, rtBefore15Min, rtBefore1Hour, rtBefore1Day, rtBefore1Week, rtCustom);

  TTaskReminder = record
    id: integer;
    taskId: integer;
    reminderType: TReminderType;
    timing: TReminderTiming;
    customMinutes: integer;
    dueDateTime: TDateTime;
    reminderDateTime: TDateTime;
    acknowledged: boolean;
    createdDate: TDateTime;
  end;

  TReminderArray = array of TTaskReminder;

  TReminderStats = record
    totalReminders: integer;
    pendingReminders: integer;
    acknowledgedReminders: integer;
    overdueReminders: integer;
    emailReminders: integer;
    notificationReminders: integer;
  end;

  TTaskReminderManagerClass = class
  private
    reminders: TReminderArray;
    reminderCounter: integer;
    function FindReminderIndex(id: integer): integer;
  public
    constructor Create();
    destructor Destroy(); override;
    function AddReminder(taskId: integer; reminderType: TReminderType;
                        timing: TReminderTiming; dueDate: TDateTime): integer;
    function AddCustomReminder(taskId: integer; reminderType: TReminderType;
                               customMinutes: integer; dueDate: TDateTime): integer;
    function GetReminder(id: integer): TTaskReminder;
    function GetTaskReminders(taskId: integer): TReminderArray;
    function GetPendingReminders(): TReminderArray;
    function GetOverdueReminders(): TReminderArray;
    function AcknowledgeReminder(id: integer): boolean;
    function GetReminderStats(): TReminderStats;
    function DeleteReminder(id: integer): boolean;
    function GetRemindersForToday(): TReminderArray;
    function GetRemindersForTomorrow(): TReminderArray;
    function GetRemindersThisWeek(): TReminderArray;
    function ShouldSendReminder(reminder: TTaskReminder): boolean;
    function CalculateReminderDateTime(dueDate: TDateTime;
                                       timing: TReminderTiming;
                                       customMinutes: integer): TDateTime;
    procedure SelfTest();
  end;

implementation

constructor TTaskReminderManagerClass.Create();
begin
  inherited Create();
  SetLength(reminders, 0);
  reminderCounter := 0;
end;

destructor TTaskReminderManagerClass.Destroy();
begin
  SetLength(reminders, 0);
  inherited Destroy();
end;

function TTaskReminderManagerClass.FindReminderIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(reminders) - 1 do
  begin
    if reminders[i].id = id then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskReminderManagerClass.AddReminder(taskId: integer;
                                               reminderType: TReminderType;
                                               timing: TReminderTiming;
                                               dueDate: TDateTime): integer;
var
  newReminder: TTaskReminder;
  idx: integer;
begin
  inc(reminderCounter);
  newReminder.id := reminderCounter;
  newReminder.taskId := taskId;
  newReminder.reminderType := reminderType;
  newReminder.timing := timing;
  newReminder.customMinutes := 0;
  newReminder.dueDateTime := dueDate;
  newReminder.reminderDateTime := CalculateReminderDateTime(dueDate, timing, 0);
  newReminder.acknowledged := false;
  newReminder.createdDate := Now();

  idx := Length(reminders);
  SetLength(reminders, idx + 1);
  reminders[idx] := newReminder;

  result := reminderCounter;
end;

function TTaskReminderManagerClass.AddCustomReminder(taskId: integer;
                                                      reminderType: TReminderType;
                                                      customMinutes: integer;
                                                      dueDate: TDateTime): integer;
var
  newReminder: TTaskReminder;
  idx: integer;
begin
  inc(reminderCounter);
  newReminder.id := reminderCounter;
  newReminder.taskId := taskId;
  newReminder.reminderType := reminderType;
  newReminder.timing := rtCustom;
  newReminder.customMinutes := customMinutes;
  newReminder.dueDateTime := dueDate;
  newReminder.reminderDateTime := CalculateReminderDateTime(dueDate, rtCustom, customMinutes);
  newReminder.acknowledged := false;
  newReminder.createdDate := Now();

  idx := Length(reminders);
  SetLength(reminders, idx + 1);
  reminders[idx] := newReminder;

  result := reminderCounter;
end;

function TTaskReminderManagerClass.GetReminder(id: integer): TTaskReminder;
var
  idx: integer;
begin
  idx := FindReminderIndex(id);
  if idx >= 0 then
    result := reminders[idx]
  else
  begin
    FillChar(result, SizeOf(TTaskReminder), 0);
    result.id := -1;
  end;
end;

function TTaskReminderManagerClass.GetTaskReminders(taskId: integer): TReminderArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  for i := 0 to Length(reminders) - 1 do
  begin
    if reminders[i].taskId = taskId then
    begin
      SetLength(result, count + 1);
      result[count] := reminders[i];
      inc(count);
    end;
  end;
end;

function TTaskReminderManagerClass.GetPendingReminders(): TReminderArray;
var
  i, count: integer;
  now: TDateTime;
begin
  SetLength(result, 0);
  count := 0;
  now := SysUtils.Now();
  for i := 0 to Length(reminders) - 1 do
  begin
    if (not reminders[i].acknowledged) and (reminders[i].reminderDateTime <= now) then
    begin
      SetLength(result, count + 1);
      result[count] := reminders[i];
      inc(count);
    end;
  end;
end;

function TTaskReminderManagerClass.GetOverdueReminders(): TReminderArray;
var
  i, count: integer;
  now: TDateTime;
begin
  SetLength(result, 0);
  count := 0;
  now := SysUtils.Now();
  for i := 0 to Length(reminders) - 1 do
  begin
    if (not reminders[i].acknowledged) and (reminders[i].dueDateTime < now) then
    begin
      SetLength(result, count + 1);
      result[count] := reminders[i];
      inc(count);
    end;
  end;
end;

function TTaskReminderManagerClass.AcknowledgeReminder(id: integer): boolean;
var
  idx: integer;
begin
  idx := FindReminderIndex(id);
  if idx >= 0 then
  begin
    reminders[idx].acknowledged := true;
    result := true;
  end
  else
    result := false;
end;

function TTaskReminderManagerClass.GetReminderStats(): TReminderStats;
var
  i: integer;
  now: TDateTime;
begin
  FillChar(result, SizeOf(TReminderStats), 0);
  now := SysUtils.Now();

  result.totalReminders := Length(reminders);

  for i := 0 to Length(reminders) - 1 do
  begin
    if not reminders[i].acknowledged then
    begin
      inc(result.pendingReminders);
      if reminders[i].dueDateTime < now then
        inc(result.overdueReminders);
    end
    else
      inc(result.acknowledgedReminders);

    case reminders[i].reminderType of
      rtEmail:
        inc(result.emailReminders);
      rtNotification, rtPopup, rtSMS:
        inc(result.notificationReminders);
    end;
  end;
end;

function TTaskReminderManagerClass.DeleteReminder(id: integer): boolean;
var
  idx, i: integer;
begin
  idx := FindReminderIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(reminders) - 2 do
      reminders[i] := reminders[i + 1];
    SetLength(reminders, Length(reminders) - 1);
    result := true;
  end
  else
    result := false;
end;

function TTaskReminderManagerClass.GetRemindersForToday(): TReminderArray;
var
  i, count: integer;
  today: TDateTime;
begin
  SetLength(result, 0);
  count := 0;
  today := Date();

  for i := 0 to Length(reminders) - 1 do
  begin
    if Trunc(reminders[i].reminderDateTime) = Trunc(today) then
    begin
      SetLength(result, count + 1);
      result[count] := reminders[i];
      inc(count);
    end;
  end;
end;

function TTaskReminderManagerClass.GetRemindersForTomorrow(): TReminderArray;
var
  i, count: integer;
  tomorrow: TDateTime;
begin
  SetLength(result, 0);
  count := 0;
  tomorrow := Date() + 1;

  for i := 0 to Length(reminders) - 1 do
  begin
    if Trunc(reminders[i].reminderDateTime) = Trunc(tomorrow) then
    begin
      SetLength(result, count + 1);
      result[count] := reminders[i];
      inc(count);
    end;
  end;
end;

function TTaskReminderManagerClass.GetRemindersThisWeek(): TReminderArray;
var
  i, count: integer;
  weekStart, weekEnd: TDateTime;
begin
  SetLength(result, 0);
  count := 0;
  weekStart := Date() - DayOfWeek(Date()) + 1;
  weekEnd := weekStart + 7;

  for i := 0 to Length(reminders) - 1 do
  begin
    if (reminders[i].reminderDateTime >= weekStart) and
       (reminders[i].reminderDateTime < weekEnd) then
    begin
      SetLength(result, count + 1);
      result[count] := reminders[i];
      inc(count);
    end;
  end;
end;

function TTaskReminderManagerClass.ShouldSendReminder(reminder: TTaskReminder): boolean;
var
  now: TDateTime;
begin
  now := SysUtils.Now();
  result := (not reminder.acknowledged) and (reminder.reminderDateTime <= now);
end;

function TTaskReminderManagerClass.CalculateReminderDateTime(dueDate: TDateTime;
                                                             timing: TReminderTiming;
                                                             customMinutes: integer): TDateTime;
begin
  case timing of
    rtBefore5Min:
      result := dueDate - EncodeTime(0, 5, 0, 0);
    rtBefore15Min:
      result := dueDate - EncodeTime(0, 15, 0, 0);
    rtBefore1Hour:
      result := dueDate - EncodeTime(1, 0, 0, 0);
    rtBefore1Day:
      result := dueDate - 1;
    rtBefore1Week:
      result := dueDate - 7;
    rtCustom:
      result := dueDate - EncodeTime(0, customMinutes, 0, 0);
  else
    result := dueDate;
  end;
end;

procedure TTaskReminderManagerClass.SelfTest();
var
  remId1, remId2, remId3: integer;
  taskReminders: TReminderArray;
  stats: TReminderStats;
  tomorrow: TDateTime;
begin
  WriteLn('');
  WriteLn('=== TaskReminder Manager Self Test ===');

  tomorrow := Date() + 1;

  remId1 := AddReminder(1, rtEmail, rtBefore1Day, tomorrow);
  remId2 := AddReminder(1, rtNotification, rtBefore15Min, tomorrow);
  remId3 := AddReminder(2, rtSMS, rtCustom, tomorrow);
  AddCustomReminder(2, rtPopup, 30, tomorrow);

  WriteLn('Added 4 reminders');

  taskReminders := GetTaskReminders(1);
  WriteLn('Reminders for task 1: ' + IntToStr(Length(taskReminders)));

  if AcknowledgeReminder(remId1) then
    WriteLn('Acknowledged reminder');

  stats := GetReminderStats();
  WriteLn('Total reminders: ' + IntToStr(stats.totalReminders));
  WriteLn('Pending reminders: ' + IntToStr(stats.pendingReminders));
  WriteLn('Acknowledged: ' + IntToStr(stats.acknowledgedReminders));

  if DeleteReminder(remId3) then
    WriteLn('Reminder deleted');

  WriteLn('=== TaskReminder Self Test Complete ===');
end;

end.
