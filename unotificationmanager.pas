
unit unotificationmanager;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, utaskmanager;

type
  TNotificationType = (ntOverdue, ntDueSoon, ntStalled, ntHighPriority);

  TNotification = record
    TaskId: Integer;
    TaskTitle: string;
    NotifyType: TNotificationType;
    Message: string;
    Timestamp: TDateTime;
  end;

  TNotificationArray = array of TNotification;

  TNotificationManager = class
  private
    FNotifications: TNotificationArray;
    FDueSoonThresholdHours: Integer;
  public
    constructor Create;
    procedure ScanTasks(AManager: TTaskManager);
    function GetNotifications: TNotificationArray;
    function GetSummary: string;
    procedure Clear;
    procedure self_test(AManager: TTaskManager);
    property DueSoonThresholdHours: Integer read FDueSoonThresholdHours write FDueSoonThresholdHours;
  end;

implementation

constructor TNotificationManager.Create;
begin
  FDueSoonThresholdHours := 24;
  SetLength(FNotifications, 0);
end;

procedure TNotificationManager.Clear;
begin
  SetLength(FNotifications, 0);
end;

procedure TNotificationManager.ScanTasks(AManager: TTaskManager);
var
  Tasks: TTaskArray;
  i: Integer;
  NewIdx: Integer;
  NowTime: TDateTime;
begin
  if AManager = nil then Exit;
  Tasks := AManager.get_tasks;
  NowTime := Now;

  for i := 0 to High(Tasks) do
  begin
    // Skip done or archived tasks
    if (Tasks[i].status = ts_done) or (Tasks[i].archived) then continue;

    // Check Overdue
    if (Tasks[i].due_date < NowTime) and (Tasks[i].due_date <> 0) then
    begin
      NewIdx := Length(FNotifications);
      SetLength(FNotifications, NewIdx + 1);
      FNotifications[NewIdx].TaskId := Tasks[i].id;
      FNotifications[NewIdx].TaskTitle := Tasks[i].title;
      FNotifications[NewIdx].NotifyType := ntOverdue;
      FNotifications[NewIdx].Message := 'Task is overdue!';
      FNotifications[NewIdx].Timestamp := NowTime;
    end
    // Check Due Soon
    else if (Tasks[i].due_date <> 0) and (HoursBetween(Tasks[i].due_date, NowTime) <= FDueSoonThresholdHours) then
    begin
      NewIdx := Length(FNotifications);
      SetLength(FNotifications, NewIdx + 1);
      FNotifications[NewIdx].TaskId := Tasks[i].id;
      FNotifications[NewIdx].TaskTitle := Tasks[i].title;
      FNotifications[NewIdx].NotifyType := ntDueSoon;
      FNotifications[NewIdx].Message := Format('Task is due in %d hours', [HoursBetween(Tasks[i].due_date, NowTime)]);
      FNotifications[NewIdx].Timestamp := NowTime;
    end;

    // Check Stalled
    if AManager.is_stalled(Tasks[i].id, 3) then // 3 days threshold
    begin
       NewIdx := Length(FNotifications);
       SetLength(FNotifications, NewIdx + 1);
       FNotifications[NewIdx].TaskId := Tasks[i].id;
       FNotifications[NewIdx].TaskTitle := Tasks[i].title;
       FNotifications[NewIdx].NotifyType := ntStalled;
       FNotifications[NewIdx].Message := 'No activity for 3 days';
       FNotifications[NewIdx].Timestamp := NowTime;
    end;
  end;
end;

function TNotificationManager.GetNotifications: TNotificationArray;
begin
  Result := FNotifications;
end;

function TNotificationManager.GetSummary: string;
var
  i: Integer;
  S: string;
begin
  if Length(FNotifications) = 0 then
    Exit('No active notifications.');

  S := '--- Notifications Summary ---' + sLineBreak;
  for i := 0 to High(FNotifications) do
  begin
    S := S + Format('[%s] Task #%d "%s": %s', [
      DateTimeToStr(FNotifications[i].Timestamp),
      FNotifications[i].TaskId,
      FNotifications[i].TaskTitle,
      FNotifications[i].Message
    ]) + sLineBreak;
  end;
  Result := S;
end;

procedure TNotificationManager.self_test(AManager: TTaskManager);
begin
  WriteLn('Testing Notification Manager...');
  Self.Clear;
  Self.ScanTasks(AManager);
  WriteLn(Self.GetSummary);
  WriteLn('Notification Manager test complete.');
end;

end.
