
program test_notifications;
{$mode objfpc}{$H+}
uses
  SysUtils, DateUtils, utaskmanager, unotificationmanager;

var
  Mgr: TTaskManager;
  Notify: TNotificationManager;
  EmptyTags: TTagArray;

begin
  SetLength(EmptyTags, 0);
  Mgr := TTaskManager.Create;
  Notify := TNotificationManager.Create;
  try
    // Add an overdue task
    Mgr.add_task('Urgent Bug', 'Fix it now', 'Work', 'Dev', EmptyTags, tp_critical, IncMinute(Now, -60));
    
    // Add a due soon task
    Mgr.add_task('Meeting Prep', 'Prepare slides', 'Work', 'Dev', EmptyTags, tp_high, IncHour(Now, 2));
    
    Notify.self_test(Mgr);
    
  finally
    Notify.Free;
    Mgr.Free;
  end;
end.
