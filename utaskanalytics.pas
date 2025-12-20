
unit utaskanalytics;

{$mode objfpc}
{$h+}

interface

uses
  SysUtils, DateUtils, Math, utaskmanager;

type
  ttaskhealth = (thHealthy, thAtRisk, thCritical, thOverdue);
  
  ttaskhealthrecord = record
    task_id: integer;
    title: string;
    health: ttaskhealth;
    reason: string;
    score: double;
  end;
  
  ttaskhealtharray = array of ttaskhealthrecord;

  ttaskanalytics = class
  public
    function get_task_health(const atask: ttask; amanager: ttaskmanager): ttaskhealthrecord;
    function analyze_project_health(amanager: ttaskmanager): ttaskhealtharray;
    function calculate_efficiency(amanager: ttaskmanager): double;
    function calculate_project_risk(amanager: ttaskmanager): double;
    function generate_health_report(amanager: ttaskmanager): string;
    procedure self_test(amanager: ttaskmanager);
  end;

implementation

function ttaskanalytics.get_task_health(const atask: ttask; amanager: ttaskmanager): ttaskhealthrecord;
var
  days_to_deadline: double;
begin
  result.task_id := atask.id;
  result.title := atask.title;
  result.score := 100.0;
  result.health := thHealthy;
  result.reason := 'On track';

  if atask.status = ts_done then
  begin
    result.score := 100.0;
    exit;
  end;

  days_to_deadline := DaySpan(atask.due_date, Now);
  
  if atask.due_date < Now then
  begin
    result.health := thOverdue;
    result.score := 0.0;
    result.reason := 'Deadline passed';
  end
  else if (days_to_deadline < 2) and (atask.status <> ts_done) then
  begin
    result.health := thCritical;
    result.score := 20.0;
    result.reason := 'Deadline very close';
  end
  else if (days_to_deadline < 7) and (atask.priority >= tp_high) and (atask.status = ts_todo) then
  begin
    result.health := thAtRisk;
    result.score := 50.0;
    result.reason := 'High priority task not started';
  end;
  
  if not amanager.are_dependencies_done(atask.id) then
  begin
    result.score := result.score * 0.8;
    result.reason := result.reason + ' (Blocked by dependencies)';
  end;
end;

function ttaskanalytics.analyze_project_health(amanager: ttaskmanager): ttaskhealtharray;
var
  tasks: ttaskarray;
  i: integer;
begin
  tasks := amanager.get_tasks;
  setlength(result, length(tasks));
  for i := 0 to high(tasks) do
    result[i] := get_task_health(tasks[i], amanager);
end;

function ttaskanalytics.calculate_efficiency(amanager: ttaskmanager): double;
var
  tasks: ttaskarray;
  i, completed_count: integer;
begin
  tasks := amanager.get_tasks;
  if length(tasks) = 0 then exit(0);
  
  completed_count := 0;
  for i := 0 to high(tasks) do
    if tasks[i].status = ts_done then
      inc(completed_count);
  
  result := (completed_count / length(tasks)) * 100;
end;

function ttaskanalytics.calculate_project_risk(amanager: ttaskmanager): double;
var
  tasks: ttaskarray;
  i: integer;
  risk_sum: double;
begin
  tasks := amanager.get_tasks;
  if length(tasks) = 0 then exit(0);
  
  risk_sum := 0;
  for i := 0 to high(tasks) do
  begin
    if tasks[i].status <> ts_done then
    begin
      if tasks[i].priority = tp_critical then risk_sum := risk_sum + 30
      else if tasks[i].priority = tp_high then risk_sum := risk_sum + 20;
      
      if tasks[i].due_date < Now + 3 then risk_sum := risk_sum + 50;
    end;
  end;
  
  result := Min(100.0, risk_sum / length(tasks));
end;

function ttaskanalytics.generate_health_report(amanager: ttaskmanager): string;
var
  healths: ttaskhealtharray;
  i: integer;
  s: string;
begin
  healths := analyze_project_health(amanager);
  s := '=== PROJECT HEALTH REPORT ===' + sLineBreak;
  s := s + 'Overall Efficiency: ' + FloatToStrF(calculate_efficiency(amanager), ffFixed, 8, 2) + '%' + sLineBreak;
  s := s + 'Project Risk Index: ' + FloatToStrF(calculate_project_risk(amanager), ffFixed, 8, 2) + sLineBreak;
  s := s + '----------------------------' + sLineBreak;
  for i := 0 to high(healths) do
  begin
    if healths[i].health <> thHealthy then
    begin
      s := s + Format('Task #%d [%s]: %s (Score: %.1f)', [healths[i].task_id, healths[i].title, healths[i].reason, healths[i].score]) + sLineBreak;
    end;
  end;
  result := s;
end;

procedure ttaskanalytics.self_test(amanager: ttaskmanager);
begin
  writeln('Running ttaskanalytics self_test...');
  if generate_health_report(amanager) = '' then
    writeln('Analytics test failed: Empty report')
  else
    writeln('Analytics test passed.');
end;

end.
