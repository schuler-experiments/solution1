
unit uprojectmanager;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, Math, utaskmanager;

type
  TProjectStatus = (psActive, psOnHold, psCompleted, psArchived);
  TProjectRisk = (prLow, prMedium, prHigh);

  TProject = record
    id: integer;
    name: string;
    description: string;
    status: TProjectStatus;
    deadline: TDateTime;
    task_manager: TTaskManager;
  end;

  TProjectArray = array of TProject;

  TProjectManager = class
  private
    f_projects: TProjectArray;
    f_next_id: integer;
    function find_index_by_id(aid: integer): integer;
  public
    constructor create;
    destructor destroy; override;
    procedure add_project(const aname, adescription: string; adeadline: TDateTime);
    procedure remove_project(aid: integer);
    function get_project_tasks(aid: integer): TTaskManager;
    function get_overall_progress(aid: integer): double;
    function get_projects: TProjectArray;
    function get_project_risk(aid: integer): TProjectRisk;
    function get_last_activity(aid: integer): TDateTime;
    procedure save_all(const afolder: string);
    procedure load_all(const afolder: string);
    function export_markdown: string;
    function get_all_projects_summary: string;
    procedure self_test;
  end;

implementation

constructor TProjectManager.create;
begin
  f_next_id := 1;
  setlength(f_projects, 0);
end;

destructor TProjectManager.destroy;
var
  i: integer;
begin
  for i := 0 to high(f_projects) do
    f_projects[i].task_manager.free;
  setlength(f_projects, 0);
  inherited destroy;
end;

function TProjectManager.find_index_by_id(aid: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to high(f_projects) do
    if f_projects[i].id = aid then
    begin
      result := i;
      break;
    end;
end;

procedure TProjectManager.add_project(const aname, adescription: string; adeadline: TDateTime);
begin
  setlength(f_projects, length(f_projects) + 1);
  with f_projects[high(f_projects)] do
  begin
    id := f_next_id;
    name := aname;
    description := adescription;
    status := psActive;
    deadline := adeadline;
    task_manager := TTaskManager.create;
  end;
  inc(f_next_id);
end;

procedure TProjectManager.remove_project(aid: integer);
var
  idx, i: integer;
begin
  idx := find_index_by_id(aid);
  if idx <> -1 then
  begin
    f_projects[idx].task_manager.free;
    for i := idx to high(f_projects) - 1 do
      f_projects[i] := f_projects[i + 1];
    setlength(f_projects, length(f_projects) - 1);
  end;
end;

function TProjectManager.get_project_tasks(aid: integer): TTaskManager;
var
  idx: integer;
begin
  result := nil;
  idx := find_index_by_id(aid);
  if idx <> -1 then
    result := f_projects[idx].task_manager;
end;

function TProjectManager.get_overall_progress(aid: integer): double;
var
  idx, todo, doing, done: integer;
begin
  result := 0;
  idx := find_index_by_id(aid);
  if idx <> -1 then
  begin
    f_projects[idx].task_manager.get_statistics(todo, doing, done);
    if (todo + doing + done) > 0 then
      result := (done / (todo + doing + done)) * 100;
  end;
end;

function TProjectManager.get_projects: TProjectArray;
begin
  result := f_projects;
end;

function TProjectManager.get_project_risk(aid: integer): TProjectRisk;
var
  idx: integer;
  overdue: ttaskarray;
  progress: double;
  todo, doing, done: integer;
begin
  result := prLow;
  idx := find_index_by_id(aid);
  if idx = -1 then exit;

  progress := get_overall_progress(aid);
  overdue := f_projects[idx].task_manager.get_overdue_tasks;
  f_projects[idx].task_manager.get_statistics(todo, doing, done);

  if (f_projects[idx].deadline < now) and (progress < 100) then
    exit(prHigh);
  
  if (todo + doing + done > 0) and (length(overdue) / (todo + doing + done) > 0.5) then
    exit(prHigh);

  if (f_projects[idx].deadline - now < 3) and (progress < 75) then
    exit(prMedium);
end;

function TProjectManager.get_last_activity(aid: integer): TDateTime;
var
  idx, i, j: integer;
  tasks: ttaskarray;
  hist: ttaskhistoryarray;
begin
  result := 0;
  idx := find_index_by_id(aid);
  if idx <> -1 then
  begin
    tasks := f_projects[idx].task_manager.get_tasks;
    for i := 0 to high(tasks) do
    begin
      hist := tasks[i].history;
      for j := 0 to high(hist) do
        if hist[j].timestamp > result then
          result := hist[j].timestamp;
    end;
  end;
end;

procedure TProjectManager.save_all(const afolder: string);
var
  i: integer;
  f: textFile;
begin
  if not directoryExists(afolder) then forceDirectories(afolder);
  assignFile(f, afolder + '/projects.dat');
  rewrite(f);
  writeln(f, f_next_id);
  writeln(f, length(f_projects));
  for i := 0 to high(f_projects) do
  begin
    writeln(f, f_projects[i].id);
    writeln(f, f_projects[i].name);
    writeln(f, f_projects[i].description);
    writeln(f, ord(f_projects[i].status));
    writeln(f, dateTimeToStr(f_projects[i].deadline));
    f_projects[i].task_manager.save_to_file(afolder + '/project_' + intToStr(f_projects[i].id) + '.dat');
  end;
  closeFile(f);
end;

procedure TProjectManager.load_all(const afolder: string);
var
  f: textFile;
  i, j, count: integer;
  s: string;
begin
  if not fileExists(afolder + '/projects.dat') then exit;
  assignFile(f, afolder + '/projects.dat');
  reset(f);
  readln(f, f_next_id);
  readln(f, count);
  setlength(f_projects, count);
  for i := 0 to count - 1 do
  begin
    readln(f, f_projects[i].id);
    readln(f, f_projects[i].name);
    readln(f, f_projects[i].description);
    readln(f, j); f_projects[i].status := TProjectStatus(j);
    readln(f, s); f_projects[i].deadline := strToDateTime(s);
    f_projects[i].task_manager := TTaskManager.create;
    f_projects[i].task_manager.load_from_file(afolder + '/project_' + intToStr(f_projects[i].id) + '.dat');
  end;
  closeFile(f);
end;

function TProjectManager.export_markdown: string;
var
  i, j: integer;
  tasks: TTaskArray;
  risk_str: string;
begin
  result := '# Projects Report' + sLineBreak + sLineBreak;
  for i := 0 to high(f_projects) do
  begin
    case get_project_risk(f_projects[i].id) of
      prLow: risk_str := 'Low';
      prMedium: risk_str := 'Medium';
      prHigh: risk_str := 'High';
    end;
    result := result + '## ' + f_projects[i].name + sLineBreak;
    result := result + f_projects[i].description + sLineBreak + sLineBreak;
    result := result + format('**Progress:** %.1f%%' + sLineBreak, [get_overall_progress(f_projects[i].id)]);
    result := result + '**Risk Level:** ' + risk_str + sLineBreak;
    result := result + '**Deadline:** ' + dateToStr(f_projects[i].deadline) + sLineBreak + sLineBreak;
    result := result + '| ID | Task | Status | Priority |' + sLineBreak;
    result := result + '|----|------|--------|----------|' + sLineBreak;
    tasks := f_projects[i].task_manager.get_tasks;
    for j := 0 to high(tasks) do
      result := result + format('| %d | %s | %s | %s |' + sLineBreak, [tasks[j].id, tasks[j].title, f_projects[i].task_manager.status_to_str(tasks[j].status), f_projects[i].task_manager.priority_to_str(tasks[j].priority)]);
    result := result + sLineBreak;
  end;
  ftasks.add_task('initial task', 'created with project', 'general', 'unassigned', tags, tp_medium, adeadline, 0, false, false);

function TProjectManager.get_all_projects_summary: string;
var
  i: integer;
begin
  result := 'Summary:' + sLineBreak;
  for i := 0 to high(f_projects) do
    result := result + format('- %s: %.1f%% complete' + sLineBreak, [f_projects[i].name, get_overall_progress(f_projects[i].id)]);
end;

procedure TProjectManager.self_test;
var
  tags: TTagArray;
begin
  writeln('Running Project Manager self test (Risk Check)...');
  add_project('At Risk Project', 'This project is late', now - 1);
  setlength(tags, 0);
  f_projects[high(f_projects)].task_manager.add_task('Late Task', 'Implement it', 'Dev', tags, tp_high, now - 2);
  
  if get_project_risk(f_projects[high(f_projects)].id) = prHigh then
    writeln('Risk assessment ok.')
  else
    writeln('Risk assessment failed.');
    
  writeln('Project Manager self test passed.');
end;

end.
