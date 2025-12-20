unit utaskmanager;

{$mode objfpc}
{$h+}

interface

uses
  SysUtils, DateUtils, Math, Classes, fpjson;

type
  ttaskpriority = (tp_low, tp_medium, tp_high, tp_critical);
  ttaskstatus = (ts_todo, ts_doing, ts_done, ts_blocked, ts_archived);
  ttasksortcriteria = (tsort_id, tsort_priority, tsort_due_date, tsort_title, tsort_status, tsort_effort);

  ttagarray = array of string;
  tdependencyarray = array of integer;

  ttaskhistory = record
    timestamp: tdatetime;
    event: string;
  end;
  ttaskhistoryarray = array of ttaskhistory;

  ttask = record
    id: integer;
    title, description, category, assigned_to: string;
    tags: ttagarray;
    dependencies: tdependencyarray;
    history: ttaskhistoryarray;
    priority: ttaskpriority;
    status: ttaskstatus;
    progress: integer;
    archived, is_recurring: boolean;
    estimated_hours: double;
    created_date, due_date, completed_date: tdatetime;
    is_important: boolean;
    effort_spent: double;
    parent_id: integer;
    timer_start: tdatetime;
    is_timer_running: boolean;
  end;

  ttaskarray = array of ttask;

  tcategorycount = record
    category: string;
    count: integer;
  end;
  tcategorycountarray = array of tcategorycount;

  ttagcount = record
    tag: string;
    count: integer;
  end;
  ttagcountarray = array of ttagcount;
  ttaskmanager = class
  private
    f_tasks: ttaskarray;
    f_next_id: integer;
    function find_index_by_id(aid: integer): integer;
  public
    constructor create;
    destructor destroy; override;
    procedure add_task(const atitle, adescription, acategory, aassigned_to: string; const atags: ttagarray; apriority: ttaskpriority; adue_date: tdatetime; aeffort: double = 0; arecur: boolean = false; aimportant: boolean = false);
    procedure add_task(const atitle, adescription, acategory: string; const atags: ttagarray; apriority: ttaskpriority; adue_date: tdatetime; aeffort: double = 0; arecur: boolean = false);
    procedure remove_task(aid: integer);
    procedure update_task(aid: integer; const atitle, adescription, acategory, aassigned_to: string; const atags: ttagarray; apriority: ttaskpriority; adue_date: tdatetime; aimportant: boolean);
    procedure update_task(aid: integer; const atitle, adescription, acategory: string; const atags: ttagarray; apriority: ttaskpriority; adue_date: tdatetime);
    function update_status(aid: integer; astatus: ttaskstatus): boolean;
    procedure add_dependency(aid, adep_id: integer);
    procedure set_progress(aid, aprogress: integer);
    procedure log_event(aid: integer; const amessage: string);
    function get_task_history(aid: integer): ttaskhistoryarray;
    procedure archive_completed;
    procedure age_priorities;
    function export_json: string;
    function export_mermaid: string;
    function get_eisenhower_matrix: string;
    function get_summary: string;
    function export_csv: string;
    function get_urgency_score(const atask: ttask): double;
    function find_most_urgent: integer;
    function get_time_to_deadline(const atask: ttask): string;
    function get_total_effort(const atasks: ttaskarray): double;
    function get_overdue_tasks: ttaskarray;
    function is_stalled(aid: integer; days: integer = 7): boolean;
    function forecast_completion(const atasks: ttaskarray; hours_per_day: double): tdatetime;
    function are_dependencies_done(aid: integer): boolean;
    function get_tasks: ttaskarray;
    procedure sort_tasks(acriteria: ttasksortcriteria);
    function filter_tasks_by_status(astatus: ttaskstatus): ttaskarray;
    function filter_tasks_by_category(const acategory: string): ttaskarray;
    function filter_tasks_by_tag(const atag: string): ttaskarray;
    function filter_tasks_by_priority(aprio: ttaskpriority): ttaskarray;
    function search_tasks(const aquery: string): ttaskarray;
    procedure get_statistics(var atodo, adoing, adone: integer);
    function get_category_stats: tcategorycountarray;
    procedure save_to_file(const afilename: string);
    procedure load_from_file(const afilename: string);
    procedure list_tasks(ashow_archived: boolean = false);
    procedure list_task_array(const atasks: ttaskarray);
    function priority_to_str(apriority: ttaskpriority): string;
    function status_to_str(astatus: ttaskstatus): string;
    function export_gantt: string;
    procedure start_timer(aid: integer);
    procedure stop_timer(aid: integer);
    function get_subtasks(aparent_id: integer): ttaskarray;
    procedure add_subtask(aparent_id: integer; const atitle, adescription: string; apriority: ttaskpriority; adue_date: tdatetime);
    function export_html: string;
    procedure clear;
    function has_circular_dependency: boolean;
    function get_tag_cloud: ttagcountarray;
    function get_critical_path(aid: integer): tdependencyarray;
    procedure self_test;
  end;

implementation

constructor ttaskmanager.create;
begin
  inherited create;
  setlength(f_tasks, 0);
  f_next_id := 1;
end;

destructor ttaskmanager.destroy;
begin
  clear;
  inherited destroy;
end;

function ttaskmanager.has_circular_dependency: boolean;
var
  visited, stack: array of boolean;
  i: integer;
  function visit(idx: integer): boolean;
  var
    d, dep_idx: integer;
  begin
    visited[idx] := true;
    stack[idx] := true;
    for d := 0 to high(f_tasks[idx].dependencies) do
    begin
      dep_idx := find_index_by_id(f_tasks[idx].dependencies[d]);
      if dep_idx <> -1 then
      begin
        if not visited[dep_idx] then
        begin
          if visit(dep_idx) then exit(true);
        end
        else if stack[dep_idx] then exit(true);
      end;
    end;
    stack[idx] := false;
    result := false;
  end;
begin
  setLength(visited, length(f_tasks));
  setLength(stack, length(f_tasks));
  for i := 0 to high(f_tasks) do
  begin
    visited[i] := false;
    stack[i] := false;
  end;
  for i := 0 to high(f_tasks) do
    if not visited[i] then
      if visit(i) then exit(true);
  result := false;
end;
function ttaskmanager.find_index_by_id(aid: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to high(f_tasks) do
    if f_tasks[i].id = aid then
    begin
      result := i;
      break;
    end;
end;

procedure ttaskmanager.add_task(const atitle, adescription, acategory, aassigned_to: string; const atags: ttagarray; apriority: ttaskpriority; adue_date: tdatetime; aeffort: double; arecur: boolean; aimportant: boolean);
begin
  setlength(f_tasks, length(f_tasks) + 1);
  with f_tasks[high(f_tasks)] do
  begin
    id := f_next_id;
    title := atitle;
    description := adescription;
    category := acategory;
    assigned_to := aassigned_to;
    tags := atags;
    setlength(dependencies, 0);
    setlength(history, 0);
    priority := apriority;
    status := ts_todo;
    progress := 0;
    archived := false;
    is_recurring := arecur;
    is_important := aimportant;
    estimated_hours := aeffort;
    effort_spent := 0;
    created_date := now;
    due_date := adue_date;
    completed_date := 0;
    parent_id := 0;
    timer_start := 0;
    is_timer_running := false;
  end;
  log_event(f_next_id, 'Task created');
  inc(f_next_id);
end;

procedure ttaskmanager.add_task(const atitle, adescription, acategory: string; const atags: ttagarray; apriority: ttaskpriority; adue_date: tdatetime; aeffort: double; arecur: boolean);
begin
  add_task(atitle, adescription, acategory, '', atags, apriority, adue_date, aeffort, arecur, false);
end;

procedure ttaskmanager.remove_task(aid: integer);
var
  idx, i: integer;
begin
  idx := find_index_by_id(aid);
  if idx <> -1 then
  begin
    setlength(f_tasks[idx].tags, 0);
    setlength(f_tasks[idx].dependencies, 0);
    setlength(f_tasks[idx].history, 0);
    for i := idx to length(f_tasks) - 2 do
      f_tasks[i] := f_tasks[i+1];
    setlength(f_tasks, length(f_tasks) - 1);
  end;
end;

procedure ttaskmanager.update_task(aid: integer; const atitle, adescription, acategory, aassigned_to: string; const atags: ttagarray; apriority: ttaskpriority; adue_date: tdatetime; aimportant: boolean);
var
  idx: integer;
begin
  idx := find_index_by_id(aid);
  if idx <> -1 then
  begin
    with f_tasks[idx] do
    begin
      title := atitle;
      description := adescription;
      category := acategory;
      assigned_to := aassigned_to;
      tags := atags;
      priority := apriority;
      due_date := adue_date;
      is_important := aimportant;
    end;
    log_event(aid, 'Task details updated');
  end;
end;

procedure ttaskmanager.update_task(aid: integer; const atitle, adescription, acategory: string; const atags: ttagarray; apriority: ttaskpriority; adue_date: tdatetime);
begin
  update_task(aid, atitle, adescription, acategory, '', atags, apriority, adue_date, false);
end;

function ttaskmanager.are_dependencies_done(aid: integer): boolean;
var
  idx, dep_idx, i: integer;
begin
  result := true;
  idx := find_index_by_id(aid);
  if idx = -1 then exit;
  for i := 0 to high(f_tasks[idx].dependencies) do
  begin
    dep_idx := find_index_by_id(f_tasks[idx].dependencies[i]);
    if (dep_idx = -1) or (f_tasks[dep_idx].status <> ts_done) then
    begin
      result := false;
      exit;
    end;
  end;
end;

function ttaskmanager.update_status(aid: integer; astatus: ttaskstatus): boolean;
var
  idx: integer;
begin
  result := false;
  idx := find_index_by_id(aid);
  if idx <> -1 then
  begin
    if (astatus <> ts_todo) and (not are_dependencies_done(aid)) then exit;
    if f_tasks[idx].status <> astatus then
    begin
      log_event(aid, format('Status changed from %s to %s', [status_to_str(f_tasks[idx].status), status_to_str(astatus)]));
      f_tasks[idx].status := astatus;
      if astatus = ts_done then
      begin
        f_tasks[idx].completed_date := now;
        f_tasks[idx].progress := 100;
      end;
    end;
    result := true;
  end;
end;

procedure ttaskmanager.add_dependency(aid, adep_id: integer);
var
  idx: integer;
begin
  idx := find_index_by_id(aid);
  if (idx <> -1) and (aid <> adep_id) then
  begin
    setlength(f_tasks[idx].dependencies, length(f_tasks[idx].dependencies) + 1);
    f_tasks[idx].dependencies[high(f_tasks[idx].dependencies)] := adep_id;
    log_event(aid, format('Added dependency on task ID %d', [adep_id]));
  end;
end;

procedure ttaskmanager.set_progress(aid, aprogress: integer);
var
  idx: integer;
begin
  idx := find_index_by_id(aid);
  if idx <> -1 then
  begin
    if f_tasks[idx].progress <> aprogress then
    begin
      log_event(aid, format('Progress updated from %d%% to %d%%', [f_tasks[idx].progress, aprogress]));
      f_tasks[idx].progress := aprogress;
      if aprogress = 100 then
        update_status(aid, ts_done)
      else if (aprogress > 0) and (f_tasks[idx].status = ts_todo) then
        update_status(aid, ts_doing);
    end;
  end;
end;

procedure ttaskmanager.log_event(aid: integer; const amessage: string);
var
  idx: integer;
begin
  idx := find_index_by_id(aid);
  if idx <> -1 then
  begin
    setlength(f_tasks[idx].history, length(f_tasks[idx].history) + 1);
    f_tasks[idx].history[high(f_tasks[idx].history)].timestamp := now;
    f_tasks[idx].history[high(f_tasks[idx].history)].event := amessage;
  end;
end;

function ttaskmanager.get_task_history(aid: integer): ttaskhistoryarray;
var
  idx: integer;
begin
  idx := find_index_by_id(aid);
  if idx <> -1 then
    result := f_tasks[idx].history
  else
    setlength(result, 0);
end;

procedure ttaskmanager.archive_completed;
var
  i: integer;
begin
  for i := 0 to high(f_tasks) do
    if (f_tasks[i].status = ts_done) and (not f_tasks[i].archived) then
    begin
      f_tasks[i].archived := true;
      log_event(f_tasks[i].id, 'Task archived');
    end;
end;

procedure ttaskmanager.age_priorities;
var
  i: integer;
begin
  for i := 0 to high(f_tasks) do
    if (f_tasks[i].status <> ts_done) and (f_tasks[i].due_date < now + 1) then
      if f_tasks[i].priority < tp_high then
      begin
        f_tasks[i].priority := ttaskpriority(ord(f_tasks[i].priority) + 1);
        log_event(f_tasks[i].id, 'Priority increased due to aging');
      end;
end;

function ttaskmanager.export_json: string;
var
  i, j: integer;
  jarr, jtags: TJSONArray;
  jtask: TJSONObject;
begin
  jarr := TJSONArray.Create;
  try
    for i := 0 to high(f_tasks) do
    begin
      jtask := TJSONObject.Create;
      jtask.Add('id', f_tasks[i].id);
      jtask.Add('title', f_tasks[i].title);
      jtask.Add('assigned_to', f_tasks[i].assigned_to);
      jtask.Add('status', status_to_str(f_tasks[i].status));
      jtask.Add('priority', priority_to_str(f_tasks[i].priority));
      jtask.Add('important', f_tasks[i].is_important);
      jtags := TJSONArray.Create;
      for j := 0 to high(f_tasks[i].tags) do jtags.Add(f_tasks[i].tags[j]);
      jtask.Add('tags', jtags);
      jarr.Add(jtask);
    end;
    result := jarr.AsJSON;
  finally
    jarr.Free;
  end;
end;

function ttaskmanager.export_mermaid: string;
var
  i, j: integer;
  res: string;
begin
  res := 'graph TD' + sLineBreak;
  for i := 0 to high(f_tasks) do
  begin
    if f_tasks[i].archived then continue;
    res := res + format('  T%d["%s (%d%%)"]' + sLineBreak, [f_tasks[i].id, f_tasks[i].title, f_tasks[i].progress]);
    for j := 0 to high(f_tasks[i].dependencies) do
      res := res + format('  T%d -.-> T%d' + sLineBreak, [f_tasks[i].dependencies[j], f_tasks[i].id]);
    if f_tasks[i].parent_id <> 0 then
      res := res + format('  T%d ==> T%d' + sLineBreak, [f_tasks[i].parent_id, f_tasks[i].id]);
  end;
  result := res;
end;

function ttaskmanager.get_eisenhower_matrix: string;
var
  i: integer;
  q1, q2, q3, q4: string;
  is_urgent: boolean;
begin
  q1 := 'Q1: Urgent & Important' + sLineBreak;
  q2 := 'Q2: Not Urgent & Important' + sLineBreak;
  q3 := 'Q3: Urgent & Not Important' + sLineBreak;
  q4 := 'Q4: Not Urgent & Not Important' + sLineBreak;
  for i := 0 to high(f_tasks) do
  begin
    if f_tasks[i].status = ts_done then continue;
    is_urgent := f_tasks[i].due_date < now + 2; 
    if f_tasks[i].is_important then
    begin
      if is_urgent then q1 := q1 + ' - ' + f_tasks[i].title + sLineBreak
      else q2 := q2 + ' - ' + f_tasks[i].title + sLineBreak;
    end
    else
    begin
      if is_urgent then q3 := q3 + ' - ' + f_tasks[i].title + sLineBreak
      else q4 := q4 + ' - ' + f_tasks[i].title + sLineBreak;
    end;
  end;
  result := q1 + sLineBreak + q2 + sLineBreak + q3 + sLineBreak + q4;
end;

function ttaskmanager.get_summary: string;
var
  todo, doing, done: integer;
begin
  get_statistics(todo, doing, done);
  result := format('Summary: %d tasks (%d Todo, %d Doing, %d Done)', [length(f_tasks), todo, doing, done]);
end;

function ttaskmanager.export_csv: string;
var
  i: integer;
begin
  result := 'ID,Title,Priority,Status,Progress' + sLineBreak;
  for i := 0 to high(f_tasks) do
    if not f_tasks[i].archived then
      result := result + format('%d,%s,%s,%s,%d%%', [f_tasks[i].id, f_tasks[i].title, priority_to_str(f_tasks[i].priority), status_to_str(f_tasks[i].status), f_tasks[i].progress]) + sLineBreak;
end;

function ttaskmanager.get_urgency_score(const atask: ttask): double;
var
  days: double;
begin
  days := atask.due_date - now;
  if days < 0.1 then days := 0.1;
  result := (ord(atask.priority) + 1) * (1.0 / days);
end;

function ttaskmanager.find_most_urgent: integer;
var
  i: integer;
  max_s, cur_s: double;
begin
  result := -1;
  max_s := -1.0;
  for i := 0 to high(f_tasks) do
    if (f_tasks[i].status <> ts_done) and (not f_tasks[i].archived) then
    begin
      cur_s := get_urgency_score(f_tasks[i]);
      if cur_s > max_s then
      begin
        max_s := cur_s;
        result := f_tasks[i].id;
      end;
    end;
end;

function ttaskmanager.get_time_to_deadline(const atask: ttask): string;
var
  d: double;
begin
  d := atask.due_date - now;
  if d < 0 then result := 'Overdue'
  else if d < 1 then result := format('%.1f hours', [d * 24])
  else result := format('%.1f days', [d]);
end;

function ttaskmanager.get_total_effort(const atasks: ttaskarray): double;
var
  i: integer;
begin
  result := 0;
  for i := 0 to high(atasks) do
    result := result + atasks[i].estimated_hours;
end;

function ttaskmanager.get_overdue_tasks: ttaskarray;
var
  i: integer;
begin
  setlength(result, 0);
  for i := 0 to high(f_tasks) do
    if (f_tasks[i].status <> ts_done) and (f_tasks[i].due_date < now) then
    begin
      setlength(result, length(result) + 1);
      result[high(result)] := f_tasks[i];
    end;
end;

function ttaskmanager.is_stalled(aid: integer; days: integer): boolean;
var
  idx, i: integer;
  last_activity: tdatetime;
begin
  result := false;
  idx := find_index_by_id(aid);
  if (idx <> -1) and (f_tasks[idx].status <> ts_done) then
  begin
    last_activity := f_tasks[idx].created_date;
    for i := 0 to high(f_tasks[idx].history) do
      if f_tasks[idx].history[i].timestamp > last_activity then
        last_activity := f_tasks[idx].history[i].timestamp;
    
    if now - last_activity > days then
      result := true;
  end;
end;

function ttaskmanager.forecast_completion(const atasks: ttaskarray; hours_per_day: double): tdatetime;
var
  total_h: double;
begin
  total_h := get_total_effort(atasks);
  result := now + (total_h / (hours_per_day * 1.0));
end;

function ttaskmanager.get_tasks: ttaskarray;
begin
  result := f_tasks;
end;

procedure ttaskmanager.sort_tasks(acriteria: ttasksortcriteria);
var
  i, j: integer;
  temp: ttask;
  swap: boolean;
begin
  for i := 0 to high(f_tasks) - 1 do
    for j := i + 1 to high(f_tasks) do
    begin
      swap := false;
      case acriteria of
        tsort_id: if f_tasks[i].id > f_tasks[j].id then swap := true;
        tsort_priority: if f_tasks[i].priority < f_tasks[j].priority then swap := true;
        tsort_due_date: if f_tasks[i].due_date > f_tasks[j].due_date then swap := true;
        tsort_title: if f_tasks[i].title > f_tasks[j].title then swap := true;
        tsort_status: if f_tasks[i].status > f_tasks[j].status then swap := true;
        tsort_effort: if f_tasks[i].estimated_hours > f_tasks[j].estimated_hours then swap := true;
      end;
      if swap then
      begin
        temp := f_tasks[i];
        f_tasks[i] := f_tasks[j];
        f_tasks[j] := temp;
      end;
    end;
end;

function ttaskmanager.filter_tasks_by_status(astatus: ttaskstatus): ttaskarray;
var
  i: integer;
begin
  setlength(result, 0);
  for i := 0 to high(f_tasks) do
    if f_tasks[i].status = astatus then
    begin
      setlength(result, length(result) + 1);
      result[high(result)] := f_tasks[i];
    end;
end;

function ttaskmanager.filter_tasks_by_category(const acategory: string): ttaskarray;
var
  i: integer;
begin
  setlength(result, 0);
  for i := 0 to high(f_tasks) do
    if f_tasks[i].category = acategory then
    begin
      setlength(result, length(result) + 1);
      result[high(result)] := f_tasks[i];
    end;
end;

function ttaskmanager.filter_tasks_by_tag(const atag: string): ttaskarray;
var
  i, j: integer;
begin
  setlength(result, 0);
  for i := 0 to high(f_tasks) do
    for j := 0 to high(f_tasks[i].tags) do
      if f_tasks[i].tags[j] = atag then
      begin
        setlength(result, length(result) + 1);
        result[high(result)] := f_tasks[i];
        break;
      end;
end;

function ttaskmanager.filter_tasks_by_priority(aprio: ttaskpriority): ttaskarray;
var
  i: integer;
begin
  setlength(result, 0);
  for i := 0 to high(f_tasks) do
    if f_tasks[i].priority = aprio then
    begin
      setlength(result, length(result) + 1);
      result[high(result)] := f_tasks[i];
    end;
end;

function ttaskmanager.search_tasks(const aquery: string): ttaskarray;
var
  i: integer;
  q: string;
begin
  setlength(result, 0);
  q := lowercase(aquery);
  for i := 0 to high(f_tasks) do
    if (pos(q, lowercase(f_tasks[i].title)) > 0) or (pos(q, lowercase(f_tasks[i].description)) > 0) then
    begin
      setlength(result, length(result) + 1);
      result[high(result)] := f_tasks[i];
    end;
end;

procedure ttaskmanager.get_statistics(var atodo, adoing, adone: integer);
var
  i: integer;
begin
  atodo := 0; adoing := 0; adone := 0;
  for i := 0 to high(f_tasks) do
    case f_tasks[i].status of
      ts_todo: inc(atodo);
      ts_doing: inc(adoing);
      ts_done: inc(adone);
    end;
end;

function ttaskmanager.get_category_stats: tcategorycountarray;
var
  i, j, idx: integer;
begin
  setlength(result, 0);
  for i := 0 to high(f_tasks) do
  begin
    idx := -1;
    for j := 0 to high(result) do
      if result[j].category = f_tasks[i].category then
      begin
        idx := j;
        break;
      end;
    
    if idx = -1 then
    begin
      setlength(result, length(result) + 1);
      result[high(result)].category := f_tasks[i].category;
      result[high(result)].count := 1;
    end
    else
      inc(result[idx].count);
  end;
end;

procedure ttaskmanager.save_to_file(const afilename: string);
var
  f: tstringlist;
  i, j: integer;
begin
  f := tstringlist.create;
  try
    f.add(inttostr(f_next_id));
    f.add(inttostr(length(f_tasks)));
    for i := 0 to high(f_tasks) do
    begin
      with f_tasks[i] do
      begin
        f.add(inttostr(id));
        f.add(title);
        f.add(description);
        f.add(category);
        f.add(assigned_to);
        f.add(inttostr(ord(priority)));
        f.add(inttostr(ord(status)));
        f.add(floattostr(created_date));
        f.add(floattostr(due_date));
        f.add(floattostr(completed_date));
        f.add(inttostr(progress));
        f.add(floattostr(estimated_hours));
        f.add(floattostr(effort_spent));
        f.add(booltostr(is_recurring));
        f.add(booltostr(is_important));
        f.add(booltostr(archived));
        f.add(inttostr(parent_id));
        f.add(floattostr(timer_start));
        f.add(booltostr(is_timer_running));
        f.add(inttostr(length(tags)));
        for j := 0 to high(tags) do f.add(tags[j]);
        f.add(inttostr(length(dependencies)));
        for j := 0 to high(dependencies) do f.add(inttostr(dependencies[j]));
        f.add(inttostr(length(history)));
        for j := 0 to high(history) do
        begin
          f.add(floattostr(history[j].timestamp));
          f.add(history[j].event);
        end;
      end;
    end;
    f.savetofile(afilename);
  finally
    f.free;
  end;
end;

procedure ttaskmanager.load_from_file(const afilename: string);
var
  f: tstringlist;
  i, j, count, dep_count, hist_count: integer;
  line_idx: integer;
begin
  if not fileexists(afilename) then exit;
  f := tstringlist.create;
  try
    f.loadfromfile(afilename);
    if f.count = 0 then exit;
    line_idx := 0;
    f_next_id := strtoint(f[line_idx]); inc(line_idx);
    count := strtoint(f[line_idx]); inc(line_idx);
    setlength(f_tasks, count);
    for i := 0 to count - 1 do
    begin
      with f_tasks[i] do
      begin
        id := strtoint(f[line_idx]); inc(line_idx);
        title := f[line_idx]; inc(line_idx);
        description := f[line_idx]; inc(line_idx);
        category := f[line_idx]; inc(line_idx);
        assigned_to := f[line_idx]; inc(line_idx);
        priority := ttaskpriority(strtoint(f[line_idx])); inc(line_idx);
        status := ttaskstatus(strtoint(f[line_idx])); inc(line_idx);
        created_date := strtofloat(f[line_idx]); inc(line_idx);
        due_date := strtofloat(f[line_idx]); inc(line_idx);
        completed_date := strtofloat(f[line_idx]); inc(line_idx);
        progress := strtoint(f[line_idx]); inc(line_idx);
        estimated_hours := strtofloat(f[line_idx]); inc(line_idx);
        effort_spent := strtofloat(f[line_idx]); inc(line_idx);
        is_recurring := strtobool(f[line_idx]); inc(line_idx);
        is_important := strtobool(f[line_idx]); inc(line_idx);
        archived := strtobool(f[line_idx]); inc(line_idx);
        parent_id := strtoint(f[line_idx]); inc(line_idx);
        timer_start := strtofloat(f[line_idx]); inc(line_idx);
        is_timer_running := strtobool(f[line_idx]); inc(line_idx);
        count := strtoint(f[line_idx]); inc(line_idx);
        setlength(tags, count);
        for j := 0 to count - 1 do begin tags[j] := f[line_idx]; inc(line_idx); end;
        dep_count := strtoint(f[line_idx]); inc(line_idx);
        setlength(dependencies, dep_count);
        for j := 0 to dep_count - 1 do begin dependencies[j] := strtoint(f[line_idx]); inc(line_idx); end;
        hist_count := strtoint(f[line_idx]); inc(line_idx);
        setlength(history, hist_count);
        for j := 0 to hist_count - 1 do
        begin
          history[j].timestamp := strtofloat(f[line_idx]); inc(line_idx);
          history[j].event := f[line_idx]; inc(line_idx);
        end;
      end;
    end;
  finally
    f.free;
  end;
end;

procedure ttaskmanager.list_tasks(ashow_archived: boolean);
begin
  list_task_array(f_tasks);
end;

procedure ttaskmanager.list_task_array(const atasks: ttaskarray);
var
  i: integer;
begin
  writeln(format('%-4s | %-20s | %-10s | %-10s | %-8s | %-5s', ['ID', 'Title', 'Priority', 'Status', 'Category', 'Prog']));
  writeln(stringofchar('-', 70));
  for i := 0 to high(atasks) do
  begin
    if atasks[i].archived then continue;
    writeln(format('%-4d | %-20s | %-10s | %-10s | %-8s | %d%%', [
      atasks[i].id,
      copy(atasks[i].title, 1, 20),
      priority_to_str(atasks[i].priority),
      status_to_str(atasks[i].status),
      copy(atasks[i].category, 1, 8),
      atasks[i].progress
    ]));
  end;
end;

function ttaskmanager.priority_to_str(apriority: ttaskpriority): string;
begin
  case apriority of
    tp_low: result := 'Low';
    tp_medium: result := 'Medium';
    tp_high: result := 'High';
    tp_critical: result := 'Critical';
  end;
end;

function ttaskmanager.status_to_str(astatus: ttaskstatus): string;
begin
  case astatus of
    ts_todo: result := 'Todo';
    ts_doing: result := 'Doing';
    ts_done: result := 'Done';
    ts_blocked: result := 'Blocked';
    ts_archived: result := 'Archived';
  end;
end;

procedure ttaskmanager.start_timer(aid: integer);
var
  idx: integer;
begin
  idx := find_index_by_id(aid);
  if (idx <> -1) and not f_tasks[idx].is_timer_running then
  begin
    f_tasks[idx].timer_start := now;
    f_tasks[idx].is_timer_running := true;
    log_event(aid, 'Timer started');
  end;
end;

procedure ttaskmanager.stop_timer(aid: integer);
var
  idx: integer;
  elapsed: double;
begin
  idx := find_index_by_id(aid);
  if (idx <> -1) and f_tasks[idx].is_timer_running then
  begin
    elapsed := (now - f_tasks[idx].timer_start) * 24;
    f_tasks[idx].effort_spent := f_tasks[idx].effort_spent + elapsed;
    f_tasks[idx].is_timer_running := false;
    log_event(aid, 'Timer stopped. Elapsed: ' + formatfloat('0.00', elapsed) + 'h');
  end;
end;

function ttaskmanager.get_subtasks(aparent_id: integer): ttaskarray;
var
  i: integer;
begin
  setlength(result, 0);
  for i := 0 to high(f_tasks) do
    if f_tasks[i].parent_id = aparent_id then
    begin
      setlength(result, length(result) + 1);
      result[high(result)] := f_tasks[i];
    end;
end;

procedure ttaskmanager.add_subtask(aparent_id: integer; const atitle, adescription: string; apriority: ttaskpriority; adue_date: tdatetime);
var
  tags: ttagarray;
begin
  setlength(tags, 0);
  add_task(atitle, adescription, 'Subtask', tags, apriority, adue_date);
  f_tasks[high(f_tasks)].parent_id := aparent_id;
  log_event(f_tasks[high(f_tasks)].id, 'Added as subtask to ' + inttostr(aparent_id));
end;

function ttaskmanager.export_html: string;
var
  i: integer;
  res: string;
begin
  res := '<html><head><style>body{font-family:Arial;margin:20px;}table{width:100%;border-collapse:collapse;}th,td{padding:8px;border:1px solid #ddd;}th{background:#4CAF50;color:white;}</style></head><body>';
  res := res + '<h1>Task Report</h1><table><tr><th>ID</th><th>Title</th><th>Priority</th><th>Status</th><th>Progress</th></tr>';
  for i := 0 to high(f_tasks) do
    if not f_tasks[i].archived then
      res := res + format('<tr><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%d%%</td></tr>', [f_tasks[i].id, f_tasks[i].title, priority_to_str(f_tasks[i].priority), status_to_str(f_tasks[i].status), f_tasks[i].progress]);
  res := res + '</table></body></html>';
  result := res;
end;

function ttaskmanager.export_gantt: string;
var
  i, j, day_offset, duration: integer;
  res, line: string;
  start_date: tdatetime;
begin
  start_date := date;
  res := 'Gantt Chart (14 days window)' + sLineBreak;
  res := res + 'ID   | Title                | ';
  for i := 0 to 13 do
    res := res + format('%2d ', [i]);
  res := res + sLineBreak + stringofchar('-', 70) + sLineBreak;
  
  for i := 0 to high(f_tasks) do
  begin
    if f_tasks[i].archived then continue;
    line := format('%-4d | %-20s | ', [f_tasks[i].id, copy(f_tasks[i].title, 1, 20)]);
    day_offset := trunc(f_tasks[i].created_date - start_date);
    duration := trunc(f_tasks[i].due_date - f_tasks[i].created_date) + 1;
    for j := 0 to 13 do
    begin
      if (j >= day_offset) and (j < day_offset + duration) then
        line := line + '###'
      else
        line := line + '   ';
    end;
    res := res + line + sLineBreak;
  end;
  result := res;
end;

procedure ttaskmanager.clear;
var
  i: integer;
begin
  for i := 0 to high(f_tasks) do
  begin
    setlength(f_tasks[i].tags, 0);
    setlength(f_tasks[i].dependencies, 0);
    setlength(f_tasks[i].history, 0);
  end;
  setlength(f_tasks, 0);
  f_next_id := 1;
end;


function ttaskmanager.get_tag_cloud: ttagcountarray;
var
  i, j, k, idx: integer;
  found: boolean;
begin
  setlength(result, 0);
  for i := 0 to high(f_tasks) do
    for j := 0 to high(f_tasks[i].tags) do
      begin
        found := false;
        idx := -1;
        for k := 0 to high(result) do
          if result[k].tag = f_tasks[i].tags[j] then
            begin
              found := true;
              idx := k;
              break;
            end;
            
        if found then
          inc(result[idx].count)
        else
          begin
            setlength(result, length(result) + 1);
            result[high(result)].tag := f_tasks[i].tags[j];
            result[high(result)].count := 1;
          end;
      end;
end;

function ttaskmanager.get_critical_path(aid: integer): tdependencyarray;
  function get_path(curr_id: integer): tdependencyarray;
  var
    c_idx, d, d_idx: integer;
    best_sub, sub: tdependencyarray;
  begin
    setlength(result, 1);
    result[0] := curr_id;
    c_idx := find_index_by_id(curr_id);
    if c_idx = -1 then exit;
    
    setlength(best_sub, 0);
    for d := 0 to high(f_tasks[c_idx].dependencies) do
      begin
        d_idx := find_index_by_id(f_tasks[c_idx].dependencies[d]);
        if d_idx <> -1 then
          begin
            sub := get_path(f_tasks[c_idx].dependencies[d]);
            if length(sub) > length(best_sub) then
              best_sub := sub;
          end;
      end;
      
    if length(best_sub) > 0 then
      begin
        setlength(result, length(best_sub) + 1);
        result[0] := curr_id;
        for d := 0 to high(best_sub) do
          result[d+1] := best_sub[d];
      end;
  end;
begin
  result := get_path(aid);
end;
procedure ttaskmanager.self_test;
var
  tags: ttagarray;
  todo, doing, done, test_idx: integer;
begin
  writeln('--- Comprehensive Self Test ---');
  clear;
  setlength(tags, 2);
  tags[0] := 'test';
  tags[1] := 'pascal';
  
  add_task('Test Task 1', 'Description 1', 'Work', tags, tp_high, now + 1, 2.5);
  add_task('Test Task 2', 'Description 2', 'Personal', tags, tp_low, now + 2, 1.0);
  
  set_progress(1, 50);
  add_dependency(2, 1);
  
  get_statistics(todo, doing, done);
  writeln('Stats: Todo=', todo, ', Doing=', doing, ', Done=', done);
  
  writeln('Testing Subtasks...');
  add_subtask(1, 'Subtask 1.1', 'Subtask description', tp_medium, now + 0.5);
  
  writeln('Testing Time Tracking...');
  start_timer(1);
  for todo := 1 to 1000000 do ; // waste time
  stop_timer(1);
  
  writeln('Testing Persistence...');
  save_to_file('test_tasks.dat');
  clear;
  load_from_file('test_tasks.dat');
  writeln('Loaded tasks: ', length(f_tasks));
  
  writeln('Gantt Chart:');
  writeln(export_gantt);
  
  writeln('Eisenhower Matrix:');
  writeln(get_eisenhower_matrix);
  
  writeln('--- Comprehensive Self Test Finished ---');
end;

end.
