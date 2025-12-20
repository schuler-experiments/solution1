
unit utaskbulk;

{$mode objfpc}
{$h+}

interface

uses
  sysutils, utaskmanager;

type
  ttaskbulkops = class
  public
    function replace_category(amanager: ttaskmanager; const aold_cat, anew_cat: string): integer;
    function replace_tag(amanager: ttaskmanager; const aold_tag, anew_tag: string): integer;
    function set_priority_by_category(amanager: ttaskmanager; const acategory: string; anew_prio: ttaskpriority): integer;
    procedure self_test(amanager: ttaskmanager);
  end;

implementation

function ttaskbulkops.replace_category(amanager: ttaskmanager; const aold_cat, anew_cat: string): integer;
var
  tasks: ttaskarray;
  i: integer;
begin
  result := 0;
  tasks := amanager.get_tasks;
  for i := 0 to high(tasks) do
  begin
    if tasks[i].category = aold_cat then
    begin
      amanager.update_task(tasks[i].id, tasks[i].title, tasks[i].description, anew_cat, 
                          tasks[i].assigned_to, tasks[i].tags, tasks[i].priority, 
                          tasks[i].due_date, tasks[i].is_important);
      inc(result);
    end;
  end;
end;

function ttaskbulkops.replace_tag(amanager: ttaskmanager; const aold_tag, anew_tag: string): integer;
var
  tasks: ttaskarray;
  i, j: integer;
  tags: ttagarray;
  changed: boolean;
begin
  result := 0;
  tasks := amanager.get_tasks;
  for i := 0 to high(tasks) do
  begin
    changed := false;
    tags := tasks[i].tags;
    for j := 0 to high(tags) do
    begin
      if tags[j] = aold_tag then
      begin
        tags[j] := anew_tag;
        changed := true;
      end;
    end;
    if changed then
    begin
      amanager.update_task(tasks[i].id, tasks[i].title, tasks[i].description, tasks[i].category, 
                          tasks[i].assigned_to, tags, tasks[i].priority, 
                          tasks[i].due_date, tasks[i].is_important);
      inc(result);
    end;
  end;
end;

function ttaskbulkops.set_priority_by_category(amanager: ttaskmanager; const acategory: string; anew_prio: ttaskpriority): integer;
var
  tasks: ttaskarray;
  i: integer;
begin
  result := 0;
  tasks := amanager.get_tasks;
  for i := 0 to high(tasks) do
  begin
    if tasks[i].category = acategory then
    begin
      amanager.update_task(tasks[i].id, tasks[i].title, tasks[i].description, tasks[i].category, 
                          tasks[i].assigned_to, tasks[i].tags, anew_prio, 
                          tasks[i].due_date, tasks[i].is_important);
      inc(result);
    end;
  end;
end;

procedure ttaskbulkops.self_test(amanager: ttaskmanager);
var
  count: integer;
begin
  writeln('--- Starting TTaskBulkOps Self Test ---');
  count := replace_category(amanager, 'Cat', 'NewCat');
  writeln(format('Updated %d tasks to NewCat', [count]));
  
  if count > 0 then
    writeln('Success: Bulk category update works.')
  else
    writeln('Note: No tasks updated.');
  writeln('--- TTaskBulkOps Self Test Completed ---');
end;

end.
