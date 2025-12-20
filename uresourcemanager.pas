
unit uresourcemanager;

{$mode objfpc}
{$h+}

interface

uses
  SysUtils, Classes, utaskmanager;

type
  TResourceWorkload = record
    resource_name: string;
    task_count: integer;
    total_effort: double;
    overloaded: boolean;
  end;

  TWorkloadArray = array of TResourceWorkload;

  TResourceManager = class
  private
    f_max_effort_per_resource: double;
  public
    constructor create(a_max_effort: double = 8.0);
    function calculate_workload(a_manager: ttaskmanager): TWorkloadArray;
    function get_overloaded_resources(a_manager: ttaskmanager): ttagarray;
    procedure self_test(a_manager: ttaskmanager);
  end;

implementation

constructor TResourceManager.create(a_max_effort: double);
begin
  f_max_effort_per_resource := a_max_effort;
end;

function TResourceManager.calculate_workload(a_manager: ttaskmanager): TWorkloadArray;
var
  tasks: ttaskarray;
  i, j: integer;
  found: boolean;
  r_name: string;
begin
  setLength(result, 0);
  tasks := a_manager.get_tasks;
  
  for i := 0 to high(tasks) do
  begin
    r_name := tasks[i].assigned_to;
    if r_name = '' then r_name := 'Unassigned';
    
    found := false;
    for j := 0 to high(result) do
    begin
      if result[j].resource_name = r_name then
      begin
        result[j].task_count := result[j].task_count + 1;
        result[j].total_effort := result[j].total_effort + tasks[i].estimated_hours;
        result[j].overloaded := result[j].total_effort > f_max_effort_per_resource;
        found := true;
        break;
      end;
    end;
    
    if not found then
    begin
      setLength(result, length(result) + 1);
      result[high(result)].resource_name := r_name;
      result[high(result)].task_count := 1;
      result[high(result)].total_effort := tasks[i].estimated_hours;
      result[high(result)].overloaded := result[high(result)].total_effort > f_max_effort_per_resource;
    end;
  end;
end;

function TResourceManager.get_overloaded_resources(a_manager: ttaskmanager): ttagarray;
var
  workloads: TWorkloadArray;
  i: integer;
begin
  setLength(result, 0);
  workloads := calculate_workload(a_manager);
  for i := 0 to high(workloads) do
  begin
    if workloads[i].overloaded then
    begin
      setLength(result, length(result) + 1);
      result[high(result)] := workloads[i].resource_name;
    end;
  end;
end;

procedure TResourceManager.self_test(a_manager: ttaskmanager);
var
  overloaded: ttagarray;
  i: integer;
begin
  writeLn('--- Resource Manager Self Test ---');
  overloaded := get_overloaded_resources(a_manager);
  writeLn('Overloaded resources count: ', length(overloaded));
  for i := 0 to high(overloaded) do
    writeLn(' - ', overloaded[i]);
end;

end.
