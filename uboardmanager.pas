
unit uboardmanager;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, Classes, utaskmanager, uprojectmanager;

type
  TBoard = record
    id: integer;
    name: string;
    description: string;
    project_manager: TProjectManager;
  end;

  TBoardArray = array of TBoard;

  TBoardManager = class
  private
    f_boards: TBoardArray;
    f_next_id: integer;
    function find_index_by_id(aid: integer): integer;
  public
    constructor create;
    destructor destroy; override;
    procedure add_board(const aname, adescription: string);
    procedure remove_board(aid: integer);
    function get_board_projects(aid: integer): TProjectManager;
    function get_boards: TBoardArray;
    function global_search(const aquery: string): ttaskarray;
    procedure age_all_tasks;
    function get_global_summary: string;
    procedure save_state(const aroot_folder: string);
    procedure load_state(const aroot_folder: string);
    procedure self_test;
  end;

implementation

constructor TBoardManager.create;
begin
  f_next_id := 1;
  setlength(f_boards, 0);
end;

destructor TBoardManager.destroy;
var
  i: integer;
begin
  for i := 0 to high(f_boards) do
    f_boards[i].project_manager.free;
  setlength(f_boards, 0);
  inherited destroy;
end;

function TBoardManager.find_index_by_id(aid: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to high(f_boards) do
    if f_boards[i].id = aid then
    begin
      result := i;
      break;
    end;
end;

procedure TBoardManager.add_board(const aname, adescription: string);
begin
  setlength(f_boards, length(f_boards) + 1);
  with f_boards[high(f_boards)] do
  begin
    id := f_next_id;
    name := aname;
    description := adescription;
    project_manager := TProjectManager.create;
  end;
  inc(f_next_id);
end;

procedure TBoardManager.remove_board(aid: integer);
var
  idx, i: integer;
begin
  idx := find_index_by_id(aid);
  if idx <> -1 then
  begin
    f_boards[idx].project_manager.free;
    for i := idx to high(f_boards) - 1 do
      f_boards[i] := f_boards[i + 1];
    setlength(f_boards, length(f_boards) - 1);
  end;
end;

function TBoardManager.get_board_projects(aid: integer): TProjectManager;
var
  idx: integer;
begin
  result := nil;
  idx := find_index_by_id(aid);
  if idx <> -1 then
    result := f_boards[idx].project_manager;
end;

function TBoardManager.get_boards: TBoardArray;
begin
  result := f_boards;
end;

function TBoardManager.global_search(const aquery: string): ttaskarray;
var
  i, j, k: integer;
  projects: TProjectArray;
  ptasks: ttaskarray;
begin
  setlength(result, 0);
  for i := 0 to high(f_boards) do
  begin
    projects := f_boards[i].project_manager.get_projects;
    for j := 0 to high(projects) do
    begin
      ptasks := projects[j].task_manager.search_tasks(aquery);
      for k := 0 to high(ptasks) do
      begin
        setlength(result, length(result) + 1);
        result[high(result)] := ptasks[k];
      end;
    end;
  end;
end;

procedure TBoardManager.age_all_tasks;
var
  i, j: integer;
  projects: TProjectArray;
begin
  for i := 0 to high(f_boards) do
  begin
    projects := f_boards[i].project_manager.get_projects;
    for j := 0 to high(projects) do
      projects[j].task_manager.age_priorities;
  end;
end;

function TBoardManager.get_global_summary: string;
var
  i, j: integer;
  projects: TProjectArray;
  total_tasks, high_risk: integer;
begin
  total_tasks := 0;
  high_risk := 0;
  result := '--- Global Executive Dashboard ---' + sLineBreak;
  for i := 0 to high(f_boards) do
  begin
    result := result + 'Board: ' + f_boards[i].name + sLineBreak;
    projects := f_boards[i].project_manager.get_projects;
    for j := 0 to high(projects) do
    begin
      total_tasks := total_tasks + length(projects[j].task_manager.get_tasks);
      if f_boards[i].project_manager.get_project_risk(projects[j].id) = prHigh then
        inc(high_risk);
      result := result + format('  - Project: %s (Progress: %.1f%%)' + sLineBreak, [projects[j].name, f_boards[i].project_manager.get_overall_progress(projects[j].id)]);
    end;
  end;
  result := result + sLineBreak + format('Total Tasks: %d | High Risk Projects: %d', [total_tasks, high_risk]) + sLineBreak;
end;

procedure TBoardManager.save_state(const aroot_folder: string);
var
  f: textFile;
  i: integer;
begin
  if not directoryExists(aroot_folder) then forceDirectories(aroot_folder);
  assignFile(f, aroot_folder + '/boards.dat');
  rewrite(f);
  writeln(f, f_next_id);
  writeln(f, length(f_boards));
  for i := 0 to high(f_boards) do
  begin
    writeln(f, f_boards[i].id);
    writeln(f, f_boards[i].name);
    writeln(f, f_boards[i].description);
    f_boards[i].project_manager.save_all(aroot_folder + '/board_' + intToStr(f_boards[i].id));
  end;
  closefile(f);
end;

procedure TBoardManager.load_state(const aroot_folder: string);
var
  f: textFile;
  i, count: integer;
begin
  if not fileExists(aroot_folder + '/boards.dat') then exit;
  assignFile(f, aroot_folder + '/boards.dat');
  reset(f);
  readln(f, f_next_id);
  readln(f, count);
  setlength(f_boards, count);
  for i := 0 to count - 1 do
  begin
    readln(f, f_boards[i].id);
    readln(f, f_boards[i].name);
    readln(f, f_boards[i].description);
    f_boards[i].project_manager := TProjectManager.create;
    f_boards[i].project_manager.load_all(aroot_folder + '/board_' + intToStr(f_boards[i].id));
  end;
  closefile(f);
end;

procedure TBoardManager.self_test;
var
  res: ttaskarray;
begin
  writeln('Running Board Manager self test (Global Search)...');
  add_board('Search Board', 'Test board');
  f_boards[high(f_boards)].project_manager.add_project('Search Project', 'Test project', now + 1);
  f_boards[high(f_boards)].project_manager.get_projects[0].task_manager.add_task('UniqueTask', 'Desc', 'Cat', nil, tp_medium, now + 1);
  
  res := global_search('UniqueTask');
  if length(res) > 0 then
    writeln('Global search ok.')
  else
    writeln('Global search failed.');
    
  writeln('Board Manager self test passed.');
end;

end.
