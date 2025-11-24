
unit TaskDependencies;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, Math;

type
  { Task dependency relationship }
  TTaskDependency = record
    dependencyId: integer;
    taskId: integer;           { Task that has the dependency }
    dependsOnTaskId: integer;  { Task that must be completed first }
    dependencyType: string;    { 'finish-start', 'finish-finish', 'start-start', 'start-finish' }
    lagDays: integer;          { Days to wait after dependency is complete }
  end;

  TTaskDependencyArray = array of TTaskDependency;

  TIntegerArray = array of integer;
  TDependencyChain = array of integer;

  { Task dependency manager }
  TTaskDependencyManagerClass = class
  private
    dependencies: TTaskDependencyArray;
    nextDependencyId: integer;
  public
    constructor Create();
    destructor Destroy();
    function AddDependency(taskId, dependsOnTaskId: integer; depType: string; lagDays: integer): integer;
    function HasDependency(taskId: integer): boolean;
    function GetDependencies(taskId: integer): TTaskDependencyArray;
    function GetBlockingTasks(taskId: integer): TIntegerArray;
    function GetDependentTasks(taskId: integer): TIntegerArray;
    function CanTaskStart(taskId: integer; completedTasks: TIntegerArray): boolean;
    function GetCriticalPath(): TDependencyChain;
    function DetectCircularDependency(taskId: integer): boolean;
    function DeleteDependency(dependencyId: integer): boolean;
    function ClearDependencies(taskId: integer): integer;
    procedure SelfTest();
  end;

  { Analysis for task readiness }
  TTaskReadinessAnalyzer = class
  private
    dependencies: TTaskDependencyArray;
  public
    constructor Create(var deps: TTaskDependencyArray);
    destructor Destroy();
    function GetReadyTasks(allTaskIds: TIntegerArray; completedTasks: TIntegerArray): TIntegerArray;
    function GetDependencyCount(taskId: integer): integer;
    function EstimateTaskStartDate(taskId: integer; taskDueDates: array of TDateTime): TDateTime;
    function GetCriticalPathLength(): integer;
  end;

implementation

{ ===== TTaskDependencyManagerClass ===== }

constructor TTaskDependencyManagerClass.Create();
begin
  inherited Create();
  SetLength(dependencies, 0);
  nextDependencyId := 1;
end;

destructor TTaskDependencyManagerClass.Destroy();
begin
  SetLength(dependencies, 0);
  inherited Destroy();
end;

function TTaskDependencyManagerClass.AddDependency(taskId, dependsOnTaskId: integer; depType: string; lagDays: integer): integer;
var
  len: integer;
  dep: TTaskDependency;
begin
  { Prevent self-dependency }
  if taskId = dependsOnTaskId then
  begin
    result := -1;
    exit;
  end;

  { Prevent circular dependency }
  if DetectCircularDependency(taskId) then
  begin
    result := -1;
    exit;
  end;

  dep.dependencyId := nextDependencyId;
  dep.taskId := taskId;
  dep.dependsOnTaskId := dependsOnTaskId;
  dep.dependencyType := depType;
  dep.lagDays := lagDays;

  len := Length(dependencies);
  SetLength(dependencies, len + 1);
  dependencies[len] := dep;
  result := nextDependencyId;
  inc(nextDependencyId);
end;

function TTaskDependencyManagerClass.HasDependency(taskId: integer): boolean;
var
  i: integer;
begin
  for i := 0 to Length(dependencies) - 1 do
  begin
    if dependencies[i].taskId = taskId then
    begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TTaskDependencyManagerClass.GetDependencies(taskId: integer): TTaskDependencyArray;
var
  i, count: integer;
  resultArr: TTaskDependencyArray;
begin
  count := 0;
  SetLength(resultArr, 0);

  for i := 0 to Length(dependencies) - 1 do
  begin
    if dependencies[i].taskId = taskId then
    begin
      SetLength(resultArr, count + 1);
      resultArr[count] := dependencies[i];
      inc(count);
    end;
  end;

  result := resultArr;
end;

function TTaskDependencyManagerClass.GetBlockingTasks(taskId: integer): TIntegerArray;
var
  i, count: integer;
  resultArr: TIntegerArray;
  taskDeps: TTaskDependencyArray;
begin
  taskDeps := GetDependencies(taskId);
  count := 0;
  SetLength(resultArr, 0);

  for i := 0 to Length(taskDeps) - 1 do
  begin
    SetLength(resultArr, count + 1);
    resultArr[count] := taskDeps[i].dependsOnTaskId;
    inc(count);
  end;

  result := resultArr;
end;

function TTaskDependencyManagerClass.GetDependentTasks(taskId: integer): TIntegerArray;
var
  i, count: integer;
  resultArr: TIntegerArray;
begin
  count := 0;
  SetLength(resultArr, 0);

  for i := 0 to Length(dependencies) - 1 do
  begin
    if dependencies[i].dependsOnTaskId = taskId then
    begin
      SetLength(resultArr, count + 1);
      resultArr[count] := dependencies[i].taskId;
      inc(count);
    end;
  end;

  result := resultArr;
end;

function TTaskDependencyManagerClass.CanTaskStart(taskId: integer; completedTasks: TIntegerArray): boolean;
var
  blockingTasks: TIntegerArray;
  i, j: integer;
  found: boolean;
begin
  blockingTasks := GetBlockingTasks(taskId);

  if Length(blockingTasks) = 0 then
  begin
    result := true;
    exit;
  end;

  for i := 0 to Length(blockingTasks) - 1 do
  begin
    found := false;
    for j := 0 to Length(completedTasks) - 1 do
    begin
      if blockingTasks[i] = completedTasks[j] then
      begin
        found := true;
        break;
      end;
    end;
    if not found then
    begin
      result := false;
      exit;
    end;
  end;

  result := true;
end;

function TTaskDependencyManagerClass.GetCriticalPath(): TDependencyChain;
var
  chain: TDependencyChain;
begin
  SetLength(chain, 0);
  result := chain;
end;

function TTaskDependencyManagerClass.DetectCircularDependency(taskId: integer): boolean;
var
  visited: array of integer;
  queue: array of integer;
  current, i, j, k: integer;
  found: boolean;
  blockingTasks: TIntegerArray;
begin
  SetLength(visited, 0);
  SetLength(queue, 0);
  
  SetLength(queue, 1);
  queue[0] := taskId;

  while Length(queue) > 0 do
  begin
    current := queue[0];
    
    for i := 0 to Length(queue) - 2 do
      queue[i] := queue[i + 1];
    SetLength(queue, Length(queue) - 1);

    { Check if current is already visited (circular) }
    found := false;
    for i := 0 to Length(visited) - 1 do
    begin
      if visited[i] = current then
      begin
        result := true;
        SetLength(visited, 0);
        SetLength(queue, 0);
        exit;
      end;
    end;

    { Add to visited }
    SetLength(visited, Length(visited) + 1);
    visited[Length(visited) - 1] := current;

    { Add blocking tasks to queue }
    blockingTasks := GetBlockingTasks(current);
    for k := 0 to Length(blockingTasks) - 1 do
    begin
      SetLength(queue, Length(queue) + 1);
      queue[Length(queue) - 1] := blockingTasks[k];
    end;
  end;

  SetLength(visited, 0);
  result := false;
end;

function TTaskDependencyManagerClass.DeleteDependency(dependencyId: integer): boolean;
var
  i, j: integer;
begin
  for i := 0 to Length(dependencies) - 1 do
  begin
    if dependencies[i].dependencyId = dependencyId then
    begin
      for j := i to Length(dependencies) - 2 do
        dependencies[j] := dependencies[j + 1];
      SetLength(dependencies, Length(dependencies) - 1);
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TTaskDependencyManagerClass.ClearDependencies(taskId: integer): integer;
var
  i, j, count: integer;
begin
  count := 0;
  i := 0;
  while i < Length(dependencies) do
  begin
    if (dependencies[i].taskId = taskId) or (dependencies[i].dependsOnTaskId = taskId) then
    begin
      for j := i to Length(dependencies) - 2 do
        dependencies[j] := dependencies[j + 1];
      SetLength(dependencies, Length(dependencies) - 1);
      inc(count);
    end
    else
      inc(i);
  end;
  result := count;
end;

procedure TTaskDependencyManagerClass.SelfTest();
var
  depMgr: TTaskDependencyManagerClass;
  depId1, depId2, cleared: integer;
  blockingTasks: TIntegerArray;
  canStart: boolean;
  completedTasks: TIntegerArray;
begin
  WriteLn('=== TaskDependencies Manager Self Test ===');
  depMgr := TTaskDependencyManagerClass.Create();

  { Add some dependencies }
  depId1 := depMgr.AddDependency(2, 1, 'finish-start', 0);
  depId2 := depMgr.AddDependency(3, 2, 'finish-start', 0);
  depMgr.AddDependency(4, 1, 'finish-start', 1);

  WriteLn('Added 3 dependencies:');
  WriteLn('  Task 2 depends on task 1');
  WriteLn('  Task 3 depends on task 2');
  WriteLn('  Task 4 depends on task 1 (with 1 day lag)');

  { Get blocking tasks }
  blockingTasks := depMgr.GetBlockingTasks(2);
  WriteLn(Format('Task 2 is blocked by %d task(s)', [Length(blockingTasks)]));

  { Check if task can start }
  SetLength(completedTasks, 1);
  completedTasks[0] := 1;
  canStart := depMgr.CanTaskStart(2, completedTasks);
  WriteLn(Format('Task 2 can start (with task 1 complete): %s', [BoolToStr(canStart, true)]));

  { Detect circular dependency }
  WriteLn(Format('Circular dependency detected: %s', 
    [BoolToStr(depMgr.DetectCircularDependency(1), true)]));

  { Delete dependency }
  if depMgr.DeleteDependency(depId2) then
    WriteLn('✓ Deleted dependency: Task 3 no longer depends on Task 2');

  { Clear dependencies for a task }
  cleared := depMgr.ClearDependencies(1);
  WriteLn(Format('✓ Cleared %d dependencies related to task 1', [cleared]));

  depMgr.Destroy();
  WriteLn('=== TaskDependencies Manager Self Test Complete ===');
end;

{ ===== TTaskReadinessAnalyzer ===== }

constructor TTaskReadinessAnalyzer.Create(var deps: TTaskDependencyArray);
var
  i: integer;
begin
  inherited Create();
  SetLength(dependencies, Length(deps));
  for i := 0 to Length(deps) - 1 do
    dependencies[i] := deps[i];
end;

destructor TTaskReadinessAnalyzer.Destroy();
begin
  SetLength(dependencies, 0);
  inherited Destroy();
end;

function TTaskReadinessAnalyzer.GetReadyTasks(allTaskIds: TIntegerArray; completedTasks: TIntegerArray): TIntegerArray;
var
  readyTasks: TIntegerArray;
  i, j, k, count: integer;
  isReady, isCompleted, found: boolean;
begin
  count := 0;
  SetLength(readyTasks, 0);

  for i := 0 to Length(allTaskIds) - 1 do
  begin
    { Check if already completed }
    isCompleted := false;
    for j := 0 to Length(completedTasks) - 1 do
    begin
      if allTaskIds[i] = completedTasks[j] then
      begin
        isCompleted := true;
        break;
      end;
    end;

    if isCompleted then
      continue;

    { Check if has any incomplete dependencies }
    isReady := true;
    for j := 0 to Length(dependencies) - 1 do
    begin
      if dependencies[j].taskId = allTaskIds[i] then
      begin
        found := false;
        for k := 0 to Length(completedTasks) - 1 do
        begin
          if completedTasks[k] = dependencies[j].dependsOnTaskId then
          begin
            found := true;
            break;
          end;
        end;
        if not found then
        begin
          isReady := false;
          break;
        end;
      end;
    end;

    if isReady then
    begin
      SetLength(readyTasks, count + 1);
      readyTasks[count] := allTaskIds[i];
      inc(count);
    end;
  end;

  result := readyTasks;
end;

function TTaskReadinessAnalyzer.GetDependencyCount(taskId: integer): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(dependencies) - 1 do
  begin
    if dependencies[i].taskId = taskId then
      inc(count);
  end;
  result := count;
end;

function TTaskReadinessAnalyzer.EstimateTaskStartDate(taskId: integer; taskDueDates: array of TDateTime): TDateTime;
var
  i, j: integer;
  maxDate: TDateTime;
  found: boolean;
begin
  maxDate := Now();

  for i := 0 to Length(dependencies) - 1 do
  begin
    if dependencies[i].taskId = taskId then
    begin
      found := false;
      for j := 0 to Length(taskDueDates) - 1 do
      begin
        if (j + 1) = dependencies[i].dependsOnTaskId then
        begin
          if taskDueDates[j] > maxDate then
            maxDate := taskDueDates[j];
          found := true;
          break;
        end;
      end;
    end;
  end;

  result := maxDate;
end;

function TTaskReadinessAnalyzer.GetCriticalPathLength(): integer;
begin
  result := 0;
end;

end.
