
unit TaskBudget;

{$mode objfpc}

interface

uses
  SysUtils,
  Math,
  TaskTypes;

type
  { Budget record }
  TBudgetEntry = record
    ID: integer;
    TaskID: integer;
    Description: string;
    PlannedCost: double;
    ActualCost: double;
    Category: string;
    EntryDate: TDateTime;
    Status: TTaskStatus;
  end;

  { Array type for budget entries }
  TBudgetEntryArray = array of TBudgetEntry;

  { Budget stats record }
  TBudgetStats = record
    TotalPlanned: double;
    TotalActual: double;
    Variance: double;
    VariancePercent: double;
    RemainingBudget: double;
    BudgetUtilization: double;
  end;

  { Budget manager class }
  TTaskBudgetManagerClass = class
  private
    fBudgetEntries: TBudgetEntryArray;
    fNextBudgetID: integer;
    fProjectBudgetLimit: double;
    function FindBudgetIndex(id: integer): integer;
  public
    constructor Create(projectBudgetLimit: double);
    destructor Destroy();
    function AddBudgetEntry(taskId: integer; description: string; plannedCost: double; category: string): integer;
    function RecordActualCost(budgetId: integer; actualCost: double): boolean;
    function GetBudgetEntry(id: integer): TBudgetEntry;
    function GetTaskBudget(taskId: integer): TBudgetEntry;
    function GetAllBudgetEntries(): TBudgetEntryArray;
    function GetBudgetStats(): TBudgetStats;
    function GetCategoryBudget(category: string): TBudgetStats;
    function IsWithinBudget(): boolean;
    function GetBudgetRemaining(): double;
    function GetBudgetUtilization(): double;
    function CalculateVariance(budgetId: integer): double;
    function GetTotalPlannedCost(): double;
    function GetTotalActualCost(): double;
    function GetOverBudgetEntries(): TBudgetEntryArray;
    procedure DeleteBudgetEntry(id: integer);
    procedure SelfTest();
  end;

implementation

constructor TTaskBudgetManagerClass.Create(projectBudgetLimit: double);
begin
  SetLength(fBudgetEntries, 0);
  fNextBudgetID := 1;
  fProjectBudgetLimit := projectBudgetLimit;
end;

destructor TTaskBudgetManagerClass.Destroy();
begin
  SetLength(fBudgetEntries, 0);
  inherited Destroy();
end;

function TTaskBudgetManagerClass.FindBudgetIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(fBudgetEntries) - 1 do
  begin
    if fBudgetEntries[i].ID = id then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskBudgetManagerClass.AddBudgetEntry(taskId: integer; description: string; plannedCost: double; category: string): integer;
var
  newEntry: TBudgetEntry;
begin
  newEntry.ID := fNextBudgetID;
  newEntry.TaskID := taskId;
  newEntry.Description := description;
  newEntry.PlannedCost := plannedCost;
  newEntry.ActualCost := 0;
  newEntry.Category := category;
  newEntry.EntryDate := Now();
  newEntry.Status := tsNotStarted;
  
  SetLength(fBudgetEntries, Length(fBudgetEntries) + 1);
  fBudgetEntries[Length(fBudgetEntries) - 1] := newEntry;
  
  result := fNextBudgetID;
  fNextBudgetID := fNextBudgetID + 1;
end;

function TTaskBudgetManagerClass.RecordActualCost(budgetId: integer; actualCost: double): boolean;
var
  idx: integer;
begin
  idx := FindBudgetIndex(budgetId);
  if idx >= 0 then
  begin
    fBudgetEntries[idx].ActualCost := actualCost;
    result := true;
  end
  else
    result := false;
end;

function TTaskBudgetManagerClass.GetBudgetEntry(id: integer): TBudgetEntry;
var
  idx: integer;
begin
  idx := FindBudgetIndex(id);
  if idx >= 0 then
    result := fBudgetEntries[idx]
  else
  begin
    result.ID := -1;
    result.PlannedCost := 0;
  end;
end;

function TTaskBudgetManagerClass.GetTaskBudget(taskId: integer): TBudgetEntry;
var
  i: integer;
begin
  for i := 0 to Length(fBudgetEntries) - 1 do
  begin
    if fBudgetEntries[i].TaskID = taskId then
    begin
      result := fBudgetEntries[i];
      exit;
    end;
  end;
  result.ID := -1;
end;

function TTaskBudgetManagerClass.GetAllBudgetEntries(): TBudgetEntryArray;
begin
  result := fBudgetEntries;
end;

function TTaskBudgetManagerClass.GetBudgetStats(): TBudgetStats;
var
  i: integer;
  stats: TBudgetStats;
begin
  stats.TotalPlanned := 0;
  stats.TotalActual := 0;
  
  for i := 0 to Length(fBudgetEntries) - 1 do
  begin
    stats.TotalPlanned := stats.TotalPlanned + fBudgetEntries[i].PlannedCost;
    stats.TotalActual := stats.TotalActual + fBudgetEntries[i].ActualCost;
  end;
  
  stats.Variance := stats.TotalPlanned - stats.TotalActual;
  if stats.TotalPlanned > 0 then
    stats.VariancePercent := (stats.Variance / stats.TotalPlanned) * 100
  else
    stats.VariancePercent := 0;
  
  stats.RemainingBudget := fProjectBudgetLimit - stats.TotalActual;
  if fProjectBudgetLimit > 0 then
    stats.BudgetUtilization := (stats.TotalActual / fProjectBudgetLimit) * 100
  else
    stats.BudgetUtilization := 0;
  
  result := stats;
end;

function TTaskBudgetManagerClass.GetCategoryBudget(category: string): TBudgetStats;
var
  i: integer;
  stats: TBudgetStats;
begin
  stats.TotalPlanned := 0;
  stats.TotalActual := 0;
  
  for i := 0 to Length(fBudgetEntries) - 1 do
  begin
    if fBudgetEntries[i].Category = category then
    begin
      stats.TotalPlanned := stats.TotalPlanned + fBudgetEntries[i].PlannedCost;
      stats.TotalActual := stats.TotalActual + fBudgetEntries[i].ActualCost;
    end;
  end;
  
  stats.Variance := stats.TotalPlanned - stats.TotalActual;
  if stats.TotalPlanned > 0 then
    stats.VariancePercent := (stats.Variance / stats.TotalPlanned) * 100
  else
    stats.VariancePercent := 0;
  
  stats.RemainingBudget := fProjectBudgetLimit - stats.TotalActual;
  if fProjectBudgetLimit > 0 then
    stats.BudgetUtilization := (stats.TotalActual / fProjectBudgetLimit) * 100
  else
    stats.BudgetUtilization := 0;
  
  result := stats;
end;

function TTaskBudgetManagerClass.IsWithinBudget(): boolean;
begin
  result := GetTotalActualCost() <= fProjectBudgetLimit;
end;

function TTaskBudgetManagerClass.GetBudgetRemaining(): double;
begin
  result := fProjectBudgetLimit - GetTotalActualCost();
end;

function TTaskBudgetManagerClass.GetBudgetUtilization(): double;
var
  stats: TBudgetStats;
begin
  stats := GetBudgetStats();
  result := stats.BudgetUtilization;
end;

function TTaskBudgetManagerClass.CalculateVariance(budgetId: integer): double;
var
  idx: integer;
begin
  idx := FindBudgetIndex(budgetId);
  if idx >= 0 then
    result := fBudgetEntries[idx].PlannedCost - fBudgetEntries[idx].ActualCost
  else
    result := 0;
end;

function TTaskBudgetManagerClass.GetTotalPlannedCost(): double;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to Length(fBudgetEntries) - 1 do
    total := total + fBudgetEntries[i].PlannedCost;
  result := total;
end;

function TTaskBudgetManagerClass.GetTotalActualCost(): double;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to Length(fBudgetEntries) - 1 do
    total := total + fBudgetEntries[i].ActualCost;
  result := total;
end;

function TTaskBudgetManagerClass.GetOverBudgetEntries(): TBudgetEntryArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  for i := 0 to Length(fBudgetEntries) - 1 do
  begin
    if fBudgetEntries[i].ActualCost > fBudgetEntries[i].PlannedCost then
    begin
      SetLength(result, count + 1);
      result[count] := fBudgetEntries[i];
      count := count + 1;
    end;
  end;
end;

procedure TTaskBudgetManagerClass.DeleteBudgetEntry(id: integer);
var
  idx, i: integer;
begin
  idx := FindBudgetIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(fBudgetEntries) - 2 do
      fBudgetEntries[i] := fBudgetEntries[i + 1];
    SetLength(fBudgetEntries, Length(fBudgetEntries) - 1);
  end;
end;

procedure TTaskBudgetManagerClass.SelfTest();
var
  budget_mgr: TTaskBudgetManagerClass;
  budgetId1, budgetId2: integer;
  stats: TBudgetStats;
begin
  WriteLn('');
  WriteLn('=== TaskBudget Manager Self Test ===');
  
  budget_mgr := TTaskBudgetManagerClass.Create(10000.0);
  
  try
    { Add budget entries }
    budgetId1 := budget_mgr.AddBudgetEntry(1, 'Development', 3000.0, 'Labor');
    budgetId2 := budget_mgr.AddBudgetEntry(2, 'Testing', 1500.0, 'Labor');
    WriteLn('Added 2 budget entries');
    
    { Record actual costs }
    budget_mgr.RecordActualCost(budgetId1, 2800.0);
    budget_mgr.RecordActualCost(budgetId2, 1600.0);
    WriteLn('Recorded actual costs');
    
    { Get budget stats }
    stats := budget_mgr.GetBudgetStats();
    WriteLn('Total Planned: $' + FormatFloat('0.00', stats.TotalPlanned));
    WriteLn('Total Actual: $' + FormatFloat('0.00', stats.TotalActual));
    WriteLn('Variance: $' + FormatFloat('0.00', stats.Variance));
    WriteLn('Budget Utilization: ' + FormatFloat('0.0', stats.BudgetUtilization) + '%');
    
    if budget_mgr.IsWithinBudget() then
      WriteLn('✓ Project is within budget')
    else
      WriteLn('✗ Project exceeds budget');
    
    WriteLn('Budget Remaining: $' + FormatFloat('0.00', budget_mgr.GetBudgetRemaining()));
    WriteLn('=== TaskBudget Manager Self Test Complete ===');
  finally
    budget_mgr.Free();
  end;
end;

end.
