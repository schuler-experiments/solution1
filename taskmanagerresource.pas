
{$mode objfpc}{$H+}
unit taskmanagerresource;

interface

uses
  SysUtils, DateUtils, Classes, Math,
  taskmanager, taskmanageradvanced, taskmanagerenhanced,
  taskmanagerteam, taskmanagergamify, taskmanagersmart, taskmanagerfocus;

type
  // Resource types
  TResourceType = (rtHuman, rtMaterial, rtEquipment, rtFinancial, rtTime);
  
  TBudgetStatus = (bsUnderBudget, bsOnBudget, bsOverBudget, bsAtRisk);
  
  TCostCategory = (ccLabor, ccMaterials, ccEquipment, ccOverhead, ccContingency, ccOther);
  
  TForecastMethod = (fmLinear, fmExponential, fmHistorical);

  // Resource definition
  TResource = record
    ID: Integer;
    Name: string;
    ResourceType: TResourceType;
    UnitCost: Double;
    AvailableQuantity: Double;
    UnitOfMeasure: string; // hours, kg, units, etc.
    Description: string;
    IsActive: Boolean;
    DateAdded: TDateTime;
  end;
  TResourceArray = array of TResource;

  // Task budget allocation
  TTaskBudget = record
    ID: Integer;
    TaskID: Integer;
    AllocatedAmount: Double;
    SpentAmount: Double;
    CommittedAmount: Double;
    CostCategory: TCostCategory;
    Currency: string;
    Notes: string;
    DateCreated: TDateTime;
    LastUpdated: TDateTime;
  end;
  TTaskBudgetArray = array of TTaskBudget;

  // Resource allocation to tasks
  TResourceAllocation = record
    ID: Integer;
    TaskID: Integer;
    ResourceID: Integer;
    QuantityAllocated: Double;
    QuantityUsed: Double;
    UnitCost: Double;
    TotalCost: Double;
    DateAllocated: TDateTime;
    DateCompleted: TDateTime;
    Notes: string;
  end;
  TResourceAllocationArray = array of TResourceAllocation;

  // Budget variance tracking
  TBudgetVariance = record
    TaskID: Integer;
    TaskTitle: string;
    PlannedCost: Double;
    ActualCost: Double;
    Variance: Double;
    VariancePercent: Double;
    Status: TBudgetStatus;
  end;
  TBudgetVarianceArray = array of TBudgetVariance;

  // Financial forecast
  TFinancialForecast = record
    TaskID: Integer;
    TaskTitle: string;
    CurrentSpent: Double;
    EstimatedFinalCost: Double;
    BudgetRemaining: Double;
    PercentComplete: Double;
    PredictedOverrun: Double;
    ForecastDate: TDateTime;
    Method: TForecastMethod;
    Confidence: Double; // 0-100%
  end;
  TFinancialForecastArray = array of TFinancialForecast;

  // Dashboard metrics
  TDashboardMetrics = record
    TotalBudget: Double;
    TotalSpent: Double;
    TotalCommitted: Double;
    BudgetUtilization: Double;
    AverageVariance: Double;
    TasksOverBudget: Integer;
    TasksUnderBudget: Integer;
    ResourceUtilization: Double;
    ProjectedOverrun: Double;
    ROI: Double;
  end;

  // Cost breakdown
  TCostBreakdown = record
    Category: TCostCategory;
    Amount: Double;
    Percentage: Double;
  end;
  TCostBreakdownArray = array of TCostBreakdown;

  { TResourceTaskManager }
  TResourceTaskManager = class(TFocusTaskManager)
  private
    FResources: TResourceArray;
    FTaskBudgets: TTaskBudgetArray;
    FResourceAllocations: TResourceAllocationArray;
    FNextResourceID: Integer;
    FNextBudgetID: Integer;
    FNextAllocationID: Integer;
    FDefaultCurrency: string;
    FContingencyPercent: Double;
    
    function FindResourceIndex(AResourceID: Integer): Integer;
    function FindBudgetIndex(ABudgetID: Integer): Integer;
    function FindAllocationIndex(AAllocationID: Integer): Integer;
    function CalculateBudgetStatus(APlanned, AActual: Double): TBudgetStatus;
    function CalculateEAC(ATaskID: Integer): Double; // Estimate At Completion
  public
    constructor Create;
    destructor Destroy; override;
    
    // Resource management
    function AddResource(const AName: string; AType: TResourceType;
      AUnitCost: Double; AQuantity: Double; const AUnitOfMeasure, ADescription: string): Integer;
    function UpdateResource(AResourceID: Integer; const AName: string;
      AUnitCost: Double; AQuantity: Double): Boolean;
    function DeactivateResource(AResourceID: Integer): Boolean;
    function GetResource(AResourceID: Integer): TResource;
    function GetAllResources: TResourceArray;
    function GetActiveResources: TResourceArray;
    function GetResourcesByType(AType: TResourceType): TResourceArray;
    
    // Budget management
    function SetTaskBudget(ATaskID: Integer; AAmount: Double;
      ACategory: TCostCategory; const ANotes: string): Integer;
    function UpdateTaskBudget(ABudgetID: Integer; AAmount: Double): Boolean;
    function GetTaskBudget(ATaskID: Integer): TTaskBudgetArray;
    function GetTotalTaskBudget(ATaskID: Integer): Double;
    function RecordExpense(ABudgetID: Integer; AAmount: Double;
      const ANotes: string): Boolean;
    function CommitFunds(ABudgetID: Integer; AAmount: Double): Boolean;
    
    // Resource allocation
    function AllocateResource(ATaskID, AResourceID: Integer;
      AQuantity: Double; const ANotes: string): Integer;
    function RecordResourceUsage(AAllocationID: Integer; AQuantityUsed: Double): Boolean;
    function GetTaskAllocations(ATaskID: Integer): TResourceAllocationArray;
    function GetResourceAllocations(AResourceID: Integer): TResourceAllocationArray;
    function CalculateResourceCost(AAllocationID: Integer): Double;
    
    // Analysis and reporting
    function GetBudgetVariance(ATaskID: Integer): TBudgetVariance;
    function GetAllBudgetVariances: TBudgetVarianceArray;
    function GetOverBudgetTasks: TBudgetVarianceArray;
    function ForecastTaskCost(ATaskID: Integer; AMethod: TForecastMethod): TFinancialForecast;
    function GetDashboardMetrics: TDashboardMetrics;
    function GetCostBreakdown: TCostBreakdownArray;
    function GetResourceUtilization: string;
    function CalculateROI(ATaskID: Integer; AExpectedValue: Double): Double;
    
    // Advanced reports
    function GenerateBudgetReport: string;
    function GenerateResourceReport: string;
    function GenerateFinancialSummary: string;
    function GenerateCostTrendAnalysis: string;
    function ExportFinancialData: string;
    
    // Utilities
    function ResourceTypeToString(AType: TResourceType): string;
    function CostCategoryToString(ACategory: TCostCategory): string;
    function BudgetStatusToString(AStatus: TBudgetStatus): string;
    function FormatCurrency(AAmount: Double): string;
    
    property DefaultCurrency: string read FDefaultCurrency write FDefaultCurrency;
    property ContingencyPercent: Double read FContingencyPercent write FContingencyPercent;
  end;

implementation

{ TResourceTaskManager }

constructor TResourceTaskManager.Create;
begin
  inherited Create;
  SetLength(FResources, 0);
  SetLength(FTaskBudgets, 0);
  SetLength(FResourceAllocations, 0);
  FNextResourceID := 1;
  FNextBudgetID := 1;
  FNextAllocationID := 1;
  FDefaultCurrency := 'USD';
  FContingencyPercent := 10.0;
end;

destructor TResourceTaskManager.Destroy;
begin
  SetLength(FResources, 0);
  SetLength(FTaskBudgets, 0);
  SetLength(FResourceAllocations, 0);
  inherited Destroy;
end;

function TResourceTaskManager.FindResourceIndex(AResourceID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FResources) do
    if FResources[i].ID = AResourceID then
    begin
      Result := i;
      Exit;
    end;
end;

function TResourceTaskManager.FindBudgetIndex(ABudgetID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTaskBudgets) do
    if FTaskBudgets[i].ID = ABudgetID then
    begin
      Result := i;
      Exit;
    end;
end;

function TResourceTaskManager.FindAllocationIndex(AAllocationID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FResourceAllocations) do
    if FResourceAllocations[i].ID = AAllocationID then
    begin
      Result := i;
      Exit;
    end;
end;

function TResourceTaskManager.CalculateBudgetStatus(APlanned, AActual: Double): TBudgetStatus;
var
  variance: Double;
begin
  if APlanned <= 0 then
  begin
    Result := bsOnBudget;
    Exit;
  end;
  
  variance := ((AActual - APlanned) / APlanned) * 100;
  
  if variance < -5 then
    Result := bsUnderBudget
  else if variance <= 5 then
    Result := bsOnBudget
  else if variance <= 15 then
    Result := bsAtRisk
  else
    Result := bsOverBudget;
end;

function TResourceTaskManager.CalculateEAC(ATaskID: Integer): Double;
var
  budgets: TTaskBudgetArray;
  totalBudget, totalSpent: Double;
  i: Integer;
  task: TTask;
  taskIdx: Integer;
  progress: Double;
begin
  Result := 0;
  budgets := GetTaskBudget(ATaskID);
  if Length(budgets) = 0 then Exit;
  
  totalBudget := 0;
  totalSpent := 0;
  for i := 0 to High(budgets) do
  begin
    totalBudget := totalBudget + budgets[i].AllocatedAmount;
    totalSpent := totalSpent + budgets[i].SpentAmount;
  end;
  
  taskIdx := GetTaskByID(ATaskID);
  if taskIdx < 0 then
  begin
    Result := totalBudget;
    Exit;
  end;
  
  task := GetAllTasks[taskIdx];
  
  if task.Status = tsCompleted then
  begin
    Result := totalSpent;
    Exit;
  end;
  
  if (task.EstimatedHours > 0) and (task.ActualHours > 0) then
    progress := task.ActualHours / task.EstimatedHours
  else
    progress := 0.5;
  
  if progress > 0 then
    Result := totalSpent / progress
  else
    Result := totalBudget;
end;

function TResourceTaskManager.AddResource(const AName: string; AType: TResourceType;
  AUnitCost: Double; AQuantity: Double; const AUnitOfMeasure, ADescription: string): Integer;
var
  res: TResource;
begin
  res.ID := FNextResourceID;
  Inc(FNextResourceID);
  res.Name := AName;
  res.ResourceType := AType;
  res.UnitCost := AUnitCost;
  res.AvailableQuantity := AQuantity;
  res.UnitOfMeasure := AUnitOfMeasure;
  res.Description := ADescription;
  res.IsActive := True;
  res.DateAdded := Now;
  
  SetLength(FResources, Length(FResources) + 1);
  FResources[High(FResources)] := res;
  
  Result := res.ID;
end;

function TResourceTaskManager.UpdateResource(AResourceID: Integer; const AName: string;
  AUnitCost: Double; AQuantity: Double): Boolean;
var
  idx: Integer;
begin
  idx := FindResourceIndex(AResourceID);
  Result := idx >= 0;
  if not Result then Exit;
  
  FResources[idx].Name := AName;
  FResources[idx].UnitCost := AUnitCost;
  FResources[idx].AvailableQuantity := AQuantity;
end;

function TResourceTaskManager.DeactivateResource(AResourceID: Integer): Boolean;
var
  idx: Integer;
begin
  idx := FindResourceIndex(AResourceID);
  Result := idx >= 0;
  if Result then
    FResources[idx].IsActive := False;
end;

function TResourceTaskManager.GetResource(AResourceID: Integer): TResource;
var
  idx: Integer;
begin
  idx := FindResourceIndex(AResourceID);
  if idx >= 0 then
    Result := FResources[idx]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TResourceTaskManager.GetAllResources: TResourceArray;
begin
  Result := Copy(FResources, 0, Length(FResources));
end;

function TResourceTaskManager.GetActiveResources: TResourceArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FResources) do
    if FResources[i].IsActive then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FResources) do
    if FResources[i].IsActive then
    begin
      Result[count] := FResources[i];
      Inc(count);
    end;
end;

function TResourceTaskManager.GetResourcesByType(AType: TResourceType): TResourceArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FResources) do
    if (FResources[i].ResourceType = AType) and FResources[i].IsActive then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FResources) do
    if (FResources[i].ResourceType = AType) and FResources[i].IsActive then
    begin
      Result[count] := FResources[i];
      Inc(count);
    end;
end;

function TResourceTaskManager.SetTaskBudget(ATaskID: Integer; AAmount: Double;
  ACategory: TCostCategory; const ANotes: string): Integer;
var
  budget: TTaskBudget;
begin
  budget.ID := FNextBudgetID;
  Inc(FNextBudgetID);
  budget.TaskID := ATaskID;
  budget.AllocatedAmount := AAmount;
  budget.SpentAmount := 0;
  budget.CommittedAmount := 0;
  budget.CostCategory := ACategory;
  budget.Currency := FDefaultCurrency;
  budget.Notes := ANotes;
  budget.DateCreated := Now;
  budget.LastUpdated := Now;
  
  SetLength(FTaskBudgets, Length(FTaskBudgets) + 1);
  FTaskBudgets[High(FTaskBudgets)] := budget;
  
  Result := budget.ID;
end;

function TResourceTaskManager.UpdateTaskBudget(ABudgetID: Integer; AAmount: Double): Boolean;
var
  idx: Integer;
begin
  idx := FindBudgetIndex(ABudgetID);
  Result := idx >= 0;
  if not Result then Exit;
  
  FTaskBudgets[idx].AllocatedAmount := AAmount;
  FTaskBudgets[idx].LastUpdated := Now;
end;

function TResourceTaskManager.GetTaskBudget(ATaskID: Integer): TTaskBudgetArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FTaskBudgets) do
    if FTaskBudgets[i].TaskID = ATaskID then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FTaskBudgets) do
    if FTaskBudgets[i].TaskID = ATaskID then
    begin
      Result[count] := FTaskBudgets[i];
      Inc(count);
    end;
end;

function TResourceTaskManager.GetTotalTaskBudget(ATaskID: Integer): Double;
var
  budgets: TTaskBudgetArray;
  i: Integer;
begin
  Result := 0;
  budgets := GetTaskBudget(ATaskID);
  for i := 0 to High(budgets) do
    Result := Result + budgets[i].AllocatedAmount;
end;

function TResourceTaskManager.RecordExpense(ABudgetID: Integer; AAmount: Double;
  const ANotes: string): Boolean;
var
  idx: Integer;
begin
  idx := FindBudgetIndex(ABudgetID);
  Result := idx >= 0;
  if not Result then Exit;
  
  FTaskBudgets[idx].SpentAmount := FTaskBudgets[idx].SpentAmount + AAmount;
  FTaskBudgets[idx].LastUpdated := Now;
  FTaskBudgets[idx].Notes := FTaskBudgets[idx].Notes + #13#10 + 
    FormatDateTime('yyyy-mm-dd hh:nn', Now) + ': ' + ANotes;
end;

function TResourceTaskManager.CommitFunds(ABudgetID: Integer; AAmount: Double): Boolean;
var
  idx: Integer;
begin
  idx := FindBudgetIndex(ABudgetID);
  Result := idx >= 0;
  if not Result then Exit;
  
  FTaskBudgets[idx].CommittedAmount := FTaskBudgets[idx].CommittedAmount + AAmount;
  FTaskBudgets[idx].LastUpdated := Now;
end;

function TResourceTaskManager.AllocateResource(ATaskID, AResourceID: Integer;
  AQuantity: Double; const ANotes: string): Integer;
var
  alloc: TResourceAllocation;
  res: TResource;
begin
  res := GetResource(AResourceID);
  if res.ID = 0 then
  begin
    Result := -1;
    Exit;
  end;
  
  alloc.ID := FNextAllocationID;
  Inc(FNextAllocationID);
  alloc.TaskID := ATaskID;
  alloc.ResourceID := AResourceID;
  alloc.QuantityAllocated := AQuantity;
  alloc.QuantityUsed := 0;
  alloc.UnitCost := res.UnitCost;
  alloc.TotalCost := 0;
  alloc.DateAllocated := Now;
  alloc.DateCompleted := 0;
  alloc.Notes := ANotes;
  
  SetLength(FResourceAllocations, Length(FResourceAllocations) + 1);
  FResourceAllocations[High(FResourceAllocations)] := alloc;
  
  Result := alloc.ID;
end;

function TResourceTaskManager.RecordResourceUsage(AAllocationID: Integer;
  AQuantityUsed: Double): Boolean;
var
  idx: Integer;
begin
  idx := FindAllocationIndex(AAllocationID);
  Result := idx >= 0;
  if not Result then Exit;
  
  FResourceAllocations[idx].QuantityUsed := 
    FResourceAllocations[idx].QuantityUsed + AQuantityUsed;
  FResourceAllocations[idx].TotalCost := 
    FResourceAllocations[idx].QuantityUsed * FResourceAllocations[idx].UnitCost;
  FResourceAllocations[idx].DateCompleted := Now;
end;

function TResourceTaskManager.GetTaskAllocations(ATaskID: Integer): TResourceAllocationArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FResourceAllocations) do
    if FResourceAllocations[i].TaskID = ATaskID then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FResourceAllocations) do
    if FResourceAllocations[i].TaskID = ATaskID then
    begin
      Result[count] := FResourceAllocations[i];
      Inc(count);
    end;
end;

function TResourceTaskManager.GetResourceAllocations(AResourceID: Integer): TResourceAllocationArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FResourceAllocations) do
    if FResourceAllocations[i].ResourceID = AResourceID then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FResourceAllocations) do
    if FResourceAllocations[i].ResourceID = AResourceID then
    begin
      Result[count] := FResourceAllocations[i];
      Inc(count);
    end;
end;

function TResourceTaskManager.CalculateResourceCost(AAllocationID: Integer): Double;
var
  idx: Integer;
begin
  idx := FindAllocationIndex(AAllocationID);
  if idx >= 0 then
    Result := FResourceAllocations[idx].TotalCost
  else
    Result := 0;
end;

function TResourceTaskManager.GetBudgetVariance(ATaskID: Integer): TBudgetVariance;
var
  budgets: TTaskBudgetArray;
  allocs: TResourceAllocationArray;
  i: Integer;
  taskIdx: Integer;
  tasks: TTaskArray;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TaskID := ATaskID;
  
  budgets := GetTaskBudget(ATaskID);
  for i := 0 to High(budgets) do
  begin
    Result.PlannedCost := Result.PlannedCost + budgets[i].AllocatedAmount;
    Result.ActualCost := Result.ActualCost + budgets[i].SpentAmount;
  end;
  
  allocs := GetTaskAllocations(ATaskID);
  for i := 0 to High(allocs) do
    Result.ActualCost := Result.ActualCost + allocs[i].TotalCost;
  
  Result.Variance := Result.ActualCost - Result.PlannedCost;
  if Result.PlannedCost > 0 then
    Result.VariancePercent := (Result.Variance / Result.PlannedCost) * 100
  else
    Result.VariancePercent := 0;
  
  Result.Status := CalculateBudgetStatus(Result.PlannedCost, Result.ActualCost);
  
  taskIdx := GetTaskByID(ATaskID);
  if taskIdx >= 0 then
  begin
    tasks := GetAllTasks;
    Result.TaskTitle := tasks[taskIdx].Title;
  end
  else
    Result.TaskTitle := 'Unknown Task';
end;

function TResourceTaskManager.GetAllBudgetVariances: TBudgetVarianceArray;
var
  tasks: TTaskArray;
  i: Integer;
begin
  tasks := GetAllTasks;
  SetLength(Result, Length(tasks));
  for i := 0 to High(tasks) do
    Result[i] := GetBudgetVariance(tasks[i].ID);
end;

function TResourceTaskManager.GetOverBudgetTasks: TBudgetVarianceArray;
var
  all: TBudgetVarianceArray;
  i, count: Integer;
begin
  all := GetAllBudgetVariances;
  count := 0;
  for i := 0 to High(all) do
    if all[i].Status in [bsOverBudget, bsAtRisk] then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(all) do
    if all[i].Status in [bsOverBudget, bsAtRisk] then
    begin
      Result[count] := all[i];
      Inc(count);
    end;
end;

function TResourceTaskManager.ForecastTaskCost(ATaskID: Integer;
  AMethod: TForecastMethod): TFinancialForecast;
var
  variance: TBudgetVariance;
  eac: Double;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TaskID := ATaskID;
  variance := GetBudgetVariance(ATaskID);
  Result.TaskTitle := variance.TaskTitle;
  Result.CurrentSpent := variance.ActualCost;
  
  eac := CalculateEAC(ATaskID);
  Result.EstimatedFinalCost := eac;
  Result.BudgetRemaining := variance.PlannedCost - variance.ActualCost;
  Result.PredictedOverrun := eac - variance.PlannedCost;
  Result.ForecastDate := Now;
  Result.Method := AMethod;
  
  if variance.PlannedCost > 0 then
    Result.PercentComplete := (variance.ActualCost / variance.PlannedCost) * 100
  else
    Result.PercentComplete := 0;
  
  if Result.PredictedOverrun <= 0 then
    Result.Confidence := 85
  else if Result.PredictedOverrun < variance.PlannedCost * 0.1 then
    Result.Confidence := 70
  else
    Result.Confidence := 50;
end;

function TResourceTaskManager.GetDashboardMetrics: TDashboardMetrics;
var
  i: Integer;
  variances: TBudgetVarianceArray;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  for i := 0 to High(FTaskBudgets) do
  begin
    Result.TotalBudget := Result.TotalBudget + FTaskBudgets[i].AllocatedAmount;
    Result.TotalSpent := Result.TotalSpent + FTaskBudgets[i].SpentAmount;
    Result.TotalCommitted := Result.TotalCommitted + FTaskBudgets[i].CommittedAmount;
  end;
  
  for i := 0 to High(FResourceAllocations) do
    Result.TotalSpent := Result.TotalSpent + FResourceAllocations[i].TotalCost;
  
  if Result.TotalBudget > 0 then
    Result.BudgetUtilization := (Result.TotalSpent / Result.TotalBudget) * 100
  else
    Result.BudgetUtilization := 0;
  
  variances := GetAllBudgetVariances;
  for i := 0 to High(variances) do
  begin
    if variances[i].PlannedCost > 0 then
      Result.AverageVariance := Result.AverageVariance + variances[i].VariancePercent;
    if variances[i].Status = bsOverBudget then
      Inc(Result.TasksOverBudget)
    else if variances[i].Status = bsUnderBudget then
      Inc(Result.TasksUnderBudget);
    if variances[i].Variance > 0 then
      Result.ProjectedOverrun := Result.ProjectedOverrun + variances[i].Variance;
  end;
  
  if Length(variances) > 0 then
    Result.AverageVariance := Result.AverageVariance / Length(variances);
  
  if Result.TotalSpent > 0 then
    Result.ResourceUtilization := (Result.TotalSpent / 
      (Result.TotalBudget + Result.TotalCommitted)) * 100
  else
    Result.ResourceUtilization := 0;
  
  if Result.TotalSpent > 0 then
    Result.ROI := ((Result.TotalBudget - Result.TotalSpent) / Result.TotalSpent) * 100
  else
    Result.ROI := 0;
end;

function TResourceTaskManager.GetCostBreakdown: TCostBreakdownArray;
var
  i: Integer;
  total: Double;
  cat: TCostCategory;
  breakdown: array[TCostCategory] of Double;
begin
  for cat := Low(TCostCategory) to High(TCostCategory) do
    breakdown[cat] := 0;
  
  total := 0;
  for i := 0 to High(FTaskBudgets) do
  begin
    breakdown[FTaskBudgets[i].CostCategory] := 
      breakdown[FTaskBudgets[i].CostCategory] + FTaskBudgets[i].SpentAmount;
    total := total + FTaskBudgets[i].SpentAmount;
  end;
  
  SetLength(Result, Ord(High(TCostCategory)) + 1);
  for cat := Low(TCostCategory) to High(TCostCategory) do
  begin
    Result[Ord(cat)].Category := cat;
    Result[Ord(cat)].Amount := breakdown[cat];
    if total > 0 then
      Result[Ord(cat)].Percentage := (breakdown[cat] / total) * 100
    else
      Result[Ord(cat)].Percentage := 0;
  end;
end;

function TResourceTaskManager.GetResourceUtilization: string;
var
  i, j: Integer;
  res: TResource;
  allocs: TResourceAllocationArray;
  totalAllocated, totalUsed: Double;
begin
  Result := 'Resource Utilization Report' + #13#10;
  Result := Result + '=============================' + #13#10#13#10;
  
  for i := 0 to High(FResources) do
  begin
    res := FResources[i];
    if not res.IsActive then Continue;
    
    allocs := GetResourceAllocations(res.ID);
    totalAllocated := 0;
    totalUsed := 0;
    
    for j := 0 to High(allocs) do
    begin
      totalAllocated := totalAllocated + allocs[j].QuantityAllocated;
      totalUsed := totalUsed + allocs[j].QuantityUsed;
    end;
    
    Result := Result + Format('%s (%s):', [res.Name, ResourceTypeToString(res.ResourceType)]) + #13#10;
    Result := Result + Format('  Available: %.2f %s', [res.AvailableQuantity, res.UnitOfMeasure]) + #13#10;
    Result := Result + Format('  Allocated: %.2f %s', [totalAllocated, res.UnitOfMeasure]) + #13#10;
    Result := Result + Format('  Used: %.2f %s', [totalUsed, res.UnitOfMeasure]) + #13#10;
    if totalAllocated > 0 then
      Result := Result + Format('  Utilization: %.1f%%', 
        [(totalUsed / totalAllocated) * 100]) + #13#10;
    Result := Result + #13#10;
  end;
end;

function TResourceTaskManager.CalculateROI(ATaskID: Integer; AExpectedValue: Double): Double;
var
  variance: TBudgetVariance;
begin
  variance := GetBudgetVariance(ATaskID);
  if variance.ActualCost > 0 then
    Result := ((AExpectedValue - variance.ActualCost) / variance.ActualCost) * 100
  else
    Result := 0;
end;

function TResourceTaskManager.GenerateBudgetReport: string;
var
  variances: TBudgetVarianceArray;
  i: Integer;
begin
  Result := 'Budget Status Report' + #13#10;
  Result := Result + '====================' + #13#10#13#10;
  
  variances := GetAllBudgetVariances;
  for i := 0 to High(variances) do
  begin
    Result := Result + Format('Task: %s (ID: %d)', 
      [variances[i].TaskTitle, variances[i].TaskID]) + #13#10;
    Result := Result + Format('  Planned: %s', 
      [FormatCurrency(variances[i].PlannedCost)]) + #13#10;
    Result := Result + Format('  Actual: %s', 
      [FormatCurrency(variances[i].ActualCost)]) + #13#10;
    Result := Result + Format('  Variance: %s (%.1f%%)', 
      [FormatCurrency(variances[i].Variance), variances[i].VariancePercent]) + #13#10;
    Result := Result + Format('  Status: %s', 
      [BudgetStatusToString(variances[i].Status)]) + #13#10#13#10;
  end;
end;

function TResourceTaskManager.GenerateResourceReport: string;
begin
  Result := GetResourceUtilization;
end;

function TResourceTaskManager.GenerateFinancialSummary: string;
var
  metrics: TDashboardMetrics;
  breakdown: TCostBreakdownArray;
  i: Integer;
begin
  metrics := GetDashboardMetrics;
  breakdown := GetCostBreakdown;
  
  Result := 'Financial Summary' + #13#10;
  Result := Result + '=================' + #13#10#13#10;
  Result := Result + Format('Total Budget: %s', [FormatCurrency(metrics.TotalBudget)]) + #13#10;
  Result := Result + Format('Total Spent: %s', [FormatCurrency(metrics.TotalSpent)]) + #13#10;
  Result := Result + Format('Total Committed: %s', [FormatCurrency(metrics.TotalCommitted)]) + #13#10;
  Result := Result + Format('Budget Utilization: %.1f%%', [metrics.BudgetUtilization]) + #13#10;
  Result := Result + Format('Average Variance: %.1f%%', [metrics.AverageVariance]) + #13#10;
  Result := Result + Format('Tasks Over Budget: %d', [metrics.TasksOverBudget]) + #13#10;
  Result := Result + Format('Tasks Under Budget: %d', [metrics.TasksUnderBudget]) + #13#10;
  Result := Result + Format('Projected Overrun: %s', 
    [FormatCurrency(metrics.ProjectedOverrun)]) + #13#10#13#10;
  
  Result := Result + 'Cost Breakdown by Category:' + #13#10;
  for i := 0 to High(breakdown) do
    if breakdown[i].Amount > 0 then
      Result := Result + Format('  %s: %s (%.1f%%)', 
        [CostCategoryToString(breakdown[i].Category), 
         FormatCurrency(breakdown[i].Amount), 
         breakdown[i].Percentage]) + #13#10;
end;

function TResourceTaskManager.GenerateCostTrendAnalysis: string;
var
  tasks: TTaskArray;
  i: Integer;
  forecast: TFinancialForecast;
begin
  Result := 'Cost Trend Analysis' + #13#10;
  Result := Result + '===================' + #13#10#13#10;
  
  tasks := GetAllTasks;
  for i := 0 to High(tasks) do
  begin
    if tasks[i].Status = tsCompleted then Continue;
    
    forecast := ForecastTaskCost(tasks[i].ID, fmLinear);
    if forecast.CurrentSpent > 0 then
    begin
      Result := Result + Format('Task: %s', [forecast.TaskTitle]) + #13#10;
      Result := Result + Format('  Current Spent: %s', 
        [FormatCurrency(forecast.CurrentSpent)]) + #13#10;
      Result := Result + Format('  Estimated Final Cost: %s', 
        [FormatCurrency(forecast.EstimatedFinalCost)]) + #13#10;
      Result := Result + Format('  Budget Remaining: %s', 
        [FormatCurrency(forecast.BudgetRemaining)]) + #13#10;
      if forecast.PredictedOverrun > 0 then
        Result := Result + Format('  Predicted Overrun: %s', 
          [FormatCurrency(forecast.PredictedOverrun)]) + #13#10;
      Result := Result + Format('  Confidence: %.0f%%', [forecast.Confidence]) + #13#10#13#10;
    end;
  end;
end;

function TResourceTaskManager.ExportFinancialData: string;
var
  variances: TBudgetVarianceArray;
  i: Integer;
begin
  Result := 'TaskID,TaskTitle,PlannedCost,ActualCost,Variance,VariancePercent,Status' + #13#10;
  
  variances := GetAllBudgetVariances;
  for i := 0 to High(variances) do
  begin
    Result := Result + Format('%d,"%s",%.2f,%.2f,%.2f,%.2f,%s',
      [variances[i].TaskID,
       variances[i].TaskTitle,
       variances[i].PlannedCost,
       variances[i].ActualCost,
       variances[i].Variance,
       variances[i].VariancePercent,
       BudgetStatusToString(variances[i].Status)]) + #13#10;
  end;
end;

function TResourceTaskManager.ResourceTypeToString(AType: TResourceType): string;
begin
  case AType of
    rtHuman: Result := 'Human Resource';
    rtMaterial: Result := 'Material';
    rtEquipment: Result := 'Equipment';
    rtFinancial: Result := 'Financial';
    rtTime: Result := 'Time';
  else
    Result := 'Unknown';
  end;
end;

function TResourceTaskManager.CostCategoryToString(ACategory: TCostCategory): string;
begin
  case ACategory of
    ccLabor: Result := 'Labor';
    ccMaterials: Result := 'Materials';
    ccEquipment: Result := 'Equipment';
    ccOverhead: Result := 'Overhead';
    ccContingency: Result := 'Contingency';
    ccOther: Result := 'Other';
  else
    Result := 'Unknown';
  end;
end;

function TResourceTaskManager.BudgetStatusToString(AStatus: TBudgetStatus): string;
begin
  case AStatus of
    bsUnderBudget: Result := 'Under Budget';
    bsOnBudget: Result := 'On Budget';
    bsOverBudget: Result := 'Over Budget';
    bsAtRisk: Result := 'At Risk';
  else
    Result := 'Unknown';
  end;
end;

function TResourceTaskManager.FormatCurrency(AAmount: Double): string;
begin
  Result := Format('%s %.2f', [FDefaultCurrency, AAmount]);
end;

end.
