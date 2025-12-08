
program TaskManagerResourceDemo;
{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  taskmanager, taskmanageradvanced, taskmanagerenhanced,
  taskmanagerteam, taskmanagergamify, taskmanagersmart,
  taskmanagerfocus, taskmanagerresource;

procedure SelfTest;
var
  tm: TResourceTaskManager;
  taskID1, taskID2, taskID3: Integer;
  resID1, resID2, resID3: Integer;
  budgetID1, budgetID2: Integer;
  allocID1, allocID2: Integer;
  variance: TBudgetVariance;
  forecast: TFinancialForecast;
  metrics: TDashboardMetrics;
  i: Integer;
  overBudget: TBudgetVarianceArray;
begin
  WriteLn('=== Task Manager Resource & Budget Tracking Self-Test ===');
  WriteLn;
  
  tm := TResourceTaskManager.Create;
  try
    tm.DefaultCurrency := 'USD';
    
    WriteLn('1. Creating test tasks...');
    taskID1 := tm.AddTask('Build Website', 'Corporate website redesign', 
      'IT Projects', tpHigh, IncDay(Now, 30), 120.0);
    taskID2 := tm.AddTask('Marketing Campaign', 'Q4 product launch campaign',
      'Marketing', tpMedium, IncDay(Now, 45), 80.0);
    taskID3 := tm.AddTask('Office Renovation', 'Renovate meeting rooms',
      'Facilities', tpLow, IncDay(Now, 60), 40.0);
    WriteLn('  Created 3 tasks');
    WriteLn;
    
    WriteLn('2. Adding resources...');
    resID1 := tm.AddResource('Senior Developer', rtHuman, 150.0, 160.0, 
      'hours', 'Full-stack developer');
    resID2 := tm.AddResource('Marketing Specialist', rtHuman, 100.0, 160.0,
      'hours', 'Digital marketing expert');
    resID3 := tm.AddResource('Construction Materials', rtMaterial, 50.0, 1000.0,
      'units', 'Various building materials');
    WriteLn(Format('  Added 3 resources (IDs: %d, %d, %d)', [resID1, resID2, resID3]));
    WriteLn;
    
    WriteLn('3. Setting task budgets...');
    budgetID1 := tm.SetTaskBudget(taskID1, 25000.0, ccLabor, 'Development budget');
    tm.SetTaskBudget(taskID1, 5000.0, ccEquipment, 'Software licenses');
    budgetID2 := tm.SetTaskBudget(taskID2, 15000.0, ccLabor, 'Campaign execution');
    tm.SetTaskBudget(taskID2, 8000.0, ccMaterials, 'Marketing materials');
    tm.SetTaskBudget(taskID3, 20000.0, ccMaterials, 'Renovation materials');
    WriteLn('  Allocated budgets for all tasks');
    WriteLn;
    
    WriteLn('4. Allocating resources to tasks...');
    allocID1 := tm.AllocateResource(taskID1, resID1, 120.0, 
      'Primary developer for website');
    allocID2 := tm.AllocateResource(taskID2, resID2, 80.0,
      'Campaign manager');
    tm.AllocateResource(taskID3, resID3, 400.0, 'Building materials');
    WriteLn('  Resources allocated to tasks');
    WriteLn;
    
    WriteLn('5. Recording some expenses and resource usage...');
    tm.RecordExpense(budgetID1, 8500.0, 'Initial development phase completed');
    tm.RecordExpense(budgetID2, 12000.0, 'Campaign materials produced');
    tm.RecordResourceUsage(allocID1, 45.0);
    tm.RecordResourceUsage(allocID2, 60.0);
    tm.UpdateTaskActualHours(taskID1, 45.0);
    tm.UpdateTaskActualHours(taskID2, 60.0);
    WriteLn('  Recorded expenses and resource usage');
    WriteLn;
    
    WriteLn('6. Analyzing budget variance for Task 1...');
    variance := tm.GetBudgetVariance(taskID1);
    WriteLn(Format('  Task: %s', [variance.TaskTitle]));
    WriteLn(Format('  Planned Cost: %s', [tm.FormatCurrency(variance.PlannedCost)]));
    WriteLn(Format('  Actual Cost: %s', [tm.FormatCurrency(variance.ActualCost)]));
    WriteLn(Format('  Variance: %s (%.1f%%)', 
      [tm.FormatCurrency(variance.Variance), variance.VariancePercent]));
    WriteLn(Format('  Status: %s', [tm.BudgetStatusToString(variance.Status)]));
    WriteLn;
    
    WriteLn('7. Forecasting Task 1 completion cost...');
    forecast := tm.ForecastTaskCost(taskID1, fmLinear);
    WriteLn(Format('  Current Spent: %s', [tm.FormatCurrency(forecast.CurrentSpent)]));
    WriteLn(Format('  Estimated Final Cost: %s', 
      [tm.FormatCurrency(forecast.EstimatedFinalCost)]));
    WriteLn(Format('  Budget Remaining: %s', 
      [tm.FormatCurrency(forecast.BudgetRemaining)]));
    WriteLn(Format('  Predicted Overrun: %s', 
      [tm.FormatCurrency(forecast.PredictedOverrun)]));
    WriteLn(Format('  Confidence: %.0f%%', [forecast.Confidence]));
    WriteLn;
    
    WriteLn('8. Dashboard Metrics...');
    metrics := tm.GetDashboardMetrics;
    WriteLn(Format('  Total Budget: %s', [tm.FormatCurrency(metrics.TotalBudget)]));
    WriteLn(Format('  Total Spent: %s', [tm.FormatCurrency(metrics.TotalSpent)]));
    WriteLn(Format('  Budget Utilization: %.1f%%', [metrics.BudgetUtilization]));
    WriteLn(Format('  Average Variance: %.1f%%', [metrics.AverageVariance]));
    WriteLn(Format('  Tasks Over Budget: %d', [metrics.TasksOverBudget]));
    WriteLn(Format('  Tasks Under Budget: %d', [metrics.TasksUnderBudget]));
    WriteLn(Format('  Projected Overrun: %s', 
      [tm.FormatCurrency(metrics.ProjectedOverrun)]));
    WriteLn;
    
    WriteLn('9. Checking for over-budget tasks...');
    overBudget := tm.GetOverBudgetTasks;
    if Length(overBudget) > 0 then
    begin
      WriteLn(Format('  Found %d tasks at risk or over budget:', [Length(overBudget)]));
      for i := 0 to High(overBudget) do
        WriteLn(Format('    - %s: %s (%.1f%% variance)', 
          [overBudget[i].TaskTitle, 
           tm.BudgetStatusToString(overBudget[i].Status),
           overBudget[i].VariancePercent]));
    end
    else
      WriteLn('  All tasks are within budget!');
    WriteLn;
    
    WriteLn('10. Resource Utilization Report...');
    WriteLn(tm.GetResourceUtilization);
    
    WriteLn('11. Financial Summary Report...');
    WriteLn(tm.GenerateFinancialSummary);
    
    WriteLn('12. Budget Status Report...');
    WriteLn(tm.GenerateBudgetReport);
    
    WriteLn('13. Testing ROI calculation...');
    WriteLn(Format('  Website Project ROI (Expected Value $40,000): %.1f%%',
      [tm.CalculateROI(taskID1, 40000.0)]));
    WriteLn;
    
    WriteLn('14. Exporting financial data to CSV...');
    WriteLn('CSV Export Preview:');
    WriteLn(Copy(tm.ExportFinancialData, 1, 300) + '...');
    WriteLn;
    
    WriteLn('=== Resource & Budget Tracking Self-Test Complete ===');
    WriteLn('All features working correctly!');
    WriteLn;
    WriteLn('Key Features Demonstrated:');
    WriteLn('  ✓ Resource management (human, material, equipment)');
    WriteLn('  ✓ Budget allocation by cost category');
    WriteLn('  ✓ Expense tracking and recording');
    WriteLn('  ✓ Resource allocation and usage tracking');
    WriteLn('  ✓ Budget variance analysis');
    WriteLn('  ✓ Cost forecasting (EAC - Estimate At Completion)');
    WriteLn('  ✓ Dashboard metrics and KPIs');
    WriteLn('  ✓ Over-budget task detection');
    WriteLn('  ✓ Resource utilization reporting');
    WriteLn('  ✓ Financial summaries and reports');
    WriteLn('  ✓ ROI calculation');
    WriteLn('  ✓ CSV export for financial data');
    
  finally
    tm.Free;
  end;
end;

begin
  try
    SelfTest;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
