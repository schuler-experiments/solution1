
unit taskmanagerext;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, DateUtils, Math, Classes, taskmanager;

type
  // Recurrence pattern enumeration
  TRecurrencePattern = (rpNone, rpDaily, rpWeekly, rpBiWeekly, rpMonthly, rpQuarterly, rpYearly);
  
  // Extended task record with new features
  TExtendedTask = record
    BaseTask: TTask;  // Inherits all basic task fields
    ParentID: Integer;  // For subtasks (0 = no parent)
    RecurrencePattern: TRecurrencePattern;
    RecurrenceCount: Integer;  // How many times it has recurred
    NextRecurrenceDate: TDateTime;
    PriorityScore: Double;  // Auto-calculated priority score
    IsRecurring: Boolean;
    IsSubtask: Boolean;
    SubtaskIDs: array of Integer;  // Child task IDs
    LastModifiedDate: TDateTime;
    NotificationDays: Integer;  // Days before due date to notify
  end;
  
  // Dynamic array of extended tasks
  TExtendedTaskArray = array of TExtendedTask;
  
  // Batch operation result
  TBatchOperationResult = record
    SuccessCount: Integer;
    FailureCount: Integer;
    TotalProcessed: Integer;
    Message: string;
  end;
  
  // Extended Task Manager class
  TExtendedTaskManager = class(TTaskManager)
  private
    FExtendedTasks: TExtendedTaskArray;
    function CalculatePriorityScore(const ATask: TExtendedTask): Double;
    function GetDaysUntilDue(const ATask: TExtendedTask): Integer;
    procedure ProcessRecurringTasks;
    function CreateRecurringInstance(const ATask: TExtendedTask): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Enhanced task operations
    function AddExtendedTask(const ATitle, ADescription, ACategory: string;
                            APriority: TTaskPriority; ADueDate: TDateTime;
                            AEstimatedHours: Double; ARecurrence: TRecurrencePattern): Integer;
    function AddSubtask(AParentID: Integer; const ATitle, ADescription: string;
                       APriority: TTaskPriority; ADueDate: TDateTime): Integer;
    function GetSubtasks(AParentID: Integer): TExtendedTaskArray;
    function GetTaskHierarchy(ATaskID: Integer): string;
    
    // Recurring task management
    function SetTaskRecurrence(ATaskID: Integer; APattern: TRecurrencePattern): Boolean;
    function GetRecurringTasks: TExtendedTaskArray;
    function GenerateNextRecurrence(ATaskID: Integer): Integer;
    procedure UpdateAllRecurringTasks;
    
    // Priority scoring and smart suggestions
    function UpdatePriorityScores: Integer;
    function GetTopPriorityTasks(ACount: Integer): TExtendedTaskArray;
    function GetTasksNeedingAttention: TExtendedTaskArray;
    function GetTasksDueSoon(ADays: Integer): TExtendedTaskArray;
    
    // Batch operations
    function BatchUpdateStatus(const ATaskIDs: array of Integer; 
                              ANewStatus: TTaskStatus): TBatchOperationResult;
    function BatchUpdatePriority(const ATaskIDs: array of Integer;
                                ANewPriority: TTaskPriority): TBatchOperationResult;
    function BatchUpdateCategory(const ATaskIDs: array of Integer;
                                const ANewCategory: string): TBatchOperationResult;
    function BatchDeleteTasks(const ATaskIDs: array of Integer): TBatchOperationResult;
    function BatchAddTag(const ATaskIDs: array of Integer;
                        const ATag: string): TBatchOperationResult;
    
    // Advanced analytics
    function GetProductivityReport: string;
    function GetCategoryPerformance: string;
    function GetTimeManagementReport: string;
    function GetTaskComplexityAnalysis: string;
    
    // Utility functions
    function RecurrencePatternToString(APattern: TRecurrencePattern): string;
    function ExtendedTaskToString(const ATask: TExtendedTask): string;
    function GetAllExtendedTasks: TExtendedTaskArray;
    
    // Import/Export extended format
    function ExportExtendedToCSV: string;
    function SaveExtendedToFile(const AFilename: string): Boolean;
    function LoadExtendedFromFile(const AFilename: string): Boolean;
  end;

implementation

{ TExtendedTaskManager }

constructor TExtendedTaskManager.Create;
begin
  inherited Create;
  SetLength(FExtendedTasks, 0);
end;

destructor TExtendedTaskManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FExtendedTasks) - 1 do
    SetLength(FExtendedTasks[i].SubtaskIDs, 0);
  SetLength(FExtendedTasks, 0);
  inherited Destroy;
end;

function TExtendedTaskManager.CalculatePriorityScore(const ATask: TExtendedTask): Double;
var
  PriorityWeight, UrgencyWeight, StatusWeight: Double;
  DaysUntilDue: Integer;
begin
  // Priority weight (0-10 scale)
  case ATask.BaseTask.Priority of
    tpLow: PriorityWeight := 2.5;
    tpMedium: PriorityWeight := 5.0;
    tpHigh: PriorityWeight := 7.5;
    tpCritical: PriorityWeight := 10.0;
  else
    PriorityWeight := 0.0;
  end;
  
  // Urgency based on due date
  DaysUntilDue := GetDaysUntilDue(ATask);
  if DaysUntilDue < 0 then
    UrgencyWeight := 10.0  // Overdue
  else if DaysUntilDue = 0 then
    UrgencyWeight := 9.0   // Due today
  else if DaysUntilDue <= 3 then
    UrgencyWeight := 7.0   // Due within 3 days
  else if DaysUntilDue <= 7 then
    UrgencyWeight := 5.0   // Due within a week
  else if DaysUntilDue <= 14 then
    UrgencyWeight := 3.0   // Due within 2 weeks
  else
    UrgencyWeight := 1.0;  // Due later
  
  // Status weight
  case ATask.BaseTask.Status of
    tsNotStarted: StatusWeight := 1.2;  // Boost for not started
    tsInProgress: StatusWeight := 1.5;  // Highest boost for in progress
    tsOnHold: StatusWeight := 0.8;      // Lower for on hold
    tsCompleted: StatusWeight := 0.0;   // Zero for completed
    tsCancelled: StatusWeight := 0.0;   // Zero for cancelled
  else
    StatusWeight := 1.0;
  end;
  
  // Calculate final score (weighted average)
  Result := (PriorityWeight * 0.4 + UrgencyWeight * 0.5) * StatusWeight;
  
  // Boost for subtasks (encourage completing parent tasks)
  if not ATask.IsSubtask and (Length(ATask.SubtaskIDs) > 0) then
    Result := Result * 1.1;
end;

function TExtendedTaskManager.GetDaysUntilDue(const ATask: TExtendedTask): Integer;
begin
  Result := DaysBetween(Now, ATask.BaseTask.DueDate);
  if ATask.BaseTask.DueDate < Now then
    Result := -Result;  // Negative for overdue
end;

function TExtendedTaskManager.AddExtendedTask(const ATitle, ADescription, ACategory: string;
  APriority: TTaskPriority; ADueDate: TDateTime; AEstimatedHours: Double;
  ARecurrence: TRecurrencePattern): Integer;
var
  NewTask: TExtendedTask;
  TaskIndex: Integer;
  BaseID: Integer;
begin
  // Add to base task manager
  BaseID := inherited AddTask(ATitle, ADescription, ACategory, APriority, ADueDate, AEstimatedHours);
  
  // Create extended task
  NewTask.BaseTask := GetAllTasks[BaseID - 1];
  NewTask.ParentID := 0;
  NewTask.RecurrencePattern := ARecurrence;
  NewTask.RecurrenceCount := 0;
  NewTask.NextRecurrenceDate := 0;
  NewTask.PriorityScore := 0.0;
  NewTask.IsRecurring := (ARecurrence <> rpNone);
  NewTask.IsSubtask := False;
  SetLength(NewTask.SubtaskIDs, 0);
  NewTask.LastModifiedDate := Now;
  NewTask.NotificationDays := 3;  // Default 3 days notification
  
  if NewTask.IsRecurring then
  begin
    case ARecurrence of
      rpDaily: NewTask.NextRecurrenceDate := IncDay(ADueDate, 1);
      rpWeekly: NewTask.NextRecurrenceDate := IncWeek(ADueDate, 1);
      rpBiWeekly: NewTask.NextRecurrenceDate := IncWeek(ADueDate, 2);
      rpMonthly: NewTask.NextRecurrenceDate := IncMonth(ADueDate, 1);
      rpQuarterly: NewTask.NextRecurrenceDate := IncMonth(ADueDate, 3);
      rpYearly: NewTask.NextRecurrenceDate := IncYear(ADueDate, 1);
    end;
  end;
  
  NewTask.PriorityScore := CalculatePriorityScore(NewTask);
  
  // Add to extended tasks array
  TaskIndex := Length(FExtendedTasks);
  SetLength(FExtendedTasks, TaskIndex + 1);
  FExtendedTasks[TaskIndex] := NewTask;
  
  Result := BaseID;
end;

function TExtendedTaskManager.AddSubtask(AParentID: Integer; const ATitle, ADescription: string;
  APriority: TTaskPriority; ADueDate: TDateTime): Integer;
var
  NewTask: TExtendedTask;
  TaskIndex, ParentIndex, i: Integer;
  BaseID: Integer;
begin
  Result := -1;
  
  // Find parent task
  ParentIndex := -1;
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    if FExtendedTasks[i].BaseTask.ID = AParentID then
    begin
      ParentIndex := i;
      Break;
    end;
  end;
  
  if ParentIndex = -1 then
    Exit;  // Parent not found
  
  // Add to base task manager
  BaseID := inherited AddTask(ATitle, ADescription, FExtendedTasks[ParentIndex].BaseTask.Category,
                              APriority, ADueDate, 0.0);
  
  // Create extended task as subtask
  NewTask.BaseTask := GetAllTasks[BaseID - 1];
  NewTask.ParentID := AParentID;
  NewTask.RecurrencePattern := rpNone;
  NewTask.RecurrenceCount := 0;
  NewTask.NextRecurrenceDate := 0;
  NewTask.PriorityScore := 0.0;
  NewTask.IsRecurring := False;
  NewTask.IsSubtask := True;
  SetLength(NewTask.SubtaskIDs, 0);
  NewTask.LastModifiedDate := Now;
  NewTask.NotificationDays := 3;
  
  NewTask.PriorityScore := CalculatePriorityScore(NewTask);
  
  // Add to extended tasks array
  TaskIndex := Length(FExtendedTasks);
  SetLength(FExtendedTasks, TaskIndex + 1);
  FExtendedTasks[TaskIndex] := NewTask;
  
  // Update parent's subtask list
  TaskIndex := Length(FExtendedTasks[ParentIndex].SubtaskIDs);
  SetLength(FExtendedTasks[ParentIndex].SubtaskIDs, TaskIndex + 1);
  FExtendedTasks[ParentIndex].SubtaskIDs[TaskIndex] := BaseID;
  
  Result := BaseID;
end;

function TExtendedTaskManager.GetSubtasks(AParentID: Integer): TExtendedTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    if FExtendedTasks[i].ParentID = AParentID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FExtendedTasks[i];
      Inc(Count);
    end;
  end;
end;

function TExtendedTaskManager.GetTaskHierarchy(ATaskID: Integer): string;
var
  i: Integer;
  Task: TExtendedTask;
  Subtasks: TExtendedTaskArray;
  Found: Boolean;
begin
  Result := '';
  Found := False;
  
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    if FExtendedTasks[i].BaseTask.ID = ATaskID then
    begin
      Task := FExtendedTasks[i];
      Found := True;
      Break;
    end;
  end;
  
  if not Found then
    Exit;
  
  Result := Format('Task #%d: %s [Priority Score: %.2f]', 
                   [Task.BaseTask.ID, Task.BaseTask.Title, Task.PriorityScore]) + sLineBreak;
  
  Subtasks := GetSubtasks(ATaskID);
  if Length(Subtasks) > 0 then
  begin
    Result := Result + Format('  Subtasks (%d):', [Length(Subtasks)]) + sLineBreak;
    for i := 0 to Length(Subtasks) - 1 do
    begin
      Result := Result + Format('    - #%d: %s [%s]', 
                               [Subtasks[i].BaseTask.ID, 
                                Subtasks[i].BaseTask.Title,
                                TaskStatusToString(Subtasks[i].BaseTask.Status)]) + sLineBreak;
    end;
  end;
end;

function TExtendedTaskManager.SetTaskRecurrence(ATaskID: Integer; 
  APattern: TRecurrencePattern): Boolean;
var
  i: Integer;
begin
  Result := False;
  
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    if FExtendedTasks[i].BaseTask.ID = ATaskID then
    begin
      FExtendedTasks[i].RecurrencePattern := APattern;
      FExtendedTasks[i].IsRecurring := (APattern <> rpNone);
      
      if FExtendedTasks[i].IsRecurring then
      begin
        case APattern of
          rpDaily: FExtendedTasks[i].NextRecurrenceDate := IncDay(FExtendedTasks[i].BaseTask.DueDate, 1);
          rpWeekly: FExtendedTasks[i].NextRecurrenceDate := IncWeek(FExtendedTasks[i].BaseTask.DueDate, 1);
          rpBiWeekly: FExtendedTasks[i].NextRecurrenceDate := IncWeek(FExtendedTasks[i].BaseTask.DueDate, 2);
          rpMonthly: FExtendedTasks[i].NextRecurrenceDate := IncMonth(FExtendedTasks[i].BaseTask.DueDate, 1);
          rpQuarterly: FExtendedTasks[i].NextRecurrenceDate := IncMonth(FExtendedTasks[i].BaseTask.DueDate, 3);
          rpYearly: FExtendedTasks[i].NextRecurrenceDate := IncYear(FExtendedTasks[i].BaseTask.DueDate, 1);
        end;
      end;
      
      Result := True;
      Break;
    end;
  end;
end;

function TExtendedTaskManager.GetRecurringTasks: TExtendedTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    if FExtendedTasks[i].IsRecurring then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FExtendedTasks[i];
      Inc(Count);
    end;
  end;
end;

function TExtendedTaskManager.GenerateNextRecurrence(ATaskID: Integer): Integer;
var
  i: Integer;
  SourceTask: TExtendedTask;
  Found: Boolean;
begin
  Result := -1;
  Found := False;
  
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    if FExtendedTasks[i].BaseTask.ID = ATaskID then
    begin
      SourceTask := FExtendedTasks[i];
      Found := True;
      Break;
    end;
  end;
  
  if not Found or not SourceTask.IsRecurring then
    Exit;
  
  // Create new instance
  Result := AddExtendedTask(
    SourceTask.BaseTask.Title,
    SourceTask.BaseTask.Description,
    SourceTask.BaseTask.Category,
    SourceTask.BaseTask.Priority,
    SourceTask.NextRecurrenceDate,
    SourceTask.BaseTask.EstimatedHours,
    SourceTask.RecurrencePattern
  );
  
  // Update recurrence count
  FExtendedTasks[i].RecurrenceCount := FExtendedTasks[i].RecurrenceCount + 1;
end;

procedure TExtendedTaskManager.UpdateAllRecurringTasks;
var
  i: Integer;
begin
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    if FExtendedTasks[i].IsRecurring and 
       (FExtendedTasks[i].BaseTask.Status = tsCompleted) and
       (FExtendedTasks[i].NextRecurrenceDate <= Now) then
    begin
      GenerateNextRecurrence(FExtendedTasks[i].BaseTask.ID);
    end;
  end;
end;

function TExtendedTaskManager.UpdatePriorityScores: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    FExtendedTasks[i].PriorityScore := CalculatePriorityScore(FExtendedTasks[i]);
    Inc(Result);
  end;
end;

function TExtendedTaskManager.GetTopPriorityTasks(ACount: Integer): TExtendedTaskArray;
var
  i, j, Count: Integer;
  TempTask: TExtendedTask;
  SortedTasks: TExtendedTaskArray;
begin
  // Create a copy of all non-completed tasks
  SetLength(SortedTasks, 0);
  Count := 0;
  
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    if (FExtendedTasks[i].BaseTask.Status <> tsCompleted) and
       (FExtendedTasks[i].BaseTask.Status <> tsCancelled) then
    begin
      SetLength(SortedTasks, Count + 1);
      SortedTasks[Count] := FExtendedTasks[i];
      Inc(Count);
    end;
  end;
  
  // Simple bubble sort by priority score (descending)
  for i := 0 to Length(SortedTasks) - 2 do
  begin
    for j := i + 1 to Length(SortedTasks) - 1 do
    begin
      if SortedTasks[j].PriorityScore > SortedTasks[i].PriorityScore then
      begin
        TempTask := SortedTasks[i];
        SortedTasks[i] := SortedTasks[j];
        SortedTasks[j] := TempTask;
      end;
    end;
  end;
  
  // Return top ACount tasks
  if ACount > Length(SortedTasks) then
    ACount := Length(SortedTasks);
  
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := SortedTasks[i];
end;

function TExtendedTaskManager.GetTasksNeedingAttention: TExtendedTaskArray;
var
  i, Count: Integer;
  DaysUntilDue: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    DaysUntilDue := GetDaysUntilDue(FExtendedTasks[i]);
    
    // Tasks needing attention: overdue, due soon, or high priority not started
    if ((DaysUntilDue <= 3) or 
        ((FExtendedTasks[i].BaseTask.Priority in [tpHigh, tpCritical]) and 
         (FExtendedTasks[i].BaseTask.Status = tsNotStarted))) and
       (FExtendedTasks[i].BaseTask.Status <> tsCompleted) and
       (FExtendedTasks[i].BaseTask.Status <> tsCancelled) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FExtendedTasks[i];
      Inc(Count);
    end;
  end;
end;

function TExtendedTaskManager.GetTasksDueSoon(ADays: Integer): TExtendedTaskArray;
var
  i, Count: Integer;
  DaysUntilDue: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    DaysUntilDue := GetDaysUntilDue(FExtendedTasks[i]);
    
    if (DaysUntilDue >= 0) and (DaysUntilDue <= ADays) and
       (FExtendedTasks[i].BaseTask.Status <> tsCompleted) and
       (FExtendedTasks[i].BaseTask.Status <> tsCancelled) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FExtendedTasks[i];
      Inc(Count);
    end;
  end;
end;

function TExtendedTaskManager.BatchUpdateStatus(const ATaskIDs: array of Integer;
  ANewStatus: TTaskStatus): TBatchOperationResult;
var
  i: Integer;
begin
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.TotalProcessed := Length(ATaskIDs);
  
  for i := 0 to Length(ATaskIDs) - 1 do
  begin
    if UpdateTaskStatus(ATaskIDs[i], ANewStatus) then
      Inc(Result.SuccessCount)
    else
      Inc(Result.FailureCount);
  end;
  
  Result.Message := Format('Batch status update: %d succeeded, %d failed out of %d tasks',
                          [Result.SuccessCount, Result.FailureCount, Result.TotalProcessed]);
end;

function TExtendedTaskManager.BatchUpdatePriority(const ATaskIDs: array of Integer;
  ANewPriority: TTaskPriority): TBatchOperationResult;
var
  i: Integer;
begin
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.TotalProcessed := Length(ATaskIDs);
  
  for i := 0 to Length(ATaskIDs) - 1 do
  begin
    if UpdateTaskPriority(ATaskIDs[i], ANewPriority) then
      Inc(Result.SuccessCount)
    else
      Inc(Result.FailureCount);
  end;
  
  Result.Message := Format('Batch priority update: %d succeeded, %d failed out of %d tasks',
                          [Result.SuccessCount, Result.FailureCount, Result.TotalProcessed]);
end;

function TExtendedTaskManager.BatchUpdateCategory(const ATaskIDs: array of Integer;
  const ANewCategory: string): TBatchOperationResult;
var
  i: Integer;
begin
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.TotalProcessed := Length(ATaskIDs);
  
  for i := 0 to Length(ATaskIDs) - 1 do
  begin
    if UpdateTaskCategory(ATaskIDs[i], ANewCategory) then
      Inc(Result.SuccessCount)
    else
      Inc(Result.FailureCount);
  end;
  
  Result.Message := Format('Batch category update: %d succeeded, %d failed out of %d tasks',
                          [Result.SuccessCount, Result.FailureCount, Result.TotalProcessed]);
end;

function TExtendedTaskManager.BatchDeleteTasks(const ATaskIDs: array of Integer): TBatchOperationResult;
var
  i: Integer;
begin
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.TotalProcessed := Length(ATaskIDs);
  
  for i := 0 to Length(ATaskIDs) - 1 do
  begin
    if DeleteTask(ATaskIDs[i]) then
      Inc(Result.SuccessCount)
    else
      Inc(Result.FailureCount);
  end;
  
  Result.Message := Format('Batch delete: %d succeeded, %d failed out of %d tasks',
                          [Result.SuccessCount, Result.FailureCount, Result.TotalProcessed]);
end;

function TExtendedTaskManager.BatchAddTag(const ATaskIDs: array of Integer;
  const ATag: string): TBatchOperationResult;
var
  i: Integer;
begin
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.TotalProcessed := Length(ATaskIDs);
  
  for i := 0 to Length(ATaskIDs) - 1 do
  begin
    if AddTagToTask(ATaskIDs[i], ATag) then
      Inc(Result.SuccessCount)
    else
      Inc(Result.FailureCount);
  end;
  
  Result.Message := Format('Batch tag addition: %d succeeded, %d failed out of %d tasks',
                          [Result.SuccessCount, Result.FailureCount, Result.TotalProcessed]);
end;

function TExtendedTaskManager.GetProductivityReport: string;
var
  CompletedTasks, TotalTasks: Integer;
  AvgScore: Double;
  i: Integer;
begin
  TotalTasks := Length(FExtendedTasks);
  CompletedTasks := GetCompletedCount;
  
  AvgScore := 0.0;
  if TotalTasks > 0 then
  begin
    for i := 0 to Length(FExtendedTasks) - 1 do
      AvgScore := AvgScore + FExtendedTasks[i].PriorityScore;
    AvgScore := AvgScore / TotalTasks;
  end;
  
  Result := '=== Productivity Report ===' + sLineBreak;
  Result := Result + Format('Total tasks: %d', [TotalTasks]) + sLineBreak;
  Result := Result + Format('Completed: %d (%.1f%%)', [CompletedTasks, GetCompletionRate]) + sLineBreak;
  Result := Result + Format('Overdue: %d', [GetOverdueCount]) + sLineBreak;
  Result := Result + Format('Average priority score: %.2f', [AvgScore]) + sLineBreak;
  Result := Result + Format('Total estimated hours: %.2f', [GetTotalEstimatedHours]) + sLineBreak;
  Result := Result + Format('Total actual hours: %.2f', [GetTotalActualHours]) + sLineBreak;
  
  if GetTotalEstimatedHours > 0 then
  begin
    Result := Result + Format('Time accuracy: %.1f%%', 
                             [(GetTotalActualHours / GetTotalEstimatedHours) * 100]) + sLineBreak;
  end;
end;

function TExtendedTaskManager.GetCategoryPerformance: string;
begin
  Result := '=== Category Performance ===' + sLineBreak;
  Result := Result + GetTasksByCategory + sLineBreak;
end;

function TExtendedTaskManager.GetTimeManagementReport: string;
var
  EstimatedTotal, ActualTotal: Double;
begin
  EstimatedTotal := GetTotalEstimatedHours;
  ActualTotal := GetTotalActualHours;
  
  Result := '=== Time Management Report ===' + sLineBreak;
  Result := Result + Format('Total estimated time: %.2f hours', [EstimatedTotal]) + sLineBreak;
  Result := Result + Format('Total actual time: %.2f hours', [ActualTotal]) + sLineBreak;
  
  if EstimatedTotal > 0 then
  begin
    if ActualTotal > EstimatedTotal then
      Result := Result + Format('Over budget by: %.2f hours (%.1f%%)', 
                               [ActualTotal - EstimatedTotal, 
                                ((ActualTotal - EstimatedTotal) / EstimatedTotal) * 100])
    else
      Result := Result + Format('Under budget by: %.2f hours (%.1f%%)',
                               [EstimatedTotal - ActualTotal,
                                ((EstimatedTotal - ActualTotal) / EstimatedTotal) * 100]);
  end;
  Result := Result + sLineBreak;
end;

function TExtendedTaskManager.GetTaskComplexityAnalysis: string;
var
  i, SimpleCount, ModerateCount, ComplexCount: Integer;
begin
  SimpleCount := 0;
  ModerateCount := 0;
  ComplexCount := 0;
  
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    if FExtendedTasks[i].BaseTask.EstimatedHours < 4.0 then
      Inc(SimpleCount)
    else if FExtendedTasks[i].BaseTask.EstimatedHours < 8.0 then
      Inc(ModerateCount)
    else
      Inc(ComplexCount);
  end;
  
  Result := '=== Task Complexity Analysis ===' + sLineBreak;
  Result := Result + Format('Simple tasks (<4h): %d', [SimpleCount]) + sLineBreak;
  Result := Result + Format('Moderate tasks (4-8h): %d', [ModerateCount]) + sLineBreak;
  Result := Result + Format('Complex tasks (>8h): %d', [ComplexCount]) + sLineBreak;
end;

function TExtendedTaskManager.RecurrencePatternToString(APattern: TRecurrencePattern): string;
begin
  case APattern of
    rpNone: Result := 'None';
    rpDaily: Result := 'Daily';
    rpWeekly: Result := 'Weekly';
    rpBiWeekly: Result := 'Bi-weekly';
    rpMonthly: Result := 'Monthly';
    rpQuarterly: Result := 'Quarterly';
    rpYearly: Result := 'Yearly';
  else
    Result := 'Unknown';
  end;
end;

function TExtendedTaskManager.ExtendedTaskToString(const ATask: TExtendedTask): string;
begin
  Result := TaskToString(ATask.BaseTask);
  Result := Result + Format(' | Score: %.2f', [ATask.PriorityScore]);
  
  if ATask.IsRecurring then
    Result := Result + Format(' | Recurrence: %s', [RecurrencePatternToString(ATask.RecurrencePattern)]);
  
  if ATask.IsSubtask then
    Result := Result + Format(' | Parent: #%d', [ATask.ParentID]);
  
  if Length(ATask.SubtaskIDs) > 0 then
    Result := Result + Format(' | Subtasks: %d', [Length(ATask.SubtaskIDs)]);
end;

function TExtendedTaskManager.GetAllExtendedTasks: TExtendedTaskArray;
begin
  SetLength(Result, Length(FExtendedTasks));
  Result := Copy(FExtendedTasks, 0, Length(FExtendedTasks));
end;

function TExtendedTaskManager.ExportExtendedToCSV: string;
var
  i, j: Integer;
  Task: TExtendedTask;
  TagStr: string;
begin
  Result := 'ID,Title,Description,Category,Status,Priority,CreatedDate,DueDate,CompletedDate,' +
            'EstimatedHours,ActualHours,Tags,PriorityScore,IsRecurring,RecurrencePattern,' +
            'ParentID,SubtaskCount' + sLineBreak;
  
  for i := 0 to Length(FExtendedTasks) - 1 do
  begin
    Task := FExtendedTasks[i];
    
    TagStr := '';
    for j := 0 to Length(Task.BaseTask.Tags) - 1 do
    begin
      if j > 0 then
        TagStr := TagStr + '|';
      TagStr := TagStr + Task.BaseTask.Tags[j];
    end;
    
    Result := Result + Format('%d,"%s","%s","%s",%s,%s,%s,%s,%s,%.2f,%.2f,"%s",%.2f,%s,%s,%d,%d',
      [Task.BaseTask.ID,
       Task.BaseTask.Title,
       Task.BaseTask.Description,
       Task.BaseTask.Category,
       TaskStatusToString(Task.BaseTask.Status),
       TaskPriorityToString(Task.BaseTask.Priority),
       FormatDateTime('dd-mm-yy hh:nn:ss', Task.BaseTask.CreatedDate),
       FormatDateTime('dd-mm-yy', Task.BaseTask.DueDate),
       FormatDateTime('dd-mm-yy', Task.BaseTask.CompletedDate),
       Task.BaseTask.EstimatedHours,
       Task.BaseTask.ActualHours,
       TagStr,
       Task.PriorityScore,
       BoolToStr(Task.IsRecurring, True),
       RecurrencePatternToString(Task.RecurrencePattern),
       Task.ParentID,
       Length(Task.SubtaskIDs)]) + sLineBreak;
  end;
end;

function TExtendedTaskManager.SaveExtendedToFile(const AFilename: string): Boolean;
var
  F: TextFile;
  i, j: Integer;
  Task: TExtendedTask;
begin
  Result := False;
  try
    AssignFile(F, AFilename);
    Rewrite(F);
    
    WriteLn(F, '[EXTENDED_TASK_MANAGER_DATA_V1]');
    WriteLn(F, Length(FExtendedTasks));
    
    for i := 0 to Length(FExtendedTasks) - 1 do
    begin
      Task := FExtendedTasks[i];
      WriteLn(F, Task.BaseTask.ID);
      WriteLn(F, Task.BaseTask.Title);
      WriteLn(F, Task.BaseTask.Description);
      WriteLn(F, Task.BaseTask.Category);
      WriteLn(F, Ord(Task.BaseTask.Status));
      WriteLn(F, Ord(Task.BaseTask.Priority));
      WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Task.BaseTask.CreatedDate));
      WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Task.BaseTask.DueDate));
      WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Task.BaseTask.CompletedDate));
      WriteLn(F, FloatToStr(Task.BaseTask.EstimatedHours));
      WriteLn(F, FloatToStr(Task.BaseTask.ActualHours));
      
      WriteLn(F, Length(Task.BaseTask.Tags));
      for j := 0 to Length(Task.BaseTask.Tags) - 1 do
        WriteLn(F, Task.BaseTask.Tags[j]);
      
      WriteLn(F, Task.ParentID);
      WriteLn(F, Ord(Task.RecurrencePattern));
      WriteLn(F, Task.RecurrenceCount);
      WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Task.NextRecurrenceDate));
      WriteLn(F, FloatToStr(Task.PriorityScore));
      WriteLn(F, BoolToStr(Task.IsRecurring, True));
      WriteLn(F, BoolToStr(Task.IsSubtask, True));
      
      WriteLn(F, Length(Task.SubtaskIDs));
      for j := 0 to Length(Task.SubtaskIDs) - 1 do
        WriteLn(F, Task.SubtaskIDs[j]);
    end;
    
    CloseFile(F);
    Result := True;
  except
    Result := False;
  end;
end;

function TExtendedTaskManager.LoadExtendedFromFile(const AFilename: string): Boolean;
var
  F: TextFile;
  Header: string;
  ExtTaskCount, i, j, TagCount, SubtaskCount: Integer;
  Task: TExtendedTask;
  TempStr: string;
begin
  Result := False;
  try
    AssignFile(F, AFilename);
    Reset(F);
    
    ReadLn(F, Header);
    if Header <> '[EXTENDED_TASK_MANAGER_DATA_V1]' then
    begin
      CloseFile(F);
      Exit;
    end;
    
    ReadLn(F, ExtTaskCount);
    SetLength(FExtendedTasks, ExtTaskCount);
    
    for i := 0 to ExtTaskCount - 1 do
    begin
      ReadLn(F, Task.BaseTask.ID);
      ReadLn(F, Task.BaseTask.Title);
      ReadLn(F, Task.BaseTask.Description);
      ReadLn(F, Task.BaseTask.Category);
      ReadLn(F, TempStr);
      Task.BaseTask.Status := TTaskStatus(StrToInt(TempStr));
      ReadLn(F, TempStr);
      Task.BaseTask.Priority := TTaskPriority(StrToInt(TempStr));
      ReadLn(F, TempStr);
      Task.BaseTask.CreatedDate := StrToDateTime(TempStr);
      ReadLn(F, TempStr);
      Task.BaseTask.DueDate := StrToDateTime(TempStr);
      ReadLn(F, TempStr);
      Task.BaseTask.CompletedDate := StrToDateTime(TempStr);
      ReadLn(F, TempStr);
      Task.BaseTask.EstimatedHours := StrToFloat(TempStr);
      ReadLn(F, TempStr);
      Task.BaseTask.ActualHours := StrToFloat(TempStr);
      
      ReadLn(F, TagCount);
      SetLength(Task.BaseTask.Tags, TagCount);
      for j := 0 to TagCount - 1 do
        ReadLn(F, Task.BaseTask.Tags[j]);
      
      ReadLn(F, Task.ParentID);
      ReadLn(F, TempStr);
      Task.RecurrencePattern := TRecurrencePattern(StrToInt(TempStr));
      ReadLn(F, Task.RecurrenceCount);
      ReadLn(F, TempStr);
      Task.NextRecurrenceDate := StrToDateTime(TempStr);
      ReadLn(F, TempStr);
      Task.PriorityScore := StrToFloat(TempStr);
      ReadLn(F, TempStr);
      Task.IsRecurring := StrToBool(TempStr);
      ReadLn(F, TempStr);
      Task.IsSubtask := StrToBool(TempStr);
      
      ReadLn(F, SubtaskCount);
      SetLength(Task.SubtaskIDs, SubtaskCount);
      for j := 0 to SubtaskCount - 1 do
        ReadLn(F, Task.SubtaskIDs[j]);
      
      Task.LastModifiedDate := Now;
      Task.NotificationDays := 3;
      
      FExtendedTasks[i] := Task;
    end;
    
    CloseFile(F);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TExtendedTaskManager.ProcessRecurringTasks;
begin
  UpdateAllRecurringTasks;
end;

function TExtendedTaskManager.CreateRecurringInstance(const ATask: TExtendedTask): Integer;
begin
  Result := GenerateNextRecurrence(ATask.BaseTask.ID);
end;

end.
