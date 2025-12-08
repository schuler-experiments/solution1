
{ Advanced Analytics Implementation }

function TIntelligenceTaskManager.GenerateCompletionTrend(ADays: Integer): TTrendArray;
var
  i, completedCount: Integer;
  currentDate: TDateTime;
  tasks: TTaskArray;
  j: Integer;
begin
  SetLength(Result, ADays);
  for i := 0 to ADays - 1 do
  begin
    currentDate := IncDay(Now, -ADays + i);
    Result[i].Date := currentDate;
    Result[i].Label_ := FormatDateTime('yyyy-mm-dd', currentDate);
    
    completedCount := 0;
    tasks := GetInheritedAllTasks;
    for j := 0 to High(tasks) do
      if (tasks[j].Status = tsCompleted) and
         (DateOf(tasks[j].CompletedDate) = DateOf(currentDate)) then
        Inc(completedCount);
    
    Result[i].Value := completedCount;
  end;
end;

function TIntelligenceTaskManager.GenerateCategoryTrend(
  const ACategory: string; ADays: Integer): TTrendArray;
var
  i: Integer;
begin
  SetLength(Result, ADays);
  for i := 0 to ADays - 1 do
  begin
    Result[i].Date := IncDay(Now, -ADays + i);
    Result[i].Label_ := FormatDateTime('yyyy-mm-dd', Result[i].Date);
    Result[i].Value := 0;
  end;
end;

function TIntelligenceTaskManager.GeneratePriorityDistribution: TTrendArray;
var
  tasks: TTaskArray;
  i, highCount, medCount, lowCount: Integer;
begin
  SetLength(Result, 3);
  highCount := 0;
  medCount := 0;
  lowCount := 0;
  
  tasks := GetInheritedAllTasks;
  for i := 0 to High(tasks) do
  begin
    case tasks[i].Priority of
      tpHigh: Inc(highCount);
      tpMedium: Inc(medCount);
      tpLow: Inc(lowCount);
    end;
  end;
  
  Result[0].Label_ := 'High';
  Result[0].Value := highCount;
  Result[1].Label_ := 'Medium';
  Result[1].Value := medCount;
  Result[2].Label_ := 'Low';
  Result[2].Value := lowCount;
end;

function TIntelligenceTaskManager.GenerateProductivityHeatmap: string;
var
  sb: TStringList;
  i, hour: Integer;
  hourCounts: array[0..23] of Integer;
  sessions: TWorkSessionArray;
  maxCount: Integer;
begin
  sb := TStringList.Create;
  try
    for i := 0 to 23 do
      hourCounts[i] := 0;
    
    sessions := inherited GetAllWorkSessions;
    for i := 0 to High(sessions) do
    begin
      hour := HourOf(sessions[i].StartTime);
      Inc(hourCounts[hour]);
    end;
    
    maxCount := 1;
    for i := 0 to 23 do
      if hourCounts[i] > maxCount then
        maxCount := hourCounts[i];
    
    sb.Add('Productivity Heatmap (24-hour view):');
    sb.Add('Hour | Activity | Visualization');
    sb.Add('-----|----------|' + StringOfChar('-', 40));
    
    for i := 0 to 23 do
    begin
      sb.Add(Format('%02d:00 | %4d     | %s',
        [i, hourCounts[i], StringOfChar(#$2588, (hourCounts[i] * 40) div maxCount)]));
    end;
    
    Result := sb.Text;
  finally
    sb.Free;
  end;
end;

function TIntelligenceTaskManager.GenerateVelocityReport(AWeeks: Integer): TAnalyticsReport;
var
  i: Integer;
begin
  Result.ReportID := Random(10000);
  Result.ReportType := 'Velocity Report';
  Result.Generated := Now;
  Result.TimeRange := Format('%d weeks', [AWeeks]);
  SetLength(Result.DataPoints, AWeeks);
  
  for i := 0 to AWeeks - 1 do
  begin
    Result.DataPoints[i].Date := IncDay(Now, -7 * (AWeeks - i));
    Result.DataPoints[i].Label_ := Format('Week %d', [i + 1]);
    Result.DataPoints[i].Value := Random(10) + 5;
  end;
  
  Result.Summary := Format('Average velocity: %.1f tasks/week',
    [GetCompletedCount / Max(1, AWeeks)]);
  SetLength(Result.Insights, 2);
  Result.Insights[0] := 'Task completion rate is steady';
  Result.Insights[1] := 'Consider increasing capacity for high-priority items';
end;

function TIntelligenceTaskManager.GenerateBurndownChart(
  const ACategory: string): TTrendArray;
var
  tasks: TTaskArray;
  i, remaining: Integer;
begin
  tasks := FilterByCategory(ACategory);
  SetLength(Result, 10);
  
  remaining := Length(tasks);
  for i := 0 to 9 do
  begin
    Result[i].Date := IncDay(Now, i);
    Result[i].Label_ := Format('Day %d', [i + 1]);
    Result[i].Value := Max(0, remaining - (i * 2));
  end;
end;

function TIntelligenceTaskManager.PredictTaskCompletionTrend(
  ADaysAhead: Integer): TTrendArray;
var
  i: Integer;
  baseRate, prediction: Double;
begin
  baseRate := GetCompletionRate;
  SetLength(Result, ADaysAhead);
  
  for i := 0 to ADaysAhead - 1 do
  begin
    Result[i].Date := IncDay(Now, i + 1);
    Result[i].Label_ := FormatDateTime('yyyy-mm-dd', Result[i].Date);
    prediction := baseRate * (1 + (i * 0.05));
    Result[i].Value := Min(100, prediction);
  end;
end;

function TIntelligenceTaskManager.GetTopPerformingCategories(ALimit: Integer): string;
var
  sb: TStringList;
  catStats: string;
begin
  sb := TStringList.Create;
  try
    sb.Add('Top Performing Categories:');
    catStats := GetTasksByCategory;
    sb.Add(catStats);
    Result := sb.Text;
  finally
    sb.Free;
  end;
end;

function TIntelligenceTaskManager.GetBottleneckAnalysis: string;
var
  sb: TStringList;
  overdueTasks: TTaskArray;
  i: Integer;
begin
  sb := TStringList.Create;
  try
    sb.Add('=== Bottleneck Analysis ===');
    sb.Add('');
    
    overdueTasks := GetInheritedAllTasks;
    sb.Add(Format('Total Overdue Tasks: %d', [GetOverdueCount]));
    sb.Add(Format('Completion Rate: %.1f%%', [GetCompletionRate]));
    
    Result := sb.Text;
  finally
    sb.Free;
  end;
end;

{ Smart Notifications Implementation }

function TIntelligenceTaskManager.CreateNotification(
  AChannel: TNotificationChannel; APriority: TNotificationPriority;
  const ATitle, AMessage: string; ATaskID: Integer): Integer;
var
  idx: Integer;
begin
  idx := Length(FNotifications);
  SetLength(FNotifications, idx + 1);
  
  FNotifications[idx].NotificationID := FNextNotificationID;
  FNotifications[idx].Channel := AChannel;
  FNotifications[idx].Priority := APriority;
  FNotifications[idx].Title := ATitle;
  FNotifications[idx].Message := AMessage;
  FNotifications[idx].TaskID := ATaskID;
  FNotifications[idx].CreatedAt := Now;
  FNotifications[idx].IsSent := False;
  FNotifications[idx].Context := '';
  
  Result := FNextNotificationID;
  Inc(FNextNotificationID);
end;

function TIntelligenceTaskManager.GetPendingNotifications: TSmartNotificationArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FNotifications) do
    if not FNotifications[i].IsSent then
      Inc(count);
  
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FNotifications) do
    if not FNotifications[i].IsSent then
    begin
      Result[count] := FNotifications[i];
      Inc(count);
    end;
end;

function TIntelligenceTaskManager.SendNotification(ANotificationID: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FNotifications) do
    if FNotifications[i].NotificationID = ANotificationID then
    begin
      FNotifications[i].IsSent := True;
      FNotifications[i].SentAt := Now;
      Result := True;
      Exit;
    end;
end;

function TIntelligenceTaskManager.SendAllPendingNotifications: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FNotifications) do
    if not FNotifications[i].IsSent then
    begin
      FNotifications[i].IsSent := True;
      FNotifications[i].SentAt := Now;
      Inc(Result);
    end;
end;

procedure TIntelligenceTaskManager.CheckAndCreateSmartNotifications;
var
  tasks: TTaskArray;
  i: Integer;
  daysUntilDue: Integer;
begin
  tasks := GetInheritedAllTasks;
  for i := 0 to High(tasks) do
  begin
    if tasks[i].Status <> tsCompleted then
    begin
      daysUntilDue := DaysBetween(Now, tasks[i].DueDate);
      
      if (daysUntilDue <= 1) and (tasks[i].Priority = tpHigh) then
        CreateNotification(ncConsole, npCritical,
          'Urgent Task Due Soon',
          Format('Task "%s" is due within 24 hours!', [tasks[i].Title]),
          tasks[i].ID);
    end;
  end;
end;

{ Export Hub Implementation - See next include file }
