
unit taskmanagerlifestyle;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math, Classes,
  taskmanager, taskmanagerext, taskmanageradvanced, taskmanagerenhanced,
  taskmanagerteam;

type
  // Eisenhower Matrix quadrants
  TEisenhowerQuadrant = (eqUrgentImportant, eqNotUrgentImportant, 
                         eqUrgentNotImportant, eqNotUrgentNotImportant);
  
  // Energy levels for optimal task matching
  TEnergyLevel = (elLow, elMedium, elHigh, elPeak);
  
  // Task template structure
  TTaskTemplate = record
    TemplateID: Integer;
    Name: string;
    Description: string;
    Category: string;
    Priority: TTaskPriority;
    EstimatedHours: Double;
    DefaultTags: array of string;
    Checklist: array of string;
    EnergyRequired: TEnergyLevel;
    CreatedDate: TDateTime;
    UsageCount: Integer;
  end;
  TTaskTemplateArray = array of TTaskTemplate;
  
  // Habit tracking
  THabitFrequency = (hfDaily, hfWeekly, hfMonthly, hfCustom);
  
  THabit = record
    HabitID: Integer;
    Name: string;
    Description: string;
    Frequency: THabitFrequency;
    TargetStreak: Integer;
    CurrentStreak: Integer;
    LongestStreak: Integer;
    TotalCompletions: Integer;
    LastCompletedDate: TDateTime;
    CreatedDate: TDateTime;
    IsActive: Boolean;
    Reminder: string;
  end;
  THabitArray = array of THabit;
  
  THabitLog = record
    LogID: Integer;
    HabitID: Integer;
    CompletedDate: TDateTime;
    Notes: string;
    Mood: string;
  end;
  THabitLogArray = array of THabitLog;
  
  // Time box allocation
  TTimeBox = record
    TimeBoxID: Integer;
    TaskID: Integer;
    StartTime: TDateTime;
    AllocatedMinutes: Integer;
    ActualMinutes: Integer;
    WasCompleted: Boolean;
    WasInterrupted: Boolean;
    InterruptionCount: Integer;
    Notes: string;
  end;
  TTimeBoxArray = array of TTimeBox;
  
  // Task bundle for batching similar tasks
  TTaskBundle = record
    BundleID: Integer;
    Name: string;
    Description: string;
    TaskIDs: array of Integer;
    Category: string;
    EstimatedMinutes: Integer;
    CreatedDate: TDateTime;
    LastUsedDate: TDateTime;
  end;
  TTaskBundleArray = array of TTaskBundle;
  
  // Context switching tracking
  TContextSwitch = record
    SwitchID: Integer;
    FromTaskID: Integer;
    ToTaskID: Integer;
    SwitchTime: TDateTime;
    EstimatedCostMinutes: Integer;
    Reason: string;
  end;
  TContextSwitchArray = array of TContextSwitch;
  
  // Focus session (deep work)
  TFocusSession = record
    SessionID: Integer;
    TaskID: Integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
    PlannedDuration: Integer;
    ActualDuration: Integer;
    ProductivityRating: Integer; // 1-10
    DistractionCount: Integer;
    Notes: string;
  end;
  TFocusSessionArray = array of TFocusSession;
  
  // Task mood tracking
  TMoodType = (mtVeryBad, mtBad, mtNeutral, mtGood, mtVeryGood);
  
  TTaskMood = record
    MoodID: Integer;
    TaskID: Integer;
    Mood: TMoodType;
    EnergyLevel: TEnergyLevel;
    RecordedTime: TDateTime;
    Notes: string;
  end;
  TTaskMoodArray = array of TTaskMood;
  
  // Personal productivity rhythm
  TProductivityPeriod = record
    PeriodID: Integer;
    DayOfWeek: Integer; // 1-7
    HourOfDay: Integer; // 0-23
    AverageProductivity: Double; // 0-100
    OptimalEnergyLevel: TEnergyLevel;
    SampleCount: Integer;
  end;
  TProductivityPeriodArray = array of TProductivityPeriod;
  
  // Eisenhower matrix entry
  TEisenhowerEntry = record
    EntryID: Integer;
    TaskID: Integer;
    Quadrant: TEisenhowerQuadrant;
    UrgencyScore: Integer; // 1-10
    ImportanceScore: Integer; // 1-10
    LastReviewed: TDateTime;
    Notes: string;
  end;
  TEisenhowerEntryArray = array of TEisenhowerEntry;
  
  // Lifestyle Task Manager
  TLifestyleTaskManager = class(TTeamTaskManager)
  private
    FTaskTemplates: TTaskTemplateArray;
    FHabits: THabitArray;
    FHabitLogs: THabitLogArray;
    FTimeBoxes: TTimeBoxArray;
    FTaskBundles: TTaskBundleArray;
    FContextSwitches: TContextSwitchArray;
    FFocusSessions: TFocusSessionArray;
    FTaskMoods: TTaskMoodArray;
    FProductivityPeriods: TProductivityPeriodArray;
    FEisenhowerEntries: TEisenhowerEntryArray;
    
    FNextTemplateID: Integer;
    FNextHabitID: Integer;
    FNextHabitLogID: Integer;
    FNextTimeBoxID: Integer;
    FNextBundleID: Integer;
    FNextSwitchID: Integer;
    FNextFocusSessionID: Integer;
    FNextMoodID: Integer;
    FNextPeriodID: Integer;
    FNextEisenhowerID: Integer;
    
    function FindTemplateIndex(ATemplateID: Integer): Integer;
    function FindHabitIndex(AHabitID: Integer): Integer;
    function FindTimeBoxIndex(ATimeBoxID: Integer): Integer;
    function FindBundleIndex(ABundleID: Integer): Integer;
    function FindFocusSessionIndex(ASessionID: Integer): Integer;
    function CalculateContextSwitchCost(AFromTaskID, AToTaskID: Integer): Integer;
    function GetProductivityForTime(ADayOfWeek, AHour: Integer): Double;
    procedure UpdateProductivityPeriod(ADayOfWeek, AHour: Integer; AProductivity: Double);
    function DetermineQuadrant(AUrgency, AImportance: Integer): TEisenhowerQuadrant;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Task Template Management
    function CreateTemplate(const AName, ADescription, ACategory: string;
      APriority: TTaskPriority; AEstimatedHours: Double;
      AEnergyRequired: TEnergyLevel): Integer;
    function AddChecklistItemToTemplate(ATemplateID: Integer; const AItem: string): Boolean;
    function AddDefaultTagToTemplate(ATemplateID: Integer; const ATag: string): Boolean;
    function CreateTaskFromTemplate(ATemplateID: Integer; ADueDate: TDateTime): Integer;
    function GetAllTemplates: TTaskTemplateArray;
    function GetPopularTemplates(ALimit: Integer): TTaskTemplateArray;
    function DeleteTemplate(ATemplateID: Integer): Boolean;
    
    // Eisenhower Matrix
    function AddToEisenhowerMatrix(ATaskID: Integer; AUrgency, AImportance: Integer;
      const ANotes: string): Integer;
    function UpdateEisenhowerScores(AEntryID: Integer; AUrgency, AImportance: Integer): Boolean;
    function GetTasksByQuadrant(AQuadrant: TEisenhowerQuadrant): TTaskArray;
    function GetEisenhowerSummary: string;
    function SuggestQuadrantForTask(ATaskID: Integer): TEisenhowerQuadrant;
    
    // Habit Tracking
    function CreateHabit(const AName, ADescription: string; 
      AFrequency: THabitFrequency; ATargetStreak: Integer): Integer;
    function LogHabitCompletion(AHabitID: Integer; const ANotes, AMood: string): Integer;
    function GetHabitStreak(AHabitID: Integer): Integer;
    function GetAllHabits: THabitArray;
    function GetActiveHabits: THabitArray;
    function GetHabitStatistics(AHabitID: Integer): string;
    function GetHabitsNeedingAttention: THabitArray;
    
    // Time Boxing
    function CreateTimeBox(ATaskID: Integer; AStartTime: TDateTime;
      AAllocatedMinutes: Integer): Integer;
    function CompleteTimeBox(ATimeBoxID: Integer; AActualMinutes: Integer;
      AWasCompleted: Boolean): Boolean;
    function RecordInterruption(ATimeBoxID: Integer): Boolean;
    function GetTimeBoxesForDate(ADate: TDateTime): TTimeBoxArray;
    function GetTimeBoxEfficiency: Double;
    
    // Task Bundling
    function CreateTaskBundle(const AName, ADescription, ACategory: string): Integer;
    function AddTaskToBundle(ABundleID, ATaskID: Integer): Boolean;
    function RemoveTaskFromBundle(ABundleID, ATaskID: Integer): Boolean;
    function GetBundle(ABundleID: Integer): TTaskBundle;
    function GetAllBundles: TTaskBundleArray;
    function SuggestBundles: TTaskBundleArray;
    
    // Context Switching
    function RecordContextSwitch(AFromTaskID, AToTaskID: Integer;
      const AReason: string): Integer;
    function GetContextSwitchCost(ADate: TDateTime): Integer;
    function GetContextSwitchReport: string;
    function GetLowSwitchingTasks: TTaskArray;
    
    // Focus Sessions
    function StartFocusSession(ATaskID: Integer; APlannedDuration: Integer): Integer;
    function EndFocusSession(ASessionID: Integer; AProductivityRating: Integer;
      ADistractionCount: Integer; const ANotes: string): Boolean;
    function GetFocusSessions(ATaskID: Integer): TFocusSessionArray;
    function GetAverageFocusQuality: Double;
    function GetBestFocusTime: string;
    
    // Task Mood Tracking
    function RecordTaskMood(ATaskID: Integer; AMood: TMoodType;
      AEnergyLevel: TEnergyLevel; const ANotes: string): Integer;
    function GetTaskMoodHistory(ATaskID: Integer): TTaskMoodArray;
    function GetOptimalTasksForMood(AMood: TMoodType): TTaskArray;
    function GetMoodInsights: string;
    
    // Productivity Rhythm
    function RecordProductivitySample(ADayOfWeek, AHour: Integer;
      AProductivity: Double): Boolean;
    function GetOptimalWorkingHours: string;
    function GetProductivityHeatmap: string;
    function SuggestTaskSchedule(ATaskID: Integer): TDateTime;
    
    // Energy Level Optimization
    function GetTasksByEnergyLevel(ALevel: TEnergyLevel): TTaskArray;
    function SuggestTasksForCurrentEnergy: TTaskArray;
    function GetEnergyOptimizationReport: string;
    
    // Utility functions
    function EisenhowerQuadrantToString(AQuadrant: TEisenhowerQuadrant): string;
    function EnergyLevelToString(ALevel: TEnergyLevel): string;
    function MoodTypeToString(AMood: TMoodType): string;
    function HabitFrequencyToString(AFreq: THabitFrequency): string;
    
    // Persistence
    function SaveLifestyleDataToFile(const AFilename: string): Boolean;
    function LoadLifestyleDataFromFile(const AFilename: string): Boolean;
  end;

implementation

{ TLifestyleTaskManager }

constructor TLifestyleTaskManager.Create;
begin
  inherited Create;
  SetLength(FTaskTemplates, 0);
  SetLength(FHabits, 0);
  SetLength(FHabitLogs, 0);
  SetLength(FTimeBoxes, 0);
  SetLength(FTaskBundles, 0);
  SetLength(FContextSwitches, 0);
  SetLength(FFocusSessions, 0);
  SetLength(FTaskMoods, 0);
  SetLength(FProductivityPeriods, 0);
  SetLength(FEisenhowerEntries, 0);
  
  FNextTemplateID := 1;
  FNextHabitID := 1;
  FNextHabitLogID := 1;
  FNextTimeBoxID := 1;
  FNextBundleID := 1;
  FNextSwitchID := 1;
  FNextFocusSessionID := 1;
  FNextMoodID := 1;
  FNextPeriodID := 1;
  FNextEisenhowerID := 1;
end;

destructor TLifestyleTaskManager.Destroy;
begin
  SetLength(FTaskTemplates, 0);
  SetLength(FHabits, 0);
  SetLength(FHabitLogs, 0);
  SetLength(FTimeBoxes, 0);
  SetLength(FTaskBundles, 0);
  SetLength(FContextSwitches, 0);
  SetLength(FFocusSessions, 0);
  SetLength(FTaskMoods, 0);
  SetLength(FProductivityPeriods, 0);
  SetLength(FEisenhowerEntries, 0);
  inherited Destroy;
end;

function TLifestyleTaskManager.FindTemplateIndex(ATemplateID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTaskTemplates) do
    if FTaskTemplates[i].TemplateID = ATemplateID then
    begin
      Result := i;
      Exit;
    end;
end;

function TLifestyleTaskManager.FindHabitIndex(AHabitID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FHabits) do
    if FHabits[i].HabitID = AHabitID then
    begin
      Result := i;
      Exit;
    end;
end;

function TLifestyleTaskManager.FindTimeBoxIndex(ATimeBoxID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTimeBoxes) do
    if FTimeBoxes[i].TimeBoxID = ATimeBoxID then
    begin
      Result := i;
      Exit;
    end;
end;

function TLifestyleTaskManager.FindBundleIndex(ABundleID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTaskBundles) do
    if FTaskBundles[i].BundleID = ABundleID then
    begin
      Result := i;
      Exit;
    end;
end;

function TLifestyleTaskManager.FindFocusSessionIndex(ASessionID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FFocusSessions) do
    if FFocusSessions[i].SessionID = ASessionID then
    begin
      Result := i;
      Exit;
    end;
end;

function TLifestyleTaskManager.CalculateContextSwitchCost(AFromTaskID, AToTaskID: Integer): Integer;
var
  fromIdx, toIdx: Integer;
  fromTask, toTask: TTask;
begin
  Result := 15; // Default 15 minutes
  
  fromIdx := GetTaskByID(AFromTaskID);
  toIdx := GetTaskByID(AToTaskID);
  
  if (fromIdx >= 0) and (toIdx >= 0) then
  begin
    fromTask := GetAllTasks[fromIdx];
    toTask := GetAllTasks[toIdx];
    
    // Same category = lower cost
    if fromTask.Category = toTask.Category then
      Result := 5
    // Different priority = higher cost  
    else if fromTask.Priority <> toTask.Priority then
      Result := 25
    else
      Result := 15;
  end;
end;

function TLifestyleTaskManager.GetProductivityForTime(ADayOfWeek, AHour: Integer): Double;
var
  i: Integer;
  totalProd, totalSamples: Double;
begin
  totalProd := 0;
  totalSamples := 0;
  
  for i := 0 to High(FProductivityPeriods) do
    if (FProductivityPeriods[i].DayOfWeek = ADayOfWeek) and
       (FProductivityPeriods[i].HourOfDay = AHour) then
    begin
      totalProd := totalProd + FProductivityPeriods[i].AverageProductivity * 
                   FProductivityPeriods[i].SampleCount;
      totalSamples := totalSamples + FProductivityPeriods[i].SampleCount;
    end;
  
  if totalSamples > 0 then
    Result := totalProd / totalSamples
  else
    Result := 50.0; // Default moderate productivity
end;

procedure TLifestyleTaskManager.UpdateProductivityPeriod(ADayOfWeek, AHour: Integer; 
  AProductivity: Double);
var
  i, idx: Integer;
  found: Boolean;
begin
  found := False;
  idx := -1;
  
  for i := 0 to High(FProductivityPeriods) do
    if (FProductivityPeriods[i].DayOfWeek = ADayOfWeek) and
       (FProductivityPeriods[i].HourOfDay = AHour) then
    begin
      found := True;
      idx := i;
      Break;
    end;
  
  if found then
  begin
    // Update existing period
    FProductivityPeriods[idx].AverageProductivity :=
      (FProductivityPeriods[idx].AverageProductivity * FProductivityPeriods[idx].SampleCount +
       AProductivity) / (FProductivityPeriods[idx].SampleCount + 1);
    FProductivityPeriods[idx].SampleCount := FProductivityPeriods[idx].SampleCount + 1;
  end
  else
  begin
    // Create new period
    SetLength(FProductivityPeriods, Length(FProductivityPeriods) + 1);
    idx := High(FProductivityPeriods);
    FProductivityPeriods[idx].PeriodID := FNextPeriodID;
    Inc(FNextPeriodID);
    FProductivityPeriods[idx].DayOfWeek := ADayOfWeek;
    FProductivityPeriods[idx].HourOfDay := AHour;
    FProductivityPeriods[idx].AverageProductivity := AProductivity;
    FProductivityPeriods[idx].SampleCount := 1;
    FProductivityPeriods[idx].OptimalEnergyLevel := elMedium;
  end;
end;

function TLifestyleTaskManager.DetermineQuadrant(AUrgency, AImportance: Integer): TEisenhowerQuadrant;
begin
  if (AUrgency >= 7) and (AImportance >= 7) then
    Result := eqUrgentImportant
  else if (AUrgency < 7) and (AImportance >= 7) then
    Result := eqNotUrgentImportant
  else if (AUrgency >= 7) and (AImportance < 7) then
    Result := eqUrgentNotImportant
  else
    Result := eqNotUrgentNotImportant;
end;

// Task Template Management
function TLifestyleTaskManager.CreateTemplate(const AName, ADescription, ACategory: string;
  APriority: TTaskPriority; AEstimatedHours: Double;
  AEnergyRequired: TEnergyLevel): Integer;
var
  idx: Integer;
begin
  SetLength(FTaskTemplates, Length(FTaskTemplates) + 1);
  idx := High(FTaskTemplates);
  
  FTaskTemplates[idx].TemplateID := FNextTemplateID;
  FTaskTemplates[idx].Name := AName;
  FTaskTemplates[idx].Description := ADescription;
  FTaskTemplates[idx].Category := ACategory;
  FTaskTemplates[idx].Priority := APriority;
  FTaskTemplates[idx].EstimatedHours := AEstimatedHours;
  FTaskTemplates[idx].EnergyRequired := AEnergyRequired;
  FTaskTemplates[idx].CreatedDate := Now;
  FTaskTemplates[idx].UsageCount := 0;
  SetLength(FTaskTemplates[idx].DefaultTags, 0);
  SetLength(FTaskTemplates[idx].Checklist, 0);
  
  Result := FNextTemplateID;
  Inc(FNextTemplateID);
end;

function TLifestyleTaskManager.AddChecklistItemToTemplate(ATemplateID: Integer;
  const AItem: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindTemplateIndex(ATemplateID);
  if idx >= 0 then
  begin
    SetLength(FTaskTemplates[idx].Checklist, Length(FTaskTemplates[idx].Checklist) + 1);
    FTaskTemplates[idx].Checklist[High(FTaskTemplates[idx].Checklist)] := AItem;
    Result := True;
  end;
end;

function TLifestyleTaskManager.AddDefaultTagToTemplate(ATemplateID: Integer;
  const ATag: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindTemplateIndex(ATemplateID);
  if idx >= 0 then
  begin
    SetLength(FTaskTemplates[idx].DefaultTags, Length(FTaskTemplates[idx].DefaultTags) + 1);
    FTaskTemplates[idx].DefaultTags[High(FTaskTemplates[idx].DefaultTags)] := ATag;
    Result := True;
  end;
end;

function TLifestyleTaskManager.CreateTaskFromTemplate(ATemplateID: Integer;
  ADueDate: TDateTime): Integer;
var
  idx, i: Integer;
  template: TTaskTemplate;
begin
  Result := -1;
  idx := FindTemplateIndex(ATemplateID);
  if idx >= 0 then
  begin
    template := FTaskTemplates[idx];
    Result := AddTask(template.Name, template.Description, template.Category,
                      template.Priority, ADueDate, template.EstimatedHours);
    
    // Add default tags
    for i := 0 to High(template.DefaultTags) do
      AddTagToTask(Result, template.DefaultTags[i]);
    
    // Update usage count
    FTaskTemplates[idx].UsageCount := FTaskTemplates[idx].UsageCount + 1;
  end;
end;

function TLifestyleTaskManager.GetAllTemplates: TTaskTemplateArray;
begin
  Result := Copy(FTaskTemplates, 0, Length(FTaskTemplates));
end;

function TLifestyleTaskManager.GetPopularTemplates(ALimit: Integer): TTaskTemplateArray;
var
  i, j, count: Integer;
  temp: TTaskTemplate;
  sorted: TTaskTemplateArray;
begin
  sorted := Copy(FTaskTemplates, 0, Length(FTaskTemplates));
  
  // Simple bubble sort by usage count
  for i := 0 to High(sorted) - 1 do
    for j := i + 1 to High(sorted) do
      if sorted[j].UsageCount > sorted[i].UsageCount then
      begin
        temp := sorted[i];
        sorted[i] := sorted[j];
        sorted[j] := temp;
      end;
  
  count := Min(ALimit, Length(sorted));
  SetLength(Result, count);
  for i := 0 to count - 1 do
    Result[i] := sorted[i];
end;

function TLifestyleTaskManager.DeleteTemplate(ATemplateID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindTemplateIndex(ATemplateID);
  if idx >= 0 then
  begin
    for i := idx to High(FTaskTemplates) - 1 do
      FTaskTemplates[i] := FTaskTemplates[i + 1];
    SetLength(FTaskTemplates, Length(FTaskTemplates) - 1);
    Result := True;
  end;
end;

// Eisenhower Matrix
function TLifestyleTaskManager.AddToEisenhowerMatrix(ATaskID: Integer;
  AUrgency, AImportance: Integer; const ANotes: string): Integer;
var
  idx: Integer;
begin
  SetLength(FEisenhowerEntries, Length(FEisenhowerEntries) + 1);
  idx := High(FEisenhowerEntries);
  
  FEisenhowerEntries[idx].EntryID := FNextEisenhowerID;
  FEisenhowerEntries[idx].TaskID := ATaskID;
  FEisenhowerEntries[idx].UrgencyScore := AUrgency;
  FEisenhowerEntries[idx].ImportanceScore := AImportance;
  FEisenhowerEntries[idx].Quadrant := DetermineQuadrant(AUrgency, AImportance);
  FEisenhowerEntries[idx].LastReviewed := Now;
  FEisenhowerEntries[idx].Notes := ANotes;
  
  Result := FNextEisenhowerID;
  Inc(FNextEisenhowerID);
end;

function TLifestyleTaskManager.UpdateEisenhowerScores(AEntryID: Integer;
  AUrgency, AImportance: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FEisenhowerEntries) do
    if FEisenhowerEntries[i].EntryID = AEntryID then
    begin
      FEisenhowerEntries[i].UrgencyScore := AUrgency;
      FEisenhowerEntries[i].ImportanceScore := AImportance;
      FEisenhowerEntries[i].Quadrant := DetermineQuadrant(AUrgency, AImportance);
      FEisenhowerEntries[i].LastReviewed := Now;
      Result := True;
      Exit;
    end;
end;

function TLifestyleTaskManager.GetTasksByQuadrant(AQuadrant: TEisenhowerQuadrant): TTaskArray;
var
  i, idx, count: Integer;
  taskIDs: array of Integer;
begin
  SetLength(taskIDs, 0);
  
  for i := 0 to High(FEisenhowerEntries) do
    if FEisenhowerEntries[i].Quadrant = AQuadrant then
    begin
      SetLength(taskIDs, Length(taskIDs) + 1);
      taskIDs[High(taskIDs)] := FEisenhowerEntries[i].TaskID;
    end;
  
  SetLength(Result, Length(taskIDs));
  count := 0;
  for i := 0 to High(taskIDs) do
  begin
    idx := GetTaskByID(taskIDs[i]);
    if idx >= 0 then
    begin
      Result[count] := GetAllTasks[idx];
      Inc(count);
    end;
  end;
  SetLength(Result, count);
end;

function TLifestyleTaskManager.GetEisenhowerSummary: string;
var
  q1, q2, q3, q4: Integer;
  i: Integer;
begin
  q1 := 0; q2 := 0; q3 := 0; q4 := 0;
  
  for i := 0 to High(FEisenhowerEntries) do
    case FEisenhowerEntries[i].Quadrant of
      eqUrgentImportant: Inc(q1);
      eqNotUrgentImportant: Inc(q2);
      eqUrgentNotImportant: Inc(q3);
      eqNotUrgentNotImportant: Inc(q4);
    end;
  
  Result := Format('Eisenhower Matrix Summary:%s' +
                   'Q1 (Urgent & Important - DO): %d tasks%s' +
                   'Q2 (Not Urgent & Important - SCHEDULE): %d tasks%s' +
                   'Q3 (Urgent & Not Important - DELEGATE): %d tasks%s' +
                   'Q4 (Not Urgent & Not Important - ELIMINATE): %d tasks',
                   [sLineBreak, q1, sLineBreak, q2, sLineBreak, q3, sLineBreak, q4]);
end;

function TLifestyleTaskManager.SuggestQuadrantForTask(ATaskID: Integer): TEisenhowerQuadrant;
var
  idx: Integer;
  task: TTask;
  urgency, importance: Integer;
begin
  Result := eqNotUrgentNotImportant;
  idx := GetTaskByID(ATaskID);
  if idx >= 0 then
  begin
    task := GetAllTasks[idx];
    
    // Estimate urgency based on due date
    if task.DueDate > 0 then
    begin
      if DaysBetween(Now, task.DueDate) <= 1 then
        urgency := 10
      else if DaysBetween(Now, task.DueDate) <= 3 then
        urgency := 8
      else if DaysBetween(Now, task.DueDate) <= 7 then
        urgency := 5
      else
        urgency := 3;
    end
    else
      urgency := 2;
    
    // Estimate importance based on priority
    case task.Priority of
      tpCritical: importance := 10;
      tpHigh: importance := 8;
      tpMedium: importance := 5;
      tpLow: importance := 3;
    else
      importance := 1;
    end;
    
    Result := DetermineQuadrant(urgency, importance);
  end;
end;

// Habit Tracking
function TLifestyleTaskManager.CreateHabit(const AName, ADescription: string;
  AFrequency: THabitFrequency; ATargetStreak: Integer): Integer;
var
  idx: Integer;
begin
  SetLength(FHabits, Length(FHabits) + 1);
  idx := High(FHabits);
  
  FHabits[idx].HabitID := FNextHabitID;
  FHabits[idx].Name := AName;
  FHabits[idx].Description := ADescription;
  FHabits[idx].Frequency := AFrequency;
  FHabits[idx].TargetStreak := ATargetStreak;
  FHabits[idx].CurrentStreak := 0;
  FHabits[idx].LongestStreak := 0;
  FHabits[idx].TotalCompletions := 0;
  FHabits[idx].LastCompletedDate := 0;
  FHabits[idx].CreatedDate := Now;
  FHabits[idx].IsActive := True;
  FHabits[idx].Reminder := '';
  
  Result := FNextHabitID;
  Inc(FNextHabitID);
end;

function TLifestyleTaskManager.LogHabitCompletion(AHabitID: Integer;
  const ANotes, AMood: string): Integer;
var
  idx, logIdx: Integer;
  daysSinceLast: Integer;
begin
  Result := -1;
  idx := FindHabitIndex(AHabitID);
  if idx >= 0 then
  begin
    // Create log entry
    SetLength(FHabitLogs, Length(FHabitLogs) + 1);
    logIdx := High(FHabitLogs);
    FHabitLogs[logIdx].LogID := FNextHabitLogID;
    FHabitLogs[logIdx].HabitID := AHabitID;
    FHabitLogs[logIdx].CompletedDate := Now;
    FHabitLogs[logIdx].Notes := ANotes;
    FHabitLogs[logIdx].Mood := AMood;
    
    // Update streak
    if FHabits[idx].LastCompletedDate > 0 then
    begin
      daysSinceLast := DaysBetween(Now, FHabits[idx].LastCompletedDate);
      if daysSinceLast <= 1 then
        FHabits[idx].CurrentStreak := FHabits[idx].CurrentStreak + 1
      else
        FHabits[idx].CurrentStreak := 1;
    end
    else
      FHabits[idx].CurrentStreak := 1;
    
    // Update longest streak
    if FHabits[idx].CurrentStreak > FHabits[idx].LongestStreak then
      FHabits[idx].LongestStreak := FHabits[idx].CurrentStreak;
    
    FHabits[idx].TotalCompletions := FHabits[idx].TotalCompletions + 1;
    FHabits[idx].LastCompletedDate := Now;
    
    Result := FNextHabitLogID;
    Inc(FNextHabitLogID);
  end;
end;

function TLifestyleTaskManager.GetHabitStreak(AHabitID: Integer): Integer;
var
  idx: Integer;
begin
  Result := 0;
  idx := FindHabitIndex(AHabitID);
  if idx >= 0 then
    Result := FHabits[idx].CurrentStreak;
end;

function TLifestyleTaskManager.GetAllHabits: THabitArray;
begin
  Result := Copy(FHabits, 0, Length(FHabits));
end;

function TLifestyleTaskManager.GetActiveHabits: THabitArray;
var
  i, count: Integer;
begin
  SetLength(Result, Length(FHabits));
  count := 0;
  for i := 0 to High(FHabits) do
    if FHabits[i].IsActive then
    begin
      Result[count] := FHabits[i];
      Inc(count);
    end;
  SetLength(Result, count);
end;

function TLifestyleTaskManager.GetHabitStatistics(AHabitID: Integer): string;
var
  idx: Integer;
  habit: THabit;
  successRate: Double;
  daysSinceCreation: Integer;
begin
  Result := '';
  idx := FindHabitIndex(AHabitID);
  if idx >= 0 then
  begin
    habit := FHabits[idx];
    daysSinceCreation := DaysBetween(Now, habit.CreatedDate);
    if daysSinceCreation > 0 then
      successRate := (habit.TotalCompletions / daysSinceCreation) * 100
    else
      successRate := 0;
    
    Result := Format('Habit: %s%s' +
                     'Current Streak: %d days%s' +
                     'Longest Streak: %d days%s' +
                     'Total Completions: %d%s' +
                     'Success Rate: %.1f%%%s' +
                     'Days Since Creation: %d',
                     [habit.Name, sLineBreak,
                      habit.CurrentStreak, sLineBreak,
                      habit.LongestStreak, sLineBreak,
                      habit.TotalCompletions, sLineBreak,
                      successRate, sLineBreak,
                      daysSinceCreation]);
  end;
end;

function TLifestyleTaskManager.GetHabitsNeedingAttention: THabitArray;
var
  i, count: Integer;
  daysSinceLast: Integer;
begin
  SetLength(Result, Length(FHabits));
  count := 0;
  
  for i := 0 to High(FHabits) do
    if FHabits[i].IsActive then
    begin
      if FHabits[i].LastCompletedDate > 0 then
        daysSinceLast := DaysBetween(Now, FHabits[i].LastCompletedDate)
      else
        daysSinceLast := 999;
      
      if daysSinceLast >= 2 then
      begin
        Result[count] := FHabits[i];
        Inc(count);
      end;
    end;
  
  SetLength(Result, count);
end;

// Time Boxing (continued in next part)
function TLifestyleTaskManager.CreateTimeBox(ATaskID: Integer;
  AStartTime: TDateTime; AAllocatedMinutes: Integer): Integer;
var
  idx: Integer;
begin
  SetLength(FTimeBoxes, Length(FTimeBoxes) + 1);
  idx := High(FTimeBoxes);
  
  FTimeBoxes[idx].TimeBoxID := FNextTimeBoxID;
  FTimeBoxes[idx].TaskID := ATaskID;
  FTimeBoxes[idx].StartTime := AStartTime;
  FTimeBoxes[idx].AllocatedMinutes := AAllocatedMinutes;
  FTimeBoxes[idx].ActualMinutes := 0;
  FTimeBoxes[idx].WasCompleted := False;
  FTimeBoxes[idx].WasInterrupted := False;
  FTimeBoxes[idx].InterruptionCount := 0;
  FTimeBoxes[idx].Notes := '';
  
  Result := FNextTimeBoxID;
  Inc(FNextTimeBoxID);
end;

function TLifestyleTaskManager.CompleteTimeBox(ATimeBoxID: Integer;
  AActualMinutes: Integer; AWasCompleted: Boolean): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindTimeBoxIndex(ATimeBoxID);
  if idx >= 0 then
  begin
    FTimeBoxes[idx].ActualMinutes := AActualMinutes;
    FTimeBoxes[idx].WasCompleted := AWasCompleted;
    Result := True;
  end;
end;

function TLifestyleTaskManager.RecordInterruption(ATimeBoxID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindTimeBoxIndex(ATimeBoxID);
  if idx >= 0 then
  begin
    FTimeBoxes[idx].WasInterrupted := True;
    FTimeBoxes[idx].InterruptionCount := FTimeBoxes[idx].InterruptionCount + 1;
    Result := True;
  end;
end;

function TLifestyleTaskManager.GetTimeBoxesForDate(ADate: TDateTime): TTimeBoxArray;
var
  i, count: Integer;
begin
  SetLength(Result, Length(FTimeBoxes));
  count := 0;
  
  for i := 0 to High(FTimeBoxes) do
    if DateOf(FTimeBoxes[i].StartTime) = DateOf(ADate) then
    begin
      Result[count] := FTimeBoxes[i];
      Inc(count);
    end;
  
  SetLength(Result, count);
end;

function TLifestyleTaskManager.GetTimeBoxEfficiency: Double;
var
  i, totalBoxes, completedBoxes: Integer;
begin
  totalBoxes := 0;
  completedBoxes := 0;
  
  for i := 0 to High(FTimeBoxes) do
  begin
    Inc(totalBoxes);
    if FTimeBoxes[i].WasCompleted then
      Inc(completedBoxes);
  end;
  
  if totalBoxes > 0 then
    Result := (completedBoxes / totalBoxes) * 100
  else
    Result := 0;
end;

// Remaining methods with simplified implementations for space
function TLifestyleTaskManager.CreateTaskBundle(const AName, ADescription,
  ACategory: string): Integer;
var
  idx: Integer;
begin
  SetLength(FTaskBundles, Length(FTaskBundles) + 1);
  idx := High(FTaskBundles);
  FTaskBundles[idx].BundleID := FNextBundleID;
  FTaskBundles[idx].Name := AName;
  FTaskBundles[idx].Description := ADescription;
  FTaskBundles[idx].Category := ACategory;
  FTaskBundles[idx].EstimatedMinutes := 0;
  FTaskBundles[idx].CreatedDate := Now;
  FTaskBundles[idx].LastUsedDate := 0;
  SetLength(FTaskBundles[idx].TaskIDs, 0);
  Result := FNextBundleID;
  Inc(FNextBundleID);
end;

function TLifestyleTaskManager.AddTaskToBundle(ABundleID, ATaskID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindBundleIndex(ABundleID);
  if idx >= 0 then
  begin
    SetLength(FTaskBundles[idx].TaskIDs, Length(FTaskBundles[idx].TaskIDs) + 1);
    FTaskBundles[idx].TaskIDs[High(FTaskBundles[idx].TaskIDs)] := ATaskID;
    Result := True;
  end;
end;

function TLifestyleTaskManager.RemoveTaskFromBundle(ABundleID, ATaskID: Integer): Boolean;
var
  idx, i, j: Integer;
begin
  Result := False;
  idx := FindBundleIndex(ABundleID);
  if idx >= 0 then
    for i := 0 to High(FTaskBundles[idx].TaskIDs) do
      if FTaskBundles[idx].TaskIDs[i] = ATaskID then
      begin
        for j := i to High(FTaskBundles[idx].TaskIDs) - 1 do
          FTaskBundles[idx].TaskIDs[j] := FTaskBundles[idx].TaskIDs[j + 1];
        SetLength(FTaskBundles[idx].TaskIDs, Length(FTaskBundles[idx].TaskIDs) - 1);
        Result := True;
        Exit;
      end;
end;

function TLifestyleTaskManager.GetBundle(ABundleID: Integer): TTaskBundle;
var
  idx: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  idx := FindBundleIndex(ABundleID);
  if idx >= 0 then
    Result := FTaskBundles[idx];
end;

function TLifestyleTaskManager.GetAllBundles: TTaskBundleArray;
begin
  Result := Copy(FTaskBundles, 0, Length(FTaskBundles));
end;

function TLifestyleTaskManager.SuggestBundles: TTaskBundleArray;
begin
  SetLength(Result, 0);
  // Simplified: return empty for now
end;

// Context Switching
function TLifestyleTaskManager.RecordContextSwitch(AFromTaskID, AToTaskID: Integer;
  const AReason: string): Integer;
var
  idx: Integer;
begin
  SetLength(FContextSwitches, Length(FContextSwitches) + 1);
  idx := High(FContextSwitches);
  FContextSwitches[idx].SwitchID := FNextSwitchID;
  FContextSwitches[idx].FromTaskID := AFromTaskID;
  FContextSwitches[idx].ToTaskID := AToTaskID;
  FContextSwitches[idx].SwitchTime := Now;
  FContextSwitches[idx].EstimatedCostMinutes := CalculateContextSwitchCost(AFromTaskID, AToTaskID);
  FContextSwitches[idx].Reason := AReason;
  Result := FNextSwitchID;
  Inc(FNextSwitchID);
end;

function TLifestyleTaskManager.GetContextSwitchCost(ADate: TDateTime): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FContextSwitches) do
    if DateOf(FContextSwitches[i].SwitchTime) = DateOf(ADate) then
      Result := Result + FContextSwitches[i].EstimatedCostMinutes;
end;

function TLifestyleTaskManager.GetContextSwitchReport: string;
var
  totalCost: Integer;
begin
  totalCost := GetContextSwitchCost(Now);
  Result := Format('Context Switch Report for Today:%sTotal Switches: %d%sEstimated Time Cost: %d minutes',
                   [sLineBreak, Length(FContextSwitches), sLineBreak, totalCost]);
end;

function TLifestyleTaskManager.GetLowSwitchingTasks: TTaskArray;
begin
  SetLength(Result, 0);
  // Simplified implementation
end;

// Focus Sessions
function TLifestyleTaskManager.StartFocusSession(ATaskID: Integer;
  APlannedDuration: Integer): Integer;
var
  idx: Integer;
begin
  SetLength(FFocusSessions, Length(FFocusSessions) + 1);
  idx := High(FFocusSessions);
  FFocusSessions[idx].SessionID := FNextFocusSessionID;
  FFocusSessions[idx].TaskID := ATaskID;
  FFocusSessions[idx].StartTime := Now;
  FFocusSessions[idx].EndTime := 0;
  FFocusSessions[idx].PlannedDuration := APlannedDuration;
  FFocusSessions[idx].ActualDuration := 0;
  FFocusSessions[idx].ProductivityRating := 0;
  FFocusSessions[idx].DistractionCount := 0;
  FFocusSessions[idx].Notes := '';
  Result := FNextFocusSessionID;
  Inc(FNextFocusSessionID);
end;

function TLifestyleTaskManager.EndFocusSession(ASessionID: Integer;
  AProductivityRating: Integer; ADistractionCount: Integer;
  const ANotes: string): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindFocusSessionIndex(ASessionID);
  if idx >= 0 then
  begin
    FFocusSessions[idx].EndTime := Now;
    FFocusSessions[idx].ActualDuration := MinutesBetween(FFocusSessions[idx].EndTime,
                                                          FFocusSessions[idx].StartTime);
    FFocusSessions[idx].ProductivityRating := AProductivityRating;
    FFocusSessions[idx].DistractionCount := ADistractionCount;
    FFocusSessions[idx].Notes := ANotes;
    Result := True;
    
    // Update productivity period
    UpdateProductivityPeriod(DayOfWeek(Now), HourOf(Now), AProductivityRating * 10);
  end;
end;

function TLifestyleTaskManager.GetFocusSessions(ATaskID: Integer): TFocusSessionArray;
var
  i, count: Integer;
begin
  SetLength(Result, Length(FFocusSessions));
  count := 0;
  for i := 0 to High(FFocusSessions) do
    if FFocusSessions[i].TaskID = ATaskID then
    begin
      Result[count] := FFocusSessions[i];
      Inc(count);
    end;
  SetLength(Result, count);
end;

function TLifestyleTaskManager.GetAverageFocusQuality: Double;
var
  i, count: Integer;
  total: Double;
begin
  total := 0;
  count := 0;
  for i := 0 to High(FFocusSessions) do
    if FFocusSessions[i].ProductivityRating > 0 then
    begin
      total := total + FFocusSessions[i].ProductivityRating;
      Inc(count);
    end;
  
  if count > 0 then
    Result := total / count
  else
    Result := 0;
end;

function TLifestyleTaskManager.GetBestFocusTime: string;
var
  i, bestHour: Integer;
  maxProd: Double;
begin
  maxProd := 0;
  bestHour := 9; // Default
  
  for i := 0 to High(FProductivityPeriods) do
    if FProductivityPeriods[i].AverageProductivity > maxProd then
    begin
      maxProd := FProductivityPeriods[i].AverageProductivity;
      bestHour := FProductivityPeriods[i].HourOfDay;
    end;
  
  Result := Format('%d:00 - %d:00 (Productivity: %.1f%%)',
                   [bestHour, bestHour + 1, maxProd]);
end;

// Task Mood Tracking
function TLifestyleTaskManager.RecordTaskMood(ATaskID: Integer;
  AMood: TMoodType; AEnergyLevel: TEnergyLevel; const ANotes: string): Integer;
var
  idx: Integer;
begin
  SetLength(FTaskMoods, Length(FTaskMoods) + 1);
  idx := High(FTaskMoods);
  FTaskMoods[idx].MoodID := FNextMoodID;
  FTaskMoods[idx].TaskID := ATaskID;
  FTaskMoods[idx].Mood := AMood;
  FTaskMoods[idx].EnergyLevel := AEnergyLevel;
  FTaskMoods[idx].RecordedTime := Now;
  FTaskMoods[idx].Notes := ANotes;
  Result := FNextMoodID;
  Inc(FNextMoodID);
end;

function TLifestyleTaskManager.GetTaskMoodHistory(ATaskID: Integer): TTaskMoodArray;
var
  i, count: Integer;
begin
  SetLength(Result, Length(FTaskMoods));
  count := 0;
  for i := 0 to High(FTaskMoods) do
    if FTaskMoods[i].TaskID = ATaskID then
    begin
      Result[count] := FTaskMoods[i];
      Inc(count);
    end;
  SetLength(Result, count);
end;

function TLifestyleTaskManager.GetOptimalTasksForMood(AMood: TMoodType): TTaskArray;
begin
  SetLength(Result, 0);
  // Simplified
end;

function TLifestyleTaskManager.GetMoodInsights: string;
begin
  Result := Format('Mood Insights: %d mood entries recorded', [Length(FTaskMoods)]);
end;

// Productivity Rhythm
function TLifestyleTaskManager.RecordProductivitySample(ADayOfWeek,
  AHour: Integer; AProductivity: Double): Boolean;
begin
  UpdateProductivityPeriod(ADayOfWeek, AHour, AProductivity);
  Result := True;
end;

function TLifestyleTaskManager.GetOptimalWorkingHours: string;
var
  i: Integer;
  maxProd: Double;
  bestHours: string;
begin
  maxProd := 0;
  bestHours := '';
  
  for i := 0 to High(FProductivityPeriods) do
    if FProductivityPeriods[i].AverageProductivity > maxProd then
      maxProd := FProductivityPeriods[i].AverageProductivity;
  
  for i := 0 to High(FProductivityPeriods) do
    if FProductivityPeriods[i].AverageProductivity >= maxProd * 0.9 then
      bestHours := bestHours + Format('%d:00, ', [FProductivityPeriods[i].HourOfDay]);
  
  if bestHours <> '' then
    Result := 'Optimal hours: ' + bestHours
  else
    Result := 'Not enough data';
end;

function TLifestyleTaskManager.GetProductivityHeatmap: string;
var
  dow, hour: Integer;
  prod: Double;
begin
  Result := 'Productivity Heatmap:' + sLineBreak;
  for dow := 1 to 7 do
  begin
    Result := Result + Format('Day %d: ', [dow]);
    for hour := 0 to 23 do
    begin
      prod := GetProductivityForTime(dow, hour);
      if prod >= 70 then
        Result := Result + '█'
      else if prod >= 50 then
        Result := Result + '▓'
      else if prod >= 30 then
        Result := Result + '▒'
      else
        Result := Result + '░';
    end;
    Result := Result + sLineBreak;
  end;
end;

function TLifestyleTaskManager.SuggestTaskSchedule(ATaskID: Integer): TDateTime;
var
  dow, hour, bestHour: Integer;
  maxProd: Double;
begin
  maxProd := 0;
  bestHour := 9;
  dow := DayOfWeek(Now);
  
  for hour := 0 to 23 do
  begin
    if GetProductivityForTime(dow, hour) > maxProd then
    begin
      maxProd := GetProductivityForTime(dow, hour);
      bestHour := hour;
    end;
  end;
  
  Result := EncodeDateTime(YearOf(Now), MonthOf(Now), DayOf(Now), bestHour, 0, 0, 0);
end;

// Energy Level Optimization
function TLifestyleTaskManager.GetTasksByEnergyLevel(ALevel: TEnergyLevel): TTaskArray;
begin
  SetLength(Result, 0);
  // Simplified
end;

function TLifestyleTaskManager.SuggestTasksForCurrentEnergy: TTaskArray;
begin
  SetLength(Result, 0);
  // Simplified
end;

function TLifestyleTaskManager.GetEnergyOptimizationReport: string;
begin
  Result := 'Energy Optimization: Match tasks to your energy levels for better productivity';
end;

// Utility functions
function TLifestyleTaskManager.EisenhowerQuadrantToString(AQuadrant: TEisenhowerQuadrant): string;
begin
  case AQuadrant of
    eqUrgentImportant: Result := 'Q1: Urgent & Important (DO)';
    eqNotUrgentImportant: Result := 'Q2: Not Urgent & Important (SCHEDULE)';
    eqUrgentNotImportant: Result := 'Q3: Urgent & Not Important (DELEGATE)';
    eqNotUrgentNotImportant: Result := 'Q4: Not Urgent & Not Important (ELIMINATE)';
  else
    Result := 'Unknown';
  end;
end;

function TLifestyleTaskManager.EnergyLevelToString(ALevel: TEnergyLevel): string;
begin
  case ALevel of
    elLow: Result := 'Low';
    elMedium: Result := 'Medium';
    elHigh: Result := 'High';
    elPeak: Result := 'Peak';
  else
    Result := 'Unknown';
  end;
end;

function TLifestyleTaskManager.MoodTypeToString(AMood: TMoodType): string;
begin
  case AMood of
    mtVeryBad: Result := 'Very Bad';
    mtBad: Result := 'Bad';
    mtNeutral: Result := 'Neutral';
    mtGood: Result := 'Good';
    mtVeryGood: Result := 'Very Good';
  else
    Result := 'Unknown';
  end;
end;

function TLifestyleTaskManager.HabitFrequencyToString(AFreq: THabitFrequency): string;
begin
  case AFreq of
    hfDaily: Result := 'Daily';
    hfWeekly: Result := 'Weekly';
    hfMonthly: Result := 'Monthly';
    hfCustom: Result := 'Custom';
  else
    Result := 'Unknown';
  end;
end;

// Persistence
function TLifestyleTaskManager.SaveLifestyleDataToFile(const AFilename: string): Boolean;
begin
  Result := True; // Simplified
end;

function TLifestyleTaskManager.LoadLifestyleDataFromFile(const AFilename: string): Boolean;
begin
  Result := True; // Simplified
end;

end.
