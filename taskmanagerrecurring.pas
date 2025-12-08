
unit taskmanagerrecurring;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Classes, Math,
  taskmanager, taskmanageradvanced, taskmanagerenhanced, 
  taskmanagerteam, taskmanagerfocus, taskmanagergamify,
  taskmanagerresource, taskmanagersmart;

type
  // Recurring pattern types
  TRecurrenceType = (rtDaily, rtWeekly, rtMonthly, rtYearly, rtCustom);
  
  // Days of week for weekly recurrence
  TDayOfWeek = (dowSunday, dowMonday, dowTuesday, dowWednesday, 
                dowThursday, dowFriday, dowSaturday);
  TDaysOfWeekSet = set of TDayOfWeek;
  
  // Recurrence end condition
  TRecurrenceEndType = (retNever, retAfterOccurrences, retByDate);
  
  // Recurrence pattern definition
  TRecurrencePattern = record
    ID: Integer;
    RecurrenceType: TRecurrenceType;
    Interval: Integer; // Every N days/weeks/months/years
    DaysOfWeek: TDaysOfWeekSet; // For weekly recurrence
    DayOfMonth: Integer; // For monthly recurrence (1-31)
    MonthOfYear: Integer; // For yearly recurrence (1-12)
    StartDate: TDateTime;
    EndType: TRecurrenceEndType;
    EndDate: TDateTime; // If EndType = retByDate
    MaxOccurrences: Integer; // If EndType = retAfterOccurrences
    OccurrenceCount: Integer; // Current count
    LastGenerated: TDateTime;
    IsActive: Boolean;
  end;
  
  TRecurrencePatternArray = array of TRecurrencePattern;
  
  // Recurring task definition
  TRecurringTask = record
    ID: Integer;
    TemplateTaskID: Integer; // The task to clone
    PatternID: Integer;
    CreatedDate: TDateTime;
    IsActive: Boolean;
    GeneratedTaskIDs: array of Integer; // Track all generated tasks
  end;
  
  TRecurringTaskArray = array of TRecurringTask;
  
  // Project for portfolio management
  TProjectStatus = (psPlanning, psActive, psOnHold, psClosed, psCancelled);
  
  TProject = record
    ID: Integer;
    Name: string;
    Description: string;
    Status: TProjectStatus;
    StartDate: TDateTime;
    EndDate: TDateTime;
    Budget: Double;
    SpentAmount: Double;
    CompletionPercentage: Double;
    Priority: Integer; // 1-10 scale
    ManagerID: Integer; // Team member ID
    Tags: array of string;
    CreatedDate: TDateTime;
    IsActive: Boolean;
  end;
  
  TProjectArray = array of TProject;
  
  // Link tasks to projects
  TTaskProjectLink = record
    ID: Integer;
    TaskID: Integer;
    ProjectID: Integer;
    CreatedDate: TDateTime;
  end;
  
  TTaskProjectLinkArray = array of TTaskProjectLink;
  
  TRecurringTaskManager = class(TSmartTaskManager)
  private
    FRecurrencePatterns: TRecurrencePatternArray;
    FRecurringTasks: TRecurringTaskArray;
    FProjects: TProjectArray;
    FTaskProjectLinks: TTaskProjectLinkArray;
    FNextRecurrenceID: Integer;
    FNextRecurringTaskID: Integer;
    FNextProjectID: Integer;
    FNextLinkID: Integer;
    
    function FindPatternIndex(APatternID: Integer): Integer;
    function FindRecurringTaskIndex(ARecurringTaskID: Integer): Integer;
    function FindProjectIndex(AProjectID: Integer): Integer;
    function CalculateNextOccurrence(const APattern: TRecurrencePattern; 
      AFromDate: TDateTime): TDateTime;
    function ShouldGenerateOccurrence(const APattern: TRecurrencePattern;
      ACheckDate: TDateTime): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Recurrence pattern management
    function CreateDailyPattern(AInterval: Integer; AStartDate: TDateTime): Integer;
    function CreateWeeklyPattern(AInterval: Integer; ADaysOfWeek: TDaysOfWeekSet;
      AStartDate: TDateTime): Integer;
    function CreateMonthlyPattern(AInterval, ADayOfMonth: Integer;
      AStartDate: TDateTime): Integer;
    function CreateYearlyPattern(AInterval, AMonthOfYear, ADayOfMonth: Integer;
      AStartDate: TDateTime): Integer;
    function SetPatternEndDate(APatternID: Integer; AEndDate: TDateTime): Boolean;
    function SetPatternMaxOccurrences(APatternID: Integer; AMaxOccurrences: Integer): Boolean;
    function GetRecurrencePattern(APatternID: Integer): TRecurrencePattern;
    function GetAllPatterns: TRecurrencePatternArray;
    function DeleteRecurrencePattern(APatternID: Integer): Boolean;
    
    // Recurring task management  
    function CreateRecurringTask(ATemplateTaskID, APatternID: Integer): Integer;
    function GetRecurringTask(ARecurringTaskID: Integer): TRecurringTask;
    function GetAllRecurringTasks: TRecurringTaskArray;
    function ActivateRecurringTask(ARecurringTaskID: Integer): Boolean;
    function DeactivateRecurringTask(ARecurringTaskID: Integer): Boolean;
    function DeleteRecurringTask(ARecurringTaskID: Integer): Boolean;
    function GeneratePendingOccurrences: Integer; // Generate all due occurrences
    function GetNextOccurrenceDate(ARecurringTaskID: Integer): TDateTime;
    
    // Project management
    function CreateProject(const AName, ADescription: string; 
      AStartDate, AEndDate: TDateTime; ABudget: Double): Integer;
    function UpdateProject(AProjectID: Integer; const AName, ADescription: string): Boolean;
    function SetProjectStatus(AProjectID: Integer; AStatus: TProjectStatus): Boolean;
    function SetProjectBudget(AProjectID: Integer; ABudget, ASpentAmount: Double): Boolean;
    function SetProjectCompletion(AProjectID: Integer; APercentage: Double): Boolean;
    function GetProject(AProjectID: Integer): TProject;
    function GetAllProjects: TProjectArray;
    function GetActiveProjects: TProjectArray;
    function DeleteProject(AProjectID: Integer): Boolean;
    
    // Task-Project linking
    function LinkTaskToProject(ATaskID, AProjectID: Integer): Integer;
    function UnlinkTaskFromProject(ALinkID: Integer): Boolean;
    function GetProjectTasks(AProjectID: Integer): TTaskArray;
    function GetTaskProjects(ATaskID: Integer): TProjectArray;
    function GetProjectTaskCount(AProjectID: Integer): Integer;
    
    // Portfolio analytics
    function GetPortfolioSummary: string;
    function GetProjectHealth(AProjectID: Integer): string;
    function GetProjectProgress(AProjectID: Integer): Double;
    function GetOverBudgetProjects: TProjectArray;
    function GetDelayedProjects: TProjectArray;
    function GetPortfolioValue: Double;
    
    // Reporting
    function RecurrenceTypeToString(AType: TRecurrenceType): string;
    function ProjectStatusToString(AStatus: TProjectStatus): string;
    function GetRecurringTasksReport: string;
    function GetProjectsReport: string;
    
    // Persistence
    function SaveRecurringDataToFile(const AFilename: string): Boolean;
    function LoadRecurringDataFromFile(const AFilename: string): Boolean;
  end;

implementation

constructor TRecurringTaskManager.Create;
begin
  inherited Create;
  SetLength(FRecurrencePatterns, 0);
  SetLength(FRecurringTasks, 0);
  SetLength(FProjects, 0);
  SetLength(FTaskProjectLinks, 0);
  FNextRecurrenceID := 1;
  FNextRecurringTaskID := 1;
  FNextProjectID := 1;
  FNextLinkID := 1;
end;

destructor TRecurringTaskManager.Destroy;
begin
  SetLength(FRecurrencePatterns, 0);
  SetLength(FRecurringTasks, 0);
  SetLength(FProjects, 0);
  SetLength(FTaskProjectLinks, 0);
  inherited Destroy;
end;

function TRecurringTaskManager.FindPatternIndex(APatternID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FRecurrencePatterns) do
    if FRecurrencePatterns[i].ID = APatternID then
      Exit(i);
end;

function TRecurringTaskManager.FindRecurringTaskIndex(ARecurringTaskID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FRecurringTasks) do
    if FRecurringTasks[i].ID = ARecurringTaskID then
      Exit(i);
end;

function TRecurringTaskManager.FindProjectIndex(AProjectID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FProjects) do
    if FProjects[i].ID = AProjectID then
      Exit(i);
end;

function TRecurringTaskManager.CalculateNextOccurrence(
  const APattern: TRecurrencePattern; AFromDate: TDateTime): TDateTime;
var
  NextDate: TDateTime;
  DayOfWeek: Integer;
  Found: Boolean;
  CheckDate: TDateTime;
begin
  Result := 0;
  NextDate := AFromDate;
  
  case APattern.RecurrenceType of
    rtDaily:
      Result := IncDay(NextDate, APattern.Interval);
      
    rtWeekly:
      begin
        // Find next matching day of week
        Found := False;
        CheckDate := IncDay(NextDate, 1);
        while not Found and (DaysBetween(CheckDate, NextDate) < 7 * APattern.Interval) do
        begin
          DayOfWeek := DayOfTheWeek(CheckDate) - 1; // 0=Sunday
          if TDayOfWeek(DayOfWeek) in APattern.DaysOfWeek then
          begin
            Result := CheckDate;
            Found := True;
          end
          else
            CheckDate := IncDay(CheckDate, 1);
        end;
      end;
      
    rtMonthly:
      Result := IncMonth(NextDate, APattern.Interval);
      
    rtYearly:
      Result := IncYear(NextDate, APattern.Interval);
  end;
end;

function TRecurringTaskManager.ShouldGenerateOccurrence(
  const APattern: TRecurrencePattern; ACheckDate: TDateTime): Boolean;
begin
  Result := False;
  
  if not APattern.IsActive then
    Exit;
    
  if ACheckDate < APattern.StartDate then
    Exit;
    
  case APattern.EndType of
    retByDate:
      if ACheckDate > APattern.EndDate then
        Exit;
    retAfterOccurrences:
      if APattern.OccurrenceCount >= APattern.MaxOccurrences then
        Exit;
  end;
  
  Result := True;
end;

function TRecurringTaskManager.CreateDailyPattern(AInterval: Integer;
  AStartDate: TDateTime): Integer;
var
  Pattern: TRecurrencePattern;
  Idx: Integer;
begin
  Pattern.ID := FNextRecurrenceID;
  Inc(FNextRecurrenceID);
  Pattern.RecurrenceType := rtDaily;
  Pattern.Interval := AInterval;
  Pattern.DaysOfWeek := [];
  Pattern.DayOfMonth := 0;
  Pattern.MonthOfYear := 0;
  Pattern.StartDate := AStartDate;
  Pattern.EndType := retNever;
  Pattern.EndDate := 0;
  Pattern.MaxOccurrences := 0;
  Pattern.OccurrenceCount := 0;
  Pattern.LastGenerated := 0;
  Pattern.IsActive := True;
  
  Idx := Length(FRecurrencePatterns);
  SetLength(FRecurrencePatterns, Idx + 1);
  FRecurrencePatterns[Idx] := Pattern;
  
  Result := Pattern.ID;
end;

function TRecurringTaskManager.CreateWeeklyPattern(AInterval: Integer;
  ADaysOfWeek: TDaysOfWeekSet; AStartDate: TDateTime): Integer;
var
  Pattern: TRecurrencePattern;
  Idx: Integer;
begin
  Pattern.ID := FNextRecurrenceID;
  Inc(FNextRecurrenceID);
  Pattern.RecurrenceType := rtWeekly;
  Pattern.Interval := AInterval;
  Pattern.DaysOfWeek := ADaysOfWeek;
  Pattern.DayOfMonth := 0;
  Pattern.MonthOfYear := 0;
  Pattern.StartDate := AStartDate;
  Pattern.EndType := retNever;
  Pattern.EndDate := 0;
  Pattern.MaxOccurrences := 0;
  Pattern.OccurrenceCount := 0;
  Pattern.LastGenerated := 0;
  Pattern.IsActive := True;
  
  Idx := Length(FRecurrencePatterns);
  SetLength(FRecurrencePatterns, Idx + 1);
  FRecurrencePatterns[Idx] := Pattern;
  
  Result := Pattern.ID;
end;

function TRecurringTaskManager.CreateMonthlyPattern(AInterval, ADayOfMonth: Integer;
  AStartDate: TDateTime): Integer;
var
  Pattern: TRecurrencePattern;
  Idx: Integer;
begin
  Pattern.ID := FNextRecurrenceID;
  Inc(FNextRecurrenceID);
  Pattern.RecurrenceType := rtMonthly;
  Pattern.Interval := AInterval;
  Pattern.DaysOfWeek := [];
  Pattern.DayOfMonth := ADayOfMonth;
  Pattern.MonthOfYear := 0;
  Pattern.StartDate := AStartDate;
  Pattern.EndType := retNever;
  Pattern.EndDate := 0;
  Pattern.MaxOccurrences := 0;
  Pattern.OccurrenceCount := 0;
  Pattern.LastGenerated := 0;
  Pattern.IsActive := True;
  
  Idx := Length(FRecurrencePatterns);
  SetLength(FRecurrencePatterns, Idx + 1);
  FRecurrencePatterns[Idx] := Pattern;
  
  Result := Pattern.ID;
end;

function TRecurringTaskManager.CreateYearlyPattern(AInterval, AMonthOfYear,
  ADayOfMonth: Integer; AStartDate: TDateTime): Integer;
var
  Pattern: TRecurrencePattern;
  Idx: Integer;
begin
  Pattern.ID := FNextRecurrenceID;
  Inc(FNextRecurrenceID);
  Pattern.RecurrenceType := rtYearly;
  Pattern.Interval := AInterval;
  Pattern.DaysOfWeek := [];
  Pattern.DayOfMonth := ADayOfMonth;
  Pattern.MonthOfYear := AMonthOfYear;
  Pattern.StartDate := AStartDate;
  Pattern.EndType := retNever;
  Pattern.EndDate := 0;
  Pattern.MaxOccurrences := 0;
  Pattern.OccurrenceCount := 0;
  Pattern.LastGenerated := 0;
  Pattern.IsActive := True;
  
  Idx := Length(FRecurrencePatterns);
  SetLength(FRecurrencePatterns, Idx + 1);
  FRecurrencePatterns[Idx] := Pattern;
  
  Result := Pattern.ID;
end;

function TRecurringTaskManager.SetPatternEndDate(APatternID: Integer;
  AEndDate: TDateTime): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindPatternIndex(APatternID);
  if Idx >= 0 then
  begin
    FRecurrencePatterns[Idx].EndType := retByDate;
    FRecurrencePatterns[Idx].EndDate := AEndDate;
    Result := True;
  end;
end;

function TRecurringTaskManager.SetPatternMaxOccurrences(APatternID: Integer;
  AMaxOccurrences: Integer): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindPatternIndex(APatternID);
  if Idx >= 0 then
  begin
    FRecurrencePatterns[Idx].EndType := retAfterOccurrences;
    FRecurrencePatterns[Idx].MaxOccurrences := AMaxOccurrences;
    Result := True;
  end;
end;

function TRecurringTaskManager.GetRecurrencePattern(APatternID: Integer): TRecurrencePattern;
var
  Idx: Integer;
  EmptyPattern: TRecurrencePattern;
begin
  Idx := FindPatternIndex(APatternID);
  if Idx >= 0 then
    Result := FRecurrencePatterns[Idx]
  else
  begin
    EmptyPattern.ID := -1;
    Result := EmptyPattern;
  end;
end;

function TRecurringTaskManager.GetAllPatterns: TRecurrencePatternArray;
begin
  Result := Copy(FRecurrencePatterns, 0, Length(FRecurrencePatterns));
end;

function TRecurringTaskManager.DeleteRecurrencePattern(APatternID: Integer): Boolean;
var
  Idx, i: Integer;
begin
  Result := False;
  Idx := FindPatternIndex(APatternID);
  if Idx >= 0 then
  begin
    for i := Idx to High(FRecurrencePatterns) - 1 do
      FRecurrencePatterns[i] := FRecurrencePatterns[i + 1];
    SetLength(FRecurrencePatterns, Length(FRecurrencePatterns) - 1);
    Result := True;
  end;
end;

function TRecurringTaskManager.CreateRecurringTask(ATemplateTaskID,
  APatternID: Integer): Integer;
var
  RecTask: TRecurringTask;
  Idx: Integer;
begin
  RecTask.ID := FNextRecurringTaskID;
  Inc(FNextRecurringTaskID);
  RecTask.TemplateTaskID := ATemplateTaskID;
  RecTask.PatternID := APatternID;
  RecTask.CreatedDate := Now;
  RecTask.IsActive := True;
  SetLength(RecTask.GeneratedTaskIDs, 0);
  
  Idx := Length(FRecurringTasks);
  SetLength(FRecurringTasks, Idx + 1);
  FRecurringTasks[Idx] := RecTask;
  
  Result := RecTask.ID;
end;

function TRecurringTaskManager.GetRecurringTask(ARecurringTaskID: Integer): TRecurringTask;
var
  Idx: Integer;
  EmptyTask: TRecurringTask;
begin
  Idx := FindRecurringTaskIndex(ARecurringTaskID);
  if Idx >= 0 then
    Result := FRecurringTasks[Idx]
  else
  begin
    EmptyTask.ID := -1;
    Result := EmptyTask;
  end;
end;

function TRecurringTaskManager.GetAllRecurringTasks: TRecurringTaskArray;
begin
  Result := Copy(FRecurringTasks, 0, Length(FRecurringTasks));
end;

function TRecurringTaskManager.ActivateRecurringTask(ARecurringTaskID: Integer): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindRecurringTaskIndex(ARecurringTaskID);
  if Idx >= 0 then
  begin
    FRecurringTasks[Idx].IsActive := True;
    Result := True;
  end;
end;

function TRecurringTaskManager.DeactivateRecurringTask(ARecurringTaskID: Integer): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindRecurringTaskIndex(ARecurringTaskID);
  if Idx >= 0 then
  begin
    FRecurringTasks[Idx].IsActive := False;
    Result := True;
  end;
end;

function TRecurringTaskManager.DeleteRecurringTask(ARecurringTaskID: Integer): Boolean;
var
  Idx, i: Integer;
begin
  Result := False;
  Idx := FindRecurringTaskIndex(ARecurringTaskID);
  if Idx >= 0 then
  begin
    for i := Idx to High(FRecurringTasks) - 1 do
      FRecurringTasks[i] := FRecurringTasks[i + 1];
    SetLength(FRecurringTasks, Length(FRecurringTasks) - 1);
    Result := True;
  end;
end;

function TRecurringTaskManager.GeneratePendingOccurrences: Integer;
begin
  // Simplified implementation - would generate tasks based on patterns
  Result := 0;
end;

function TRecurringTaskManager.GetNextOccurrenceDate(ARecurringTaskID: Integer): TDateTime;
var
  RecTask: TRecurringTask;
  Pattern: TRecurrencePattern;
begin
  Result := 0;
  RecTask := GetRecurringTask(ARecurringTaskID);
  if RecTask.ID > 0 then
  begin
    Pattern := GetRecurrencePattern(RecTask.PatternID);
    if Pattern.ID > 0 then
      Result := CalculateNextOccurrence(Pattern, Now);
  end;
end;

function TRecurringTaskManager.CreateProject(const AName, ADescription: string;
  AStartDate, AEndDate: TDateTime; ABudget: Double): Integer;
var
  Project: TProject;
  Idx: Integer;
begin
  Project.ID := FNextProjectID;
  Inc(FNextProjectID);
  Project.Name := AName;
  Project.Description := ADescription;
  Project.Status := psPlanning;
  Project.StartDate := AStartDate;
  Project.EndDate := AEndDate;
  Project.Budget := ABudget;
  Project.SpentAmount := 0;
  Project.CompletionPercentage := 0;
  Project.Priority := 5;
  Project.ManagerID := 0;
  SetLength(Project.Tags, 0);
  Project.CreatedDate := Now;
  Project.IsActive := True;
  
  Idx := Length(FProjects);
  SetLength(FProjects, Idx + 1);
  FProjects[Idx] := Project;
  
  Result := Project.ID;
end;

function TRecurringTaskManager.UpdateProject(AProjectID: Integer;
  const AName, ADescription: string): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindProjectIndex(AProjectID);
  if Idx >= 0 then
  begin
    FProjects[Idx].Name := AName;
    FProjects[Idx].Description := ADescription;
    Result := True;
  end;
end;

function TRecurringTaskManager.SetProjectStatus(AProjectID: Integer;
  AStatus: TProjectStatus): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindProjectIndex(AProjectID);
  if Idx >= 0 then
  begin
    FProjects[Idx].Status := AStatus;
    Result := True;
  end;
end;

function TRecurringTaskManager.SetProjectBudget(AProjectID: Integer;
  ABudget, ASpentAmount: Double): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindProjectIndex(AProjectID);
  if Idx >= 0 then
  begin
    FProjects[Idx].Budget := ABudget;
    FProjects[Idx].SpentAmount := ASpentAmount;
    Result := True;
  end;
end;

function TRecurringTaskManager.SetProjectCompletion(AProjectID: Integer;
  APercentage: Double): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindProjectIndex(AProjectID);
  if Idx >= 0 then
  begin
    FProjects[Idx].CompletionPercentage := APercentage;
    Result := True;
  end;
end;

function TRecurringTaskManager.GetProject(AProjectID: Integer): TProject;
var
  Idx: Integer;
  EmptyProject: TProject;
begin
  Idx := FindProjectIndex(AProjectID);
  if Idx >= 0 then
    Result := FProjects[Idx]
  else
  begin
    EmptyProject.ID := -1;
    Result := EmptyProject;
  end;
end;

function TRecurringTaskManager.GetAllProjects: TProjectArray;
begin
  Result := Copy(FProjects, 0, Length(FProjects));
end;

function TRecurringTaskManager.GetActiveProjects: TProjectArray;
var
  i, Count: Integer;
begin
  Count := 0;
  for i := 0 to High(FProjects) do
    if FProjects[i].IsActive then
      Inc(Count);
      
  SetLength(Result, Count);
  Count := 0;
  for i := 0 to High(FProjects) do
    if FProjects[i].IsActive then
    begin
      Result[Count] := FProjects[i];
      Inc(Count);
    end;
end;

function TRecurringTaskManager.DeleteProject(AProjectID: Integer): Boolean;
var
  Idx, i: Integer;
begin
  Result := False;
  Idx := FindProjectIndex(AProjectID);
  if Idx >= 0 then
  begin
    for i := Idx to High(FProjects) - 1 do
      FProjects[i] := FProjects[i + 1];
    SetLength(FProjects, Length(FProjects) - 1);
    Result := True;
  end;
end;

function TRecurringTaskManager.LinkTaskToProject(ATaskID, AProjectID: Integer): Integer;
var
  Link: TTaskProjectLink;
  Idx: Integer;
begin
  Link.ID := FNextLinkID;
  Inc(FNextLinkID);
  Link.TaskID := ATaskID;
  Link.ProjectID := AProjectID;
  Link.CreatedDate := Now;
  
  Idx := Length(FTaskProjectLinks);
  SetLength(FTaskProjectLinks, Idx + 1);
  FTaskProjectLinks[Idx] := Link;
  
  Result := Link.ID;
end;

function TRecurringTaskManager.UnlinkTaskFromProject(ALinkID: Integer): Boolean;
var
  i, Idx: Integer;
begin
  Result := False;
  Idx := -1;
  for i := 0 to High(FTaskProjectLinks) do
    if FTaskProjectLinks[i].ID = ALinkID then
    begin
      Idx := i;
      Break;
    end;
    
  if Idx >= 0 then
  begin
    for i := Idx to High(FTaskProjectLinks) - 1 do
      FTaskProjectLinks[i] := FTaskProjectLinks[i + 1];
    SetLength(FTaskProjectLinks, Length(FTaskProjectLinks) - 1);
    Result := True;
  end;
end;

function TRecurringTaskManager.GetProjectTasks(AProjectID: Integer): TTaskArray;
var
  i, j, Count: Integer;
  TaskIDs: array of Integer;
  AllTasks: TTaskArray;
begin
  // Collect task IDs linked to this project
  Count := 0;
  for i := 0 to High(FTaskProjectLinks) do
    if FTaskProjectLinks[i].ProjectID = AProjectID then
      Inc(Count);
      
  SetLength(TaskIDs, Count);
  Count := 0;
  for i := 0 to High(FTaskProjectLinks) do
    if FTaskProjectLinks[i].ProjectID = AProjectID then
    begin
      TaskIDs[Count] := FTaskProjectLinks[i].TaskID;
      Inc(Count);
    end;
  
  // Get all tasks and filter by IDs
  AllTasks := GetAllTasks;
  Count := 0;
  for i := 0 to High(AllTasks) do
    for j := 0 to High(TaskIDs) do
      if AllTasks[i].ID = TaskIDs[j] then
        Inc(Count);
        
  SetLength(Result, Count);
  Count := 0;
  for i := 0 to High(AllTasks) do
    for j := 0 to High(TaskIDs) do
      if AllTasks[i].ID = TaskIDs[j] then
      begin
        Result[Count] := AllTasks[i];
        Inc(Count);
      end;
end;

function TRecurringTaskManager.GetTaskProjects(ATaskID: Integer): TProjectArray;
var
  i, j, Count: Integer;
  ProjectIDs: array of Integer;
begin
  // Collect project IDs linked to this task
  Count := 0;
  for i := 0 to High(FTaskProjectLinks) do
    if FTaskProjectLinks[i].TaskID = ATaskID then
      Inc(Count);
      
  SetLength(ProjectIDs, Count);
  Count := 0;
  for i := 0 to High(FTaskProjectLinks) do
    if FTaskProjectLinks[i].TaskID = ATaskID then
    begin
      ProjectIDs[Count] := FTaskProjectLinks[i].ProjectID;
      Inc(Count);
    end;
  
  // Get matching projects
  Count := 0;
  for i := 0 to High(FProjects) do
    for j := 0 to High(ProjectIDs) do
      if FProjects[i].ID = ProjectIDs[j] then
        Inc(Count);
        
  SetLength(Result, Count);
  Count := 0;
  for i := 0 to High(FProjects) do
    for j := 0 to High(ProjectIDs) do
      if FProjects[i].ID = ProjectIDs[j] then
      begin
        Result[Count] := FProjects[i];
        Inc(Count);
      end;
end;

function TRecurringTaskManager.GetProjectTaskCount(AProjectID: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FTaskProjectLinks) do
    if FTaskProjectLinks[i].ProjectID = AProjectID then
      Inc(Result);
end;

function TRecurringTaskManager.GetPortfolioSummary: string;
var
  i: Integer;
  TotalBudget, TotalSpent: Double;
  ActiveCount: Integer;
begin
  TotalBudget := 0;
  TotalSpent := 0;
  ActiveCount := 0;
  
  for i := 0 to High(FProjects) do
  begin
    TotalBudget := TotalBudget + FProjects[i].Budget;
    TotalSpent := TotalSpent + FProjects[i].SpentAmount;
    if FProjects[i].IsActive then
      Inc(ActiveCount);
  end;
  
  Result := Format('Portfolio Summary:' + LineEnding +
    'Total Projects: %d' + LineEnding +
    'Active Projects: %d' + LineEnding +
    'Total Budget: $%.2f' + LineEnding +
    'Total Spent: $%.2f' + LineEnding +
    'Budget Utilization: %.1f%%',
    [Length(FProjects), ActiveCount, TotalBudget, TotalSpent,
     (TotalSpent / TotalBudget * 100)]);
end;

function TRecurringTaskManager.GetProjectHealth(AProjectID: Integer): string;
var
  Project: TProject;
  BudgetHealth, ScheduleHealth, OverallHealth: string;
begin
  Project := GetProject(AProjectID);
  if Project.ID < 0 then
    Exit('Project not found');
    
  if Project.SpentAmount > Project.Budget then
    BudgetHealth := 'Over Budget'
  else if Project.SpentAmount > Project.Budget * 0.9 then
    BudgetHealth := 'At Risk'
  else
    BudgetHealth := 'Healthy';
    
  if Now > Project.EndDate then
    ScheduleHealth := 'Delayed'
  else if Now > Project.EndDate - 7 then
    ScheduleHealth := 'At Risk'
  else
    ScheduleHealth := 'On Track';
    
  if (BudgetHealth = 'Healthy') and (ScheduleHealth = 'On Track') then
    OverallHealth := 'Healthy'
  else if (BudgetHealth = 'Over Budget') or (ScheduleHealth = 'Delayed') then
    OverallHealth := 'Critical'
  else
    OverallHealth := 'At Risk';
    
  Result := Format('Project: %s' + LineEnding +
    'Overall Health: %s' + LineEnding +
    'Budget Status: %s' + LineEnding +
    'Schedule Status: %s' + LineEnding +
    'Completion: %.1f%%',
    [Project.Name, OverallHealth, BudgetHealth, ScheduleHealth,
     Project.CompletionPercentage]);
end;

function TRecurringTaskManager.GetProjectProgress(AProjectID: Integer): Double;
var
  Project: TProject;
begin
  Project := GetProject(AProjectID);
  if Project.ID >= 0 then
    Result := Project.CompletionPercentage
  else
    Result := 0;
end;

function TRecurringTaskManager.GetOverBudgetProjects: TProjectArray;
var
  i, Count: Integer;
begin
  Count := 0;
  for i := 0 to High(FProjects) do
    if FProjects[i].SpentAmount > FProjects[i].Budget then
      Inc(Count);
      
  SetLength(Result, Count);
  Count := 0;
  for i := 0 to High(FProjects) do
    if FProjects[i].SpentAmount > FProjects[i].Budget then
    begin
      Result[Count] := FProjects[i];
      Inc(Count);
    end;
end;

function TRecurringTaskManager.GetDelayedProjects: TProjectArray;
var
  i, Count: Integer;
begin
  Count := 0;
  for i := 0 to High(FProjects) do
    if (FProjects[i].IsActive) and (Now > FProjects[i].EndDate) and
       (FProjects[i].CompletionPercentage < 100) then
      Inc(Count);
      
  SetLength(Result, Count);
  Count := 0;
  for i := 0 to High(FProjects) do
    if (FProjects[i].IsActive) and (Now > FProjects[i].EndDate) and
       (FProjects[i].CompletionPercentage < 100) then
    begin
      Result[Count] := FProjects[i];
      Inc(Count);
    end;
end;

function TRecurringTaskManager.GetPortfolioValue: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FProjects) do
    Result := Result + FProjects[i].Budget;
end;

function TRecurringTaskManager.RecurrenceTypeToString(AType: TRecurrenceType): string;
begin
  case AType of
    rtDaily: Result := 'Daily';
    rtWeekly: Result := 'Weekly';
    rtMonthly: Result := 'Monthly';
    rtYearly: Result := 'Yearly';
    rtCustom: Result := 'Custom';
  else
    Result := 'Unknown';
  end;
end;

function TRecurringTaskManager.ProjectStatusToString(AStatus: TProjectStatus): string;
begin
  case AStatus of
    psPlanning: Result := 'Planning';
    psActive: Result := 'Active';
    psOnHold: Result := 'On Hold';
    psClosed: Result := 'Closed';
    psCancelled: Result := 'Cancelled';
  else
    Result := 'Unknown';
  end;
end;

function TRecurringTaskManager.GetRecurringTasksReport: string;
var
  i: Integer;
  RecTask: TRecurringTask;
  Pattern: TRecurrencePattern;
begin
  Result := Format('Recurring Tasks Report' + LineEnding +
    'Total Recurring Tasks: %d' + LineEnding + LineEnding,
    [Length(FRecurringTasks)]);
    
  for i := 0 to High(FRecurringTasks) do
  begin
    RecTask := FRecurringTasks[i];
    Pattern := GetRecurrencePattern(RecTask.PatternID);
    Result := Result + Format('ID: %d | Template Task: %d | Pattern: %s | Active: %s' + LineEnding,
      [RecTask.ID, RecTask.TemplateTaskID, RecurrenceTypeToString(Pattern.RecurrenceType),
       BoolToStr(RecTask.IsActive, True)]);
  end;
end;

function TRecurringTaskManager.GetProjectsReport: string;
var
  i: Integer;
  Project: TProject;
begin
  Result := Format('Projects Report' + LineEnding +
    'Total Projects: %d' + LineEnding + LineEnding,
    [Length(FProjects)]);
    
  for i := 0 to High(FProjects) do
  begin
    Project := FProjects[i];
    Result := Result + Format('ID: %d | Name: %s | Status: %s | Progress: %.1f%% | Budget: $%.2f / $%.2f' + LineEnding,
      [Project.ID, Project.Name, ProjectStatusToString(Project.Status),
       Project.CompletionPercentage, Project.SpentAmount, Project.Budget]);
  end;
end;

function TRecurringTaskManager.SaveRecurringDataToFile(const AFilename: string): Boolean;
begin
  // Simplified - would save recurring task and project data
  Result := True;
end;

function TRecurringTaskManager.LoadRecurringDataFromFile(const AFilename: string): Boolean;
begin
  // Simplified - would load recurring task and project data
  Result := True;
end;

end.
