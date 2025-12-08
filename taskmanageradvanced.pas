
unit taskmanageradvanced;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Classes, Math, taskmanager, taskmanagerext;

type
  // Work session for Pomodoro-style time tracking
  TWorkSession = record
    SessionID: Integer;
    TaskID: Integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
    DurationMinutes: Integer;
    Notes: string;
    WasCompleted: Boolean; // Did the session complete or was it interrupted?
  end;
  TWorkSessionArray = array of TWorkSession;
  
  // Task note/comment with timestamp
  TTaskNote = record
    NoteID: Integer;
    TaskID: Integer;
    Timestamp: TDateTime;
    Author: string;
    Content: string;
    NoteType: string; // 'comment', 'update', 'decision', 'blocker'
  end;
  TTaskNoteArray = array of TTaskNote;
  
  // Task dependency relationship
  TDependencyType = (dtFinishToStart, dtStartToStart, dtFinishToFinish, dtStartToFinish);
  
  TTaskDependency = record
    DependencyID: Integer;
    TaskID: Integer;        // The dependent task
    DependsOnTaskID: Integer; // The task it depends on
    DependencyType: TDependencyType;
    LagDays: Integer;      // Lag time in days (can be negative for lead time)
  end;
  TTaskDependencyArray = array of TTaskDependency;
  
  // Task template for reusable workflows
  TTaskTemplate = record
    TemplateID: Integer;
    Name: string;
    Description: string;
    Category: string;
    DefaultPriority: TTaskPriority;
    EstimatedHours: Double;
    Tags: array of string;
    Checklist: array of string;
    DefaultDueDays: Integer; // Days from creation
  end;
  TTaskTemplateArray = array of TTaskTemplate;

  // Advanced task manager with dependencies, sessions, notes, templates
  TAdvancedTaskManager = class(TExtendedTaskManager)
  private
    FWorkSessions: TWorkSessionArray;
    FTaskNotes: TTaskNoteArray;
    FDependencies: TTaskDependencyArray;
    FTemplates: TTaskTemplateArray;
    FNextSessionID: Integer;
    FNextNoteID: Integer;
    FNextDependencyID: Integer;
    FNextTemplateID: Integer;
    
    function GetSessionsForTask(ATaskID: Integer): TWorkSessionArray;
    function GetNotesForTask(ATaskID: Integer): TTaskNoteArray;
    function GetTaskDependencies(ATaskID: Integer): TTaskDependencyArray;
    function CanCompleteTask(ATaskID: Integer): Boolean;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Work session management
    function StartWorkSession(ATaskID: Integer; const ANotes: string): Integer;
    function EndWorkSession(ASessionID: Integer; AWasCompleted: Boolean): Boolean;
    function GetActiveSession: Integer; // Returns active session ID or -1
    function GetSessionsByTask(ATaskID: Integer): TWorkSessionArray;
    function GetAllWorkSessions: TWorkSessionArray;
    function GetTotalWorkTime(ATaskID: Integer): Integer; // Minutes
    function GetSessionStats: string;
    
    // Task notes/comments
    function AddNote(ATaskID: Integer; const AAuthor, AContent, ANoteType: string): Integer;
    function GetNotes(ATaskID: Integer): TTaskNoteArray;
    function GetAllNotes: TTaskNoteArray;
    function DeleteNote(ANoteID: Integer): Boolean;
    function NotesToString(ATaskID: Integer): string;
    
    // Task dependencies
    function AddDependency(ATaskID, ADependsOnTaskID: Integer; 
                          AType: TDependencyType; ALagDays: Integer): Integer;
    function RemoveDependency(ADependencyID: Integer): Boolean;
    function GetDependenciesFor(ATaskID: Integer): TTaskDependencyArray;
    function GetBlockedBy(ATaskID: Integer): TTaskDependencyArray;
    function GetBlocking(ATaskID: Integer): TTaskDependencyArray;
    function ValidateTaskCompletion(ATaskID: Integer): Boolean;
    function GetDependencyChain(ATaskID: Integer): string;
    function DetectCircularDependencies: string;
    
    // Task templates
    function CreateTemplate(const AName, ADescription, ACategory: string;
                           APriority: TTaskPriority; AEstimatedHours: Double;
                           ADefaultDueDays: Integer): Integer;
    function CreateTaskFromTemplate(ATemplateID: Integer; const ATitle: string;
                                    ADueDate: TDateTime): Integer;
    function GetAllTemplates: TTaskTemplateArray;
    function DeleteTemplate(ATemplateID: Integer): Boolean;
    function AddChecklistToTemplate(ATemplateID: Integer; const AItem: string): Boolean;
    function TemplateToString(const ATemplate: TTaskTemplate): string;
    
    // Advanced analytics
    function GetProductivityByTimeOfDay: string;
    function GetAverageSessionDuration: Double;
    function GetMostProductiveDays: string;
    function GetTaskCompletionForecast(ATaskID: Integer): string;
    
    // File operations
    function SaveAdvancedToFile(const AFilename: string): Boolean;
    function LoadAdvancedFromFile(const AFilename: string): Boolean;
    function ExportAdvancedToCSV: string;
  end;

implementation

constructor TAdvancedTaskManager.Create;
begin
  inherited Create;
  SetLength(FWorkSessions, 0);
  SetLength(FTaskNotes, 0);
  SetLength(FDependencies, 0);
  SetLength(FTemplates, 0);
  FNextSessionID := 1;
  FNextNoteID := 1;
  FNextDependencyID := 1;
  FNextTemplateID := 1;
end;

destructor TAdvancedTaskManager.Destroy;
begin
  SetLength(FWorkSessions, 0);
  SetLength(FTaskNotes, 0);
  SetLength(FDependencies, 0);
  SetLength(FTemplates, 0);
  inherited Destroy;
end;

// Work Session Management
function TAdvancedTaskManager.StartWorkSession(ATaskID: Integer; const ANotes: string): Integer;
var
  Session: TWorkSession;
  ActiveID: Integer;
begin
  Result := -1;
  
  // Check if there's already an active session
  ActiveID := GetActiveSession;
  if ActiveID <> -1 then
    Exit; // Can't start a new session while one is active
    
  // Verify task exists
  if GetTaskByID(ATaskID) = -1 then
    Exit;
    
  Session.SessionID := FNextSessionID;
  Inc(FNextSessionID);
  Session.TaskID := ATaskID;
  Session.StartTime := Now;
  Session.EndTime := 0;
  Session.DurationMinutes := 0;
  Session.Notes := ANotes;
  Session.WasCompleted := False;
  
  SetLength(FWorkSessions, Length(FWorkSessions) + 1);
  FWorkSessions[High(FWorkSessions)] := Session;
  
  Result := Session.SessionID;
end;

function TAdvancedTaskManager.EndWorkSession(ASessionID: Integer; AWasCompleted: Boolean): Boolean;
var
  i: Integer;
  Duration: TDateTime;
begin
  Result := False;
  for i := 0 to High(FWorkSessions) do
  begin
    if FWorkSessions[i].SessionID = ASessionID then
    begin
      if FWorkSessions[i].EndTime = 0 then // Session is still active
      begin
        FWorkSessions[i].EndTime := Now;
        Duration := FWorkSessions[i].EndTime - FWorkSessions[i].StartTime;
        FWorkSessions[i].DurationMinutes := Round(Duration * 24 * 60);
        FWorkSessions[i].WasCompleted := AWasCompleted;
        Result := True;
      end;
      Exit;
    end;
  end;
end;

function TAdvancedTaskManager.GetActiveSession: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FWorkSessions) do
  begin
    if FWorkSessions[i].EndTime = 0 then
    begin
      Result := FWorkSessions[i].SessionID;
      Exit;
    end;
  end;
end;

function TAdvancedTaskManager.GetSessionsForTask(ATaskID: Integer): TWorkSessionArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FWorkSessions) do
  begin
    if FWorkSessions[i].TaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FWorkSessions[i];
      Inc(Count);
    end;
  end;
end;

function TAdvancedTaskManager.GetSessionsByTask(ATaskID: Integer): TWorkSessionArray;
begin
  Result := GetSessionsForTask(ATaskID);
end;

function TAdvancedTaskManager.GetAllWorkSessions: TWorkSessionArray;
begin
  Result := FWorkSessions;
end;

function TAdvancedTaskManager.GetTotalWorkTime(ATaskID: Integer): Integer;
var
  Sessions: TWorkSessionArray;
  i: Integer;
begin
  Result := 0;
  Sessions := GetSessionsForTask(ATaskID);
  for i := 0 to High(Sessions) do
  begin
    if Sessions[i].WasCompleted then
      Result := Result + Sessions[i].DurationMinutes;
  end;
end;

function TAdvancedTaskManager.GetSessionStats: string;
var
  TotalSessions, CompletedSessions, i: Integer;
  TotalMinutes: Integer;
begin
  TotalSessions := Length(FWorkSessions);
  CompletedSessions := 0;
  TotalMinutes := 0;
  
  for i := 0 to High(FWorkSessions) do
  begin
    if FWorkSessions[i].EndTime > 0 then
    begin
      Inc(CompletedSessions);
      if FWorkSessions[i].WasCompleted then
        TotalMinutes := TotalMinutes + FWorkSessions[i].DurationMinutes;
    end;
  end;
  
  Result := Format('Total Sessions: %d' + LineEnding +
                   'Completed Sessions: %d' + LineEnding +
                   'Total Work Time: %.1f hours' + LineEnding +
                   'Average Session: %.1f minutes',
                   [TotalSessions, CompletedSessions, 
                    TotalMinutes / 60.0,
                    TotalMinutes / Max(1, CompletedSessions)]);
end;

// Task Notes Management
function TAdvancedTaskManager.AddNote(ATaskID: Integer; const AAuthor, AContent, ANoteType: string): Integer;
var
  Note: TTaskNote;
begin
  Result := -1;
  
  // Verify task exists
  if GetTaskByID(ATaskID) = -1 then
    Exit;
    
  Note.NoteID := FNextNoteID;
  Inc(FNextNoteID);
  Note.TaskID := ATaskID;
  Note.Timestamp := Now;
  Note.Author := AAuthor;
  Note.Content := AContent;
  Note.NoteType := ANoteType;
  
  SetLength(FTaskNotes, Length(FTaskNotes) + 1);
  FTaskNotes[High(FTaskNotes)] := Note;
  
  Result := Note.NoteID;
end;

function TAdvancedTaskManager.GetNotesForTask(ATaskID: Integer): TTaskNoteArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FTaskNotes) do
  begin
    if FTaskNotes[i].TaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTaskNotes[i];
      Inc(Count);
    end;
  end;
end;

function TAdvancedTaskManager.GetNotes(ATaskID: Integer): TTaskNoteArray;
begin
  Result := GetNotesForTask(ATaskID);
end;

function TAdvancedTaskManager.GetAllNotes: TTaskNoteArray;
begin
  Result := FTaskNotes;
end;

function TAdvancedTaskManager.DeleteNote(ANoteID: Integer): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to High(FTaskNotes) do
  begin
    if FTaskNotes[i].NoteID = ANoteID then
    begin
      for j := i to High(FTaskNotes) - 1 do
        FTaskNotes[j] := FTaskNotes[j + 1];
      SetLength(FTaskNotes, Length(FTaskNotes) - 1);
      Result := True;
      Exit;
    end;
  end;
end;

function TAdvancedTaskManager.NotesToString(ATaskID: Integer): string;
var
  Notes: TTaskNoteArray;
  i: Integer;
begin
  Result := '';
  Notes := GetNotesForTask(ATaskID);
  for i := 0 to High(Notes) do
  begin
    Result := Result + Format('[%s] %s (%s): %s',
                             [DateTimeToStr(Notes[i].Timestamp),
                              Notes[i].Author,
                              Notes[i].NoteType,
                              Notes[i].Content]) + LineEnding;
  end;
end;

// Task Dependencies
function TAdvancedTaskManager.AddDependency(ATaskID, ADependsOnTaskID: Integer;
                                            AType: TDependencyType; ALagDays: Integer): Integer;
var
  Dep: TTaskDependency;
begin
  Result := -1;
  
  // Verify both tasks exist
  if (GetTaskByID(ATaskID) = -1) or (GetTaskByID(ADependsOnTaskID) = -1) then
    Exit;
    
  // Don't allow self-dependency
  if ATaskID = ADependsOnTaskID then
    Exit;
    
  Dep.DependencyID := FNextDependencyID;
  Inc(FNextDependencyID);
  Dep.TaskID := ATaskID;
  Dep.DependsOnTaskID := ADependsOnTaskID;
  Dep.DependencyType := AType;
  Dep.LagDays := ALagDays;
  
  SetLength(FDependencies, Length(FDependencies) + 1);
  FDependencies[High(FDependencies)] := Dep;
  
  Result := Dep.DependencyID;
end;

function TAdvancedTaskManager.RemoveDependency(ADependencyID: Integer): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to High(FDependencies) do
  begin
    if FDependencies[i].DependencyID = ADependencyID then
    begin
      for j := i to High(FDependencies) - 1 do
        FDependencies[j] := FDependencies[j + 1];
      SetLength(FDependencies, Length(FDependencies) - 1);
      Result := True;
      Exit;
    end;
  end;
end;

function TAdvancedTaskManager.GetTaskDependencies(ATaskID: Integer): TTaskDependencyArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FDependencies) do
  begin
    if FDependencies[i].TaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FDependencies[i];
      Inc(Count);
    end;
  end;
end;

function TAdvancedTaskManager.GetDependenciesFor(ATaskID: Integer): TTaskDependencyArray;
begin
  Result := GetTaskDependencies(ATaskID);
end;

function TAdvancedTaskManager.GetBlockedBy(ATaskID: Integer): TTaskDependencyArray;
begin
  Result := GetTaskDependencies(ATaskID);
end;

function TAdvancedTaskManager.GetBlocking(ATaskID: Integer): TTaskDependencyArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to High(FDependencies) do
  begin
    if FDependencies[i].DependsOnTaskID = ATaskID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FDependencies[i];
      Inc(Count);
    end;
  end;
end;

function TAdvancedTaskManager.CanCompleteTask(ATaskID: Integer): Boolean;
var
  Deps: TTaskDependencyArray;
  i, Idx: Integer;
  Tasks: TExtendedTaskArray;
begin
  Result := True;
  Deps := GetTaskDependencies(ATaskID);
  Tasks := GetAllExtendedTasks;
  
  for i := 0 to High(Deps) do
  begin
    Idx := GetTaskByID(Deps[i].DependsOnTaskID);
    if Idx <> -1 then
    begin
      if Tasks[Idx].BaseTask.Status <> tsCompleted then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TAdvancedTaskManager.ValidateTaskCompletion(ATaskID: Integer): Boolean;
begin
  Result := CanCompleteTask(ATaskID);
end;

function TAdvancedTaskManager.GetDependencyChain(ATaskID: Integer): string;
var
  Deps: TTaskDependencyArray;
  i: Integer;
  Tasks: TExtendedTaskArray;
  Idx: Integer;
begin
  Result := '';
  Deps := GetTaskDependencies(ATaskID);
  Tasks := GetAllExtendedTasks;
  
  if Length(Deps) = 0 then
  begin
    Result := 'No dependencies';
    Exit;
  end;
  
  Result := 'Dependencies:' + LineEnding;
  for i := 0 to High(Deps) do
  begin
    Idx := GetTaskByID(Deps[i].DependsOnTaskID);
    if Idx <> -1 then
    begin
      Result := Result + Format('  - Task #%d: %s [%s]',
                               [Tasks[Idx].BaseTask.ID,
                                Tasks[Idx].BaseTask.Title,
                                TaskStatusToString(Tasks[Idx].BaseTask.Status)]) + LineEnding;
    end;
  end;
end;

function TAdvancedTaskManager.DetectCircularDependencies: string;
begin
  Result := 'Circular dependency detection not yet implemented';
end;

// Task Templates
function TAdvancedTaskManager.CreateTemplate(const AName, ADescription, ACategory: string;
                                             APriority: TTaskPriority; AEstimatedHours: Double;
                                             ADefaultDueDays: Integer): Integer;
var
  Template: TTaskTemplate;
begin
  Template.TemplateID := FNextTemplateID;
  Inc(FNextTemplateID);
  Template.Name := AName;
  Template.Description := ADescription;
  Template.Category := ACategory;
  Template.DefaultPriority := APriority;
  Template.EstimatedHours := AEstimatedHours;
  Template.DefaultDueDays := ADefaultDueDays;
  SetLength(Template.Tags, 0);
  SetLength(Template.Checklist, 0);
  
  SetLength(FTemplates, Length(FTemplates) + 1);
  FTemplates[High(FTemplates)] := Template;
  
  Result := Template.TemplateID;
end;

function TAdvancedTaskManager.CreateTaskFromTemplate(ATemplateID: Integer; const ATitle: string;
                                                     ADueDate: TDateTime): Integer;
var
  i: Integer;
  Template: TTaskTemplate;
  Found: Boolean;
begin
  Result := -1;
  Found := False;
  
  for i := 0 to High(FTemplates) do
  begin
    if FTemplates[i].TemplateID = ATemplateID then
    begin
      Template := FTemplates[i];
      Found := True;
      Break;
    end;
  end;
  
  if not Found then
    Exit;
    
  Result := AddExtendedTask(ATitle, Template.Description, Template.Category,
                           Template.DefaultPriority, ADueDate,
                           Template.EstimatedHours, rpNone);
end;

function TAdvancedTaskManager.GetAllTemplates: TTaskTemplateArray;
begin
  Result := FTemplates;
end;

function TAdvancedTaskManager.DeleteTemplate(ATemplateID: Integer): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to High(FTemplates) do
  begin
    if FTemplates[i].TemplateID = ATemplateID then
    begin
      for j := i to High(FTemplates) - 1 do
        FTemplates[j] := FTemplates[j + 1];
      SetLength(FTemplates, Length(FTemplates) - 1);
      Result := True;
      Exit;
    end;
  end;
end;

function TAdvancedTaskManager.AddChecklistToTemplate(ATemplateID: Integer; const AItem: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FTemplates) do
  begin
    if FTemplates[i].TemplateID = ATemplateID then
    begin
      SetLength(FTemplates[i].Checklist, Length(FTemplates[i].Checklist) + 1);
      FTemplates[i].Checklist[High(FTemplates[i].Checklist)] := AItem;
      Result := True;
      Exit;
    end;
  end;
end;

function TAdvancedTaskManager.TemplateToString(const ATemplate: TTaskTemplate): string;
begin
  Result := Format('Template #%d: %s' + LineEnding +
                   '  Description: %s' + LineEnding +
                   '  Category: %s' + LineEnding +
                   '  Priority: %s' + LineEnding +
                   '  Estimated: %.1f hours' + LineEnding +
                   '  Default due: %d days',
                   [ATemplate.TemplateID, ATemplate.Name,
                    ATemplate.Description, ATemplate.Category,
                    TaskPriorityToString(ATemplate.DefaultPriority),
                    ATemplate.EstimatedHours, ATemplate.DefaultDueDays]);
end;

// Advanced Analytics
function TAdvancedTaskManager.GetProductivityByTimeOfDay: string;
begin
  Result := 'Time-of-day productivity analysis not yet implemented';
end;

function TAdvancedTaskManager.GetAverageSessionDuration: Double;
var
  i, Count: Integer;
  Total: Integer;
begin
  Result := 0;
  Total := 0;
  Count := 0;
  
  for i := 0 to High(FWorkSessions) do
  begin
    if FWorkSessions[i].WasCompleted then
    begin
      Total := Total + FWorkSessions[i].DurationMinutes;
      Inc(Count);
    end;
  end;
  
  if Count > 0 then
    Result := Total / Count;
end;

function TAdvancedTaskManager.GetMostProductiveDays: string;
begin
  Result := 'Most productive days analysis not yet implemented';
end;

function TAdvancedTaskManager.GetTaskCompletionForecast(ATaskID: Integer): string;
begin
  Result := 'Task completion forecast not yet implemented';
end;

// File Operations
function TAdvancedTaskManager.SaveAdvancedToFile(const AFilename: string): Boolean;
begin
  Result := SaveExtendedToFile(AFilename);
  // TODO: Add saving of sessions, notes, dependencies, templates
end;

function TAdvancedTaskManager.LoadAdvancedFromFile(const AFilename: string): Boolean;
begin
  Result := LoadExtendedFromFile(AFilename);
  // TODO: Add loading of sessions, notes, dependencies, templates
end;

function TAdvancedTaskManager.ExportAdvancedToCSV: string;
begin
  Result := ExportExtendedToCSV;
  // TODO: Add CSV export of sessions, notes, dependencies
end;

end.
