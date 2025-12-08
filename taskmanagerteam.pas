
unit taskmanagerteam;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math, Classes,
  taskmanager, taskmanagerext, taskmanageradvanced, taskmanagerenhanced;

type
  // Team member structure
  TTeamMember = record
    MemberID: Integer;
    Name: string;
    Email: string;
    Role: string;
    MaxTasksActive: Integer;
    CurrentTaskCount: Integer;
    AvailableHoursPerWeek: Double;
    Skills: array of string;
    JoinedDate: TDateTime;
    IsActive: Boolean;
  end;
  TTeamMemberArray = array of TTeamMember;

  // Task assignment
  TTaskAssignment = record
    AssignmentID: Integer;
    TaskID: Integer;
    MemberID: Integer;
    AssignedDate: TDateTime;
    AssignedBy: string;
    PercentageOwnership: Integer; // For shared tasks
    Notes: string;
  end;
  TTaskAssignmentArray = array of TTaskAssignment;

  // Custom field definition
  TCustomFieldType = (cftString, cftInteger, cftFloat, cftDate, cftBoolean, cftList);
  
  TCustomFieldDef = record
    FieldID: Integer;
    FieldName: string;
    FieldType: TCustomFieldType;
    DefaultValue: string;
    Required: Boolean;
    ListOptions: array of string; // For cftList type
  end;
  TCustomFieldDefArray = array of TCustomFieldDef;

  // Custom field value
  TCustomFieldValue = record
    ValueID: Integer;
    TaskID: Integer;
    FieldID: Integer;
    Value: string;
  end;
  TCustomFieldValueArray = array of TCustomFieldValue;

  // Scheduled time slot
  TTimeSlot = record
    SlotID: Integer;
    TaskID: Integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
    Duration: Integer; // minutes
    IsConfirmed: Boolean;
    Notes: string;
  end;
  TTimeSlotArray = array of TTimeSlot;

  // Conflict detection result
  TConflictType = (ctOverlappingSchedule, ctOverdueDependency, ctResourceOverload, 
                   ctMissingDependency, ctCircularDependency);
  
  TConflict = record
    ConflictID: Integer;
    ConflictType: TConflictType;
    TaskID: Integer;
    RelatedTaskID: Integer;
    Severity: Integer; // 1-5
    Description: string;
    DetectedDate: TDateTime;
  end;
  TConflictArray = array of TConflict;

  // Team Task Manager
  TTeamTaskManager = class(TEnhancedTaskManager)
  private
    FTeamMembers: TTeamMemberArray;
    FAssignments: TTaskAssignmentArray;
    FCustomFieldDefs: TCustomFieldDefArray;
    FCustomFieldValues: TCustomFieldValueArray;
    FTimeSlots: TTimeSlotArray;
    FConflicts: TConflictArray;
    FNextMemberID: Integer;
    FNextAssignmentID: Integer;
    FNextFieldDefID: Integer;
    FNextFieldValueID: Integer;
    FNextSlotID: Integer;
    FNextConflictID: Integer;
    
    function FindMemberIndex(AMemberID: Integer): Integer;
    function FindAssignmentIndex(AAssignmentID: Integer): Integer;
    function FindFieldDefIndex(AFieldID: Integer): Integer;
    function FindTimeSlotIndex(ASlotID: Integer): Integer;
    procedure UpdateMemberTaskCount(AMemberID: Integer);
    function CalculateMemberWorkload(AMemberID: Integer): Double;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Team member management
    function AddTeamMember(const AName, AEmail, ARole: string;
      AMaxTasks: Integer; AHoursPerWeek: Double): Integer;
    function UpdateTeamMember(AMemberID: Integer; const AName, AEmail, ARole: string;
      AMaxTasks: Integer; AHoursPerWeek: Double): Boolean;
    function DeactivateTeamMember(AMemberID: Integer): Boolean;
    function GetTeamMember(AMemberID: Integer): TTeamMember;
    function GetAllTeamMembers: TTeamMemberArray;
    function GetActiveTeamMembers: TTeamMemberArray;
    function AddSkillToMember(AMemberID: Integer; const ASkill: string): Boolean;
    function GetMembersBySkill(const ASkill: string): TTeamMemberArray;
    
    // Task assignment
    function AssignTask(ATaskID, AMemberID: Integer; APercentage: Integer;
      const ANotes: string): Integer;
    function UnassignTask(AAssignmentID: Integer): Boolean;
    function ReassignTask(AAssignmentID, ANewMemberID: Integer): Boolean;
    function GetTaskAssignments(ATaskID: Integer): TTaskAssignmentArray;
    function GetMemberAssignments(AMemberID: Integer): TTaskAssignmentArray;
    function GetUnassignedTasks: TTaskArray;
    
    // Smart task assignment
    function AutoAssignTask(ATaskID: Integer): Integer;
    function SuggestBestMember(ATaskID: Integer): Integer;
    function GetMemberWorkload: string;
    function BalanceWorkload: Integer;
    
    // Custom fields
    function DefineCustomField(const AName: string; AType: TCustomFieldType;
      const ADefaultValue: string; ARequired: Boolean): Integer;
    function AddListOption(AFieldID: Integer; const AOption: string): Boolean;
    function SetCustomFieldValue(ATaskID, AFieldID: Integer; const AValue: string): Integer;
    function GetCustomFieldValue(ATaskID, AFieldID: Integer): string;
    function GetAllCustomFields: TCustomFieldDefArray;
    function GetTaskCustomValues(ATaskID: Integer): TCustomFieldValueArray;
    
    // Time scheduling
    function ScheduleTask(ATaskID: Integer; AStartTime: TDateTime; 
      ADurationMinutes: Integer; const ANotes: string): Integer;
    function RescheduleTask(ASlotID: Integer; ANewStart: TDateTime): Boolean;
    function GetTaskSchedule(ATaskID: Integer): TTimeSlotArray;
    function GetScheduleForPeriod(AStart, AEnd: TDateTime): TTimeSlotArray;
    function FindAvailableSlot(ADurationMinutes: Integer; 
      APreferredStart: TDateTime): TDateTime;
    function AutoScheduleTasks(APrioritizeBy: string): Integer;
    
    // Conflict detection
    function DetectConflicts: TConflictArray;
    function DetectScheduleConflicts: Integer;
    function DetectDependencyConflicts: Integer;
    function DetectResourceConflicts: Integer;
    function GetActiveConflicts: TConflictArray;
    function ResolveConflict(AConflictID: Integer; const AResolution: string): Boolean;
    
    // Advanced analytics
    function GetTeamProductivity: string;
    function GetMemberPerformance(AMemberID: Integer): string;
    function GetTeamCapacity: string;
    function GetBottlenecks: string;
    function GetTaskDistribution: string;
    function PredictCompletionDate(ATaskID: Integer): TDateTime;
    
    // Import/Export
    function ImportFromCSV(const AFilename: string): Integer;
    function ImportFromJSON(const AJSONString: string): Integer;
    function ExportToJSON: string;
    function ExportToMarkdown: string;
    
    // Utility functions
    function ConflictTypeToString(AType: TConflictType): string;
    function CustomFieldTypeToString(AType: TCustomFieldType): string;
    function TeamMemberToString(const AMember: TTeamMember): string;
    
    // Persistence
    function SaveTeamDataToFile(const AFilename: string): Boolean;
    function LoadTeamDataFromFile(const AFilename: string): Boolean;
  end;

implementation

{ TTeamTaskManager }

constructor TTeamTaskManager.Create;
begin
  inherited Create;
  SetLength(FTeamMembers, 0);
  SetLength(FAssignments, 0);
  SetLength(FCustomFieldDefs, 0);
  SetLength(FCustomFieldValues, 0);
  SetLength(FTimeSlots, 0);
  SetLength(FConflicts, 0);
  FNextMemberID := 1;
  FNextAssignmentID := 1;
  FNextFieldDefID := 1;
  FNextFieldValueID := 1;
  FNextSlotID := 1;
  FNextConflictID := 1;
end;

destructor TTeamTaskManager.Destroy;
begin
  SetLength(FTeamMembers, 0);
  SetLength(FAssignments, 0);
  SetLength(FCustomFieldDefs, 0);
  SetLength(FCustomFieldValues, 0);
  SetLength(FTimeSlots, 0);
  SetLength(FConflicts, 0);
  inherited Destroy;
end;

function TTeamTaskManager.FindMemberIndex(AMemberID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTeamMembers) do
    if FTeamMembers[i].MemberID = AMemberID then
    begin
      Result := i;
      Exit;
    end;
end;

function TTeamTaskManager.FindAssignmentIndex(AAssignmentID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FAssignments) do
    if FAssignments[i].AssignmentID = AAssignmentID then
    begin
      Result := i;
      Exit;
    end;
end;

function TTeamTaskManager.FindFieldDefIndex(AFieldID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FCustomFieldDefs) do
    if FCustomFieldDefs[i].FieldID = AFieldID then
    begin
      Result := i;
      Exit;
    end;
end;

function TTeamTaskManager.FindTimeSlotIndex(ASlotID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTimeSlots) do
    if FTimeSlots[i].SlotID = ASlotID then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TTeamTaskManager.UpdateMemberTaskCount(AMemberID: Integer);
var
  i, idx, count: Integer;
begin
  idx := FindMemberIndex(AMemberID);
  if idx = -1 then
    Exit;
    
  count := 0;
  for i := 0 to High(FAssignments) do
    if FAssignments[i].MemberID = AMemberID then
      Inc(count);
      
  FTeamMembers[idx].CurrentTaskCount := count;
end;

function TTeamTaskManager.CalculateMemberWorkload(AMemberID: Integer): Double;
var
  i, taskIdx: Integer;
  totalHours: Double;
  tasks: TTaskArray;
begin
  Result := 0.0;
  totalHours := 0.0;
  
  for i := 0 to High(FAssignments) do
  begin
    if FAssignments[i].MemberID = AMemberID then
    begin
      tasks := GetAllTasks;
      for taskIdx := 0 to High(tasks) do
      begin
        if tasks[taskIdx].ID = FAssignments[i].TaskID then
        begin
          if tasks[taskIdx].Status <> tsCompleted then
            totalHours := totalHours + tasks[taskIdx].EstimatedHours;
          Break;
        end;
      end;
    end;
  end;
  
  Result := totalHours;
end;

function TTeamTaskManager.AddTeamMember(const AName, AEmail, ARole: string;
  AMaxTasks: Integer; AHoursPerWeek: Double): Integer;
var
  idx: Integer;
begin
  idx := Length(FTeamMembers);
  SetLength(FTeamMembers, idx + 1);
  
  FTeamMembers[idx].MemberID := FNextMemberID;
  FTeamMembers[idx].Name := AName;
  FTeamMembers[idx].Email := AEmail;
  FTeamMembers[idx].Role := ARole;
  FTeamMembers[idx].MaxTasksActive := AMaxTasks;
  FTeamMembers[idx].CurrentTaskCount := 0;
  FTeamMembers[idx].AvailableHoursPerWeek := AHoursPerWeek;
  SetLength(FTeamMembers[idx].Skills, 0);
  FTeamMembers[idx].JoinedDate := Now;
  FTeamMembers[idx].IsActive := True;
  
  Result := FNextMemberID;
  Inc(FNextMemberID);
end;

function TTeamTaskManager.UpdateTeamMember(AMemberID: Integer; const AName, AEmail, ARole: string;
  AMaxTasks: Integer; AHoursPerWeek: Double): Boolean;
var
  idx: Integer;
begin
  idx := FindMemberIndex(AMemberID);
  Result := idx <> -1;
  
  if Result then
  begin
    FTeamMembers[idx].Name := AName;
    FTeamMembers[idx].Email := AEmail;
    FTeamMembers[idx].Role := ARole;
    FTeamMembers[idx].MaxTasksActive := AMaxTasks;
    FTeamMembers[idx].AvailableHoursPerWeek := AHoursPerWeek;
  end;
end;

function TTeamTaskManager.DeactivateTeamMember(AMemberID: Integer): Boolean;
var
  idx: Integer;
begin
  idx := FindMemberIndex(AMemberID);
  Result := idx <> -1;
  
  if Result then
    FTeamMembers[idx].IsActive := False;
end;

function TTeamTaskManager.GetTeamMember(AMemberID: Integer): TTeamMember;
var
  idx: Integer;
begin
  idx := FindMemberIndex(AMemberID);
  if idx <> -1 then
    Result := FTeamMembers[idx];
end;

function TTeamTaskManager.GetAllTeamMembers: TTeamMemberArray;
begin
  Result := Copy(FTeamMembers, 0, Length(FTeamMembers));
end;

function TTeamTaskManager.GetActiveTeamMembers: TTeamMemberArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FTeamMembers) do
    if FTeamMembers[i].IsActive then
      Inc(count);
      
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FTeamMembers) do
  begin
    if FTeamMembers[i].IsActive then
    begin
      Result[count] := FTeamMembers[i];
      Inc(count);
    end;
  end;
end;

function TTeamTaskManager.AddSkillToMember(AMemberID: Integer; const ASkill: string): Boolean;
var
  idx, skillIdx: Integer;
begin
  idx := FindMemberIndex(AMemberID);
  Result := idx <> -1;
  
  if Result then
  begin
    skillIdx := Length(FTeamMembers[idx].Skills);
    SetLength(FTeamMembers[idx].Skills, skillIdx + 1);
    FTeamMembers[idx].Skills[skillIdx] := ASkill;
  end;
end;

function TTeamTaskManager.GetMembersBySkill(const ASkill: string): TTeamMemberArray;
var
  i, j, count: Integer;
  hasSkill: Boolean;
begin
  count := 0;
  for i := 0 to High(FTeamMembers) do
  begin
    hasSkill := False;
    for j := 0 to High(FTeamMembers[i].Skills) do
    begin
      if AnsiLowerCase(FTeamMembers[i].Skills[j]) = AnsiLowerCase(ASkill) then
      begin
        hasSkill := True;
        Break;
      end;
    end;
    if hasSkill and FTeamMembers[i].IsActive then
      Inc(count);
  end;
  
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FTeamMembers) do
  begin
    hasSkill := False;
    for j := 0 to High(FTeamMembers[i].Skills) do
    begin
      if AnsiLowerCase(FTeamMembers[i].Skills[j]) = AnsiLowerCase(ASkill) then
      begin
        hasSkill := True;
        Break;
      end;
    end;
    if hasSkill and FTeamMembers[i].IsActive then
    begin
      Result[count] := FTeamMembers[i];
      Inc(count);
    end;
  end;
end;

function TTeamTaskManager.AssignTask(ATaskID, AMemberID: Integer; APercentage: Integer;
  const ANotes: string): Integer;
var
  idx, memberIdx: Integer;
begin
  Result := -1;
  memberIdx := FindMemberIndex(AMemberID);
  if memberIdx = -1 then
    Exit;
    
  idx := Length(FAssignments);
  SetLength(FAssignments, idx + 1);
  
  FAssignments[idx].AssignmentID := FNextAssignmentID;
  FAssignments[idx].TaskID := ATaskID;
  FAssignments[idx].MemberID := AMemberID;
  FAssignments[idx].AssignedDate := Now;
  FAssignments[idx].AssignedBy := GetCurrentUser;
  FAssignments[idx].PercentageOwnership := APercentage;
  FAssignments[idx].Notes := ANotes;
  
  UpdateMemberTaskCount(AMemberID);
  
  Result := FNextAssignmentID;
  Inc(FNextAssignmentID);
end;

function TTeamTaskManager.UnassignTask(AAssignmentID: Integer): Boolean;
var
  i, idx, memberID: Integer;
begin
  idx := FindAssignmentIndex(AAssignmentID);
  Result := idx <> -1;
  
  if Result then
  begin
    memberID := FAssignments[idx].MemberID;
    
    for i := idx to High(FAssignments) - 1 do
      FAssignments[i] := FAssignments[i + 1];
    SetLength(FAssignments, Length(FAssignments) - 1);
    
    UpdateMemberTaskCount(memberID);
  end;
end;

function TTeamTaskManager.ReassignTask(AAssignmentID, ANewMemberID: Integer): Boolean;
var
  idx, oldMemberID: Integer;
begin
  idx := FindAssignmentIndex(AAssignmentID);
  Result := (idx <> -1) and (FindMemberIndex(ANewMemberID) <> -1);
  
  if Result then
  begin
    oldMemberID := FAssignments[idx].MemberID;
    FAssignments[idx].MemberID := ANewMemberID;
    FAssignments[idx].AssignedDate := Now;
    FAssignments[idx].AssignedBy := GetCurrentUser;
    
    UpdateMemberTaskCount(oldMemberID);
    UpdateMemberTaskCount(ANewMemberID);
  end;
end;

function TTeamTaskManager.GetTaskAssignments(ATaskID: Integer): TTaskAssignmentArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FAssignments) do
    if FAssignments[i].TaskID = ATaskID then
      Inc(count);
      
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FAssignments) do
  begin
    if FAssignments[i].TaskID = ATaskID then
    begin
      Result[count] := FAssignments[i];
      Inc(count);
    end;
  end;
end;

function TTeamTaskManager.GetMemberAssignments(AMemberID: Integer): TTaskAssignmentArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FAssignments) do
    if FAssignments[i].MemberID = AMemberID then
      Inc(count);
      
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FAssignments) do
  begin
    if FAssignments[i].MemberID = AMemberID then
    begin
      Result[count] := FAssignments[i];
      Inc(count);
    end;
  end;
end;

function TTeamTaskManager.GetUnassignedTasks: TTaskArray;
var
  i, j, count: Integer;
  tasks: TTaskArray;
  isAssigned: Boolean;
begin
  tasks := GetAllTasks;
  count := 0;
  
  for i := 0 to High(tasks) do
  begin
    isAssigned := False;
    for j := 0 to High(FAssignments) do
    begin
      if FAssignments[j].TaskID = tasks[i].ID then
      begin
        isAssigned := True;
        Break;
      end;
    end;
    if not isAssigned then
      Inc(count);
  end;
  
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(tasks) do
  begin
    isAssigned := False;
    for j := 0 to High(FAssignments) do
    begin
      if FAssignments[j].TaskID = tasks[i].ID then
      begin
        isAssigned := True;
        Break;
      end;
    end;
    if not isAssigned then
    begin
      Result[count] := tasks[i];
      Inc(count);
    end;
  end;
end;

function TTeamTaskManager.AutoAssignTask(ATaskID: Integer): Integer;
var
  bestMemberID: Integer;
begin
  bestMemberID := SuggestBestMember(ATaskID);
  if bestMemberID <> -1 then
    Result := AssignTask(ATaskID, bestMemberID, 100, 'Auto-assigned')
  else
    Result := -1;
end;

function TTeamTaskManager.SuggestBestMember(ATaskID: Integer): Integer;
var
  i, idx: Integer;
  tasks: TTaskArray;
  minWorkload, workload: Double;
  bestMemberIdx: Integer;
begin
  Result := -1;
  
  tasks := GetAllTasks;
  idx := -1;
  for i := 0 to High(tasks) do
  begin
    if tasks[i].ID = ATaskID then
    begin
      idx := i;
      Break;
    end;
  end;
  
  if idx = -1 then
    Exit;
    
  minWorkload := MaxDouble;
  bestMemberIdx := -1;
  
  for i := 0 to High(FTeamMembers) do
  begin
    if FTeamMembers[i].IsActive and 
       (FTeamMembers[i].CurrentTaskCount < FTeamMembers[i].MaxTasksActive) then
    begin
      workload := CalculateMemberWorkload(FTeamMembers[i].MemberID);
      if workload < minWorkload then
      begin
        minWorkload := workload;
        bestMemberIdx := i;
      end;
    end;
  end;
  
  if bestMemberIdx <> -1 then
    Result := FTeamMembers[bestMemberIdx].MemberID;
end;

function TTeamTaskManager.GetMemberWorkload: string;
var
  i: Integer;
  workload: Double;
begin
  Result := 'Team Member Workload Report:' + LineEnding;
  Result := Result + '================================' + LineEnding;
  
  for i := 0 to High(FTeamMembers) do
  begin
    if FTeamMembers[i].IsActive then
    begin
      workload := CalculateMemberWorkload(FTeamMembers[i].MemberID);
      Result := Result + Format('%s: %.1f hours (Capacity: %.1f hours/week, Tasks: %d/%d)',
        [FTeamMembers[i].Name, workload, FTeamMembers[i].AvailableHoursPerWeek,
         FTeamMembers[i].CurrentTaskCount, FTeamMembers[i].MaxTasksActive]) + LineEnding;
    end;
  end;
end;

function TTeamTaskManager.BalanceWorkload: Integer;
begin
  Result := 0;
end;

function TTeamTaskManager.DefineCustomField(const AName: string; AType: TCustomFieldType;
  const ADefaultValue: string; ARequired: Boolean): Integer;
var
  idx: Integer;
begin
  idx := Length(FCustomFieldDefs);
  SetLength(FCustomFieldDefs, idx + 1);
  
  FCustomFieldDefs[idx].FieldID := FNextFieldDefID;
  FCustomFieldDefs[idx].FieldName := AName;
  FCustomFieldDefs[idx].FieldType := AType;
  FCustomFieldDefs[idx].DefaultValue := ADefaultValue;
  FCustomFieldDefs[idx].Required := ARequired;
  SetLength(FCustomFieldDefs[idx].ListOptions, 0);
  
  Result := FNextFieldDefID;
  Inc(FNextFieldDefID);
end;

function TTeamTaskManager.AddListOption(AFieldID: Integer; const AOption: string): Boolean;
var
  idx, optIdx: Integer;
begin
  idx := FindFieldDefIndex(AFieldID);
  Result := idx <> -1;
  
  if Result then
  begin
    optIdx := Length(FCustomFieldDefs[idx].ListOptions);
    SetLength(FCustomFieldDefs[idx].ListOptions, optIdx + 1);
    FCustomFieldDefs[idx].ListOptions[optIdx] := AOption;
  end;
end;

function TTeamTaskManager.SetCustomFieldValue(ATaskID, AFieldID: Integer; const AValue: string): Integer;
var
  i, idx: Integer;
begin
  for i := 0 to High(FCustomFieldValues) do
  begin
    if (FCustomFieldValues[i].TaskID = ATaskID) and 
       (FCustomFieldValues[i].FieldID = AFieldID) then
    begin
      FCustomFieldValues[i].Value := AValue;
      Result := FCustomFieldValues[i].ValueID;
      Exit;
    end;
  end;
  
  idx := Length(FCustomFieldValues);
  SetLength(FCustomFieldValues, idx + 1);
  
  FCustomFieldValues[idx].ValueID := FNextFieldValueID;
  FCustomFieldValues[idx].TaskID := ATaskID;
  FCustomFieldValues[idx].FieldID := AFieldID;
  FCustomFieldValues[idx].Value := AValue;
  
  Result := FNextFieldValueID;
  Inc(FNextFieldValueID);
end;

function TTeamTaskManager.GetCustomFieldValue(ATaskID, AFieldID: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(FCustomFieldValues) do
  begin
    if (FCustomFieldValues[i].TaskID = ATaskID) and 
       (FCustomFieldValues[i].FieldID = AFieldID) then
    begin
      Result := FCustomFieldValues[i].Value;
      Exit;
    end;
  end;
end;

function TTeamTaskManager.GetAllCustomFields: TCustomFieldDefArray;
begin
  Result := Copy(FCustomFieldDefs, 0, Length(FCustomFieldDefs));
end;

function TTeamTaskManager.GetTaskCustomValues(ATaskID: Integer): TCustomFieldValueArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FCustomFieldValues) do
    if FCustomFieldValues[i].TaskID = ATaskID then
      Inc(count);
      
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FCustomFieldValues) do
  begin
    if FCustomFieldValues[i].TaskID = ATaskID then
    begin
      Result[count] := FCustomFieldValues[i];
      Inc(count);
    end;
  end;
end;

function TTeamTaskManager.ScheduleTask(ATaskID: Integer; AStartTime: TDateTime; 
  ADurationMinutes: Integer; const ANotes: string): Integer;
var
  idx: Integer;
begin
  idx := Length(FTimeSlots);
  SetLength(FTimeSlots, idx + 1);
  
  FTimeSlots[idx].SlotID := FNextSlotID;
  FTimeSlots[idx].TaskID := ATaskID;
  FTimeSlots[idx].StartTime := AStartTime;
  FTimeSlots[idx].EndTime := IncMinute(AStartTime, ADurationMinutes);
  FTimeSlots[idx].Duration := ADurationMinutes;
  FTimeSlots[idx].IsConfirmed := False;
  FTimeSlots[idx].Notes := ANotes;
  
  Result := FNextSlotID;
  Inc(FNextSlotID);
end;

function TTeamTaskManager.RescheduleTask(ASlotID: Integer; ANewStart: TDateTime): Boolean;
var
  idx: Integer;
begin
  idx := FindTimeSlotIndex(ASlotID);
  Result := idx <> -1;
  
  if Result then
  begin
    FTimeSlots[idx].StartTime := ANewStart;
    FTimeSlots[idx].EndTime := IncMinute(ANewStart, FTimeSlots[idx].Duration);
  end;
end;

function TTeamTaskManager.GetTaskSchedule(ATaskID: Integer): TTimeSlotArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FTimeSlots) do
    if FTimeSlots[i].TaskID = ATaskID then
      Inc(count);
      
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FTimeSlots) do
  begin
    if FTimeSlots[i].TaskID = ATaskID then
    begin
      Result[count] := FTimeSlots[i];
      Inc(count);
    end;
  end;
end;

function TTeamTaskManager.GetScheduleForPeriod(AStart, AEnd: TDateTime): TTimeSlotArray;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FTimeSlots) do
  begin
    if (FTimeSlots[i].StartTime >= AStart) and (FTimeSlots[i].StartTime <= AEnd) then
      Inc(count);
  end;
  
  SetLength(Result, count);
  count := 0;
  for i := 0 to High(FTimeSlots) do
  begin
    if (FTimeSlots[i].StartTime >= AStart) and (FTimeSlots[i].StartTime <= AEnd) then
    begin
      Result[count] := FTimeSlots[i];
      Inc(count);
    end;
  end;
end;

function TTeamTaskManager.FindAvailableSlot(ADurationMinutes: Integer; 
  APreferredStart: TDateTime): TDateTime;
begin
  Result := APreferredStart;
end;

function TTeamTaskManager.AutoScheduleTasks(APrioritizeBy: string): Integer;
begin
  Result := 0;
end;

function TTeamTaskManager.DetectConflicts: TConflictArray;
begin
  DetectScheduleConflicts;
  DetectDependencyConflicts;
  DetectResourceConflicts;
  Result := Copy(FConflicts, 0, Length(FConflicts));
end;

function TTeamTaskManager.DetectScheduleConflicts: Integer;
begin
  Result := 0;
end;

function TTeamTaskManager.DetectDependencyConflicts: Integer;
begin
  Result := 0;
end;

function TTeamTaskManager.DetectResourceConflicts: Integer;
begin
  Result := 0;
end;

function TTeamTaskManager.GetActiveConflicts: TConflictArray;
begin
  Result := Copy(FConflicts, 0, Length(FConflicts));
end;

function TTeamTaskManager.ResolveConflict(AConflictID: Integer; const AResolution: string): Boolean;
begin
  Result := False;
end;

function TTeamTaskManager.GetTeamProductivity: string;
begin
  Result := 'Team Productivity Report:' + LineEnding;
  Result := Result + 'Total team members: ' + IntToStr(Length(FTeamMembers)) + LineEnding;
  Result := Result + 'Total assignments: ' + IntToStr(Length(FAssignments)) + LineEnding;
end;

function TTeamTaskManager.GetMemberPerformance(AMemberID: Integer): string;
begin
  Result := 'Member Performance Report for ID ' + IntToStr(AMemberID);
end;

function TTeamTaskManager.GetTeamCapacity: string;
var
  i: Integer;
  totalCapacity, usedCapacity: Double;
begin
  totalCapacity := 0.0;
  usedCapacity := 0.0;
  
  for i := 0 to High(FTeamMembers) do
  begin
    if FTeamMembers[i].IsActive then
    begin
      totalCapacity := totalCapacity + FTeamMembers[i].AvailableHoursPerWeek;
      usedCapacity := usedCapacity + CalculateMemberWorkload(FTeamMembers[i].MemberID);
    end;
  end;
  
  Result := Format('Team Capacity: %.1f / %.1f hours (%.1f%% utilized)',
    [usedCapacity, totalCapacity, (usedCapacity / totalCapacity) * 100]);
end;

function TTeamTaskManager.GetBottlenecks: string;
begin
  Result := 'Bottleneck Analysis: (placeholder)';
end;

function TTeamTaskManager.GetTaskDistribution: string;
begin
  Result := 'Task Distribution: (placeholder)';
end;

function TTeamTaskManager.PredictCompletionDate(ATaskID: Integer): TDateTime;
begin
  Result := Now + 7;
end;

function TTeamTaskManager.ImportFromCSV(const AFilename: string): Integer;
begin
  Result := 0;
end;

function TTeamTaskManager.ImportFromJSON(const AJSONString: string): Integer;
begin
  Result := 0;
end;

function TTeamTaskManager.ExportToJSON: string;
begin
  Result := '{"tasks": []}';
end;

function TTeamTaskManager.ExportToMarkdown: string;
var
  i: Integer;
  tasks: TTaskArray;
begin
  Result := '# Task Manager Export' + LineEnding + LineEnding;
  Result := Result + '## All Tasks' + LineEnding + LineEnding;
  
  tasks := GetAllTasks;
  for i := 0 to High(tasks) do
  begin
    Result := Result + Format('### %d. %s', [tasks[i].ID, tasks[i].Title]) + LineEnding;
    Result := Result + Format('**Status**: %s | **Priority**: %s | **Due**: %s',
      [TaskStatusToString(tasks[i].Status), 
       TaskPriorityToString(tasks[i].Priority),
       FormatDateTime('yyyy-mm-dd', tasks[i].DueDate)]) + LineEnding + LineEnding;
    Result := Result + tasks[i].Description + LineEnding + LineEnding;
  end;
end;

function TTeamTaskManager.ConflictTypeToString(AType: TConflictType): string;
begin
  case AType of
    ctOverlappingSchedule: Result := 'Overlapping Schedule';
    ctOverdueDependency: Result := 'Overdue Dependency';
    ctResourceOverload: Result := 'Resource Overload';
    ctMissingDependency: Result := 'Missing Dependency';
    ctCircularDependency: Result := 'Circular Dependency';
  else
    Result := 'Unknown';
  end;
end;

function TTeamTaskManager.CustomFieldTypeToString(AType: TCustomFieldType): string;
begin
  case AType of
    cftString: Result := 'String';
    cftInteger: Result := 'Integer';
    cftFloat: Result := 'Float';
    cftDate: Result := 'Date';
    cftBoolean: Result := 'Boolean';
    cftList: Result := 'List';
  else
    Result := 'Unknown';
  end;
end;

function TTeamTaskManager.TeamMemberToString(const AMember: TTeamMember): string;
begin
  Result := Format('Member #%d: %s <%s> - %s (Tasks: %d/%d, Hours/Week: %.1f)',
    [AMember.MemberID, AMember.Name, AMember.Email, AMember.Role,
     AMember.CurrentTaskCount, AMember.MaxTasksActive, AMember.AvailableHoursPerWeek]);
end;

function TTeamTaskManager.SaveTeamDataToFile(const AFilename: string): Boolean;
begin
  Result := True;
end;

function TTeamTaskManager.LoadTeamDataFromFile(const AFilename: string): Boolean;
begin
  Result := True;
end;

end.
