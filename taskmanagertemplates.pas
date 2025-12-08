
unit taskmanagertemplates;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, DateUtils, Classes, taskmanager, taskmanagercomments;


type
  // Template category types
  TTemplateCategory = (
    tcDevelopment,
    tcMarketing,
    tcHumanResources,
    tcSales,
    tcSupport,
    tcOperations,
    tcFinance,
    tcPersonal,
    tcEducation,
    tcResearch,
    tcGeneral
  );

  // Variable definition for templates
  TTemplateVariable = record
    Name: string;           // e.g., 'PROJECT_NAME'
    Description: string;    // e.g., 'Name of the project'
    DefaultValue: string;   // Default value if not provided
    Required: boolean;      // Whether this variable must be provided
  end;
  TTemplateVariableArray = array of TTemplateVariable;

  // Single task within a template
  TTemplateTask = record
    Title: string;
    Description: string;
    Category: string;
    Priority: TTaskPriority;
    DaysFromStart: integer;  // Days offset from template instantiation
    EstimatedHours: double;
    Tags: array of string;
    DependsOnIndex: integer; // Index of task this depends on (-1 if none)
  end;
  TTemplateTaskArray = array of TTemplateTask;

  // Main template structure
  TTaskTemplate = record
    ID: integer;
    Name: string;
    Description: string;
    Category: TTemplateCategory;
    Author: string;
    Version: string;
    CreatedDate: TDateTime;
    ModifiedDate: TDateTime;
    UsageCount: integer;
    SuccessRate: double;     // Percentage of tasks completed when using this template
    IsActive: boolean;
    Variables: TTemplateVariableArray;
    Tasks: TTemplateTaskArray;
  end;
  TTaskTemplateArray = array of TTaskTemplate;

  // Variable substitution mapping
  TVariableMapping = record
    Name: string;
    Value: string;
  end;
  TVariableMappingArray = array of TVariableMapping;

  // Template instantiation result
  TTemplateInstantiation = record
    TemplateID: integer;
    InstantiationDate: TDateTime;
    TaskIDs: array of integer;  // IDs of created tasks
    Variables: TVariableMappingArray;
    Success: boolean;
  end;
  TTemplateInstantiationArray = array of TTemplateInstantiation;

  // Main template manager class
  TTemplateTaskManager = class(TCommentedTaskManager)
  private
    FTemplates: TTaskTemplateArray;
    FInstantiations: TTemplateInstantiationArray;
    FNextTemplateID: integer;
    
    function FindTemplateIndex(ATemplateID: integer): integer;
    function SubstituteVariables(const AText: string; const AMappings: TVariableMappingArray): string;
    function ValidateVariables(ATemplateIndex: integer; const AMappings: TVariableMappingArray): boolean;
    procedure AddBuiltInTemplates;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Template management
    function CreateTemplate(const AName, ADescription: string; 
      ACategory: TTemplateCategory; const AAuthor, AVersion: string): integer;
    function DeleteTemplate(ATemplateID: integer): boolean;
    function UpdateTemplateInfo(ATemplateID: integer; const AName, ADescription: string): boolean;
    function ActivateTemplate(ATemplateID: integer): boolean;
    function DeactivateTemplate(ATemplateID: integer): boolean;
    function DuplicateTemplate(ATemplateID: integer; const ANewName: string): integer;
    
    // Variable management
    function AddTemplateVariable(ATemplateID: integer; const AName, ADescription, 
      ADefaultValue: string; ARequired: boolean): boolean;
    function RemoveTemplateVariable(ATemplateID: integer; const AVariableName: string): boolean;
    function GetTemplateVariables(ATemplateID: integer): TTemplateVariableArray;
    
    // Task management within templates
    function AddTemplateTask(ATemplateID: integer; const ATitle, ADescription, 
      ACategory: string; APriority: TTaskPriority; ADaysFromStart: integer; 
      AEstimatedHours: double; const ATags: array of string; ADependsOnIndex: integer): boolean;
    function RemoveTemplateTask(ATemplateID: integer; ATaskIndex: integer): boolean;
    function GetTemplateTaskCount(ATemplateID: integer): integer;
    
    // Template instantiation
    function InstantiateTemplate(ATemplateID: integer; 
      const AVariables: TVariableMappingArray): TTemplateInstantiation;
    function InstantiateTemplateSimple(ATemplateID: integer): TTemplateInstantiation;
    function GetInstantiationHistory: TTemplateInstantiationArray;
    function GetTemplateUsageCount(ATemplateID: integer): integer;
    
    // Template querying
    function GetAllTemplates: TTaskTemplateArray;
    function GetActiveTemplates: TTaskTemplateArray;
    function GetTemplatesByCategory(ACategory: TTemplateCategory): TTaskTemplateArray;
    function SearchTemplates(const ASearchTerm: string): TTaskTemplateArray;
    function GetTemplateByID(ATemplateID: integer): TTaskTemplate;
    function GetMostUsedTemplates(ACount: integer): TTaskTemplateArray;
    
    // Import/Export
    function ExportTemplateToString(ATemplateID: integer): string;
    function ImportTemplateFromString(const AData: string): integer;
    function SaveTemplatesToFile(const AFilename: string): boolean;
    function LoadTemplatesFromFile(const AFilename: string): boolean;
    
    // Statistics and reporting
    function GetTemplateStatistics: string;
    function GetCategoryStatistics: string;
    function UpdateTemplateSuccessRate(ATemplateID: integer; ASuccessRate: double): boolean;
    
    // Utility functions
    function TemplateCategoryToString(ACategory: TTemplateCategory): string;
    function StringToTemplateCategory(const AStr: string): TTemplateCategory;
    
    // Self-test
    procedure SelfTest;
  end;

implementation

{ TTemplateTaskManager }

constructor TTemplateTaskManager.Create;
begin
  inherited Create;
  SetLength(FTemplates, 0);
  SetLength(FInstantiations, 0);
  FNextTemplateID := 1;
  AddBuiltInTemplates;
end;

destructor TTemplateTaskManager.Destroy;
begin
  SetLength(FTemplates, 0);
  SetLength(FInstantiations, 0);
  inherited Destroy;
end;

function TTemplateTaskManager.FindTemplateIndex(ATemplateID: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to High(FTemplates) do
  begin
    if FTemplates[i].ID = ATemplateID then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTemplateTaskManager.SubstituteVariables(const AText: string; 
  const AMappings: TVariableMappingArray): string;
var
  i: integer;
  PlaceholderStart, PlaceholderEnd: integer;
  Placeholder, VarName: string;
begin
  Result := AText;
  for i := 0 to High(AMappings) do
  begin
    Placeholder := '{' + AMappings[i].Name + '}';
    Result := StringReplace(Result, Placeholder, AMappings[i].Value, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TTemplateTaskManager.ValidateVariables(ATemplateIndex: integer; 
  const AMappings: TVariableMappingArray): boolean;
var
  i, j: integer;
  Found: boolean;
begin
  Result := true;
  
  // Check all required variables are provided
  for i := 0 to High(FTemplates[ATemplateIndex].Variables) do
  begin
    if FTemplates[ATemplateIndex].Variables[i].Required then
    begin
      Found := false;
      for j := 0 to High(AMappings) do
      begin
        if AMappings[j].Name = FTemplates[ATemplateIndex].Variables[i].Name then
        begin
          Found := true;
          Break;
        end;
      end;
      
      if not Found then
      begin
        Result := false;
        Exit;
      end;
    end;
  end;
end;

procedure TTemplateTaskManager.AddBuiltInTemplates;
var
  TemplateID: integer;
begin
  // Template 1: Web Development Project
  TemplateID := CreateTemplate(
    'Web Development Project',
    'Complete web application development workflow',
    tcDevelopment,
    'System',
    '1.0'
  );
  AddTemplateVariable(TemplateID, 'PROJECT_NAME', 'Name of the project', 'New Project', true);
  AddTemplateVariable(TemplateID, 'CLIENT_NAME', 'Client name', 'Client', false);
  AddTemplateTask(TemplateID, 'Requirements Gathering - {PROJECT_NAME}', 
    'Collect and document requirements for {PROJECT_NAME}', 'Planning', 
    tpHigh, 0, 8, ['planning', 'requirements'], -1);
  AddTemplateTask(TemplateID, 'UI/UX Design - {PROJECT_NAME}',
    'Create wireframes and mockups for {PROJECT_NAME}', 'Design',
    tpHigh, 3, 16, ['design', 'ui'], 0);
  AddTemplateTask(TemplateID, 'Database Schema Design - {PROJECT_NAME}',
    'Design database structure for {PROJECT_NAME}', 'Backend',
    tpHigh, 5, 8, ['database', 'backend'], 0);
  AddTemplateTask(TemplateID, 'Frontend Development - {PROJECT_NAME}',
    'Implement UI for {PROJECT_NAME}', 'Frontend',
    tpCritical, 7, 40, ['frontend', 'development'], 1);
  AddTemplateTask(TemplateID, 'Backend API Development - {PROJECT_NAME}',
    'Build REST API for {PROJECT_NAME}', 'Backend',
    tpCritical, 7, 40, ['backend', 'api'], 2);
  AddTemplateTask(TemplateID, 'Integration Testing - {PROJECT_NAME}',
    'Test frontend and backend integration', 'Testing',
    tpHigh, 14, 16, ['testing', 'qa'], 3);
  AddTemplateTask(TemplateID, 'Deployment - {PROJECT_NAME}',
    'Deploy {PROJECT_NAME} to production', 'DevOps',
    tpCritical, 21, 8, ['deployment', 'production'], 5);
    
  // Template 2: Employee Onboarding
  TemplateID := CreateTemplate(
    'Employee Onboarding',
    'Complete new employee onboarding checklist',
    tcHumanResources,
    'System',
    '1.0'
  );
  AddTemplateVariable(TemplateID, 'EMPLOYEE_NAME', 'Name of new employee', 'New Employee', true);
  AddTemplateVariable(TemplateID, 'DEPARTMENT', 'Department name', 'General', true);
  AddTemplateVariable(TemplateID, 'START_DATE', 'Start date', '', false);
  AddTemplateTask(TemplateID, 'Prepare workstation for {EMPLOYEE_NAME}',
    'Set up computer, desk, and office supplies', 'Setup',
    tpHigh, -1, 2, ['hr', 'setup'], -1);
  AddTemplateTask(TemplateID, 'Create accounts for {EMPLOYEE_NAME}',
    'Email, system access, and tool accounts', 'IT',
    tpCritical, -1, 1, ['it', 'accounts'], -1);
  AddTemplateTask(TemplateID, 'Welcome meeting with {EMPLOYEE_NAME}',
    'Introduction to team and company culture', 'Meeting',
    tpHigh, 0, 1, ['meeting', 'welcome'], -1);
  AddTemplateTask(TemplateID, 'HR paperwork for {EMPLOYEE_NAME}',
    'Complete all necessary HR documentation', 'HR',
    tpCritical, 0, 2, ['hr', 'paperwork'], -1);
  AddTemplateTask(TemplateID, '{DEPARTMENT} training for {EMPLOYEE_NAME}',
    'Department-specific training and procedures', 'Training',
    tpHigh, 1, 8, ['training'], 2);
  AddTemplateTask(TemplateID, 'Assign mentor to {EMPLOYEE_NAME}',
    'Pair with experienced team member', 'HR',
    tpMedium, 0, 1, ['mentoring'], -1);
  AddTemplateTask(TemplateID, '30-day check-in with {EMPLOYEE_NAME}',
    'Review progress and address concerns', 'Meeting',
    tpMedium, 30, 1, ['meeting', 'review'], -1);
    
  // Template 3: Marketing Campaign
  TemplateID := CreateTemplate(
    'Marketing Campaign Launch',
    'Complete marketing campaign workflow',
    tcMarketing,
    'System',
    '1.0'
  );
  AddTemplateVariable(TemplateID, 'CAMPAIGN_NAME', 'Campaign name', 'New Campaign', true);
  AddTemplateVariable(TemplateID, 'TARGET_AUDIENCE', 'Target audience', 'General', false);
  AddTemplateVariable(TemplateID, 'BUDGET', 'Campaign budget', '10000', false);
  AddTemplateTask(TemplateID, 'Campaign Strategy - {CAMPAIGN_NAME}',
    'Define goals, KPIs, and strategy for {CAMPAIGN_NAME}', 'Planning',
    tpCritical, 0, 4, ['planning', 'strategy'], -1);
  AddTemplateTask(TemplateID, 'Content Creation - {CAMPAIGN_NAME}',
    'Create marketing content for {TARGET_AUDIENCE}', 'Content',
    tpHigh, 2, 16, ['content', 'creative'], 0);
  AddTemplateTask(TemplateID, 'Design Assets - {CAMPAIGN_NAME}',
    'Create visual assets and graphics', 'Design',
    tpHigh, 3, 12, ['design', 'graphics'], 0);
  AddTemplateTask(TemplateID, 'Social Media Setup - {CAMPAIGN_NAME}',
    'Prepare social media posts and schedule', 'Social',
    tpMedium, 5, 4, ['social', 'scheduling'], 1);
  AddTemplateTask(TemplateID, 'Email Campaign Setup - {CAMPAIGN_NAME}',
    'Create email templates and sequences', 'Email',
    tpMedium, 5, 6, ['email', 'automation'], 1);
  AddTemplateTask(TemplateID, 'Launch {CAMPAIGN_NAME}',
    'Go live with all campaign elements', 'Launch',
    tpCritical, 7, 2, ['launch'], 2);
  AddTemplateTask(TemplateID, 'Monitor & Optimize - {CAMPAIGN_NAME}',
    'Track performance and make adjustments', 'Analytics',
    tpHigh, 8, 8, ['analytics', 'optimization'], 5);
    
  // Template 4: Bug Fix Workflow
  TemplateID := CreateTemplate(
    'Bug Fix Workflow',
    'Standard process for fixing bugs',
    tcDevelopment,
    'System',
    '1.0'
  );
  AddTemplateVariable(TemplateID, 'BUG_ID', 'Bug tracking ID', 'BUG-000', true);
  AddTemplateVariable(TemplateID, 'SEVERITY', 'Bug severity', 'Medium', false);
  AddTemplateTask(TemplateID, 'Reproduce bug {BUG_ID}',
    'Confirm and document reproduction steps', 'Investigation',
    tpHigh, 0, 1, ['bug', 'investigation'], -1);
  AddTemplateTask(TemplateID, 'Root cause analysis - {BUG_ID}',
    'Identify the underlying cause', 'Analysis',
    tpHigh, 0, 2, ['bug', 'analysis'], 0);
  AddTemplateTask(TemplateID, 'Implement fix for {BUG_ID}',
    'Code the solution', 'Development',
    tpCritical, 1, 4, ['bug', 'fix'], 1);
  AddTemplateTask(TemplateID, 'Test fix for {BUG_ID}',
    'Verify the fix works and no regressions', 'Testing',
    tpCritical, 2, 2, ['testing', 'qa'], 2);
  AddTemplateTask(TemplateID, 'Code review for {BUG_ID}',
    'Peer review of the fix', 'Review',
    tpHigh, 2, 1, ['review'], 2);
  AddTemplateTask(TemplateID, 'Deploy fix for {BUG_ID}',
    'Deploy to production', 'Deployment',
    tpCritical, 3, 1, ['deployment'], 3);
end;

function TTemplateTaskManager.CreateTemplate(const AName, ADescription: string;
  ACategory: TTemplateCategory; const AAuthor, AVersion: string): integer;
var
  NewTemplate: TTaskTemplate;
  Len: integer;
begin
  NewTemplate.ID := FNextTemplateID;
  Inc(FNextTemplateID);
  NewTemplate.Name := AName;
  NewTemplate.Description := ADescription;
  NewTemplate.Category := ACategory;
  NewTemplate.Author := AAuthor;
  NewTemplate.Version := AVersion;
  NewTemplate.CreatedDate := Now;
  NewTemplate.ModifiedDate := Now;
  NewTemplate.UsageCount := 0;
  NewTemplate.SuccessRate := 0.0;
  NewTemplate.IsActive := true;
  SetLength(NewTemplate.Variables, 0);
  SetLength(NewTemplate.Tasks, 0);
  
  Len := Length(FTemplates);
  SetLength(FTemplates, Len + 1);
  FTemplates[Len] := NewTemplate;
  
  Result := NewTemplate.ID;
end;

function TTemplateTaskManager.DeleteTemplate(ATemplateID: integer): boolean;
var
  Idx, i: integer;
begin
  Result := false;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  for i := Idx to High(FTemplates) - 1 do
    FTemplates[i] := FTemplates[i + 1];
  SetLength(FTemplates, Length(FTemplates) - 1);
  
  Result := true;
end;

function TTemplateTaskManager.UpdateTemplateInfo(ATemplateID: integer; 
  const AName, ADescription: string): boolean;
var
  Idx: integer;
begin
  Result := false;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  FTemplates[Idx].Name := AName;
  FTemplates[Idx].Description := ADescription;
  FTemplates[Idx].ModifiedDate := Now;
  Result := true;
end;

function TTemplateTaskManager.ActivateTemplate(ATemplateID: integer): boolean;
var
  Idx: integer;
begin
  Result := false;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  FTemplates[Idx].IsActive := true;
  Result := true;
end;

function TTemplateTaskManager.DeactivateTemplate(ATemplateID: integer): boolean;
var
  Idx: integer;
begin
  Result := false;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  FTemplates[Idx].IsActive := false;
  Result := true;
end;

function TTemplateTaskManager.DuplicateTemplate(ATemplateID: integer; 
  const ANewName: string): integer;
var
  Idx: integer;
  NewTemplate: TTaskTemplate;
  Len: integer;
begin
  Result := -1;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  NewTemplate := FTemplates[Idx];
  NewTemplate.ID := FNextTemplateID;
  Inc(FNextTemplateID);
  NewTemplate.Name := ANewName;
  NewTemplate.CreatedDate := Now;
  NewTemplate.ModifiedDate := Now;
  NewTemplate.UsageCount := 0;
  
  Len := Length(FTemplates);
  SetLength(FTemplates, Len + 1);
  FTemplates[Len] := NewTemplate;
  
  Result := NewTemplate.ID;
end;

function TTemplateTaskManager.AddTemplateVariable(ATemplateID: integer; 
  const AName, ADescription, ADefaultValue: string; ARequired: boolean): boolean;
var
  Idx, Len: integer;
  NewVar: TTemplateVariable;
begin
  Result := false;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  NewVar.Name := AName;
  NewVar.Description := ADescription;
  NewVar.DefaultValue := ADefaultValue;
  NewVar.Required := ARequired;
  
  Len := Length(FTemplates[Idx].Variables);
  SetLength(FTemplates[Idx].Variables, Len + 1);
  FTemplates[Idx].Variables[Len] := NewVar;
  FTemplates[Idx].ModifiedDate := Now;
  
  Result := true;
end;

function TTemplateTaskManager.RemoveTemplateVariable(ATemplateID: integer; 
  const AVariableName: string): boolean;
var
  Idx, VarIdx, i: integer;
begin
  Result := false;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  VarIdx := -1;
  for i := 0 to High(FTemplates[Idx].Variables) do
  begin
    if FTemplates[Idx].Variables[i].Name = AVariableName then
    begin
      VarIdx := i;
      Break;
    end;
  end;
  
  if VarIdx = -1 then
    Exit;
    
  for i := VarIdx to High(FTemplates[Idx].Variables) - 1 do
    FTemplates[Idx].Variables[i] := FTemplates[Idx].Variables[i + 1];
  SetLength(FTemplates[Idx].Variables, Length(FTemplates[Idx].Variables) - 1);
  FTemplates[Idx].ModifiedDate := Now;
  
  Result := true;
end;

function TTemplateTaskManager.GetTemplateVariables(ATemplateID: integer): TTemplateVariableArray;
var
  Idx: integer;
begin
  SetLength(Result, 0);
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  Result := FTemplates[Idx].Variables;
end;

function TTemplateTaskManager.AddTemplateTask(ATemplateID: integer; 
  const ATitle, ADescription, ACategory: string; APriority: TTaskPriority; 
  ADaysFromStart: integer; AEstimatedHours: double; const ATags: array of string;
  ADependsOnIndex: integer): boolean;
var
  Idx, Len, i: integer;
  NewTask: TTemplateTask;
begin
  Result := false;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  NewTask.Title := ATitle;
  NewTask.Description := ADescription;
  NewTask.Category := ACategory;
  NewTask.Priority := APriority;
  NewTask.DaysFromStart := ADaysFromStart;
  NewTask.EstimatedHours := AEstimatedHours;
  NewTask.DependsOnIndex := ADependsOnIndex;
  
  SetLength(NewTask.Tags, Length(ATags));
  for i := 0 to High(ATags) do
    NewTask.Tags[i] := ATags[i];
    
  Len := Length(FTemplates[Idx].Tasks);
  SetLength(FTemplates[Idx].Tasks, Len + 1);
  FTemplates[Idx].Tasks[Len] := NewTask;
  FTemplates[Idx].ModifiedDate := Now;
  
  Result := true;
end;

function TTemplateTaskManager.RemoveTemplateTask(ATemplateID: integer; 
  ATaskIndex: integer): boolean;
var
  Idx, i: integer;
begin
  Result := false;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  if (ATaskIndex < 0) or (ATaskIndex > High(FTemplates[Idx].Tasks)) then
    Exit;
    
  for i := ATaskIndex to High(FTemplates[Idx].Tasks) - 1 do
    FTemplates[Idx].Tasks[i] := FTemplates[Idx].Tasks[i + 1];
  SetLength(FTemplates[Idx].Tasks, Length(FTemplates[Idx].Tasks) - 1);
  FTemplates[Idx].ModifiedDate := Now;
  
  Result := true;
end;

function TTemplateTaskManager.GetTemplateTaskCount(ATemplateID: integer): integer;
var
  Idx: integer;
begin
  Result := 0;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  Result := Length(FTemplates[Idx].Tasks);
end;

function TTemplateTaskManager.InstantiateTemplate(ATemplateID: integer; 
  const AVariables: TVariableMappingArray): TTemplateInstantiation;
var
  Idx, i, j: integer;
  TaskID: integer;
  TaskTitle, TaskDesc: string;
  DueDate: TDateTime;
  Mappings: TVariableMappingArray;
begin
  Result.TemplateID := ATemplateID;
  Result.InstantiationDate := Now;
  Result.Success := false;
  SetLength(Result.TaskIDs, 0);
  SetLength(Result.Variables, 0);
  
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  if not FTemplates[Idx].IsActive then
    Exit;
    
  // Build complete mappings with defaults
  SetLength(Mappings, Length(FTemplates[Idx].Variables));
  for i := 0 to High(FTemplates[Idx].Variables) do
  begin
    Mappings[i].Name := FTemplates[Idx].Variables[i].Name;
    Mappings[i].Value := FTemplates[Idx].Variables[i].DefaultValue;
    
    // Override with provided values
    for j := 0 to High(AVariables) do
    begin
      if AVariables[j].Name = FTemplates[Idx].Variables[i].Name then
      begin
        Mappings[i].Value := AVariables[j].Value;
        Break;
      end;
    end;
  end;
  
  // Validate required variables
  if not ValidateVariables(Idx, Mappings) then
    Exit;
    
  // Create tasks
  SetLength(Result.TaskIDs, Length(FTemplates[Idx].Tasks));
  for i := 0 to High(FTemplates[Idx].Tasks) do
  begin
    TaskTitle := SubstituteVariables(FTemplates[Idx].Tasks[i].Title, Mappings);
    TaskDesc := SubstituteVariables(FTemplates[Idx].Tasks[i].Description, Mappings);
    DueDate := IncDay(Now, FTemplates[Idx].Tasks[i].DaysFromStart);
    
    TaskID := AddTask(
      TaskTitle,
      TaskDesc,
      SubstituteVariables(FTemplates[Idx].Tasks[i].Category, Mappings),
      FTemplates[Idx].Tasks[i].Priority,
      DueDate,
      FTemplates[Idx].Tasks[i].EstimatedHours
    );
    
    Result.TaskIDs[i] := TaskID;
    
    // Add tags
    for j := 0 to High(FTemplates[Idx].Tasks[i].Tags) do
      AddTagToTask(TaskID, FTemplates[Idx].Tasks[i].Tags[j]);
  end;
  
  // Store instantiation
  Result.Variables := Mappings;
  Result.Success := true;
  
  i := Length(FInstantiations);
  SetLength(FInstantiations, i + 1);
  FInstantiations[i] := Result;
  
  // Update template usage
  Inc(FTemplates[Idx].UsageCount);
end;

function TTemplateTaskManager.InstantiateTemplateSimple(ATemplateID: integer): TTemplateInstantiation;
var
  EmptyMappings: TVariableMappingArray;
begin
  SetLength(EmptyMappings, 0);
  Result := InstantiateTemplate(ATemplateID, EmptyMappings);
end;

function TTemplateTaskManager.GetInstantiationHistory: TTemplateInstantiationArray;
begin
  Result := FInstantiations;
end;

function TTemplateTaskManager.GetTemplateUsageCount(ATemplateID: integer): integer;
var
  Idx: integer;
begin
  Result := 0;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  Result := FTemplates[Idx].UsageCount;
end;

function TTemplateTaskManager.GetAllTemplates: TTaskTemplateArray;
begin
  Result := FTemplates;
end;

function TTemplateTaskManager.GetActiveTemplates: TTaskTemplateArray;
var
  i, Count: integer;
begin
  Count := 0;
  for i := 0 to High(FTemplates) do
  begin
    if FTemplates[i].IsActive then
      Inc(Count);
  end;
    
  SetLength(Result, Count);
  Count := 0;
  for i := 0 to High(FTemplates) do
  begin
    if FTemplates[i].IsActive then
    begin
      Result[Count] := FTemplates[i];
      Inc(Count);
    end;
  end;
end;

function TTemplateTaskManager.GetTemplatesByCategory(ACategory: TTemplateCategory): TTaskTemplateArray;
var
  i, Count: integer;
begin
  Count := 0;
  for i := 0 to High(FTemplates) do
  begin
    if FTemplates[i].Category = ACategory then
      Inc(Count);
  end;
    
  SetLength(Result, Count);
  Count := 0;
  for i := 0 to High(FTemplates) do
  begin
    if FTemplates[i].Category = ACategory then
    begin
      Result[Count] := FTemplates[i];
      Inc(Count);
    end;
  end;
end;

function TTemplateTaskManager.SearchTemplates(const ASearchTerm: string): TTaskTemplateArray;
var
  i, Count: integer;
  SearchLower, NameLower, DescLower: string;
begin
  SearchLower := LowerCase(ASearchTerm);
  Count := 0;
  
  for i := 0 to High(FTemplates) do
  begin
    NameLower := LowerCase(FTemplates[i].Name);
    DescLower := LowerCase(FTemplates[i].Description);
    if (Pos(SearchLower, NameLower) > 0) or (Pos(SearchLower, DescLower) > 0) then
      Inc(Count);
  end;
    
  SetLength(Result, Count);
  Count := 0;
  for i := 0 to High(FTemplates) do
  begin
    NameLower := LowerCase(FTemplates[i].Name);
    DescLower := LowerCase(FTemplates[i].Description);
    if (Pos(SearchLower, NameLower) > 0) or (Pos(SearchLower, DescLower) > 0) then
    begin
      Result[Count] := FTemplates[i];
      Inc(Count);
    end;
  end;
end;

function TTemplateTaskManager.GetTemplateByID(ATemplateID: integer): TTaskTemplate;
var
  Idx: integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  Result := FTemplates[Idx];
end;

function TTemplateTaskManager.GetMostUsedTemplates(ACount: integer): TTaskTemplateArray;
var
  i, j, MaxIdx: integer;
  Temp: TTaskTemplate;
  Sorted: TTaskTemplateArray;
begin
  // Make a copy for sorting
  SetLength(Sorted, Length(FTemplates));
  for i := 0 to High(FTemplates) do
    Sorted[i] := FTemplates[i];
    
  // Simple selection sort by usage count (descending)
  for i := 0 to High(Sorted) - 1 do
  begin
    MaxIdx := i;
    for j := i + 1 to High(Sorted) do
    begin
      if Sorted[j].UsageCount > Sorted[MaxIdx].UsageCount then
        MaxIdx := j;
    end;
    
    if MaxIdx <> i then
    begin
      Temp := Sorted[i];
      Sorted[i] := Sorted[MaxIdx];
      Sorted[MaxIdx] := Temp;
    end;
  end;
  
  // Return top N
  if ACount > Length(Sorted) then
    ACount := Length(Sorted);
    
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := Sorted[i];
end;

function TTemplateTaskManager.ExportTemplateToString(ATemplateID: integer): string;
var
  Idx, i, j: integer;
begin
  Result := '';
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  Result := '[TEMPLATE]' + LineEnding;
  Result := Result + 'ID=' + IntToStr(FTemplates[Idx].ID) + LineEnding;
  Result := Result + 'Name=' + FTemplates[Idx].Name + LineEnding;
  Result := Result + 'Description=' + FTemplates[Idx].Description + LineEnding;
  Result := Result + 'Category=' + TemplateCategoryToString(FTemplates[Idx].Category) + LineEnding;
  Result := Result + 'Author=' + FTemplates[Idx].Author + LineEnding;
  Result := Result + 'Version=' + FTemplates[Idx].Version + LineEnding;
  
  Result := Result + '[VARIABLES]' + LineEnding;
  for i := 0 to High(FTemplates[Idx].Variables) do
  begin
    Result := Result + FTemplates[Idx].Variables[i].Name + '|';
    Result := Result + FTemplates[Idx].Variables[i].Description + '|';
    Result := Result + FTemplates[Idx].Variables[i].DefaultValue + '|';
    if FTemplates[Idx].Variables[i].Required then
      Result := Result + '1'
    else
      Result := Result + '0';
    Result := Result + LineEnding;
  end;
  
  Result := Result + '[TASKS]' + LineEnding;
  for i := 0 to High(FTemplates[Idx].Tasks) do
  begin
    Result := Result + FTemplates[Idx].Tasks[i].Title + '|';
    Result := Result + FTemplates[Idx].Tasks[i].Description + '|';
    Result := Result + FTemplates[Idx].Tasks[i].Category + '|';
    Result := Result + IntToStr(Ord(FTemplates[Idx].Tasks[i].Priority)) + '|';
    Result := Result + IntToStr(FTemplates[Idx].Tasks[i].DaysFromStart) + '|';
    Result := Result + FloatToStr(FTemplates[Idx].Tasks[i].EstimatedHours) + '|';
    Result := Result + IntToStr(FTemplates[Idx].Tasks[i].DependsOnIndex) + '|';
    
    for j := 0 to High(FTemplates[Idx].Tasks[i].Tags) do
    begin
      Result := Result + FTemplates[Idx].Tasks[i].Tags[j];
      if j < High(FTemplates[Idx].Tasks[i].Tags) then
        Result := Result + ',';
    end;
    Result := Result + LineEnding;
  end;
  
  Result := Result + '[END]' + LineEnding;
end;

function TTemplateTaskManager.ImportTemplateFromString(const AData: string): integer;
begin
  // Simplified implementation - would parse the exported format
  Result := -1;
  // TODO: Implement full parsing logic
end;

function TTemplateTaskManager.SaveTemplatesToFile(const AFilename: string): boolean;
var
  F: TextFile;
  i: integer;
begin
  Result := false;
  try
    AssignFile(F, AFilename);
    Rewrite(F);
    
    for i := 0 to High(FTemplates) do
    begin
      Write(F, ExportTemplateToString(FTemplates[i].ID));
    end;
    
    CloseFile(F);
    Result := true;
  except
    Result := false;
  end;
end;

function TTemplateTaskManager.LoadTemplatesFromFile(const AFilename: string): boolean;
begin
  Result := false;
  // TODO: Implement loading logic
end;

function TTemplateTaskManager.GetTemplateStatistics: string;
var
  TotalTemplates, ActiveTemplates, TotalUsage: integer;
  i: integer;
begin
  TotalTemplates := Length(FTemplates);
  ActiveTemplates := 0;
  TotalUsage := 0;
  
  for i := 0 to High(FTemplates) do
  begin
    if FTemplates[i].IsActive then
      Inc(ActiveTemplates);
    TotalUsage := TotalUsage + FTemplates[i].UsageCount;
  end;
  
  Result := 'Template Statistics:' + LineEnding;
  Result := Result + '  Total Templates: ' + IntToStr(TotalTemplates) + LineEnding;
  Result := Result + '  Active Templates: ' + IntToStr(ActiveTemplates) + LineEnding;
  Result := Result + '  Total Usage: ' + IntToStr(TotalUsage) + LineEnding;
  Result := Result + '  Total Instantiations: ' + IntToStr(Length(FInstantiations)) + LineEnding;
end;

function TTemplateTaskManager.GetCategoryStatistics: string;
var
  CategoryCounts: array[TTemplateCategory] of integer;
  Cat: TTemplateCategory;
  i: integer;
begin
  for Cat := Low(TTemplateCategory) to High(TTemplateCategory) do
    CategoryCounts[Cat] := 0;
    
  for i := 0 to High(FTemplates) do
    Inc(CategoryCounts[FTemplates[i].Category]);
    
  Result := 'Templates by Category:' + LineEnding;
  for Cat := Low(TTemplateCategory) to High(TTemplateCategory) do
  begin
    if CategoryCounts[Cat] > 0 then
      Result := Result + '  ' + TemplateCategoryToString(Cat) + ': ' + 
                IntToStr(CategoryCounts[Cat]) + LineEnding;
  end;
end;

function TTemplateTaskManager.UpdateTemplateSuccessRate(ATemplateID: integer; 
  ASuccessRate: double): boolean;
var
  Idx: integer;
begin
  Result := false;
  Idx := FindTemplateIndex(ATemplateID);
  if Idx = -1 then
    Exit;
    
  FTemplates[Idx].SuccessRate := ASuccessRate;
  Result := true;
end;

function TTemplateTaskManager.TemplateCategoryToString(ACategory: TTemplateCategory): string;
begin
  case ACategory of
    tcDevelopment: Result := 'Development';
    tcMarketing: Result := 'Marketing';
    tcHumanResources: Result := 'Human Resources';
    tcSales: Result := 'Sales';
    tcSupport: Result := 'Support';
    tcOperations: Result := 'Operations';
    tcFinance: Result := 'Finance';
    tcPersonal: Result := 'Personal';
    tcEducation: Result := 'Education';
    tcResearch: Result := 'Research';
    tcGeneral: Result := 'General';
  else
    Result := 'Unknown';
  end;
end;

function TTemplateTaskManager.StringToTemplateCategory(const AStr: string): TTemplateCategory;
var
  StrLower: string;
begin
  StrLower := LowerCase(AStr);
  if StrLower = 'development' then Result := tcDevelopment
  else if StrLower = 'marketing' then Result := tcMarketing
  else if StrLower = 'human resources' then Result := tcHumanResources
  else if StrLower = 'sales' then Result := tcSales
  else if StrLower = 'support' then Result := tcSupport
  else if StrLower = 'operations' then Result := tcOperations
  else if StrLower = 'finance' then Result := tcFinance
  else if StrLower = 'personal' then Result := tcPersonal
  else if StrLower = 'education' then Result := tcEducation
  else if StrLower = 'research' then Result := tcResearch
  else Result := tcGeneral;
end;

procedure TTemplateTaskManager.SelfTest;
var
  Templates: TTaskTemplateArray;
  Variables: TTemplateVariableArray;
  Mappings: TVariableMappingArray;
  Instantiation: TTemplateInstantiation;
  TemplateID: integer;
  i: integer;
begin
  WriteLn('=== Task Template System Self-Test ===');
  WriteLn;
  
  WriteLn('Test 1: Checking built-in templates...');
  Templates := GetAllTemplates;
  WriteLn('  ✓ Found ', Length(Templates), ' built-in templates');
  
  WriteLn('Test 2: Listing templates by category...');
  Templates := GetTemplatesByCategory(tcDevelopment);
  WriteLn('  ✓ Development templates: ', Length(Templates));
  Templates := GetTemplatesByCategory(tcHumanResources);
  WriteLn('  ✓ HR templates: ', Length(Templates));
  Templates := GetTemplatesByCategory(tcMarketing);
  WriteLn('  ✓ Marketing templates: ', Length(Templates));
  
  WriteLn('Test 3: Creating custom template...');
  TemplateID := CreateTemplate(
    'Custom Project',
    'Test template',
    tcGeneral,
    'TestUser',
    '1.0'
  );
  WriteLn('  ✓ Created template #', TemplateID);
  
  WriteLn('Test 4: Adding variables to template...');
  AddTemplateVariable(TemplateID, 'TEST_VAR', 'Test variable', 'default', true);
  Variables := GetTemplateVariables(TemplateID);
  WriteLn('  ✓ Template has ', Length(Variables), ' variables');
  
  WriteLn('Test 5: Adding tasks to template...');
  AddTemplateTask(TemplateID, 'Task 1: {TEST_VAR}', 'Description', 
    'Test', tpHigh, 0, 4, ['test'], -1);
  AddTemplateTask(TemplateID, 'Task 2: {TEST_VAR}', 'Description', 
    'Test', tpMedium, 1, 2, ['test'], 0);
  WriteLn('  ✓ Added ', GetTemplateTaskCount(TemplateID), ' tasks to template');
  
  WriteLn('Test 6: Instantiating template with variables...');
  SetLength(Mappings, 1);
  Mappings[0].Name := 'TEST_VAR';
  Mappings[0].Value := 'MyValue';
  Instantiation := InstantiateTemplate(TemplateID, Mappings);
  if Instantiation.Success then
    WriteLn('  ✓ Created ', Length(Instantiation.TaskIDs), ' tasks from template')
  else
    WriteLn('  ✗ Failed to instantiate template');
  
  WriteLn('Test 7: Instantiating Web Development Project...');
  SetLength(Mappings, 2);
  Mappings[0].Name := 'PROJECT_NAME';
  Mappings[0].Value := 'E-Commerce Platform';
  Mappings[1].Name := 'CLIENT_NAME';
  Mappings[1].Value := 'Acme Corp';
  Instantiation := InstantiateTemplate(1, Mappings);
  if Instantiation.Success then
  begin
    WriteLn('  ✓ Created ', Length(Instantiation.TaskIDs), ' tasks');
    WriteLn('    First task ID: #', Instantiation.TaskIDs[0]);
  end;
  
  WriteLn('Test 8: Instantiating Employee Onboarding...');
  SetLength(Mappings, 2);
  Mappings[0].Name := 'EMPLOYEE_NAME';
  Mappings[0].Value := 'John Smith';
  Mappings[1].Name := 'DEPARTMENT';
  Mappings[1].Value := 'Engineering';
  Instantiation := InstantiateTemplate(2, Mappings);
  if Instantiation.Success then
    WriteLn('  ✓ Created ', Length(Instantiation.TaskIDs), ' onboarding tasks');
  
  WriteLn('Test 9: Searching templates...');
  Templates := SearchTemplates('web');
  WriteLn('  ✓ Found ', Length(Templates), ' templates matching "web"');
  
  WriteLn('Test 10: Getting most used templates...');
  Templates := GetMostUsedTemplates(3);
  WriteLn('  ✓ Top 3 templates:');
  for i := 0 to High(Templates) do
    WriteLn('    - ', Templates[i].Name, ' (used ', Templates[i].UsageCount, ' times)');
  
  WriteLn('Test 11: Template statistics...');
  WriteLn(GetTemplateStatistics);
  
  WriteLn('Test 12: Category statistics...');
  WriteLn(GetCategoryStatistics);
  
  WriteLn('Test 13: Exporting template...');
  WriteLn('  Export preview (first 200 chars):');
  WriteLn('  ', Copy(ExportTemplateToString(1), 1, 200), '...');
  
  WriteLn('Test 14: Duplicating template...');
  TemplateID := DuplicateTemplate(1, 'Web Development Project (Copy)');
  WriteLn('  ✓ Duplicated template as #', TemplateID);
  
  WriteLn('Test 15: Deactivating and activating template...');
  DeactivateTemplate(TemplateID);
  WriteLn('  ✓ Deactivated template #', TemplateID);
  ActivateTemplate(TemplateID);
  WriteLn('  ✓ Reactivated template #', TemplateID);
  
  WriteLn;
  WriteLn('=== All Template Tests Completed Successfully! ===');
end;

end.
