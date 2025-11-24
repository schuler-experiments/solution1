
unit TaskTemplates;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, TaskTypes;

type
  { Template field configuration }
  TTemplateField = record
    fieldName: string;
    fieldType: string;
    defaultValue: string;
    isRequired: boolean;
  end;

  TTemplateFieldArray = array of TTemplateField;

  { Task template definition }
  TTaskTemplate = record
    templateId: integer;
    templateName: string;
    description: string;
    basePriority: TTaskPriority;
    defaultAssignee: string;
    estimatedHours: double;
    category: string;
    customFields: TTemplateFieldArray;
    createdDate: TDateTime;
    usageCount: integer;
  end;

  TTaskTemplateArray = array of TTaskTemplate;

  { Template-based task instance }
  TTemplateInstance = record
    instanceId: integer;
    templateId: integer;
    createdTaskId: integer;
    creationDate: TDateTime;
  end;

  TTemplateInstanceArray = array of TTemplateInstance;

  { Template manager class }
  TTaskTemplateManagerClass = class
  private
    templates: TTaskTemplateArray;
    instances: TTemplateInstanceArray;
    nextTemplateId: integer;
    nextInstanceId: integer;
    function FindTemplateIndex(templateId: integer): integer;
    function FindInstanceIndex(instanceId: integer): integer;
  public
    constructor Create();
    destructor Destroy(); override;

    { Template management }
    function CreateTemplate(name, description: string; priority: TTaskPriority;
                           assignee: string; estimatedHours: double;
                           category: string): integer;
    function GetTemplate(templateId: integer): TTaskTemplate;
    function GetAllTemplates(): TTaskTemplateArray;
    function DeleteTemplate(templateId: integer): boolean;
    function UpdateTemplate(templateId: integer; newTemplate: TTaskTemplate): boolean;

    { Template field management }
    function AddFieldToTemplate(templateId: integer; fieldName, fieldType: string;
                               defaultValue: string; isRequired: boolean): boolean;
    function RemoveFieldFromTemplate(templateId: integer; fieldName: string): boolean;
    function GetTemplateFields(templateId: integer): TTemplateFieldArray;

    { Template instantiation }
    function CreateTaskFromTemplate(templateId: integer; title: string;
                                   dueDate: TDateTime; taskId: integer): integer;
    function GetInstance(instanceId: integer): TTemplateInstance;
    function GetInstancesForTemplate(templateId: integer): TTemplateInstanceArray;
    function GetAllInstances(): TTemplateInstanceArray;

    { Template analysis }
    function GetTemplateUsageCount(templateId: integer): integer;
    function GetMostUsedTemplate(): integer;
    function GetTemplatesByCategory(category: string): TTaskTemplateArray;
    function SearchTemplates(keyword: string): TTaskTemplateArray;
    function GetTemplateStatistics(): string;

    { Template cloning }
    function CloneTemplate(sourceTemplateId: integer; newName: string): integer;

    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TTaskTemplateManagerClass.Create();
begin
  inherited Create();
  SetLength(templates, 0);
  SetLength(instances, 0);
  nextTemplateId := 1;
  nextInstanceId := 1;
end;

destructor TTaskTemplateManagerClass.Destroy();
var
  i: integer;
begin
  for i := 0 to Length(templates) - 1 do
    SetLength(templates[i].customFields, 0);
  SetLength(templates, 0);
  SetLength(instances, 0);
  inherited Destroy();
end;

function TTaskTemplateManagerClass.FindTemplateIndex(templateId: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(templates) - 1 do
  begin
    if templates[i].templateId = templateId then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TTaskTemplateManagerClass.FindInstanceIndex(instanceId: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(instances) - 1 do
  begin
    if instances[i].instanceId = instanceId then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TTaskTemplateManagerClass.CreateTemplate(name, description: string;
                                                 priority: TTaskPriority;
                                                 assignee: string; estimatedHours: double;
                                                 category: string): integer;
var
  newTemplate: TTaskTemplate;
begin
  newTemplate.templateId := nextTemplateId;
  newTemplate.templateName := name;
  newTemplate.description := description;
  newTemplate.basePriority := priority;
  newTemplate.defaultAssignee := assignee;
  newTemplate.estimatedHours := estimatedHours;
  newTemplate.category := category;
  SetLength(newTemplate.customFields, 0);
  newTemplate.createdDate := Now;
  newTemplate.usageCount := 0;

  SetLength(templates, Length(templates) + 1);
  templates[Length(templates) - 1] := newTemplate;

  result := nextTemplateId;
  Inc(nextTemplateId);
end;

function TTaskTemplateManagerClass.GetTemplate(templateId: integer): TTaskTemplate;
var
  idx: integer;
begin
  idx := FindTemplateIndex(templateId);
  if idx >= 0 then
    result := templates[idx]
  else
  begin
    result.templateId := -1;
    SetLength(result.customFields, 0);
  end;
end;

function TTaskTemplateManagerClass.GetAllTemplates(): TTaskTemplateArray;
begin
  result := Copy(templates, 0, Length(templates));
end;

function TTaskTemplateManagerClass.DeleteTemplate(templateId: integer): boolean;
var
  idx, i: integer;
begin
  result := False;
  idx := FindTemplateIndex(templateId);
  if idx >= 0 then
  begin
    SetLength(templates[idx].customFields, 0);
    for i := idx to Length(templates) - 2 do
      templates[i] := templates[i + 1];
    SetLength(templates, Length(templates) - 1);
    result := True;
  end;
end;

function TTaskTemplateManagerClass.UpdateTemplate(templateId: integer;
                                                 newTemplate: TTaskTemplate): boolean;
var
  idx: integer;
begin
  idx := FindTemplateIndex(templateId);
  if idx >= 0 then
  begin
    SetLength(templates[idx].customFields, 0);
    templates[idx] := newTemplate;
    result := True;
  end
  else
    result := False;
end;

function TTaskTemplateManagerClass.AddFieldToTemplate(templateId: integer;
                                                     fieldName, fieldType: string;
                                                     defaultValue: string;
                                                     isRequired: boolean): boolean;
var
  idx: integer;
  newField: TTemplateField;
begin
  idx := FindTemplateIndex(templateId);
  if idx < 0 then
  begin
    result := False;
    Exit;
  end;

  newField.fieldName := fieldName;
  newField.fieldType := fieldType;
  newField.defaultValue := defaultValue;
  newField.isRequired := isRequired;

  SetLength(templates[idx].customFields, Length(templates[idx].customFields) + 1);
  templates[idx].customFields[Length(templates[idx].customFields) - 1] := newField;
  result := True;
end;

function TTaskTemplateManagerClass.RemoveFieldFromTemplate(templateId: integer;
                                                          fieldName: string): boolean;
var
  idx, i, fieldIdx: integer;
begin
  result := False;
  idx := FindTemplateIndex(templateId);
  if idx < 0 then
    Exit;

  fieldIdx := -1;
  for i := 0 to Length(templates[idx].customFields) - 1 do
  begin
    if templates[idx].customFields[i].fieldName = fieldName then
    begin
      fieldIdx := i;
      Break;
    end;
  end;

  if fieldIdx >= 0 then
  begin
    for i := fieldIdx to Length(templates[idx].customFields) - 2 do
      templates[idx].customFields[i] := templates[idx].customFields[i + 1];
    SetLength(templates[idx].customFields, Length(templates[idx].customFields) - 1);
    result := True;
  end;
end;

function TTaskTemplateManagerClass.GetTemplateFields(templateId: integer): TTemplateFieldArray;
var
  idx: integer;
begin
  SetLength(result, 0);
  idx := FindTemplateIndex(templateId);
  if idx >= 0 then
    result := Copy(templates[idx].customFields, 0, Length(templates[idx].customFields));
end;

function TTaskTemplateManagerClass.CreateTaskFromTemplate(templateId: integer;
                                                         title: string;
                                                         dueDate: TDateTime;
                                                         taskId: integer): integer;
var
  idx: integer;
  newInst: TTemplateInstance;
begin
  idx := FindTemplateIndex(templateId);
  if idx < 0 then
  begin
    result := -1;
    Exit;
  end;

  newInst.instanceId := nextInstanceId;
  newInst.templateId := templateId;
  newInst.createdTaskId := taskId;
  newInst.creationDate := Now;

  SetLength(instances, Length(instances) + 1);
  instances[Length(instances) - 1] := newInst;

  Inc(templates[idx].usageCount);
  result := nextInstanceId;
  Inc(nextInstanceId);
end;

function TTaskTemplateManagerClass.GetInstance(instanceId: integer): TTemplateInstance;
var
  idx: integer;
begin
  idx := FindInstanceIndex(instanceId);
  if idx >= 0 then
    result := instances[idx]
  else
  begin
    result.instanceId := -1;
    result.templateId := -1;
  end;
end;

function TTaskTemplateManagerClass.GetInstancesForTemplate(templateId: integer): TTemplateInstanceArray;
var
  i: integer;
begin
  SetLength(result, 0);
  for i := 0 to Length(instances) - 1 do
  begin
    if instances[i].templateId = templateId then
    begin
      SetLength(result, Length(result) + 1);
      result[Length(result) - 1] := instances[i];
    end;
  end;
end;

function TTaskTemplateManagerClass.GetAllInstances(): TTemplateInstanceArray;
begin
  result := Copy(instances, 0, Length(instances));
end;

function TTaskTemplateManagerClass.GetTemplateUsageCount(templateId: integer): integer;
var
  idx: integer;
begin
  result := 0;
  idx := FindTemplateIndex(templateId);
  if idx >= 0 then
    result := templates[idx].usageCount;
end;

function TTaskTemplateManagerClass.GetMostUsedTemplate(): integer;
var
  i: integer;
  maxCount: integer;
begin
  result := -1;
  maxCount := 0;
  for i := 0 to Length(templates) - 1 do
  begin
    if templates[i].usageCount > maxCount then
    begin
      maxCount := templates[i].usageCount;
      result := templates[i].templateId;
    end;
  end;
end;

function TTaskTemplateManagerClass.GetTemplatesByCategory(category: string): TTaskTemplateArray;
var
  i: integer;
begin
  SetLength(result, 0);
  for i := 0 to Length(templates) - 1 do
  begin
    if templates[i].category = category then
    begin
      SetLength(result, Length(result) + 1);
      result[Length(result) - 1] := templates[i];
    end;
  end;
end;

function TTaskTemplateManagerClass.SearchTemplates(keyword: string): TTaskTemplateArray;
var
  i: integer;
  keyLower: string;
begin
  SetLength(result, 0);
  keyLower := LowerCase(keyword);

  for i := 0 to Length(templates) - 1 do
  begin
    if (Pos(keyLower, LowerCase(templates[i].templateName)) > 0) or
       (Pos(keyLower, LowerCase(templates[i].description)) > 0) then
    begin
      SetLength(result, Length(result) + 1);
      result[Length(result) - 1] := templates[i];
    end;
  end;
end;

function TTaskTemplateManagerClass.GetTemplateStatistics(): string;
var
  i: integer;
  totalUsage: integer;
  avgUsage: double;
begin
  result := 'Template Statistics:' + #10#13;
  result := result + 'Total Templates: ' + IntToStr(Length(templates)) + #10#13;
  result := result + 'Total Instances: ' + IntToStr(Length(instances)) + #10#13;

  totalUsage := 0;
  for i := 0 to Length(templates) - 1 do
    totalUsage := totalUsage + templates[i].usageCount;

  if Length(templates) > 0 then
    avgUsage := totalUsage / Length(templates)
  else
    avgUsage := 0;

  result := result + 'Average Usage: ' + FormatFloat('0.00', avgUsage) + #10#13;
  result := result + 'Most Used: ';
  if GetMostUsedTemplate() >= 0 then
    result := result + GetTemplate(GetMostUsedTemplate()).templateName
  else
    result := result + 'None';
end;

function TTaskTemplateManagerClass.CloneTemplate(sourceTemplateId: integer;
                                                newName: string): integer;
var
  idx: integer;
  sourceTemplate: TTaskTemplate;
  newTemplate: TTaskTemplate;
  i: integer;
begin
  result := -1;
  idx := FindTemplateIndex(sourceTemplateId);
  if idx < 0 then
    Exit;

  sourceTemplate := templates[idx];
  newTemplate := sourceTemplate;
  newTemplate.templateId := nextTemplateId;
  newTemplate.templateName := newName;
  newTemplate.createdDate := Now;
  newTemplate.usageCount := 0;

  SetLength(newTemplate.customFields, Length(sourceTemplate.customFields));
  for i := 0 to Length(sourceTemplate.customFields) - 1 do
    newTemplate.customFields[i] := sourceTemplate.customFields[i];

  SetLength(templates, Length(templates) + 1);
  templates[Length(templates) - 1] := newTemplate;

  result := nextTemplateId;
  Inc(nextTemplateId);
end;

procedure TTaskTemplateManagerClass.SelfTest();
var
  mgr: TTaskTemplateManagerClass;
  template1, template2, template3: integer;
  allTemplates: TTaskTemplateArray;
  searchResults: TTaskTemplateArray;
begin
  WriteLn('');
  WriteLn('=== TaskTemplates Self Test ===');

  mgr := TTaskTemplateManagerClass.Create();

  WriteLn('Creating templates...');
  template1 := mgr.CreateTemplate('Bug Fix', 'Standard bug fix template',
                                  tpHigh, 'Developer', 4.0, 'Maintenance');
  template2 := mgr.CreateTemplate('Feature', 'Standard feature template',
                                  tpMedium, 'Developer', 8.0, 'Development');
  template3 := mgr.CreateTemplate('Documentation', 'Standard doc template',
                                  tpLow, 'Writer', 2.0, 'Documentation');
  WriteLn('Total templates created: ', Length(mgr.GetAllTemplates()));

  WriteLn('');
  WriteLn('Adding custom fields to template 1...');
  mgr.AddFieldToTemplate(template1, 'severity', 'string', 'medium', True);
  mgr.AddFieldToTemplate(template1, 'affectedUsers', 'integer', '0', False);
  WriteLn('Fields added to template');

  WriteLn('');
  WriteLn('Creating instances from templates...');
  mgr.CreateTaskFromTemplate(template1, 'Fix login button', Now + 3, 101);
  mgr.CreateTaskFromTemplate(template1, 'Fix search function', Now + 5, 102);
  mgr.CreateTaskFromTemplate(template2, 'Add export feature', Now + 14, 103);
  WriteLn('Total instances: ', Length(mgr.GetAllInstances()));

  WriteLn('');
  WriteLn('Template usage:');
  WriteLn('  Bug Fix: ', mgr.GetTemplateUsageCount(template1), ' uses');
  WriteLn('  Feature: ', mgr.GetTemplateUsageCount(template2), ' uses');
  WriteLn('  Documentation: ', mgr.GetTemplateUsageCount(template3), ' uses');

  WriteLn('');
  WriteLn('Most used template ID: ', mgr.GetMostUsedTemplate());

  WriteLn('');
  WriteLn('Searching templates with keyword "Fix"...');
  searchResults := mgr.SearchTemplates('Fix');
  WriteLn('Found: ', Length(searchResults), ' templates');

  WriteLn('');
  WriteLn('Getting templates by category "Development"...');
  allTemplates := mgr.GetTemplatesByCategory('Development');
  WriteLn('Found: ', Length(allTemplates), ' templates');

  WriteLn('');
  WriteLn('Cloning template...');
  mgr.CloneTemplate(template1, 'Urgent Bug Fix');
  WriteLn('New template created');

  WriteLn('');
  WriteLn(mgr.GetTemplateStatistics());

  mgr.Free();
  SetLength(allTemplates, 0);
  SetLength(searchResults, 0);

  WriteLn('=== TaskTemplates Self Test Complete ===');
  WriteLn('');
end;

end.
