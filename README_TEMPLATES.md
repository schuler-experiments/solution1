
# Task Template System

## Overview

The Task Template System is a powerful feature that allows you to create reusable task blueprints with variable substitution, making it easy to quickly instantiate common workflows and project patterns.

## Features

### Core Capabilities

1. **Template Creation**: Define reusable task templates with multiple tasks
2. **Variable Substitution**: Use placeholders like `{PROJECT_NAME}` that get replaced when instantiating
3. **Template Categories**: Organize templates by category (Development, Marketing, HR, Sales, etc.)
4. **Multi-Task Templates**: Create templates that generate multiple related tasks at once
5. **Task Dependencies**: Define dependencies between tasks within a template
6. **Built-in Templates**: Pre-configured templates for common workflows
7. **Template Versioning**: Track template versions and authors
8. **Usage Statistics**: Monitor template usage and success rates

### Template Categories

- **Development**: Software development workflows
- **Marketing**: Marketing campaigns and initiatives
- **Human Resources**: Employee onboarding, training, etc.
- **Sales**: Sales processes and customer acquisition
- **Support**: Customer support workflows
- **Operations**: Operational procedures
- **Finance**: Financial processes
- **Personal**: Personal productivity templates
- **Education**: Learning and training templates
- **Research**: Research project templates
- **General**: Miscellaneous templates

## Built-in Templates

### 1. Web Development Project

Creates a complete web application development workflow with 7 tasks:

**Variables:**
- `PROJECT_NAME` (required): Name of the project
- `CLIENT_NAME` (optional): Client name

**Tasks Created:**
1. Requirements Gathering - Day 0 (8 hours)
2. UI/UX Design - Day 3 (16 hours)
3. Database Schema Design - Day 5 (8 hours)
4. Frontend Development - Day 7 (40 hours)
5. Backend API Development - Day 7 (40 hours)
6. Integration Testing - Day 14 (16 hours)
7. Deployment - Day 21 (8 hours)

**Example Usage:**
```pascal
SetLength(Mappings, 2);
Mappings[0].Name := 'PROJECT_NAME';
Mappings[0].Value := 'E-Commerce Platform';
Mappings[1].Name := 'CLIENT_NAME';
Mappings[1].Value := 'Acme Corp';
Instantiation := Manager.InstantiateTemplate(1, Mappings);
```

### 2. Employee Onboarding

Complete new employee onboarding checklist with 7 tasks:

**Variables:**
- `EMPLOYEE_NAME` (required): Name of new employee
- `DEPARTMENT` (required): Department name
- `START_DATE` (optional): Start date

**Tasks Created:**
1. Prepare workstation - Day -1 (2 hours)
2. Create accounts - Day -1 (1 hour)
3. Welcome meeting - Day 0 (1 hour)
4. HR paperwork - Day 0 (2 hours)
5. Department training - Day 1 (8 hours)
6. Assign mentor - Day 0 (1 hour)
7. 30-day check-in - Day 30 (1 hour)

**Example Usage:**
```pascal
SetLength(Mappings, 2);
Mappings[0].Name := 'EMPLOYEE_NAME';
Mappings[0].Value := 'John Smith';
Mappings[1].Name := 'DEPARTMENT';
Mappings[1].Value := 'Engineering';
Instantiation := Manager.InstantiateTemplate(2, Mappings);
```

### 3. Marketing Campaign Launch

Complete marketing campaign workflow with 7 tasks:

**Variables:**
- `CAMPAIGN_NAME` (required): Campaign name
- `TARGET_AUDIENCE` (optional): Target audience
- `BUDGET` (optional): Campaign budget

**Tasks Created:**
1. Campaign Strategy - Day 0 (4 hours)
2. Content Creation - Day 2 (16 hours)
3. Design Assets - Day 3 (12 hours)
4. Social Media Setup - Day 5 (4 hours)
5. Email Campaign Setup - Day 5 (6 hours)
6. Launch Campaign - Day 7 (2 hours)
7. Monitor & Optimize - Day 8 (8 hours)

### 4. Bug Fix Workflow

Standard process for fixing bugs with 6 tasks:

**Variables:**
- `BUG_ID` (required): Bug tracking ID
- `SEVERITY` (optional): Bug severity

**Tasks Created:**
1. Reproduce bug - Day 0 (1 hour)
2. Root cause analysis - Day 0 (2 hours)
3. Implement fix - Day 1 (4 hours)
4. Test fix - Day 2 (2 hours)
5. Code review - Day 2 (1 hour)
6. Deploy fix - Day 3 (1 hour)

## API Reference

### Template Management

#### CreateTemplate
```pascal
function CreateTemplate(
  const AName, ADescription: string;
  ACategory: TTemplateCategory;
  const AAuthor, AVersion: string
): integer;
```
Creates a new template and returns its ID.

#### DeleteTemplate
```pascal
function DeleteTemplate(ATemplateID: integer): boolean;
```
Deletes a template from the system.

#### UpdateTemplateInfo
```pascal
function UpdateTemplateInfo(
  ATemplateID: integer;
  const AName, ADescription: string
): boolean;
```
Updates template name and description.

#### DuplicateTemplate
```pascal
function DuplicateTemplate(
  ATemplateID: integer;
  const ANewName: string
): integer;
```
Creates a copy of an existing template.

#### ActivateTemplate / DeactivateTemplate
```pascal
function ActivateTemplate(ATemplateID: integer): boolean;
function DeactivateTemplate(ATemplateID: integer): boolean;
```
Enable or disable a template.

### Variable Management

#### AddTemplateVariable
```pascal
function AddTemplateVariable(
  ATemplateID: integer;
  const AName, ADescription, ADefaultValue: string;
  ARequired: boolean
): boolean;
```
Adds a variable to a template.

#### RemoveTemplateVariable
```pascal
function RemoveTemplateVariable(
  ATemplateID: integer;
  const AVariableName: string
): boolean;
```
Removes a variable from a template.

#### GetTemplateVariables
```pascal
function GetTemplateVariables(
  ATemplateID: integer
): TTemplateVariableArray;
```
Returns all variables defined in a template.

### Task Management Within Templates

#### AddTemplateTask
```pascal
function AddTemplateTask(
  ATemplateID: integer;
  const ATitle, ADescription, ACategory: string;
  APriority: TTaskPriority;
  ADaysFromStart: integer;
  AEstimatedHours: double;
  const ATags: array of string;
  ADependsOnIndex: integer
): boolean;
```
Adds a task to a template. `ADaysFromStart` is the offset from instantiation date. `ADependsOnIndex` specifies task dependencies (-1 for none).

#### RemoveTemplateTask
```pascal
function RemoveTemplateTask(
  ATemplateID: integer;
  ATaskIndex: integer
): boolean;
```
Removes a task from a template.

#### GetTemplateTaskCount
```pascal
function GetTemplateTaskCount(ATemplateID: integer): integer;
```
Returns the number of tasks in a template.

### Template Instantiation

#### InstantiateTemplate
```pascal
function InstantiateTemplate(
  ATemplateID: integer;
  const AVariables: TVariableMappingArray
): TTemplateInstantiation;
```
Creates actual tasks from a template with variable substitution.

#### InstantiateTemplateSimple
```pascal
function InstantiateTemplateSimple(
  ATemplateID: integer
): TTemplateInstantiation;
```
Instantiates a template using only default variable values.

#### GetInstantiationHistory
```pascal
function GetInstantiationHistory: TTemplateInstantiationArray;
```
Returns the history of all template instantiations.

#### GetTemplateUsageCount
```pascal
function GetTemplateUsageCount(ATemplateID: integer): integer;
```
Returns how many times a template has been used.

### Template Querying

#### GetAllTemplates
```pascal
function GetAllTemplates: TTaskTemplateArray;
```
Returns all templates in the system.

#### GetActiveTemplates
```pascal
function GetActiveTemplates: TTaskTemplateArray;
```
Returns only active templates.

#### GetTemplatesByCategory
```pascal
function GetTemplatesByCategory(
  ACategory: TTemplateCategory
): TTaskTemplateArray;
```
Returns templates in a specific category.

#### SearchTemplates
```pascal
function SearchTemplates(const ASearchTerm: string): TTaskTemplateArray;
```
Searches templates by name or description.

#### GetTemplateByID
```pascal
function GetTemplateByID(ATemplateID: integer): TTaskTemplate;
```
Returns a specific template by ID.

#### GetMostUsedTemplates
```pascal
function GetMostUsedTemplates(ACount: integer): TTaskTemplateArray;
```
Returns the most frequently used templates.

### Import/Export

#### ExportTemplateToString
```pascal
function ExportTemplateToString(ATemplateID: integer): string;
```
Exports a template to a text format.

#### ImportTemplateFromString
```pascal
function ImportTemplateFromString(const AData: string): integer;
```
Imports a template from text format (TODO: implementation pending).

#### SaveTemplatesToFile / LoadTemplatesFromFile
```pascal
function SaveTemplatesToFile(const AFilename: string): boolean;
function LoadTemplatesFromFile(const AFilename: string): boolean;
```
Save/load all templates to/from a file.

### Statistics and Reporting

#### GetTemplateStatistics
```pascal
function GetTemplateStatistics: string;
```
Returns overall template system statistics.

#### GetCategoryStatistics
```pascal
function GetCategoryStatistics: string;
```
Returns templates grouped by category.

#### UpdateTemplateSuccessRate
```pascal
function UpdateTemplateSuccessRate(
  ATemplateID: integer;
  ASuccessRate: double
): boolean;
```
Updates the success rate of a template (0.0 to 100.0).

## Usage Examples

### Example 1: Create a Custom Template

```pascal
var
  Manager: TTemplateTaskManager;
  TemplateID: integer;
begin
  Manager := TTemplateTaskManager.Create;
  try
    // Create the template
    TemplateID := Manager.CreateTemplate(
      'Code Review Process',
      'Standard code review workflow',
      tcDevelopment,
      'DevTeam',
      '1.0'
    );
    
    // Add variables
    Manager.AddTemplateVariable(TemplateID, 
      'PR_NUMBER', 'Pull request number', '0', true);
    Manager.AddTemplateVariable(TemplateID,
      'REVIEWER', 'Name of reviewer', 'Team', false);
    
    // Add tasks
    Manager.AddTemplateTask(TemplateID,
      'Review PR #{PR_NUMBER}',
      'Code review by {REVIEWER}',
      'Review',
      tpHigh,
      0,  // Same day
      2,  // 2 hours
      ['review', 'code'],
      -1  // No dependency
    );
    
    Manager.AddTemplateTask(TemplateID,
      'Address feedback for PR #{PR_NUMBER}',
      'Fix issues found in review',
      'Development',
      tpHigh,
      1,  // Next day
      4,  // 4 hours
      ['development', 'fix'],
      0   // Depends on first task
    );
    
  finally
    Manager.Free;
  end;
end;
```

### Example 2: Use a Template

```pascal
var
  Manager: TTemplateTaskManager;
  Mappings: TVariableMappingArray;
  Result: TTemplateInstantiation;
begin
  Manager := TTemplateTaskManager.Create;
  try
    // Set up variable values
    SetLength(Mappings, 2);
    Mappings[0].Name := 'PROJECT_NAME';
    Mappings[0].Value := 'Mobile App Redesign';
    Mappings[1].Name := 'CLIENT_NAME';
    Mappings[1].Value := 'TechCorp';
    
    // Instantiate the template
    Result := Manager.InstantiateTemplate(1, Mappings);
    
    if Result.Success then
      WriteLn('Created ', Length(Result.TaskIDs), ' tasks from template');
      
  finally
    Manager.Free;
  end;
end;
```

### Example 3: Find and Use Templates

```pascal
var
  Manager: TTemplateTaskManager;
  Templates: TTaskTemplateArray;
  i: integer;
begin
  Manager := TTemplateTaskManager.Create;
  try
    // Search for templates
    Templates := Manager.SearchTemplates('development');
    
    WriteLn('Found ', Length(Templates), ' development templates:');
    for i := 0 to High(Templates) do
    begin
      WriteLn('  ', Templates[i].Name);
      WriteLn('    Tasks: ', Length(Templates[i].Tasks));
      WriteLn('    Used: ', Templates[i].UsageCount, ' times');
    end;
    
  finally
    Manager.Free;
  end;
end;
```

## Data Structures

### TTemplateVariable
```pascal
type
  TTemplateVariable = record
    Name: string;
    Description: string;
    DefaultValue: string;
    Required: boolean;
  end;
```

### TTemplateTask
```pascal
type
  TTemplateTask = record
    Title: string;
    Description: string;
    Category: string;
    Priority: TTaskPriority;
    DaysFromStart: integer;
    EstimatedHours: double;
    Tags: array of string;
    DependsOnIndex: integer;
  end;
```

### TTaskTemplate
```pascal
type
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
    SuccessRate: double;
    IsActive: boolean;
    Variables: TTemplateVariableArray;
    Tasks: TTemplateTaskArray;
  end;
```

### TTemplateInstantiation
```pascal
type
  TTemplateInstantiation = record
    TemplateID: integer;
    InstantiationDate: TDateTime;
    TaskIDs: array of integer;
    Variables: TVariableMappingArray;
    Success: boolean;
  end;
```

## Best Practices

1. **Use Descriptive Variable Names**: Use clear, uppercase names like `PROJECT_NAME` or `CLIENT_NAME`
2. **Mark Required Variables**: Set `ARequired := true` for variables that must be provided
3. **Set Reasonable Defaults**: Provide sensible default values for optional variables
4. **Document Templates**: Use clear names and descriptions for templates
5. **Plan Task Dependencies**: Use `ADependsOnIndex` to create logical task sequences
6. **Set Realistic Estimates**: Use `DaysFromStart` and `EstimatedHours` based on experience
7. **Use Tags Effectively**: Add relevant tags to help categorize and find tasks later
8. **Version Templates**: Update the version string when making significant changes
9. **Monitor Success Rates**: Track which templates work well and which need improvement
10. **Keep Templates Focused**: Create specific templates rather than overly generic ones

## Integration

The Template System integrates seamlessly with the existing task manager hierarchy:

```
TTaskManager (base)
  ↓
TTaskManagerExt (extended features)
  ↓
TAdvancedTaskManager (reminders, audit, archive)
  ↓
TEnhancedTaskManager (attachments)
  ↓
TTeamTaskManager (team collaboration)
  ↓
... (other layers)
  ↓
TCommentedTaskManager (comments & discussions)
  ↓
TTemplateTaskManager (templates) ← You are here
```

All features from parent classes are available, including:
- Task management (add, update, delete, search)
- Team collaboration
- Comments and discussions
- File attachments
- Reminders and notifications
- Audit trails
- And much more!

## Future Enhancements

Potential future additions to the template system:

1. **Template Marketplace**: Share templates with other users
2. **Template Analytics**: Track success rates and completion metrics
3. **Smart Template Suggestions**: AI-powered template recommendations
4. **Template Inheritance**: Create templates based on other templates
5. **Conditional Logic**: Add if/then rules for task creation
6. **Dynamic Task Count**: Create variable numbers of tasks based on parameters
7. **Template Validation**: Check for logical errors in template definitions
8. **Template Previews**: Preview what tasks will be created before instantiation
9. **Bulk Template Operations**: Apply templates to multiple projects at once
10. **Template Libraries**: Import/export template collections

## Conclusion

The Task Template System provides a powerful way to standardize and accelerate common workflows. By creating reusable templates with variable substitution, you can quickly instantiate complex task structures while maintaining consistency and best practices across your projects.

For more information about other features, see:
- [README.md](README.md) - Core task management
- [README_EXTENDED.md](README_EXTENDED.md) - Extended features
- [README_COMMENTS.md](README_COMMENTS.md) - Comments system
- [TEAM_FEATURES.md](TEAM_FEATURES.md) - Team collaboration
