
unit TaskValidation;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, TaskTypes;

type
  { Validation result record }
  TValidationResult = record
    isValid: boolean;
    errorMessage: string;
    errorCode: integer;
  end;

  { Task validation and constraint checking }
  TTaskValidatorClass = class
  public
    constructor Create();
    destructor Destroy();
    function ValidateTaskTitle(title: string): TValidationResult;
    function ValidateTaskDescription(description: string): TValidationResult;
    function ValidateDueDate(dueDate: TDateTime; createdDate: TDateTime): TValidationResult;
    function ValidateTaskData(title, description: string; priority: TTaskPriority; dueDate: TDateTime): TValidationResult;
    function ValidatePriority(priority: TTaskPriority): TValidationResult;
    function ValidateAssignee(assignee: string): TValidationResult;
    function ValidateTimeEstimate(hours: double): TValidationResult;
    function ValidateCategory(category: string): TValidationResult;
    function CanDeleteTask(hasDependents: boolean; status: TTaskStatus): TValidationResult;
    function CanChangeStatus(oldStatus, newStatus: TTaskStatus): TValidationResult;
    procedure SelfTest();
  end;

implementation

{ ===== TTaskValidatorClass ===== }

constructor TTaskValidatorClass.Create();
begin
  inherited Create();
end;

destructor TTaskValidatorClass.Destroy();
begin
  inherited Destroy();
end;

function TTaskValidatorClass.ValidateTaskTitle(title: string): TValidationResult;
begin
  result.isValid := true;
  result.errorCode := 0;
  result.errorMessage := '';

  if Length(Trim(title)) = 0 then
  begin
    result.isValid := false;
    result.errorCode := 101;
    result.errorMessage := 'Task title cannot be empty';
  end
  else if Length(title) > 255 then
  begin
    result.isValid := false;
    result.errorCode := 102;
    result.errorMessage := 'Task title exceeds maximum length (255 characters)';
  end;
end;

function TTaskValidatorClass.ValidateTaskDescription(description: string): TValidationResult;
begin
  result.isValid := true;
  result.errorCode := 0;
  result.errorMessage := '';

  if Length(description) > 2000 then
  begin
    result.isValid := false;
    result.errorCode := 201;
    result.errorMessage := 'Task description exceeds maximum length (2000 characters)';
  end;
end;

function TTaskValidatorClass.ValidateDueDate(dueDate: TDateTime; createdDate: TDateTime): TValidationResult;
begin
  result.isValid := true;
  result.errorCode := 0;
  result.errorMessage := '';

  if dueDate < createdDate then
  begin
    result.isValid := false;
    result.errorCode := 301;
    result.errorMessage := 'Due date cannot be before creation date';
  end
  else if dueDate > (createdDate + 3650) then
  begin
    result.isValid := false;
    result.errorCode := 302;
    result.errorMessage := 'Due date too far in future (max 10 years)';
  end;
end;

function TTaskValidatorClass.ValidateTaskData(title, description: string; priority: TTaskPriority; dueDate: TDateTime): TValidationResult;
var
  titleResult, descResult, priorityResult, dateResult: TValidationResult;
begin
  { Validate title }
  titleResult := ValidateTaskTitle(title);
  if not titleResult.isValid then
  begin
    result := titleResult;
    exit;
  end;

  { Validate description }
  descResult := ValidateTaskDescription(description);
  if not descResult.isValid then
  begin
    result := descResult;
    exit;
  end;

  { Validate priority }
  priorityResult := ValidatePriority(priority);
  if not priorityResult.isValid then
  begin
    result := priorityResult;
    exit;
  end;

  { Validate due date }
  dateResult := ValidateDueDate(dueDate, Now());
  if not dateResult.isValid then
  begin
    result := dateResult;
    exit;
  end;

  { All validations passed }
  result.isValid := true;
  result.errorCode := 0;
  result.errorMessage := '';
end;

function TTaskValidatorClass.ValidatePriority(priority: TTaskPriority): TValidationResult;
begin
  result.isValid := true;
  result.errorCode := 0;
  result.errorMessage := '';
end;

function TTaskValidatorClass.ValidateAssignee(assignee: string): TValidationResult;
begin
  result.isValid := true;
  result.errorCode := 0;
  result.errorMessage := '';

  if Length(assignee) > 100 then
  begin
    result.isValid := false;
    result.errorCode := 501;
    result.errorMessage := 'Assignee name exceeds maximum length (100 characters)';
  end;
end;

function TTaskValidatorClass.ValidateTimeEstimate(hours: double): TValidationResult;
begin
  result.isValid := true;
  result.errorCode := 0;
  result.errorMessage := '';

  if hours < 0 then
  begin
    result.isValid := false;
    result.errorCode := 601;
    result.errorMessage := 'Time estimate cannot be negative';
  end
  else if hours > 1000 then
  begin
    result.isValid := false;
    result.errorCode := 602;
    result.errorMessage := 'Time estimate exceeds maximum (1000 hours)';
  end;
end;

function TTaskValidatorClass.ValidateCategory(category: string): TValidationResult;
begin
  result.isValid := true;
  result.errorCode := 0;
  result.errorMessage := '';

  if Length(category) > 50 then
  begin
    result.isValid := false;
    result.errorCode := 701;
    result.errorMessage := 'Category name exceeds maximum length (50 characters)';
  end;
end;

function TTaskValidatorClass.CanDeleteTask(hasDependents: boolean; status: TTaskStatus): TValidationResult;
begin
  result.isValid := true;
  result.errorCode := 0;
  result.errorMessage := '';

  if hasDependents then
  begin
    result.isValid := false;
    result.errorCode := 1001;
    result.errorMessage := 'Cannot delete task that has dependent tasks';
  end
  else if status = tsInProgress then
  begin
    result.isValid := false;
    result.errorCode := 1002;
    result.errorMessage := 'Cannot delete task that is in progress';
  end;
end;

function TTaskValidatorClass.CanChangeStatus(oldStatus, newStatus: TTaskStatus): TValidationResult;
begin
  result.isValid := true;
  result.errorCode := 0;
  result.errorMessage := '';

  if oldStatus = tsCompleted then
  begin
    result.isValid := false;
    result.errorCode := 1101;
    result.errorMessage := 'Cannot change status of completed task';
  end;
end;

procedure TTaskValidatorClass.SelfTest();
var
  validator: TTaskValidatorClass;
  result: TValidationResult;
begin
  WriteLn('=== TaskValidation Validator Self Test ===');
  validator := TTaskValidatorClass.Create();

  { Test valid title }
  result := validator.ValidateTaskTitle('Write documentation');
  WriteLn(Format('Valid title test: %s', [BoolToStr(result.isValid, true)]));

  { Test invalid title (empty) }
  result := validator.ValidateTaskTitle('');
  WriteLn(Format('Empty title validation: %s (code: %d)', [BoolToStr(not result.isValid, true), result.errorCode]));

  { Test long title }
  result := validator.ValidateTaskTitle(StringOfChar('A', 300));
  WriteLn(Format('Too long title validation: %s', [BoolToStr(not result.isValid, true)]));

  { Test priority validation }
  result := validator.ValidatePriority(tpHigh);
  WriteLn(Format('Valid priority test: %s', [BoolToStr(result.isValid, true)]));

  { Test assignee validation }
  result := validator.ValidateAssignee('John Doe');
  WriteLn(Format('Valid assignee test: %s', [BoolToStr(result.isValid, true)]));

  { Test time estimate validation }
  result := validator.ValidateTimeEstimate(8.5);
  WriteLn(Format('Valid time estimate test: %s', [BoolToStr(result.isValid, true)]));

  { Test invalid time estimate }
  result := validator.ValidateTimeEstimate(-5);
  WriteLn(Format('Negative time estimate validation: %s', [BoolToStr(not result.isValid, true)]));

  { Test status change validation }
  result := validator.CanChangeStatus(tsCompleted, tsInProgress);
  WriteLn(Format('Cannot change completed task status: %s', [BoolToStr(not result.isValid, true)]));

  { Test delete task validation }
  result := validator.CanDeleteTask(true, tsNotStarted);
  WriteLn(Format('Cannot delete task with dependents: %s', [BoolToStr(not result.isValid, true)]));

  validator.Destroy();
  WriteLn('=== TaskValidation Validator Self Test Complete ===');
end;

end.
