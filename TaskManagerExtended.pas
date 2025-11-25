unit TaskManagerExtended;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager;

type
  TTaskManagerExtended = class(TTaskManager)
  private
    templates: TTemplateArray;
    milestones: TMilestoneArray;
    nextTemplateId: integer;
    nextMilestoneId: integer;

    function findTemplateIndex(id: integer): integer;
    function findMilestoneIndex(id: integer): integer;

  public
    constructor Create;
    destructor Destroy; override;

    // Task Notes/Comments management
    function addTaskNote(taskId: integer; const noteText: string; isImportant: boolean): boolean;
    function removeTaskNote(taskId: integer; noteIndex: integer): boolean;
    function getTaskNotes(taskId: integer): TNoteArray;
    function getTaskNoteCount(taskId: integer): integer;
    function getImportantNotes: TTaskArray;

    // Task assignment
    function assignTaskTo(taskId: integer; const assignee: string): boolean;
    function getTaskAssignment(taskId: integer): string;
    function getTasksAssignedTo(const assignee: string): TTaskArray;

    // Milestone management
    function addMilestone(const name, description: string; targetDate: TDateTime): integer;
    function deleteMilestone(milestoneId: integer): boolean;
    function assignTaskToMilestone(taskId: integer; milestoneId: integer): boolean;
    function getTasksByMilestone(milestoneId: integer): TTaskArray;
    function getMilestoneProgress(milestoneId: integer): TTaskStats;
    function getAllMilestones: TMilestoneArray;
    function setMilestoneStatus(milestoneId: integer; newStatus: TTaskStatus): boolean;

    // Task Templates
    function createTemplate(const name, description, category: string;
                           priority: TTaskPriority; estimatedHours: double): integer;
    function deleteTemplate(templateId: integer): boolean;
    function getTemplate(templateId: integer; var template: TTaskTemplate): boolean;
    function getAllTemplates: TTemplateArray;
    function createTaskFromTemplate(templateId: integer; const taskName: string;
                                   dueDate: TDateTime): integer;
    function addTagToTemplate(templateId: integer; const tag: string): boolean;
  end;

implementation

constructor TTaskManagerExtended.Create;
begin
  inherited Create;
  setLength(templates, 0);
  setLength(milestones, 0);
  nextTemplateId := 1;
  nextMilestoneId := 1;
end;

destructor TTaskManagerExtended.Destroy;
begin
  setLength(templates, 0);
  setLength(milestones, 0);
  inherited Destroy;
end;

function TTaskManagerExtended.findTemplateIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to length(templates) - 1 do
    if templates[i].id = id then
      begin
        result := i;
        exit;
      end;
end;

function TTaskManagerExtended.findMilestoneIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to length(milestones) - 1 do
    if milestones[i].id = id then
      begin
        result := i;
        exit;
      end;
end;

function TTaskManagerExtended.addTaskNote(taskId: integer; const noteText: string;
                                         isImportant: boolean): boolean;
var
  idx, noteCount: integer;
  task: TTask;
  newNote: TTaskNote;
begin
  clearError;
  idx := findTaskIndex(taskId);
  if idx < 0 then
    begin
      lastError := 'Task not found: ' + intToStr(taskId);
      result := false;
      exit;
    end;

  task := tasks[idx];
  noteCount := length(task.notes);
  if noteCount >= maxNotesPerTask then
    begin
      lastError := 'Maximum notes per task exceeded';
      result := false;
      exit;
    end;

  newNote.text := noteText;
  newNote.createdDate := now;
  newNote.isImportant := isImportant;

  setLength(task.notes, noteCount + 1);
  task.notes[noteCount] := newNote;
  tasks[idx] := task;
  result := true;
end;

function TTaskManagerExtended.removeTaskNote(taskId: integer; noteIndex: integer): boolean;
var
  idx, i, noteCount: integer;
  task: TTask;
begin
  clearError;
  idx := findTaskIndex(taskId);
  if idx < 0 then
    begin
      lastError := 'Task not found: ' + intToStr(taskId);
      result := false;
      exit;
    end;

  task := tasks[idx];
  noteCount := length(task.notes);
  if (noteIndex < 0) or (noteIndex >= noteCount) then
    begin
      lastError := 'Invalid note index';
      result := false;
      exit;
    end;

  for i := noteIndex to noteCount - 2 do
    task.notes[i] := task.notes[i + 1];
  setLength(task.notes, noteCount - 1);
  tasks[idx] := task;
  result := true;
end;

function TTaskManagerExtended.getTaskNotes(taskId: integer): TNoteArray;
var
  idx: integer;
begin
  setLength(result, 0);
  idx := findTaskIndex(taskId);
  if idx >= 0 then
    result := copy(tasks[idx].notes, 0, length(tasks[idx].notes))
  else
    lastError := 'Task not found: ' + intToStr(taskId);
end;

function TTaskManagerExtended.getTaskNoteCount(taskId: integer): integer;
var
  idx: integer;
begin
  result := 0;
  idx := findTaskIndex(taskId);
  if idx >= 0 then
    result := length(tasks[idx].notes);
end;

function TTaskManagerExtended.getImportantNotes: TTaskArray;
var
  i, j, count: integer;
begin
  setLength(result, 0);
  count := 0;

  for i := 0 to length(tasks) - 1 do
    for j := 0 to length(tasks[i].notes) - 1 do
      if tasks[i].notes[j].isImportant then
        begin
          setLength(result, count + 1);
          result[count] := tasks[i];
          inc(count);
          break;
        end;
end;

function TTaskManagerExtended.assignTaskTo(taskId: integer; const assignee: string): boolean;
var
  idx: integer;
  task: TTask;
begin
  clearError;
  idx := findTaskIndex(taskId);
  if idx < 0 then
    begin
      lastError := 'Task not found: ' + intToStr(taskId);
      result := false;
      exit;
    end;

  task := tasks[idx];
  task.assignedTo := assignee;
  tasks[idx] := task;
  result := true;
end;

function TTaskManagerExtended.getTaskAssignment(taskId: integer): string;
var
  idx: integer;
begin
  result := '';
  idx := findTaskIndex(taskId);
  if idx >= 0 then
    result := tasks[idx].assignedTo;
end;

function TTaskManagerExtended.getTasksAssignedTo(const assignee: string): TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;

  for i := 0 to length(tasks) - 1 do
    if tasks[i].assignedTo = assignee then
      begin
        setLength(result, count + 1);
        result[count] := tasks[i];
        inc(count);
      end;
end;

function TTaskManagerExtended.addMilestone(const name, description: string;
                                          targetDate: TDateTime): integer;
var
  newMilestone: TMilestone;
begin
  clearError;

  if length(name) = 0 then
    begin
      lastError := 'Milestone name cannot be empty';
      result := -1;
      exit;
    end;

  newMilestone.id := nextMilestoneId;
  inc(nextMilestoneId);
  newMilestone.name := name;
  newMilestone.description := description;
  newMilestone.targetDate := targetDate;
  newMilestone.status := tsNew;
  newMilestone.createdDate := now;

  setLength(milestones, length(milestones) + 1);
  milestones[length(milestones) - 1] := newMilestone;

  result := newMilestone.id;
end;

function TTaskManagerExtended.deleteMilestone(milestoneId: integer): boolean;
var
  idx, i: integer;
begin
  clearError;
  idx := findMilestoneIndex(milestoneId);
  if idx >= 0 then
    begin
      for i := idx to length(milestones) - 2 do
        milestones[i] := milestones[i + 1];
      setLength(milestones, length(milestones) - 1);
      result := true;
    end
  else
    begin
      lastError := 'Milestone not found: ' + intToStr(milestoneId);
      result := false;
    end;
end;

function TTaskManagerExtended.assignTaskToMilestone(taskId: integer;
                                                   milestoneId: integer): boolean;
var
  idx: integer;
  task: TTask;
begin
  clearError;
  idx := findTaskIndex(taskId);
  if idx < 0 then
    begin
      lastError := 'Task not found: ' + intToStr(taskId);
      result := false;
      exit;
    end;

  if findMilestoneIndex(milestoneId) < 0 then
    begin
      lastError := 'Milestone not found: ' + intToStr(milestoneId);
      result := false;
      exit;
    end;

  task := tasks[idx];
  task.milestoneId := milestoneId;
  tasks[idx] := task;
  result := true;
end;

function TTaskManagerExtended.getTasksByMilestone(milestoneId: integer): TTaskArray;
var
  i, count: integer;
begin
  setLength(result, 0);
  count := 0;

  for i := 0 to length(tasks) - 1 do
    if tasks[i].milestoneId = milestoneId then
      begin
        setLength(result, count + 1);
        result[count] := tasks[i];
        inc(count);
      end;
end;

function TTaskManagerExtended.getMilestoneProgress(milestoneId: integer): TTaskStats;
var
  milestoneTasks: TTaskArray;
  i: integer;
begin
  fillChar(result, sizeof(result), 0);
  milestoneTasks := getTasksByMilestone(milestoneId);
  result.totalTasks := length(milestoneTasks);

  for i := 0 to length(milestoneTasks) - 1 do
    begin
      case milestoneTasks[i].status of
        tsNew: inc(result.newTasks);
        tsInProgress: inc(result.inProgressTasks);
        tsBlocked: inc(result.blockedTasks);
        tsCompleted: inc(result.completedTasks);
        tsCancelled: inc(result.cancelledTasks);
      end;
    end;
end;

function TTaskManagerExtended.getAllMilestones: TMilestoneArray;
begin
  result := copy(milestones, 0, length(milestones));
end;

function TTaskManagerExtended.setMilestoneStatus(milestoneId: integer;
                                                newStatus: TTaskStatus): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findMilestoneIndex(milestoneId);
  if idx >= 0 then
    begin
      milestones[idx].status := newStatus;
      result := true;
    end
  else
    begin
      lastError := 'Milestone not found: ' + intToStr(milestoneId);
      result := false;
    end;
end;

function TTaskManagerExtended.createTemplate(const name, description, category: string;
                                            priority: TTaskPriority;
                                            estimatedHours: double): integer;
var
  newTemplate: TTaskTemplate;
begin
  clearError;

  if length(name) = 0 then
    begin
      lastError := 'Template name cannot be empty';
      result := -1;
      exit;
    end;

  newTemplate.id := nextTemplateId;
  inc(nextTemplateId);
  newTemplate.name := name;
  newTemplate.description := description;
  newTemplate.category := category;
  newTemplate.priority := priority;
  newTemplate.estimatedHours := estimatedHours;
  setLength(newTemplate.tags, 0);

  setLength(templates, length(templates) + 1);
  templates[length(templates) - 1] := newTemplate;

  result := newTemplate.id;
end;

function TTaskManagerExtended.deleteTemplate(templateId: integer): boolean;
var
  idx, i: integer;
begin
  clearError;
  idx := findTemplateIndex(templateId);
  if idx >= 0 then
    begin
      for i := idx to length(templates) - 2 do
        templates[i] := templates[i + 1];
      setLength(templates, length(templates) - 1);
      result := true;
    end
  else
    begin
      lastError := 'Template not found: ' + intToStr(templateId);
      result := false;
    end;
end;

function TTaskManagerExtended.getTemplate(templateId: integer;
                                         var template: TTaskTemplate): boolean;
var
  idx: integer;
begin
  clearError;
  idx := findTemplateIndex(templateId);
  if idx >= 0 then
    begin
      template := templates[idx];
      result := true;
    end
  else
    begin
      lastError := 'Template not found: ' + intToStr(templateId);
      result := false;
    end;
end;

function TTaskManagerExtended.getAllTemplates: TTemplateArray;
begin
  result := copy(templates, 0, length(templates));
end;

function TTaskManagerExtended.createTaskFromTemplate(templateId: integer;
                                                   const taskName: string;
                                                   dueDate: TDateTime): integer;
var
  template: TTaskTemplate;
  taskId: integer;
  i: integer;
begin
  clearError;
  if not getTemplate(templateId, template) then
    begin
      result := -1;
      exit;
    end;

  taskId := addTask(taskName, template.description, template.category,
                    template.priority, dueDate);

  if taskId > 0 then
    begin
      setTaskEstimatedHours(taskId, template.estimatedHours);
      for i := 0 to length(template.tags) - 1 do
        addTaskTag(taskId, template.tags[i]);
    end;

  result := taskId;
end;

function TTaskManagerExtended.addTagToTemplate(templateId: integer;
                                              const tag: string): boolean;
var
  idx, count: integer;
begin
  clearError;
  idx := findTemplateIndex(templateId);
  if idx < 0 then
    begin
      lastError := 'Template not found: ' + intToStr(templateId);
      result := false;
      exit;
    end;

  count := length(templates[idx].tags);
  if count >= maxTagsPerTask then
    begin
      lastError := 'Maximum tags per template exceeded';
      result := false;
      exit;
    end;

  setLength(templates[idx].tags, count + 1);
  templates[idx].tags[count] := tag;
  result := true;
end;

end.
