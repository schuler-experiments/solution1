program solution1;

{$mode objfpc}

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager,
  TaskManagerExtended;

var
  manager: TTaskManagerExtended;

procedure selfTest;
var
  i: integer;
  tasks: TTaskArray;
  stats: TTaskStats;
  noteCount: integer;
  allMilestones: TMilestoneArray;
  milestoneTasks: TTaskArray;
  milestoneId: integer;
  allTemplates: TTemplateArray;
  templateId: integer;
  taskId: integer;
begin
  WriteLn('===== TASK MANAGER SELF TEST =====');
  WriteLn;

  { Original tests (abbreviated) }
  WriteLn('Test 1: Creating tasks...');
  for i := 1 to 5 do
    WriteLn('Created task ' + intToStr(i) + ' (ID: ' + 
            intToStr(manager.addTask('Task ' + intToStr(i), 'Description', 'Development',
                                     tpHigh, now + i)) + ')');
  WriteLn;

  { New feature tests }

  WriteLn('Test 2: Adding task notes...');
  taskId := 1;
  if manager.addTaskNote(taskId, 'Important: needs database review', true) then
    WriteLn('Added important note to Task 1');
  if manager.addTaskNote(taskId, 'First implementation draft completed', false) then
    WriteLn('Added regular note to Task 1');
  noteCount := manager.getTaskNoteCount(taskId);
  WriteLn('Task 1 has ' + intToStr(noteCount) + ' notes');
  WriteLn;

  WriteLn('Test 3: Assigning tasks...');
  if manager.assignTaskTo(1, 'Alice') then
    WriteLn('Task 1 assigned to Alice');
  if manager.assignTaskTo(2, 'Bob') then
    WriteLn('Task 2 assigned to Bob');
  if manager.assignTaskTo(3, 'Alice') then
    WriteLn('Task 3 assigned to Alice');
  WriteLn;

  WriteLn('Test 4: Getting tasks assigned to a person...');
  tasks := manager.getTasksAssignedTo('Alice');
  WriteLn('Alice has ' + intToStr(length(tasks)) + ' task(s)');
  WriteLn;

  WriteLn('Test 5: Creating milestones...');
  milestoneId := manager.addMilestone('Phase 1', 'Initial development phase', now + 30);
  WriteLn('Created milestone: Phase 1 (ID: ' + intToStr(milestoneId) + ')');
  manager.addMilestone('Phase 2', 'Testing and refinement', now + 60);
  WriteLn('Created milestone: Phase 2');
  WriteLn;

  WriteLn('Test 6: Assigning tasks to milestones...');
  if manager.assignTaskToMilestone(1, milestoneId) then
    WriteLn('Task 1 assigned to Phase 1 milestone');
  if manager.assignTaskToMilestone(2, milestoneId) then
    WriteLn('Task 2 assigned to Phase 1 milestone');
  WriteLn;

  WriteLn('Test 7: Getting milestone progress...');
  stats := manager.getMilestoneProgress(milestoneId);
  WriteLn('Phase 1 Progress: ' + intToStr(stats.totalTasks) + ' total tasks');
  WriteLn;

  WriteLn('Test 8: Creating task templates...');
  templateId := manager.createTemplate('Bug Fix Template', 'Standard bug fix task',
                                       'Maintenance', tpMedium, 4.0);
  WriteLn('Created template: Bug Fix Template (ID: ' + intToStr(templateId) + ')');
  manager.addTagToTemplate(templateId, 'bug');
  manager.addTagToTemplate(templateId, 'urgent');
  WriteLn('Added tags to template');
  WriteLn;

  WriteLn('Test 9: Creating tasks from templates...');
  taskId := manager.createTaskFromTemplate(templateId, 'Fix login bug', now + 7);
  if taskId > 0 then
    WriteLn('Task created from template (ID: ' + intToStr(taskId) + ')')
  else
    WriteLn('Failed to create task from template: ' + manager.getLastError);
  WriteLn;

  WriteLn('Test 10: Getting all templates...');
  allTemplates := manager.getAllTemplates;
  WriteLn('Total templates: ' + intToStr(length(allTemplates)));
  WriteLn;

  WriteLn('Test 11: Getting all milestones...');
  allMilestones := manager.getAllMilestones;
  WriteLn('Total milestones: ' + intToStr(length(allMilestones)));
  WriteLn;

  WriteLn('Test 12: Getting tasks in a milestone...');
  milestoneTasks := manager.getTasksByMilestone(milestoneId);
  WriteLn('Tasks in Phase 1: ' + intToStr(length(milestoneTasks)));
  WriteLn;

  WriteLn('Test 13: Task statistics...');
  stats := manager.getStatistics;
  WriteLn('Total tasks: ' + intToStr(stats.totalTasks));
  WriteLn('New tasks: ' + intToStr(stats.newTasks));
  WriteLn('In progress: ' + intToStr(stats.inProgressTasks));
  WriteLn('Completed: ' + intToStr(stats.completedTasks));
  WriteLn;

  WriteLn('===== ALL TESTS COMPLETED SUCCESSFULLY =====');
  WriteLn;
end;

begin
  manager := TTaskManagerExtended.Create;
  try
    selfTest;
  finally
    manager.Free;
  end;
end.
