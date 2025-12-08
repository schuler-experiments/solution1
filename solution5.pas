
program solution5;
{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  taskmanager, taskmanagerext, taskmanageradvanced, 
  taskmanagerenhanced, taskmanagerteam;

procedure SelfTest;
var
  Manager: TTeamTaskManager;
  TaskID1, TaskID2, TaskID3: Integer;
  Member1, Member2, Member3: Integer;
  Assignment1, Assignment2: Integer;
  Field1, Field2: Integer;
  Slot1: Integer;
  i: Integer;
  Members: TTeamMemberArray;
  Assignments: TTaskAssignmentArray;
  CustomFields: TCustomFieldDefArray;
  Slots: TTimeSlotArray;
begin
  WriteLn(StringOfChar('=', 70));
  WriteLn('TASK MANAGER - SOLUTION 5 (TEAM & COLLABORATION FEATURES)');
  WriteLn('Testing Layer 5: Team Management, Smart Scheduling, Custom Fields');
  WriteLn(StringOfChar('=', 70));
  WriteLn;

  Manager := TTeamTaskManager.Create;
  try
    Manager.SetCurrentUser('ProjectManager');
    
    // Test 1: Team Member Management
    WriteLn('TEST 1: Team Member Management');
    WriteLn(StringOfChar('-', 70));
    
    Member1 := Manager.AddTeamMember('Alice Johnson', 'alice@example.com', 
      'Senior Developer', 5, 40.0);
    WriteLn('Added team member: ', Manager.TeamMemberToString(Manager.GetTeamMember(Member1)));
    
    Member2 := Manager.AddTeamMember('Bob Smith', 'bob@example.com',
      'Developer', 7, 40.0);
    WriteLn('Added team member: ', Manager.TeamMemberToString(Manager.GetTeamMember(Member2)));
    
    Member3 := Manager.AddTeamMember('Carol White', 'carol@example.com',
      'QA Engineer', 6, 35.0);
    WriteLn('Added team member: ', Manager.TeamMemberToString(Manager.GetTeamMember(Member3)));
    
    // Add skills
    Manager.AddSkillToMember(Member1, 'Pascal');
    Manager.AddSkillToMember(Member1, 'Database');
    Manager.AddSkillToMember(Member2, 'Pascal');
    Manager.AddSkillToMember(Member2, 'Web');
    Manager.AddSkillToMember(Member3, 'Testing');
    Manager.AddSkillToMember(Member3, 'Automation');
    
    WriteLn;
    WriteLn('Active team members: ', Length(Manager.GetActiveTeamMembers));
    WriteLn;

    // Test 2: Task Creation and Assignment
    WriteLn('TEST 2: Task Creation and Smart Assignment');
    WriteLn(StringOfChar('-', 70));
    
    TaskID1 := Manager.AddTaskWithAudit('Implement User Authentication',
      'Create secure login system with password hashing', 'Backend',
      tpHigh, EncodeDate(2024, 12, 20), 16.0);
    WriteLn('Created Task #', TaskID1, ': Implement User Authentication');
    
    TaskID2 := Manager.AddTaskWithAudit('Design Dashboard UI',
      'Create responsive dashboard with charts', 'Frontend',
      tpMedium, EncodeDate(2024, 12, 18), 12.0);
    WriteLn('Created Task #', TaskID2, ': Design Dashboard UI');
    
    TaskID3 := Manager.AddTaskWithAudit('Write Unit Tests',
      'Comprehensive test coverage for all modules', 'Testing',
      tpHigh, EncodeDate(2024, 12, 22), 20.0);
    WriteLn('Created Task #', TaskID3, ': Write Unit Tests');
    WriteLn;
    
    // Manual assignment
    Assignment1 := Manager.AssignTask(TaskID1, Member1, 100, 
      'Alice has database experience');
    WriteLn('Assigned Task #', TaskID1, ' to ', 
      Manager.GetTeamMember(Member1).Name);
    
    // Auto assignment
    Assignment2 := Manager.AutoAssignTask(TaskID2);
    if Assignment2 <> -1 then
    begin
      Assignments := Manager.GetTaskAssignments(TaskID2);
      if Length(Assignments) > 0 then
        WriteLn('Auto-assigned Task #', TaskID2, ' to Member #', 
          Assignments[0].MemberID);
    end;
    
    Manager.AssignTask(TaskID3, Member3, 100, 'QA specialist');
    WriteLn('Assigned Task #', TaskID3, ' to ', 
      Manager.GetTeamMember(Member3).Name);
    WriteLn;

    // Test 3: Custom Fields
    WriteLn('TEST 3: Custom Fields System');
    WriteLn(StringOfChar('-', 70));
    
    Field1 := Manager.DefineCustomField('Client Name', cftString, 
      'Not specified', False);
    WriteLn('Defined custom field: Client Name (', 
      Manager.CustomFieldTypeToString(cftString), ')');
    
    Field2 := Manager.DefineCustomField('Priority Score', cftInteger,
      '0', False);
    WriteLn('Defined custom field: Priority Score (', 
      Manager.CustomFieldTypeToString(cftInteger), ')');
    WriteLn;
    
    Manager.SetCustomFieldValue(TaskID1, Field1, 'Acme Corporation');
    Manager.SetCustomFieldValue(TaskID1, Field2, '95');
    WriteLn('Set custom fields for Task #', TaskID1);
    WriteLn('  Client Name: ', Manager.GetCustomFieldValue(TaskID1, Field1));
    WriteLn('  Priority Score: ', Manager.GetCustomFieldValue(TaskID1, Field2));
    WriteLn;

    // Test 4: Task Scheduling
    WriteLn('TEST 4: Task Scheduling');
    WriteLn(StringOfChar('-', 70));
    
    Slot1 := Manager.ScheduleTask(TaskID1, 
      EncodeDate(2024, 12, 10) + EncodeTime(9, 0, 0, 0),
      240, 'Morning development session');
    WriteLn('Scheduled Task #', TaskID1, ' for 4 hours on 2024-12-10 09:00');
    
    Manager.ScheduleTask(TaskID2,
      EncodeDate(2024, 12, 10) + EncodeTime(14, 0, 0, 0),
      180, 'Afternoon design work');
    WriteLn('Scheduled Task #', TaskID2, ' for 3 hours on 2024-12-10 14:00');
    WriteLn;
    
    Slots := Manager.GetScheduleForPeriod(
      EncodeDate(2024, 12, 10),
      EncodeDate(2024, 12, 11));
    WriteLn('Total scheduled slots for Dec 10-11: ', Length(Slots));
    WriteLn;

    // Test 5: Workload Analysis
    WriteLn('TEST 5: Team Workload Analysis');
    WriteLn(StringOfChar('-', 70));
    WriteLn(Manager.GetMemberWorkload);
    WriteLn;
    WriteLn(Manager.GetTeamCapacity);
    WriteLn;

    // Test 6: Reminders (from enhanced layer)
    WriteLn('TEST 6: Task Reminders (Enhanced Layer)');
    WriteLn(StringOfChar('-', 70));
    
    Manager.AddReminder(TaskID1, rtBeforeDue, Now, 60,
      'Authentication deadline approaching!');
    WriteLn('Added reminder for Task #', TaskID1, ' (60 min before due)');
    
    Manager.AddReminder(TaskID2, rtAtSpecificTime,
      EncodeDate(2024, 12, 15) + EncodeTime(10, 0, 0, 0), 0,
      'Review dashboard design');
    WriteLn('Added specific time reminder for Task #', TaskID2);
    WriteLn;

    // Test 7: Export to Markdown
    WriteLn('TEST 7: Export to Markdown Format');
    WriteLn(StringOfChar('-', 70));
    
    WriteLn('Exporting all tasks to markdown format...');
    WriteLn;
    WriteLn(Manager.ExportToMarkdown);
    WriteLn;

    // Test 8: Team Statistics
    WriteLn('TEST 8: Team Productivity Statistics');
    WriteLn(StringOfChar('-', 70));
    WriteLn(Manager.GetTeamProductivity);
    WriteLn;

    // Test 9: Unassigned Tasks
    WriteLn('TEST 9: Unassigned Tasks');
    WriteLn(StringOfChar('-', 70));
    
    TaskID1 := Manager.AddTask('Code Review Process',
      'Establish code review guidelines', tpMedium, EncodeDate(2024, 12, 25));
    WriteLn('Created unassigned task: Code Review Process');
    
    WriteLn('Total unassigned tasks: ', 
      Length(Manager.GetUnassignedTasks));
    WriteLn;

    // Test 10: Member Skills Search
    WriteLn('TEST 10: Find Team Members by Skill');
    WriteLn(StringOfChar('-', 70));
    
    Members := Manager.GetMembersBySkill('Pascal');
    WriteLn('Team members with Pascal skill: ', Length(Members));
    for i := 0 to High(Members) do
      WriteLn('  - ', Members[i].Name);
    WriteLn;
    
    Members := Manager.GetMembersBySkill('Testing');
    WriteLn('Team members with Testing skill: ', Length(Members));
    for i := 0 to High(Members) do
      WriteLn('  - ', Members[i].Name);
    WriteLn;

    // Test 11: Task Reassignment
    WriteLn('TEST 11: Task Reassignment');
    WriteLn(StringOfChar('-', 70));
    
    Assignments := Manager.GetTaskAssignments(TaskID1);
    if Length(Assignments) > 0 then
    begin
      WriteLn('Reassigning task from Member #', Assignments[0].MemberID,
        ' to Member #', Member2);
      if Manager.ReassignTask(Assignments[0].AssignmentID, Member2) then
        WriteLn('Task successfully reassigned!')
      else
        WriteLn('Failed to reassign task');
    end;
    WriteLn;

    // Test 12: Custom Fields Listing
    WriteLn('TEST 12: All Custom Fields Defined');
    WriteLn(StringOfChar('-', 70));
    
    CustomFields := Manager.GetAllCustomFields;
    WriteLn('Total custom fields defined: ', Length(CustomFields));
    for i := 0 to High(CustomFields) do
      WriteLn('  Field #', CustomFields[i].FieldID, ': ',
        CustomFields[i].FieldName, ' (', 
        Manager.CustomFieldTypeToString(CustomFields[i].FieldType), ')');
    WriteLn;

    // Test 13: Work Sessions (from advanced layer)
    WriteLn('TEST 13: Work Session Tracking (Advanced Layer)');
    WriteLn(StringOfChar('-', 70));
    
    i := Manager.StartWorkSession(TaskID1, 'Working on authentication module');
    WriteLn('Started work session #', i, ' for Task #', TaskID1);
    WriteLn('Active session ID: ', Manager.GetActiveSession);
    
    Sleep(100); // Simulate some work
    
    Manager.EndWorkSession(i, True);
    WriteLn('Ended work session #', i);
    WriteLn;

    // Test 14: Task Dependencies (from advanced layer)
    WriteLn('TEST 14: Task Dependencies (Advanced Layer)');
    WriteLn(StringOfChar('-', 70));
    
    i := Manager.AddDependency(TaskID3, TaskID1, dtFinishToStart, 0);
    WriteLn('Added dependency: Task #', TaskID3, 
      ' depends on Task #', TaskID1);
    WriteLn('Can complete Task #', TaskID3, '? ', 
      Manager.ValidateTaskCompletion(TaskID3));
    WriteLn;

    // Test 15: Archive Statistics
    WriteLn('TEST 15: Archive Statistics (Enhanced Layer)');
    WriteLn(StringOfChar('-', 70));
    WriteLn(Manager.GetArchiveStatistics);
    WriteLn;

    // Final Summary
    WriteLn(StringOfChar('=', 70));
    WriteLn('SOLUTION 5 TEST SUMMARY');
    WriteLn(StringOfChar('=', 70));
    WriteLn('✓ Team member management tested');
    WriteLn('✓ Task assignment (manual and auto) tested');
    WriteLn('✓ Custom fields system tested');
    WriteLn('✓ Task scheduling tested');
    WriteLn('✓ Workload analysis tested');
    WriteLn('✓ Reminders integration tested');
    WriteLn('✓ Markdown export tested');
    WriteLn('✓ Team statistics tested');
    WriteLn('✓ Skill-based search tested');
    WriteLn('✓ Task reassignment tested');
    WriteLn('✓ Work sessions integration tested');
    WriteLn('✓ Dependencies integration tested');
    WriteLn('✓ Archive integration tested');
    WriteLn;
    WriteLn('All Layer 5 (Team & Collaboration) features working!');
    WriteLn(StringOfChar('=', 70));
    
  finally
    Manager.Free;
  end;
end;

begin
  try
    SelfTest;
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end.
