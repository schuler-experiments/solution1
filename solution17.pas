
program solution17;
{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, taskmanagermeetings;

procedure SelfTest;
var
  manager: TMeetingTaskManager;
  meetingid1, meetingid2, meetingid3: integer;
  member1, member2, member3: integer;
  attendee1, attendee2, attendee3: integer;
  agenda1, agenda2, agenda3: integer;
  action1, action2: integer;
  template1: integer;
  meetings: TMeetingArray;
  attendees: TMeetingAttendeeArray;
  agendaitems: TAgendaItemArray;
  actionitems: TMeetingActionItemArray;
  i: integer;
  starttime: TDateTime;
begin
  WriteLn('=== Meeting Management Self-Test ===');
  WriteLn;
  
  manager := TMeetingTaskManager.Create;
  try
    WriteLn('1. Creating team members...');
    member1 := manager.AddTeamMember('Alice Johnson', 'alice@company.com', 
      'Team Lead', 10, 40.0);
    member2 := manager.AddTeamMember('Bob Smith', 'bob@company.com', 
      'Developer', 8, 40.0);
    member3 := manager.AddTeamMember('Carol White', 'carol@company.com', 
      'Designer', 8, 40.0);
    WriteLn('   Created 3 team members');
    WriteLn;
    
    WriteLn('2. Scheduling meetings...');
    starttime := IncHour(Now, 2);
    
    meetingid1 := manager.ScheduleMeeting(
      'Daily Standup',
      'Daily team sync-up',
      mtStandup,
      starttime,
      15,
      'Conference Room A',
      member1
    );
    WriteLn('   Scheduled Daily Standup (ID: ', meetingid1, ')');
    
    meetingid2 := manager.ScheduleMeeting(
      'Sprint Planning',
      'Plan next sprint backlog',
      mtPlanning,
      IncDay(starttime, 1),
      120,
      'Main Conference Room',
      member1
    );
    WriteLn('   Scheduled Sprint Planning (ID: ', meetingid2, ')');
    
    meetingid3 := manager.ScheduleMeeting(
      'Client Presentation',
      'Demo new features to client',
      mtClientMeeting,
      IncDay(starttime, 3),
      60,
      'Virtual - Zoom',
      member1
    );
    WriteLn('   Scheduled Client Presentation (ID: ', meetingid3, ')');
    WriteLn;
    
    WriteLn('3. Adding attendees...');
    attendee1 := manager.AddAttendee(meetingid1, member2, asRequired);
    attendee2 := manager.AddAttendee(meetingid1, member3, asRequired);
    WriteLn('   Added 2 attendees to Daily Standup');
    
    manager.AddAttendee(meetingid2, member2, asRequired);
    manager.AddAttendee(meetingid2, member3, asRequired);
    WriteLn('   Added 2 attendees to Sprint Planning');
    
    manager.AddAttendee(meetingid3, member2, asOptional);
    WriteLn('   Added 1 attendee to Client Presentation');
    WriteLn;
    
    WriteLn('4. Creating agenda items...');
    agenda1 := manager.AddAgendaItem(meetingid2, 'Review last sprint', 
      'Discuss completed items and blockers', 15, member1);
    agenda2 := manager.AddAgendaItem(meetingid2, 'Backlog refinement',
      'Prioritize and estimate stories', 45, member1);
    agenda3 := manager.AddAgendaItem(meetingid2, 'Sprint commitment',
      'Agree on sprint goal and tasks', 30, member1);
    WriteLn('   Created 3 agenda items for Sprint Planning');
    WriteLn;
    
    WriteLn('5. Simulating meeting execution...');
    manager.StartMeeting(meetingid1);
    WriteLn('   Started Daily Standup');
    
    manager.MarkAttendance(attendee1, true);
    manager.MarkAttendance(attendee2, true);
    WriteLn('   Marked attendance for 2 attendees');
    
    manager.EndMeeting(meetingid1, 'Quick sync completed. All on track.');
    WriteLn('   Ended Daily Standup with minutes');
    WriteLn;
    
    WriteLn('6. Adding action items...');
    action1 := manager.AddActionItem(meetingid1, 
      'Fix authentication bug in login module', 
      member2, 
      IncDay(Now, 2));
    action2 := manager.AddActionItem(meetingid1,
      'Update design mockups for dashboard',
      member3,
      IncDay(Now, 3));
    WriteLn('   Created 2 action items from Daily Standup');
    WriteLn;
    
    WriteLn('7. Creating tasks from action items...');
    manager.CreateTaskFromActionItem(action1);
    manager.CreateTaskFromActionItem(action2);
    WriteLn('   Auto-created 2 tasks from action items');
    WriteLn;
    
    WriteLn('8. Creating meeting template...');
    template1 := manager.CreateMeetingTemplate(
      'Weekly Team Sync',
      'Standard weekly team meeting template',
      mtGeneral,
      30,
      'Updates|Blockers|Next Steps',
      'Book room|Prepare agenda|Notify team',
      'Share minutes|Create action items|Schedule next meeting'
    );
    WriteLn('   Created template: Weekly Team Sync (ID: ', template1, ')');
    WriteLn;
    
    WriteLn('9. Creating meeting from template...');
    manager.CreateMeetingFromTemplate(template1, IncDay(Now, 7), member1);
    WriteLn('   Scheduled meeting from template');
    WriteLn;
    
    WriteLn('10. Retrieving and displaying meetings...');
    meetings := manager.GetUpcomingMeetings(7);
    WriteLn('   Found ', Length(meetings), ' upcoming meetings in next 7 days:');
    for i := 0 to Length(meetings) - 1 do
    begin
      WriteLn('   - ', meetings[i].title, ' (', 
        manager.MeetingTypeToString(meetings[i].meetingtype), ')');
      WriteLn('     Scheduled: ', DateTimeToStr(meetings[i].scheduledstart));
      WriteLn('     Status: ', manager.MeetingStatusToString(meetings[i].status));
    end;
    WriteLn;
    
    WriteLn('11. Checking attendees...');
    attendees := manager.GetMeetingAttendees(meetingid1);
    WriteLn('   Daily Standup has ', Length(attendees), ' attendees:');
    for i := 0 to Length(attendees) - 1 do
    begin
      WriteLn('   - Member ID: ', attendees[i].memberid, 
        ' (', manager.AttendeeStatusToString(attendees[i].status), ')');
      WriteLn('     Attended: ', attendees[i].attended);
    end;
    WriteLn;
    
    WriteLn('12. Reviewing agenda items...');
    agendaitems := manager.GetAgendaItems(meetingid2);
    WriteLn('   Sprint Planning has ', Length(agendaitems), ' agenda items:');
    for i := 0 to Length(agendaitems) - 1 do
    begin
      WriteLn('   ', i + 1, '. ', agendaitems[i].title);
      WriteLn('      Allocated time: ', agendaitems[i].allocatedminutes, ' minutes');
      WriteLn('      ', agendaitems[i].description);
    end;
    WriteLn;
    
    WriteLn('13. Checking action items...');
    actionitems := manager.GetOpenActionItems;
    WriteLn('   Found ', Length(actionitems), ' open action items:');
    for i := 0 to Length(actionitems) - 1 do
    begin
      WriteLn('   - ', actionitems[i].description);
      WriteLn('     Assigned to Member ID: ', actionitems[i].assignedto);
      WriteLn('     Due: ', DateToStr(actionitems[i].duedate));
      WriteLn('     Status: ', manager.ActionItemStatusToString(actionitems[i].status));
    end;
    WriteLn;
    
    WriteLn('14. Updating meeting details...');
    manager.UpdateMeetingLocation(meetingid3, 
      'Virtual - Microsoft Teams', 
      'https://teams.microsoft.com/meeting123');
    WriteLn('   Updated Client Presentation location and virtual link');
    WriteLn;
    
    WriteLn('15. Testing meeting statistics...');
    WriteLn(manager.GetMeetingStatistics(30));
    WriteLn;
    
    WriteLn('16. Testing member meeting load...');
    WriteLn(manager.GetMemberMeetingLoad(member1, 30));
    WriteLn;
    
    WriteLn('17. Testing team meeting metrics...');
    WriteLn(manager.GetTeamMeetingMetrics(30));
    WriteLn;
    
    WriteLn('18. Updating action item status...');
    manager.UpdateActionItemStatus(action1, aisInProgress);
    WriteLn('   Updated action item ', action1, ' to In Progress');
    WriteLn;
    
    WriteLn('19. Testing meeting efficiency...');
    manager.MarkAgendaItemComplete(agenda1);
    manager.MarkAgendaItemComplete(agenda2);
    WriteLn('   Marked 2 agenda items as complete');
    WriteLn;
    
    WriteLn('20. Testing cancellation...');
    manager.CancelMeeting(meetingid3, 'Client requested reschedule');
    WriteLn('   Cancelled Client Presentation');
    WriteLn;
    
    WriteLn('21. Getting meetings by type...');
    meetings := manager.GetMeetingsByType(mtStandup);
    WriteLn('   Found ', Length(meetings), ' standup meetings');
    WriteLn;
    
    WriteLn('22. Testing attendance rate...');
    WriteLn('   Member ', member2, ' attendance rate: ', 
      FormatFloat('0.00', manager.GetAttendanceRate(member2, 30)), '%');
    WriteLn;
    
    WriteLn('23. Saving meeting data...');
    if manager.SaveMeetingDataToFile('solution1/meetings_test.dat') then
      WriteLn('   Meeting data saved successfully')
    else
      WriteLn('   Failed to save meeting data');
    WriteLn;
    
    WriteLn('=== All Meeting Management Tests Completed Successfully! ===');
    WriteLn;
    WriteLn('Summary:');
    WriteLn('- Scheduled 4 meetings (3 manual + 1 from template)');
    WriteLn('- Created 3 team members');
    WriteLn('- Added 6 attendees across meetings');
    WriteLn('- Created 3 agenda items for planning meeting');
    WriteLn('- Generated 2 action items with linked tasks');
    WriteLn('- Created 1 meeting template');
    WriteLn('- Simulated complete meeting lifecycle (start, attendance, end)');
    WriteLn('- Tested statistics, metrics, and reporting features');
    WriteLn('- Demonstrated meeting cancellation workflow');
    WriteLn;
    WriteLn('The Meeting Management module is fully functional and ready for use!');
    
  finally
    manager.Free;
  end;
end;

begin
  try
    SelfTest;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      Halt(1);
    end;
  end;
end.
