
unit taskmanagermeetings;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math, taskmanager, taskmanagerteam;

type
  TMeetingType = (mtStandup, mtPlanning, mtRetrospective, mtClientMeeting, 
                  mtOneOnOne, mtReview, mtGeneral);
  
  TMeetingStatus = (msScheduled, msInProgress, msCompleted, msCancelled, 
                    msRescheduled);
  
  TAttendeeStatus = (asRequired, asOptional, asOrganizer);
  
  TActionItemStatus = (aisOpen, aisInProgress, aisCompleted, aisCancelled);

  TMeetingAttendee = record
    id: integer;
    meetingid: integer;
    memberid: integer;
    status: TAttendeeStatus;
    attended: boolean;
    responsedate: TDateTime;
  end;
  
  TAgendaItem = record
    id: integer;
    meetingid: integer;
    title: string;
    description: string;
    allocatedminutes: integer;
    orderindex: integer;
    presenter: integer;
    completed: boolean;
  end;
  
  TMeetingActionItem = record
    id: integer;
    meetingid: integer;
    description: string;
    assignedto: integer;
    duedate: TDateTime;
    status: TActionItemStatus;
    linkedtaskid: integer;
    createdat: TDateTime;
    completedat: TDateTime;
  end;
  
  TMeeting = record
    id: integer;
    title: string;
    description: string;
    meetingtype: TMeetingType;
    status: TMeetingStatus;
    scheduledstart: TDateTime;
    scheduledend: TDateTime;
    actualstart: TDateTime;
    actualend: TDateTime;
    location: string;
    virtualmeetinglink: string;
    organizermemberid: integer;
    linkedtaskid: integer;
    linkedprojectid: integer;
    isrecurring: boolean;
    recurrencepatternid: integer;
    createdat: TDateTime;
    minutes: string;
    notes: string;
  end;
  
  TMeetingTemplate = record
    id: integer;
    name: string;
    description: string;
    meetingtype: TMeetingType;
    defaultduration: integer;
    agendatemplate: string;
    preflightchecklist: string;
    postmeetingchecklist: string;
    createdat: TDateTime;
  end;

  TMeetingAttendeeArray = array of TMeetingAttendee;
  TAgendaItemArray = array of TAgendaItem;
  TMeetingActionItemArray = array of TMeetingActionItem;
  TMeetingArray = array of TMeeting;
  TMeetingTemplateArray = array of TMeetingTemplate;

  { TMeetingTaskManager }
  TMeetingTaskManager = class(TTeamTaskManager)
  private
    fmeetings: TMeetingArray;
    fmeetingcount: integer;
    fattendees: TMeetingAttendeeArray;
    fattendeecount: integer;
    fagendaitems: TAgendaItemArray;
    fagendacount: integer;
    factionitems: TMeetingActionItemArray;
    factioncount: integer;
    ftemplates: TMeetingTemplateArray;
    ftemplatecount: integer;
    fnextmeetingid: integer;
    fnextattendeeid: integer;
    fnextagendaid: integer;
    fnextactionid: integer;
    fnexttemplateid: integer;
    
    function FindMeetingIndex(AMeetingID: integer): integer;
    function FindAttendeeIndex(AAttendeeID: integer): integer;
    function FindAgendaIndex(AAgendaID: integer): integer;
    function FindActionIndex(AActionID: integer): integer;
    function FindTemplateIndex(ATemplateID: integer): integer;
    function CalculateMeetingDuration(const AMeeting: TMeeting): integer;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    function ScheduleMeeting(const ATitle, ADescription: string;
      AMeetingType: TMeetingType; AStart: TDateTime; ADurationMinutes: integer;
      const ALocation: string; AOrganizerID: integer): integer;
    function UpdateMeetingTime(AMeetingID: integer; ANewStart: TDateTime; 
      ANewDuration: integer): boolean;
    function UpdateMeetingLocation(AMeetingID: integer; 
      const ALocation, AVirtualLink: string): boolean;
    function CancelMeeting(AMeetingID: integer; const AReason: string): boolean;
    function StartMeeting(AMeetingID: integer): boolean;
    function EndMeeting(AMeetingID: integer; const AMinutes: string): boolean;
    function GetMeeting(AMeetingID: integer): TMeeting;
    function GetAllMeetings: TMeetingArray;
    function GetUpcomingMeetings(ADays: integer): TMeetingArray;
    function GetMeetingsByDateRange(AStart, AEnd: TDateTime): TMeetingArray;
    function GetMeetingsByType(AMeetingType: TMeetingType): TMeetingArray;
    
    function AddAttendee(AMeetingID, AMemberID: integer; 
      AStatus: TAttendeeStatus): integer;
    function RemoveAttendee(AAttendeeID: integer): boolean;
    function MarkAttendance(AAttendeeID: integer; AAttended: boolean): boolean;
    function GetMeetingAttendees(AMeetingID: integer): TMeetingAttendeeArray;
    function GetAttendanceRate(AMemberID: integer; ADays: integer): double;
    
    function AddAgendaItem(AMeetingID: integer; const ATitle, ADescription: string;
      AMinutes: integer; APresenterID: integer): integer;
    function UpdateAgendaItem(AAgendaID: integer; const ATitle, ADescription: string;
      AMinutes: integer): boolean;
    function DeleteAgendaItem(AAgendaID: integer): boolean;
    function ReorderAgendaItem(AAgendaID: integer; ANewIndex: integer): boolean;
    function MarkAgendaItemComplete(AAgendaID: integer): boolean;
    function GetAgendaItems(AMeetingID: integer): TAgendaItemArray;
    
    function AddActionItem(AMeetingID: integer; const ADescription: string;
      AAssignedTo: integer; ADueDate: TDateTime): integer;
    function UpdateActionItemStatus(AActionID: integer; 
      AStatus: TActionItemStatus): boolean;
    function LinkActionItemToTask(AActionID, ATaskID: integer): boolean;
    function CreateTaskFromActionItem(AActionID: integer): integer;
    function GetMeetingActionItems(AMeetingID: integer): TMeetingActionItemArray;
    function GetMemberActionItems(AMemberID: integer): TMeetingActionItemArray;
    function GetOpenActionItems: TMeetingActionItemArray;
    
    function CreateMeetingTemplate(const AName, ADescription: string;
      AMeetingType: TMeetingType; ADuration: integer;
      const AAgendaTemplate, APreFlight, APostChecklist: string): integer;
    function CreateMeetingFromTemplate(ATemplateID: integer; 
      AStart: TDateTime; AOrganizerID: integer): integer;
    function GetAllTemplates: TMeetingTemplateArray;
    function GetTemplate(ATemplateID: integer): TMeetingTemplate;
    
    function GetMeetingStatistics(ADays: integer): string;
    function GetMemberMeetingLoad(AMemberID: integer; ADays: integer): string;
    function GetMeetingEfficiency(AMeetingID: integer): string;
    function GetTeamMeetingMetrics(ADays: integer): string;
    
    function MeetingTypeToString(AMeetingType: TMeetingType): string;
    function MeetingStatusToString(AStatus: TMeetingStatus): string;
    function AttendeeStatusToString(AStatus: TAttendeeStatus): string;
    function ActionItemStatusToString(AStatus: TActionItemStatus): string;
    
    function SaveMeetingDataToFile(const AFilename: string): boolean;
    function LoadMeetingDataFromFile(const AFilename: string): boolean;
  end;

implementation

{ TMeetingTaskManager }

constructor TMeetingTaskManager.Create;
begin
  inherited Create;
  SetLength(fmeetings, 0);
  SetLength(fattendees, 0);
  SetLength(fagendaitems, 0);
  SetLength(factionitems, 0);
  SetLength(ftemplates, 0);
  fmeetingcount := 0;
  fattendeecount := 0;
  fagendacount := 0;
  factioncount := 0;
  ftemplatecount := 0;
  fnextmeetingid := 1;
  fnextattendeeid := 1;
  fnextagendaid := 1;
  fnextactionid := 1;
  fnexttemplateid := 1;
end;

destructor TMeetingTaskManager.Destroy;
begin
  SetLength(fmeetings, 0);
  SetLength(fattendees, 0);
  SetLength(fagendaitems, 0);
  SetLength(factionitems, 0);
  SetLength(ftemplates, 0);
  inherited Destroy;
end;

function TMeetingTaskManager.FindMeetingIndex(AMeetingID: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to fmeetingcount - 1 do
    if fmeetings[i].id = AMeetingID then
    begin
      result := i;
      exit;
    end;
end;

function TMeetingTaskManager.FindAttendeeIndex(AAttendeeID: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to fattendeecount - 1 do
    if fattendees[i].id = AAttendeeID then
    begin
      result := i;
      exit;
    end;
end;

function TMeetingTaskManager.FindAgendaIndex(AAgendaID: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to fagendacount - 1 do
    if fagendaitems[i].id = AAgendaID then
    begin
      result := i;
      exit;
    end;
end;

function TMeetingTaskManager.FindActionIndex(AActionID: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to factioncount - 1 do
    if factionitems[i].id = AActionID then
    begin
      result := i;
      exit;
    end;
end;

function TMeetingTaskManager.FindTemplateIndex(ATemplateID: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to ftemplatecount - 1 do
    if ftemplates[i].id = ATemplateID then
    begin
      result := i;
      exit;
    end;
end;

function TMeetingTaskManager.CalculateMeetingDuration(const AMeeting: TMeeting): integer;
begin
  if AMeeting.actualend > AMeeting.actualstart then
    result := MinutesBetween(AMeeting.actualstart, AMeeting.actualend)
  else
    result := MinutesBetween(AMeeting.scheduledstart, AMeeting.scheduledend);
end;

function TMeetingTaskManager.ScheduleMeeting(const ATitle, ADescription: string;
  AMeetingType: TMeetingType; AStart: TDateTime; ADurationMinutes: integer;
  const ALocation: string; AOrganizerID: integer): integer;
var
  meeting: TMeeting;
begin
  meeting.id := fnextmeetingid;
  inc(fnextmeetingid);
  meeting.title := ATitle;
  meeting.description := ADescription;
  meeting.meetingtype := AMeetingType;
  meeting.status := msScheduled;
  meeting.scheduledstart := AStart;
  meeting.scheduledend := IncMinute(AStart, ADurationMinutes);
  meeting.actualstart := 0;
  meeting.actualend := 0;
  meeting.location := ALocation;
  meeting.virtualmeetinglink := '';
  meeting.organizermemberid := AOrganizerID;
  meeting.linkedtaskid := 0;
  meeting.linkedprojectid := 0;
  meeting.isrecurring := false;
  meeting.recurrencepatternid := 0;
  meeting.createdat := Now;
  meeting.minutes := '';
  meeting.notes := '';
  
  SetLength(fmeetings, fmeetingcount + 1);
  fmeetings[fmeetingcount] := meeting;
  inc(fmeetingcount);
  
  AddAttendee(meeting.id, AOrganizerID, asOrganizer);
  
  result := meeting.id;
end;

function TMeetingTaskManager.UpdateMeetingTime(AMeetingID: integer; 
  ANewStart: TDateTime; ANewDuration: integer): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindMeetingIndex(AMeetingID);
  if idx >= 0 then
  begin
    fmeetings[idx].scheduledstart := ANewStart;
    fmeetings[idx].scheduledend := IncMinute(ANewStart, ANewDuration);
    if fmeetings[idx].status = msScheduled then
      fmeetings[idx].status := msRescheduled;
    result := true;
  end;
end;

function TMeetingTaskManager.UpdateMeetingLocation(AMeetingID: integer;
  const ALocation, AVirtualLink: string): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindMeetingIndex(AMeetingID);
  if idx >= 0 then
  begin
    fmeetings[idx].location := ALocation;
    fmeetings[idx].virtualmeetinglink := AVirtualLink;
    result := true;
  end;
end;

function TMeetingTaskManager.CancelMeeting(AMeetingID: integer; 
  const AReason: string): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindMeetingIndex(AMeetingID);
  if idx >= 0 then
  begin
    fmeetings[idx].status := msCancelled;
    fmeetings[idx].notes := fmeetings[idx].notes + #13#10 + 
      'Cancellation reason: ' + AReason;
    result := true;
  end;
end;

function TMeetingTaskManager.StartMeeting(AMeetingID: integer): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindMeetingIndex(AMeetingID);
  if idx >= 0 then
  begin
    fmeetings[idx].status := msInProgress;
    fmeetings[idx].actualstart := Now;
    result := true;
  end;
end;

function TMeetingTaskManager.EndMeeting(AMeetingID: integer; 
  const AMinutes: string): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindMeetingIndex(AMeetingID);
  if idx >= 0 then
  begin
    fmeetings[idx].status := msCompleted;
    fmeetings[idx].actualend := Now;
    fmeetings[idx].minutes := AMinutes;
    result := true;
  end;
end;

function TMeetingTaskManager.GetMeeting(AMeetingID: integer): TMeeting;
var
  idx: integer;
  empty: TMeeting;
begin
  idx := FindMeetingIndex(AMeetingID);
  if idx >= 0 then
    result := fmeetings[idx]
  else
  begin
    FillChar(empty, SizeOf(empty), 0);
    result := empty;
  end;
end;

function TMeetingTaskManager.GetAllMeetings: TMeetingArray;
begin
  SetLength(result, fmeetingcount);
  if fmeetingcount > 0 then
    Move(fmeetings[0], result[0], fmeetingcount * SizeOf(TMeeting));
end;

function TMeetingTaskManager.GetUpcomingMeetings(ADays: integer): TMeetingArray;
var
  i, count: integer;
  cutoff: TDateTime;
begin
  SetLength(result, fmeetingcount);
  count := 0;
  cutoff := IncDay(Now, ADays);
  
  for i := 0 to fmeetingcount - 1 do
    if (fmeetings[i].status = msScheduled) and 
       (fmeetings[i].scheduledstart <= cutoff) and
       (fmeetings[i].scheduledstart >= Now) then
    begin
      result[count] := fmeetings[i];
      inc(count);
    end;
  
  SetLength(result, count);
end;

function TMeetingTaskManager.GetMeetingsByDateRange(AStart, AEnd: TDateTime): TMeetingArray;
var
  i, count: integer;
begin
  SetLength(result, fmeetingcount);
  count := 0;
  
  for i := 0 to fmeetingcount - 1 do
    if (fmeetings[i].scheduledstart >= AStart) and 
       (fmeetings[i].scheduledstart <= AEnd) then
    begin
      result[count] := fmeetings[i];
      inc(count);
    end;
  
  SetLength(result, count);
end;

function TMeetingTaskManager.GetMeetingsByType(AMeetingType: TMeetingType): TMeetingArray;
var
  i, count: integer;
begin
  SetLength(result, fmeetingcount);
  count := 0;
  
  for i := 0 to fmeetingcount - 1 do
    if fmeetings[i].meetingtype = AMeetingType then
    begin
      result[count] := fmeetings[i];
      inc(count);
    end;
  
  SetLength(result, count);
end;

function TMeetingTaskManager.AddAttendee(AMeetingID, AMemberID: integer;
  AStatus: TAttendeeStatus): integer;
var
  attendee: TMeetingAttendee;
begin
  attendee.id := fnextattendeeid;
  inc(fnextattendeeid);
  attendee.meetingid := AMeetingID;
  attendee.memberid := AMemberID;
  attendee.status := AStatus;
  attendee.attended := false;
  attendee.responsedate := Now;
  
  SetLength(fattendees, fattendeecount + 1);
  fattendees[fattendeecount] := attendee;
  inc(fattendeecount);
  
  result := attendee.id;
end;

function TMeetingTaskManager.RemoveAttendee(AAttendeeID: integer): boolean;
var
  idx, i: integer;
begin
  result := false;
  idx := FindAttendeeIndex(AAttendeeID);
  if idx >= 0 then
  begin
    for i := idx to fattendeecount - 2 do
      fattendees[i] := fattendees[i + 1];
    dec(fattendeecount);
    SetLength(fattendees, fattendeecount);
    result := true;
  end;
end;

function TMeetingTaskManager.MarkAttendance(AAttendeeID: integer; 
  AAttended: boolean): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindAttendeeIndex(AAttendeeID);
  if idx >= 0 then
  begin
    fattendees[idx].attended := AAttended;
    result := true;
  end;
end;

function TMeetingTaskManager.GetMeetingAttendees(AMeetingID: integer): TMeetingAttendeeArray;
var
  i, count: integer;
begin
  SetLength(result, fattendeecount);
  count := 0;
  
  for i := 0 to fattendeecount - 1 do
    if fattendees[i].meetingid = AMeetingID then
    begin
      result[count] := fattendees[i];
      inc(count);
    end;
  
  SetLength(result, count);
end;

function TMeetingTaskManager.GetAttendanceRate(AMemberID: integer; 
  ADays: integer): double;
var
  i, total, attended: integer;
  cutoff: TDateTime;
  meetingidx: integer;
begin
  result := 0.0;
  total := 0;
  attended := 0;
  cutoff := IncDay(Now, -ADays);
  
  for i := 0 to fattendeecount - 1 do
    if fattendees[i].memberid = AMemberID then
    begin
      meetingidx := FindMeetingIndex(fattendees[i].meetingid);
      if (meetingidx >= 0) and 
         (fmeetings[meetingidx].scheduledstart >= cutoff) and
         (fmeetings[meetingidx].status = msCompleted) then
      begin
        inc(total);
        if fattendees[i].attended then
          inc(attended);
      end;
    end;
  
  if total > 0 then
    result := (attended / total) * 100.0;
end;

function TMeetingTaskManager.AddAgendaItem(AMeetingID: integer; 
  const ATitle, ADescription: string; AMinutes: integer; 
  APresenterID: integer): integer;
var
  agenda: TAgendaItem;
begin
  agenda.id := fnextagendaid;
  inc(fnextagendaid);
  agenda.meetingid := AMeetingID;
  agenda.title := ATitle;
  agenda.description := ADescription;
  agenda.allocatedminutes := AMinutes;
  agenda.orderindex := fagendacount;
  agenda.presenter := APresenterID;
  agenda.completed := false;
  
  SetLength(fagendaitems, fagendacount + 1);
  fagendaitems[fagendacount] := agenda;
  inc(fagendacount);
  
  result := agenda.id;
end;

function TMeetingTaskManager.UpdateAgendaItem(AAgendaID: integer; 
  const ATitle, ADescription: string; AMinutes: integer): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindAgendaIndex(AAgendaID);
  if idx >= 0 then
  begin
    fagendaitems[idx].title := ATitle;
    fagendaitems[idx].description := ADescription;
    fagendaitems[idx].allocatedminutes := AMinutes;
    result := true;
  end;
end;

function TMeetingTaskManager.DeleteAgendaItem(AAgendaID: integer): boolean;
var
  idx, i: integer;
begin
  result := false;
  idx := FindAgendaIndex(AAgendaID);
  if idx >= 0 then
  begin
    for i := idx to fagendacount - 2 do
      fagendaitems[i] := fagendaitems[i + 1];
    dec(fagendacount);
    SetLength(fagendaitems, fagendacount);
    result := true;
  end;
end;

function TMeetingTaskManager.ReorderAgendaItem(AAgendaID: integer; 
  ANewIndex: integer): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindAgendaIndex(AAgendaID);
  if idx >= 0 then
  begin
    fagendaitems[idx].orderindex := ANewIndex;
    result := true;
  end;
end;

function TMeetingTaskManager.MarkAgendaItemComplete(AAgendaID: integer): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindAgendaIndex(AAgendaID);
  if idx >= 0 then
  begin
    fagendaitems[idx].completed := true;
    result := true;
  end;
end;

function TMeetingTaskManager.GetAgendaItems(AMeetingID: integer): TAgendaItemArray;
var
  i, count: integer;
begin
  SetLength(result, fagendacount);
  count := 0;
  
  for i := 0 to fagendacount - 1 do
    if fagendaitems[i].meetingid = AMeetingID then
    begin
      result[count] := fagendaitems[i];
      inc(count);
    end;
  
  SetLength(result, count);
end;

function TMeetingTaskManager.AddActionItem(AMeetingID: integer; 
  const ADescription: string; AAssignedTo: integer; ADueDate: TDateTime): integer;
var
  action: TMeetingActionItem;
begin
  action.id := fnextactionid;
  inc(fnextactionid);
  action.meetingid := AMeetingID;
  action.description := ADescription;
  action.assignedto := AAssignedTo;
  action.duedate := ADueDate;
  action.status := aisOpen;
  action.linkedtaskid := 0;
  action.createdat := Now;
  action.completedat := 0;
  
  SetLength(factionitems, factioncount + 1);
  factionitems[factioncount] := action;
  inc(factioncount);
  
  result := action.id;
end;

function TMeetingTaskManager.UpdateActionItemStatus(AActionID: integer;
  AStatus: TActionItemStatus): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindActionIndex(AActionID);
  if idx >= 0 then
  begin
    factionitems[idx].status := AStatus;
    if AStatus = aisCompleted then
      factionitems[idx].completedat := Now;
    result := true;
  end;
end;

function TMeetingTaskManager.LinkActionItemToTask(AActionID, ATaskID: integer): boolean;
var
  idx: integer;
begin
  result := false;
  idx := FindActionIndex(AActionID);
  if idx >= 0 then
  begin
    factionitems[idx].linkedtaskid := ATaskID;
    result := true;
  end;
end;

function TMeetingTaskManager.CreateTaskFromActionItem(AActionID: integer): integer;
var
  idx: integer;
  taskid: integer;
begin
  result := 0;
  idx := FindActionIndex(AActionID);
  if idx >= 0 then
  begin
    taskid := AddTask(
      'Action from meeting',
      factionitems[idx].description,
      tpMedium,
      factionitems[idx].duedate
    );
    factionitems[idx].linkedtaskid := taskid;
    result := taskid;
  end;
end;

function TMeetingTaskManager.GetMeetingActionItems(AMeetingID: integer): TMeetingActionItemArray;
var
  i, count: integer;
begin
  SetLength(result, factioncount);
  count := 0;
  
  for i := 0 to factioncount - 1 do
    if factionitems[i].meetingid = AMeetingID then
    begin
      result[count] := factionitems[i];
      inc(count);
    end;
  
  SetLength(result, count);
end;

function TMeetingTaskManager.GetMemberActionItems(AMemberID: integer): TMeetingActionItemArray;
var
  i, count: integer;
begin
  SetLength(result, factioncount);
  count := 0;
  
  for i := 0 to factioncount - 1 do
    if factionitems[i].assignedto = AMemberID then
    begin
      result[count] := factionitems[i];
      inc(count);
    end;
  
  SetLength(result, count);
end;

function TMeetingTaskManager.GetOpenActionItems: TMeetingActionItemArray;
var
  i, count: integer;
begin
  SetLength(result, factioncount);
  count := 0;
  
  for i := 0 to factioncount - 1 do
    if factionitems[i].status = aisOpen then
    begin
      result[count] := factionitems[i];
      inc(count);
    end;
  
  SetLength(result, count);
end;

function TMeetingTaskManager.CreateMeetingTemplate(const AName, ADescription: string;
  AMeetingType: TMeetingType; ADuration: integer;
  const AAgendaTemplate, APreFlight, APostChecklist: string): integer;
var
  template: TMeetingTemplate;
begin
  template.id := fnexttemplateid;
  inc(fnexttemplateid);
  template.name := AName;
  template.description := ADescription;
  template.meetingtype := AMeetingType;
  template.defaultduration := ADuration;
  template.agendatemplate := AAgendaTemplate;
  template.preflightchecklist := APreFlight;
  template.postmeetingchecklist := APostChecklist;
  template.createdat := Now;
  
  SetLength(ftemplates, ftemplatecount + 1);
  ftemplates[ftemplatecount] := template;
  inc(ftemplatecount);
  
  result := template.id;
end;

function TMeetingTaskManager.CreateMeetingFromTemplate(ATemplateID: integer;
  AStart: TDateTime; AOrganizerID: integer): integer;
var
  idx: integer;
  meetingid: integer;
begin
  result := 0;
  idx := FindTemplateIndex(ATemplateID);
  if idx >= 0 then
  begin
    meetingid := ScheduleMeeting(
      ftemplates[idx].name,
      ftemplates[idx].description,
      ftemplates[idx].meetingtype,
      AStart,
      ftemplates[idx].defaultduration,
      '',
      AOrganizerID
    );
    result := meetingid;
  end;
end;

function TMeetingTaskManager.GetAllTemplates: TMeetingTemplateArray;
begin
  SetLength(result, ftemplatecount);
  if ftemplatecount > 0 then
    Move(ftemplates[0], result[0], ftemplatecount * SizeOf(TMeetingTemplate));
end;

function TMeetingTaskManager.GetTemplate(ATemplateID: integer): TMeetingTemplate;
var
  idx: integer;
  empty: TMeetingTemplate;
begin
  idx := FindTemplateIndex(ATemplateID);
  if idx >= 0 then
    result := ftemplates[idx]
  else
  begin
    FillChar(empty, SizeOf(empty), 0);
    result := empty;
  end;
end;

function TMeetingTaskManager.GetMeetingStatistics(ADays: integer): string;
var
  i, total, completed, cancelled: integer;
  totalminutes: integer;
  cutoff: TDateTime;
  avgduration: double;
begin
  total := 0;
  completed := 0;
  cancelled := 0;
  totalminutes := 0;
  cutoff := IncDay(Now, -ADays);
  
  for i := 0 to fmeetingcount - 1 do
    if fmeetings[i].scheduledstart >= cutoff then
    begin
      inc(total);
      if fmeetings[i].status = msCompleted then
      begin
        inc(completed);
        totalminutes := totalminutes + CalculateMeetingDuration(fmeetings[i]);
      end;
      if fmeetings[i].status = msCancelled then
        inc(cancelled);
    end;
  
  avgduration := 0;
  if completed > 0 then
    avgduration := totalminutes / completed;
  
  result := Format('Meeting Statistics (Last %d days):'#13#10 +
    'Total meetings: %d'#13#10 +
    'Completed: %d'#13#10 +
    'Cancelled: %d'#13#10 +
    'Total meeting time: %d minutes (%.1f hours)'#13#10 +
    'Average meeting duration: %.1f minutes',
    [ADays, total, completed, cancelled, totalminutes, 
     totalminutes / 60.0, avgduration]);
end;

function TMeetingTaskManager.GetMemberMeetingLoad(AMemberID: integer; 
  ADays: integer): string;
var
  i, meetingcount, totalminutes: integer;
  cutoff: TDateTime;
  meetingidx: integer;
begin
  meetingcount := 0;
  totalminutes := 0;
  cutoff := IncDay(Now, -ADays);
  
  for i := 0 to fattendeecount - 1 do
    if fattendees[i].memberid = AMemberID then
    begin
      meetingidx := FindMeetingIndex(fattendees[i].meetingid);
      if (meetingidx >= 0) and 
         (fmeetings[meetingidx].scheduledstart >= cutoff) then
      begin
        inc(meetingcount);
        totalminutes := totalminutes + CalculateMeetingDuration(fmeetings[meetingidx]);
      end;
    end;
  
  result := Format('Member Meeting Load (Last %d days):'#13#10 +
    'Meetings attended: %d'#13#10 +
    'Total time in meetings: %d minutes (%.1f hours)'#13#10 +
    'Average per day: %.1f minutes',
    [ADays, meetingcount, totalminutes, totalminutes / 60.0,
     totalminutes / ADays]);
end;

function TMeetingTaskManager.GetMeetingEfficiency(AMeetingID: integer): string;
var
  idx, i: integer;
  agendaitems: TAgendaItemArray;
  plannedminutes, completedcount: integer;
  actualminutes: integer;
begin
  idx := FindMeetingIndex(AMeetingID);
  if idx < 0 then
  begin
    result := 'Meeting not found';
    exit;
  end;
  
  agendaitems := GetAgendaItems(AMeetingID);
  plannedminutes := 0;
  completedcount := 0;
  
  for i := 0 to Length(agendaitems) - 1 do
  begin
    plannedminutes := plannedminutes + agendaitems[i].allocatedminutes;
    if agendaitems[i].completed then
      inc(completedcount);
  end;
  
  actualminutes := CalculateMeetingDuration(fmeetings[idx]);
  
  result := Format('Meeting Efficiency Report:'#13#10 +
    'Planned duration: %d minutes'#13#10 +
    'Actual duration: %d minutes'#13#10 +
    'Agenda items: %d'#13#10 +
    'Completed items: %d (%.1f%%)'#13#10 +
    'Time variance: %d minutes',
    [plannedminutes, actualminutes, Length(agendaitems), completedcount,
     (completedcount / Length(agendaitems)) * 100.0,
     actualminutes - plannedminutes]);
end;

function TMeetingTaskManager.GetTeamMeetingMetrics(ADays: integer): string;
var
  i: integer;
  typecounts: array[TMeetingType] of integer;
  mt: TMeetingType;
  cutoff: TDateTime;
begin
  for mt := Low(TMeetingType) to High(TMeetingType) do
    typecounts[mt] := 0;
  
  cutoff := IncDay(Now, -ADays);
  
  for i := 0 to fmeetingcount - 1 do
    if fmeetings[i].scheduledstart >= cutoff then
      inc(typecounts[fmeetings[i].meetingtype]);
  
  result := Format('Team Meeting Metrics (Last %d days):'#13#10, [ADays]);
  for mt := Low(TMeetingType) to High(TMeetingType) do
    result := result + Format('%s: %d'#13#10, 
      [MeetingTypeToString(mt), typecounts[mt]]);
end;

function TMeetingTaskManager.MeetingTypeToString(AMeetingType: TMeetingType): string;
begin
  case AMeetingType of
    mtStandup: result := 'Daily Standup';
    mtPlanning: result := 'Planning';
    mtRetrospective: result := 'Retrospective';
    mtClientMeeting: result := 'Client Meeting';
    mtOneOnOne: result := 'One-on-One';
    mtReview: result := 'Review/Demo';
    mtGeneral: result := 'General';
  else
    result := 'Unknown';
  end;
end;

function TMeetingTaskManager.MeetingStatusToString(AStatus: TMeetingStatus): string;
begin
  case AStatus of
    msScheduled: result := 'Scheduled';
    msInProgress: result := 'In Progress';
    msCompleted: result := 'Completed';
    msCancelled: result := 'Cancelled';
    msRescheduled: result := 'Rescheduled';
  else
    result := 'Unknown';
  end;
end;

function TMeetingTaskManager.AttendeeStatusToString(AStatus: TAttendeeStatus): string;
begin
  case AStatus of
    asRequired: result := 'Required';
    asOptional: result := 'Optional';
    asOrganizer: result := 'Organizer';
  else
    result := 'Unknown';
  end;
end;

function TMeetingTaskManager.ActionItemStatusToString(AStatus: TActionItemStatus): string;
begin
  case AStatus of
    aisOpen: result := 'Open';
    aisInProgress: result := 'In Progress';
    aisCompleted: result := 'Completed';
    aisCancelled: result := 'Cancelled';
  else
    result := 'Unknown';
  end;
end;

function TMeetingTaskManager.SaveMeetingDataToFile(const AFilename: string): boolean;
var
  f: text;
  i: integer;
begin
  result := false;
  try
    assign(f, AFilename);
    rewrite(f);
    
    writeln(f, '[MEETINGS]');
    for i := 0 to fmeetingcount - 1 do
      writeln(f, fmeetings[i].id, '|', fmeetings[i].title);
    
    writeln(f, '[END]');
    close(f);
    result := true;
  except
    result := false;
  end;
end;

function TMeetingTaskManager.LoadMeetingDataFromFile(const AFilename: string): boolean;
begin
  result := false;
end;

end.
