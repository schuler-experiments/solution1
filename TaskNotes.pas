
unit TaskNotes;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils, TaskTypes;

type
  { Task note/comment with timestamp }
  TTaskNote = record
    noteId: integer;
    taskId: integer;
    noteText: string;
    author: string;
    createdDate: TDateTime;
  end;

  TTaskNoteArray = array of TTaskNote;

  { Task status change history record }
  TTaskHistoryEntry = record
    entryId: integer;
    taskId: integer;
    oldStatus: TTaskStatus;
    newStatus: TTaskStatus;
    changedDate: TDateTime;
    changedBy: string;
  end;

  TTaskHistoryArray = array of TTaskHistoryEntry;

  { Task notes manager }
  TTaskNotesManagerClass = class
  private
    notes: TTaskNoteArray;
    nextNoteId: integer;
  public
    constructor Create();
    destructor Destroy();
    function AddNote(taskId: integer; noteText, author: string): integer;
    function GetNotesForTask(taskId: integer): TTaskNoteArray;
    function GetNoteCount(taskId: integer): integer;
    function DeleteNote(noteId: integer): boolean;
    function UpdateNote(noteId: integer; newText: string): boolean;
    procedure SelfTest();
  end;

  { Task history tracker }
  TTaskHistoryTrackerClass = class
  private
    history: TTaskHistoryArray;
    nextEntryId: integer;
  public
    constructor Create();
    destructor Destroy();
    procedure RecordStatusChange(taskId: integer; oldStatus, newStatus: TTaskStatus; changedBy: string);
    function GetTaskHistory(taskId: integer): TTaskHistoryArray;
    function GetChangeCount(taskId: integer): integer;
    function GetLastStatusChange(taskId: integer): TTaskHistoryEntry;
    procedure ClearHistory(taskId: integer);
    procedure SelfTest();
  end;

implementation

{ ===== TTaskNotesManagerClass ===== }

constructor TTaskNotesManagerClass.Create();
begin
  inherited Create();
  SetLength(notes, 0);
  nextNoteId := 1;
end;

destructor TTaskNotesManagerClass.Destroy();
begin
  SetLength(notes, 0);
  inherited Destroy();
end;

function TTaskNotesManagerClass.AddNote(taskId: integer; noteText, author: string): integer;
var
  len: integer;
  newNote: TTaskNote;
begin
  newNote.noteId := nextNoteId;
  newNote.taskId := taskId;
  newNote.noteText := noteText;
  newNote.author := author;
  newNote.createdDate := Now();

  len := Length(notes);
  SetLength(notes, len + 1);
  notes[len] := newNote;
  result := nextNoteId;
  inc(nextNoteId);
end;

function TTaskNotesManagerClass.GetNotesForTask(taskId: integer): TTaskNoteArray;
var
  i, count: integer;
  resultArr: TTaskNoteArray;
begin
  count := 0;
  SetLength(resultArr, 0);

  for i := 0 to Length(notes) - 1 do
  begin
    if notes[i].taskId = taskId then
    begin
      SetLength(resultArr, count + 1);
      resultArr[count] := notes[i];
      inc(count);
    end;
  end;

  result := resultArr;
end;

function TTaskNotesManagerClass.GetNoteCount(taskId: integer): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(notes) - 1 do
  begin
    if notes[i].taskId = taskId then
      inc(count);
  end;
  result := count;
end;

function TTaskNotesManagerClass.DeleteNote(noteId: integer): boolean;
var
  i, j: integer;
begin
  for i := 0 to Length(notes) - 1 do
  begin
    if notes[i].noteId = noteId then
    begin
      for j := i to Length(notes) - 2 do
        notes[j] := notes[j + 1];
      SetLength(notes, Length(notes) - 1);
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TTaskNotesManagerClass.UpdateNote(noteId: integer; newText: string): boolean;
var
  i: integer;
begin
  for i := 0 to Length(notes) - 1 do
  begin
    if notes[i].noteId = noteId then
    begin
      notes[i].noteText := newText;
      result := true;
      exit;
    end;
  end;
  result := false;
end;

procedure TTaskNotesManagerClass.SelfTest();
var
  notesMgr: TTaskNotesManagerClass;
  noteId1, noteId2: integer;
begin
  WriteLn('=== TaskNotes Manager Self Test ===');
  notesMgr := TTaskNotesManagerClass.Create();

  { Add notes to task 1 }
  noteId1 := notesMgr.AddNote(1, 'First note for task 1', 'Admin');
  noteId2 := notesMgr.AddNote(1, 'Second note for task 1', 'User');
  notesMgr.AddNote(2, 'Note for task 2', 'Admin');

  WriteLn('Added 3 notes');
  WriteLn('Notes for task 1: ' + IntToStr(notesMgr.GetNoteCount(1)));
  WriteLn('Notes for task 2: ' + IntToStr(notesMgr.GetNoteCount(2)));

  { Update a note }
  if notesMgr.UpdateNote(noteId1, 'Updated first note') then
    WriteLn('✓ Note updated successfully');

  { Delete a note }
  if notesMgr.DeleteNote(noteId2) then
    WriteLn('✓ Note deleted successfully');

  WriteLn('Notes for task 1 after deletion: ' + IntToStr(notesMgr.GetNoteCount(1)));

  notesMgr.Destroy();
  WriteLn('=== TaskNotes Manager Self Test Complete ===');
end;

{ ===== TTaskHistoryTrackerClass ===== }

constructor TTaskHistoryTrackerClass.Create();
begin
  inherited Create();
  SetLength(history, 0);
  nextEntryId := 1;
end;

destructor TTaskHistoryTrackerClass.Destroy();
begin
  SetLength(history, 0);
  inherited Destroy();
end;

procedure TTaskHistoryTrackerClass.RecordStatusChange(taskId: integer; oldStatus, newStatus: TTaskStatus; changedBy: string);
var
  len: integer;
  entry: TTaskHistoryEntry;
begin
  entry.entryId := nextEntryId;
  entry.taskId := taskId;
  entry.oldStatus := oldStatus;
  entry.newStatus := newStatus;
  entry.changedDate := Now();
  entry.changedBy := changedBy;

  len := Length(history);
  SetLength(history, len + 1);
  history[len] := entry;
  inc(nextEntryId);
end;

function TTaskHistoryTrackerClass.GetTaskHistory(taskId: integer): TTaskHistoryArray;
var
  i, count: integer;
  resultArr: TTaskHistoryArray;
begin
  count := 0;
  SetLength(resultArr, 0);

  for i := 0 to Length(history) - 1 do
  begin
    if history[i].taskId = taskId then
    begin
      SetLength(resultArr, count + 1);
      resultArr[count] := history[i];
      inc(count);
    end;
  end;

  result := resultArr;
end;

function TTaskHistoryTrackerClass.GetChangeCount(taskId: integer): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(history) - 1 do
  begin
    if history[i].taskId = taskId then
      inc(count);
  end;
  result := count;
end;

function TTaskHistoryTrackerClass.GetLastStatusChange(taskId: integer): TTaskHistoryEntry;
var
  i, lastIdx: integer;
begin
  lastIdx := -1;
  for i := 0 to Length(history) - 1 do
  begin
    if history[i].taskId = taskId then
      lastIdx := i;
  end;

  if lastIdx >= 0 then
    result := history[lastIdx]
  else
  begin
    result.entryId := -1;
    result.taskId := -1;
  end;
end;

procedure TTaskHistoryTrackerClass.ClearHistory(taskId: integer);
var
  i, j: integer;
begin
  i := 0;
  while i < Length(history) do
  begin
    if history[i].taskId = taskId then
    begin
      for j := i to Length(history) - 2 do
        history[j] := history[j + 1];
      SetLength(history, Length(history) - 1);
    end
    else
      inc(i);
  end;
end;

procedure TTaskHistoryTrackerClass.SelfTest();
var
  historyMgr: TTaskHistoryTrackerClass;
begin
  WriteLn('=== TaskHistory Tracker Self Test ===');
  historyMgr := TTaskHistoryTrackerClass.Create();

  { Record some status changes }
  historyMgr.RecordStatusChange(1, tsNotStarted, tsInProgress, 'Admin');
  historyMgr.RecordStatusChange(1, tsInProgress, tsCompleted, 'User');
  historyMgr.RecordStatusChange(2, tsNotStarted, tsInProgress, 'Admin');

  WriteLn('Recorded 3 status changes');
  WriteLn('Changes for task 1: ' + IntToStr(historyMgr.GetChangeCount(1)));
  WriteLn('Changes for task 2: ' + IntToStr(historyMgr.GetChangeCount(2)));

  historyMgr.Destroy();
  WriteLn('=== TaskHistory Tracker Self Test Complete ===');
end;

end.
