
unit TaskComments;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TCommentSentiment = (csSentiment_Positive, csSentiment_Neutral, csSentiment_Negative);

  TComment = record
    ID: string;
    TaskID: string;
    Author: string;
    Content: string;
    CreatedTime: TDateTime;
    ModifiedTime: TDateTime;
    ParentCommentID: string;
    Sentiment: TCommentSentiment;
    LikeCount: integer;
    IsResolved: boolean;
  end;

  TCommentArray = array of TComment;

  TTaskCommentManagerClass = class
  private
    FComments: TCommentArray;
    FCommentCounter: integer;
    function GenerateCommentID(): string;
    function FindCommentIndex(aCommentID: string): integer;
    function AnalyzeSentiment(aText: string): TCommentSentiment;
  public
    constructor Create();
    destructor Destroy(); override;
    function AddComment(aTaskID: string; aAuthor: string; aContent: string): string;
    function AddReply(aParentCommentID: string; aAuthor: string; aContent: string): string;
    function GetComment(aCommentID: string): TComment;
    function GetTaskComments(aTaskID: string): TCommentArray;
    function GetCommentThread(aCommentID: string): TCommentArray;
    procedure EditComment(aCommentID: string; aNewContent: string);
    procedure DeleteComment(aCommentID: string);
    procedure LikeComment(aCommentID: string);
    procedure MarkAsResolved(aCommentID: string);
    function GetCommentsByAuthor(aAuthor: string): TCommentArray;
    function GetCommentsBySentiment(aSentiment: TCommentSentiment): TCommentArray;
    function SearchComments(aKeyword: string): TCommentArray;
    function GetMostLikedComments(aLimit: integer): TCommentArray;
    function GetUnresolvedComments(): TCommentArray;
    procedure ClearTaskComments(aTaskID: string);
    function GetAllComments(): TCommentArray;
    function GetCommentCount(): integer;
    procedure SelfTest();
  end;

implementation

constructor TTaskCommentManagerClass.Create();
begin
  inherited Create();
  SetLength(FComments, 0);
  FCommentCounter := 0;
end;

destructor TTaskCommentManagerClass.Destroy();
begin
  SetLength(FComments, 0);
  inherited Destroy();
end;

function TTaskCommentManagerClass.GenerateCommentID(): string;
begin
  inc(FCommentCounter);
  GenerateCommentID := 'CMMNT-' + IntToStr(FCommentCounter);
end;

function TTaskCommentManagerClass.FindCommentIndex(aCommentID: string): integer;
var
  i: integer;
begin
  FindCommentIndex := -1;
  for i := 0 to Length(FComments) - 1 do
  begin
    if FComments[i].ID = aCommentID then
    begin
      FindCommentIndex := i;
      Exit;
    end;
  end;
end;

function TTaskCommentManagerClass.AnalyzeSentiment(aText: string): TCommentSentiment;
var
  lowerText: string;
begin
  lowerText := LowerCase(aText);

  if (Pos('great', lowerText) > 0) or (Pos('excellent', lowerText) > 0) or
     (Pos('good', lowerText) > 0) or (Pos('awesome', lowerText) > 0) then
    AnalyzeSentiment := csSentiment_Positive
  else if (Pos('bad', lowerText) > 0) or (Pos('terrible', lowerText) > 0) or
          (Pos('poor', lowerText) > 0) or (Pos('issue', lowerText) > 0) then
    AnalyzeSentiment := csSentiment_Negative
  else
    AnalyzeSentiment := csSentiment_Neutral;
end;

function TTaskCommentManagerClass.AddComment(aTaskID: string; aAuthor: string; aContent: string): string;
var
  newComment: TComment;
  idx: integer;
begin
  newComment.ID := GenerateCommentID();
  newComment.TaskID := aTaskID;
  newComment.Author := aAuthor;
  newComment.Content := aContent;
  newComment.CreatedTime := Now();
  newComment.ModifiedTime := Now();
  newComment.ParentCommentID := '';
  newComment.Sentiment := AnalyzeSentiment(aContent);
  newComment.LikeCount := 0;
  newComment.IsResolved := false;

  idx := Length(FComments);
  SetLength(FComments, idx + 1);
  FComments[idx] := newComment;

  AddComment := newComment.ID;
end;

function TTaskCommentManagerClass.AddReply(aParentCommentID: string; aAuthor: string; aContent: string): string;
var
  replyID: string;
  idx: integer;
begin
  replyID := AddComment('', aAuthor, aContent);
  idx := FindCommentIndex(replyID);

  if idx >= 0 then
  begin
    FComments[idx].ParentCommentID := aParentCommentID;
  end;

  AddReply := replyID;
end;

function TTaskCommentManagerClass.GetComment(aCommentID: string): TComment;
var
  idx: integer;
  emptyComment: TComment;
begin
  FillChar(emptyComment, SizeOf(emptyComment), 0);
  idx := FindCommentIndex(aCommentID);
  if idx >= 0 then
  begin
    GetComment := FComments[idx];
  end
  else
  begin
    GetComment := emptyComment;
  end;
end;

function TTaskCommentManagerClass.GetTaskComments(aTaskID: string): TCommentArray;
var
  i, count: integer;
  commentArray: TCommentArray;
begin
  count := 0;
  SetLength(commentArray, 0);
  for i := 0 to Length(FComments) - 1 do
  begin
    if FComments[i].TaskID = aTaskID then
    begin
      SetLength(commentArray, count + 1);
      commentArray[count] := FComments[i];
      inc(count);
    end;
  end;
  GetTaskComments := commentArray;
end;

function TTaskCommentManagerClass.GetCommentThread(aCommentID: string): TCommentArray;
var
  i, count: integer;
  threadArray: TCommentArray;
begin
  count := 0;
  SetLength(threadArray, 0);

  for i := 0 to Length(FComments) - 1 do
  begin
    if (FComments[i].ID = aCommentID) or (FComments[i].ParentCommentID = aCommentID) then
    begin
      SetLength(threadArray, count + 1);
      threadArray[count] := FComments[i];
      inc(count);
    end;
  end;

  GetCommentThread := threadArray;
end;

procedure TTaskCommentManagerClass.EditComment(aCommentID: string; aNewContent: string);
var
  idx: integer;
begin
  idx := FindCommentIndex(aCommentID);
  if idx >= 0 then
  begin
    FComments[idx].Content := aNewContent;
    FComments[idx].ModifiedTime := Now();
    FComments[idx].Sentiment := AnalyzeSentiment(aNewContent);
  end;
end;

procedure TTaskCommentManagerClass.DeleteComment(aCommentID: string);
var
  idx, i: integer;
begin
  idx := FindCommentIndex(aCommentID);
  if idx >= 0 then
  begin
    for i := idx to Length(FComments) - 2 do
      FComments[i] := FComments[i + 1];
    SetLength(FComments, Length(FComments) - 1);
  end;
end;

procedure TTaskCommentManagerClass.LikeComment(aCommentID: string);
var
  idx: integer;
begin
  idx := FindCommentIndex(aCommentID);
  if idx >= 0 then
    inc(FComments[idx].LikeCount);
end;

procedure TTaskCommentManagerClass.MarkAsResolved(aCommentID: string);
var
  idx: integer;
begin
  idx := FindCommentIndex(aCommentID);
  if idx >= 0 then
    FComments[idx].IsResolved := true;
end;

function TTaskCommentManagerClass.GetCommentsByAuthor(aAuthor: string): TCommentArray;
var
  i, count: integer;
  commentArray: TCommentArray;
begin
  count := 0;
  SetLength(commentArray, 0);
  for i := 0 to Length(FComments) - 1 do
  begin
    if FComments[i].Author = aAuthor then
    begin
      SetLength(commentArray, count + 1);
      commentArray[count] := FComments[i];
      inc(count);
    end;
  end;
  GetCommentsByAuthor := commentArray;
end;

function TTaskCommentManagerClass.GetCommentsBySentiment(aSentiment: TCommentSentiment): TCommentArray;
var
  i, count: integer;
  commentArray: TCommentArray;
begin
  count := 0;
  SetLength(commentArray, 0);
  for i := 0 to Length(FComments) - 1 do
  begin
    if FComments[i].Sentiment = aSentiment then
    begin
      SetLength(commentArray, count + 1);
      commentArray[count] := FComments[i];
      inc(count);
    end;
  end;
  GetCommentsBySentiment := commentArray;
end;

function TTaskCommentManagerClass.SearchComments(aKeyword: string): TCommentArray;
var
  i, count: integer;
  commentArray: TCommentArray;
  lowerKeyword, lowerContent: string;
begin
  count := 0;
  SetLength(commentArray, 0);
  lowerKeyword := LowerCase(aKeyword);

  for i := 0 to Length(FComments) - 1 do
  begin
    lowerContent := LowerCase(FComments[i].Content);
    if Pos(lowerKeyword, lowerContent) > 0 then
    begin
      SetLength(commentArray, count + 1);
      commentArray[count] := FComments[i];
      inc(count);
    end;
  end;

  SearchComments := commentArray;
end;

function TTaskCommentManagerClass.GetMostLikedComments(aLimit: integer): TCommentArray;
var
  i, j, count: integer;
  commentArray: TCommentArray;
  tempComment: TComment;
begin
  SetLength(commentArray, Length(FComments));
  for i := 0 to Length(FComments) - 1 do
    commentArray[i] := FComments[i];

  { Bubble sort by likes }
  for i := 0 to Length(commentArray) - 1 do
  begin
    for j := 0 to Length(commentArray) - 2 - i do
    begin
      if commentArray[j].LikeCount < commentArray[j + 1].LikeCount then
      begin
        tempComment := commentArray[j];
        commentArray[j] := commentArray[j + 1];
        commentArray[j + 1] := tempComment;
      end;
    end;
  end;

  count := 0;
  if aLimit > Length(commentArray) then
    SetLength(commentArray, Length(commentArray))
  else if aLimit > 0 then
    SetLength(commentArray, aLimit);

  GetMostLikedComments := commentArray;
end;

function TTaskCommentManagerClass.GetUnresolvedComments(): TCommentArray;
var
  i, count: integer;
  commentArray: TCommentArray;
begin
  count := 0;
  SetLength(commentArray, 0);
  for i := 0 to Length(FComments) - 1 do
  begin
    if not FComments[i].IsResolved then
    begin
      SetLength(commentArray, count + 1);
      commentArray[count] := FComments[i];
      inc(count);
    end;
  end;
  GetUnresolvedComments := commentArray;
end;

procedure TTaskCommentManagerClass.ClearTaskComments(aTaskID: string);
var
  i, writeIdx: integer;
begin
  writeIdx := 0;
  for i := 0 to Length(FComments) - 1 do
  begin
    if FComments[i].TaskID <> aTaskID then
    begin
      FComments[writeIdx] := FComments[i];
      inc(writeIdx);
    end;
  end;
  SetLength(FComments, writeIdx);
end;

function TTaskCommentManagerClass.GetAllComments(): TCommentArray;
begin
  GetAllComments := FComments;
end;

function TTaskCommentManagerClass.GetCommentCount(): integer;
begin
  GetCommentCount := Length(FComments);
end;

procedure TTaskCommentManagerClass.SelfTest();
var
  cid1, cid2, cid3: string;
  comments: TCommentArray;
  likedComments: TCommentArray;
begin
  WriteLn('--- Task Comments Manager Self Test ---');

  { Test adding comments }
  cid1 := AddComment('TASK-001', 'john@company.com', 'This is a great solution!');
  cid2 := AddComment('TASK-001', 'jane@company.com', 'I have an issue with this approach');
  cid3 := AddComment('TASK-002', 'bob@company.com', 'Neutral comment here');

  WriteLn('✓ Created 3 comments');

  { Test retrieving comments }
  comments := GetTaskComments('TASK-001');
  if Length(comments) = 2 then
    WriteLn('✓ Task comment retrieval works')
  else
    WriteLn('✗ Task comment retrieval failed');

  { Test comment editing }
  EditComment(cid1, 'This is an excellent solution!');
  WriteLn('✓ Comment editing works');

  { Test liking comments }
  LikeComment(cid1);
  LikeComment(cid1);
  WriteLn('✓ Comment liking works: 2 likes');

  { Test comment search }
  comments := SearchComments('solution');
  if Length(comments) > 0 then
    WriteLn('✓ Comment search works')
  else
    WriteLn('✗ Comment search failed');

  { Test most liked }
  likedComments := GetMostLikedComments(5);
  WriteLn('✓ Most liked comments: ', Length(likedComments));

  { Test mark resolved }
  MarkAsResolved(cid2);
  WriteLn('✓ Mark resolved works');

  WriteLn('--- Comments Manager Tests Complete ---');
  WriteLn('');
end;

end.
