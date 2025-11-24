unit TaskCollaboration;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  { String array type }
  TStringArray = array of string;

  TTaskComment = record
    CommentID: integer;
    TaskID: integer;
    Author: string;
    Content: string;
    CreatedDate: TDateTime;
    UpdatedDate: TDateTime;
    MentionedUsers: TStringArray;
    IsResolved: boolean;
    ReplyToID: integer;
  end;

  { Array type for comments }
  TTaskCommentArray = array of TTaskComment;

  { Collaboration activity record }
  TCollaborationActivity = record
    ActivityID: integer;
    TaskID: integer;
    UserName: string;
    ActivityType: string;
    Description: string;
    Timestamp: TDateTime;
  end;

  { Array type for activities }
  TCollaborationActivityArray = array of TCollaborationActivity;

  { Collaboration metrics record }
  TCollaborationMetrics = record
    TotalComments: integer;
    TotalMentions: integer;
    ActiveCollaborators: integer;
    UnresolvedComments: integer;
    AverageResponseTime: double;
    CollaborationScore: double;
  end;

  { Collaboration manager class }
  TTaskCollaborationManagerClass = class
  private
    fComments: TTaskCommentArray;
    fActivities: TCollaborationActivityArray;
    fNextCommentID: integer;
    fNextActivityID: integer;
    function FindCommentIndex(id: integer): integer;
    function CountMentions(content: string): integer;
    function ParseMentions(content: string): TStringArray;
  public
    constructor Create();
    destructor Destroy();
    function AddComment(taskId: integer; author, content: string): integer;
    function ReplyToComment(taskId, replyToId: integer; author, content: string): integer;
    function GetTaskComments(taskId: integer): TTaskCommentArray;
    function GetCommentReplies(commentId: integer): TTaskCommentArray;
    function GetCommentCount(taskId: integer): integer;
    function UpdateComment(commentId: integer; newContent: string): boolean;
    function ResolveComment(commentId: integer): boolean;
    function DeleteComment(commentId: integer): boolean;
    function RecordActivity(taskId: integer; userName, activityType, description: string): integer;
    function GetTaskActivity(taskId: integer): TCollaborationActivityArray;
    function GetCollaborationMetrics(taskId: integer): TCollaborationMetrics;
    function GetMostActiveUsers(taskId: integer): TStringArray;
    function GetCollaborationHealth(taskId: integer): string;
    function GetUnresolvedComments(taskId: integer): TTaskCommentArray;
    procedure SelfTest();
  end;

implementation

constructor TTaskCollaborationManagerClass.Create();
begin
  SetLength(fComments, 0);
  SetLength(fActivities, 0);
  fNextCommentID := 1;
  fNextActivityID := 1;
end;

destructor TTaskCollaborationManagerClass.Destroy();
begin
  SetLength(fComments, 0);
  SetLength(fActivities, 0);
  inherited Destroy();
end;

function TTaskCollaborationManagerClass.FindCommentIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(fComments) - 1 do
  begin
    if fComments[i].CommentID = id then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskCollaborationManagerClass.CountMentions(content: string): integer;
var
  count, i: integer;
begin
  count := 0;
  for i := 1 to Length(content) - 1 do
  begin
    if (content[i] = '@') and (content[i+1] <> ' ') then
      count := count + 1;
  end;
  result := count;
end;

function TTaskCollaborationManagerClass.ParseMentions(content: string): TStringArray;
var
  mentions: TStringArray;
  i, count: integer;
begin
  SetLength(mentions, 0);
  count := 0;
  i := 1;
  while i < Length(content) do
  begin
    if content[i] = '@' then
    begin
      SetLength(mentions, count + 1);
      mentions[count] := '@' + copy(content, i+1, 20);
      count := count + 1;
      i := i + 20;
    end
    else
      i := i + 1;
  end;
  result := mentions;
end;

function TTaskCollaborationManagerClass.AddComment(taskId: integer; author, content: string): integer;
var
  newComment: TTaskComment;
begin
  newComment.CommentID := fNextCommentID;
  newComment.TaskID := taskId;
  newComment.Author := author;
  newComment.Content := content;
  newComment.CreatedDate := Now();
  newComment.UpdatedDate := Now();
  newComment.MentionedUsers := ParseMentions(content);
  newComment.IsResolved := false;
  newComment.ReplyToID := 0;

  SetLength(fComments, Length(fComments) + 1);
  fComments[Length(fComments) - 1] := newComment;

  RecordActivity(taskId, author, 'Comment Added', 'Added comment: ' + copy(content, 1, 50));

  result := fNextCommentID;
  fNextCommentID := fNextCommentID + 1;
end;

function TTaskCollaborationManagerClass.ReplyToComment(taskId, replyToId: integer; author, content: string): integer;
var
  newComment: TTaskComment;
begin
  newComment.CommentID := fNextCommentID;
  newComment.TaskID := taskId;
  newComment.Author := author;
  newComment.Content := content;
  newComment.CreatedDate := Now();
  newComment.UpdatedDate := Now();
  newComment.MentionedUsers := ParseMentions(content);
  newComment.IsResolved := false;
  newComment.ReplyToID := replyToId;

  SetLength(fComments, Length(fComments) + 1);
  fComments[Length(fComments) - 1] := newComment;

  result := fNextCommentID;
  fNextCommentID := fNextCommentID + 1;
end;

function TTaskCollaborationManagerClass.GetTaskComments(taskId: integer): TTaskCommentArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  for i := 0 to Length(fComments) - 1 do
  begin
    if (fComments[i].TaskID = taskId) and (fComments[i].ReplyToID = 0) then
    begin
      SetLength(result, count + 1);
      result[count] := fComments[i];
      count := count + 1;
    end;
  end;
end;

function TTaskCollaborationManagerClass.GetCommentReplies(commentId: integer): TTaskCommentArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  for i := 0 to Length(fComments) - 1 do
  begin
    if fComments[i].ReplyToID = commentId then
    begin
      SetLength(result, count + 1);
      result[count] := fComments[i];
      count := count + 1;
    end;
  end;
end;

function TTaskCollaborationManagerClass.GetCommentCount(taskId: integer): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(fComments) - 1 do
  begin
    if fComments[i].TaskID = taskId then
      count := count + 1;
  end;
  result := count;
end;

function TTaskCollaborationManagerClass.UpdateComment(commentId: integer; newContent: string): boolean;
var
  idx: integer;
begin
  idx := FindCommentIndex(commentId);
  if idx >= 0 then
  begin
    fComments[idx].Content := newContent;
    fComments[idx].UpdatedDate := Now();
    result := true;
  end
  else
    result := false;
end;

function TTaskCollaborationManagerClass.ResolveComment(commentId: integer): boolean;
var
  idx: integer;
begin
  idx := FindCommentIndex(commentId);
  if idx >= 0 then
  begin
    fComments[idx].IsResolved := true;
    result := true;
  end
  else
    result := false;
end;

function TTaskCollaborationManagerClass.DeleteComment(commentId: integer): boolean;
var
  idx, i: integer;
begin
  idx := FindCommentIndex(commentId);
  if idx >= 0 then
  begin
    for i := idx to Length(fComments) - 2 do
      fComments[i] := fComments[i + 1];
    SetLength(fComments, Length(fComments) - 1);
    result := true;
  end
  else
    result := false;
end;

function TTaskCollaborationManagerClass.RecordActivity(taskId: integer; userName, activityType, description: string): integer;
var
  newActivity: TCollaborationActivity;
begin
  newActivity.ActivityID := fNextActivityID;
  newActivity.TaskID := taskId;
  newActivity.UserName := userName;
  newActivity.ActivityType := activityType;
  newActivity.Description := description;
  newActivity.Timestamp := Now();

  SetLength(fActivities, Length(fActivities) + 1);
  fActivities[Length(fActivities) - 1] := newActivity;

  result := fNextActivityID;
  fNextActivityID := fNextActivityID + 1;
end;

function TTaskCollaborationManagerClass.GetTaskActivity(taskId: integer): TCollaborationActivityArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  for i := 0 to Length(fActivities) - 1 do
  begin
    if fActivities[i].TaskID = taskId then
    begin
      SetLength(result, count + 1);
      result[count] := fActivities[i];
      count := count + 1;
    end;
  end;
end;

function TTaskCollaborationManagerClass.GetCollaborationMetrics(taskId: integer): TCollaborationMetrics;
var
  i: integer;
  metrics: TCollaborationMetrics;
  comments: TTaskCommentArray;
begin
  comments := GetTaskComments(taskId);
  metrics.TotalComments := Length(fComments);
  metrics.TotalMentions := 0;
  metrics.UnresolvedComments := 0;
  metrics.ActiveCollaborators := 0;
  metrics.AverageResponseTime := 0;

  for i := 0 to Length(fComments) - 1 do
  begin
    if fComments[i].TaskID = taskId then
    begin
      metrics.TotalMentions := metrics.TotalMentions + CountMentions(fComments[i].Content);
      if not fComments[i].IsResolved then
        metrics.UnresolvedComments := metrics.UnresolvedComments + 1;
    end;
  end;

  if metrics.TotalComments > 0 then
    metrics.CollaborationScore := (Length(comments) / metrics.TotalComments) * 100
  else
    metrics.CollaborationScore := 0;

  result := metrics;
end;

function TTaskCollaborationManagerClass.GetMostActiveUsers(taskId: integer): TStringArray;
var
  users: TStringArray;
  counts: array of integer;
  i, j, userIdx: integer;
begin
  SetLength(users, 0);
  SetLength(counts, 0);

  for i := 0 to Length(fComments) - 1 do
  begin
    if fComments[i].TaskID = taskId then
    begin
      userIdx := -1;
      for j := 0 to Length(users) - 1 do
      begin
        if users[j] = fComments[i].Author then
        begin
          userIdx := j;
          break;
        end;
      end;

      if userIdx >= 0 then
        counts[userIdx] := counts[userIdx] + 1
      else
      begin
        SetLength(users, Length(users) + 1);
        SetLength(counts, Length(counts) + 1);
        users[Length(users) - 1] := fComments[i].Author;
        counts[Length(counts) - 1] := 1;
      end;
    end;
  end;

  result := users;
end;

function TTaskCollaborationManagerClass.GetCollaborationHealth(taskId: integer): string;
var
  metrics: TCollaborationMetrics;
  health: string;
begin
  metrics := GetCollaborationMetrics(taskId);

  if metrics.TotalComments = 0 then
    health := 'No collaboration yet'
  else if metrics.UnresolvedComments > 5 then
    health := 'High discussion activity - many unresolved comments'
  else if metrics.TotalMentions > metrics.TotalComments then
    health := 'Strong team engagement with mentions'
  else
    health := 'Healthy collaboration';

  result := health;
end;

function TTaskCollaborationManagerClass.GetUnresolvedComments(taskId: integer): TTaskCommentArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  for i := 0 to Length(fComments) - 1 do
  begin
    if (fComments[i].TaskID = taskId) and (not fComments[i].IsResolved) then
    begin
      SetLength(result, count + 1);
      result[count] := fComments[i];
      count := count + 1;
    end;
  end;
end;

procedure TTaskCollaborationManagerClass.SelfTest();
var
  collab_mgr: TTaskCollaborationManagerClass;
  commentId1, commentId2, replyId: integer;
  metrics: TCollaborationMetrics;
begin
  WriteLn('');
  WriteLn('=== TaskCollaboration Manager Self Test ===');

  collab_mgr := TTaskCollaborationManagerClass.Create();

  try
    { Add comments }
    commentId1 := collab_mgr.AddComment(1, 'Alice', 'Let us discuss the @requirements for this task');
    commentId2 := collab_mgr.AddComment(1, 'Bob', 'I agree with @Alice, we need more clarification');
    replyId := collab_mgr.ReplyToComment(1, commentId1, 'Charlie', '@Alice and @Bob, I have prepared the documentation');

    WriteLn('Added 3 comments with mentions');

    { Record activities }
    collab_mgr.RecordActivity(1, 'Alice', 'Status Changed', 'Changed status to In Progress');
    collab_mgr.RecordActivity(1, 'Bob', 'Assignment', 'Assigned to Bob');

    WriteLn('Recorded 2 activities');

    { Get collaboration metrics }
    metrics := collab_mgr.GetCollaborationMetrics(1);
    WriteLn('Total Comments: ' + IntToStr(metrics.TotalComments));
    WriteLn('Total Mentions: ' + IntToStr(metrics.TotalMentions));
    WriteLn('Collaboration Score: ' + FormatFloat('0.0', metrics.CollaborationScore));

    { Check collaboration health }
    WriteLn('Collaboration Health: ' + collab_mgr.GetCollaborationHealth(1));

    { Get unresolved comments }
    WriteLn('Unresolved Comments: ' + IntToStr(Length(collab_mgr.GetUnresolvedComments(1))));

    { Resolve a comment }
    if collab_mgr.ResolveComment(commentId1) then
      WriteLn('✓ Resolved comment 1');

    WriteLn('=== TaskCollaboration Manager Self Test Complete ===');
  finally
    collab_mgr.Free();
  end;
end;

end.
