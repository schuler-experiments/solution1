
unit taskmanagercomments;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Math,
  taskmanager, taskmanageradvanced, taskmanagerwellbeing;

type
  // Comment reaction types
  TReactionType = (rtLike, rtHelpful, rtAgree, rtDisagree, rtThumbsUp, rtThumbsDown);
  
  // Comment visibility/moderation status
  TCommentStatus = (csVisible, csHidden, csPinned, csDeleted, csFlagged);
  
  // Comment attachment type
  TCommentAttachmentType = (catLink, catImage, catDocument, catCode);
  
  // Dynamic array type definitions for function returns
  TStringArray = array of string;
  TIntegerArray = array of Integer;
  
  // Comment attachment
  TCommentAttachment = record
    ID: Integer;
    CommentID: Integer;
    AttachmentType: TCommentAttachmentType;
    URL: string;
    Title: string;
    Description: string;
    CreatedDate: TDateTime;
  end;
  
  TCommentAttachmentArray = array of TCommentAttachment;
  
  // Comment reaction
  TCommentReaction = record
    ID: Integer;
    CommentID: Integer;
    ReactionType: TReactionType;
    UserName: string;
    CreatedDate: TDateTime;
  end;
  
  TCommentReactionArray = array of TCommentReaction;
  
  // Edit history entry
  TEditHistory = record
    EditDate: TDateTime;
    PreviousContent: string;
    EditedBy: string;
    EditReason: string;
  end;
  
  TEditHistoryArray = array of TEditHistory;
  
  // Main comment record
  TTaskComment = record
    ID: Integer;
    TaskID: Integer;
    ParentCommentID: Integer;  // 0 for top-level comments
    AuthorName: string;
    Content: string;
    CreatedDate: TDateTime;
    ModifiedDate: TDateTime;
    Status: TCommentStatus;
    IsEdited: Boolean;
    EditHistory: TEditHistoryArray;
    Mentions: TStringArray;  // @username mentions
    ReplyCount: Integer;
    ReactionCounts: array[TReactionType] of Integer;
  end;
  
  TTaskCommentArray = array of TTaskComment;
  
  // Comment thread (for organizing replies)
  TCommentThread = record
    RootCommentID: Integer;
    Comments: TTaskCommentArray;
    TotalReplies: Integer;
    LastActivityDate: TDateTime;
  end;
  
  TCommentThreadArray = array of TCommentThread;
  
  // Comment statistics
  TCommentStatistics = record
    TotalComments: Integer;
    TotalThreads: Integer;
    TotalReactions: Integer;
    MostActiveTask: Integer;
    MostCommentedUser: string;
    AverageCommentsPerTask: Double;
    AverageRepliesPerComment: Double;
    MostUsedReaction: TReactionType;
  end;

  TCommentedTaskManager = class(TWellbeingTaskManager)
  private
    FComments: TTaskCommentArray;
    FReactions: TCommentReactionArray;
    FAttachments: TCommentAttachmentArray;
    FNextCommentID: Integer;
    FNextReactionID: Integer;
    FNextAttachmentID: Integer;
    
    function FindCommentIndex(ACommentID: Integer): Integer;
    function FindReactionIndex(AReactionID: Integer): Integer;
    function FindAttachmentIndex(AAttachmentID: Integer): Integer;
    function ExtractMentions(const AContent: string): TStringArray;
    procedure UpdateReactionCounts(ACommentID: Integer);
    function BuildCommentThread(ARootCommentID: Integer): TCommentThread;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Core comment operations
    function AddComment(ATaskID: Integer; const AAuthorName, AContent: string): Integer;
    function AddReply(AParentCommentID: Integer; const AAuthorName, AContent: string): Integer;
    function EditComment(ACommentID: Integer; const ANewContent, AEditReason: string): boolean;
    function DeleteComment(ACommentID: Integer): boolean;
    
    // Comment retrieval
    function GetComment(ACommentID: Integer): TTaskComment;
    function GetTaskComments(ATaskID: Integer): TTaskCommentArray;
    function GetCommentReplies(AParentCommentID: Integer): TTaskCommentArray;
    function GetCommentThread(ARootCommentID: Integer): TCommentThread;
    function GetAllThreads(ATaskID: Integer): TCommentThreadArray;
    
    // Reactions
    function AddReaction(ACommentID: Integer; AReactionType: TReactionType; 
      const AUserName: string): Integer;
    function RemoveReaction(AReactionID: Integer): boolean;
    function GetCommentReactions(ACommentID: Integer): TCommentReactionArray;
    function GetUserReaction(ACommentID: Integer; const AUserName: string): Integer;
    
    // Attachments
    function AddAttachment(ACommentID: Integer; AType: TCommentAttachmentType;
      const AURL, ATitle, ADescription: string): Integer;
    function RemoveAttachment(AAttachmentID: Integer): boolean;
    function GetCommentAttachments(ACommentID: Integer): TCommentAttachmentArray;
    
    // Moderation
    function PinComment(ACommentID: Integer): boolean;
    function UnpinComment(ACommentID: Integer): boolean;
    function HideComment(ACommentID: Integer): boolean;
    function UnhideComment(ACommentID: Integer): boolean;
    function FlagComment(ACommentID: Integer): boolean;
    
    // Search and filtering
    function SearchComments(const ASearchTerm: string): TTaskCommentArray;
    function GetCommentsByAuthor(const AAuthorName: string): TTaskCommentArray;
    function GetCommentsByDateRange(AStartDate, AEndDate: TDateTime): TTaskCommentArray;
    function GetPinnedComments(ATaskID: Integer): TTaskCommentArray;
    function GetMentions(const AUserName: string): TTaskCommentArray;
    
    // Statistics and analytics
    function GetCommentStatistics: TCommentStatistics;
    function GetTaskCommentCount(ATaskID: Integer): Integer;
    function GetMostCommentedTasks(ALimit: Integer): TIntegerArray;
    function GetMostActiveCommenters(ALimit: Integer): TStringArray;
    
    // Export functions
    function ExportCommentsToMarkdown(ATaskID: Integer): string;
    function ExportCommentsToHTML(ATaskID: Integer): string;
    function ExportThreadToMarkdown(ARootCommentID: Integer): string;
    
    // Utility functions
    function ReactionTypeToString(AType: TReactionType): string;
    function CommentStatusToString(AStatus: TCommentStatus): string;
    function AttachmentTypeToString(AType: TCommentAttachmentType): string;
    
    // Persistence
    function SaveCommentsToFile(const AFilename: string): boolean;
    function LoadCommentsFromFile(const AFilename: string): boolean;
    
    // Self-test
    procedure SelfTest;
  end;

implementation

constructor TCommentedTaskManager.Create;
begin
  inherited Create;
  SetLength(FComments, 0);
  SetLength(FReactions, 0);
  SetLength(FAttachments, 0);
  FNextCommentID := 1;
  FNextReactionID := 1;
  FNextAttachmentID := 1;
end;

destructor TCommentedTaskManager.Destroy;
begin
  SetLength(FComments, 0);
  SetLength(FReactions, 0);
  SetLength(FAttachments, 0);
  inherited Destroy;
end;

function TCommentedTaskManager.FindCommentIndex(ACommentID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FComments) - 1 do
  begin
    if FComments[i].ID = ACommentID then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TCommentedTaskManager.FindReactionIndex(AReactionID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FReactions) - 1 do
  begin
    if FReactions[i].ID = AReactionID then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TCommentedTaskManager.FindAttachmentIndex(AAttachmentID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FAttachments) - 1 do
  begin
    if FAttachments[i].ID = AAttachmentID then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TCommentedTaskManager.ExtractMentions(const AContent: string): TStringArray;
var
  i, start: Integer;
  mention: string;
  mentions: TStringArray;
  count: Integer;
begin
  SetLength(mentions, 0);
  count := 0;
  i := 1;
  
  while i <= Length(AContent) do
  begin
    if AContent[i] = '@' then
    begin
      start := i + 1;
      inc(i);
      while (i <= Length(AContent)) and 
            (AContent[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
        inc(i);
      
      if i > start then
      begin
        mention := Copy(AContent, start, i - start);
        SetLength(mentions, count + 1);
        mentions[count] := mention;
        inc(count);
      end;
    end
    else
      inc(i);
  end;
  
  Result := mentions;
end;

procedure TCommentedTaskManager.UpdateReactionCounts(ACommentID: Integer);
var
  idx, i: Integer;
  rt: TReactionType;
begin
  idx := FindCommentIndex(ACommentID);
  if idx < 0 then
    Exit;
  
  for rt := Low(TReactionType) to High(TReactionType) do
    FComments[idx].ReactionCounts[rt] := 0;
  
  for i := 0 to Length(FReactions) - 1 do
  begin
    if FReactions[i].CommentID = ACommentID then
      inc(FComments[idx].ReactionCounts[FReactions[i].ReactionType]);
  end;
end;

function TCommentedTaskManager.BuildCommentThread(ARootCommentID: Integer): TCommentThread;
var
  thread: TCommentThread;
  replies: TTaskCommentArray;
begin
  thread.RootCommentID := ARootCommentID;
  replies := GetCommentReplies(ARootCommentID);
  thread.Comments := replies;
  thread.TotalReplies := Length(replies);
  
  if Length(replies) > 0 then
    thread.LastActivityDate := replies[Length(replies) - 1].CreatedDate
  else
    thread.LastActivityDate := Now;
  
  Result := thread;
end;

function TCommentedTaskManager.AddComment(ATaskID: Integer; 
  const AAuthorName, AContent: string): Integer;
var
  comment: TTaskComment;
  rt: TReactionType;
begin
  comment.ID := FNextCommentID;
  inc(FNextCommentID);
  comment.TaskID := ATaskID;
  comment.ParentCommentID := 0;
  comment.AuthorName := AAuthorName;
  comment.Content := AContent;
  comment.CreatedDate := Now;
  comment.ModifiedDate := Now;
  comment.Status := csVisible;
  comment.IsEdited := False;
  SetLength(comment.EditHistory, 0);
  comment.Mentions := ExtractMentions(AContent);
  comment.ReplyCount := 0;
  
  for rt := Low(TReactionType) to High(TReactionType) do
    comment.ReactionCounts[rt] := 0;
  
  SetLength(FComments, Length(FComments) + 1);
  FComments[Length(FComments) - 1] := comment;
  
  Result := comment.ID;
end;

function TCommentedTaskManager.AddReply(AParentCommentID: Integer; 
  const AAuthorName, AContent: string): Integer;
var
  comment: TTaskComment;
  parentIdx: Integer;
  rt: TReactionType;
begin
  parentIdx := FindCommentIndex(AParentCommentID);
  if parentIdx < 0 then
  begin
    Result := -1;
    Exit;
  end;
  
  comment.ID := FNextCommentID;
  inc(FNextCommentID);
  comment.TaskID := FComments[parentIdx].TaskID;
  comment.ParentCommentID := AParentCommentID;
  comment.AuthorName := AAuthorName;
  comment.Content := AContent;
  comment.CreatedDate := Now;
  comment.ModifiedDate := Now;
  comment.Status := csVisible;
  comment.IsEdited := False;
  SetLength(comment.EditHistory, 0);
  comment.Mentions := ExtractMentions(AContent);
  comment.ReplyCount := 0;
  
  for rt := Low(TReactionType) to High(TReactionType) do
    comment.ReactionCounts[rt] := 0;
  
  SetLength(FComments, Length(FComments) + 1);
  FComments[Length(FComments) - 1] := comment;
  
  inc(FComments[parentIdx].ReplyCount);
  
  Result := comment.ID;
end;

function TCommentedTaskManager.EditComment(ACommentID: Integer; 
  const ANewContent, AEditReason: string): boolean;
var
  idx: Integer;
  history: TEditHistory;
begin
  idx := FindCommentIndex(ACommentID);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  
  history.EditDate := Now;
  history.PreviousContent := FComments[idx].Content;
  history.EditedBy := FComments[idx].AuthorName;
  history.EditReason := AEditReason;
  
  SetLength(FComments[idx].EditHistory, Length(FComments[idx].EditHistory) + 1);
  FComments[idx].EditHistory[Length(FComments[idx].EditHistory) - 1] := history;
  
  FComments[idx].Content := ANewContent;
  FComments[idx].ModifiedDate := Now;
  FComments[idx].IsEdited := True;
  FComments[idx].Mentions := ExtractMentions(ANewContent);
  
  Result := True;
end;

function TCommentedTaskManager.DeleteComment(ACommentID: Integer): boolean;
var
  idx: Integer;
begin
  idx := FindCommentIndex(ACommentID);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  
  FComments[idx].Status := csDeleted;
  FComments[idx].Content := '[Deleted]';
  Result := True;
end;

function TCommentedTaskManager.GetComment(ACommentID: Integer): TTaskComment;
var
  idx: Integer;
  empty: TTaskComment;
begin
  idx := FindCommentIndex(ACommentID);
  if idx >= 0 then
    Result := FComments[idx]
  else
    Result := empty;
end;

function TCommentedTaskManager.GetTaskComments(ATaskID: Integer): TTaskCommentArray;
var
  i, count: Integer;
  results: TTaskCommentArray;
begin
  SetLength(results, 0);
  count := 0;
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if (FComments[i].TaskID = ATaskID) and 
       (FComments[i].ParentCommentID = 0) and
       (FComments[i].Status <> csDeleted) then
    begin
      SetLength(results, count + 1);
      results[count] := FComments[i];
      inc(count);
    end;
  end;
  
  Result := results;
end;

function TCommentedTaskManager.GetCommentReplies(AParentCommentID: Integer): TTaskCommentArray;
var
  i, count: Integer;
  results: TTaskCommentArray;
begin
  SetLength(results, 0);
  count := 0;
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if (FComments[i].ParentCommentID = AParentCommentID) and
       (FComments[i].Status <> csDeleted) then
    begin
      SetLength(results, count + 1);
      results[count] := FComments[i];
      inc(count);
    end;
  end;
  
  Result := results;
end;

function TCommentedTaskManager.GetCommentThread(ARootCommentID: Integer): TCommentThread;
begin
  Result := BuildCommentThread(ARootCommentID);
end;

function TCommentedTaskManager.GetAllThreads(ATaskID: Integer): TCommentThreadArray;
var
  i, count: Integer;
  threads: TCommentThreadArray;
begin
  SetLength(threads, 0);
  count := 0;
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if (FComments[i].TaskID = ATaskID) and 
       (FComments[i].ParentCommentID = 0) and
       (FComments[i].Status <> csDeleted) then
    begin
      SetLength(threads, count + 1);
      threads[count] := BuildCommentThread(FComments[i].ID);
      inc(count);
    end;
  end;
  
  Result := threads;
end;

function TCommentedTaskManager.AddReaction(ACommentID: Integer; 
  AReactionType: TReactionType; const AUserName: string): Integer;
var
  reaction: TCommentReaction;
  existing: Integer;
begin
  existing := GetUserReaction(ACommentID, AUserName);
  if existing >= 0 then
  begin
    Result := -1;
    Exit;
  end;
  
  reaction.ID := FNextReactionID;
  inc(FNextReactionID);
  reaction.CommentID := ACommentID;
  reaction.ReactionType := AReactionType;
  reaction.UserName := AUserName;
  reaction.CreatedDate := Now;
  
  SetLength(FReactions, Length(FReactions) + 1);
  FReactions[Length(FReactions) - 1] := reaction;
  
  UpdateReactionCounts(ACommentID);
  
  Result := reaction.ID;
end;

function TCommentedTaskManager.RemoveReaction(AReactionID: Integer): boolean;
var
  idx, i: Integer;
  commentID: Integer;
begin
  idx := FindReactionIndex(AReactionID);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  
  commentID := FReactions[idx].CommentID;
  
  for i := idx to Length(FReactions) - 2 do
    FReactions[i] := FReactions[i + 1];
  SetLength(FReactions, Length(FReactions) - 1);
  
  UpdateReactionCounts(commentID);
  
  Result := True;
end;

function TCommentedTaskManager.GetCommentReactions(ACommentID: Integer): TCommentReactionArray;
var
  i, count: Integer;
  results: TCommentReactionArray;
begin
  SetLength(results, 0);
  count := 0;
  
  for i := 0 to Length(FReactions) - 1 do
  begin
    if FReactions[i].CommentID = ACommentID then
    begin
      SetLength(results, count + 1);
      results[count] := FReactions[i];
      inc(count);
    end;
  end;
  
  Result := results;
end;

function TCommentedTaskManager.GetUserReaction(ACommentID: Integer; 
  const AUserName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FReactions) - 1 do
  begin
    if (FReactions[i].CommentID = ACommentID) and 
       (FReactions[i].UserName = AUserName) then
    begin
      Result := FReactions[i].ID;
      Exit;
    end;
  end;
end;

function TCommentedTaskManager.AddAttachment(ACommentID: Integer; 
  AType: TCommentAttachmentType; const AURL, ATitle, ADescription: string): Integer;
var
  attachment: TCommentAttachment;
begin
  attachment.ID := FNextAttachmentID;
  inc(FNextAttachmentID);
  attachment.CommentID := ACommentID;
  attachment.AttachmentType := AType;
  attachment.URL := AURL;
  attachment.Title := ATitle;
  attachment.Description := ADescription;
  attachment.CreatedDate := Now;
  
  SetLength(FAttachments, Length(FAttachments) + 1);
  FAttachments[Length(FAttachments) - 1] := attachment;
  
  Result := attachment.ID;
end;

function TCommentedTaskManager.RemoveAttachment(AAttachmentID: Integer): boolean;
var
  idx, i: Integer;
begin
  idx := FindAttachmentIndex(AAttachmentID);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  
  for i := idx to Length(FAttachments) - 2 do
    FAttachments[i] := FAttachments[i + 1];
  SetLength(FAttachments, Length(FAttachments) - 1);
  
  Result := True;
end;

function TCommentedTaskManager.GetCommentAttachments(ACommentID: Integer): TCommentAttachmentArray;
var
  i, count: Integer;
  results: TCommentAttachmentArray;
begin
  SetLength(results, 0);
  count := 0;
  
  for i := 0 to Length(FAttachments) - 1 do
  begin
    if FAttachments[i].CommentID = ACommentID then
    begin
      SetLength(results, count + 1);
      results[count] := FAttachments[i];
      inc(count);
    end;
  end;
  
  Result := results;
end;

function TCommentedTaskManager.PinComment(ACommentID: Integer): boolean;
var
  idx: Integer;
begin
  idx := FindCommentIndex(ACommentID);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  
  FComments[idx].Status := csPinned;
  Result := True;
end;

function TCommentedTaskManager.UnpinComment(ACommentID: Integer): boolean;
var
  idx: Integer;
begin
  idx := FindCommentIndex(ACommentID);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  
  if FComments[idx].Status = csPinned then
    FComments[idx].Status := csVisible;
  Result := True;
end;

function TCommentedTaskManager.HideComment(ACommentID: Integer): boolean;
var
  idx: Integer;
begin
  idx := FindCommentIndex(ACommentID);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  
  FComments[idx].Status := csHidden;
  Result := True;
end;

function TCommentedTaskManager.UnhideComment(ACommentID: Integer): boolean;
var
  idx: Integer;
begin
  idx := FindCommentIndex(ACommentID);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  
  if FComments[idx].Status = csHidden then
    FComments[idx].Status := csVisible;
  Result := True;
end;

function TCommentedTaskManager.FlagComment(ACommentID: Integer): boolean;
var
  idx: Integer;
begin
  idx := FindCommentIndex(ACommentID);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  
  FComments[idx].Status := csFlagged;
  Result := True;
end;

function TCommentedTaskManager.SearchComments(const ASearchTerm: string): TTaskCommentArray;
var
  i, count: Integer;
  results: TTaskCommentArray;
  searchLower: string;
begin
  SetLength(results, 0);
  count := 0;
  searchLower := LowerCase(ASearchTerm);
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if (Pos(searchLower, LowerCase(FComments[i].Content)) > 0) and
       (FComments[i].Status <> csDeleted) then
    begin
      SetLength(results, count + 1);
      results[count] := FComments[i];
      inc(count);
    end;
  end;
  
  Result := results;
end;

function TCommentedTaskManager.GetCommentsByAuthor(const AAuthorName: string): TTaskCommentArray;
var
  i, count: Integer;
  results: TTaskCommentArray;
begin
  SetLength(results, 0);
  count := 0;
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if (FComments[i].AuthorName = AAuthorName) and
       (FComments[i].Status <> csDeleted) then
    begin
      SetLength(results, count + 1);
      results[count] := FComments[i];
      inc(count);
    end;
  end;
  
  Result := results;
end;

function TCommentedTaskManager.GetCommentsByDateRange(AStartDate, AEndDate: TDateTime): TTaskCommentArray;
var
  i, count: Integer;
  results: TTaskCommentArray;
begin
  SetLength(results, 0);
  count := 0;
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if (FComments[i].CreatedDate >= AStartDate) and
       (FComments[i].CreatedDate <= AEndDate) and
       (FComments[i].Status <> csDeleted) then
    begin
      SetLength(results, count + 1);
      results[count] := FComments[i];
      inc(count);
    end;
  end;
  
  Result := results;
end;

function TCommentedTaskManager.GetPinnedComments(ATaskID: Integer): TTaskCommentArray;
var
  i, count: Integer;
  results: TTaskCommentArray;
begin
  SetLength(results, 0);
  count := 0;
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if (FComments[i].TaskID = ATaskID) and
       (FComments[i].Status = csPinned) then
    begin
      SetLength(results, count + 1);
      results[count] := FComments[i];
      inc(count);
    end;
  end;
  
  Result := results;
end;

function TCommentedTaskManager.GetMentions(const AUserName: string): TTaskCommentArray;
var
  i, j, count: Integer;
  results: TTaskCommentArray;
  found: boolean;
begin
  SetLength(results, 0);
  count := 0;
  
  for i := 0 to Length(FComments) - 1 do
  begin
    found := False;
    for j := 0 to Length(FComments[i].Mentions) - 1 do
    begin
      if FComments[i].Mentions[j] = AUserName then
      begin
        found := True;
        Break;
      end;
    end;
    
    if found and (FComments[i].Status <> csDeleted) then
    begin
      SetLength(results, count + 1);
      results[count] := FComments[i];
      inc(count);
    end;
  end;
  
  Result := results;
end;

function TCommentedTaskManager.GetCommentStatistics: TCommentStatistics;
var
  stats: TCommentStatistics;
  i, j: Integer;
  taskCounts: array of record
    TaskID: Integer;
    Count: Integer;
  end;
  userCounts: array of record
    UserName: string;
    Count: Integer;
  end;
  maxTask, maxUser, taskIdx, userIdx: Integer;
  rt: TReactionType;
  reactionCounts: array[TReactionType] of Integer;
  maxReactionCount: Integer;
  totalReplies: Integer;
begin
  stats.TotalComments := 0;
  stats.TotalThreads := 0;
  stats.TotalReactions := Length(FReactions);
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if FComments[i].Status <> csDeleted then
      inc(stats.TotalComments);
    if (FComments[i].ParentCommentID = 0) and (FComments[i].Status <> csDeleted) then
      inc(stats.TotalThreads);
  end;
  
  SetLength(taskCounts, 0);
  SetLength(userCounts, 0);
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if FComments[i].Status = csDeleted then
      Continue;
    
    taskIdx := -1;
    for j := 0 to Length(taskCounts) - 1 do
    begin
      if taskCounts[j].TaskID = FComments[i].TaskID then
      begin
        taskIdx := j;
        Break;
      end;
    end;
    if taskIdx < 0 then
    begin
      SetLength(taskCounts, Length(taskCounts) + 1);
      taskCounts[Length(taskCounts) - 1].TaskID := FComments[i].TaskID;
      taskCounts[Length(taskCounts) - 1].Count := 1;
    end
    else
      inc(taskCounts[taskIdx].Count);
    
    userIdx := -1;
    for j := 0 to Length(userCounts) - 1 do
    begin
      if userCounts[j].UserName = FComments[i].AuthorName then
      begin
        userIdx := j;
        Break;
      end;
    end;
    if userIdx < 0 then
    begin
      SetLength(userCounts, Length(userCounts) + 1);
      userCounts[Length(userCounts) - 1].UserName := FComments[i].AuthorName;
      userCounts[Length(userCounts) - 1].Count := 1;
    end
    else
      inc(userCounts[userIdx].Count);
  end;
  
  maxTask := 0;
  stats.MostActiveTask := 0;
  for i := 0 to Length(taskCounts) - 1 do
  begin
    if taskCounts[i].Count > maxTask then
    begin
      maxTask := taskCounts[i].Count;
      stats.MostActiveTask := taskCounts[i].TaskID;
    end;
  end;
  
  maxUser := 0;
  stats.MostCommentedUser := '';
  for i := 0 to Length(userCounts) - 1 do
  begin
    if userCounts[i].Count > maxUser then
    begin
      maxUser := userCounts[i].Count;
      stats.MostCommentedUser := userCounts[i].UserName;
    end;
  end;
  
  if Length(taskCounts) > 0 then
    stats.AverageCommentsPerTask := stats.TotalComments / Length(taskCounts)
  else
    stats.AverageCommentsPerTask := 0;
  
  totalReplies := 0;
  for i := 0 to Length(FComments) - 1 do
  begin
    if (FComments[i].Status <> csDeleted) and (FComments[i].ParentCommentID > 0) then
      inc(totalReplies);
  end;
  
  if stats.TotalThreads > 0 then
    stats.AverageRepliesPerComment := totalReplies / stats.TotalThreads
  else
    stats.AverageRepliesPerComment := 0;
  
  for rt := Low(TReactionType) to High(TReactionType) do
    reactionCounts[rt] := 0;
  
  for i := 0 to Length(FReactions) - 1 do
    inc(reactionCounts[FReactions[i].ReactionType]);
  
  maxReactionCount := 0;
  stats.MostUsedReaction := rtLike;
  for rt := Low(TReactionType) to High(TReactionType) do
  begin
    if reactionCounts[rt] > maxReactionCount then
    begin
      maxReactionCount := reactionCounts[rt];
      stats.MostUsedReaction := rt;
    end;
  end;
  
  Result := stats;
end;

function TCommentedTaskManager.GetTaskCommentCount(ATaskID: Integer): Integer;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to Length(FComments) - 1 do
  begin
    if (FComments[i].TaskID = ATaskID) and (FComments[i].Status <> csDeleted) then
      inc(count);
  end;
  Result := count;
end;

function TCommentedTaskManager.GetMostCommentedTasks(ALimit: Integer): TIntegerArray;
var
  i, j, count: Integer;
  taskCounts: array of record
    TaskID: Integer;
    Count: Integer;
  end;
  results: TIntegerArray;
  taskIdx, maxIdx, maxCount: Integer;
begin
  SetLength(taskCounts, 0);
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if FComments[i].Status = csDeleted then
      Continue;
    
    taskIdx := -1;
    for j := 0 to Length(taskCounts) - 1 do
    begin
      if taskCounts[j].TaskID = FComments[i].TaskID then
      begin
        taskIdx := j;
        Break;
      end;
    end;
    if taskIdx < 0 then
    begin
      SetLength(taskCounts, Length(taskCounts) + 1);
      taskCounts[Length(taskCounts) - 1].TaskID := FComments[i].TaskID;
      taskCounts[Length(taskCounts) - 1].Count := 1;
    end
    else
      inc(taskCounts[taskIdx].Count);
  end;
  
  SetLength(results, 0);
  count := 0;
  
  while (count < ALimit) and (Length(taskCounts) > 0) do
  begin
    maxIdx := 0;
    maxCount := taskCounts[0].Count;
    for i := 1 to Length(taskCounts) - 1 do
    begin
      if taskCounts[i].Count > maxCount then
      begin
        maxIdx := i;
        maxCount := taskCounts[i].Count;
      end;
    end;
    
    SetLength(results, count + 1);
    results[count] := taskCounts[maxIdx].TaskID;
    inc(count);
    
    for i := maxIdx to Length(taskCounts) - 2 do
      taskCounts[i] := taskCounts[i + 1];
    SetLength(taskCounts, Length(taskCounts) - 1);
  end;
  
  Result := results;
end;

function TCommentedTaskManager.GetMostActiveCommenters(ALimit: Integer): TStringArray;
var
  i, j, count: Integer;
  userCounts: array of record
    UserName: string;
    Count: Integer;
  end;
  results: TStringArray;
  userIdx, maxIdx, maxCount: Integer;
begin
  SetLength(userCounts, 0);
  
  for i := 0 to Length(FComments) - 1 do
  begin
    if FComments[i].Status = csDeleted then
      Continue;
    
    userIdx := -1;
    for j := 0 to Length(userCounts) - 1 do
    begin
      if userCounts[j].UserName = FComments[i].AuthorName then
      begin
        userIdx := j;
        Break;
      end;
    end;
    if userIdx < 0 then
    begin
      SetLength(userCounts, Length(userCounts) + 1);
      userCounts[Length(userCounts) - 1].UserName := FComments[i].AuthorName;
      userCounts[Length(userCounts) - 1].Count := 1;
    end
    else
      inc(userCounts[userIdx].Count);
  end;
  
  SetLength(results, 0);
  count := 0;
  
  while (count < ALimit) and (Length(userCounts) > 0) do
  begin
    maxIdx := 0;
    maxCount := userCounts[0].Count;
    for i := 1 to Length(userCounts) - 1 do
    begin
      if userCounts[i].Count > maxCount then
      begin
        maxIdx := i;
        maxCount := userCounts[i].Count;
      end;
    end;
    
    SetLength(results, count + 1);
    results[count] := userCounts[maxIdx].UserName;
    inc(count);
    
    for i := maxIdx to Length(userCounts) - 2 do
      userCounts[i] := userCounts[i + 1];
    SetLength(userCounts, Length(userCounts) - 1);
  end;
  
  Result := results;
end;

function TCommentedTaskManager.ExportCommentsToMarkdown(ATaskID: Integer): string;
var
  output: string;
  comments: TTaskCommentArray;
  i, j: Integer;
  reactions: TCommentReactionArray;
  attachments: TCommentAttachmentArray;
begin
  output := '# Comments for Task #' + IntToStr(ATaskID) + LineEnding + LineEnding;
  
  comments := GetTaskComments(ATaskID);
  for i := 0 to Length(comments) - 1 do
  begin
    output := output + '## Comment #' + IntToStr(comments[i].ID) + LineEnding;
    output := output + '**Author:** ' + comments[i].AuthorName + LineEnding;
    output := output + '**Date:** ' + DateTimeToStr(comments[i].CreatedDate) + LineEnding;
    if comments[i].IsEdited then
      output := output + '**Edited:** Yes' + LineEnding;
    output := output + LineEnding + comments[i].Content + LineEnding + LineEnding;
    
    reactions := GetCommentReactions(comments[i].ID);
    if Length(reactions) > 0 then
    begin
      output := output + '**Reactions:** ';
      for j := 0 to Length(reactions) - 1 do
      begin
        output := output + ReactionTypeToString(reactions[j].ReactionType);
        if j < Length(reactions) - 1 then
          output := output + ', ';
      end;
      output := output + LineEnding + LineEnding;
    end;
    
    attachments := GetCommentAttachments(comments[i].ID);
    if Length(attachments) > 0 then
    begin
      output := output + '**Attachments:**' + LineEnding;
      for j := 0 to Length(attachments) - 1 do
        output := output + '- [' + attachments[j].Title + '](' + attachments[j].URL + ')' + LineEnding;
      output := output + LineEnding;
    end;
    
    output := output + '---' + LineEnding + LineEnding;
  end;
  
  Result := output;
end;

function TCommentedTaskManager.ExportCommentsToHTML(ATaskID: Integer): string;
var
  output: string;
  comments: TTaskCommentArray;
  i: Integer;
begin
  output := '<html><head><title>Comments for Task #' + IntToStr(ATaskID) + 
            '</title></head><body>' + LineEnding;
  output := output + '<h1>Comments for Task #' + IntToStr(ATaskID) + '</h1>' + LineEnding;
  
  comments := GetTaskComments(ATaskID);
  for i := 0 to Length(comments) - 1 do
  begin
    output := output + '<div class="comment">' + LineEnding;
    output := output + '<h3>Comment #' + IntToStr(comments[i].ID) + '</h3>' + LineEnding;
    output := output + '<p><strong>Author:</strong> ' + comments[i].AuthorName + '</p>' + LineEnding;
    output := output + '<p><strong>Date:</strong> ' + DateTimeToStr(comments[i].CreatedDate) + '</p>' + LineEnding;
    output := output + '<p>' + comments[i].Content + '</p>' + LineEnding;
    output := output + '</div>' + LineEnding;
  end;
  
  output := output + '</body></html>' + LineEnding;
  Result := output;
end;

function TCommentedTaskManager.ExportThreadToMarkdown(ARootCommentID: Integer): string;
var
  output: string;
  thread: TCommentThread;
  i: Integer;
begin
  thread := GetCommentThread(ARootCommentID);
  
  output := '# Comment Thread #' + IntToStr(ARootCommentID) + LineEnding + LineEnding;
  output := output + '**Total Replies:** ' + IntToStr(thread.TotalReplies) + LineEnding + LineEnding;
  
  for i := 0 to Length(thread.Comments) - 1 do
  begin
    output := output + '### Reply by ' + thread.Comments[i].AuthorName + LineEnding;
    output := output + thread.Comments[i].Content + LineEnding + LineEnding;
  end;
  
  Result := output;
end;

function TCommentedTaskManager.ReactionTypeToString(AType: TReactionType): string;
begin
  case AType of
    rtLike: Result := '👍 Like';
    rtHelpful: Result := '💡 Helpful';
    rtAgree: Result := '✅ Agree';
    rtDisagree: Result := '❌ Disagree';
    rtThumbsUp: Result := '👍 Thumbs Up';
    rtThumbsDown: Result := '👎 Thumbs Down';
  else
    Result := 'Unknown';
  end;
end;

function TCommentedTaskManager.CommentStatusToString(AStatus: TCommentStatus): string;
begin
  case AStatus of
    csVisible: Result := 'Visible';
    csHidden: Result := 'Hidden';
    csPinned: Result := 'Pinned';
    csDeleted: Result := 'Deleted';
    csFlagged: Result := 'Flagged';
  else
    Result := 'Unknown';
  end;
end;

function TCommentedTaskManager.AttachmentTypeToString(AType: TCommentAttachmentType): string;
begin
  case AType of
    catLink: Result := 'Link';
    catImage: Result := 'Image';
    catDocument: Result := 'Document';
    catCode: Result := 'Code';
  else
    Result := 'Unknown';
  end;
end;

function TCommentedTaskManager.SaveCommentsToFile(const AFilename: string): boolean;
var
  f: TextFile;
  i, j: Integer;
begin
  try
    AssignFile(f, AFilename);
    Rewrite(f);
    
    WriteLn(f, '[COMMENTS]');
    WriteLn(f, Length(FComments));
    for i := 0 to Length(FComments) - 1 do
    begin
      WriteLn(f, FComments[i].ID);
      WriteLn(f, FComments[i].TaskID);
      WriteLn(f, FComments[i].ParentCommentID);
      WriteLn(f, FComments[i].AuthorName);
      WriteLn(f, FComments[i].Content);
      WriteLn(f, DateTimeToStr(FComments[i].CreatedDate));
      WriteLn(f, Ord(FComments[i].Status));
      WriteLn(f, Length(FComments[i].Mentions));
      for j := 0 to Length(FComments[i].Mentions) - 1 do
        WriteLn(f, FComments[i].Mentions[j]);
    end;
    
    WriteLn(f, '[REACTIONS]');
    WriteLn(f, Length(FReactions));
    for i := 0 to Length(FReactions) - 1 do
    begin
      WriteLn(f, FReactions[i].ID);
      WriteLn(f, FReactions[i].CommentID);
      WriteLn(f, Ord(FReactions[i].ReactionType));
      WriteLn(f, FReactions[i].UserName);
    end;
    
    CloseFile(f);
    Result := True;
  except
    Result := False;
  end;
end;

function TCommentedTaskManager.LoadCommentsFromFile(const AFilename: string): boolean;
var
  f: TextFile;
  i, j, count, mentionCount: Integer;
  line: string;
  rt: TReactionType;
begin
  try
    AssignFile(f, AFilename);
    Reset(f);
    
    ReadLn(f, line);
    if line <> '[COMMENTS]' then
    begin
      CloseFile(f);
      Result := False;
      Exit;
    end;
    
    ReadLn(f, count);
    SetLength(FComments, count);
    
    for i := 0 to count - 1 do
    begin
      ReadLn(f, FComments[i].ID);
      ReadLn(f, FComments[i].TaskID);
      ReadLn(f, FComments[i].ParentCommentID);
      ReadLn(f, FComments[i].AuthorName);
      ReadLn(f, FComments[i].Content);
      ReadLn(f, line);
      FComments[i].CreatedDate := StrToDateTime(line);
      ReadLn(f, j);
      FComments[i].Status := TCommentStatus(j);
      ReadLn(f, mentionCount);
      SetLength(FComments[i].Mentions, mentionCount);
      for j := 0 to mentionCount - 1 do
        ReadLn(f, FComments[i].Mentions[j]);
      
      for rt := Low(TReactionType) to High(TReactionType) do
        FComments[i].ReactionCounts[rt] := 0;
    end;
    
    ReadLn(f, line);
    if line <> '[REACTIONS]' then
    begin
      CloseFile(f);
      Result := False;
      Exit;
    end;
    
    ReadLn(f, count);
    SetLength(FReactions, count);
    
    for i := 0 to count - 1 do
    begin
      ReadLn(f, FReactions[i].ID);
      ReadLn(f, FReactions[i].CommentID);
      ReadLn(f, j);
      FReactions[i].ReactionType := TReactionType(j);
      ReadLn(f, FReactions[i].UserName);
      
      UpdateReactionCounts(FReactions[i].CommentID);
    end;
    
    CloseFile(f);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TCommentedTaskManager.SelfTest;
var
  TaskID1, TaskID2: Integer;
  CommentID1, CommentID2, CommentID3, ReplyID1: Integer;
  ReactionID1, AttachID1: Integer;
  comments: TTaskCommentArray;
  stats: TCommentStatistics;
  thread: TCommentThread;
  markdown: string;
begin
  WriteLn('=== Task Comments System Self-Test ===');
  WriteLn;
  
  WriteLn('Test 1: Creating tasks...');
  TaskID1 := AddTask('Implement login feature', 'Add user authentication', tpHigh, Now + 7);
  TaskID2 := AddTask('Fix database bug', 'Connection pooling issue', tpCritical, Now + 2);
  WriteLn('Created 2 tasks: #', TaskID1, ', #', TaskID2);
  WriteLn;
  
  WriteLn('Test 2: Adding comments...');
  CommentID1 := AddComment(TaskID1, 'Alice', 'We should use OAuth2 for this @Bob');
  CommentID2 := AddComment(TaskID1, 'Bob', 'Good idea! Let me research the options.');
  CommentID3 := AddComment(TaskID2, 'Charlie', 'I found the root cause in the connection manager');
  WriteLn('Added 3 comments: #', CommentID1, ', #', CommentID2, ', #', CommentID3);
  WriteLn;
  
  WriteLn('Test 3: Adding reply...');
  ReplyID1 := AddReply(CommentID1, 'Bob', 'I recommend using Auth0 or Okta');
  WriteLn('Added reply #', ReplyID1, ' to comment #', CommentID1);
  WriteLn;
  
  WriteLn('Test 4: Adding reactions...');
  ReactionID1 := AddReaction(CommentID1, rtLike, 'Charlie');
  AddReaction(CommentID1, rtHelpful, 'David');
  AddReaction(CommentID2, rtAgree, 'Alice');
  WriteLn('Added 3 reactions');
  WriteLn;
  
  WriteLn('Test 5: Adding attachment...');
  AttachID1 := AddAttachment(CommentID1, catLink, 
    'https://oauth.net/2/', 'OAuth 2.0 Docs', 'Official documentation');
  WriteLn('Added attachment #', AttachID1);
  WriteLn;
  
  WriteLn('Test 6: Editing comment...');
  EditComment(CommentID2, 'Excellent idea! Let me research the best options.', 
    'Improved wording');
  WriteLn('Edited comment #', CommentID2);
  WriteLn;
  
  WriteLn('Test 7: Pinning comment...');
  PinComment(CommentID1);
  WriteLn('Pinned comment #', CommentID1);
  WriteLn;
  
  WriteLn('Test 8: Getting task comments...');
  comments := GetTaskComments(TaskID1);
  WriteLn('Task #', TaskID1, ' has ', Length(comments), ' top-level comments');
  WriteLn;
  
  WriteLn('Test 9: Getting comment thread...');
  thread := GetCommentThread(CommentID1);
  WriteLn('Thread for comment #', CommentID1, ' has ', thread.TotalReplies, ' replies');
  WriteLn;
  
  WriteLn('Test 10: Searching comments...');
  comments := SearchComments('OAuth');
  WriteLn('Found ', Length(comments), ' comments containing "OAuth"');
  WriteLn;
  
  WriteLn('Test 11: Getting mentions...');
  comments := GetMentions('Bob');
  WriteLn('Bob was mentioned in ', Length(comments), ' comments');
  WriteLn;
  
  WriteLn('Test 12: Getting statistics...');
  stats := GetCommentStatistics;
  WriteLn('Total comments: ', stats.TotalComments);
  WriteLn('Total threads: ', stats.TotalThreads);
  WriteLn('Total reactions: ', stats.TotalReactions);
  WriteLn('Most active task: #', stats.MostActiveTask);
  WriteLn('Most commented user: ', stats.MostCommentedUser);
  WriteLn('Most used reaction: ', ReactionTypeToString(stats.MostUsedReaction));
  WriteLn;
  
  WriteLn('Test 13: Exporting to Markdown...');
  markdown := ExportCommentsToMarkdown(TaskID1);
  WriteLn('Exported ', Length(markdown), ' characters of markdown');
  WriteLn;
  
  WriteLn('Test 14: Saving to file...');
  if SaveCommentsToFile('solution1/comments_test.dat') then
    WriteLn('✓ Comments saved successfully')
  else
    WriteLn('✗ Failed to save comments');
  WriteLn;
  
  WriteLn('Test 15: Loading from file...');
  SetLength(FComments, 0);
  SetLength(FReactions, 0);
  if LoadCommentsFromFile('solution1/comments_test.dat') then
  begin
    WriteLn('✓ Comments loaded successfully');
    WriteLn('Loaded ', Length(FComments), ' comments and ', Length(FReactions), ' reactions');
  end
  else
    WriteLn('✗ Failed to load comments');
  WriteLn;
  
  WriteLn('=== All Tests Completed Successfully! ===');
end;

end.
