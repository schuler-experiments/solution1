unit TaskCollaboration;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskManager,
  TaskTypes;

type
  TNotificationType = (ntTaskAssigned, ntTaskCompleted, ntTaskUpdated, 
                       ntTaskDeleted, ntCommentAdded, ntMentioned, 
                       ntTaskDueToday, ntTaskOverdue, ntDependencyBlocked);

  TNotificationPriority = (npLow, npNormal, npHigh, npCritical);

  TTaskComment = record
    commentId: integer;
    taskId: integer;
    author: string;
    content: string;
    createdDate: TDateTime;
    mentions: array of string;
    isEdited: boolean;
    editedDate: TDateTime;
    likes: integer;
    replyToCommentId: integer;
  end;

  TTaskNotification = record
    notificationId: integer;
    taskId: integer;
    recipient: string;
    notificationType: TNotificationType;
    message: string;
    createdDate: TDateTime;
    isRead: boolean;
    readDate: TDateTime;
    priority: TNotificationPriority;
    actionUrl: string;
  end;

  TActivityFeedEntry = record
    entryId: integer;
    taskId: integer;
    actor: string;
    action: string;
    details: string;
    createdDate: TDateTime;
    affectedUsers: array of string;
  end;

  TCommentArray = array of TTaskComment;
  TNotificationArray = array of TTaskNotification;
  TActivityFeedArray = array of TActivityFeedEntry;
  TStringArray = array of string;
  TIntArray = array of integer;

  TTaskCollaborationEngine = class
  private
    fManager: TTaskManager;
    fComments: TCommentArray;
    fNotifications: TNotificationArray;
    fActivityFeed: TActivityFeedArray;
    fNextCommentId: integer;
    fNextNotificationId: integer;
    fNextActivityId: integer;
    fLastError: string;
    fNotificationSettings: array of string;
    fMutedUsers: array of string;
  public
    constructor Create(aManager: TTaskManager);
    destructor Destroy;
    
    { Comment Management }
    function addComment(taskId: integer; const author, content: string;
                       replyToCommentId: integer = 0): integer;
    function editComment(commentId: integer; const newContent: string): boolean;
    function deleteComment(commentId: integer): boolean;
    function getComment(commentId: integer; var comment: TTaskComment): boolean;
    function getTaskComments(taskId: integer): TCommentArray;
    function getCommentCount(taskId: integer): integer;
    function getCommentThread(rootCommentId: integer): TCommentArray;
    function likeComment(commentId: integer): boolean;
    function getCommentLikes(commentId: integer): integer;
    
    { Mention Management }
    function addMentionToComment(commentId: integer; const mentionedUser: string): boolean;
    function removeMentionFromComment(commentId: integer; const mentionedUser: string): boolean;
    function getMentionsInComment(commentId: integer): TStringArray;
    function getUserMentions(const username: string): TCommentArray;
    function getMentionCount(const username: string): integer;
    
    { Notification Management }
    function createNotification(taskId: integer; const recipient: string;
                              notificationType: TNotificationType;
                              const message: string;
                              priority: TNotificationPriority = npNormal): integer;
    function getNotification(notificationId: integer; var notification: TTaskNotification): boolean;
    function getUserNotifications(const username: string): TNotificationArray;
    function getUnreadNotifications(const username: string): TNotificationArray;
    function markNotificationAsRead(notificationId: integer): boolean;
    function markAllNotificationsAsRead(const username: string): integer;
    function deleteNotification(notificationId: integer): boolean;
    function getNotificationCount(const username: string): integer;
    function getUnreadNotificationCount(const username: string): integer;
    
    { Notification Preferences }
    function setNotificationEnabled(const username, notificationType: string; enabled: boolean): boolean;
    function isNotificationEnabled(const username, notificationType: string): boolean;
    function muteUser(const username, mutedUser: string): boolean;
    function unmuteUser(const username, unmutedUser: string): boolean;
    function isUserMuted(const username, checkUser: string): boolean;
    
    { Activity Feed }
    function recordActivity(taskId: integer; const actor, action, details: string): integer;
    function getTaskActivityFeed(taskId: integer): TActivityFeedArray;
    function getUserActivityFeed(const username: string): TActivityFeedArray;
    function getGlobalActivityFeed(limit: integer = 100): TActivityFeedArray;
    function getActivityCount(taskId: integer): integer;
    
    { Collaboration Statistics }
    function getMostCommentedTasks(limit: integer = 10): TIntArray;
    function getMostActiveMember(const startDate, endDate: TDateTime): string;
    function getCollaborationScore(taskId: integer): double;
    function getTeamCollaborationMetrics: string;
    function getCommentFrequencyPerTask: string;
    
    { Automated Notifications }
    procedure notifyTaskAssignment(taskId: integer; const assignee: string);
    procedure notifyTaskCompletion(taskId: integer);
    procedure notifyTaskUpdate(taskId: integer; const changeDescription: string);
    procedure notifyMentionedUsers(commentId: integer);
    procedure notifyOverdueTasks;
    
    { Utility }
    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskCollaborationEngine.Create(aManager: TTaskManager);
begin
  inherited Create;
  fManager := aManager;
  fNextCommentId := 1;
  fNextNotificationId := 1;
  fNextActivityId := 1;
  fLastError := '';
  SetLength(fComments, 0);
  SetLength(fNotifications, 0);
  SetLength(fActivityFeed, 0);
  SetLength(fNotificationSettings, 0);
  SetLength(fMutedUsers, 0);
end;

destructor TTaskCollaborationEngine.Destroy;
begin
  SetLength(fComments, 0);
  SetLength(fNotifications, 0);
  SetLength(fActivityFeed, 0);
  SetLength(fNotificationSettings, 0);
  SetLength(fMutedUsers, 0);
  inherited Destroy;
end;

function TTaskCollaborationEngine.addComment(taskId: integer; const author, content: string;
                                             replyToCommentId: integer = 0): integer;
var
  comment: TTaskComment;
  task: TTask;
begin
  result := -1;
  
  if not fManager.getTask(taskId, task) then
  begin
    fLastError := 'Task not found';
    exit;
  end;
  
  if length(content) = 0 then
  begin
    fLastError := 'Comment content cannot be empty';
    exit;
  end;
  
  if length(author) = 0 then
  begin
    fLastError := 'Author name cannot be empty';
    exit;
  end;
  
  comment.commentId := fNextCommentId;
  comment.taskId := taskId;
  comment.author := author;
  comment.content := content;
  comment.createdDate := Now;
  comment.isEdited := false;
  comment.editedDate := 0;
  comment.likes := 0;
  comment.replyToCommentId := replyToCommentId;
  SetLength(comment.mentions, 0);
  
  SetLength(fComments, length(fComments) + 1);
  fComments[length(fComments) - 1] := comment;
  
  result := fNextCommentId;
  inc(fNextCommentId);
  
  recordActivity(taskId, author, 'commented', 'Added a comment on task');
end;

function TTaskCollaborationEngine.editComment(commentId: integer; const newContent: string): boolean;
var
  i: integer;
begin
  result := false;
  
  if length(newContent) = 0 then
  begin
    fLastError := 'New comment content cannot be empty';
    exit;
  end;
  
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].commentId = commentId then
    begin
      fComments[i].content := newContent;
      fComments[i].isEdited := true;
      fComments[i].editedDate := Now;
      result := true;
      exit;
    end;
  end;
  
  fLastError := 'Comment not found';
end;

function TTaskCollaborationEngine.deleteComment(commentId: integer): boolean;
var
  i, j: integer;
begin
  result := false;
  
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].commentId = commentId then
    begin
      for j := i to length(fComments) - 2 do
        fComments[j] := fComments[j + 1];
      SetLength(fComments, length(fComments) - 1);
      result := true;
      exit;
    end;
  end;
  
  fLastError := 'Comment not found';
end;

function TTaskCollaborationEngine.getComment(commentId: integer; var comment: TTaskComment): boolean;
var
  i: integer;
begin
  result := false;
  
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].commentId = commentId then
    begin
      comment := fComments[i];
      result := true;
      exit;
    end;
  end;
  
  fLastError := 'Comment not found';
end;

function TTaskCollaborationEngine.getTaskComments(taskId: integer): TCommentArray;
var
  i, count: integer;
  result_array: TCommentArray;
begin
  SetLength(result_array, 0);
  count := 0;
  
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].taskId = taskId then
    begin
      SetLength(result_array, count + 1);
      result_array[count] := fComments[i];
      inc(count);
    end;
  end;
  
  result := result_array;
end;

function TTaskCollaborationEngine.getCommentCount(taskId: integer): integer;
var
  i: integer;
begin
  result := 0;
  
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].taskId = taskId then
      inc(result);
  end;
end;

function TTaskCollaborationEngine.getCommentThread(rootCommentId: integer): TCommentArray;
var
  i, count: integer;
  result_array: TCommentArray;
begin
  SetLength(result_array, 0);
  count := 0;
  
  if (rootCommentId >= 0) and (rootCommentId < length(fComments)) then
  begin
    SetLength(result_array, count + 1);
    result_array[count] := fComments[rootCommentId];
    inc(count);
  end;
  
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].replyToCommentId = rootCommentId then
    begin
      SetLength(result_array, count + 1);
      result_array[count] := fComments[i];
      inc(count);
    end;
  end;
  
  result := result_array;
end;

function TTaskCollaborationEngine.likeComment(commentId: integer): boolean;
var
  i: integer;
begin
  result := false;
  
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].commentId = commentId then
    begin
      inc(fComments[i].likes);
      result := true;
      exit;
    end;
  end;
  
  fLastError := 'Comment not found';
end;

function TTaskCollaborationEngine.getCommentLikes(commentId: integer): integer;
var
  i: integer;
begin
  result := 0;
  
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].commentId = commentId then
    begin
      result := fComments[i].likes;
      exit;
    end;
  end;
  
  fLastError := 'Comment not found';
end;

function TTaskCollaborationEngine.addMentionToComment(commentId: integer; const mentionedUser: string): boolean;
var
  i, j: integer;
  already_mentioned: boolean;
begin
  result := false;
  
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].commentId = commentId then
    begin
      already_mentioned := false;
      for j := 0 to length(fComments[i].mentions) - 1 do
      begin
        if fComments[i].mentions[j] = mentionedUser then
        begin
          already_mentioned := true;
          break;
        end;
      end;
      
      if not already_mentioned then
      begin
        SetLength(fComments[i].mentions, length(fComments[i].mentions) + 1);
        fComments[i].mentions[length(fComments[i].mentions) - 1] := mentionedUser;
      end;
      
      result := true;
      exit;
    end;
  end;
  
  fLastError := 'Comment not found';
end;

function TTaskCollaborationEngine.removeMentionFromComment(commentId: integer; const mentionedUser: string): boolean;
var
  i, j, k: integer;
begin
  result := false;
  
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].commentId = commentId then
    begin
      for j := 0 to length(fComments[i].mentions) - 1 do
      begin
        if fComments[i].mentions[j] = mentionedUser then
        begin
          for k := j to length(fComments[i].mentions) - 2 do
            fComments[i].mentions[k] := fComments[i].mentions[k + 1];
          SetLength(fComments[i].mentions, length(fComments[i].mentions) - 1);
          result := true;
          exit;
        end;
      end;
      exit;
    end;
  end;
  
  fLastError := 'Comment not found';
end;

function TTaskCollaborationEngine.getMentionsInComment(commentId: integer): TStringArray;
var
  i: integer;
begin
  for i := 0 to length(fComments) - 1 do
  begin
    if fComments[i].commentId = commentId then
    begin
      result := fComments[i].mentions;
      exit;
    end;
  end;
  
  SetLength(result, 0);
  fLastError := 'Comment not found';
end;

function TTaskCollaborationEngine.getUserMentions(const username: string): TCommentArray;
var
  i, j, count: integer;
  result_array: TCommentArray;
  found: boolean;
begin
  SetLength(result_array, 0);
  count := 0;
  
  for i := 0 to length(fComments) - 1 do
  begin
    found := false;
    for j := 0 to length(fComments[i].mentions) - 1 do
    begin
      if fComments[i].mentions[j] = username then
      begin
        found := true;
        break;
      end;
    end;
    
    if found then
    begin
      SetLength(result_array, count + 1);
      result_array[count] := fComments[i];
      inc(count);
    end;
  end;
  
  result := result_array;
end;

function TTaskCollaborationEngine.getMentionCount(const username: string): integer;
var
  i, j: integer;
begin
  result := 0;
  
  for i := 0 to length(fComments) - 1 do
  begin
    for j := 0 to length(fComments[i].mentions) - 1 do
    begin
      if fComments[i].mentions[j] = username then
      begin
        inc(result);
        break;
      end;
    end;
  end;
end;

function TTaskCollaborationEngine.createNotification(taskId: integer; const recipient: string;
                                                    notificationType: TNotificationType;
                                                    const message: string;
                                                    priority: TNotificationPriority = npNormal): integer;
var
  notification: TTaskNotification;
begin
  result := -1;
  
  if length(recipient) = 0 then
  begin
    fLastError := 'Recipient cannot be empty';
    exit;
  end;
  
  if length(message) = 0 then
  begin
    fLastError := 'Message cannot be empty';
    exit;
  end;
  
  notification.notificationId := fNextNotificationId;
  notification.taskId := taskId;
  notification.recipient := recipient;
  notification.notificationType := notificationType;
  notification.message := message;
  notification.createdDate := Now;
  notification.isRead := false;
  notification.readDate := 0;
  notification.priority := priority;
  notification.actionUrl := 'task:' + IntToStr(taskId);
  
  SetLength(fNotifications, length(fNotifications) + 1);
  fNotifications[length(fNotifications) - 1] := notification;
  
  result := fNextNotificationId;
  inc(fNextNotificationId);
end;

function TTaskCollaborationEngine.getNotification(notificationId: integer; var notification: TTaskNotification): boolean;
var
  i: integer;
begin
  result := false;
  
  for i := 0 to length(fNotifications) - 1 do
  begin
    if fNotifications[i].notificationId = notificationId then
    begin
      notification := fNotifications[i];
      result := true;
      exit;
    end;
  end;
  
  fLastError := 'Notification not found';
end;

function TTaskCollaborationEngine.getUserNotifications(const username: string): TNotificationArray;
var
  i, count: integer;
  result_array: TNotificationArray;
begin
  SetLength(result_array, 0);
  count := 0;
  
  for i := 0 to length(fNotifications) - 1 do
  begin
    if fNotifications[i].recipient = username then
    begin
      SetLength(result_array, count + 1);
      result_array[count] := fNotifications[i];
      inc(count);
    end;
  end;
  
  result := result_array;
end;

function TTaskCollaborationEngine.getUnreadNotifications(const username: string): TNotificationArray;
var
  i, count: integer;
  result_array: TNotificationArray;
begin
  SetLength(result_array, 0);
  count := 0;
  
  for i := 0 to length(fNotifications) - 1 do
  begin
    if (fNotifications[i].recipient = username) and (not fNotifications[i].isRead) then
    begin
      SetLength(result_array, count + 1);
      result_array[count] := fNotifications[i];
      inc(count);
    end;
  end;
  
  result := result_array;
end;

function TTaskCollaborationEngine.markNotificationAsRead(notificationId: integer): boolean;
var
  i: integer;
begin
  result := false;
  
  for i := 0 to length(fNotifications) - 1 do
  begin
    if fNotifications[i].notificationId = notificationId then
    begin
      fNotifications[i].isRead := true;
      fNotifications[i].readDate := Now;
      result := true;
      exit;
    end;
  end;
  
  fLastError := 'Notification not found';
end;

function TTaskCollaborationEngine.markAllNotificationsAsRead(const username: string): integer;
var
  i: integer;
begin
  result := 0;
  
  for i := 0 to length(fNotifications) - 1 do
  begin
    if (fNotifications[i].recipient = username) and (not fNotifications[i].isRead) then
    begin
      fNotifications[i].isRead := true;
      fNotifications[i].readDate := Now;
      inc(result);
    end;
  end;
end;

function TTaskCollaborationEngine.deleteNotification(notificationId: integer): boolean;
var
  i, j: integer;
begin
  result := false;
  
  for i := 0 to length(fNotifications) - 1 do
  begin
    if fNotifications[i].notificationId = notificationId then
    begin
      for j := i to length(fNotifications) - 2 do
        fNotifications[j] := fNotifications[j + 1];
      SetLength(fNotifications, length(fNotifications) - 1);
      result := true;
      exit;
    end;
  end;
  
  fLastError := 'Notification not found';
end;

function TTaskCollaborationEngine.getNotificationCount(const username: string): integer;
var
  i: integer;
begin
  result := 0;
  
  for i := 0 to length(fNotifications) - 1 do
  begin
    if fNotifications[i].recipient = username then
      inc(result);
  end;
end;

function TTaskCollaborationEngine.getUnreadNotificationCount(const username: string): integer;
var
  i: integer;
begin
  result := 0;
  
  for i := 0 to length(fNotifications) - 1 do
  begin
    if (fNotifications[i].recipient = username) and (not fNotifications[i].isRead) then
      inc(result);
  end;
end;

function TTaskCollaborationEngine.setNotificationEnabled(const username, notificationType: string; enabled: boolean): boolean;
var
  i: integer;
  setting: string;
begin
  result := true;
  setting := username + ':' + notificationType + ':' + BoolToStr(enabled);
  
  for i := 0 to length(fNotificationSettings) - 1 do
  begin
    if pos(username + ':' + notificationType, fNotificationSettings[i]) > 0 then
    begin
      fNotificationSettings[i] := setting;
      exit;
    end;
  end;
  
  SetLength(fNotificationSettings, length(fNotificationSettings) + 1);
  fNotificationSettings[length(fNotificationSettings) - 1] := setting;
end;

function TTaskCollaborationEngine.isNotificationEnabled(const username, notificationType: string): boolean;
var
  i: integer;
begin
  result := true;
  
  for i := 0 to length(fNotificationSettings) - 1 do
  begin
    if pos(username + ':' + notificationType + ':false', fNotificationSettings[i]) > 0 then
    begin
      result := false;
      exit;
    end;
  end;
end;

function TTaskCollaborationEngine.muteUser(const username, mutedUser: string): boolean;
var
  i: integer;
  entry: string;
begin
  result := true;
  entry := username + ':' + mutedUser;
  
  for i := 0 to length(fMutedUsers) - 1 do
  begin
    if fMutedUsers[i] = entry then
    begin
      result := false;
      fLastError := 'User already muted';
      exit;
    end;
  end;
  
  SetLength(fMutedUsers, length(fMutedUsers) + 1);
  fMutedUsers[length(fMutedUsers) - 1] := entry;
end;

function TTaskCollaborationEngine.unmuteUser(const username, unmutedUser: string): boolean;
var
  i, j: integer;
  entry: string;
begin
  result := false;
  entry := username + ':' + unmutedUser;
  
  for i := 0 to length(fMutedUsers) - 1 do
  begin
    if fMutedUsers[i] = entry then
    begin
      for j := i to length(fMutedUsers) - 2 do
        fMutedUsers[j] := fMutedUsers[j + 1];
      SetLength(fMutedUsers, length(fMutedUsers) - 1);
      result := true;
      exit;
    end;
  end;
  
  fLastError := 'Muted user not found';
end;

function TTaskCollaborationEngine.isUserMuted(const username, checkUser: string): boolean;
var
  i: integer;
begin
  result := false;
  
  for i := 0 to length(fMutedUsers) - 1 do
  begin
    if fMutedUsers[i] = username + ':' + checkUser then
    begin
      result := true;
      exit;
    end;
  end;
end;

function TTaskCollaborationEngine.recordActivity(taskId: integer; const actor, action, details: string): integer;
var
  entry: TActivityFeedEntry;
begin
  result := -1;
  
  entry.entryId := fNextActivityId;
  entry.taskId := taskId;
  entry.actor := actor;
  entry.action := action;
  entry.details := details;
  entry.createdDate := Now;
  SetLength(entry.affectedUsers, 0);
  
  SetLength(fActivityFeed, length(fActivityFeed) + 1);
  fActivityFeed[length(fActivityFeed) - 1] := entry;
  
  result := fNextActivityId;
  inc(fNextActivityId);
end;

function TTaskCollaborationEngine.getTaskActivityFeed(taskId: integer): TActivityFeedArray;
var
  i, count: integer;
  result_array: TActivityFeedArray;
begin
  SetLength(result_array, 0);
  count := 0;
  
  for i := 0 to length(fActivityFeed) - 1 do
  begin
    if fActivityFeed[i].taskId = taskId then
    begin
      SetLength(result_array, count + 1);
      result_array[count] := fActivityFeed[i];
      inc(count);
    end;
  end;
  
  result := result_array;
end;

function TTaskCollaborationEngine.getUserActivityFeed(const username: string): TActivityFeedArray;
var
  i, count: integer;
  result_array: TActivityFeedArray;
begin
  SetLength(result_array, 0);
  count := 0;
  
  for i := 0 to length(fActivityFeed) - 1 do
  begin
    if fActivityFeed[i].actor = username then
    begin
      SetLength(result_array, count + 1);
      result_array[count] := fActivityFeed[i];
      inc(count);
    end;
  end;
  
  result := result_array;
end;

function TTaskCollaborationEngine.getGlobalActivityFeed(limit: integer = 100): TActivityFeedArray;
var
  i, start_idx, count: integer;
  result_array: TActivityFeedArray;
begin
  SetLength(result_array, 0);
  count := length(fActivityFeed);
  
  if count > limit then
    start_idx := count - limit
  else
    start_idx := 0;
  
  for i := start_idx to count - 1 do
  begin
    SetLength(result_array, i - start_idx + 1);
    result_array[i - start_idx] := fActivityFeed[i];
  end;
  
  result := result_array;
end;

function TTaskCollaborationEngine.getActivityCount(taskId: integer): integer;
var
  i: integer;
begin
  result := 0;
  
  for i := 0 to length(fActivityFeed) - 1 do
  begin
    if fActivityFeed[i].taskId = taskId then
      inc(result);
  end;
end;

function TTaskCollaborationEngine.getMostCommentedTasks(limit: integer = 10): TIntArray;
var
  i, j, k: integer;
  taskId: integer;
  count: integer;
  found: boolean;
  commentCounts: array of integer;
  taskIds: array of integer;
  result_array: TIntArray;
begin
  SetLength(result_array, 0);
  SetLength(taskIds, 0);
  SetLength(commentCounts, 0);
  
  for i := 0 to length(fComments) - 1 do
  begin
    taskId := fComments[i].taskId;
    found := false;
    
    for j := 0 to length(taskIds) - 1 do
    begin
      if taskIds[j] = taskId then
      begin
        inc(commentCounts[j]);
        found := true;
        break;
      end;
    end;
    
    if not found then
    begin
      SetLength(taskIds, length(taskIds) + 1);
      SetLength(commentCounts, length(commentCounts) + 1);
      taskIds[length(taskIds) - 1] := taskId;
      commentCounts[length(commentCounts) - 1] := 1;
    end;
  end;
  
  for i := 0 to length(taskIds) - 1 do
  begin
    if (i < limit) and (i < length(taskIds)) then
    begin
      SetLength(result_array, i + 1);
      result_array[i] := taskIds[i];
    end;
  end;
  
  SetLength(taskIds, 0);
  SetLength(commentCounts, 0);
  result := result_array;
end;

function TTaskCollaborationEngine.getMostActiveMember(const startDate, endDate: TDateTime): string;
var
  i, j, max_count: integer;
  member: string;
  found: boolean;
  members: array of string;
  counts: array of integer;
begin
  result := '';
  SetLength(members, 0);
  SetLength(counts, 0);
  max_count := 0;
  
  for i := 0 to length(fActivityFeed) - 1 do
  begin
    if (fActivityFeed[i].createdDate >= startDate) and 
       (fActivityFeed[i].createdDate <= endDate) then
    begin
      member := fActivityFeed[i].actor;
      found := false;
      
      for j := 0 to length(members) - 1 do
      begin
        if members[j] = member then
        begin
          inc(counts[j]);
          if counts[j] > max_count then
          begin
            max_count := counts[j];
            result := member;
          end;
          found := true;
          break;
        end;
      end;
      
      if not found then
      begin
        SetLength(members, length(members) + 1);
        SetLength(counts, length(counts) + 1);
        members[length(members) - 1] := member;
        counts[length(counts) - 1] := 1;
        if 1 > max_count then
        begin
          max_count := 1;
          result := member;
        end;
      end;
    end;
  end;
  
  SetLength(members, 0);
  SetLength(counts, 0);
end;

function TTaskCollaborationEngine.getCollaborationScore(taskId: integer): double;
var
  comment_count: integer;
  activity_count: integer;
  notification_count: integer;
  score: double;
  i: integer;
begin
  comment_count := getCommentCount(taskId);
  activity_count := getActivityCount(taskId);
  notification_count := 0;
  
  for i := 0 to length(fNotifications) - 1 do
  begin
    if fNotifications[i].taskId = taskId then
      inc(notification_count);
  end;
  
  score := (comment_count * 2) + (activity_count * 1) + (notification_count * 0.5);
  result := score;
end;

function TTaskCollaborationEngine.getTeamCollaborationMetrics: string;
var
  total_comments: integer;
  total_notifications: integer;
  total_activities: integer;
  avg_comments_per_task: double;
begin
  total_comments := length(fComments);
  total_notifications := length(fNotifications);
  total_activities := length(fActivityFeed);
  
  if total_comments > 0 then
    avg_comments_per_task := total_comments / (fManager.getTaskCount + 1)
  else
    avg_comments_per_task := 0;
  
  result := 'Team Collaboration Metrics:' + #13#10 +
    '  Total Comments: ' + IntToStr(total_comments) + #13#10 +
    '  Total Notifications: ' + IntToStr(total_notifications) + #13#10 +
    '  Total Activities: ' + IntToStr(total_activities) + #13#10 +
    '  Avg Comments per Task: ' + FormatFloat('0.00', avg_comments_per_task) + #13#10;
end;

function TTaskCollaborationEngine.getCommentFrequencyPerTask: string;
var
  i: integer;
  taskId: integer;
  count: integer;
  found: boolean;
  frequency: string;
begin
  frequency := '';
  
  for i := 0 to length(fComments) - 1 do
  begin
    taskId := fComments[i].taskId;
    count := getCommentCount(taskId);
    
    if pos('Task ' + IntToStr(taskId), frequency) = 0 then
    begin
      if frequency <> '' then
        frequency := frequency + #13#10;
      frequency := frequency + 'Task ' + IntToStr(taskId) + ': ' + IntToStr(count) + ' comments';
    end;
  end;
  
  result := frequency;
end;

procedure TTaskCollaborationEngine.notifyTaskAssignment(taskId: integer; const assignee: string);
begin
  createNotification(taskId, assignee, ntTaskAssigned,
    'You have been assigned to Task ' + IntToStr(taskId), npHigh);
end;

procedure TTaskCollaborationEngine.notifyTaskCompletion(taskId: integer);
var
  task: TTask;
begin
  if fManager.getTask(taskId, task) then
  begin
    if task.assignedTo <> '' then
    begin
      createNotification(taskId, task.assignedTo, ntTaskCompleted,
        'Task ' + IntToStr(taskId) + ' has been completed', npNormal);
    end;
  end;
end;

procedure TTaskCollaborationEngine.notifyTaskUpdate(taskId: integer; const changeDescription: string);
var
  task: TTask;
begin
  if fManager.getTask(taskId, task) then
  begin
    if task.assignedTo <> '' then
    begin
      createNotification(taskId, task.assignedTo, ntTaskUpdated,
        'Task ' + IntToStr(taskId) + ' has been updated: ' + changeDescription, npNormal);
    end;
  end;
end;

procedure TTaskCollaborationEngine.notifyMentionedUsers(commentId: integer);
var
  comment: TTaskComment;
  i: integer;
begin
  if getComment(commentId, comment) then
  begin
    for i := 0 to length(comment.mentions) - 1 do
    begin
      createNotification(comment.taskId, comment.mentions[i], ntMentioned,
        comment.author + ' mentioned you in a comment on Task ' + IntToStr(comment.taskId),
        npHigh);
    end;
  end;
end;

procedure TTaskCollaborationEngine.notifyOverdueTasks;
var
  tasks: TTaskArray;
  i: integer;
begin
  tasks := fManager.getTasksByStatus(tsNew);
  
  for i := 0 to length(tasks) - 1 do
  begin
    if (tasks[i].dueDate < Now) and (tasks[i].assignedTo <> '') then
    begin
      createNotification(tasks[i].id, tasks[i].assignedTo, ntTaskOverdue,
        'Task ' + IntToStr(tasks[i].id) + ' is overdue', npCritical);
    end;
  end;
end;

function TTaskCollaborationEngine.getLastError: string;
begin
  result := fLastError;
end;

procedure TTaskCollaborationEngine.clearError;
begin
  fLastError := '';
end;

end.
