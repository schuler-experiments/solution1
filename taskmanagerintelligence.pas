
unit taskmanagerintelligence;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Classes, StrUtils, Math,
  taskmanager, taskmanageradvanced, taskmanagerenhanced,
  taskmanagerteam, taskmanagergamify, taskmanagersmart,
  taskmanagerfocus, taskmanagerresource;

type
  { Basic Types }
  TIntArray = array of Integer;
  
  { Natural Language Processing Types }
  TNLPToken = record
    TokenType: string;
    Value: string;
    Confidence: Double;
  end;
  TNLPTokenArray = array of TNLPToken;
  
  TParsedTask = record
    Title: string;
    Description: string;
    Category: string;
    Priority: TTaskPriority;
    DueDate: TDateTime;
    EstimatedHours: Double;
    Tags: array of string;
    Confidence: Double;
    ParsedSuccessfully: Boolean;
  end;
  
  { Backup & Versioning Types }
  TBackupVersion = record
    VersionID: Integer;
    Timestamp: TDateTime;
    Description: string;
    FilePath: string;
    FileSize: Int64;
    TaskCount: Integer;
    Checksum: string;
  end;
  TBackupVersionArray = array of TBackupVersion;
  
  TRestorePoint = record
    PointID: Integer;
    Created: TDateTime;
    Label_: string;
    AutoCreated: Boolean;
    DataSnapshot: string;
  end;
  TRestorePointArray = array of TRestorePoint;
  
  { Bulk Operation Types }
  TBulkOperationType = (
    boUpdateStatus, boUpdatePriority, boUpdateCategory,
    boAddTag, boRemoveTag, boDelete, boArchive,
    boAssignMember, boSetDueDate, boAddToGoal
  );
  
  TBulkOperation = record
    OperationID: Integer;
    OpType: TBulkOperationType;
    TargetTaskIDs: array of Integer;
    Parameters: string;
    ExecutedAt: TDateTime;
    ExecutedBy: string;
    SuccessCount: Integer;
    FailureCount: Integer;
    ResultLog: string;
  end;
  TBulkOperationArray = array of TBulkOperation;
  
  { Analytics Types }
  TTrendPoint = record
    Date: TDateTime;
    Value: Double;
    Label_: string;
  end;
  TTrendArray = array of TTrendPoint;
  
  TAnalyticsReport = record
    ReportID: Integer;
    ReportType: string;
    Generated: TDateTime;
    TimeRange: string;
    DataPoints: TTrendArray;
    Summary: string;
    Insights: array of string;
  end;
  TAnalyticsReportArray = array of TAnalyticsReport;
  
  { Export Format Types }
  TExportFormat = (efJSON, efXML, efICalendar, efMarkdown, efHTML, efCSV);
  
  TExportResult = record
    Success: Boolean;
    Format: TExportFormat;
    Content: string;
    FileSize: Integer;
    ExportedAt: TDateTime;
    ErrorMessage: string;
  end;
  
  { Smart Notification Types }
  TNotificationChannel = (ncConsole, ncFile, ncEmail, ncWebhook);
  TNotificationPriority = (npLow, npNormal, npHigh, npCritical);
  
  TSmartNotification = record
    NotificationID: Integer;
    Channel: TNotificationChannel;
    Priority: TNotificationPriority;
    Title: string;
    Message: string;
    TaskID: Integer;
    CreatedAt: TDateTime;
    SentAt: TDateTime;
    IsSent: Boolean;
    Context: string;
  end;
  TSmartNotificationArray = array of TSmartNotification;

  { Intelligence Task Manager Class }
  TIntelligenceTaskManager = class(TResourceTaskManager)
  private
    FBackupVersions: TBackupVersionArray;
    FRestorePoints: TRestorePointArray;
    FBulkOperations: TBulkOperationArray;
    FNotifications: TSmartNotificationArray;
    FNextVersionID: Integer;
    FNextRestorePointID: Integer;
    FNextBulkOpID: Integer;
    FNextNotificationID: Integer;
    FAutoBackupEnabled: Boolean;
    FAutoBackupIntervalHours: Integer;
    FLastAutoBackup: TDateTime;
    
    function FindBackupVersionIndex(AVersionID: Integer): Integer;
    function FindRestorePointIndex(APointID: Integer): Integer;
    function CalculateChecksum(const AData: string): string;
    function TokenizeNLPInput(const AInput: string): TNLPTokenArray;
    function ExtractPriorityFromTokens(const ATokens: TNLPTokenArray): TTaskPriority;
    function ExtractDateFromTokens(const ATokens: TNLPTokenArray): TDateTime;
    function ExtractNumberFromTokens(const ATokens: TNLPTokenArray;
      const AContext: string): Double;
    { Helper methods to access inherited functionality }
    function GetInheritedTaskCount: Integer;
    function GetInheritedCompletedCount: Integer;
    function GetInheritedPendingCount: Integer;
    function GetInheritedAllTasks: TTaskArray;
    function GetInheritedCSVExport: string;
    function GetInheritedTaskStatusToString(AStatus: TTaskStatus): string;
    function GetInheritedTaskPriorityToString(APriority: TTaskPriority): string;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    { Natural Language Processing }
    function ParseNaturalLanguageTask(const AInput: string): TParsedTask;
    function CreateTaskFromNL(const AInput: string): Integer;
    function BulkCreateFromNL(const AInputs: array of string): TIntArray;
    
    { Backup & Versioning }
    function CreateBackupVersion(const ADescription: string): Integer;
    function RestoreFromVersion(AVersionID: Integer): Boolean;
    function GetBackupVersions: TBackupVersionArray;
    function DeleteBackupVersion(AVersionID: Integer): Boolean;
    function CreateRestorePoint(const ALabel: string): Integer;
    function RestoreToPoint(APointID: Integer): Boolean;
    function GetRestorePoints: TRestorePointArray;
    procedure EnableAutoBackup(AEnabled: Boolean; AIntervalHours: Integer);
    procedure CheckAndPerformAutoBackup;
    
    { Bulk Operations }
    function BulkUpdateStatus(const ATaskIDs: array of Integer;
      ANewStatus: TTaskStatus): Integer;
    function BulkUpdatePriority(const ATaskIDs: array of Integer;
      ANewPriority: TTaskPriority): Integer;
    function BulkUpdateCategory(const ATaskIDs: array of Integer;
      const ANewCategory: string): Integer;
    function BulkAddTag(const ATaskIDs: array of Integer;
      const ATag: string): Integer;
    function BulkDelete(const ATaskIDs: array of Integer): Integer;
    function BulkArchive(const ATaskIDs: array of Integer;
      const AReason: string): Integer;
    function GetBulkOperationHistory: TBulkOperationArray;
    
    { Advanced Analytics }
    function GenerateCompletionTrend(ADays: Integer): TTrendArray;
    function GenerateCategoryTrend(const ACategory: string; ADays: Integer): TTrendArray;
    function GeneratePriorityDistribution: TTrendArray;
    function GenerateProductivityHeatmap: string;
    function GenerateVelocityReport(AWeeks: Integer): TAnalyticsReport;
    function GenerateBurndownChart(const ACategory: string): TTrendArray;
    function PredictTaskCompletionTrend(ADaysAhead: Integer): TTrendArray;
    function GetTopPerformingCategories(ALimit: Integer): string;
    function GetBottleneckAnalysis: string;
    
    { Smart Notifications }
    function CreateNotification(AChannel: TNotificationChannel;
      APriority: TNotificationPriority; const ATitle, AMessage: string;
      ATaskID: Integer): Integer;
    function GetPendingNotifications: TSmartNotificationArray;
    function SendNotification(ANotificationID: Integer): Boolean;
    function SendAllPendingNotifications: Integer;
    procedure CheckAndCreateSmartNotifications;
    
    { Export Hub }
    function ExportToJSON: TExportResult;
    function ExportToXML: TExportResult;
    function ExportToICalendar: TExportResult;
    function ExportToMarkdown: TExportResult;
    function ExportToHTML: TExportResult;
    function ExportWithFormat(AFormat: TExportFormat): TExportResult;
    
    { Utility Functions }
    function BulkOperationTypeToString(AType: TBulkOperationType): string;
    function NotificationChannelToString(AChannel: TNotificationChannel): string;
    function NotificationPriorityToString(APriority: TNotificationPriority): string;
    function ExportFormatToString(AFormat: TExportFormat): string;
  end;

implementation

{$I taskmanagerintelligence_analytics.inc}
{$I taskmanagerintelligence_export.inc}

{ TIntelligenceTaskManager }

constructor TIntelligenceTaskManager.Create;
begin
  inherited Create;
  SetLength(FBackupVersions, 0);
  SetLength(FRestorePoints, 0);
  SetLength(FBulkOperations, 0);
  SetLength(FNotifications, 0);
  FNextVersionID := 1;
  FNextRestorePointID := 1;
  FNextBulkOpID := 1;
  FNextNotificationID := 1;
  FAutoBackupEnabled := False;
  FAutoBackupIntervalHours := 24;
  FLastAutoBackup := Now;
end;

destructor TIntelligenceTaskManager.Destroy;
begin
  SetLength(FBackupVersions, 0);
  SetLength(FRestorePoints, 0);
  SetLength(FBulkOperations, 0);
  SetLength(FNotifications, 0);
  inherited Destroy;
end;

{ Helper Functions }

function TIntelligenceTaskManager.FindBackupVersionIndex(AVersionID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FBackupVersions) do
    if FBackupVersions[i].VersionID = AVersionID then
    begin
      Result := i;
      Exit;
    end;
end;

function TIntelligenceTaskManager.FindRestorePointIndex(APointID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FRestorePoints) do
    if FRestorePoints[i].PointID = APointID then
    begin
      Result := i;
      Exit;
    end;
end;

function TIntelligenceTaskManager.CalculateChecksum(const AData: string): string;
var
  i, hash: Cardinal;
begin
  hash := 0;
  for i := 1 to Length(AData) do
    hash := hash * 31 + Ord(AData[i]);
  Result := IntToHex(hash, 8);
end;

{ Natural Language Processing Implementation }

function TIntelligenceTaskManager.TokenizeNLPInput(const AInput: string): TNLPTokenArray;
var
  words: TStringArray;
  i: Integer;
begin
  SetLength(Result, 0);
  words := AInput.Split([' ', ',', '.', ';']);
  SetLength(Result, Length(words));
  
  for i := 0 to High(words) do
  begin
    Result[i].Value := LowerCase(Trim(words[i]));
    Result[i].Confidence := 1.0;
    
    if (Result[i].Value = 'high') or (Result[i].Value = 'urgent') or
       (Result[i].Value = 'critical') then
      Result[i].TokenType := 'PRIORITY_HIGH'
    else if (Result[i].Value = 'low') or (Result[i].Value = 'minor') then
      Result[i].TokenType := 'PRIORITY_LOW'
    else if (Result[i].Value = 'medium') or (Result[i].Value = 'normal') then
      Result[i].TokenType := 'PRIORITY_MEDIUM'
    else if (Result[i].Value = 'today') then
      Result[i].TokenType := 'DATE_TODAY'
    else if (Result[i].Value = 'tomorrow') then
      Result[i].TokenType := 'DATE_TOMORROW'
    else if (Result[i].Value = 'week') or (Result[i].Value = 'next') then
      Result[i].TokenType := 'DATE_WEEK'
    else if (Result[i].Value = 'month') then
      Result[i].TokenType := 'DATE_MONTH'
    else
      Result[i].TokenType := 'TEXT';
  end;
end;

function TIntelligenceTaskManager.ExtractPriorityFromTokens(
  const ATokens: TNLPTokenArray): TTaskPriority;
var
  i: Integer;
begin
  Result := tpMedium;
  for i := 0 to High(ATokens) do
  begin
    if ATokens[i].TokenType = 'PRIORITY_HIGH' then
      Exit(tpHigh)
    else if ATokens[i].TokenType = 'PRIORITY_LOW' then
      Exit(tpLow)
    else if ATokens[i].TokenType = 'PRIORITY_MEDIUM' then
      Exit(tpMedium);
  end;
end;

function TIntelligenceTaskManager.ExtractDateFromTokens(
  const ATokens: TNLPTokenArray): TDateTime;
var
  i: Integer;
begin
  Result := IncDay(Now, 7);
  for i := 0 to High(ATokens) do
  begin
    if ATokens[i].TokenType = 'DATE_TODAY' then
      Exit(Now)
    else if ATokens[i].TokenType = 'DATE_TOMORROW' then
      Exit(IncDay(Now, 1))
    else if ATokens[i].TokenType = 'DATE_WEEK' then
      Exit(IncDay(Now, 7))
    else if ATokens[i].TokenType = 'DATE_MONTH' then
      Exit(IncDay(Now, 30));
  end;
end;

function TIntelligenceTaskManager.ExtractNumberFromTokens(
  const ATokens: TNLPTokenArray; const AContext: string): Double;
var
  i: Integer;
  val: Double;
begin
  Result := 0.0;
  for i := 0 to High(ATokens) do
  begin
    if TryStrToFloat(ATokens[i].Value, val) then
    begin
      Result := val;
      Exit;
    end;
  end;
end;

function TIntelligenceTaskManager.ParseNaturalLanguageTask(
  const AInput: string): TParsedTask;
var
  tokens: TNLPTokenArray;
  titleWords: TStringList;
  i: Integer;
begin
  Result.ParsedSuccessfully := False;
  Result.Confidence := 0.5;
  
  if Trim(AInput) = '' then
    Exit;
    
  tokens := TokenizeNLPInput(AInput);
  
  titleWords := TStringList.Create;
  try
    for i := 0 to High(tokens) do
      if tokens[i].TokenType = 'TEXT' then
        titleWords.Add(tokens[i].Value);
    
    if titleWords.Count > 0 then
    begin
      Result.Title := titleWords.DelimitedText;
      Result.Title := StringReplace(Result.Title, '"', '', [rfReplaceAll]);
      Result.Description := AInput;
      Result.Priority := ExtractPriorityFromTokens(tokens);
      Result.DueDate := ExtractDateFromTokens(tokens);
      Result.EstimatedHours := ExtractNumberFromTokens(tokens, 'hours');
      Result.Category := 'General';
      Result.ParsedSuccessfully := True;
      Result.Confidence := 0.75;
      SetLength(Result.Tags, 0);
    end;
  finally
    titleWords.Free;
  end;
end;

function TIntelligenceTaskManager.CreateTaskFromNL(const AInput: string): Integer;
var
  parsed: TParsedTask;
begin
  Result := -1;
  parsed := ParseNaturalLanguageTask(AInput);
  
  if parsed.ParsedSuccessfully then
    Result := AddTask(parsed.Title, parsed.Description, parsed.Category,
      parsed.Priority, parsed.DueDate, parsed.EstimatedHours);
end;

function TIntelligenceTaskManager.BulkCreateFromNL(
  const AInputs: array of string): TIntArray;
var
  i, taskID: Integer;
begin
  SetLength(Result, Length(AInputs));
  for i := 0 to High(AInputs) do
  begin
    taskID := CreateTaskFromNL(AInputs[i]);
    Result[i] := taskID;
  end;
end;

{ Backup & Versioning Implementation }

function TIntelligenceTaskManager.CreateBackupVersion(
  const ADescription: string): Integer;
var
  idx: Integer;
  dataStr: string;
  filename: string;
begin
  idx := Length(FBackupVersions);
  SetLength(FBackupVersions, idx + 1);
  
  FBackupVersions[idx].VersionID := FNextVersionID;
  FBackupVersions[idx].Timestamp := Now;
  FBackupVersions[idx].Description := ADescription;
  FBackupVersions[idx].TaskCount := GetInheritedTaskCount;
  
  filename := Format('backup_v%d_%s.dat',
    [FNextVersionID, FormatDateTime('yyyymmdd_hhnnss', Now)]);
  FBackupVersions[idx].FilePath := filename;
  
  dataStr := ExportToCSV;
  FBackupVersions[idx].Checksum := CalculateChecksum(dataStr);
  FBackupVersions[idx].FileSize := Length(dataStr);
  
  Result := FNextVersionID;
  Inc(FNextVersionID);
  FLastAutoBackup := Now;
end;

function TIntelligenceTaskManager.RestoreFromVersion(AVersionID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindBackupVersionIndex(AVersionID);
  if idx >= 0 then
  begin
    Result := True;
  end;
end;

function TIntelligenceTaskManager.GetBackupVersions: TBackupVersionArray;
begin
  Result := Copy(FBackupVersions, 0, Length(FBackupVersions));
end;

function TIntelligenceTaskManager.DeleteBackupVersion(AVersionID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindBackupVersionIndex(AVersionID);
  if idx >= 0 then
  begin
    for i := idx to High(FBackupVersions) - 1 do
      FBackupVersions[i] := FBackupVersions[i + 1];
    SetLength(FBackupVersions, Length(FBackupVersions) - 1);
    Result := True;
  end;
end;

function TIntelligenceTaskManager.CreateRestorePoint(const ALabel: string): Integer;
var
  idx: Integer;
begin
  idx := Length(FRestorePoints);
  SetLength(FRestorePoints, idx + 1);
  
  FRestorePoints[idx].PointID := FNextRestorePointID;
  FRestorePoints[idx].Created := Now;
  FRestorePoints[idx].Label_ := ALabel;
  FRestorePoints[idx].AutoCreated := False;
  FRestorePoints[idx].DataSnapshot := GetInheritedCSVExport;
  
  Result := FNextRestorePointID;
  Inc(FNextRestorePointID);
end;

function TIntelligenceTaskManager.RestoreToPoint(APointID: Integer): Boolean;
begin
  Result := False;
end;

function TIntelligenceTaskManager.GetRestorePoints: TRestorePointArray;
begin
  Result := Copy(FRestorePoints, 0, Length(FRestorePoints));
end;

procedure TIntelligenceTaskManager.EnableAutoBackup(AEnabled: Boolean;
  AIntervalHours: Integer);
begin
  FAutoBackupEnabled := AEnabled;
  FAutoBackupIntervalHours := AIntervalHours;
end;

procedure TIntelligenceTaskManager.CheckAndPerformAutoBackup;
var
  hoursSinceBackup: Double;
begin
  if not FAutoBackupEnabled then
    Exit;
    
  hoursSinceBackup := HoursBetween(Now, FLastAutoBackup);
  if hoursSinceBackup >= FAutoBackupIntervalHours then
    CreateBackupVersion('Auto backup');
end;

{ Bulk Operations Implementation }

function TIntelligenceTaskManager.BulkUpdateStatus(
  const ATaskIDs: array of Integer; ANewStatus: TTaskStatus): Integer;
var
  i, successCount: Integer;
  op: TBulkOperation;
  idx: Integer;
begin
  successCount := 0;
  for i := 0 to High(ATaskIDs) do
    if UpdateTaskStatus(ATaskIDs[i], ANewStatus) then
      Inc(successCount);
  
  idx := Length(FBulkOperations);
  SetLength(FBulkOperations, idx + 1);
  op.OperationID := FNextBulkOpID;
  op.OpType := boUpdateStatus;
  SetLength(op.TargetTaskIDs, Length(ATaskIDs));
  for i := 0 to High(ATaskIDs) do
    op.TargetTaskIDs[i] := ATaskIDs[i];
  op.ExecutedAt := Now;
  op.ExecutedBy := 'System';
  op.SuccessCount := successCount;
  op.FailureCount := Length(ATaskIDs) - successCount;
  op.ResultLog := Format('Updated %d tasks to status %s',
    [successCount, TaskStatusToString(ANewStatus)]);
  FBulkOperations[idx] := op;
  
  Inc(FNextBulkOpID);
  Result := successCount;
end;

function TIntelligenceTaskManager.BulkUpdatePriority(
  const ATaskIDs: array of Integer; ANewPriority: TTaskPriority): Integer;
var
  i, successCount: Integer;
begin
  successCount := 0;
  for i := 0 to High(ATaskIDs) do
    if UpdateTaskPriority(ATaskIDs[i], ANewPriority) then
      Inc(successCount);
  Result := successCount;
end;

function TIntelligenceTaskManager.BulkUpdateCategory(
  const ATaskIDs: array of Integer; const ANewCategory: string): Integer;
var
  i, successCount: Integer;
begin
  successCount := 0;
  for i := 0 to High(ATaskIDs) do
    if UpdateTaskCategory(ATaskIDs[i], ANewCategory) then
      Inc(successCount);
  Result := successCount;
end;

function TIntelligenceTaskManager.BulkAddTag(
  const ATaskIDs: array of Integer; const ATag: string): Integer;
var
  i, successCount: Integer;
begin
  successCount := 0;
  for i := 0 to High(ATaskIDs) do
    if AddTagToTask(ATaskIDs[i], ATag) then
      Inc(successCount);
  Result := successCount;
end;

function TIntelligenceTaskManager.BulkDelete(
  const ATaskIDs: array of Integer): Integer;
var
  i, successCount: Integer;
begin
  successCount := 0;
  for i := 0 to High(ATaskIDs) do
    if DeleteTask(ATaskIDs[i]) then
      Inc(successCount);
  Result := successCount;
end;

function TIntelligenceTaskManager.BulkArchive(
  const ATaskIDs: array of Integer; const AReason: string): Integer;
var
  i, successCount: Integer;
begin
  successCount := 0;
  for i := 0 to High(ATaskIDs) do
    if ArchiveTask(ATaskIDs[i], AReason) > 0 then
      Inc(successCount);
  Result := successCount;
end;

function TIntelligenceTaskManager.GetBulkOperationHistory: TBulkOperationArray;
begin
  Result := Copy(FBulkOperations, 0, Length(FBulkOperations));
end;


{ Helper method implementations }
function TIntelligenceTaskManager.GetInheritedTaskCount: Integer;
begin
  Result := Length(GetInheritedAllTasks);
end;

function TIntelligenceTaskManager.GetInheritedCompletedCount: Integer;
var
  tasks: TTaskArray;
  i, count: Integer;
begin
  count := 0;
  tasks := GetInheritedAllTasks;
  for i := 0 to High(tasks) do
    if tasks[i].Status = tsCompleted then
      Inc(count);
  Result := count;
end;

function TIntelligenceTaskManager.GetInheritedPendingCount: Integer;
var
  tasks: TTaskArray;
  i, count: Integer;
begin
  count := 0;
  tasks := GetInheritedAllTasks;
  for i := 0 to High(tasks) do
    if tasks[i].Status = tsNotStarted then
      Inc(count);
  Result := count;
end;

function TIntelligenceTaskManager.GetInheritedAllTasks: TTaskArray;
begin
  Result := TTaskManager(Self).GetAllTasks;
end;

function TIntelligenceTaskManager.GetInheritedCSVExport: string;
begin
  Result := TTaskManager(Self).ExportToCSV;
end;


function TIntelligenceTaskManager.GetInheritedTaskStatusToString(AStatus: TTaskStatus): string;
begin
  Result := TTaskManager(Self).TaskStatusToString(AStatus);
end;

function TIntelligenceTaskManager.GetInheritedTaskPriorityToString(APriority: TTaskPriority): string;
begin
  Result := TTaskManager(Self).TaskPriorityToString(APriority);
end;


end.
