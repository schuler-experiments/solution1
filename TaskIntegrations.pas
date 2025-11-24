
unit TaskIntegrations;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TIntegrationType = (itEmail, itSlack, itCalendar, itJira, itAsana, itWebhook);

  TIntegrationStatus = (isDisabled, isConnecting, isConnected, isError);

  TIntegrationConfig = record
    ID: string;
    Name: string;
    IntegrationType: TIntegrationType;
    Status: TIntegrationStatus;
    ApiUrl: string;
    ApiKey: string;
    IsActive: boolean;
    CreatedTime: TDateTime;
    LastSyncTime: TDateTime;
    SyncIntervalMinutes: integer;
    ErrorMessage: string;
  end;

  TIntegrationConfigArray = array of TIntegrationConfig;

  TIntegrationEvent = record
    ID: string;
    IntegrationID: string;
    TaskID: string;
    EventType: string;
    Timestamp: TDateTime;
    Payload: string;
    IsProcessed: boolean;
  end;

  TIntegrationEventArray = array of TIntegrationEvent;

  TSyncStatus = record
    lastSync: TDateTime;
    isConnected: boolean;
    errorCount: integer;
  end;

  TTaskIntegrationManagerClass = class
  private
    FConfigs: TIntegrationConfigArray;
    FEvents: TIntegrationEventArray;
    FConfigCounter: integer;
    FEventCounter: integer;
    function GenerateConfigID(): string;
    function GenerateEventID(): string;
    function FindConfigIndex(aConfigID: string): integer;
  public
    constructor Create();
    destructor Destroy(); override;
    function AddIntegration(aName: string; aType: TIntegrationType;
                           aApiUrl: string; aApiKey: string): string;
    function GetIntegration(aConfigID: string): TIntegrationConfig;
    function GetIntegrationsByType(aType: TIntegrationType): TIntegrationConfigArray;
    function GetActiveIntegrations(): TIntegrationConfigArray;
    procedure UpdateIntegrationStatus(aConfigID: string; aStatus: TIntegrationStatus);
    procedure SetError(aConfigID: string; aErrorMessage: string);
    procedure UpdateSyncTime(aConfigID: string);
    procedure EnableIntegration(aConfigID: string);
    procedure DisableIntegration(aConfigID: string);
    function LogEvent(aIntegrationID: string; aTaskID: string;
                     aEventType: string; aPayload: string): string;
    function GetIntegrationEvents(aIntegrationID: string): TIntegrationEventArray;
    function GetTaskIntegrationEvents(aTaskID: string): TIntegrationEventArray;
    function GetUnprocessedEvents(): TIntegrationEventArray;
    procedure MarkEventProcessed(aEventID: string);
    function GetSyncStatus(aConfigID: string): TSyncStatus;
    function GetIntegrationCount(): integer;
    function GetIntegrationTypeName(aType: TIntegrationType): string;
    function GetStatusName(aStatus: TIntegrationStatus): string;
    procedure ClearProcessedEvents();
    function GetAllConfigs(): TIntegrationConfigArray;
    procedure SelfTest();
  end;

implementation

constructor TTaskIntegrationManagerClass.Create();
begin
  inherited Create();
  SetLength(FConfigs, 0);
  SetLength(FEvents, 0);
  FConfigCounter := 0;
  FEventCounter := 0;
end;

destructor TTaskIntegrationManagerClass.Destroy();
begin
  SetLength(FConfigs, 0);
  SetLength(FEvents, 0);
  inherited Destroy();
end;

function TTaskIntegrationManagerClass.GenerateConfigID(): string;
begin
  inc(FConfigCounter);
  GenerateConfigID := 'INT-' + IntToStr(FConfigCounter);
end;

function TTaskIntegrationManagerClass.GenerateEventID(): string;
begin
  inc(FEventCounter);
  GenerateEventID := 'INTEVENT-' + IntToStr(FEventCounter);
end;

function TTaskIntegrationManagerClass.FindConfigIndex(aConfigID: string): integer;
var
  i: integer;
begin
  FindConfigIndex := -1;
  for i := 0 to Length(FConfigs) - 1 do
  begin
    if FConfigs[i].ID = aConfigID then
    begin
      FindConfigIndex := i;
      Exit;
    end;
  end;
end;

function TTaskIntegrationManagerClass.AddIntegration(aName: string; aType: TIntegrationType;
                                                      aApiUrl: string; aApiKey: string): string;
var
  newConfig: TIntegrationConfig;
  idx: integer;
begin
  newConfig.ID := GenerateConfigID();
  newConfig.Name := aName;
  newConfig.IntegrationType := aType;
  newConfig.Status := isDisabled;
  newConfig.ApiUrl := aApiUrl;
  newConfig.ApiKey := aApiKey;
  newConfig.IsActive := false;
  newConfig.CreatedTime := Now();
  newConfig.LastSyncTime := 0;
  newConfig.SyncIntervalMinutes := 30;
  newConfig.ErrorMessage := '';

  idx := Length(FConfigs);
  SetLength(FConfigs, idx + 1);
  FConfigs[idx] := newConfig;

  AddIntegration := newConfig.ID;
end;

function TTaskIntegrationManagerClass.GetIntegration(aConfigID: string): TIntegrationConfig;
var
  idx: integer;
  emptyConfig: TIntegrationConfig;
begin
  FillChar(emptyConfig, SizeOf(emptyConfig), 0);
  idx := FindConfigIndex(aConfigID);
  if idx >= 0 then
    GetIntegration := FConfigs[idx]
  else
    GetIntegration := emptyConfig;
end;

function TTaskIntegrationManagerClass.GetIntegrationsByType(aType: TIntegrationType): TIntegrationConfigArray;
var
  i, count: integer;
  configArray: TIntegrationConfigArray;
begin
  count := 0;
  SetLength(configArray, 0);
  for i := 0 to Length(FConfigs) - 1 do
  begin
    if FConfigs[i].IntegrationType = aType then
    begin
      SetLength(configArray, count + 1);
      configArray[count] := FConfigs[i];
      inc(count);
    end;
  end;
  GetIntegrationsByType := configArray;
end;

function TTaskIntegrationManagerClass.GetActiveIntegrations(): TIntegrationConfigArray;
var
  i, count: integer;
  configArray: TIntegrationConfigArray;
begin
  count := 0;
  SetLength(configArray, 0);
  for i := 0 to Length(FConfigs) - 1 do
  begin
    if FConfigs[i].IsActive then
    begin
      SetLength(configArray, count + 1);
      configArray[count] := FConfigs[i];
      inc(count);
    end;
  end;
  GetActiveIntegrations := configArray;
end;

procedure TTaskIntegrationManagerClass.UpdateIntegrationStatus(aConfigID: string; aStatus: TIntegrationStatus);
var
  idx: integer;
begin
  idx := FindConfigIndex(aConfigID);
  if idx >= 0 then
  begin
    FConfigs[idx].Status := aStatus;
    if aStatus = isConnected then
    begin
      FConfigs[idx].IsActive := true;
      FConfigs[idx].ErrorMessage := '';
    end;
  end;
end;

procedure TTaskIntegrationManagerClass.SetError(aConfigID: string; aErrorMessage: string);
var
  idx: integer;
begin
  idx := FindConfigIndex(aConfigID);
  if idx >= 0 then
  begin
    FConfigs[idx].Status := isError;
    FConfigs[idx].ErrorMessage := aErrorMessage;
  end;
end;

procedure TTaskIntegrationManagerClass.UpdateSyncTime(aConfigID: string);
var
  idx: integer;
begin
  idx := FindConfigIndex(aConfigID);
  if idx >= 0 then
    FConfigs[idx].LastSyncTime := Now();
end;

procedure TTaskIntegrationManagerClass.EnableIntegration(aConfigID: string);
var
  idx: integer;
begin
  idx := FindConfigIndex(aConfigID);
  if idx >= 0 then
    FConfigs[idx].IsActive := true;
end;

procedure TTaskIntegrationManagerClass.DisableIntegration(aConfigID: string);
var
  idx: integer;
begin
  idx := FindConfigIndex(aConfigID);
  if idx >= 0 then
    FConfigs[idx].IsActive := false;
end;

function TTaskIntegrationManagerClass.LogEvent(aIntegrationID: string; aTaskID: string;
                                               aEventType: string; aPayload: string): string;
var
  newEvent: TIntegrationEvent;
  idx: integer;
begin
  newEvent.ID := GenerateEventID();
  newEvent.IntegrationID := aIntegrationID;
  newEvent.TaskID := aTaskID;
  newEvent.EventType := aEventType;
  newEvent.Timestamp := Now();
  newEvent.Payload := aPayload;
  newEvent.IsProcessed := false;

  idx := Length(FEvents);
  SetLength(FEvents, idx + 1);
  FEvents[idx] := newEvent;

  LogEvent := newEvent.ID;
end;

function TTaskIntegrationManagerClass.GetIntegrationEvents(aIntegrationID: string): TIntegrationEventArray;
var
  i, count: integer;
  eventArray: TIntegrationEventArray;
begin
  count := 0;
  SetLength(eventArray, 0);
  for i := 0 to Length(FEvents) - 1 do
  begin
    if FEvents[i].IntegrationID = aIntegrationID then
    begin
      SetLength(eventArray, count + 1);
      eventArray[count] := FEvents[i];
      inc(count);
    end;
  end;
  GetIntegrationEvents := eventArray;
end;

function TTaskIntegrationManagerClass.GetTaskIntegrationEvents(aTaskID: string): TIntegrationEventArray;
var
  i, count: integer;
  eventArray: TIntegrationEventArray;
begin
  count := 0;
  SetLength(eventArray, 0);
  for i := 0 to Length(FEvents) - 1 do
  begin
    if FEvents[i].TaskID = aTaskID then
    begin
      SetLength(eventArray, count + 1);
      eventArray[count] := FEvents[i];
      inc(count);
    end;
  end;
  GetTaskIntegrationEvents := eventArray;
end;

function TTaskIntegrationManagerClass.GetUnprocessedEvents(): TIntegrationEventArray;
var
  i, count: integer;
  eventArray: TIntegrationEventArray;
begin
  count := 0;
  SetLength(eventArray, 0);
  for i := 0 to Length(FEvents) - 1 do
  begin
    if not FEvents[i].IsProcessed then
    begin
      SetLength(eventArray, count + 1);
      eventArray[count] := FEvents[i];
      inc(count);
    end;
  end;
  GetUnprocessedEvents := eventArray;
end;

procedure TTaskIntegrationManagerClass.MarkEventProcessed(aEventID: string);
var
  i: integer;
begin
  for i := 0 to Length(FEvents) - 1 do
  begin
    if FEvents[i].ID = aEventID then
    begin
      FEvents[i].IsProcessed := true;
      Exit;
    end;
  end;
end;

function TTaskIntegrationManagerClass.GetSyncStatus(aConfigID: string): TSyncStatus;
var
  syncStatus: TSyncStatus;
  idx: integer;
begin
  idx := FindConfigIndex(aConfigID);
  syncStatus.lastSync := 0;
  syncStatus.isConnected := false;
  syncStatus.errorCount := 0;

  if idx >= 0 then
  begin
    syncStatus.lastSync := FConfigs[idx].LastSyncTime;
    syncStatus.isConnected := FConfigs[idx].Status = isConnected;
  end;

  GetSyncStatus := syncStatus;
end;

function TTaskIntegrationManagerClass.GetIntegrationCount(): integer;
begin
  GetIntegrationCount := Length(FConfigs);
end;

function TTaskIntegrationManagerClass.GetIntegrationTypeName(aType: TIntegrationType): string;
begin
  case aType of
    itEmail: GetIntegrationTypeName := 'Email';
    itSlack: GetIntegrationTypeName := 'Slack';
    itCalendar: GetIntegrationTypeName := 'Calendar';
    itJira: GetIntegrationTypeName := 'Jira';
    itAsana: GetIntegrationTypeName := 'Asana';
    itWebhook: GetIntegrationTypeName := 'Webhook';
  else
    GetIntegrationTypeName := 'Unknown';
  end;
end;

function TTaskIntegrationManagerClass.GetStatusName(aStatus: TIntegrationStatus): string;
begin
  case aStatus of
    isDisabled: GetStatusName := 'Disabled';
    isConnecting: GetStatusName := 'Connecting';
    isConnected: GetStatusName := 'Connected';
    isError: GetStatusName := 'Error';
  else
    GetStatusName := 'Unknown';
  end;
end;

procedure TTaskIntegrationManagerClass.ClearProcessedEvents();
var
  i, writeIdx: integer;
begin
  writeIdx := 0;
  for i := 0 to Length(FEvents) - 1 do
  begin
    if not FEvents[i].IsProcessed then
    begin
      FEvents[writeIdx] := FEvents[i];
      inc(writeIdx);
    end;
  end;
  SetLength(FEvents, writeIdx);
end;

function TTaskIntegrationManagerClass.GetAllConfigs(): TIntegrationConfigArray;
begin
  GetAllConfigs := FConfigs;
end;

procedure TTaskIntegrationManagerClass.SelfTest();
var
  intID1, intID2: string;
  eventID: string;
  configs: TIntegrationConfigArray;
  events: TIntegrationEventArray;
begin
  WriteLn('--- Task Integration Manager Self Test ---');

  { Test adding integrations }
  intID1 := AddIntegration('Slack Integration', itSlack,
    'https://hooks.slack.com/api', 'xoxb-token-123');
  intID2 := AddIntegration('Calendar Sync', itCalendar,
    'https://calendar.google.com/api', 'auth-token-456');

  WriteLn('✓ Created 2 integrations');

  { Test updating status }
  UpdateIntegrationStatus(intID1, isConnected);
  UpdateSyncTime(intID1);
  WriteLn('✓ Updated integration status');

  { Test logging events }
  eventID := LogEvent(intID1, 'TASK-001', 'task_created',
    '{"task": "Important Project", "priority": "high"}');
  LogEvent(intID1, 'TASK-002', 'task_updated', '{"status": "in_progress"}');

  WriteLn('✓ Logged 2 events');

  { Test retrieving events }
  events := GetIntegrationEvents(intID1);
  if Length(events) = 2 then
    WriteLn('✓ Event retrieval works')
  else
    WriteLn('✗ Event retrieval failed');

  { Test unprocessed events }
  events := GetUnprocessedEvents();
  if Length(events) = 2 then
    WriteLn('✓ Unprocessed events: ', Length(events))
  else
    WriteLn('✗ Unprocessed events failed');

  { Test marking events processed }
  MarkEventProcessed(eventID);
  events := GetUnprocessedEvents();
  if Length(events) = 1 then
    WriteLn('✓ Mark event processed works')
  else
    WriteLn('✗ Mark event processed failed');

  { Test active integrations }
  configs := GetActiveIntegrations();
  if Length(configs) >= 1 then
    WriteLn('✓ Active integrations: ', Length(configs))
  else
    WriteLn('✗ Active integrations failed');

  WriteLn('--- Integration Manager Tests Complete ---');
  WriteLn('');
end;

end.
