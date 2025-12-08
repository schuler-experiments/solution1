
unit taskmanagerboards;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Classes, taskmanagersmart;

type
  // Board column status types
  TColumnType = (ctBacklog, ctTodo, ctInProgress, ctReview, ctDone, ctCustom);
  
  // Board template types
  TBoardTemplate = (btKanban, btScrum, btCustom);
  
  // Sprint status
  TSprintStatus = (ssPlanning, ssActive, ssCompleted, ssCancelled);
  
  // Board column definition
  TBoardColumn = record
    ID: Integer;
    BoardID: Integer;
    Name: string;
    ColumnType: TColumnType;
    Position: Integer;
    WIPLimit: Integer;  // Work In Progress limit (0 = no limit)
    TaskCount: Integer;
    Color: string;
  end;
  
  // Task card on a board
  TTaskCard = record
    ID: Integer;
    TaskID: Integer;
    BoardID: Integer;
    ColumnID: Integer;
    SwimLaneID: Integer;
    Position: Integer;
    EnteredColumnDate: TDateTime;
    ExitedColumnDate: TDateTime;
  end;
  
  // Swim lane for categorization
  TSwimLane = record
    ID: Integer;
    BoardID: Integer;
    Name: string;
    Position: Integer;
    Color: string;
  end;
  
  // Board definition
  TBoard = record
    ID: Integer;
    Name: string;
    Description: string;
    Template: TBoardTemplate;
    CreatedDate: TDateTime;
    IsActive: Boolean;
  end;
  
  // Sprint for agile workflow
  TSprint = record
    ID: Integer;
    BoardID: Integer;
    Name: string;
    Goal: string;
    StartDate: TDateTime;
    EndDate: TDateTime;
    Status: TSprintStatus;
    PlannedPoints: Integer;
    CompletedPoints: Integer;
    TasksCompleted: Integer;
    TasksPlanned: Integer;
  end;
  
  // Agile metrics
  TAgileMetrics = record
    BoardID: Integer;
    AverageCycleTime: Double;  // Hours from start to done
    AverageLeadTime: Double;   // Hours from backlog to done
    Velocity: Double;          // Story points per sprint
    Throughput: Double;        // Tasks completed per day
    WIPAverage: Double;        // Average work in progress
  end;

  // Dynamic arrays
  TBoardArray = array of TBoard;
  TColumnArray = array of TBoardColumn;
  TTaskCardArray = array of TTaskCard;
  TSwimLaneArray = array of TSwimLane;
  TSprintArray = array of TSprint;

  // Board Task Manager class
  TBoardTaskManager = class(TSmartTaskManager)
  private
    FBoards: TBoardArray;
    FColumns: TColumnArray;
    FCards: TTaskCardArray;
    FSwimLanes: TSwimLaneArray;
    FSprints: TSprintArray;
    FNextBoardID: Integer;
    FNextColumnID: Integer;
    FNextCardID: Integer;
    FNextSwimLaneID: Integer;
    FNextSprintID: Integer;
    
    function FindBoardIndex(ABoardID: Integer): Integer;
    function FindColumnIndex(AColumnID: Integer): Integer;
    function FindCardIndex(ACardID: Integer): Integer;
    function FindSwimLaneIndex(ASwimLaneID: Integer): Integer;
    function FindSprintIndex(ASprintID: Integer): Integer;
    function CalculateCycleTime(ACardID: Integer): Double;
    function GetColumnTaskCount(AColumnID: Integer): Integer;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Board management
    function CreateBoard(const AName, ADescription: string; 
      ATemplate: TBoardTemplate): Integer;
    function DeleteBoard(ABoardID: Integer): Boolean;
    function GetBoard(ABoardID: Integer): TBoard;
    function GetAllBoards: TBoardArray;
    function GetActiveBoards: TBoardArray;
    function ArchiveBoard(ABoardID: Integer): Boolean;
    
    // Column management
    function AddColumn(ABoardID: Integer; const AName: string; 
      AType: TColumnType; AWIPLimit: Integer): Integer;
    function UpdateColumn(AColumnID: Integer; const AName: string; 
      AWIPLimit: Integer): Boolean;
    function DeleteColumn(AColumnID: Integer): Boolean;
    function GetBoardColumns(ABoardID: Integer): TColumnArray;
    function ReorderColumns(ABoardID: Integer; const AColumnIDs: array of Integer): Boolean;
    
    // Card management
    function AddTaskToBoard(ATaskID, ABoardID, AColumnID: Integer): Integer;
    function MoveCard(ACardID, ANewColumnID: Integer): Boolean;
    function MoveCardToSwimLane(ACardID, ASwimLaneID: Integer): Boolean;
    function RemoveCardFromBoard(ACardID: Integer): Boolean;
    function GetBoardCards(ABoardID: Integer): TTaskCardArray;
    function GetColumnCards(AColumnID: Integer): TTaskCardArray;
    
    // Swim lane management
    function AddSwimLane(ABoardID: Integer; const AName, AColor: string): Integer;
    function DeleteSwimLane(ASwimLaneID: Integer): Boolean;
    function GetBoardSwimLanes(ABoardID: Integer): TSwimLaneArray;
    
    // Sprint management
    function CreateSprint(ABoardID: Integer; const AName, AGoal: string;
      AStartDate, AEndDate: TDateTime): Integer;
    function StartSprint(ASprintID: Integer): Boolean;
    function CompleteSprint(ASprintID: Integer): Boolean;
    function AddTaskToSprint(ASprintID, ATaskID, APoints: Integer): Boolean;
    function GetActiveSprint(ABoardID: Integer): Integer;
    function GetSprintMetrics(ASprintID: Integer): string;
    
    // Metrics and analytics
    function CalculateBoardMetrics(ABoardID: Integer): TAgileMetrics;
    function GetVelocity(ABoardID: Integer; ALastNSprints: Integer): Double;
    function GetBurndownData(ASprintID: Integer): string;
    function DetectBottlenecks(ABoardID: Integer): string;
    function GetCycleTimeReport(ABoardID: Integer): string;
    
    // Visualization helpers
    function RenderBoard(ABoardID: Integer): string;
    function ExportBoardToHTML(ABoardID: Integer): string;
    
    // Utility functions
    function ColumnTypeToString(AType: TColumnType): string;
    function BoardTemplateToString(ATemplate: TBoardTemplate): string;
    function SprintStatusToString(AStatus: TSprintStatus): string;
    
    // Persistence
    function SaveBoardDataToFile(const AFilename: string): Boolean;
    function LoadBoardDataFromFile(const AFilename: string): Boolean;
  end;

implementation

constructor TBoardTaskManager.Create;
begin
  inherited Create;
  SetLength(FBoards, 0);
  SetLength(FColumns, 0);
  SetLength(FCards, 0);
  SetLength(FSwimLanes, 0);
  SetLength(FSprints, 0);
  FNextBoardID := 1;
  FNextColumnID := 1;
  FNextCardID := 1;
  FNextSwimLaneID := 1;
  FNextSprintID := 1;
end;

destructor TBoardTaskManager.Destroy;
begin
  SetLength(FBoards, 0);
  SetLength(FColumns, 0);
  SetLength(FCards, 0);
  SetLength(FSwimLanes, 0);
  SetLength(FSprints, 0);
  inherited Destroy;
end;

function TBoardTaskManager.FindBoardIndex(ABoardID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FBoards) do
    if FBoards[i].ID = ABoardID then
    begin
      Result := i;
      Exit;
    end;
end;

function TBoardTaskManager.FindColumnIndex(AColumnID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FColumns) do
    if FColumns[i].ID = AColumnID then
    begin
      Result := i;
      Exit;
    end;
end;

function TBoardTaskManager.FindCardIndex(ACardID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FCards) do
    if FCards[i].ID = ACardID then
    begin
      Result := i;
      Exit;
    end;
end;

function TBoardTaskManager.FindSwimLaneIndex(ASwimLaneID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FSwimLanes) do
    if FSwimLanes[i].ID = ASwimLaneID then
    begin
      Result := i;
      Exit;
    end;
end;

function TBoardTaskManager.FindSprintIndex(ASprintID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FSprints) do
    if FSprints[i].ID = ASprintID then
    begin
      Result := i;
      Exit;
    end;
end;

function TBoardTaskManager.GetColumnTaskCount(AColumnID: Integer): Integer;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(FCards) do
    if FCards[i].ColumnID = AColumnID then
      Inc(count);
  Result := count;
end;

function TBoardTaskManager.CreateBoard(const AName, ADescription: string; 
  ATemplate: TBoardTemplate): Integer;
var
  idx: Integer;
  board: TBoard;
begin
  board.ID := FNextBoardID;
  Inc(FNextBoardID);
  board.Name := AName;
  board.Description := ADescription;
  board.Template := ATemplate;
  board.CreatedDate := Now;
  board.IsActive := True;
  
  idx := Length(FBoards);
  SetLength(FBoards, idx + 1);
  FBoards[idx] := board;
  
  // Create default columns based on template
  case ATemplate of
    btKanban:
    begin
      AddColumn(board.ID, 'Backlog', ctBacklog, 0);
      AddColumn(board.ID, 'To Do', ctTodo, 0);
      AddColumn(board.ID, 'In Progress', ctInProgress, 3);
      AddColumn(board.ID, 'Done', ctDone, 0);
    end;
    btScrum:
    begin
      AddColumn(board.ID, 'Backlog', ctBacklog, 0);
      AddColumn(board.ID, 'Sprint Backlog', ctTodo, 0);
      AddColumn(board.ID, 'In Progress', ctInProgress, 5);
      AddColumn(board.ID, 'Review', ctReview, 3);
      AddColumn(board.ID, 'Done', ctDone, 0);
    end;
  end;
  
  Result := board.ID;
end;

function TBoardTaskManager.DeleteBoard(ABoardID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindBoardIndex(ABoardID);
  if idx = -1 then Exit;
  
  // Remove all columns, cards, and swim lanes for this board
  for i := High(FColumns) downto 0 do
    if FColumns[i].BoardID = ABoardID then
      DeleteColumn(FColumns[i].ID);
      
  for i := High(FSwimLanes) downto 0 do
    if FSwimLanes[i].BoardID = ABoardID then
      DeleteSwimLane(FSwimLanes[i].ID);
  
  // Remove board
  for i := idx to High(FBoards) - 1 do
    FBoards[i] := FBoards[i + 1];
  SetLength(FBoards, Length(FBoards) - 1);
  
  Result := True;
end;

function TBoardTaskManager.GetBoard(ABoardID: Integer): TBoard;
var
  idx: Integer;
begin
  idx := FindBoardIndex(ABoardID);
  if idx >= 0 then
    Result := FBoards[idx]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TBoardTaskManager.GetAllBoards: TBoardArray;
begin
  Result := Copy(FBoards, 0, Length(FBoards));
end;

function TBoardTaskManager.GetActiveBoards: TBoardArray;
var
  i, count: Integer;
begin
  count := 0;
  SetLength(Result, 0);
  for i := 0 to High(FBoards) do
    if FBoards[i].IsActive then
    begin
      SetLength(Result, count + 1);
      Result[count] := FBoards[i];
      Inc(count);
    end;
end;

function TBoardTaskManager.ArchiveBoard(ABoardID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindBoardIndex(ABoardID);
  if idx = -1 then Exit;
  
  FBoards[idx].IsActive := False;
  Result := True;
end;

function TBoardTaskManager.AddColumn(ABoardID: Integer; const AName: string; 
  AType: TColumnType; AWIPLimit: Integer): Integer;
var
  idx: Integer;
  col: TBoardColumn;
begin
  col.ID := FNextColumnID;
  Inc(FNextColumnID);
  col.BoardID := ABoardID;
  col.Name := AName;
  col.ColumnType := AType;
  col.Position := Length(FColumns);
  col.WIPLimit := AWIPLimit;
  col.TaskCount := 0;
  col.Color := '';
  
  idx := Length(FColumns);
  SetLength(FColumns, idx + 1);
  FColumns[idx] := col;
  
  Result := col.ID;
end;

function TBoardTaskManager.UpdateColumn(AColumnID: Integer; const AName: string; 
  AWIPLimit: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindColumnIndex(AColumnID);
  if idx = -1 then Exit;
  
  FColumns[idx].Name := AName;
  FColumns[idx].WIPLimit := AWIPLimit;
  Result := True;
end;

function TBoardTaskManager.DeleteColumn(AColumnID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindColumnIndex(AColumnID);
  if idx = -1 then Exit;
  
  // Remove all cards in this column
  for i := High(FCards) downto 0 do
    if FCards[i].ColumnID = AColumnID then
      RemoveCardFromBoard(FCards[i].ID);
  
  // Remove column
  for i := idx to High(FColumns) - 1 do
    FColumns[i] := FColumns[i + 1];
  SetLength(FColumns, Length(FColumns) - 1);
  
  Result := True;
end;

function TBoardTaskManager.GetBoardColumns(ABoardID: Integer): TColumnArray;
var
  i, count: Integer;
begin
  count := 0;
  SetLength(Result, 0);
  for i := 0 to High(FColumns) do
    if FColumns[i].BoardID = ABoardID then
    begin
      SetLength(Result, count + 1);
      Result[count] := FColumns[i];
      Inc(count);
    end;
end;

function TBoardTaskManager.ReorderColumns(ABoardID: Integer; 
  const AColumnIDs: array of Integer): Boolean;
var
  i, idx: Integer;
begin
  Result := True;
  for i := 0 to High(AColumnIDs) do
  begin
    idx := FindColumnIndex(AColumnIDs[i]);
    if (idx >= 0) and (FColumns[idx].BoardID = ABoardID) then
      FColumns[idx].Position := i
    else
      Result := False;
  end;
end;

function TBoardTaskManager.AddTaskToBoard(ATaskID, ABoardID, AColumnID: Integer): Integer;
var
  idx: Integer;
  card: TTaskCard;
begin
  card.ID := FNextCardID;
  Inc(FNextCardID);
  card.TaskID := ATaskID;
  card.BoardID := ABoardID;
  card.ColumnID := AColumnID;
  card.SwimLaneID := 0;
  card.Position := 0;
  card.EnteredColumnDate := Now;
  card.ExitedColumnDate := 0;
  
  idx := Length(FCards);
  SetLength(FCards, idx + 1);
  FCards[idx] := card;
  
  Result := card.ID;
end;

function TBoardTaskManager.MoveCard(ACardID, ANewColumnID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindCardIndex(ACardID);
  if idx = -1 then Exit;
  
  FCards[idx].ExitedColumnDate := Now;
  FCards[idx].ColumnID := ANewColumnID;
  FCards[idx].EnteredColumnDate := Now;
  Result := True;
end;

function TBoardTaskManager.MoveCardToSwimLane(ACardID, ASwimLaneID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindCardIndex(ACardID);
  if idx = -1 then Exit;
  
  FCards[idx].SwimLaneID := ASwimLaneID;
  Result := True;
end;

function TBoardTaskManager.RemoveCardFromBoard(ACardID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindCardIndex(ACardID);
  if idx = -1 then Exit;
  
  for i := idx to High(FCards) - 1 do
    FCards[i] := FCards[i + 1];
  SetLength(FCards, Length(FCards) - 1);
  
  Result := True;
end;

function TBoardTaskManager.GetBoardCards(ABoardID: Integer): TTaskCardArray;
var
  i, count: Integer;
begin
  count := 0;
  SetLength(Result, 0);
  for i := 0 to High(FCards) do
    if FCards[i].BoardID = ABoardID then
    begin
      SetLength(Result, count + 1);
      Result[count] := FCards[i];
      Inc(count);
    end;
end;

function TBoardTaskManager.GetColumnCards(AColumnID: Integer): TTaskCardArray;
var
  i, count: Integer;
begin
  count := 0;
  SetLength(Result, 0);
  for i := 0 to High(FCards) do
    if FCards[i].ColumnID = AColumnID then
    begin
      SetLength(Result, count + 1);
      Result[count] := FCards[i];
      Inc(count);
    end;
end;

function TBoardTaskManager.AddSwimLane(ABoardID: Integer; const AName, AColor: string): Integer;
var
  idx: Integer;
  lane: TSwimLane;
begin
  lane.ID := FNextSwimLaneID;
  Inc(FNextSwimLaneID);
  lane.BoardID := ABoardID;
  lane.Name := AName;
  lane.Position := Length(FSwimLanes);
  lane.Color := AColor;
  
  idx := Length(FSwimLanes);
  SetLength(FSwimLanes, idx + 1);
  FSwimLanes[idx] := lane;
  
  Result := lane.ID;
end;

function TBoardTaskManager.DeleteSwimLane(ASwimLaneID: Integer): Boolean;
var
  idx, i: Integer;
begin
  Result := False;
  idx := FindSwimLaneIndex(ASwimLaneID);
  if idx = -1 then Exit;
  
  for i := idx to High(FSwimLanes) - 1 do
    FSwimLanes[i] := FSwimLanes[i + 1];
  SetLength(FSwimLanes, Length(FSwimLanes) - 1);
  
  Result := True;
end;

function TBoardTaskManager.GetBoardSwimLanes(ABoardID: Integer): TSwimLaneArray;
var
  i, count: Integer;
begin
  count := 0;
  SetLength(Result, 0);
  for i := 0 to High(FSwimLanes) do
    if FSwimLanes[i].BoardID = ABoardID then
    begin
      SetLength(Result, count + 1);
      Result[count] := FSwimLanes[i];
      Inc(count);
    end;
end;

function TBoardTaskManager.CreateSprint(ABoardID: Integer; const AName, AGoal: string;
  AStartDate, AEndDate: TDateTime): Integer;
var
  idx: Integer;
  sprint: TSprint;
begin
  sprint.ID := FNextSprintID;
  Inc(FNextSprintID);
  sprint.BoardID := ABoardID;
  sprint.Name := AName;
  sprint.Goal := AGoal;
  sprint.StartDate := AStartDate;
  sprint.EndDate := AEndDate;
  sprint.Status := ssPlanning;
  sprint.PlannedPoints := 0;
  sprint.CompletedPoints := 0;
  sprint.TasksCompleted := 0;
  sprint.TasksPlanned := 0;
  
  idx := Length(FSprints);
  SetLength(FSprints, idx + 1);
  FSprints[idx] := sprint;
  
  Result := sprint.ID;
end;

function TBoardTaskManager.StartSprint(ASprintID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindSprintIndex(ASprintID);
  if idx = -1 then Exit;
  
  FSprints[idx].Status := ssActive;
  FSprints[idx].StartDate := Now;
  Result := True;
end;

function TBoardTaskManager.CompleteSprint(ASprintID: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindSprintIndex(ASprintID);
  if idx = -1 then Exit;
  
  FSprints[idx].Status := ssCompleted;
  FSprints[idx].EndDate := Now;
  Result := True;
end;

function TBoardTaskManager.AddTaskToSprint(ASprintID, ATaskID, APoints: Integer): Boolean;
var
  idx: Integer;
begin
  Result := False;
  idx := FindSprintIndex(ASprintID);
  if idx = -1 then Exit;
  
  FSprints[idx].PlannedPoints := FSprints[idx].PlannedPoints + APoints;
  FSprints[idx].TasksPlanned := FSprints[idx].TasksPlanned + 1;
  Result := True;
end;

function TBoardTaskManager.GetActiveSprint(ABoardID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FSprints) do
    if (FSprints[i].BoardID = ABoardID) and (FSprints[i].Status = ssActive) then
    begin
      Result := FSprints[i].ID;
      Exit;
    end;
end;

function TBoardTaskManager.GetSprintMetrics(ASprintID: Integer): string;
var
  idx: Integer;
  sprint: TSprint;
  velocity, completion: Double;
begin
  idx := FindSprintIndex(ASprintID);
  if idx = -1 then
  begin
    Result := 'Sprint not found';
    Exit;
  end;
  
  sprint := FSprints[idx];
  if sprint.TasksPlanned > 0 then
    completion := (sprint.TasksCompleted / sprint.TasksPlanned) * 100
  else
    completion := 0;
    
  if sprint.PlannedPoints > 0 then
    velocity := (sprint.CompletedPoints / sprint.PlannedPoints) * 100
  else
    velocity := 0;
  
  Result := Format('Sprint: %s' + sLineBreak +
                   'Goal: %s' + sLineBreak +
                   'Status: %s' + sLineBreak +
                   'Tasks: %d/%d (%.1f%%)' + sLineBreak +
                   'Points: %d/%d (%.1f%% velocity)',
                   [sprint.Name, sprint.Goal, SprintStatusToString(sprint.Status),
                    sprint.TasksCompleted, sprint.TasksPlanned, completion,
                    sprint.CompletedPoints, sprint.PlannedPoints, velocity]);
end;

function TBoardTaskManager.CalculateCycleTime(ACardID: Integer): Double;
var
  idx: Integer;
  hours: Double;
begin
  Result := 0;
  idx := FindCardIndex(ACardID);
  if idx = -1 then Exit;
  
  if FCards[idx].ExitedColumnDate > 0 then
    hours := HoursBetween(FCards[idx].ExitedColumnDate, FCards[idx].EnteredColumnDate)
  else
    hours := HoursBetween(Now, FCards[idx].EnteredColumnDate);
    
  Result := hours;
end;

function TBoardTaskManager.CalculateBoardMetrics(ABoardID: Integer): TAgileMetrics;
var
  i, count: Integer;
  totalCycle: Double;
  cards: TTaskCardArray;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.BoardID := ABoardID;
  
  cards := GetBoardCards(ABoardID);
  count := 0;
  totalCycle := 0;
  
  for i := 0 to High(cards) do
  begin
    totalCycle := totalCycle + CalculateCycleTime(cards[i].ID);
    Inc(count);
  end;
  
  if count > 0 then
  begin
    Result.AverageCycleTime := totalCycle / count;
    Result.WIPAverage := count;
  end;
end;

function TBoardTaskManager.GetVelocity(ABoardID: Integer; ALastNSprints: Integer): Double;
var
  i, count: Integer;
  totalPoints: Integer;
begin
  count := 0;
  totalPoints := 0;
  
  for i := High(FSprints) downto 0 do
  begin
    if (FSprints[i].BoardID = ABoardID) and (FSprints[i].Status = ssCompleted) then
    begin
      totalPoints := totalPoints + FSprints[i].CompletedPoints;
      Inc(count);
      if count >= ALastNSprints then
        Break;
    end;
  end;
  
  if count > 0 then
    Result := totalPoints / count
  else
    Result := 0;
end;

function TBoardTaskManager.GetBurndownData(ASprintID: Integer): string;
var
  idx: Integer;
  sprint: TSprint;
  daysRemaining: Integer;
  idealBurnRate: Double;
  remainingPoints: Integer;
begin
  idx := FindSprintIndex(ASprintID);
  if idx = -1 then
  begin
    Result := 'Sprint not found';
    Exit;
  end;
  
  sprint := FSprints[idx];
  daysRemaining := DaysBetween(sprint.EndDate, Now);
  
  if sprint.PlannedPoints > 0 then
  begin
    idealBurnRate := sprint.PlannedPoints / DaysBetween(sprint.EndDate, sprint.StartDate);
    remainingPoints := sprint.PlannedPoints - sprint.CompletedPoints;
    
    Result := Format('Burndown for %s:' + sLineBreak +
                     'Total Points: %d' + sLineBreak +
                     'Completed: %d' + sLineBreak +
                     'Remaining: %d' + sLineBreak +
                     'Days Remaining: %d' + sLineBreak +
                     'Ideal Burn Rate: %.2f points/day',
                     [sprint.Name, sprint.PlannedPoints, sprint.CompletedPoints,
                      remainingPoints, daysRemaining, idealBurnRate]);
  end
  else
    Result := 'No points planned for this sprint';
end;

function TBoardTaskManager.DetectBottlenecks(ABoardID: Integer): string;
var
  i: Integer;
  cols: TColumnArray;
  maxCount, colCount: Integer;
  bottleneck: string;
begin
  cols := GetBoardColumns(ABoardID);
  maxCount := 0;
  bottleneck := '';
  
  for i := 0 to High(cols) do
  begin
    colCount := GetColumnTaskCount(cols[i].ID);
    if (cols[i].WIPLimit > 0) and (colCount > cols[i].WIPLimit) then
      bottleneck := bottleneck + Format('Column "%s" exceeds WIP limit: %d/%d' + sLineBreak,
                                        [cols[i].Name, colCount, cols[i].WIPLimit]);
    if colCount > maxCount then
    begin
      maxCount := colCount;
    end;
  end;
  
  if bottleneck = '' then
    Result := 'No bottlenecks detected'
  else
    Result := bottleneck;
end;

function TBoardTaskManager.GetCycleTimeReport(ABoardID: Integer): string;
var
  metrics: TAgileMetrics;
begin
  metrics := CalculateBoardMetrics(ABoardID);
  Result := Format('Average Cycle Time: %.2f hours' + sLineBreak +
                   'Average WIP: %.1f tasks',
                   [metrics.AverageCycleTime, metrics.WIPAverage]);
end;

function TBoardTaskManager.RenderBoard(ABoardID: Integer): string;
var
  i, j: Integer;
  cols: TColumnArray;
  colCards: TTaskCardArray;
  output: string;
begin
  cols := GetBoardColumns(ABoardID);
  output := '=== BOARD ===' + sLineBreak;
  
  for i := 0 to High(cols) do
  begin
    output := output + Format('%s [%d tasks]', [cols[i].Name, GetColumnTaskCount(cols[i].ID)]);
    if cols[i].WIPLimit > 0 then
      output := output + Format(' (WIP: %d)', [cols[i].WIPLimit]);
    output := output + sLineBreak;
    
    colCards := GetColumnCards(cols[i].ID);
    for j := 0 to High(colCards) do
      output := output + Format('  - Task #%d', [colCards[j].TaskID]) + sLineBreak;
    output := output + sLineBreak;
  end;
  
  Result := output;
end;

function TBoardTaskManager.ExportBoardToHTML(ABoardID: Integer): string;
begin
  Result := '<html><body><h1>Task Board</h1>' +
            '<div class="board">' + RenderBoard(ABoardID) + '</div>' +
            '</body></html>';
end;

function TBoardTaskManager.ColumnTypeToString(AType: TColumnType): string;
begin
  case AType of
    ctBacklog: Result := 'Backlog';
    ctTodo: Result := 'To Do';
    ctInProgress: Result := 'In Progress';
    ctReview: Result := 'Review';
    ctDone: Result := 'Done';
    ctCustom: Result := 'Custom';
  else
    Result := 'Unknown';
  end;
end;

function TBoardTaskManager.BoardTemplateToString(ATemplate: TBoardTemplate): string;
begin
  case ATemplate of
    btKanban: Result := 'Kanban';
    btScrum: Result := 'Scrum';
    btCustom: Result := 'Custom';
  else
    Result := 'Unknown';
  end;
end;

function TBoardTaskManager.SprintStatusToString(AStatus: TSprintStatus): string;
begin
  case AStatus of
    ssPlanning: Result := 'Planning';
    ssActive: Result := 'Active';
    ssCompleted: Result := 'Completed';
    ssCancelled: Result := 'Cancelled';
  else
    Result := 'Unknown';
  end;
end;

function TBoardTaskManager.SaveBoardDataToFile(const AFilename: string): Boolean;
begin
  // Simplified - would need actual file I/O implementation
  Result := True;
end;

function TBoardTaskManager.LoadBoardDataFromFile(const AFilename: string): Boolean;
begin
  // Simplified - would need actual file I/O implementation
  Result := True;
end;

end.
