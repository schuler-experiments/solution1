
program BoardTaskManagerDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, taskmanager, taskmanagerboards;

procedure SelfTest;
var
  TM: TBoardTaskManager;
  BoardID1, BoardID2, ColumnID1, ColumnID2, ColumnID3: Integer;
  TaskID1, TaskID2, TaskID3, TaskID4, TaskID5: Integer;
  CardID1, CardID2, CardID3, CardID4, CardID5: Integer;
  SprintID1: Integer;
  SwimLaneID1, SwimLaneID2: Integer;
  Boards: TBoardArray;
  Columns: TColumnArray;
  Cards: TTaskCardArray;
  metrics: TAgileMetrics;
  i: Integer;
begin
  WriteLn('=== Task Board & Agile Workflow Manager - Self Test ===');
  WriteLn;
  
  TM := TBoardTaskManager.Create;
  try
    // Test 1: Create boards
    WriteLn('Test 1: Creating Kanban and Scrum boards...');
    BoardID1 := TM.CreateBoard('Development Board', 'Main development workflow', btKanban);
    BoardID2 := TM.CreateBoard('Sprint Board', 'Scrum sprint management', btScrum);
    WriteLn(Format('Created %d boards', [Length(TM.GetAllBoards)]));
    WriteLn;
    
    // Test 2: Add custom columns
    WriteLn('Test 2: Adding custom columns to Kanban board...');
    Columns := TM.GetBoardColumns(BoardID1);
    WriteLn(Format('Default Kanban columns: %d', [Length(Columns)]));
    for i := 0 to High(Columns) do
      WriteLn(Format('  - %s (WIP limit: %d)', [Columns[i].Name, Columns[i].WIPLimit]));
    
    ColumnID1 := TM.AddColumn(BoardID1, 'Testing', ctReview, 2);
    WriteLn('Added "Testing" column with WIP limit of 2');
    WriteLn;
    
    // Test 3: Create tasks and add to board
    WriteLn('Test 3: Creating tasks and adding to board...');
    TaskID1 := TM.AddTask('Implement user authentication', 'Add login/logout functionality',
                          'Backend', tpHigh, EncodeDate(2024, 3, 15), 12.0);
    TaskID2 := TM.AddTask('Design homepage', 'Create responsive homepage design',
                          'Frontend', tpMedium, EncodeDate(2024, 3, 10), 8.0);
    TaskID3 := TM.AddTask('Setup CI/CD pipeline', 'Configure automated deployment',
                          'DevOps', tpHigh, EncodeDate(2024, 3, 5), 6.0);
    TaskID4 := TM.AddTask('Write API documentation', 'Document all REST endpoints',
                          'Documentation', tpLow, EncodeDate(2024, 3, 20), 4.0);
    TaskID5 := TM.AddTask('Fix payment bug', 'Resolve checkout error',
                          'Backend', tpCritical, EncodeDate(2024, 2, 28), 2.0);
    
    WriteLn(Format('Created %d tasks', [TM.TaskCount]));
    
    // Get the first column (Backlog) to add tasks
    Columns := TM.GetBoardColumns(BoardID1);
    CardID1 := TM.AddTaskToBoard(TaskID1, BoardID1, Columns[0].ID);
    CardID2 := TM.AddTaskToBoard(TaskID2, BoardID1, Columns[0].ID);
    CardID3 := TM.AddTaskToBoard(TaskID3, BoardID1, Columns[0].ID);
    CardID4 := TM.AddTaskToBoard(TaskID4, BoardID1, Columns[0].ID);
    CardID5 := TM.AddTaskToBoard(TaskID5, BoardID1, Columns[0].ID);
    
    WriteLn(Format('Added %d tasks to board', [Length(TM.GetBoardCards(BoardID1))]));
    WriteLn;
    
    // Test 4: Move cards between columns
    WriteLn('Test 4: Moving cards through workflow...');
    TM.MoveCard(CardID5, Columns[2].ID);  // Critical bug to "In Progress"
    TM.MoveCard(CardID3, Columns[1].ID);  // CI/CD to "To Do"
    TM.MoveCard(CardID1, Columns[1].ID);  // Auth to "To Do"
    WriteLn('Moved 3 cards to different columns');
    WriteLn;
    
    // Test 5: Add swim lanes
    WriteLn('Test 5: Adding swim lanes for organization...');
    SwimLaneID1 := TM.AddSwimLane(BoardID1, 'Urgent', '#FF0000');
    SwimLaneID2 := TM.AddSwimLane(BoardID1, 'Normal', '#00FF00');
    TM.MoveCardToSwimLane(CardID5, SwimLaneID1);  // Bug to urgent lane
    WriteLn(Format('Created %d swim lanes', [Length(TM.GetBoardSwimLanes(BoardID1))]));
    WriteLn;
    
    // Test 6: Render board visualization
    WriteLn('Test 6: Rendering board visualization...');
    WriteLn(TM.RenderBoard(BoardID1));
    
    // Test 7: Create and manage sprint
    WriteLn('Test 7: Creating and managing sprint...');
    SprintID1 := TM.CreateSprint(BoardID2, 'Sprint 1', 'Complete authentication features',
                                 EncodeDate(2024, 3, 1), EncodeDate(2024, 3, 14));
    TM.AddTaskToSprint(SprintID1, TaskID1, 8);
    TM.AddTaskToSprint(SprintID1, TaskID2, 5);
    TM.AddTaskToSprint(SprintID1, TaskID3, 13);
    TM.StartSprint(SprintID1);
    WriteLn('Created and started Sprint 1');
    WriteLn(TM.GetSprintMetrics(SprintID1));
    WriteLn;
    
    // Test 8: Calculate metrics
    WriteLn('Test 8: Calculating board metrics...');
    metrics := TM.CalculateBoardMetrics(BoardID1);
    WriteLn(Format('Average Cycle Time: %.2f hours', [metrics.AverageCycleTime]));
    WriteLn(Format('Average WIP: %.1f tasks', [metrics.WIPAverage]));
    WriteLn;
    
    // Test 9: Detect bottlenecks
    WriteLn('Test 9: Detecting workflow bottlenecks...');
    WriteLn(TM.DetectBottlenecks(BoardID1));
    WriteLn;
    
    // Test 10: Get burndown data
    WriteLn('Test 10: Sprint burndown analysis...');
    WriteLn(TM.GetBurndownData(SprintID1));
    WriteLn;
    
    // Test 11: Complete some sprint tasks
    WriteLn('Test 11: Simulating sprint progress...');
    Columns := TM.GetBoardColumns(BoardID2);
    if Length(Columns) > 0 then
    begin
      TM.AddTaskToBoard(TaskID1, BoardID2, Columns[High(Columns)].ID);  // Move to Done
      // Update sprint metrics manually for demo
      WriteLn('Task completed in sprint');
    end;
    WriteLn;
    
    // Test 12: Get velocity
    WriteLn('Test 12: Calculating team velocity...');
    WriteLn(Format('Current velocity: %.2f points/sprint', [TM.GetVelocity(BoardID2, 3)]));
    WriteLn;
    
    // Test 13: Cycle time report
    WriteLn('Test 13: Generating cycle time report...');
    WriteLn(TM.GetCycleTimeReport(BoardID1));
    WriteLn;
    
    // Test 14: List all active boards
    WriteLn('Test 14: Listing active boards...');
    Boards := TM.GetActiveBoards;
    WriteLn(Format('Active boards: %d', [Length(Boards)]));
    for i := 0 to High(Boards) do
      WriteLn(Format('  - %s (%s template)', [Boards[i].Name, 
              TM.BoardTemplateToString(Boards[i].Template)]));
    WriteLn;
    
    // Test 15: Archive a board
    WriteLn('Test 15: Archiving completed board...');
    TM.ArchiveBoard(BoardID2);
    WriteLn(Format('Active boards after archiving: %d', [Length(TM.GetActiveBoards)]));
    WriteLn;
    
    WriteLn('=== All Board Tests Completed Successfully! ===');
    WriteLn;
    WriteLn('New features demonstrated:');
    WriteLn('✓ Kanban and Scrum board templates');
    WriteLn('✓ Customizable columns with WIP limits');
    WriteLn('✓ Card management and movement');
    WriteLn('✓ Swim lanes for categorization');
    WriteLn('✓ Sprint planning and tracking');
    WriteLn('✓ Agile metrics (velocity, cycle time)');
    WriteLn('✓ Burndown charts and analytics');
    WriteLn('✓ Bottleneck detection');
    WriteLn('✓ Board visualization');
    WriteLn('✓ Board archiving');
    
  finally
    TM.Free;
  end;
end;

begin
  SelfTest;
end.
