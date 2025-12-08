
# Task Board & Agile Workflow Management Module

## Overview

The `taskmanagerboards.pas` unit extends the task manager with comprehensive visual board management and agile workflow capabilities. This module enables Kanban-style task management, sprint planning, and agile metrics tracking.

## Features

### 1. Board Management

Create and manage multiple task boards with different workflow templates:

- **Board Templates**:
  - **Kanban**: Continuous flow with Backlog → To Do → In Progress → Done
  - **Scrum**: Sprint-based with Backlog → Sprint Backlog → In Progress → Review → Done
  - **Custom**: Define your own workflow columns

```pascal
// Create boards
kanbanBoard := tm.CreateBoard('Development', 'Main dev workflow', btKanban);
scrumBoard := tm.CreateBoard('Sprint 1', 'Q1 Sprint', btScrum);

// Get all boards
boards := tm.GetAllBoards;

// Archive completed boards
tm.ArchiveBoard(oldBoardID);
```

### 2. Column Management

Customize your board columns with Work-In-Progress (WIP) limits:

```pascal
// Add custom column with WIP limit
columnID := tm.AddColumn(boardID, 'Code Review', ctReview, 3);  // Max 3 tasks

// Update column
tm.UpdateColumn(columnID, 'Peer Review', 5);  // Change name and WIP limit

// Get board columns
columns := tm.GetBoardColumns(boardID);

// Reorder columns
tm.ReorderColumns(boardID, [col1, col3, col2, col4]);
```

**Column Types**:
- `ctBacklog` - Ideas and future tasks
- `ctTodo` - Ready to start
- `ctInProgress` - Currently being worked on
- `ctReview` - Under review/testing
- `ctDone` - Completed tasks
- `ctCustom` - Custom workflow states

### 3. Task Cards

Manage tasks as cards on your boards:

```pascal
// Add task to board
cardID := tm.AddTaskToBoard(taskID, boardID, columnID);

// Move card between columns
tm.MoveCard(cardID, newColumnID);

// Get all cards on board
cards := tm.GetBoardCards(boardID);

// Get cards in specific column
columnCards := tm.GetColumnCards(columnID);

// Remove card from board
tm.RemoveCardFromBoard(cardID);
```

### 4. Swim Lanes

Organize cards horizontally with swim lanes:

```pascal
// Add swim lanes for categorization
urgentLane := tm.AddSwimLane(boardID, 'Urgent', '#FF0000');
normalLane := tm.AddSwimLane(boardID, 'Normal', '#00FF00');

// Move card to swim lane
tm.MoveCardToSwimLane(cardID, urgentLane);

// Get all swim lanes
lanes := tm.GetBoardSwimLanes(boardID);
```

**Use Cases**:
- Priority levels (High/Medium/Low)
- Team members
- Projects or initiatives
- Types of work (Features/Bugs/Tech Debt)

### 5. Sprint Management

Full sprint planning and tracking for Scrum teams:

```pascal
// Create sprint
sprintID := tm.CreateSprint(boardID, 'Sprint 1', 'Complete authentication',
                           EncodeDate(2024, 3, 1), EncodeDate(2024, 3, 14));

// Add tasks with story points
tm.AddTaskToSprint(sprintID, taskID1, 8);   // 8 points
tm.AddTaskToSprint(sprintID, taskID2, 5);   // 5 points

// Start sprint
tm.StartSprint(sprintID);

// Get sprint metrics
metrics := tm.GetSprintMetrics(sprintID);
WriteLn(metrics);  // Shows progress, velocity, completion rate

// Complete sprint
tm.CompleteSprint(sprintID);
```

**Sprint Data**:
- Sprint name and goal
- Start and end dates
- Planned vs completed story points
- Planned vs completed tasks
- Sprint status (Planning/Active/Completed/Cancelled)

### 6. Agile Metrics & Analytics

Track key agile metrics to improve team performance:

**Velocity**:
```pascal
// Calculate average velocity over last N sprints
velocity := tm.GetVelocity(boardID, 3);  // Last 3 sprints
WriteLn(Format('Team velocity: %.2f points/sprint', [velocity]));
```

**Cycle Time**:
```pascal
// Get average time from start to completion
metrics := tm.CalculateBoardMetrics(boardID);
WriteLn(Format('Average cycle time: %.2f hours', [metrics.AverageCycleTime]));
```

**Burndown**:
```pascal
// Get burndown chart data for sprint
burndown := tm.GetBurndownData(sprintID);
WriteLn(burndown);  // Shows ideal vs actual burn rate
```

**Work In Progress (WIP)**:
```pascal
metrics := tm.CalculateBoardMetrics(boardID);
WriteLn(Format('Average WIP: %.1f tasks', [metrics.WIPAverage]));
```

### 7. Bottleneck Detection

Automatically identify workflow problems:

```pascal
// Detect columns exceeding WIP limits
bottlenecks := tm.DetectBottlenecks(boardID);
WriteLn(bottlenecks);
// Output: "Column 'In Progress' exceeds WIP limit: 5/3"
```

### 8. Board Visualization

Render board state as text:

```pascal
// Get text representation of board
boardView := tm.RenderBoard(boardID);
WriteLn(boardView);

// Example output:
// === BOARD ===
// Backlog [3 tasks]
//   - Task #1
//   - Task #2
//   - Task #3
//
// In Progress [2 tasks] (WIP: 3)
//   - Task #4
//   - Task #5
```

### 9. Export Capabilities

```pascal
// Export board to HTML
html := tm.ExportBoardToHTML(boardID);

// Save/load board data
tm.SaveBoardDataToFile('boards.dat');
tm.LoadBoardDataFromFile('boards.dat');
```

## Data Structures

### TBoard
- `ID`: Unique board identifier
- `Name`: Board name
- `Description`: Board purpose
- `Template`: Board type (Kanban/Scrum/Custom)
- `CreatedDate`: When board was created
- `IsActive`: Whether board is active or archived

### TBoardColumn
- `ID`: Unique column identifier
- `BoardID`: Parent board
- `Name`: Column name
- `ColumnType`: Workflow stage type
- `Position`: Order in board
- `WIPLimit`: Maximum tasks allowed (0 = no limit)
- `TaskCount`: Current number of tasks
- `Color`: Visual identifier

### TTaskCard
- `ID`: Unique card identifier
- `TaskID`: Reference to task
- `BoardID`: Current board
- `ColumnID`: Current column
- `SwimLaneID`: Current swim lane (0 if none)
- `Position`: Order in column
- `EnteredColumnDate`: When moved to current column
- `ExitedColumnDate`: When left column

### TSprint
- `ID`: Unique sprint identifier
- `BoardID`: Associated board
- `Name`: Sprint name
- `Goal`: Sprint objective
- `StartDate`, `EndDate`: Sprint timeframe
- `Status`: Current sprint status
- `PlannedPoints`, `CompletedPoints`: Story points
- `TasksPlanned`, `TasksCompleted`: Task counts

### TAgileMetrics
- `BoardID`: Board being measured
- `AverageCycleTime`: Hours from start to done
- `AverageLeadTime`: Hours from backlog to done
- `Velocity`: Story points per sprint
- `Throughput`: Tasks completed per day
- `WIPAverage`: Average work in progress

## Class Hierarchy

```
TBoardTaskManager extends TSmartTaskManager
  which extends TGamifiedTaskManager
    which extends TTeamTaskManager
      which extends TEnhancedTaskManager
        which extends TAdvancedTaskManager
          which extends TExtendedTaskManager
            which extends TTaskManager
```

This creates an 8-layer architecture with complete task management capabilities.

## Best Practices

### Kanban Best Practices
1. **Set WIP Limits**: Limit work in progress to prevent multitasking
2. **Pull, Don't Push**: Only start new work when capacity is available
3. **Visualize Workflow**: Keep the board updated in real-time
4. **Monitor Cycle Time**: Track how long tasks take to complete
5. **Identify Bottlenecks**: Address columns that accumulate tasks

### Scrum Best Practices
1. **Time-box Sprints**: Keep sprints consistent (1-2 weeks typical)
2. **Set Clear Goals**: Each sprint should have a specific objective
3. **Track Velocity**: Use historical data to improve planning
4. **Daily Updates**: Move cards daily to reflect current state
5. **Sprint Reviews**: Analyze completed points vs planned

### General Board Management
1. **One Task, One Card**: Don't duplicate tasks across boards
2. **Use Swim Lanes**: Group related work for better visibility
3. **Archive Old Boards**: Keep workspace clean and focused
4. **Regular Cleanup**: Remove completed sprints and old cards
5. **Monitor Metrics**: Use data to drive continuous improvement

## Compilation

```bash
fpc solution15.pas -osolution1/bin/board_manager -O1 -Mobjfpc
```

## Example Usage

See `solution15.pas` for a comprehensive self-test demonstrating all board features.

## Benefits

✅ **Visual Workflow**: See task status at a glance  
✅ **WIP Limits**: Prevent team overload  
✅ **Sprint Planning**: Structured agile workflow  
✅ **Team Metrics**: Data-driven decisions  
✅ **Bottleneck Detection**: Identify and fix slowdowns  
✅ **Flexible Templates**: Adapt to any workflow  
✅ **Swim Lanes**: Multi-dimensional organization  
✅ **Historical Data**: Track improvement over time  

## Future Enhancements

Potential additions for future versions:
- Cumulative flow diagrams
- Lead time distribution analysis
- Monte Carlo sprint forecasting
- Automated column transitions based on rules
- Board-level dependencies
- Multi-board portfolio views
- Real-time collaboration features
- Board templates library
