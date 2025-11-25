
unit TaskTypes;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math;

const
  maxStringLength = 255;
  defaultTaskFile = 'tasks.dat';
  maxTagsPerTask = 10;

type
  // Task status enumeration
  TTaskStatus = (tsNew, tsInProgress, tsBlocked, tsCompleted, tsCancelled);

  // Task priority enumeration
  TTaskPriority = (tpLow, tpMedium, tpHigh, tpCritical);

  // Tags array
  TTagArray = array of string;

  // Task record structure
  TTask = record
    id: integer;
    name: string[maxStringLength];
    description: string[maxStringLength];
    status: TTaskStatus;
    priority: TTaskPriority;
    dueDate: TDateTime;
    createdDate: TDateTime;
    completedDate: TDateTime;
    category: string[100];
    tags: TTagArray;
    estimatedHours: double;
    actualHours: double;
  end;

  // Dynamic array of tasks
  TTaskArray = array of TTask;

  // Task statistics record
  TTaskStats = record
    totalTasks: integer;
    newTasks: integer;
    inProgressTasks: integer;
    blockedTasks: integer;
    completedTasks: integer;
    cancelledTasks: integer;
    highPriorityCount: integer;
    criticalPriorityCount: integer;
  end;

implementation

end.
