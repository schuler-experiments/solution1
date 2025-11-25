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
  maxNotesPerTask = 20;
  maxMilestonesPerManager = 50;

type
  // Task status enumeration
  TTaskStatus = (tsNew, tsInProgress, tsBlocked, tsCompleted, tsCancelled);

  // Task priority enumeration
  TTaskPriority = (tpLow, tpMedium, tpHigh, tpCritical);

  // Task note record
  TTaskNote = record
    text: string[maxStringLength];
    createdDate: TDateTime;
    isImportant: boolean;
  end;

  // Notes array
  TNoteArray = array of TTaskNote;

  // Tags array
  TTagArray = array of string;

  // Integer array (for dependency IDs)
  TIntArray = array of integer;

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
    dependencyIds: array of integer;
    notes: TNoteArray;
    assignedTo: string[100];
    milestoneId: integer;
  end;

  // Milestone record structure
  TMilestone = record
    id: integer;
    name: string[maxStringLength];
    description: string[maxStringLength];
    targetDate: TDateTime;
    status: TTaskStatus;
    createdDate: TDateTime;
  end;

  // Task template record
  TTaskTemplate = record
    id: integer;
    name: string[maxStringLength];
    description: string[maxStringLength];
    category: string[100];
    priority: TTaskPriority;
    estimatedHours: double;
    tags: TTagArray;
  end;

  // Dynamic array of tasks
  TTaskArray = array of TTask;

  // Dynamic array of templates
  TTemplateArray = array of TTaskTemplate;

  // Dynamic array of milestones
  TMilestoneArray = array of TMilestone;

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
    tasksWithNotes: integer;
    averageCompletionTime: double;
  end;

implementation

end.
