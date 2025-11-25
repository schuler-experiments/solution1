
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
  maxWatchersPerTask = 20;
  maxReferencesPerTask = 10;
  maxSubtasksPerTask = 50;
  maxHistoryEntriesPerTask = 100;

type
  // Task status enumeration
  TTaskStatus = (tsNew, tsInProgress, tsBlocked, tsCompleted, tsCancelled);

  // Task priority enumeration
  TTaskPriority = (tpLow, tpMedium, tpHigh, tpCritical);

  // Task recurrence pattern
  TRecurrencePattern = (rpNone, rpDaily, rpWeekly, rpBiWeekly, rpMonthly, rpQuarterly, rpYearly);

  // Task note record
  TTaskNote = record
    text: string[maxStringLength];
    createdDate: TDateTime;
    isImportant: boolean;
  end;

  // Task reference record (for linking external resources)
  TTaskReference = record
    referenceId: integer;
    relationshipType: string[50];
  end;

  // Subtask record
  TSubtask = record
    id: integer;
    taskId: integer;
    title: string[maxStringLength];
    status: TTaskStatus;
    createdDate: TDateTime;
    completedDate: TDateTime;
    assignedTo: string[100];
    estimatedHours: double;
    actualHours: double;
  end;

  // Task history entry for audit logging
  TTaskHistoryEntry = record
    timestamp: TDateTime;
    fieldName: string[50];
    oldValue: string[maxStringLength];
    newValue: string[maxStringLength];
    changedBy: string[100];
  end;

  // Notes array
  TNoteArray = array of TTaskNote;

  // Tags array
  TTagArray = array of string;

  // Integer array (for dependency IDs)
  TIntArray = array of integer;

  // Watcher array
  TWatcherArray = array of string;

  // References array
  TReferenceArray = array of TTaskReference;

  // Subtasks array
  TSubtaskArray = array of TSubtask;

  // History array
  THistoryArray = array of TTaskHistoryEntry;

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
    recurrencePattern: TRecurrencePattern;
    recurrenceEndDate: TDateTime;
    watchers: TWatcherArray;
    references: TReferenceArray;
    subtasks: TSubtaskArray;
    history: THistoryArray;
    storyPoints: integer;
    isBlocked: boolean;
    blockReason: string[maxStringLength];
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
    storyPoints: integer;
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
    totalStoryPoints: integer;
    completedStoryPoints: integer;
  end;

implementation

end.
