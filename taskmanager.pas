
unit taskmanager;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, DateUtils, Math, Classes;

type
  // Task status enumeration
  TTaskStatus = (tsNotStarted, tsInProgress, tsCompleted, tsCancelled, tsOnHold);
  
  // Task priority enumeration
  TTaskPriority = (tpLow, tpMedium, tpHigh, tpCritical);
  
  // Sort criteria enumeration
  TSortCriteria = (scTitle, scPriority, scDueDate, scCreatedDate, scStatus, scCategory);
  
  // Task record structure
  TTask = record
    ID: Integer;
    Title: string;
    Description: string;
    Status: TTaskStatus;
    Priority: TTaskPriority;
    Category: string;
    CreatedDate: TDateTime;
    DueDate: TDateTime;
    CompletedDate: TDateTime;
    EstimatedHours: Double;
    ActualHours: Double;
    Tags: array of string;
  end;
  
  // Dynamic array of tasks
  TTaskArray = array of TTask;
  
  // Task Manager class
  TTaskManager = class
  private
    FTasks: TTaskArray;
    FNextID: Integer;
    function GetTaskCount: Integer;
    procedure QuickSortTasks(var ATasks: TTaskArray; ALeft, ARight: Integer; ACriteria: TSortCriteria);
    function CompareTasks(const A, B: TTask; ACriteria: TSortCriteria): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Core operations
    function AddTask(const ATitle, ADescription: string; 
                     APriority: TTaskPriority; ADueDate: TDateTime): Integer; overload;
    function AddTask(const ATitle, ADescription, ACategory: string;
                     APriority: TTaskPriority; ADueDate: TDateTime;
                     AEstimatedHours: Double): Integer; overload;
    function DeleteTask(ATaskID: Integer): Boolean;
    function UpdateTaskTitle(ATaskID: Integer; const ANewTitle: string): Boolean;
    function UpdateTaskDescription(ATaskID: Integer; const ANewDesc: string): Boolean;
    function UpdateTaskStatus(ATaskID: Integer; ANewStatus: TTaskStatus): Boolean;
    function UpdateTaskPriority(ATaskID: Integer; ANewPriority: TTaskPriority): Boolean;
    function UpdateTaskDueDate(ATaskID: Integer; ANewDueDate: TDateTime): Boolean;
    function UpdateTaskCategory(ATaskID: Integer; const ANewCategory: string): Boolean;
    function UpdateTaskEstimatedHours(ATaskID: Integer; AHours: Double): Boolean;
    function UpdateTaskActualHours(ATaskID: Integer; AHours: Double): Boolean;
    
    // Search and filter
    function GetTaskByID(ATaskID: Integer): Integer;
    function GetAllTasks: TTaskArray;
    function FilterByStatus(AStatus: TTaskStatus): TTaskArray;
    function FilterByPriority(APriority: TTaskPriority): TTaskArray;
    function FilterByDateRange(AStartDate, AEndDate: TDateTime): TTaskArray;
    function SearchByTitle(const ASearchTerm: string): TTaskArray;
    function FilterByCategory(const ACategory: string): TTaskArray;
    
    // Tag management
    function AddTagToTask(ATaskID: Integer; const ATag: string): Boolean;
    function RemoveTagFromTask(ATaskID: Integer; const ATag: string): Boolean;
    function FilterByTag(const ATag: string): TTaskArray;
    
    // Sorting
    function SortTasks(ACriteria: TSortCriteria): TTaskArray;
    function SortTasksDescending(ACriteria: TSortCriteria): TTaskArray;
    
    // Statistics
    function GetCompletedCount: Integer;
    function GetPendingCount: Integer;
    function GetOverdueCount: Integer;
    function GetCompletionRate: Double;
    function GetAverageCompletionTime: Double;
    function GetTasksByCategory: string;
    function GetTotalEstimatedHours: Double;
    function GetTotalActualHours: Double;
    
    // Import/Export
    function ExportToCSV: string;
    function SaveToFile(const AFilename: string): Boolean;
    function LoadFromFile(const AFilename: string): Boolean;
    
    // Utility functions
    procedure ClearAllTasks;
    function TaskStatusToString(AStatus: TTaskStatus): string;
    function TaskPriorityToString(APriority: TTaskPriority): string;
    function TaskToString(const ATask: TTask): string;
    
    property TaskCount: Integer read GetTaskCount;
  end;

implementation

{ TTaskManager }

constructor TTaskManager.Create;
begin
  inherited Create;
  SetLength(FTasks, 0);
  FNextID := 1;
end;

destructor TTaskManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FTasks) - 1 do
    SetLength(FTasks[i].Tags, 0);
  SetLength(FTasks, 0);
  inherited Destroy;
end;

function TTaskManager.GetTaskCount: Integer;
begin
  Result := Length(FTasks);
end;

function TTaskManager.AddTask(const ATitle, ADescription: string;
  APriority: TTaskPriority; ADueDate: TDateTime): Integer;
begin
  Result := AddTask(ATitle, ADescription, '', APriority, ADueDate, 0.0);
end;

function TTaskManager.AddTask(const ATitle, ADescription, ACategory: string;
  APriority: TTaskPriority; ADueDate: TDateTime; AEstimatedHours: Double): Integer;
var
  NewTask: TTask;
  TaskIndex: Integer;
begin
  NewTask.ID := FNextID;
  NewTask.Title := ATitle;
  NewTask.Description := ADescription;
  NewTask.Category := ACategory;
  NewTask.Status := tsNotStarted;
  NewTask.Priority := APriority;
  NewTask.CreatedDate := Now;
  NewTask.DueDate := ADueDate;
  NewTask.CompletedDate := 0;
  NewTask.EstimatedHours := AEstimatedHours;
  NewTask.ActualHours := 0;
  SetLength(NewTask.Tags, 0);
  
  TaskIndex := Length(FTasks);
  SetLength(FTasks, TaskIndex + 1);
  FTasks[TaskIndex] := NewTask;
  
  Inc(FNextID);
  Result := NewTask.ID;
end;

function TTaskManager.DeleteTask(ATaskID: Integer): Boolean;
var
  Index, i: Integer;
begin
  Index := GetTaskByID(ATaskID);
  Result := Index >= 0;
  
  if Result then
  begin
    SetLength(FTasks[Index].Tags, 0);
    for i := Index to Length(FTasks) - 2 do
      FTasks[i] := FTasks[i + 1];
    SetLength(FTasks, Length(FTasks) - 1);
  end;
end;

function TTaskManager.UpdateTaskTitle(ATaskID: Integer; const ANewTitle: string): Boolean;
var
  Index: Integer;
begin
  Index := GetTaskByID(ATaskID);
  Result := Index >= 0;
  if Result then
    FTasks[Index].Title := ANewTitle;
end;

function TTaskManager.UpdateTaskDescription(ATaskID: Integer; const ANewDesc: string): Boolean;
var
  Index: Integer;
begin
  Index := GetTaskByID(ATaskID);
  Result := Index >= 0;
  if Result then
    FTasks[Index].Description := ANewDesc;
end;

function TTaskManager.UpdateTaskStatus(ATaskID: Integer; ANewStatus: TTaskStatus): Boolean;
var
  Index: Integer;
begin
  Index := GetTaskByID(ATaskID);
  Result := Index >= 0;
  if Result then
  begin
    FTasks[Index].Status := ANewStatus;
    if ANewStatus = tsCompleted then
      FTasks[Index].CompletedDate := Now;
  end;
end;

function TTaskManager.UpdateTaskPriority(ATaskID: Integer; ANewPriority: TTaskPriority): Boolean;
var
  Index: Integer;
begin
  Index := GetTaskByID(ATaskID);
  Result := Index >= 0;
  if Result then
    FTasks[Index].Priority := ANewPriority;
end;

function TTaskManager.UpdateTaskDueDate(ATaskID: Integer; ANewDueDate: TDateTime): Boolean;
var
  Index: Integer;
begin
  Index := GetTaskByID(ATaskID);
  Result := Index >= 0;
  if Result then
    FTasks[Index].DueDate := ANewDueDate;
end;

function TTaskManager.UpdateTaskCategory(ATaskID: Integer; const ANewCategory: string): Boolean;
var
  Index: Integer;
begin
  Index := GetTaskByID(ATaskID);
  Result := Index >= 0;
  if Result then
    FTasks[Index].Category := ANewCategory;
end;

function TTaskManager.UpdateTaskEstimatedHours(ATaskID: Integer; AHours: Double): Boolean;
var
  Index: Integer;
begin
  Index := GetTaskByID(ATaskID);
  Result := Index >= 0;
  if Result then
    FTasks[Index].EstimatedHours := AHours;
end;

function TTaskManager.UpdateTaskActualHours(ATaskID: Integer; AHours: Double): Boolean;
var
  Index: Integer;
begin
  Index := GetTaskByID(ATaskID);
  Result := Index >= 0;
  if Result then
    FTasks[Index].ActualHours := AHours;
end;

function TTaskManager.GetTaskByID(ATaskID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FTasks) - 1 do
    if FTasks[i].ID = ATaskID then
    begin
      Result := i;
      Exit;
    end;
end;

function TTaskManager.GetAllTasks: TTaskArray;
begin
  Result := Copy(FTasks, 0, Length(FTasks));
end;

function TTaskManager.FilterByStatus(AStatus: TTaskStatus): TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to Length(FTasks) - 1 do
    if FTasks[i].Status = AStatus then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
end;

function TTaskManager.FilterByPriority(APriority: TTaskPriority): TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to Length(FTasks) - 1 do
    if FTasks[i].Priority = APriority then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
end;

function TTaskManager.FilterByDateRange(AStartDate, AEndDate: TDateTime): TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to Length(FTasks) - 1 do
    if (FTasks[i].DueDate >= AStartDate) and (FTasks[i].DueDate <= AEndDate) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
end;

function TTaskManager.SearchByTitle(const ASearchTerm: string): TTaskArray;
var
  i, Count: Integer;
  LowerSearch, LowerTitle: string;
begin
  SetLength(Result, 0);
  Count := 0;
  LowerSearch := LowerCase(ASearchTerm);
  for i := 0 to Length(FTasks) - 1 do
  begin
    LowerTitle := LowerCase(FTasks[i].Title);
    if Pos(LowerSearch, LowerTitle) > 0 then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByCategory(const ACategory: string): TTaskArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to Length(FTasks) - 1 do
    if LowerCase(FTasks[i].Category) = LowerCase(ACategory) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
end;

function TTaskManager.AddTagToTask(ATaskID: Integer; const ATag: string): Boolean;
var
  Index, TagCount: Integer;
begin
  Index := GetTaskByID(ATaskID);
  Result := Index >= 0;
  if Result then
  begin
    TagCount := Length(FTasks[Index].Tags);
    SetLength(FTasks[Index].Tags, TagCount + 1);
    FTasks[Index].Tags[TagCount] := ATag;
  end;
end;

function TTaskManager.RemoveTagFromTask(ATaskID: Integer; const ATag: string): Boolean;
var
  TaskIndex, TagIndex, i: Integer;
  Found: Boolean;
begin
  TaskIndex := GetTaskByID(ATaskID);
  Result := TaskIndex >= 0;
  if not Result then
    Exit;
  
  Found := False;
  for i := 0 to Length(FTasks[TaskIndex].Tags) - 1 do
    if FTasks[TaskIndex].Tags[i] = ATag then
    begin
      TagIndex := i;
      Found := True;
      Break;
    end;
  
  if Found then
  begin
    for i := TagIndex to Length(FTasks[TaskIndex].Tags) - 2 do
      FTasks[TaskIndex].Tags[i] := FTasks[TaskIndex].Tags[i + 1];
    SetLength(FTasks[TaskIndex].Tags, Length(FTasks[TaskIndex].Tags) - 1);
  end;
  
  Result := Found;
end;

function TTaskManager.FilterByTag(const ATag: string): TTaskArray;
var
  i, j, Count: Integer;
  HasTag: Boolean;
begin
  SetLength(Result, 0);
  Count := 0;
  for i := 0 to Length(FTasks) - 1 do
  begin
    HasTag := False;
    for j := 0 to Length(FTasks[i].Tags) - 1 do
      if LowerCase(FTasks[i].Tags[j]) = LowerCase(ATag) then
      begin
        HasTag := True;
        Break;
      end;
    
    if HasTag then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTasks[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.CompareTasks(const A, B: TTask; ACriteria: TSortCriteria): Integer;
begin
  case ACriteria of
    scTitle: Result := CompareText(A.Title, B.Title);
    scPriority: Result := Ord(B.Priority) - Ord(A.Priority);
    scDueDate: Result := CompareDateTime(A.DueDate, B.DueDate);
    scCreatedDate: Result := CompareDateTime(A.CreatedDate, B.CreatedDate);
    scStatus: Result := Ord(A.Status) - Ord(B.Status);
    scCategory: Result := CompareText(A.Category, B.Category);
  else
    Result := 0;
  end;
end;

procedure TTaskManager.QuickSortTasks(var ATasks: TTaskArray; ALeft, ARight: Integer; ACriteria: TSortCriteria);
var
  i, j: Integer;
  Pivot, Temp: TTask;
begin
  if ALeft >= ARight then
    Exit;
  
  i := ALeft;
  j := ARight;
  Pivot := ATasks[(ALeft + ARight) div 2];
  
  repeat
    while CompareTasks(ATasks[i], Pivot, ACriteria) < 0 do
      Inc(i);
    while CompareTasks(ATasks[j], Pivot, ACriteria) > 0 do
      Dec(j);
    
    if i <= j then
    begin
      Temp := ATasks[i];
      ATasks[i] := ATasks[j];
      ATasks[j] := Temp;
      Inc(i);
      Dec(j);
    end;
  until i > j;
  
  if ALeft < j then
    QuickSortTasks(ATasks, ALeft, j, ACriteria);
  if i < ARight then
    QuickSortTasks(ATasks, i, ARight, ACriteria);
end;

function TTaskManager.SortTasks(ACriteria: TSortCriteria): TTaskArray;
begin
  Result := GetAllTasks;
  if Length(Result) > 1 then
    QuickSortTasks(Result, 0, Length(Result) - 1, ACriteria);
end;

function TTaskManager.SortTasksDescending(ACriteria: TSortCriteria): TTaskArray;
var
  i, Len: Integer;
  Temp: TTask;
begin
  Result := SortTasks(ACriteria);
  Len := Length(Result);
  for i := 0 to (Len div 2) - 1 do
  begin
    Temp := Result[i];
    Result[i] := Result[Len - 1 - i];
    Result[Len - 1 - i] := Temp;
  end;
end;

function TTaskManager.GetCompletedCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FTasks) - 1 do
    if FTasks[i].Status = tsCompleted then
      Inc(Result);
end;

function TTaskManager.GetPendingCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FTasks) - 1 do
    if FTasks[i].Status in [tsNotStarted, tsInProgress, tsOnHold] then
      Inc(Result);
end;

function TTaskManager.GetOverdueCount: Integer;
var
  i: Integer;
  CurrentDate: TDateTime;
begin
  Result := 0;
  CurrentDate := Now;
  for i := 0 to Length(FTasks) - 1 do
    if (FTasks[i].Status <> tsCompleted) and (FTasks[i].DueDate > 0) and 
       (FTasks[i].DueDate < CurrentDate) then
      Inc(Result);
end;

function TTaskManager.GetCompletionRate: Double;
var
  Total: Integer;
begin
  Total := Length(FTasks);
  if Total = 0 then
    Result := 0.0
  else
    Result := (GetCompletedCount / Total) * 100.0;
end;

function TTaskManager.GetAverageCompletionTime: Double;
var
  i, Count: Integer;
  TotalTime: Double;
begin
  TotalTime := 0;
  Count := 0;
  for i := 0 to Length(FTasks) - 1 do
    if (FTasks[i].Status = tsCompleted) and (FTasks[i].CompletedDate > FTasks[i].CreatedDate) then
    begin
      TotalTime := TotalTime + (FTasks[i].CompletedDate - FTasks[i].CreatedDate);
      Inc(Count);
    end;
  
  if Count = 0 then
    Result := 0.0
  else
    Result := TotalTime / Count;
end;

function TTaskManager.GetTasksByCategory: string;
var
  i, j: Integer;
  Categories: array of string;
  Counts: array of Integer;
  Found: Boolean;
  CatIndex: Integer;
begin
  SetLength(Categories, 0);
  SetLength(Counts, 0);
  
  for i := 0 to Length(FTasks) - 1 do
  begin
    if FTasks[i].Category = '' then
      Continue;
    
    Found := False;
    CatIndex := -1;
    for j := 0 to Length(Categories) - 1 do
      if Categories[j] = FTasks[i].Category then
      begin
        Found := True;
        CatIndex := j;
        Break;
      end;
    
    if Found then
      Inc(Counts[CatIndex])
    else
    begin
      SetLength(Categories, Length(Categories) + 1);
      SetLength(Counts, Length(Counts) + 1);
      Categories[Length(Categories) - 1] := FTasks[i].Category;
      Counts[Length(Counts) - 1] := 1;
    end;
  end;
  
  Result := '';
  for i := 0 to Length(Categories) - 1 do
  begin
    if i > 0 then
      Result := Result + '; ';
    Result := Result + Format('%s: %d', [Categories[i], Counts[i]]);
  end;
  
  SetLength(Categories, 0);
  SetLength(Counts, 0);
end;

function TTaskManager.GetTotalEstimatedHours: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FTasks) - 1 do
    Result := Result + FTasks[i].EstimatedHours;
end;

function TTaskManager.GetTotalActualHours: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FTasks) - 1 do
    Result := Result + FTasks[i].ActualHours;
end;

function TTaskManager.ExportToCSV: string;
var
  i, j: Integer;
  TagsStr: string;
begin
  Result := 'ID,Title,Description,Category,Status,Priority,CreatedDate,DueDate,CompletedDate,EstimatedHours,ActualHours,Tags' + LineEnding;
  
  for i := 0 to Length(FTasks) - 1 do
  begin
    TagsStr := '';
    for j := 0 to Length(FTasks[i].Tags) - 1 do
    begin
      if j > 0 then
        TagsStr := TagsStr + '|';
      TagsStr := TagsStr + FTasks[i].Tags[j];
    end;
    
    Result := Result + Format('%d,"%s","%s","%s",%s,%s,%s,%s,%s,%.2f,%.2f,"%s"' + LineEnding,
      [FTasks[i].ID,
       StringReplace(FTasks[i].Title, '"', '""', [rfReplaceAll]),
       StringReplace(FTasks[i].Description, '"', '""', [rfReplaceAll]),
       StringReplace(FTasks[i].Category, '"', '""', [rfReplaceAll]),
       TaskStatusToString(FTasks[i].Status),
       TaskPriorityToString(FTasks[i].Priority),
       DateTimeToStr(FTasks[i].CreatedDate),
       DateTimeToStr(FTasks[i].DueDate),
       DateTimeToStr(FTasks[i].CompletedDate),
       FTasks[i].EstimatedHours,
       FTasks[i].ActualHours,
       TagsStr]);
  end;
end;

function TTaskManager.SaveToFile(const AFilename: string): Boolean;
var
  F: TextFile;
  i, j: Integer;
begin
  Result := False;
  try
    AssignFile(F, AFilename);
    Rewrite(F);
    
    WriteLn(F, '[TASKMANAGER_DATA_V1]');
    WriteLn(F, 'NextID=', FNextID);
    WriteLn(F, 'TaskCount=', Length(FTasks));
    
    for i := 0 to Length(FTasks) - 1 do
    begin
      WriteLn(F, '[TASK]');
      WriteLn(F, 'ID=', FTasks[i].ID);
      WriteLn(F, 'Title=', FTasks[i].Title);
      WriteLn(F, 'Description=', FTasks[i].Description);
      WriteLn(F, 'Category=', FTasks[i].Category);
      WriteLn(F, 'Status=', Ord(FTasks[i].Status));
      WriteLn(F, 'Priority=', Ord(FTasks[i].Priority));
      WriteLn(F, 'CreatedDate=', DateTimeToStr(FTasks[i].CreatedDate));
      WriteLn(F, 'DueDate=', DateTimeToStr(FTasks[i].DueDate));
      WriteLn(F, 'CompletedDate=', DateTimeToStr(FTasks[i].CompletedDate));
      WriteLn(F, 'EstimatedHours=', FTasks[i].EstimatedHours:0:2);
      WriteLn(F, 'ActualHours=', FTasks[i].ActualHours:0:2);
      WriteLn(F, 'TagCount=', Length(FTasks[i].Tags));
      for j := 0 to Length(FTasks[i].Tags) - 1 do
        WriteLn(F, 'Tag=', FTasks[i].Tags[j]);
    end;
    
    CloseFile(F);
    Result := True;
  except
    Result := False;
  end;
end;

function TTaskManager.LoadFromFile(const AFilename: string): Boolean;
var
  F: TextFile;
  Line, Key, Value: string;
  SepPos: Integer;
  NewTask: TTask;
  TagCount, CurrentTag: Integer;
  InTask: Boolean;
begin
  Result := False;
  if not FileExists(AFilename) then
    Exit;
  
  try
    ClearAllTasks;
    AssignFile(F, AFilename);
    Reset(F);
    
    ReadLn(F, Line);
    if Line <> '[TASKMANAGER_DATA_V1]' then
    begin
      CloseFile(F);
      Exit;
    end;
    
    ReadLn(F, Line);
    SepPos := Pos('=', Line);
    if SepPos > 0 then
      FNextID := StrToIntDef(Copy(Line, SepPos + 1, Length(Line)), 1);
    
    ReadLn(F, Line);
    
    InTask := False;
    TagCount := 0;
    CurrentTag := 0;
    
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      
      if Line = '[TASK]' then
      begin
        if InTask then
        begin
          SetLength(FTasks, Length(FTasks) + 1);
          FTasks[Length(FTasks) - 1] := NewTask;
        end;
        
        SetLength(NewTask.Tags, 0);
        NewTask.ID := 0;
        NewTask.Title := '';
        NewTask.Description := '';
        NewTask.Category := '';
        NewTask.Status := tsNotStarted;
        NewTask.Priority := tpMedium;
        NewTask.CreatedDate := 0;
        NewTask.DueDate := 0;
        NewTask.CompletedDate := 0;
        NewTask.EstimatedHours := 0;
        NewTask.ActualHours := 0;
        TagCount := 0;
        CurrentTag := 0;
        InTask := True;
        Continue;
      end;
      
      if InTask then
      begin
        SepPos := Pos('=', Line);
        if SepPos > 0 then
        begin
          Key := Copy(Line, 1, SepPos - 1);
          Value := Copy(Line, SepPos + 1, Length(Line));
          
          if Key = 'ID' then NewTask.ID := StrToIntDef(Value, 0)
          else if Key = 'Title' then NewTask.Title := Value
          else if Key = 'Description' then NewTask.Description := Value
          else if Key = 'Category' then NewTask.Category := Value
          else if Key = 'Status' then NewTask.Status := TTaskStatus(StrToIntDef(Value, 0))
          else if Key = 'Priority' then NewTask.Priority := TTaskPriority(StrToIntDef(Value, 0))
          else if Key = 'CreatedDate' then NewTask.CreatedDate := StrToDateTimeDef(Value, 0)
          else if Key = 'DueDate' then NewTask.DueDate := StrToDateTimeDef(Value, 0)
          else if Key = 'CompletedDate' then NewTask.CompletedDate := StrToDateTimeDef(Value, 0)
          else if Key = 'EstimatedHours' then NewTask.EstimatedHours := StrToFloatDef(Value, 0)
          else if Key = 'ActualHours' then NewTask.ActualHours := StrToFloatDef(Value, 0)
          else if Key = 'TagCount' then
          begin
            TagCount := StrToIntDef(Value, 0);
            SetLength(NewTask.Tags, TagCount);
            CurrentTag := 0;
          end
          else if Key = 'Tag' then
          begin
            if CurrentTag < TagCount then
            begin
              NewTask.Tags[CurrentTag] := Value;
              Inc(CurrentTag);
            end;
          end;
        end;
      end;
    end;
    
    if InTask then
    begin
      SetLength(FTasks, Length(FTasks) + 1);
      FTasks[Length(FTasks) - 1] := NewTask;
    end;
    
    CloseFile(F);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TTaskManager.ClearAllTasks;
var
  i: Integer;
begin
  for i := 0 to Length(FTasks) - 1 do
    SetLength(FTasks[i].Tags, 0);
  SetLength(FTasks, 0);
  FNextID := 1;
end;

function TTaskManager.TaskStatusToString(AStatus: TTaskStatus): string;
begin
  case AStatus of
    tsNotStarted: Result := 'Not Started';
    tsInProgress: Result := 'In Progress';
    tsCompleted: Result := 'Completed';
    tsCancelled: Result := 'Cancelled';
    tsOnHold: Result := 'On Hold';
  else
    Result := 'Unknown';
  end;
end;

function TTaskManager.TaskPriorityToString(APriority: TTaskPriority): string;
begin
  case APriority of
    tpLow: Result := 'Low';
    tpMedium: Result := 'Medium';
    tpHigh: Result := 'High';
    tpCritical: Result := 'Critical';
  else
    Result := 'Unknown';
  end;
end;

function TTaskManager.TaskToString(const ATask: TTask): string;
var
  i: Integer;
  TagsStr: string;
begin
  Result := Format('ID: %d | Title: %s | Status: %s | Priority: %s', 
    [ATask.ID, ATask.Title, TaskStatusToString(ATask.Status), 
     TaskPriorityToString(ATask.Priority)]);
  
  if ATask.Category <> '' then
    Result := Result + Format(' | Category: %s', [ATask.Category]);
  
  if ATask.Description <> '' then
    Result := Result + Format(' | Description: %s', [ATask.Description]);
  
  if ATask.DueDate > 0 then
    Result := Result + Format(' | Due: %s', [DateTimeToStr(ATask.DueDate)]);
  
  if ATask.EstimatedHours > 0 then
    Result := Result + Format(' | Est: %.1fh', [ATask.EstimatedHours]);
  
  if ATask.ActualHours > 0 then
    Result := Result + Format(' | Actual: %.1fh', [ATask.ActualHours]);
  
  if Length(ATask.Tags) > 0 then
  begin
    TagsStr := '';
    for i := 0 to Length(ATask.Tags) - 1 do
    begin
      if i > 0 then
        TagsStr := TagsStr + ', ';
      TagsStr := TagsStr + ATask.Tags[i];
    end;
    Result := Result + Format(' | Tags: [%s]', [TagsStr]);
  end;
end;

end.
