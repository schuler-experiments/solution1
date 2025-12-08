
unit task_csv_utils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, task_types;

function ExportTasksToCSV(Manager: TTaskManager; const Filename: String): Boolean;
function ImportTasksFromCSV(Manager: TTaskManager; const Filename: String): Boolean;

implementation

// Helper to escape CSV fields
function EscapeCSV(const S: String): String;
begin
  if (Pos(',', S) > 0) or (Pos('"', S) > 0) or (Pos(#13, S) > 0) or (Pos(#10, S) > 0) then
  begin
    Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"';
  end
  else
    Result := S;
end;

// Helper to parse a CSV line
function ParseCSVLine(const Line: String): TStringList;
var
  i: Integer;
  InQuotes: Boolean;
  CurrentField: String;
  c: Char;
begin
  Result := TStringList.Create;
  InQuotes := False;
  CurrentField := '';
  
  i := 1;
  while i <= Length(Line) do
  begin
    c := Line[i];
    
    if c = '"' then
    begin
      if InQuotes then
      begin
        if (i < Length(Line)) and (Line[i+1] = '"') then
        begin
          CurrentField := CurrentField + '"';
          Inc(i); // Skip next quote
        end
        else
          InQuotes := False;
      end
      else
        InQuotes := True;
    end
    else if (c = ',') and not InQuotes then
    begin
      Result.Add(CurrentField);
      CurrentField := '';
    end
    else
    begin
      CurrentField := CurrentField + c;
    end;
    
    Inc(i);
  end;
  // Add the last field
  Result.Add(CurrentField);
end;

function ExportTasksToCSV(Manager: TTaskManager; const Filename: String): Boolean;
var
  CSVList: TStringList;
  i: Integer;
  Task: TTask;
  Line: String;
begin
  Result := False;
  CSVList := TStringList.Create;
  try
    // Header
    CSVList.Add('ID,Title,Description,Status,Priority,CreatedAt,DueDate,Tags,Dependencies,TimeSpent,LastStartTime,IsTiming,RecurrenceInterval');
    
    for i := 0 to Manager.GetTaskCount - 1 do
    begin
      Task := Manager.GetTask(i);
      Line := Format('%d,%s,%s,%s,%s,%f,%f,%s,%s,%f,%f,%s,%d', [
        Task.ID,
        EscapeCSV(Task.Title),
        EscapeCSV(Task.Description),
        EscapeCSV(StatusToString(Task.Status)),
        EscapeCSV(PriorityToString(Task.Priority)),
        Task.CreatedAt,
        Task.DueDate,
        EscapeCSV(TagsToString(Task.Tags)),
        EscapeCSV(DepsToString(Task.Dependencies)),
        Task.TimeSpent,
        Task.LastStartTime,
        BoolToStr(Task.IsTiming, 'TRUE', 'FALSE'),
        Task.RecurrenceInterval
      ]);
      CSVList.Add(Line);
    end;
    
    CSVList.SaveToFile(Filename);
    Result := True;
  except
    on E: Exception do
      WriteLn('Error exporting to CSV: ' + E.Message);
  end;
  CSVList.Free;
end;

function ImportTasksFromCSV(Manager: TTaskManager; const Filename: String): Boolean;
var
  CSVList: TStringList;
  Fields: TStringList;
  i: Integer;
  Task: TTask;
  Line: String;
begin
  Result := False;
  if not FileExists(Filename) then Exit;
  
  CSVList := TStringList.Create;
  try
    CSVList.LoadFromFile(Filename);
    
    // Skip header (start from 1)
    for i := 1 to CSVList.Count - 1 do
    begin
      Line := CSVList[i];
      if Trim(Line) = '' then Continue;
      
      Fields := ParseCSVLine(Line);
      try
        if Fields.Count >= 13 then
        begin
          Task.ID := StrToIntDef(Fields[0], 0);
          Task.Title := Fields[1];
          Task.Description := Fields[2];
          Task.Status := StringToStatus(Fields[3]);
          Task.Priority := StringToPriority(Fields[4]);
          Task.CreatedAt := StrToFloatDef(Fields[5], 0);
          Task.DueDate := StrToFloatDef(Fields[6], 0);
          Task.Tags := StringToTags(Fields[7]);
          Task.Dependencies := StringToDeps(Fields[8]);
          Task.TimeSpent := StrToFloatDef(Fields[9], 0);
          Task.LastStartTime := StrToFloatDef(Fields[10], 0);
          Task.IsTiming := (Fields[11] = 'TRUE');
          Task.RecurrenceInterval := StrToIntDef(Fields[12], 0);
          
          Manager.RestoreTask(Task);
        end;
      finally
        Fields.Free;
      end;
    end;
    
    Result := True;
  except
    on E: Exception do
      WriteLn('Error importing from CSV: ' + E.Message);
  end;
  CSVList.Free;
end;

end.
