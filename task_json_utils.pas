
unit task_json_utils;

{$mode objfpc}{$H+}

interface

uses
  sysutils, classes, fpjson, jsonparser, task_types;

function ExportTasksToJSON(Manager: TTaskManager; const Filename: String): Boolean;
function ImportTasksFromJSON(Manager: TTaskManager; const Filename: String): Boolean;

implementation

function TaskToJSON(const Task: TTask): TJSONObject;
var
  JSON: TJSONObject;
  TagsArray, DepsArray: TJSONArray;
  i: Integer;
begin
  JSON := TJSONObject.Create;
  JSON.Add('id', Task.ID);
  JSON.Add('title', Task.Title);
  JSON.Add('description', Task.Description);
  JSON.Add('status', StatusToString(Task.Status));
  JSON.Add('priority', PriorityToString(Task.Priority));
  JSON.Add('created_at', Task.CreatedAt);
  JSON.Add('due_date', Task.DueDate);
  
  TagsArray := TJSONArray.Create;
  for i := 0 to High(Task.Tags) do
    TagsArray.Add(Task.Tags[i]);
  JSON.Add('tags', TagsArray);
  
  DepsArray := TJSONArray.Create;
  for i := 0 to High(Task.Dependencies) do
    DepsArray.Add(Task.Dependencies[i]);
  JSON.Add('dependencies', DepsArray);
  
  JSON.Add('time_spent', Task.TimeSpent);
  JSON.Add('last_start_time', Task.LastStartTime);
  JSON.Add('is_timing', Task.IsTiming);
  
  Result := JSON;
end;

function JSONToTask(JSON: TJSONObject): TTask;
var
  TagsArray, DepsArray: TJSONArray;
  i: Integer;
begin
  Result.ID := JSON.Get('id', 0);
  Result.Title := JSON.Get('title', '');
  Result.Description := JSON.Get('description', '');
  Result.Status := StringToStatus(JSON.Get('status', 'tsPending'));
  Result.Priority := StringToPriority(JSON.Get('priority', 'tpMedium'));
  Result.CreatedAt := JSON.Get('created_at', 0.0);
  Result.DueDate := JSON.Get('due_date', 0.0);
  
  SetLength(Result.Tags, 0);
  if JSON.IndexOfName('tags') <> -1 then
  begin
    TagsArray := JSON.Arrays['tags'];
    SetLength(Result.Tags, TagsArray.Count);
    for i := 0 to TagsArray.Count - 1 do
      Result.Tags[i] := TagsArray.Strings[i];
  end;
  
  SetLength(Result.Dependencies, 0);
  if JSON.IndexOfName('dependencies') <> -1 then
  begin
    DepsArray := JSON.Arrays['dependencies'];
    SetLength(Result.Dependencies, DepsArray.Count);
    for i := 0 to DepsArray.Count - 1 do
      Result.Dependencies[i] := DepsArray.Integers[i];
  end;
  
  Result.TimeSpent := JSON.Get('time_spent', 0.0);
  Result.LastStartTime := JSON.Get('last_start_time', 0.0);
  Result.IsTiming := JSON.Get('is_timing', False);
end;

function ExportTasksToJSON(Manager: TTaskManager; const Filename: String): Boolean;
var
  JSONArray: TJSONArray;
  i: Integer;
  Task: TTask;
  JSONString: String;
  Stream: TStringStream;
begin
  JSONArray := TJSONArray.Create;
  try
    for i := 0 to Manager.GetTaskCount - 1 do
    begin
      Task := Manager.GetTask(i);
      JSONArray.Add(TaskToJSON(Task));
    end;
    
    JSONString := JSONArray.AsJSON;
    Stream := TStringStream.Create(JSONString);
    try
      Stream.SaveToFile(Filename);
      Result := True;
    except
      Result := False;
    end;
    Stream.Free;
  finally
    JSONArray.Free;
  end;
end;

function ImportTasksFromJSON(Manager: TTaskManager; const Filename: String): Boolean;
var
  Parser: TJSONParser;
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  Stream: TFileStream;
  i: Integer;
  Task: TTask;
begin
  if not FileExists(Filename) then Exit(False);
  
  Manager.ClearTasks;
  Stream := TFileStream.Create(Filename, fmOpenRead);
  try
    Parser := TJSONParser.Create(Stream);
    try
      JSONData := Parser.Parse;
      if JSONData is TJSONArray then
      begin
        JSONArray := TJSONArray(JSONData);
        for i := 0 to JSONArray.Count - 1 do
        begin
          if JSONArray.Items[i] is TJSONObject then
          begin
            Task := JSONToTask(TJSONObject(JSONArray.Items[i]));
            Manager.RestoreTask(Task);
          end;
        end;
        Result := True;
      end
      else
        Result := False;
    finally
      JSONData.Free;
      Parser.Free;
    end;
  finally
    Stream.Free;
  end;
end;

end.
