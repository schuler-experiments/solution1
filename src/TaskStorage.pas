
unit TaskStorage;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TaskTypes;

type
  // Task storage handler for file-based persistence
  TTaskStorage = class
  private
    FFileName: string;
  public
    constructor Create(const aFileName: string);
    
    // Save and load operations
    function SaveTasks(const aTasks: TTaskArray; aNextID: integer): boolean;
    function LoadTasks(out aTasks: TTaskArray; out aNextID: integer): boolean;
    
    // Export/Import in different formats
    function ExportToCSV(const aTasks: TTaskArray; const aFileName: string): boolean;
    function ExportToText(const aTasks: TTaskArray; const aFileName: string): boolean;
    
    property FileName: string read FFileName write FFileName;
  end;

implementation

{ TTaskStorage }

constructor TTaskStorage.Create(const aFileName: string);
begin
  inherited Create;
  FFileName := aFileName;
end;

function TTaskStorage.SaveTasks(const aTasks: TTaskArray; aNextID: integer): boolean;
var
  fs: TFileStream;
  i, count: integer;
  task: TTask;
  titleLen, descLen, tagsLen: integer;
begin
  Result := false;
  try
    fs := TFileStream.Create(FFileName, fmCreate);
    try
      // Write header: version and next ID
      fs.WriteBuffer(aNextID, SizeOf(aNextID));
      
      // Write task count
      count := Length(aTasks);
      fs.WriteBuffer(count, SizeOf(count));
      
      // Write each task
      for i := 0 to High(aTasks) do
      begin
        task := aTasks[i];
        
        // Write fixed-size fields
        fs.WriteBuffer(task.ID, SizeOf(task.ID));
        fs.WriteBuffer(task.Priority, SizeOf(task.Priority));
        fs.WriteBuffer(task.Status, SizeOf(task.Status));
        fs.WriteBuffer(task.CreatedDate, SizeOf(task.CreatedDate));
        fs.WriteBuffer(task.DueDate, SizeOf(task.DueDate));
        fs.WriteBuffer(task.CompletedDate, SizeOf(task.CompletedDate));
        fs.WriteBuffer(task.IsActive, SizeOf(task.IsActive));
        
        // Write variable-length strings
        titleLen := Length(task.Title);
        fs.WriteBuffer(titleLen, SizeOf(titleLen));
        if titleLen > 0 then
          fs.WriteBuffer(task.Title[1], titleLen);
          
        descLen := Length(task.Description);
        fs.WriteBuffer(descLen, SizeOf(descLen));
        if descLen > 0 then
          fs.WriteBuffer(task.Description[1], descLen);
          
        tagsLen := Length(task.Tags);
        fs.WriteBuffer(tagsLen, SizeOf(tagsLen));
        if tagsLen > 0 then
          fs.WriteBuffer(task.Tags[1], tagsLen);
      end;
      
      Result := true;
    finally
      fs.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error saving tasks: ', E.Message);
  end;
end;

function TTaskStorage.LoadTasks(out aTasks: TTaskArray; out aNextID: integer): boolean;
var
  fs: TFileStream;
  i, count: integer;
  task: TTask;
  titleLen, descLen, tagsLen: integer;
begin
  Result := false;
  SetLength(aTasks, 0);
  aNextID := 1;
  
  if not FileExists(FFileName) then
    Exit;
    
  try
    fs := TFileStream.Create(FFileName, fmOpenRead);
    try
      // Read header
      fs.ReadBuffer(aNextID, SizeOf(aNextID));
      
      // Read task count
      fs.ReadBuffer(count, SizeOf(count));
      
      SetLength(aTasks, count);
      
      // Read each task
      for i := 0 to count - 1 do
      begin
        // Read fixed-size fields
        fs.ReadBuffer(task.ID, SizeOf(task.ID));
        fs.ReadBuffer(task.Priority, SizeOf(task.Priority));
        fs.ReadBuffer(task.Status, SizeOf(task.Status));
        fs.ReadBuffer(task.CreatedDate, SizeOf(task.CreatedDate));
        fs.ReadBuffer(task.DueDate, SizeOf(task.DueDate));
        fs.ReadBuffer(task.CompletedDate, SizeOf(task.CompletedDate));
        fs.ReadBuffer(task.IsActive, SizeOf(task.IsActive));
        
        // Read variable-length strings
        fs.ReadBuffer(titleLen, SizeOf(titleLen));
        SetLength(task.Title, titleLen);
        if titleLen > 0 then
          fs.ReadBuffer(task.Title[1], titleLen);
          
        fs.ReadBuffer(descLen, SizeOf(descLen));
        SetLength(task.Description, descLen);
        if descLen > 0 then
          fs.ReadBuffer(task.Description[1], descLen);
          
        fs.ReadBuffer(tagsLen, SizeOf(tagsLen));
        SetLength(task.Tags, tagsLen);
        if tagsLen > 0 then
          fs.ReadBuffer(task.Tags[1], tagsLen);
          
        aTasks[i] := task;
      end;
      
      Result := true;
    finally
      fs.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error loading tasks: ', E.Message);
      SetLength(aTasks, 0);
      aNextID := 1;
    end;
  end;
end;

function TTaskStorage.ExportToCSV(const aTasks: TTaskArray; const aFileName: string): boolean;
var
  sl: TStringList;
  i: integer;
  task: TTask;
  line: string;
begin
  Result := false;
  try
    sl := TStringList.Create;
    try
      // Write header
      sl.Add('ID,Title,Description,Priority,Status,Created,Due,Completed,Tags');
      
      // Write tasks
      for i := 0 to High(aTasks) do
      begin
        if aTasks[i].IsActive then
        begin
          task := aTasks[i];
          line := Format('%d,"%s","%s",%s,%s,%s,%s,%s,"%s"',
            [task.ID,
             StringReplace(task.Title, '"', '""', [rfReplaceAll]),
             StringReplace(task.Description, '"', '""', [rfReplaceAll]),
             PriorityToString(task.Priority),
             StatusToString(task.Status),
             DateTimeToStr(task.CreatedDate),
             DateTimeToStr(task.DueDate),
             DateTimeToStr(task.CompletedDate),
             StringReplace(task.Tags, '"', '""', [rfReplaceAll])]);
          sl.Add(line);
        end;
      end;
      
      sl.SaveToFile(aFileName);
      Result := true;
    finally
      sl.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error exporting to CSV: ', E.Message);
  end;
end;

function TTaskStorage.ExportToText(const aTasks: TTaskArray; const aFileName: string): boolean;
var
  sl: TStringList;
  i: integer;
  task: TTask;
begin
  Result := false;
  try
    sl := TStringList.Create;
    try
      sl.Add('========================================');
      sl.Add('Task Manager Export');
      sl.Add('Generated: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
      sl.Add('========================================');
      sl.Add('');
      
      for i := 0 to High(aTasks) do
      begin
        if aTasks[i].IsActive then
        begin
          task := aTasks[i];
          sl.Add('Task #' + IntToStr(task.ID));
          sl.Add('  Title: ' + task.Title);
          sl.Add('  Description: ' + task.Description);
          sl.Add('  Priority: ' + PriorityToString(task.Priority));
          sl.Add('  Status: ' + StatusToString(task.Status));
          sl.Add('  Created: ' + DateTimeToStr(task.CreatedDate));
          if task.DueDate > 0 then
            sl.Add('  Due: ' + DateTimeToStr(task.DueDate));
          if task.CompletedDate > 0 then
            sl.Add('  Completed: ' + DateTimeToStr(task.CompletedDate));
          if task.Tags <> '' then
            sl.Add('  Tags: ' + task.Tags);
          sl.Add('');
        end;
      end;
      
      sl.SaveToFile(aFileName);
      Result := true;
    finally
      sl.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error exporting to text: ', E.Message);
  end;
end;

end.
