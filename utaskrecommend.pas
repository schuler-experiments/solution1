
unit utaskrecommend;

{$mode objfpc}
{$h+}

interface

uses
  sysutils, dateutils, math, utaskmanager;

type
  ttaskrecommendation = record
    task_id: integer;
    task_title: string;
    score: double;
    reason: string;
  end;

  trecommendationarray = array of ttaskrecommendation;

  ttaskrecommender = class
  public
    function get_recommendations(amanager: ttaskmanager; alimit: integer = 5): trecommendationarray;
    function calculate_score(const atask: ttask; amanager: ttaskmanager): double;
    procedure self_test(amanager: ttaskmanager);
  end;

implementation

function ttaskrecommender.calculate_score(const atask: ttask; amanager: ttaskmanager): double;
var
  days_to_deadline: double;
  urgency, importance, complexity: double;
begin
  // Base score from priority
  importance := (ord(atask.priority) + 1) * 10.0;
  
  // Urgency based on deadline
  if atask.due_date < now then
    urgency := 60.0 // Overdue is critical
  else
  begin
    days_to_deadline := atask.due_date - now;
    if days_to_deadline < 1 then
      urgency := 40.0
    else if days_to_deadline < 3 then
      urgency := 20.0
    else
      urgency := 0.0;
  end;
    
  // Complexity penalty based on estimated hours
  complexity := atask.estimated_hours * 2.0;
  
  // Final Score
  result := importance + urgency - (complexity * 0.1);
  
  if atask.is_important then
    result := result + 15.0;
end;

function ttaskrecommender.get_recommendations(amanager: ttaskmanager; alimit: integer = 5): trecommendationarray;
var
  all_tasks: ttaskarray;
  i, j, count: integer;
  temp: ttaskrecommendation;
begin
  all_tasks := amanager.get_tasks;
  setlength(result, 0);
  count := 0;
  
  for i := 0 to high(all_tasks) do
  begin
    // Use correct status and archive check
    if (all_tasks[i].status <> ts_done) and (not all_tasks[i].archived) then
    begin
      inc(count);
      setlength(result, count);
      result[count-1].task_id := all_tasks[i].id;
      result[count-1].task_title := all_tasks[i].title;
      result[count-1].score := calculate_score(all_tasks[i], amanager);
      result[count-1].reason := 'Based on priority, importance and deadline';
    end;
  end;
  
  // Sort recommendations by score descending
  for i := 0 to high(result) - 1 do
    for j := i + 1 to high(result) do
      if result[i].score < result[j].score then
      begin
        temp := result[i];
        result[i] := result[j];
        result[j] := temp;
      end;
      
  if length(result) > alimit then
    setlength(result, alimit);
end;

procedure ttaskrecommender.self_test(amanager: ttaskmanager);
var
  recs: trecommendationarray;
  i: integer;
begin
  writeln('--- Starting TTaskRecommender Self Test ---');
  if amanager = nil then
  begin
    writeln('Error: Manager is nil');
    exit;
  end;
  
  recs := get_recommendations(amanager);
  writeln('Top Recommendations:');
  for i := 0 to high(recs) do
  begin
    writeln(format('ID: %d, Title: %s, Score: %0.2f', [recs[i].task_id, recs[i].task_title, recs[i].score]));
  end;
  
  if length(recs) > 0 then
    writeln('Success: Recommendations generated.')
  else
    writeln('Note: No pending tasks for recommendations.');
    
  writeln('--- TTaskRecommender Self Test Completed ---');
end;

end.
