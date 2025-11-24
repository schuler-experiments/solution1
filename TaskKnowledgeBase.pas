unit TaskKnowledgeBase;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TStringArray = array of string;
  TIntegerArray = array of integer;

  { Knowledge base article types }
  TKnowledgeArticleType = (katBestPractice, katLessonLearned, katTechnicalNote, katTroubleshooting, katTemplate);

  { Knowledge article structure }
  TKnowledgeArticle = record
    id: integer;
    title: string;
    content: string;
    articleType: TKnowledgeArticleType;
    author: string;
    createdDate: TDateTime;
    lastModified: TDateTime;
    views: integer;
    relatedTaskIds: array of integer;
    tags: array of string;
  end;

  TKnowledgeArticleArray = array of TKnowledgeArticle;

  TKnowledgeBaseClass = class
  private
    articles: TKnowledgeArticleArray;
    nextId: integer;
    function FindArticleIndex(id: integer): integer;
    function ArticleTypeToString(atype: TKnowledgeArticleType): string;
    function StringToArticleType(s: string): TKnowledgeArticleType;
  public
    constructor Create();
    destructor Destroy();
    
    { Article Management }
    function AddArticle(title, content: string; articleType: TKnowledgeArticleType;
                       author: string): integer;
    function GetArticle(id: integer): TKnowledgeArticle;
    function UpdateArticle(id: integer; newArticle: TKnowledgeArticle): boolean;
    function DeleteArticle(id: integer): boolean;
    function GetAllArticles(): TKnowledgeArticleArray;
    
    { Searching and Filtering }
    function SearchByTitle(keyword: string): TKnowledgeArticleArray;
    function GetArticlesByType(articleType: TKnowledgeArticleType): TKnowledgeArticleArray;
    function GetArticlesByTag(tag: string): TKnowledgeArticleArray;
    function GetArticlesForTask(taskId: integer): TKnowledgeArticleArray;
    
    { Statistics }
    function GetArticleCount(): integer;
    function GetMostViewedArticles(limit: integer): TKnowledgeArticleArray;
    function GetRecentArticles(days: integer): TKnowledgeArticleArray;
    function GetArticleStats(): string;
    
    { Tag Management }
    function GetAllTags(): TStringArray;
    function AddTagToArticle(articleId: integer; tag: string): boolean;
    function RemoveTagFromArticle(articleId: integer; tag: string): boolean;
    
    { Task Association }
    function LinkTaskToArticle(articleId, taskId: integer): boolean;
    function UnlinkTaskFromArticle(articleId, taskId: integer): boolean;
    
    { Testing }
    procedure SelfTest();
  end;

implementation

constructor TKnowledgeBaseClass.Create();
begin
  SetLength(articles, 0);
  nextId := 1;
end;

destructor TKnowledgeBaseClass.Destroy();
begin
  SetLength(articles, 0);
  inherited Destroy();
end;

function TKnowledgeBaseClass.FindArticleIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(articles) - 1 do
  begin
    if articles[i].id = id then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TKnowledgeBaseClass.ArticleTypeToString(atype: TKnowledgeArticleType): string;
begin
  case atype of
    katBestPractice: result := 'Best Practice';
    katLessonLearned: result := 'Lesson Learned';
    katTechnicalNote: result := 'Technical Note';
    katTroubleshooting: result := 'Troubleshooting';
    katTemplate: result := 'Template';
  else
    result := 'Unknown';
  end;
end;

function TKnowledgeBaseClass.StringToArticleType(s: string): TKnowledgeArticleType;
begin
  if s = 'Best Practice' then
    result := katBestPractice
  else if s = 'Lesson Learned' then
    result := katLessonLearned
  else if s = 'Technical Note' then
    result := katTechnicalNote
  else if s = 'Troubleshooting' then
    result := katTroubleshooting
  else if s = 'Template' then
    result := katTemplate
  else
    result := katBestPractice;
end;

function TKnowledgeBaseClass.AddArticle(title, content: string; 
                                       articleType: TKnowledgeArticleType;
                                       author: string): integer;
var
  article: TKnowledgeArticle;
begin
  article.id := nextId;
  article.title := title;
  article.content := content;
  article.articleType := articleType;
  article.author := author;
  article.createdDate := Now();
  article.lastModified := Now();
  article.views := 0;
  SetLength(article.relatedTaskIds, 0);
  SetLength(article.tags, 0);
  
  SetLength(articles, Length(articles) + 1);
  articles[Length(articles) - 1] := article;
  
  result := nextId;
  Inc(nextId);
end;

function TKnowledgeBaseClass.GetArticle(id: integer): TKnowledgeArticle;
var
  idx: integer;
begin
  idx := FindArticleIndex(id);
  if idx >= 0 then
  begin
    Inc(articles[idx].views);
    result := articles[idx];
  end
  else
  begin
    FillChar(result, SizeOf(result), 0);
    result.id := -1;
  end;
end;

function TKnowledgeBaseClass.UpdateArticle(id: integer; 
                                          newArticle: TKnowledgeArticle): boolean;
var
  idx: integer;
begin
  idx := FindArticleIndex(id);
  if idx >= 0 then
  begin
    newArticle.id := id;
    newArticle.lastModified := Now();
    articles[idx] := newArticle;
    result := true;
  end
  else
    result := false;
end;

function TKnowledgeBaseClass.DeleteArticle(id: integer): boolean;
var
  idx, i: integer;
begin
  idx := FindArticleIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(articles) - 2 do
      articles[i] := articles[i + 1];
    SetLength(articles, Length(articles) - 1);
    result := true;
  end
  else
    result := false;
end;

function TKnowledgeBaseClass.GetAllArticles(): TKnowledgeArticleArray;
begin
  result := Copy(articles, 0, Length(articles));
end;

function TKnowledgeBaseClass.SearchByTitle(keyword: string): TKnowledgeArticleArray;
var
  i, count: integer;
  matches: TKnowledgeArticleArray;
  keywordLower: string;
begin
  SetLength(matches, 0);
  keywordLower := LowerCase(keyword);
  count := 0;
  
  for i := 0 to Length(articles) - 1 do
  begin
    if Pos(keywordLower, LowerCase(articles[i].title)) > 0 then
    begin
      SetLength(matches, count + 1);
      matches[count] := articles[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TKnowledgeBaseClass.GetArticlesByType(
                    articleType: TKnowledgeArticleType): TKnowledgeArticleArray;
var
  i, count: integer;
  matches: TKnowledgeArticleArray;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(articles) - 1 do
  begin
    if articles[i].articleType = articleType then
    begin
      SetLength(matches, count + 1);
      matches[count] := articles[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TKnowledgeBaseClass.GetArticlesByTag(tag: string): TKnowledgeArticleArray;
var
  i, j, count: integer;
  matches: TKnowledgeArticleArray;
  found: boolean;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(articles) - 1 do
  begin
    found := false;
    for j := 0 to Length(articles[i].tags) - 1 do
    begin
      if articles[i].tags[j] = tag then
      begin
        found := true;
        Break;
      end;
    end;
    
    if found then
    begin
      SetLength(matches, count + 1);
      matches[count] := articles[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TKnowledgeBaseClass.GetArticlesForTask(taskId: integer): TKnowledgeArticleArray;
var
  i, j, count: integer;
  matches: TKnowledgeArticleArray;
  found: boolean;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(articles) - 1 do
  begin
    found := false;
    for j := 0 to Length(articles[i].relatedTaskIds) - 1 do
    begin
      if articles[i].relatedTaskIds[j] = taskId then
      begin
        found := true;
        Break;
      end;
    end;
    
    if found then
    begin
      SetLength(matches, count + 1);
      matches[count] := articles[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TKnowledgeBaseClass.GetArticleCount(): integer;
begin
  result := Length(articles);
end;

function TKnowledgeBaseClass.GetMostViewedArticles(limit: integer): TKnowledgeArticleArray;
var
  i, j, count: integer;
  sorted: TKnowledgeArticleArray;
  temp: TKnowledgeArticle;
begin
  SetLength(sorted, Length(articles));
  for i := 0 to Length(articles) - 1 do
    sorted[i] := articles[i];
  
  { Simple bubble sort by views }
  for i := 0 to Length(sorted) - 2 do
  begin
    for j := 0 to Length(sorted) - 2 - i do
    begin
      if sorted[j].views < sorted[j + 1].views then
      begin
        temp := sorted[j];
        sorted[j] := sorted[j + 1];
        sorted[j + 1] := temp;
      end;
    end;
  end;
  
  if limit > Length(sorted) then
    limit := Length(sorted);
  
  SetLength(result, limit);
  for i := 0 to limit - 1 do
    result[i] := sorted[i];
end;

function TKnowledgeBaseClass.GetRecentArticles(days: integer): TKnowledgeArticleArray;
var
  i, count: integer;
  matches: TKnowledgeArticleArray;
  cutoffDate: TDateTime;
begin
  SetLength(matches, 0);
  cutoffDate := Now() - days;
  count := 0;
  
  for i := 0 to Length(articles) - 1 do
  begin
    if articles[i].createdDate >= cutoffDate then
    begin
      SetLength(matches, count + 1);
      matches[count] := articles[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TKnowledgeBaseClass.GetArticleStats(): string;
var
  totalViews, avgViews: integer;
  i: integer;
begin
  if Length(articles) = 0 then
  begin
    result := 'No articles in knowledge base';
    Exit;
  end;
  
  totalViews := 0;
  for i := 0 to Length(articles) - 1 do
    Inc(totalViews, articles[i].views);
  
  avgViews := totalViews div Length(articles);
  
  result := Format('Total Articles: %d | Total Views: %d | Avg Views: %d', 
                   [Length(articles), totalViews, avgViews]);
end;

function TKnowledgeBaseClass.GetAllTags(): TStringArray;
var
  tags: TStringArray;
  i, j, k, count: integer;
  found: boolean;
begin
  SetLength(tags, 0);
  count := 0;
  
  for i := 0 to Length(articles) - 1 do
  begin
    for j := 0 to Length(articles[i].tags) - 1 do
    begin
      found := false;
      for k := 0 to count - 1 do
      begin
        if tags[k] = articles[i].tags[j] then
        begin
          found := true;
          Break;
        end;
      end;
      
      if not found then
      begin
        SetLength(tags, count + 1);
        tags[count] := articles[i].tags[j];
        Inc(count);
      end;
    end;
  end;
  
  result := tags;
end;

function TKnowledgeBaseClass.AddTagToArticle(articleId: integer; 
                                            tag: string): boolean;
var
  idx, i: integer;
  found: boolean;
begin
  idx := FindArticleIndex(articleId);
  if idx < 0 then
  begin
    result := false;
    Exit;
  end;
  
  found := false;
  for i := 0 to Length(articles[idx].tags) - 1 do
  begin
    if articles[idx].tags[i] = tag then
    begin
      found := true;
      Break;
    end;
  end;
  
  if not found then
  begin
    SetLength(articles[idx].tags, Length(articles[idx].tags) + 1);
    articles[idx].tags[Length(articles[idx].tags) - 1] := tag;
    result := true;
  end
  else
    result := false;
end;

function TKnowledgeBaseClass.RemoveTagFromArticle(articleId: integer; 
                                                 tag: string): boolean;
var
  idx, i, j: integer;
begin
  idx := FindArticleIndex(articleId);
  if idx < 0 then
  begin
    result := false;
    Exit;
  end;
  
  for i := 0 to Length(articles[idx].tags) - 1 do
  begin
    if articles[idx].tags[i] = tag then
    begin
      for j := i to Length(articles[idx].tags) - 2 do
        articles[idx].tags[j] := articles[idx].tags[j + 1];
      SetLength(articles[idx].tags, Length(articles[idx].tags) - 1);
      result := true;
      Exit;
    end;
  end;
  
  result := false;
end;

function TKnowledgeBaseClass.LinkTaskToArticle(articleId, taskId: integer): boolean;
var
  idx, i: integer;
  found: boolean;
begin
  idx := FindArticleIndex(articleId);
  if idx < 0 then
  begin
    result := false;
    Exit;
  end;
  
  found := false;
  for i := 0 to Length(articles[idx].relatedTaskIds) - 1 do
  begin
    if articles[idx].relatedTaskIds[i] = taskId then
    begin
      found := true;
      Break;
    end;
  end;
  
  if not found then
  begin
    SetLength(articles[idx].relatedTaskIds, Length(articles[idx].relatedTaskIds) + 1);
    articles[idx].relatedTaskIds[Length(articles[idx].relatedTaskIds) - 1] := taskId;
    result := true;
  end
  else
    result := false;
end;

function TKnowledgeBaseClass.UnlinkTaskFromArticle(articleId, taskId: integer): boolean;
var
  idx, i, j: integer;
begin
  idx := FindArticleIndex(articleId);
  if idx < 0 then
  begin
    result := false;
    Exit;
  end;
  
  for i := 0 to Length(articles[idx].relatedTaskIds) - 1 do
  begin
    if articles[idx].relatedTaskIds[i] = taskId then
    begin
      for j := i to Length(articles[idx].relatedTaskIds) - 2 do
        articles[idx].relatedTaskIds[j] := articles[idx].relatedTaskIds[j + 1];
      SetLength(articles[idx].relatedTaskIds, Length(articles[idx].relatedTaskIds) - 1);
      result := true;
      Exit;
    end;
  end;
  
  result := false;
end;

procedure TKnowledgeBaseClass.SelfTest();
var
  id1, id2, id3: integer;
  article: TKnowledgeArticle;
  allArticles: TKnowledgeArticleArray;
  searchResults: TKnowledgeArticleArray;
begin
  WriteLn('');
  WriteLn('=== Knowledge Base Self Test ===');
  
  { Add articles }
  id1 := AddArticle('Best Practices for Task Scheduling', 
                   'Break down large tasks into smaller, manageable subtasks',
                   katBestPractice, 'John Doe');
  id2 := AddArticle('Handling Scope Creep', 
                   'Use change management to handle scope changes',
                   katLessonLearned, 'Jane Smith');
  id3 := AddArticle('Performance Optimization Tips', 
                   'Optimize database queries and use caching',
                   katTechnicalNote, 'Bob Johnson');
  
  WriteLn(Format('Added 3 articles with IDs: %d, %d, %d', [id1, id2, id3]));
  
  { Add tags }
  AddTagToArticle(id1, 'scheduling');
  AddTagToArticle(id1, 'best-practice');
  AddTagToArticle(id2, 'change-management');
  WriteLn('Added tags to articles');
  
  { Link tasks }
  LinkTaskToArticle(id1, 101);
  LinkTaskToArticle(id2, 102);
  WriteLn('Linked articles to tasks');
  
  { Search }
  searchResults := SearchByTitle('Best');
  WriteLn(Format('Search for "Best": found %d articles', [Length(searchResults)]));
  
  { Get by type }
  allArticles := GetArticlesByType(katBestPractice);
  WriteLn(Format('Best Practice articles: %d', [Length(allArticles)]));
  
  { Get article count }
  WriteLn(Format('Total articles: %d', [GetArticleCount()]));
  
  { Statistics }
  WriteLn(GetArticleStats());
  
  WriteLn('=== Knowledge Base Self Test Complete ===');
end;

end.
