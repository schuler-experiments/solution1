
unit TaskKnowledgeBaseSearch;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  TStringArray = array of string;
  
  { Knowledge article types }
  TKnowledgeArticleType = (katBestPractice, katTutorial, katTroubleshoot, katReference, katGuide);

  { Knowledge article record }
  TKnowledgeArticle = record
    id: integer;
    title: string;
    content: string;
    articleType: TKnowledgeArticleType;
    tags: array of string;
    relatedTaskIds: array of integer;
    viewCount: integer;
    createdDate: TDateTime;
  end;

  TKnowledgeArticleArray = array of TKnowledgeArticle;

  { Knowledge base search helper }
  TKnowledgeBaseSearchHelper = class
  public
    { Search operations }
    function SearchByTitle(keyword: string; var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
    function GetArticlesByTag(tag: string; var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
    function GetArticlesByType(articleType: TKnowledgeArticleType; var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
    function GetArticlesForTask(taskId: integer; var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
    
    { Filtering helpers }
    function GetMostViewedArticles(limit: integer; var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
    function GetRecentArticles(days: integer; var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
    
    { Tag operations }
    function GetAllTags(var articles: array of TKnowledgeArticle): TStringArray;
    function HasTag(var article: TKnowledgeArticle; tag: string): boolean;
    
    { Type conversion }
    function ArticleTypeToString(atype: TKnowledgeArticleType): string;
    function StringToArticleType(s: string): TKnowledgeArticleType;
    
    { Self test }
    procedure SelfTest();
  end;

implementation

function TKnowledgeBaseSearchHelper.SearchByTitle(keyword: string; 
                                                  var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
var
  i, count: integer;
  matches: TKnowledgeArticleArray;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(articles) - 1 do
  begin
    if Pos(keyword, articles[i].title) > 0 then
    begin
      SetLength(matches, count + 1);
      matches[count] := articles[i];
      Inc(count);
    end;
  end;
  
  result := matches;
end;

function TKnowledgeBaseSearchHelper.GetArticlesByTag(tag: string; 
                                                     var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
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

function TKnowledgeBaseSearchHelper.GetArticlesByType(articleType: TKnowledgeArticleType; 
                                                      var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
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

function TKnowledgeBaseSearchHelper.GetArticlesForTask(taskId: integer; 
                                                       var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
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

function TKnowledgeBaseSearchHelper.GetMostViewedArticles(limit: integer; 
                                                          var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
var
  i, count: integer;
  matches: TKnowledgeArticleArray;
begin
  SetLength(matches, 0);
  count := 0;
  
  for i := 0 to Length(articles) - 1 do
  begin
    if articles[i].viewCount > 0 then
    begin
      SetLength(matches, count + 1);
      matches[count] := articles[i];
      Inc(count);
      if count >= limit then Break;
    end;
  end;
  
  result := matches;
end;

function TKnowledgeBaseSearchHelper.GetRecentArticles(days: integer; 
                                                      var articles: array of TKnowledgeArticle): TKnowledgeArticleArray;
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

function TKnowledgeBaseSearchHelper.GetAllTags(var articles: array of TKnowledgeArticle): TStringArray;
var
  i, j, k, count: integer;
  tags: TStringArray;
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

function TKnowledgeBaseSearchHelper.HasTag(var article: TKnowledgeArticle; tag: string): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Length(article.tags) - 1 do
  begin
    if article.tags[i] = tag then
    begin
      result := true;
      Exit;
    end;
  end;
end;

function TKnowledgeBaseSearchHelper.ArticleTypeToString(atype: TKnowledgeArticleType): string;
begin
  case atype of
    katBestPractice: result := 'Best Practice';
    katTutorial: result := 'Tutorial';
    katTroubleshoot: result := 'Troubleshooting';
    katReference: result := 'Reference';
    katGuide: result := 'Guide';
  else
    result := 'Unknown';
  end;
end;

function TKnowledgeBaseSearchHelper.StringToArticleType(s: string): TKnowledgeArticleType;
begin
  if s = 'Best Practice' then
    result := katBestPractice
  else if s = 'Tutorial' then
    result := katTutorial
  else if s = 'Troubleshooting' then
    result := katTroubleshoot
  else if s = 'Reference' then
    result := katReference
  else if s = 'Guide' then
    result := katGuide
  else
    result := katReference;
end;

procedure TKnowledgeBaseSearchHelper.SelfTest();
var
  testArticles: TKnowledgeArticleArray;
  results: TKnowledgeArticleArray;
begin
  WriteLn('');
  WriteLn('=== Knowledge Base Search Helper Self Test ===');
  
  { Create test articles }
  SetLength(testArticles, 2);
  testArticles[0].id := 1;
  testArticles[0].title := 'Getting Started with Pascal';
  testArticles[0].articleType := katTutorial;
  SetLength(testArticles[0].tags, 1);
  testArticles[0].tags[0] := 'beginner';
  
  testArticles[1].id := 2;
  testArticles[1].title := 'Advanced Pascal Techniques';
  testArticles[1].articleType := katGuide;
  SetLength(testArticles[1].tags, 1);
  testArticles[1].tags[0] := 'advanced';
  
  { Test search }
  results := SearchByTitle('Pascal', testArticles);
  WriteLn(Format('Search results for "Pascal": %d', [Length(results)]));
  
  { Test type conversion }
  WriteLn('Tutorial type: ' + ArticleTypeToString(katTutorial));
  
  WriteLn('=== Knowledge Base Search Helper Self Test Complete ===');
end;

end.
