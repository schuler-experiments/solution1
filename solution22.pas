
program KnowledgeBaseDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  taskmanager, taskmanageradvanced, taskmanagerenhanced,
  taskmanagerknowledge;

procedure SelfTest;
var
  KB: TKnowledgeTaskManager;
  ArticleID1, ArticleID2, ArticleID3, ArticleID4: Integer;
  TaskID1, TaskID2: Integer;
  VersionID1: Integer;
  Articles: TKnowledgeArticleArray;
  SearchResults: TSearchResultArray;
  Stats: TKnowledgeStatistics;
  Tags: TKnowledgeTagArray;
  i: Integer;
  Article: TKnowledgeArticle;
begin
  WriteLn('=== Knowledge Base Management System - Self Test ===');
  WriteLn;
  
  KB := TKnowledgeTaskManager.Create;
  try
    // Test 1: Create knowledge articles
    WriteLn('Test 1: Creating knowledge articles...');
    ArticleID1 := KB.CreateArticle(
      'Git Best Practices',
      'Always write descriptive commit messages. Use branches for new features. ' +
      'Review code before merging. Keep commits atomic and focused.',
      kcBestPractice,
      'Alice Developer'
    );
    
    ArticleID2 := KB.CreateArticle(
      'How to Setup Development Environment',
      'Step 1: Install Free Pascal Compiler (fpc).'#13#10 +
      'Step 2: Configure your editor with syntax highlighting.'#13#10 +
      'Step 3: Set up version control (git).'#13#10 +
      'Step 4: Create project structure with src/ and bin/ folders.',
      kcHowTo,
      'Bob Engineer'
    );
    
    ArticleID3 := KB.CreateArticle(
      'Troubleshooting Compilation Errors',
      'If you get "identifier expected" errors, check for:'#13#10 +
      '- Missing semicolons'#13#10 +
      '- Typos in variable names'#13#10 +
      '- Incorrect use of reserved words'#13#10 +
      'Use the -vew flag for detailed error messages.',
      kcTroubleshooting,
      'Alice Developer'
    );
    
    ArticleID4 := KB.CreateArticle(
      'Project Architecture Design',
      'The task manager uses a layered architecture:'#13#10 +
      '1. Base layer: Core task management (taskmanager.pas)'#13#10 +
      '2. Enhanced layer: Advanced features'#13#10 +
      '3. Specialized layers: Domain-specific features'#13#10 +
      'Each layer inherits from the previous, maintaining backward compatibility.',
      kcDesignDoc,
      'Charlie Architect'
    );
    
    WriteLn(Format('Created %d knowledge articles', [KB.ArticleCount - 1]));
    WriteLn;
    
    // Test 2: Add tags
    WriteLn('Test 2: Adding tags to articles...');
    KB.AddTag(ArticleID1, 'git');
    KB.AddTag(ArticleID1, 'best-practices');
    KB.AddTag(ArticleID1, 'version-control');
    
    KB.AddTag(ArticleID2, 'setup');
    KB.AddTag(ArticleID2, 'beginner');
    KB.AddTag(ArticleID2, 'tutorial');
    
    KB.AddTag(ArticleID3, 'debugging');
    KB.AddTag(ArticleID3, 'compilation');
    KB.AddTag(ArticleID3, 'errors');
    
    KB.AddTag(ArticleID4, 'architecture');
    KB.AddTag(ArticleID4, 'design');
    KB.AddTag(ArticleID4, 'pascal');
    
    Tags := KB.GetMostUsedTags(5);
    WriteLn('Tagged articles. Top tags:');
    for i := 0 to High(Tags) do
      WriteLn(Format('  - %s (%d)', [Tags[i].Name, Tags[i].Count]));
    WriteLn;
    
    // Test 3: Link articles to tasks
    WriteLn('Test 3: Linking articles to tasks...');
    TaskID1 := KB.AddTask('Improve code quality', 'Refactor and clean up code',
                          'Development', tpMedium, EncodeDate(2024, 4, 1), 0);
    TaskID2 := KB.AddTask('Onboard new developer', 'Help new team member get started',
                          'Team', tpHigh, EncodeDate(2024, 3, 20), 0);
    
    KB.LinkToTask(ArticleID1, TaskID1);
    KB.LinkToTask(ArticleID2, TaskID2);
    KB.LinkToTask(ArticleID3, TaskID2);
    
    Articles := KB.GetArticlesForTask(TaskID2);
    WriteLn(Format('Task ID %d has %d linked articles', 
                  [TaskID2, Length(Articles)]));
    WriteLn;
    
    // Test 4: Article workflow
    WriteLn('Test 4: Testing article workflow...');
    Article := KB.GetArticle(ArticleID1);
    WriteLn(Format('Article status: %s', [KB.StatusToString(Article.Status)]));
    
    KB.SubmitForReview(ArticleID1, 'Alice Developer');
    Article := KB.GetArticle(ArticleID1);
    WriteLn(Format('After submit: %s', [KB.StatusToString(Article.Status)]));
    
    KB.ApproveArticle(ArticleID1, 'Manager Review');
    Article := KB.GetArticle(ArticleID1);
    WriteLn(Format('After approval: %s', [KB.StatusToString(Article.Status)]));
    WriteLn;
    
    // Test 5: Versioning
    WriteLn('Test 5: Creating article versions...');
    VersionID1 := KB.CreateVersion(ArticleID1, 'v1.0', 'Initial approved version');
    WriteLn(Format('Created version ID %d', [VersionID1]));
    
    KB.UpdateArticle(ArticleID1, 'Git Best Practices (Updated)',
                    'Always write descriptive commit messages. Use branches. ' +
                    'Review code. Keep commits atomic. PLUS: Use conventional commits!',
                    'Alice Developer');
    KB.CreateVersion(ArticleID1, 'v1.1', 'Added conventional commits info');
    WriteLn('Updated article and created new version');
    WriteLn;
    
    // Test 6: Search functionality
    WriteLn('Test 6: Searching knowledge base...');
    SearchResults := KB.SearchArticles('compilation');
    WriteLn(Format('Search for "compilation" found %d results:', [Length(SearchResults)]));
    for i := 0 to High(SearchResults) do
      WriteLn(Format('  %d. %s (relevance: %.1f)',
                    [i + 1, SearchResults[i].Title, SearchResults[i].Relevance]));
    WriteLn;
    
    // Test 7: Ratings and popularity
    WriteLn('Test 7: Rating articles...');
    KB.RateArticle(ArticleID2, 5);
    KB.RateArticle(ArticleID2, 4);
    KB.RateArticle(ArticleID2, 5);
    KB.IncrementViewCount(ArticleID2);
    KB.IncrementViewCount(ArticleID2);
    KB.IncrementViewCount(ArticleID2);
    
    KB.RateArticle(ArticleID1, 5);
    KB.IncrementViewCount(ArticleID1);
    
    KB.SetFeatured(ArticleID2, True);
    
    Articles := KB.GetHighestRated(3);
    WriteLn('Highest rated articles:');
    for i := 0 to High(Articles) do
      WriteLn(Format('  %d. "%s" - %.1f/5.0 (%d ratings)',
                    [i + 1, Articles[i].Title, Articles[i].Rating, Articles[i].RatingCount]));
    WriteLn;
    
    // Test 8: Statistics and reports
    WriteLn('Test 8: Generating statistics...');
    Stats := KB.GetKnowledgeStatistics;
    WriteLn(Format('Total articles: %d', [Stats.TotalArticles]));
    WriteLn(Format('Total words: %d', [Stats.TotalWords]));
    WriteLn(Format('Total views: %d', [Stats.TotalViews]));
    WriteLn(Format('Average rating: %.2f/5.0', [Stats.AverageRating]));
    WriteLn;
    
    WriteLn(KB.GetCategoryReport);
    WriteLn;
    
    WriteLn(KB.GetAuthorContributions);
    WriteLn;
    
    WriteLn(KB.GetKnowledgeGaps);
    WriteLn;
    
    // Test 9: Export to markdown
    WriteLn('Test 9: Exporting to markdown...');
    WriteLn('--- Exported Article ---');
    WriteLn(KB.ExportArticleToMarkdown(ArticleID1));
    WriteLn('--- End Export ---');
    WriteLn;
    
    WriteLn('=== All Tests Completed Successfully! ===');
    
  finally
    KB.Free;
  end;
end;

begin
  try
    SelfTest;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
