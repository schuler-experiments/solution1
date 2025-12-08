
program SearchEngineDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, taskmanager, taskmanagerext, taskmanageradvanced,
  taskmanagersearch;

procedure SelfTest;
var
  TM: TSearchTaskManager;
  TaskID1, TaskID2, TaskID3, TaskID4, TaskID5: Integer;
  SearchID1, SearchID2, SearchID3: Integer;
  SearchResults: TSearchResultArray;
  SavedSearches: TSavedSearchArray;
  QuickFilters: TSavedSearchArray;
  SearchHistory: TSearchHistoryArray;
  Suggestions: TSearchSuggestionArray;
  Stats: TSearchStats;
  i: Integer;
  query: TSearchQuery;
  criteria: TSearchCriteria;
begin
  WriteLn('=== Advanced Search & Filter Engine - Self Test ===');
  WriteLn;
  
  TM := TSearchTaskManager.Create;
  try
    // Test 1: Create sample tasks
    WriteLn('Test 1: Creating sample tasks for search testing...');
    TaskID1 := TM.AddTask('Implement user authentication system', 
      'Build secure login and registration with OAuth support',
      'Backend', tpHigh, EncodeDate(2024, 3, 15), 12.0);
    
    TaskID2 := TM.AddTask('Design responsive homepage',
      'Create mobile-friendly homepage with modern UI/UX',
      'Frontend', tpMedium, EncodeDate(2024, 3, 20), 8.0);
    
    TaskID3 := TM.AddTask('Setup continuous integration pipeline',
      'Configure CI/CD with automated testing and deployment',
      'DevOps', tpHigh, EncodeDate(2024, 3, 10), 6.0);
    
    TaskID4 := TM.AddTask('Write API documentation',
      'Document all REST endpoints with examples',
      'Documentation', tpLow, EncodeDate(2024, 3, 25), 4.0);
    
    TaskID5 := TM.AddTask('Optimize database queries',
      'Improve query performance and add indexing',
      'Backend', tpCritical, EncodeDate(2024, 3, 5), 10.0);
    
    TM.AddTagToTask(TaskID1, 'security');
    TM.AddTagToTask(TaskID1, 'authentication');
    TM.AddTagToTask(TaskID2, 'ui');
    TM.AddTagToTask(TaskID2, 'design');
    TM.AddTagToTask(TaskID3, 'automation');
    TM.AddTagToTask(TaskID5, 'performance');
    
    WriteLn(Format('Created %d tasks with tags', [TM.TaskCount]));
    WriteLn;
    
    // Test 2: Quick search
    WriteLn('Test 2: Quick search for "authentication"...');
    SearchResults := TM.QuickSearch('authentication');
    WriteLn(Format('Found %d results', [Length(SearchResults)]));
    for i := 0 to High(SearchResults) do
      WriteLn(Format('  - Task ID: %d, Relevance: %.2f', 
        [SearchResults[i].TaskID, SearchResults[i].RelevanceScore]));
    WriteLn;
    
    // Test 3: Search in specific field
    WriteLn('Test 3: Search for "Backend" in category field...');
    SearchResults := TM.SearchInField('Backend', sfCategory);
    WriteLn(Format('Found %d backend tasks', [Length(SearchResults)]));
    WriteLn;
    
    // Test 4: Full-text search
    WriteLn('Test 4: Full-text search for "API"...');
    SearchResults := TM.FullTextSearch('API');
    WriteLn(Format('Found %d tasks containing "API"', [Length(SearchResults)]));
    WriteLn;
    
    // Test 5: Create saved searches
    WriteLn('Test 5: Creating saved searches...');
    SearchID1 := TM.CreateSavedSearch('High Priority Tasks', 
      'Find all high priority tasks', 'priority:high', False);
    SearchID2 := TM.CreateSavedSearch('Backend Tasks',
      'All backend development tasks', 'category:backend', True);  // Quick filter
    SearchID3 := TM.CreateSavedSearch('Due This Week',
      'Tasks due in the next 7 days', 'due:thisweek', True);  // Quick filter
    
    WriteLn(Format('Created %d saved searches', [Length(TM.GetAllSavedSearches)]));
    WriteLn;
    
    // Test 6: Get quick filters
    WriteLn('Test 6: Retrieving quick filters...');
    QuickFilters := TM.GetQuickFilters;
    WriteLn(Format('Quick filters: %d', [Length(QuickFilters)]));
    for i := 0 to High(QuickFilters) do
      WriteLn(Format('  - %s: %s', [QuickFilters[i].Name, QuickFilters[i].Query]));
    WriteLn;
    
    // Test 7: Toggle favorite
    WriteLn('Test 7: Toggling favorite status...');
    TM.ToggleFavorite(SearchID1);
    SavedSearches := TM.GetFavoriteSearches;
    WriteLn(Format('Favorite searches: %d', [Length(SavedSearches)]));
    WriteLn;
    
    // Test 8: Execute saved search
    WriteLn('Test 8: Executing saved search...');
    SearchResults := TM.ExecuteSavedSearch(SearchID2);
    WriteLn(Format('Saved search returned %d results', [Length(SearchResults)]));
    WriteLn;
    
    // Test 9: Advanced search with criteria
    WriteLn('Test 9: Advanced search with multiple criteria...');
    criteria.Field := sfCategory;
    criteria.ComparisonOp := coContains;
    criteria.Value := 'Backend';
    criteria.CaseSensitive := False;
    
    SetLength(query.Criteria, 1);
    query.Criteria[0] := criteria;
    query.LogicOperator := soAND;
    query.IncludeArchived := False;
    query.IncludeCompleted := True;
    query.MaxResults := 10;
    
    SearchResults := TM.AdvancedSearch(query);
    WriteLn(Format('Advanced search found %d tasks', [Length(SearchResults)]));
    WriteLn;
    
    // Test 10: Search history
    WriteLn('Test 10: Checking search history...');
    SearchHistory := TM.GetRecentSearches(5);
    WriteLn(Format('Recent searches: %d', [Length(SearchHistory)]));
    for i := 0 to High(SearchHistory) do
      WriteLn(Format('  - "%s" (%d results, %d ms)', 
        [SearchHistory[i].Query, SearchHistory[i].ResultCount, 
         SearchHistory[i].ExecutionTimeMs]));
    WriteLn;
    
    // Test 11: Search suggestions
    WriteLn('Test 11: Getting search suggestions for "auth"...');
    Suggestions := TM.GetSearchSuggestions('auth', 5);
    WriteLn(Format('Suggestions: %d', [Length(Suggestions)]));
    for i := 0 to High(Suggestions) do
      WriteLn(Format('  - %s (%s)', [Suggestions[i].Text, Suggestions[i].Type_]));
    WriteLn;
    
    // Test 12: Search index management
    WriteLn('Test 12: Managing search index...');
    WriteLn(TM.GetIndexStatus);
    TM.RebuildSearchIndex;
    WriteLn('Rebuilt search index');
    WriteLn(TM.GetIndexStatus);
    WriteLn;
    
    // Test 13: Search statistics
    WriteLn('Test 13: Getting search statistics...');
    Stats := TM.GetSearchStats;
    WriteLn(Format('Total searches: %d', [Stats.TotalSearches]));
    WriteLn(Format('Average execution time: %.2f ms', [Stats.AverageExecutionTime]));
    WriteLn(Format('Index size: %d entries', [Stats.IndexSize]));
    WriteLn(Format('Popular filters: %d', [Length(Stats.PopularFilters)]));
    WriteLn;
    
    // Test 14: Create snippet
    WriteLn('Test 14: Creating search snippets...');
    WriteLn('Snippet for "authentication": ' + 
      TM.CreateSnippet('Build secure login and registration with OAuth support', 
      'OAuth', 20));
    WriteLn;
    
    // Test 15: Highlight matches
    WriteLn('Test 15: Highlighting search matches...');
    WriteLn('Highlighted: ' + TM.HighlightMatches('Configure CI/CD pipeline', 'CI/CD'));
    WriteLn;
    
    // Test 16: Export saved searches
    WriteLn('Test 16: Exporting saved searches...');
    WriteLn('Exported data:');
    WriteLn(TM.ExportSavedSearches);
    
    // Test 17: Save search data to file
    WriteLn('Test 17: Saving search data to file...');
    if TM.SaveSearchDataToFile('solution1/search_data.dat') then
      WriteLn('Search data saved successfully')
    else
      WriteLn('Failed to save search data');
    WriteLn;
    
    // Test 18: Search trends
    WriteLn('Test 18: Getting search trends...');
    WriteLn(TM.GetSearchTrends);
    WriteLn;
    
    WriteLn('=== All Search Engine Tests Completed Successfully! ===');
    
  finally
    TM.Free;
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
