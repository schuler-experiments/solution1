
# Advanced Search & Filter Engine

## Overview

The **Advanced Search & Filter Engine** adds powerful search capabilities to the Task Manager system, enabling users to quickly find tasks using various search methods, save frequently used searches, and track search history.

## Features

### 1. Quick Search
- **Simple text search** across multiple fields (title, description, category, tags)
- **Full-text search** with relevance scoring
- **Field-specific search** to narrow results to specific task properties
- **Fast execution** with intelligent indexing

### 2. Saved Searches
- **Create reusable searches** with custom names and descriptions
- **Quick filters** for one-click access to common searches
- **Favorite searches** for your most-used queries
- **Usage tracking** to see which searches are most popular
- **Export/Import** saved searches for sharing or backup

### 3. Advanced Search
- **Multiple criteria** with AND/OR/NOT logic operators
- **Field-specific operators**:
  - Equals
  - Contains
  - Starts with
  - Ends with
  - Greater than / Less than
  - Between
  - Regular expression (future)
- **Case-sensitive** or case-insensitive matching
- **Limit results** with configurable maximum
- **Include/exclude** completed or archived tasks

### 4. Search History
- **Automatic tracking** of all searches performed
- **Performance metrics** (execution time, result count)
- **Recent searches** for quick re-execution
- **Popular searches** based on frequency
- **Configurable history size** (default: 100 entries)
- **Clear history** option for privacy

### 5. Search Suggestions
- **Autocomplete** based on previous searches
- **Smart suggestions** from search history
- **Recent searches** displayed first
- **Popular terms** for common queries
- **Context-aware** suggestions

### 6. Search Index
- **Fast searching** with pre-built index
- **Multiple fields indexed**: title, description, category, tags
- **Automatic updates** when tasks are modified
- **Manual rebuild** option for optimization
- **Enable/disable** indexing as needed
- **Index statistics** and health monitoring

### 7. Search Analytics
- **Total search count** across all time
- **Average execution time** for performance monitoring
- **Most searched terms** for understanding usage patterns
- **Popular filters** ranked by usage
- **Search trends** over time
- **Index size** and performance metrics

### 8. Search Result Features
- **Relevance scoring** to rank results by importance
- **Matched fields** highlighting which fields contained the search term
- **Snippets** showing context around matched terms
- **Highlight matches** in result text
- **Sort results** by relevance or other criteria

## Data Structures

### TSearchField
Fields that can be searched:
- `sfTitle` - Task title
- `sfDescription` - Task description
- `sfCategory` - Task category
- `sfTags` - Task tags
- `sfNotes` - Task notes
- `sfAll` - All text fields
- `sfAssignee` - Assigned team member
- `sfCreator` - Task creator

### TComparisonOp
Comparison operators for search criteria:
- `coEquals` - Exact match
- `coContains` - Contains substring
- `coStartsWith` - Starts with text
- `coEndsWith` - Ends with text
- `coGreaterThan` - Numeric/date comparison
- `coLessThan` - Numeric/date comparison
- `coBetween` - Range comparison
- `coRegex` - Regular expression (future)

### TSearchOperator
Logic operators for combining criteria:
- `soAND` - All criteria must match
- `soOR` - Any criteria can match
- `soNOT` - Negate criteria

### TSavedSearch
Saved search definition:
- `ID` - Unique identifier
- `Name` - User-friendly name
- `Description` - Search purpose
- `Query` - Search query string
- `CreatedDate` - When created
- `LastUsed` - Last execution time
- `UseCount` - Usage frequency
- `IsFavorite` - Favorite flag
- `IsQuickFilter` - Quick access flag

### TSearchResult
Search result with metadata:
- `TaskID` - Matching task ID
- `RelevanceScore` - Match quality (0.0-10.0)
- `MatchedFields` - Which fields matched
- `Snippet` - Context excerpt

## Usage Examples

### Quick Search
```pascal
var
  TM: TSearchTaskManager;
  Results: TSearchResultArray;
begin
  TM := TSearchTaskManager.Create;
  try
    // Simple search across all fields
    Results := TM.QuickSearch('urgent');
    
    // Display results
    WriteLn(Format('Found %d tasks', [Length(Results)]));
  finally
    TM.Free;
  end;
end;
```

### Field-Specific Search
```pascal
// Search only in task titles
Results := TM.SearchInField('authentication', sfTitle);

// Search only in categories
Results := TM.SearchInField('Backend', sfCategory);

// Search in tags
Results := TM.SearchInField('security', sfTags);
```

### Advanced Search with Criteria
```pascal
var
  criteria: TSearchCriteria;
  query: TSearchQuery;
begin
  // Setup search criteria
  criteria.Field := sfCategory;
  criteria.ComparisonOp := coContains;
  criteria.Value := 'Backend';
  criteria.CaseSensitive := False;
  
  // Build query
  SetLength(query.Criteria, 1);
  query.Criteria[0] := criteria;
  query.LogicOperator := soAND;
  query.IncludeCompleted := True;
  query.MaxResults := 50;
  
  // Execute search
  Results := TM.AdvancedSearch(query);
end;
```

### Create Saved Search
```pascal
var
  SearchID: Integer;
begin
  // Create a saved search
  SearchID := TM.CreateSavedSearch(
    'High Priority Backend',
    'All high-priority backend tasks',
    'category:backend AND priority:high',
    True  // IsQuickFilter
  );
  
  // Execute saved search
  Results := TM.ExecuteSavedSearch(SearchID);
  
  // Toggle as favorite
  TM.ToggleFavorite(SearchID);
end;
```

### Search History
```pascal
var
  History: TSearchHistoryArray;
  i: Integer;
begin
  // Get recent searches
  History := TM.GetRecentSearches(10);
  
  // Display history
  for i := 0 to High(History) do
  begin
    WriteLn(Format('"%s" - %d results in %d ms',
      [History[i].Query,
       History[i].ResultCount,
       History[i].ExecutionTimeMs]));
  end;
  
  // Clear history
  TM.ClearSearchHistory;
end;
```

### Search Suggestions
```pascal
var
  Suggestions: TSearchSuggestionArray;
  i: Integer;
begin
  // Get suggestions for partial query
  Suggestions := TM.GetSearchSuggestions('auth', 5);
  
  // Display suggestions
  for i := 0 to High(Suggestions) do
    WriteLn(Format('%s (%s)', 
      [Suggestions[i].Text, 
       Suggestions[i].Type_]));
end;
```

### Search Index Management
```pascal
begin
  // Rebuild index for optimal performance
  TM.RebuildSearchIndex;
  
  // Check index status
  WriteLn(TM.GetIndexStatus);
  
  // Disable/enable indexing
  TM.EnableSearchIndex(False);  // Disable
  TM.EnableSearchIndex(True);   // Enable and rebuild
  
  // Optimize index
  TM.OptimizeIndex;
end;
```

### Search Statistics
```pascal
var
  Stats: TSearchStats;
begin
  Stats := TM.GetSearchStats;
  
  WriteLn(Format('Total searches: %d', [Stats.TotalSearches]));
  WriteLn(Format('Avg execution: %.2f ms', [Stats.AverageExecutionTime]));
  WriteLn(Format('Index size: %d entries', [Stats.IndexSize]));
  WriteLn(Format('Popular filters: %d', [Length(Stats.PopularFilters)]));
end;
```

### Text Highlighting
```pascal
var
  highlighted, snippet: string;
begin
  // Highlight matches in text
  highlighted := TM.HighlightMatches(
    'Configure CI/CD pipeline', 
    'CI/CD'
  );
  // Result: "Configure [CI/CD] pipeline"
  
  // Create snippet with context
  snippet := TM.CreateSnippet(
    'Build secure login and registration with OAuth support',
    'OAuth',
    20  // Context characters
  );
  // Result: "...d registration with OAuth support"
end;
```

### Persistence
```pascal
begin
  // Save search data to file
  TM.SaveSearchDataToFile('searches.dat');
  
  // Load search data from file
  TM.LoadSearchDataFromFile('searches.dat');
  
  // Export saved searches as text
  WriteLn(TM.ExportSavedSearches);
end;
```

## Performance Considerations

### Search Index
- **Enabled by default** for fast searching
- **Automatically updated** when tasks change
- **Memory efficient** with normalized content
- **Rebuild periodically** for optimal performance

### Best Practices
1. **Use specific fields** when possible instead of searching all fields
2. **Limit results** to avoid processing too many matches
3. **Enable indexing** for large task lists (>100 tasks)
4. **Save frequently used searches** as quick filters
5. **Clear search history** periodically to save memory

## Integration with Other Features

The Search Engine integrates seamlessly with:
- **Task Management** - Search across all task properties
- **Tags System** - Search by tags with partial matching
- **Categories** - Category-specific searches
- **Team Collaboration** - Search by assignee or creator
- **Audit Trail** - Search historical changes
- **Archives** - Include/exclude archived tasks

## Future Enhancements

Planned features for future versions:
- Regular expression support
- Fuzzy matching for typo tolerance
- Weighted field scoring (customizable)
- Search macros and templates
- Natural language queries
- Search API for external tools
- Advanced boolean syntax parser
- Multi-language support
- Search shortcuts/hotkeys

## API Reference

### Core Search Methods
- `QuickSearch(SearchTerm: string): TSearchResultArray`
- `SearchInField(SearchTerm: string, Field: TSearchField): TSearchResultArray`
- `FullTextSearch(SearchTerm: string): TSearchResultArray`
- `AdvancedSearch(Query: TSearchQuery): TSearchResultArray`
- `SearchWithCriteria(Criteria: TSearchCriteriaArray, LogicOp: TSearchOperator): TSearchResultArray`
- `BooleanSearch(Expression: string): TSearchResultArray`

### Saved Search Methods
- `CreateSavedSearch(Name, Description, Query: string, IsQuickFilter: Boolean): Integer`
- `UpdateSavedSearch(SearchID: Integer, Name, Description, Query: string): Boolean`
- `DeleteSavedSearch(SearchID: Integer): Boolean`
- `GetSavedSearch(SearchID: Integer): TSavedSearch`
- `GetAllSavedSearches: TSavedSearchArray`
- `GetQuickFilters: TSavedSearchArray`
- `GetFavoriteSearches: TSavedSearchArray`
- `ToggleFavorite(SearchID: Integer): Boolean`
- `ExecuteSavedSearch(SearchID: Integer): TSearchResultArray`

### History & Suggestions
- `GetSearchHistory(Days: Integer): TSearchHistoryArray`
- `GetRecentSearches(Count: Integer): TSearchHistoryArray`
- `GetPopularSearches(Count: Integer): TSearchHistoryArray`
- `GetSearchSuggestions(PartialQuery: string, MaxSuggestions: Integer): TSearchSuggestionArray`
- `ClearSearchHistory: Boolean`
- `DeleteHistoryEntry(HistoryID: Integer): Boolean`

### Index Management
- `RebuildSearchIndex`
- `EnableSearchIndex(Enabled: Boolean)`
- `GetIndexStatus: string`
- `OptimizeIndex: Boolean`

### Analytics
- `GetSearchStats: TSearchStats`
- `GetMostSearchedTerms(Count: Integer): TStringArray`
- `GetSearchTrends: string`

### Utilities
- `HighlightMatches(Text, SearchTerm: string): string`
- `CreateSnippet(Text, SearchTerm: string, ContextChars: Integer): string`

### Persistence
- `SaveSearchDataToFile(Filename: string): Boolean`
- `LoadSearchDataFromFile(Filename: string): Boolean`
- `ExportSavedSearches: string`
- `ImportSavedSearches(Data: string): Integer`

## Compilation

```bash
fpc solution16.pas -obin/search_manager -O1 -Mobjfpc
```

## Testing

Run the comprehensive self-test:
```bash
bin/search_manager
```

The test program demonstrates all search engine features including quick search, saved searches, advanced queries, history tracking, suggestions, and index management.

## License

Part of the Free Pascal Task Manager project.
