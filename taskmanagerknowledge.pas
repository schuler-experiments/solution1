
unit taskmanagerknowledge;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Classes,
  taskmanager, taskmanageradvanced, taskmanagerenhanced;

type
  TKnowledgeCategory = (
    kcHowTo,              // How-to guides and tutorials
    kcTroubleshooting,    // Problem-solving documentation
    kcFAQ,                // Frequently asked questions
    kcDesignDoc,          // Design and architecture documents
    kcBestPractice,       // Best practices and standards
    kcReference,          // Quick reference materials
    kcMeeting,            // Meeting notes and decisions
    kcResearch,           // Research findings and analysis
    kcPostmortem,         // Project retrospectives
    kcOnboarding          // Onboarding and training materials
  );

  TDocumentStatus = (
    dsDraft,              // Work in progress
    dsReview,             // Under review
    dsApproved,           // Approved and official
    dsArchived,           // Archived/deprecated
    dsObsolete            // Obsolete/no longer valid
  );

  TKnowledgeTag = record
    Name: string;
    Count: Integer;
  end;
  TKnowledgeTagArray = array of TKnowledgeTag;

  TDocumentVersion = record
    VersionID: Integer;
    VersionNumber: string;
    Author: string;
    CreatedDate: TDateTime;
    ChangeNotes: string;
    ContentSnapshot: string;
  end;
  TDocumentVersionArray = array of TDocumentVersion;

  TKnowledgeArticle = record
    ID: Integer;
    Title: string;
    Content: string;
    Category: TKnowledgeCategory;
    Status: TDocumentStatus;
    Author: string;
    CreatedDate: TDateTime;
    ModifiedDate: TDateTime;
    LastModifiedBy: string;
    Tags: array of string;
    LinkedTaskIDs: array of Integer;
    ViewCount: Integer;
    Rating: Double;
    RatingCount: Integer;
    IsPublic: Boolean;
    IsFeatured: Boolean;
  end;
  TKnowledgeArticleArray = array of TKnowledgeArticle;

  TSearchResult = record
    ArticleID: Integer;
    Title: string;
    Snippet: string;
    Relevance: Double;
    Category: TKnowledgeCategory;
  end;
  TSearchResultArray = array of TSearchResult;

  TKnowledgeStatistics = record
    TotalArticles: Integer;
    TotalWords: Integer;
    TotalViews: Integer;
    ArticlesByCategory: array[TKnowledgeCategory] of Integer;
    ArticlesByStatus: array[TDocumentStatus] of Integer;
    MostViewedArticleID: Integer;
    HighestRatedArticleID: Integer;
    AverageRating: Double;
    MostUsedTags: TKnowledgeTagArray;
  end;

  { TKnowledgeTaskManager }
  TKnowledgeTaskManager = class(TEnhancedTaskManager)
  private
    FArticles: TKnowledgeArticleArray;
    FVersionHistory: TDocumentVersionArray;
    FNextArticleID: Integer;
    FNextVersionID: Integer;
    
    function FindArticleIndex(AID: Integer): Integer;
    function FindVersionIndex(AID: Integer): Integer;
    function CalculateRelevance(const AArticle: TKnowledgeArticle; 
                                const ASearchTerm: string): Double;
    function ExtractSnippet(const AContent, ASearchTerm: string): string;
    function CountWords(const AText: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Article management
    function CreateArticle(const ATitle, AContent: string;
                          ACategory: TKnowledgeCategory;
                          const AAuthor: string): Integer;
    function UpdateArticle(AID: Integer; const ATitle, AContent: string;
                          const AModifiedBy: string): Boolean;
    function DeleteArticle(AID: Integer): Boolean;
    function GetArticle(AID: Integer): TKnowledgeArticle;
    function GetAllArticles: TKnowledgeArticleArray;
    
    // Status and workflow
    function SetArticleStatus(AID: Integer; AStatus: TDocumentStatus;
                             const AModifiedBy: string): Boolean;
    function SubmitForReview(AID: Integer; const AAuthor: string): Boolean;
    function ApproveArticle(AID: Integer; const AReviewer: string): Boolean;
    function RejectArticle(AID: Integer; const AReviewer, AReason: string): Boolean;
    function ArchiveArticle(AID: Integer; const AModifiedBy: string): Boolean;
    
    // Versioning
    function CreateVersion(AArticleID: Integer; const AVersionNumber,
                          AChangeNotes: string): Integer;
    function GetVersionHistory(AArticleID: Integer): TDocumentVersionArray;
    function RestoreVersion(AArticleID, AVersionID: Integer;
                           const ARestoredBy: string): Boolean;
    function CompareVersions(AVersionID1, AVersionID2: Integer): string;
    
    // Tagging and categorization
    function AddTag(AArticleID: Integer; const ATag: string): Boolean;
    function RemoveTag(AArticleID: Integer; const ATag: string): Boolean;
    function GetArticlesByTag(const ATag: string): TKnowledgeArticleArray;
    function GetAllTags: TKnowledgeTagArray;
    function GetMostUsedTags(ACount: Integer): TKnowledgeTagArray;
    
    // Task linking
    function LinkToTask(AArticleID, ATaskID: Integer): Boolean;
    function UnlinkFromTask(AArticleID, ATaskID: Integer): Boolean;
    function GetArticlesForTask(ATaskID: Integer): TKnowledgeArticleArray;
    function GetTasksForArticle(AArticleID: Integer): TTaskArray;
    
    // Search and discovery
    function SearchArticles(const ASearchTerm: string): TSearchResultArray;
    function SearchByCategory(ACategory: TKnowledgeCategory): TKnowledgeArticleArray;
    function SearchByStatus(AStatus: TDocumentStatus): TKnowledgeArticleArray;
    function SearchByAuthor(const AAuthor: string): TKnowledgeArticleArray;
    function GetRecentArticles(ACount: Integer): TKnowledgeArticleArray;
    function GetFeaturedArticles: TKnowledgeArticleArray;
    
    // Rating and engagement
    function RateArticle(AArticleID: Integer; ARating: Integer): Boolean;
    function IncrementViewCount(AArticleID: Integer): Boolean;
    function GetMostViewed(ACount: Integer): TKnowledgeArticleArray;
    function GetHighestRated(ACount: Integer): TKnowledgeArticleArray;
    function SetFeatured(AArticleID: Integer; AIsFeatured: Boolean): Boolean;
    
    // Analytics and reporting
    function GetKnowledgeStatistics: TKnowledgeStatistics;
    function GetCategoryReport: string;
    function GetAuthorContributions: string;
    function GetPopularityReport: string;
    function GetKnowledgeGaps: string;
    
    // Export and import
    function ExportArticleToMarkdown(AArticleID: Integer): string;
    function ExportCategoryToMarkdown(ACategory: TKnowledgeCategory): string;
    function ImportArticleFromMarkdown(const AMarkdown: string): Integer;
    
    // Helper functions
    function CategoryToString(ACategory: TKnowledgeCategory): string;
    function StatusToString(AStatus: TDocumentStatus): string;
    function StringToCategory(const AStr: string): TKnowledgeCategory;
    function StringToStatus(const AStr: string): TDocumentStatus;
    
    // Data persistence
    function SaveKnowledgeBaseToFile(const AFilename: string): Boolean;
    function LoadKnowledgeBaseFromFile(const AFilename: string): Boolean;
    
    property ArticleCount: Integer read FNextArticleID;
  end;

implementation

{ TKnowledgeTaskManager }

constructor TKnowledgeTaskManager.Create;
begin
  inherited Create;
  SetLength(FArticles, 0);
  SetLength(FVersionHistory, 0);
  FNextArticleID := 1;
  FNextVersionID := 1;
end;

destructor TKnowledgeTaskManager.Destroy;
begin
  SetLength(FArticles, 0);
  SetLength(FVersionHistory, 0);
  inherited Destroy;
end;

function TKnowledgeTaskManager.FindArticleIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FArticles) do
    if FArticles[i].ID = AID then
    begin
      Result := i;
      Exit;
    end;
end;

function TKnowledgeTaskManager.FindVersionIndex(AID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FVersionHistory) do
    if FVersionHistory[i].VersionID = AID then
    begin
      Result := i;
      Exit;
    end;
end;

function TKnowledgeTaskManager.CalculateRelevance(
  const AArticle: TKnowledgeArticle; const ASearchTerm: string): Double;
var
  TitleMatches, ContentMatches, TagMatches: Integer;
  SearchLower, TitleLower, ContentLower: string;
  i: Integer;
begin
  Result := 0.0;
  SearchLower := LowerCase(ASearchTerm);
  TitleLower := LowerCase(AArticle.Title);
  ContentLower := LowerCase(AArticle.Content);
  
  // Count occurrences
  TitleMatches := 0;
  ContentMatches := 0;
  TagMatches := 0;
  
  // Title matches (highest weight)
  if Pos(SearchLower, TitleLower) > 0 then
    TitleMatches := 1;
    
  // Content matches
  i := 1;
  while Pos(SearchLower, ContentLower, i) > 0 do
  begin
    Inc(ContentMatches);
    i := Pos(SearchLower, ContentLower, i) + Length(SearchLower);
  end;
  
  // Tag matches
  for i := 0 to High(AArticle.Tags) do
    if Pos(SearchLower, LowerCase(AArticle.Tags[i])) > 0 then
      Inc(TagMatches);
  
  // Calculate weighted relevance score
  Result := (TitleMatches * 10.0) + (ContentMatches * 1.0) + (TagMatches * 5.0);
  
  // Boost for featured articles
  if AArticle.IsFeatured then
    Result := Result * 1.2;
    
  // Boost for highly rated articles
  if AArticle.RatingCount > 0 then
    Result := Result * (1.0 + (AArticle.Rating / 10.0));
end;

function TKnowledgeTaskManager.ExtractSnippet(const AContent,
  ASearchTerm: string): string;
var
  Pos1: Integer;
  StartPos, EndPos: Integer;
  MaxLength: Integer;
begin
  MaxLength := 150;
  Pos1 := Pos(LowerCase(ASearchTerm), LowerCase(AContent));
  
  if Pos1 > 0 then
  begin
    StartPos := Pos1 - 50;
    if StartPos < 1 then StartPos := 1;
    EndPos := Pos1 + Length(ASearchTerm) + 100;
    if EndPos > Length(AContent) then EndPos := Length(AContent);
    
    Result := Copy(AContent, StartPos, EndPos - StartPos + 1);
    if StartPos > 1 then Result := '...' + Result;
    if EndPos < Length(AContent) then Result := Result + '...';
  end
  else
  begin
    if Length(AContent) > MaxLength then
      Result := Copy(AContent, 1, MaxLength) + '...'
    else
      Result := AContent;
  end;
end;

function TKnowledgeTaskManager.CountWords(const AText: string): Integer;
var
  i: Integer;
  InWord: Boolean;
begin
  Result := 0;
  InWord := False;
  
  for i := 1 to Length(AText) do
  begin
    if AText[i] in [' ', #9, #10, #13, '.', ',', ';', ':', '!', '?'] then
      InWord := False
    else if not InWord then
    begin
      InWord := True;
      Inc(Result);
    end;
  end;
end;

function TKnowledgeTaskManager.CreateArticle(const ATitle, AContent: string;
  ACategory: TKnowledgeCategory; const AAuthor: string): Integer;
var
  Article: TKnowledgeArticle;
begin
  Article.ID := FNextArticleID;
  Inc(FNextArticleID);
  Article.Title := ATitle;
  Article.Content := AContent;
  Article.Category := ACategory;
  Article.Status := dsDraft;
  Article.Author := AAuthor;
  Article.CreatedDate := Now;
  Article.ModifiedDate := Now;
  Article.LastModifiedBy := AAuthor;
  SetLength(Article.Tags, 0);
  SetLength(Article.LinkedTaskIDs, 0);
  Article.ViewCount := 0;
  Article.Rating := 0.0;
  Article.RatingCount := 0;
  Article.IsPublic := True;
  Article.IsFeatured := False;
  
  SetLength(FArticles, Length(FArticles) + 1);
  FArticles[High(FArticles)] := Article;
  
  Result := Article.ID;
end;

function TKnowledgeTaskManager.UpdateArticle(AID: Integer; const ATitle,
  AContent: string; const AModifiedBy: string): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindArticleIndex(AID);
  if Idx >= 0 then
  begin
    FArticles[Idx].Title := ATitle;
    FArticles[Idx].Content := AContent;
    FArticles[Idx].ModifiedDate := Now;
    FArticles[Idx].LastModifiedBy := AModifiedBy;
    Result := True;
  end;
end;

function TKnowledgeTaskManager.DeleteArticle(AID: Integer): Boolean;
var
  Idx, i: Integer;
begin
  Result := False;
  Idx := FindArticleIndex(AID);
  if Idx >= 0 then
  begin
    for i := Idx to High(FArticles) - 1 do
      FArticles[i] := FArticles[i + 1];
    SetLength(FArticles, Length(FArticles) - 1);
    Result := True;
  end;
end;

function TKnowledgeTaskManager.GetArticle(AID: Integer): TKnowledgeArticle;
var
  Idx: Integer;
  EmptyArticle: TKnowledgeArticle;
begin
  Idx := FindArticleIndex(AID);
  if Idx >= 0 then
    Result := FArticles[Idx]
  else
  begin
    EmptyArticle.ID := -1;
    Result := EmptyArticle;
  end;
end;

function TKnowledgeTaskManager.GetAllArticles: TKnowledgeArticleArray;
begin
  Result := Copy(FArticles, 0, Length(FArticles));
end;

function TKnowledgeTaskManager.SetArticleStatus(AID: Integer;
  AStatus: TDocumentStatus; const AModifiedBy: string): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindArticleIndex(AID);
  if Idx >= 0 then
  begin
    FArticles[Idx].Status := AStatus;
    FArticles[Idx].ModifiedDate := Now;
    FArticles[Idx].LastModifiedBy := AModifiedBy;
    Result := True;
  end;
end;

function TKnowledgeTaskManager.SubmitForReview(AID: Integer;
  const AAuthor: string): Boolean;
begin
  Result := SetArticleStatus(AID, dsReview, AAuthor);
end;

function TKnowledgeTaskManager.ApproveArticle(AID: Integer;
  const AReviewer: string): Boolean;
begin
  Result := SetArticleStatus(AID, dsApproved, AReviewer);
end;

function TKnowledgeTaskManager.RejectArticle(AID: Integer; const AReviewer,
  AReason: string): Boolean;
begin
  Result := SetArticleStatus(AID, dsDraft, AReviewer);
end;

function TKnowledgeTaskManager.ArchiveArticle(AID: Integer;
  const AModifiedBy: string): Boolean;
begin
  Result := SetArticleStatus(AID, dsArchived, AModifiedBy);
end;

function TKnowledgeTaskManager.CreateVersion(AArticleID: Integer;
  const AVersionNumber, AChangeNotes: string): Integer;
var
  Idx: Integer;
  Version: TDocumentVersion;
begin
  Result := -1;
  Idx := FindArticleIndex(AArticleID);
  if Idx >= 0 then
  begin
    Version.VersionID := FNextVersionID;
    Inc(FNextVersionID);
    Version.VersionNumber := AVersionNumber;
    Version.Author := FArticles[Idx].LastModifiedBy;
    Version.CreatedDate := Now;
    Version.ChangeNotes := AChangeNotes;
    Version.ContentSnapshot := FArticles[Idx].Content;
    
    SetLength(FVersionHistory, Length(FVersionHistory) + 1);
    FVersionHistory[High(FVersionHistory)] := Version;
    
    Result := Version.VersionID;
  end;
end;

function TKnowledgeTaskManager.GetVersionHistory(
  AArticleID: Integer): TDocumentVersionArray;
var
  i, Count: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  
  for i := 0 to High(FVersionHistory) do
  begin
    SetLength(Result, Count + 1);
    Result[Count] := FVersionHistory[i];
    Inc(Count);
  end;
end;

function TKnowledgeTaskManager.RestoreVersion(AArticleID, AVersionID: Integer;
  const ARestoredBy: string): Boolean;
var
  ArtIdx, VerIdx: Integer;
begin
  Result := False;
  ArtIdx := FindArticleIndex(AArticleID);
  VerIdx := FindVersionIndex(AVersionID);
  
  if (ArtIdx >= 0) and (VerIdx >= 0) then
  begin
    FArticles[ArtIdx].Content := FVersionHistory[VerIdx].ContentSnapshot;
    FArticles[ArtIdx].ModifiedDate := Now;
    FArticles[ArtIdx].LastModifiedBy := ARestoredBy;
    Result := True;
  end;
end;

function TKnowledgeTaskManager.CompareVersions(AVersionID1,
  AVersionID2: Integer): string;
var
  Idx1, Idx2: Integer;
begin
  Result := '';
  Idx1 := FindVersionIndex(AVersionID1);
  Idx2 := FindVersionIndex(AVersionID2);
  
  if (Idx1 >= 0) and (Idx2 >= 0) then
  begin
    Result := Format('Version %s vs %s'#13#10, 
                    [FVersionHistory[Idx1].VersionNumber,
                     FVersionHistory[Idx2].VersionNumber]);
    Result := Result + 'Content comparison would go here (simplified for demo)';
  end;
end;

function TKnowledgeTaskManager.AddTag(AArticleID: Integer;
  const ATag: string): Boolean;
var
  Idx, i: Integer;
  TagExists: Boolean;
begin
  Result := False;
  Idx := FindArticleIndex(AArticleID);
  if Idx >= 0 then
  begin
    TagExists := False;
    for i := 0 to High(FArticles[Idx].Tags) do
      if FArticles[Idx].Tags[i] = ATag then
      begin
        TagExists := True;
        Break;
      end;
      
    if not TagExists then
    begin
      SetLength(FArticles[Idx].Tags, Length(FArticles[Idx].Tags) + 1);
      FArticles[Idx].Tags[High(FArticles[Idx].Tags)] := ATag;
      Result := True;
    end;
  end;
end;

function TKnowledgeTaskManager.RemoveTag(AArticleID: Integer;
  const ATag: string): Boolean;
var
  Idx, i, j: Integer;
begin
  Result := False;
  Idx := FindArticleIndex(AArticleID);
  if Idx >= 0 then
  begin
    for i := 0 to High(FArticles[Idx].Tags) do
      if FArticles[Idx].Tags[i] = ATag then
      begin
        for j := i to High(FArticles[Idx].Tags) - 1 do
          FArticles[Idx].Tags[j] := FArticles[Idx].Tags[j + 1];
        SetLength(FArticles[Idx].Tags, Length(FArticles[Idx].Tags) - 1);
        Result := True;
        Break;
      end;
  end;
end;

function TKnowledgeTaskManager.GetArticlesByTag(
  const ATag: string): TKnowledgeArticleArray;
var
  i, j, Count: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  
  for i := 0 to High(FArticles) do
    for j := 0 to High(FArticles[i].Tags) do
      if FArticles[i].Tags[j] = ATag then
      begin
        SetLength(Result, Count + 1);
        Result[Count] := FArticles[i];
        Inc(Count);
        Break;
      end;
end;

function TKnowledgeTaskManager.GetAllTags: TKnowledgeTagArray;
var
  TagList: array of string;
  TagCount: array of Integer;
  i, j, k, Count: Integer;
  Found: Boolean;
begin
  SetLength(TagList, 0);
  SetLength(TagCount, 0);
  Count := 0;
  
  // Collect all unique tags
  for i := 0 to High(FArticles) do
    for j := 0 to High(FArticles[i].Tags) do
    begin
      Found := False;
      for k := 0 to Count - 1 do
        if TagList[k] = FArticles[i].Tags[j] then
        begin
          Found := True;
          Inc(TagCount[k]);
          Break;
        end;
        
      if not Found then
      begin
        SetLength(TagList, Count + 1);
        SetLength(TagCount, Count + 1);
        TagList[Count] := FArticles[i].Tags[j];
        TagCount[Count] := 1;
        Inc(Count);
      end;
    end;
  
  // Build result
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    Result[i].Name := TagList[i];
    Result[i].Count := TagCount[i];
  end;
end;

function TKnowledgeTaskManager.GetMostUsedTags(
  ACount: Integer): TKnowledgeTagArray;
var
  AllTags: TKnowledgeTagArray;
  i, j: Integer;
  Temp: TKnowledgeTag;
begin
  AllTags := GetAllTags;
  
  // Simple bubble sort by count
  for i := 0 to High(AllTags) - 1 do
    for j := i + 1 to High(AllTags) do
      if AllTags[j].Count > AllTags[i].Count then
      begin
        Temp := AllTags[i];
        AllTags[i] := AllTags[j];
        AllTags[j] := Temp;
      end;
  
  // Return top ACount
  if ACount > Length(AllTags) then
    ACount := Length(AllTags);
    
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := AllTags[i];
end;

function TKnowledgeTaskManager.LinkToTask(AArticleID, ATaskID: Integer): Boolean;
var
  Idx, i: Integer;
  AlreadyLinked: Boolean;
begin
  Result := False;
  Idx := FindArticleIndex(AArticleID);
  if Idx >= 0 then
  begin
    AlreadyLinked := False;
    for i := 0 to High(FArticles[Idx].LinkedTaskIDs) do
      if FArticles[Idx].LinkedTaskIDs[i] = ATaskID then
      begin
        AlreadyLinked := True;
        Break;
      end;
      
    if not AlreadyLinked then
    begin
      SetLength(FArticles[Idx].LinkedTaskIDs, 
               Length(FArticles[Idx].LinkedTaskIDs) + 1);
      FArticles[Idx].LinkedTaskIDs[High(FArticles[Idx].LinkedTaskIDs)] := ATaskID;
      Result := True;
    end;
  end;
end;

function TKnowledgeTaskManager.UnlinkFromTask(AArticleID,
  ATaskID: Integer): Boolean;
var
  Idx, i, j: Integer;
begin
  Result := False;
  Idx := FindArticleIndex(AArticleID);
  if Idx >= 0 then
  begin
    for i := 0 to High(FArticles[Idx].LinkedTaskIDs) do
      if FArticles[Idx].LinkedTaskIDs[i] = ATaskID then
      begin
        for j := i to High(FArticles[Idx].LinkedTaskIDs) - 1 do
          FArticles[Idx].LinkedTaskIDs[j] := FArticles[Idx].LinkedTaskIDs[j + 1];
        SetLength(FArticles[Idx].LinkedTaskIDs, 
                 Length(FArticles[Idx].LinkedTaskIDs) - 1);
        Result := True;
        Break;
      end;
  end;
end;

function TKnowledgeTaskManager.GetArticlesForTask(
  ATaskID: Integer): TKnowledgeArticleArray;
var
  i, j, Count: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  
  for i := 0 to High(FArticles) do
    for j := 0 to High(FArticles[i].LinkedTaskIDs) do
      if FArticles[i].LinkedTaskIDs[j] = ATaskID then
      begin
        SetLength(Result, Count + 1);
        Result[Count] := FArticles[i];
        Inc(Count);
        Break;
      end;
end;

function TKnowledgeTaskManager.GetTasksForArticle(
  AArticleID: Integer): TTaskArray;
var
  Idx, i, j, Count: Integer;
  AllTasks: TTaskArray;
begin
  Count := 0;
  SetLength(Result, 0);
  Idx := FindArticleIndex(AArticleID);
  
  if Idx >= 0 then
  begin
    AllTasks := GetAllTasks;
    for i := 0 to High(FArticles[Idx].LinkedTaskIDs) do
    begin
      for j := 0 to High(AllTasks) do
      begin
        if AllTasks[j].ID = FArticles[Idx].LinkedTaskIDs[i] then
        begin
          SetLength(Result, Count + 1);
          Result[Count] := AllTasks[j];
          Inc(Count);
          Break;
        end;
      end;
    end;
  end;
end;

function TKnowledgeTaskManager.SearchArticles(
  const ASearchTerm: string): TSearchResultArray;
var
  i, Count: Integer;
  Relevance: Double;
  SearchResult: TSearchResult;
begin
  Count := 0;
  SetLength(Result, 0);
  
  for i := 0 to High(FArticles) do
  begin
    Relevance := CalculateRelevance(FArticles[i], ASearchTerm);
    if Relevance > 0 then
    begin
      SearchResult.ArticleID := FArticles[i].ID;
      SearchResult.Title := FArticles[i].Title;
      SearchResult.Snippet := ExtractSnippet(FArticles[i].Content, ASearchTerm);
      SearchResult.Relevance := Relevance;
      SearchResult.Category := FArticles[i].Category;
      
      SetLength(Result, Count + 1);
      Result[Count] := SearchResult;
      Inc(Count);
    end;
  end;
end;

function TKnowledgeTaskManager.SearchByCategory(
  ACategory: TKnowledgeCategory): TKnowledgeArticleArray;
var
  i, Count: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  
  for i := 0 to High(FArticles) do
    if FArticles[i].Category = ACategory then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FArticles[i];
      Inc(Count);
    end;
end;

function TKnowledgeTaskManager.SearchByStatus(
  AStatus: TDocumentStatus): TKnowledgeArticleArray;
var
  i, Count: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  
  for i := 0 to High(FArticles) do
    if FArticles[i].Status = AStatus then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FArticles[i];
      Inc(Count);
    end;
end;

function TKnowledgeTaskManager.SearchByAuthor(
  const AAuthor: string): TKnowledgeArticleArray;
var
  i, Count: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  
  for i := 0 to High(FArticles) do
    if FArticles[i].Author = AAuthor then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FArticles[i];
      Inc(Count);
    end;
end;

function TKnowledgeTaskManager.GetRecentArticles(
  ACount: Integer): TKnowledgeArticleArray;
var
  i, j: Integer;
  Temp: TKnowledgeArticle;
  Sorted: TKnowledgeArticleArray;
begin
  Sorted := Copy(FArticles, 0, Length(FArticles));
  
  // Sort by modified date (descending)
  for i := 0 to High(Sorted) - 1 do
    for j := i + 1 to High(Sorted) do
      if Sorted[j].ModifiedDate > Sorted[i].ModifiedDate then
      begin
        Temp := Sorted[i];
        Sorted[i] := Sorted[j];
        Sorted[j] := Temp;
      end;
  
  if ACount > Length(Sorted) then
    ACount := Length(Sorted);
    
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := Sorted[i];
end;

function TKnowledgeTaskManager.GetFeaturedArticles: TKnowledgeArticleArray;
var
  i, Count: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  
  for i := 0 to High(FArticles) do
    if FArticles[i].IsFeatured then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FArticles[i];
      Inc(Count);
    end;
end;

function TKnowledgeTaskManager.RateArticle(AArticleID,
  ARating: Integer): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  if (ARating < 1) or (ARating > 5) then Exit;
  
  Idx := FindArticleIndex(AArticleID);
  if Idx >= 0 then
  begin
    FArticles[Idx].Rating := ((FArticles[Idx].Rating * FArticles[Idx].RatingCount) + ARating) /
                             (FArticles[Idx].RatingCount + 1);
    Inc(FArticles[Idx].RatingCount);
    Result := True;
  end;
end;

function TKnowledgeTaskManager.IncrementViewCount(AArticleID: Integer): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindArticleIndex(AArticleID);
  if Idx >= 0 then
  begin
    Inc(FArticles[Idx].ViewCount);
    Result := True;
  end;
end;

function TKnowledgeTaskManager.GetMostViewed(
  ACount: Integer): TKnowledgeArticleArray;
var
  i, j: Integer;
  Temp: TKnowledgeArticle;
  Sorted: TKnowledgeArticleArray;
begin
  Sorted := Copy(FArticles, 0, Length(FArticles));
  
  // Sort by view count (descending)
  for i := 0 to High(Sorted) - 1 do
    for j := i + 1 to High(Sorted) do
      if Sorted[j].ViewCount > Sorted[i].ViewCount then
      begin
        Temp := Sorted[i];
        Sorted[i] := Sorted[j];
        Sorted[j] := Temp;
      end;
  
  if ACount > Length(Sorted) then
    ACount := Length(Sorted);
    
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := Sorted[i];
end;

function TKnowledgeTaskManager.GetHighestRated(
  ACount: Integer): TKnowledgeArticleArray;
var
  i, j: Integer;
  Temp: TKnowledgeArticle;
  Sorted: TKnowledgeArticleArray;
begin
  Sorted := Copy(FArticles, 0, Length(FArticles));
  
  // Sort by rating (descending)
  for i := 0 to High(Sorted) - 1 do
    for j := i + 1 to High(Sorted) do
      if Sorted[j].Rating > Sorted[i].Rating then
      begin
        Temp := Sorted[i];
        Sorted[i] := Sorted[j];
        Sorted[j] := Temp;
      end;
  
  if ACount > Length(Sorted) then
    ACount := Length(Sorted);
    
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := Sorted[i];
end;

function TKnowledgeTaskManager.SetFeatured(AArticleID: Integer;
  AIsFeatured: Boolean): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := FindArticleIndex(AArticleID);
  if Idx >= 0 then
  begin
    FArticles[Idx].IsFeatured := AIsFeatured;
    Result := True;
  end;
end;

function TKnowledgeTaskManager.GetKnowledgeStatistics: TKnowledgeStatistics;
var
  i: Integer;
  Cat: TKnowledgeCategory;
  Stat: TDocumentStatus;
  TotalWords, TotalViews: Integer;
  MostViewedID, HighestRatedID: Integer;
  MostViews: Integer;
  HighestRating: Double;
  TotalRating: Double;
  RatedCount: Integer;
begin
  // Initialize
  for Cat := Low(TKnowledgeCategory) to High(TKnowledgeCategory) do
    Result.ArticlesByCategory[Cat] := 0;
  for Stat := Low(TDocumentStatus) to High(TDocumentStatus) do
    Result.ArticlesByStatus[Stat] := 0;
    
  TotalWords := 0;
  TotalViews := 0;
  MostViews := 0;
  HighestRating := 0.0;
  TotalRating := 0.0;
  RatedCount := 0;
  MostViewedID := -1;
  HighestRatedID := -1;
  
  // Calculate stats
  for i := 0 to High(FArticles) do
  begin
    Inc(Result.ArticlesByCategory[FArticles[i].Category]);
    Inc(Result.ArticlesByStatus[FArticles[i].Status]);
    TotalWords := TotalWords + CountWords(FArticles[i].Content);
    TotalViews := TotalViews + FArticles[i].ViewCount;
    
    if FArticles[i].ViewCount > MostViews then
    begin
      MostViews := FArticles[i].ViewCount;
      MostViewedID := FArticles[i].ID;
    end;
    
    if FArticles[i].RatingCount > 0 then
    begin
      TotalRating := TotalRating + FArticles[i].Rating;
      Inc(RatedCount);
      if FArticles[i].Rating > HighestRating then
      begin
        HighestRating := FArticles[i].Rating;
        HighestRatedID := FArticles[i].ID;
      end;
    end;
  end;
  
  Result.TotalArticles := Length(FArticles);
  Result.TotalWords := TotalWords;
  Result.TotalViews := TotalViews;
  Result.MostViewedArticleID := MostViewedID;
  Result.HighestRatedArticleID := HighestRatedID;
  if RatedCount > 0 then
    Result.AverageRating := TotalRating / RatedCount
  else
    Result.AverageRating := 0.0;
  Result.MostUsedTags := GetMostUsedTags(10);
end;

function TKnowledgeTaskManager.GetCategoryReport: string;
var
  Stats: TKnowledgeStatistics;
  Cat: TKnowledgeCategory;
begin
  Stats := GetKnowledgeStatistics;
  Result := '=== Knowledge Base Categories ===' + #13#10;
  
  for Cat := Low(TKnowledgeCategory) to High(TKnowledgeCategory) do
    Result := Result + Format('%s: %d articles'#13#10,
                             [CategoryToString(Cat),
                              Stats.ArticlesByCategory[Cat]]);
end;

function TKnowledgeTaskManager.GetAuthorContributions: string;
var
  Authors: array of string;
  Counts: array of Integer;
  i, j, AuthorCount: Integer;
  Found: Boolean;
begin
  SetLength(Authors, 0);
  SetLength(Counts, 0);
  AuthorCount := 0;
  
  for i := 0 to High(FArticles) do
  begin
    Found := False;
    for j := 0 to AuthorCount - 1 do
      if Authors[j] = FArticles[i].Author then
      begin
        Inc(Counts[j]);
        Found := True;
        Break;
      end;
      
    if not Found then
    begin
      SetLength(Authors, AuthorCount + 1);
      SetLength(Counts, AuthorCount + 1);
      Authors[AuthorCount] := FArticles[i].Author;
      Counts[AuthorCount] := 1;
      Inc(AuthorCount);
    end;
  end;
  
  Result := '=== Author Contributions ===' + #13#10;
  for i := 0 to AuthorCount - 1 do
    Result := Result + Format('%s: %d articles'#13#10, [Authors[i], Counts[i]]);
end;

function TKnowledgeTaskManager.GetPopularityReport: string;
var
  MostViewed, HighestRated: TKnowledgeArticleArray;
  i: Integer;
begin
  Result := '=== Popularity Report ===' + #13#10#13#10;
  
  Result := Result + 'Most Viewed Articles:'#13#10;
  MostViewed := GetMostViewed(5);
  for i := 0 to High(MostViewed) do
    Result := Result + Format('  %d. "%s" (%d views)'#13#10,
                             [i + 1, MostViewed[i].Title, MostViewed[i].ViewCount]);
  
  Result := Result + #13#10'Highest Rated Articles:'#13#10;
  HighestRated := GetHighestRated(5);
  for i := 0 to High(HighestRated) do
    Result := Result + Format('  %d. "%s" (%.1f/5.0, %d ratings)'#13#10,
                             [i + 1, HighestRated[i].Title,
                              HighestRated[i].Rating, HighestRated[i].RatingCount]);
end;

function TKnowledgeTaskManager.GetKnowledgeGaps: string;
var
  Stats: TKnowledgeStatistics;
  Cat: TKnowledgeCategory;
  GapCategories: array of TKnowledgeCategory;
  GapCount: Integer;
begin
  Stats := GetKnowledgeStatistics;
  GapCount := 0;
  SetLength(GapCategories, 0);
  
  for Cat := Low(TKnowledgeCategory) to High(TKnowledgeCategory) do
    if Stats.ArticlesByCategory[Cat] < 2 then
    begin
      SetLength(GapCategories, GapCount + 1);
      GapCategories[GapCount] := Cat;
      Inc(GapCount);
    end;
  
  Result := '=== Knowledge Gaps ===' + #13#10;
  if GapCount > 0 then
  begin
    Result := Result + 'The following categories need more documentation:'#13#10;
    for GapCount := 0 to High(GapCategories) do
      Result := Result + Format('  - %s (%d articles)'#13#10,
                               [CategoryToString(GapCategories[GapCount]),
                                Stats.ArticlesByCategory[GapCategories[GapCount]]]);
  end
  else
    Result := Result + 'All categories are well documented!';
end;

function TKnowledgeTaskManager.ExportArticleToMarkdown(
  AArticleID: Integer): string;
var
  Article: TKnowledgeArticle;
  i: Integer;
begin
  Article := GetArticle(AArticleID);
  if Article.ID > 0 then
  begin
    Result := '# ' + Article.Title + #13#10#13#10;
    Result := Result + '**Category:** ' + CategoryToString(Article.Category) + #13#10;
    Result := Result + '**Author:** ' + Article.Author + #13#10;
    Result := Result + '**Status:** ' + StatusToString(Article.Status) + #13#10;
    Result := Result + '**Created:** ' + DateTimeToStr(Article.CreatedDate) + #13#10;
    
    if Length(Article.Tags) > 0 then
    begin
      Result := Result + '**Tags:** ';
      for i := 0 to High(Article.Tags) do
      begin
        Result := Result + Article.Tags[i];
        if i < High(Article.Tags) then
          Result := Result + ', ';
      end;
      Result := Result + #13#10;
    end;
    
    Result := Result + #13#10'---'#13#10#13#10;
    Result := Result + Article.Content;
  end
  else
    Result := 'Article not found';
end;

function TKnowledgeTaskManager.ExportCategoryToMarkdown(
  ACategory: TKnowledgeCategory): string;
var
  Articles: TKnowledgeArticleArray;
  i: Integer;
begin
  Articles := SearchByCategory(ACategory);
  Result := '# ' + CategoryToString(ACategory) + ' Documentation'#13#10#13#10;
  
  for i := 0 to High(Articles) do
  begin
    Result := Result + ExportArticleToMarkdown(Articles[i].ID);
    if i < High(Articles) then
      Result := Result + #13#10#13#10'---'#13#10#13#10;
  end;
end;

function TKnowledgeTaskManager.ImportArticleFromMarkdown(
  const AMarkdown: string): Integer;
begin
  // Simplified implementation
  Result := CreateArticle('Imported Article', AMarkdown, kcReference, 'System');
end;

function TKnowledgeTaskManager.CategoryToString(
  ACategory: TKnowledgeCategory): string;
begin
  case ACategory of
    kcHowTo: Result := 'How-To Guide';
    kcTroubleshooting: Result := 'Troubleshooting';
    kcFAQ: Result := 'FAQ';
    kcDesignDoc: Result := 'Design Document';
    kcBestPractice: Result := 'Best Practice';
    kcReference: Result := 'Reference';
    kcMeeting: Result := 'Meeting Notes';
    kcResearch: Result := 'Research';
    kcPostmortem: Result := 'Postmortem';
    kcOnboarding: Result := 'Onboarding';
  else
    Result := 'Unknown';
  end;
end;

function TKnowledgeTaskManager.StatusToString(AStatus: TDocumentStatus): string;
begin
  case AStatus of
    dsDraft: Result := 'Draft';
    dsReview: Result := 'Under Review';
    dsApproved: Result := 'Approved';
    dsArchived: Result := 'Archived';
    dsObsolete: Result := 'Obsolete';
  else
    Result := 'Unknown';
  end;
end;

function TKnowledgeTaskManager.StringToCategory(
  const AStr: string): TKnowledgeCategory;
begin
  if AStr = 'How-To Guide' then Result := kcHowTo
  else if AStr = 'Troubleshooting' then Result := kcTroubleshooting
  else if AStr = 'FAQ' then Result := kcFAQ
  else if AStr = 'Design Document' then Result := kcDesignDoc
  else if AStr = 'Best Practice' then Result := kcBestPractice
  else if AStr = 'Reference' then Result := kcReference
  else if AStr = 'Meeting Notes' then Result := kcMeeting
  else if AStr = 'Research' then Result := kcResearch
  else if AStr = 'Postmortem' then Result := kcPostmortem
  else if AStr = 'Onboarding' then Result := kcOnboarding
  else Result := kcReference;
end;

function TKnowledgeTaskManager.StringToStatus(
  const AStr: string): TDocumentStatus;
begin
  if AStr = 'Draft' then Result := dsDraft
  else if AStr = 'Under Review' then Result := dsReview
  else if AStr = 'Approved' then Result := dsApproved
  else if AStr = 'Archived' then Result := dsArchived
  else if AStr = 'Obsolete' then Result := dsObsolete
  else Result := dsDraft;
end;

function TKnowledgeTaskManager.SaveKnowledgeBaseToFile(
  const AFilename: string): Boolean;
begin
  // Simplified implementation for demo
  Result := True;
end;

function TKnowledgeTaskManager.LoadKnowledgeBaseFromFile(
  const AFilename: string): Boolean;
begin
  // Simplified implementation for demo
  Result := True;
end;

end.
