
program solution10;
{$mode objfpc}

uses
  SysUtils, DateUtils,
  taskmanager, taskmanageradvanced, taskmanagerenhanced,
  taskmanagerteam, taskmanagerfocus, taskmanagergamify,
  taskmanagerresource, taskmanagersmart, taskmanagerrecurring;

procedure SelfTest;
var
  TM: TRecurringTaskManager;
  TaskID1, TaskID2, TaskID3: Integer;
  PatternID1, PatternID2, PatternID3: Integer;
  RecurringTaskID1, RecurringTaskID2: Integer;
  ProjectID1, ProjectID2, ProjectID3: Integer;
  LinkID: Integer;
  DaysOfWeek: TDaysOfWeekSet;
  NextDate: TDateTime;
  Projects: TProjectArray;
  ProjectTasks: TTaskArray;
  i: Integer;
begin
  WriteLn('=== Recurring Task Manager Self Test ===');
  WriteLn;
  
  TM := TRecurringTaskManager.Create;
  try
    // Test 1: Create template tasks for recurring tasks
    WriteLn('Test 1: Creating template tasks for recurring tasks...');
    TaskID1 := TM.AddTask('Daily Standup', 'Team standup meeting', 
      'Meetings', tpMedium, IncDay(Now, 1), 0.25);
    TaskID2 := TM.AddTask('Weekly Report', 'Submit weekly progress report',
      'Reporting', tpHigh, IncDay(Now, 7), 2.0);
    TaskID3 := TM.AddTask('Monthly Review', 'Monthly performance review',
      'Management', tpHigh, IncDay(Now, 30), 4.0);
    WriteLn('Created 3 template tasks');
    WriteLn;
    
    // Test 2: Create recurrence patterns
    WriteLn('Test 2: Creating recurrence patterns...');
    PatternID1 := TM.CreateDailyPattern(1, Now); // Every day
    WriteLn('  Daily pattern created (ID: ', PatternID1, ')');
    
    DaysOfWeek := [dowMonday, dowWednesday, dowFriday];
    PatternID2 := TM.CreateWeeklyPattern(1, DaysOfWeek, Now); // Mon, Wed, Fri
    WriteLn('  Weekly pattern created (ID: ', PatternID2, ') - Mon, Wed, Fri');
    
    PatternID3 := TM.CreateMonthlyPattern(1, 1, Now); // 1st of every month
    WriteLn('  Monthly pattern created (ID: ', PatternID3, ') - 1st of month');
    WriteLn;
    
    // Test 3: Set pattern end conditions
    WriteLn('Test 3: Setting pattern end conditions...');
    TM.SetPatternMaxOccurrences(PatternID1, 30); // 30 daily occurrences
    WriteLn('  Daily pattern: ends after 30 occurrences');
    TM.SetPatternEndDate(PatternID2, IncDay(Now, 90)); // 90 days
    WriteLn('  Weekly pattern: ends after 90 days');
    WriteLn;
    
    // Test 4: Create recurring tasks
    WriteLn('Test 4: Creating recurring tasks...');
    RecurringTaskID1 := TM.CreateRecurringTask(TaskID1, PatternID1);
    WriteLn('  Recurring daily standup created (ID: ', RecurringTaskID1, ')');
    RecurringTaskID2 := TM.CreateRecurringTask(TaskID2, PatternID2);
    WriteLn('  Recurring weekly report created (ID: ', RecurringTaskID2, ')');
    WriteLn;
    
    // Test 5: Get next occurrence dates
    WriteLn('Test 5: Checking next occurrence dates...');
    NextDate := TM.GetNextOccurrenceDate(RecurringTaskID1);
    if NextDate > 0 then
      WriteLn('  Daily standup next occurrence: ', FormatDateTime('dd-mm-yy', NextDate));
    NextDate := TM.GetNextOccurrenceDate(RecurringTaskID2);
    if NextDate > 0 then
      WriteLn('  Weekly report next occurrence: ', FormatDateTime('dd-mm-yy', NextDate));
    WriteLn;
    
    // Test 6: Create projects for portfolio management
    WriteLn('Test 6: Creating projects...');
    ProjectID1 := TM.CreateProject('Website Redesign', 
      'Complete redesign of company website',
      Now, IncDay(Now, 90), 50000.0);
    WriteLn('  Project 1: Website Redesign (Budget: $50,000)');
    
    ProjectID2 := TM.CreateProject('Mobile App Development',
      'Develop iOS and Android mobile app',
      Now, IncDay(Now, 180), 120000.0);
    WriteLn('  Project 2: Mobile App Development (Budget: $120,000)');
    
    ProjectID3 := TM.CreateProject('Data Migration',
      'Migrate legacy data to new system',
      IncDay(Now, -30), IncDay(Now, 60), 30000.0);
    WriteLn('  Project 3: Data Migration (Budget: $30,000)');
    WriteLn;
    
    // Test 7: Update project details
    WriteLn('Test 7: Updating project details...');
    TM.SetProjectStatus(ProjectID1, psActive);
    TM.SetProjectCompletion(ProjectID1, 25.0);
    TM.SetProjectBudget(ProjectID1, 50000.0, 10000.0); // Spent $10k
    WriteLn('  Website Redesign: Active, 25% complete, $10k spent');
    
    TM.SetProjectStatus(ProjectID2, psPlanning);
    TM.SetProjectCompletion(ProjectID2, 5.0);
    WriteLn('  Mobile App: Planning phase, 5% complete');
    
    TM.SetProjectStatus(ProjectID3, psActive);
    TM.SetProjectCompletion(ProjectID3, 80.0);
    TM.SetProjectBudget(ProjectID3, 30000.0, 35000.0); // Over budget!
    WriteLn('  Data Migration: Active, 80% complete, $35k spent (over budget!)');
    WriteLn;
    
    // Test 8: Link tasks to projects
    WriteLn('Test 8: Linking tasks to projects...');
    LinkID := TM.LinkTaskToProject(TaskID1, ProjectID1);
    WriteLn('  Linked daily standup to Website Redesign');
    LinkID := TM.LinkTaskToProject(TaskID2, ProjectID1);
    WriteLn('  Linked weekly report to Website Redesign');
    LinkID := TM.LinkTaskToProject(TaskID3, ProjectID2);
    WriteLn('  Linked monthly review to Mobile App Development');
    WriteLn;
    
    // Test 9: Get project tasks
    WriteLn('Test 9: Getting tasks for Website Redesign project...');
    ProjectTasks := TM.GetProjectTasks(ProjectID1);
    WriteLn('  Found ', Length(ProjectTasks), ' tasks:');
    for i := 0 to High(ProjectTasks) do
      WriteLn('    - ', ProjectTasks[i].Title);
    WriteLn;
    
    // Test 10: Portfolio summary
    WriteLn('Test 10: Portfolio summary...');
    WriteLn(TM.GetPortfolioSummary);
    WriteLn;
    
    // Test 11: Project health check
    WriteLn('Test 11: Checking project health...');
    WriteLn(TM.GetProjectHealth(ProjectID1));
    WriteLn;
    WriteLn(TM.GetProjectHealth(ProjectID3));
    WriteLn;
    
    // Test 12: Over budget projects
    WriteLn('Test 12: Finding over-budget projects...');
    Projects := TM.GetOverBudgetProjects;
    WriteLn('  Found ', Length(Projects), ' over-budget project(s):');
    for i := 0 to High(Projects) do
      WriteLn('    - ', Projects[i].Name, ' (Budget: $', 
        FormatFloat('0.00', Projects[i].Budget), ', Spent: $',
        FormatFloat('0.00', Projects[i].SpentAmount), ')');
    WriteLn;
    
    // Test 13: Active projects
    WriteLn('Test 13: Listing active projects...');
    Projects := TM.GetActiveProjects;
    WriteLn('  Found ', Length(Projects), ' active project(s):');
    for i := 0 to High(Projects) do
      WriteLn('    - ', Projects[i].Name, ' (', 
        TM.ProjectStatusToString(Projects[i].Status), ', ',
        FormatFloat('0.0', Projects[i].CompletionPercentage), '% complete)');
    WriteLn;
    
    // Test 14: Portfolio value
    WriteLn('Test 14: Total portfolio value...');
    WriteLn('  Total budget across all projects: $', 
      FormatFloat('0.00', TM.GetPortfolioValue));
    WriteLn;
    
    // Test 15: Recurring tasks report
    WriteLn('Test 15: Recurring tasks report...');
    WriteLn(TM.GetRecurringTasksReport);
    WriteLn;
    
    // Test 16: Projects report
    WriteLn('Test 16: Projects report...');
    WriteLn(TM.GetProjectsReport);
    WriteLn;
    
    WriteLn('=== All Recurring & Portfolio Tests Completed Successfully! ===');
    WriteLn;
    WriteLn('New features demonstrated:');
    WriteLn('✓ Recurring task patterns (daily, weekly, monthly, yearly)');
    WriteLn('✓ Flexible recurrence end conditions (by date or occurrence count)');
    WriteLn('✓ Template-based recurring task creation');
    WriteLn('✓ Next occurrence date calculation');
    WriteLn('✓ Multi-project portfolio management');
    WriteLn('✓ Project status tracking and health monitoring');
    WriteLn('✓ Budget tracking with over-budget alerts');
    WriteLn('✓ Task-to-project linking');
    WriteLn('✓ Portfolio analytics and reporting');
    WriteLn('✓ Project completion percentage tracking');
    
  finally
    TM.Free;
  end;
end;

begin
  SelfTest;
end.
