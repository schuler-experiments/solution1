
# Changelog

All notable changes to the Task Manager project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.0.0] - 2025-12-22

### Added

#### Categories/Projects Management
- Added `Category` field to tasks for organizing tasks into projects or categories
- New `SetTaskCategory()` method to assign categories to tasks
- New `GetTasksByCategory()` method to filter tasks by category
- Category filtering in search criteria
- `GetCategoryStatistics()` method for per-category statistics
- Category statistics include total, active, completed tasks and time tracking per category

#### Task Dependencies
- Added `DependsOnIDs` field to track prerequisite tasks
- New `AddTaskDependency()` method to create dependency relationships
- New `RemoveTaskDependency()` method to remove dependencies
- New `GetTaskDependencies()` method to retrieve all prerequisite tasks
- New `CanStartTask()` method to check if all dependencies are completed
- Dependency helper functions in TaskTypes:
  - `HasDependencies()` - Check if task has any dependencies
  - `GetDependencyIDs()` - Parse dependency IDs into array
  - `AddDependency()` - Add dependency to task record
  - `RemoveDependency()` - Remove dependency from task record

#### Time Tracking
- Added `EstimatedHours` field for planning task duration
- Added `ActualHours` field for tracking time spent
- New `SetEstimatedHours()` method
- New `SetActualHours()` method
- New `AddActualHours()` method for incremental time tracking
- Time statistics in overall statistics (total estimated and actual hours)
- Time statistics per category

#### Audit Trail & History
- New `TaskHistory.pas` unit for complete change tracking
- Added `THistoryEntry` record type for audit trail
- New `TTaskHistory` class with methods:
  - `LogChange()` - Log generic field changes
  - `LogTaskCreated()` - Log task creation
  - `LogTaskDeleted()` - Log task deletion
  - `LogStatusChange()` - Log status changes
  - `LogPriorityChange()` - Log priority changes
  - `GetTaskHistory()` - Get history for specific task
  - `GetRecentHistory()` - Get N most recent changes
  - `SaveHistory()` / `LoadHistory()` - Persist history to file
- History stored in separate binary file (`tasks_history.dat`)
- Configurable maximum history entries (default 1000)

#### Advanced Export Formats
- New `TaskExport.pas` unit for enhanced export capabilities
- **JSON Export**: 
  - Structured JSON output with proper escaping
  - Version and metadata included
  - All task fields exported
- **HTML Export**:
  - Beautiful styled tables with CSS
  - Color-coded priorities (Low=green, Medium=orange, High=red, Critical=bold red)
  - Status highlighting (Completed=green, Cancelled=strikethrough, InProgress=blue)
  - Category badges with styling
  - Responsive design
- **Markdown Export**:
  - GitHub/GitLab compatible tables
  - All fields included
  - Easy to read and edit
- **HTML Reports**:
  - Comprehensive reports with overall statistics
  - Category statistics tables
  - Full task listing with styling
  - Metadata and generation timestamp

#### Enhanced Data Model
- Added `LastModifiedDate` field to track when tasks were last changed
- All update operations now set `LastModifiedDate`
- Enhanced `TTaskStatistics` record with:
  - `TotalEstimatedHours`
  - `TotalActualHours`
  - `TasksWithDependencies`
- New `TCategoryStatistics` record type
- New `TCategoryStatisticsArray` type
- Helper types: `TIntegerArray`, `TStringArray`

#### Storage Improvements
- Versioned storage format (v2) for future compatibility
- Backward compatible with v1 format files
- Enhanced CSV export with all new fields
- Enhanced text export with all new fields
- Version number stored in binary file

### Changed

#### TaskManager.pas
- `AddTask()` now has overloaded version accepting category and estimated hours
- All task modifications now update `LastModifiedDate`
- `GetStatistics()` now includes time tracking and dependency statistics
- Enhanced search to support category filtering
- High priority count now includes both High and Critical priorities

#### TaskStorage.pas
- Binary storage format upgraded to v2
- Added version detection for backward compatibility
- CSV export now includes: Category, Dependencies, EstHours, ActualHours, LastModified
- Text export now includes all new fields with formatted output

#### TaskManagerMain.pas
- Completely rewritten self-test suite
- Added tests for all new features:
  - Category management
  - Dependency tracking
  - Time tracking
  - History logging and retrieval
  - JSON, HTML, Markdown exports
  - Comprehensive reports
  - Category statistics
- Enhanced output formatting
- Added `PrintCategoryStatistics()` function
- Updated `PrintTask()` to display new fields

#### TaskTypes.pas
- Extended `TTask` record with new fields
- Added new helper types and functions
- Enhanced `TSearchCriteria` with category search
- Enhanced `TTaskStatistics` with new metrics
- Added `GetUniqueCategories()` function

### Technical Improvements
- Replaced typed file usage with TFileStream for string-containing records
- Proper JSON and HTML escaping functions
- Modular architecture with clear separation of concerns
- Type-safe dynamic array handling throughout
- Better error handling in history module
- Improved code documentation

### Version Number
- Updated VERSION constant from "1.0.0" to "2.0.0"

## [1.0.0] - 2025-12-22 (Initial Release)

### Added

#### Core Features
- Task CRUD operations (Create, Read, Update, Delete)
- Task properties:
  - ID (auto-incremented)
  - Title and Description
  - Priority (Low, Medium, High, Critical)
  - Status (New, Pending, In Progress, Completed, Cancelled)
  - Created Date, Due Date, Completed Date
  - Tags (comma-separated)
  - IsActive flag (soft delete)

#### Task Management
- `TTaskManagerCore` class for business logic
- Add, update, delete, and retrieve tasks
- Set task status with shortcuts for complete/cancel
- Get all tasks or active tasks only
- Search with custom criteria
- Filter by status or priority
- Find overdue tasks
- Task statistics

#### Storage & Persistence
- Binary file storage for efficiency
- Save and load operations
- CSV export
- Text export with formatting
- `TTaskStorage` class for file operations

#### Query & Search
- Flexible search with `TSearchCriteria`
- Title and description text search
- Status and priority filtering
- Tag-based search
- Overdue task detection

#### Statistics
- Total tasks count
- Active tasks count
- Completed and cancelled counts
- High priority tasks count
- Overdue tasks count

#### Architecture
- Modular design with separate units:
  - `TaskTypes.pas` - Data types and helpers
  - `TaskManager.pas` - Business logic
  - `TaskStorage.pas` - Persistence
  - `TaskManagerMain.pas` - Main program
- Clean separation of concerns
- Type-safe implementations
- Comprehensive self-test suite

#### Documentation
- README.md with full feature documentation
- ARCHITECTURE.md with design documentation
- Inline code comments
- .gitignore for build artifacts

### Technical Details
- Free Pascal (FPC) implementation
- Object Pascal mode
- Dynamic arrays throughout
- No global variables in business logic
- Proper memory management
- Cross-platform compatible

---

## Release Notes

### Version 2.0.0 - Major Feature Release

This release significantly enhances the Task Manager with professional project management features:

**Highlights:**
- 🏷️ **Categories** - Organize tasks into projects or categories
- 🔗 **Dependencies** - Define task relationships and prerequisites
- ⏱️ **Time Tracking** - Plan with estimates, track actual hours
- 📜 **Audit Trail** - Complete history of all changes
- 📊 **Advanced Reports** - Beautiful HTML reports with statistics
- 💾 **Multiple Export Formats** - JSON, HTML, Markdown, CSV, Text

**Backward Compatibility:**
Version 2.0.0 maintains full backward compatibility with v1.0.0 data files. Existing task files will be automatically upgraded when loaded.

**Migration:**
No manual migration needed. Simply recompile and run - existing data will be preserved and new fields will be initialized with sensible defaults.

### Version 1.0.0 - Initial Release

The first stable release of Task Manager providing core task management functionality with:
- Complete CRUD operations
- Flexible search and filtering
- Binary file persistence
- CSV and text exports
- Comprehensive statistics
- Full self-test suite

---

## Upgrade Guide

### From v1.0.0 to v2.0.0

**Data Compatibility:**
- ✅ Your existing `tasks.dat` file will work without modification
- ✅ Tasks will be automatically upgraded to v2 format on first save
- ✅ All existing tasks will have empty Category and DependsOnIDs
- ✅ Time fields will be initialized to 0
- ✅ LastModifiedDate will be set to 0 (empty)

**New Files:**
- `tasks_history.dat` - Will be created automatically for audit trail
- `tasks_export.json` - Created when using JSON export
- `tasks_export.html` - Created when using HTML export
- `tasks_export.md` - Created when using Markdown export
- `tasks_report.html` - Created when generating comprehensive reports

**API Changes:**
- `AddTask()` has a new overloaded version with category and hours
- `GetStatistics()` returns extended statistics (old code will still work)
- New methods available (all additive, no breaking changes)

**Recommended Actions:**
1. Backup your `tasks.dat` file before upgrading
2. Recompile with the new source code
3. Run the self-test to verify all features
4. Start using categories to organize your tasks
5. Add time estimates for better planning
6. Explore the new export formats

---

## Future Roadmap

Planned features for future releases:

### Version 2.1.0 (Minor Features)
- Recurring tasks
- Task templates
- Subtasks/hierarchical tasks
- Custom fields
- Bulk operations

### Version 3.0.0 (Major - Web Interface)
- mORMot2 integration
- SQLite database backend
- REST API
- Web-based UI
- Multi-user support
- Authentication and authorization

### Version 3.1.0 (Enhancements)
- Email notifications
- Calendar integration
- File attachments
- Comments and notes
- Task sharing

---

*For questions, issues, or contributions, please refer to the project documentation.*
