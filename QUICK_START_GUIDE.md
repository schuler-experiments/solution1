# Quick Start Guide

Get the Task Manager system up and running in 5 minutes!

## Prerequisites

- Free Pascal Compiler (FPC) installed on your system
- Basic familiarity with command-line interface
- A text editor for viewing source code (optional)

### Installing Free Pascal

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get install fpc
```

**macOS (with Homebrew):**
```bash
brew install fpc
```

**Windows:**
Download from [https://www.freepascal.org/download.html](https://www.freepascal.org/download.html)

Verify installation:
```bash
fpc -v
```

---

## Step 1: Navigate to the Project

```bash
cd solution1
```

---

## Step 2: Compile the Basic Example

Compile the core task manager demo:

```bash
fpc solution1.pas -obin/task_manager -O1 -Mobjfpc
```

**What this does:**
- `solution1.pas` - Source file to compile
- `-obin/task_manager` - Output executable location
- `-O1` - Optimization level 1 (fast compilation)
- `-Mobjfpc` - Use Object Pascal mode

**Expected output:**
```
Free Pascal Compiler version 3.2.2
Compiling solution1/solution1.pas
Linking bin/task_manager
```

---

## Step 3: Run the Demo

```bash
./bin/task_manager
```

Or on Windows:
```bash
bin	ask_manager.exe
```

**What you should see:**
- Test output showing task creation, updates, filtering, sorting
- Statistics like completion rate, total tasks
- CSV export example
- File save/load demonstration

---

## Step 4: Explore More Examples

Once the basic example works, try these other demos:

### Extended Features Demo
```bash
fpc solution2.pas -obin/task_manager_ext -O1 -Mobjfpc
./bin/task_manager_ext
```
**Features**: Task hierarchies, recurring tasks, batch operations

### Kanban/Scrum Boards Demo
```bash
fpc solution15.pas -obin/boards_demo -O1 -Mobjfpc
./bin/boards_demo
```
**Features**: Board management, columns, card operations

### Time Tracking Demo
```bash
fpc solution21.pas -obin/time_tracking_demo -O1 -Mobjfpc
./bin/time_tracking_demo
```
**Features**: Time tracking, analytics, hourly logs

### Intelligence/AI Demo
```bash
fpc solution9.pas -obin/intelligence_demo -O1 -Mobjfpc
./bin/intelligence_demo
```
**Features**: AI analysis, task prioritization, insights

---

## Understanding the Core Concept: 5-Minute Overview

### What is the Task Manager?

A comprehensive, object-oriented task management system built in Free Pascal with:

- **CRUD Operations**: Create, read, update, delete tasks
- **Organization**: Categories, tags, priorities, status tracking
- **Time Tracking**: Estimate and track actual hours
- **Analytics**: Statistics, completion rates, productivity metrics
- **Advanced Features**: Recurring tasks, hierarchies, boards, AI recommendations

### Core Data Structure

Every task has:
```pascal
TTask = record
  ID: Integer;              // Unique identifier
  Title: String;            // Task title
  Description: String;      // Detailed description
  Category: String;         // e.g., "Backend", "Documentation"
  Status: TTaskStatus;      // NotStarted, InProgress, Completed, Cancelled, OnHold
  Priority: TTaskPriority;  // Low, Medium, High, Critical
  CreatedDate: TDateTime;   // When created
  DueDate: TDateTime;       // When it's due
  CompletedDate: TDateTime; // When completed
  EstimatedHours: Real;     // Estimated time to complete
  ActualHours: Real;        // Actual time spent
  Tags: array of String;    // Flexible labels
end;
```

### Core Operation: Creating a Task

Here's a simple example:

```pascal
program SimpleTaskDemo;

uses
  taskmanager;

var
  Manager: TTaskManager;
  TaskID: Integer;

begin
  Manager := TTaskManager.Create;
  
  // Add a task
  TaskID := Manager.AddTask(
    'Write documentation',      // Title
    'Create API documentation', // Description
    'Documentation',            // Category
    tpHigh,                      // Priority (High)
    EncodeDate(2025, 2, 15),     // Due date
    4.0                          // Estimated hours
  );
  
  WriteLn('Task created with ID: ', TaskID);
  
  Manager.Free;
end.
```

---

## Common First Steps

### 1. Understand the Source Files

The project contains:

- **taskmanager.pas** - Core module (899 lines)
- **solution1.pas through solution22.pas** - Example programs
- **taskmanagerXXX.pas** - Feature modules (boards, teams, intelligence, etc.)

### 2. Read the Documentation

- **README.md** - Project overview
- **ARCHITECTURE.md** - System design
- **DEVELOPER_GUIDE.md** - Coding conventions
- **API_REFERENCE.md** - Complete API documentation

**→ Start with [TABLE_OF_CONTENTS_AND_INDEX.md](TABLE_OF_CONTENTS_AND_INDEX.md) for navigation!**

### 3. Run Examples in Order

1. **solution1.pas** - Core functionality
2. **solution2.pas** - Extended features
3. **solution15.pas** - Boards (Kanban)
4. **solution6.pas** - Recurring tasks
5. Pick others based on your interests

### 4. Study the Code

Each solution*.pas file is:
- **Self-contained** - Can run independently
- **Well-commented** - Explains what's happening
- **Feature-focused** - Demonstrates specific capabilities
- **Typically 150-1500 lines** - Easy to understand in one sitting

---

## Compilation Cheat Sheet

```bash
# Basic demo (start here)
fpc solution1.pas -obin/task_manager -O1 -Mobjfpc && ./bin/task_manager

# Extended features
fpc solution2.pas -obin/task_manager_ext -O1 -Mobjfpc && ./bin/task_manager_ext

# Advanced features
fpc solution3.pas -obin/task_manager_advanced -O1 -Mobjfpc && ./bin/task_manager_advanced

# Kanban/Scrum boards
fpc solution15.pas -obin/boards_demo -O1 -Mobjfpc && ./bin/boards_demo

# Recurring tasks
fpc solution6.pas -obin/recurring_demo -O1 -Mobjfpc && ./bin/recurring_demo

# Focus mode
fpc solution10.pas -obin/focus_demo -O1 -Mobjfpc && ./bin/focus_demo

# Team features
fpc solution14.pas -obin/team_demo -O1 -Mobjfpc && ./bin/team_demo

# Notifications
fpc solution13.pas -obin/notifications_demo -O1 -Mobjfpc && ./bin/notifications_demo

# Time tracking
fpc solution21.pas -obin/time_tracking_demo -O1 -Mobjfpc && ./bin/time_tracking_demo

# Intelligence/AI
fpc solution9.pas -obin/intelligence_demo -O1 -Mobjfpc && ./bin/intelligence_demo

# See SOLUTION_FILES_REFERENCE.md for all 22 examples
```

---

## Troubleshooting

### "fpc: command not found"
**Solution**: Free Pascal is not installed. Install it using the instructions above.

### "Error: file not found"
**Make sure you:**
1. Are in the solution1 directory
2. Have the source files in the current directory
3. The bin/ directory exists (create it if needed)

```bash
mkdir -p bin
```

### Compilation takes a long time
**This is normal for first compilation.** The compiler:
- Reads all source files
- Checks for errors
- Generates optimized code
- Writes the executable

Subsequent compilations are faster if you don't clean intermediate files.

### Executable won't run
**Try running with full path:**
```bash
./bin/task_manager
# or
bin/task_manager
# or on Windows
bin	ask_manager.exe
```

---

## Next Steps After Quick Start

### For Learners
1. ✅ Run solution1.pas (basic)
2. ✅ Run solution2.pas (extended)
3. → Read [README_CORE_TASKMANAGER.md](README_CORE_TASKMANAGER.md)
4. → Pick a feature from [TABLE_OF_CONTENTS_AND_INDEX.md](TABLE_OF_CONTENTS_AND_INDEX.md)
5. → Run corresponding solution*.pas file
6. → Read corresponding documentation

### For Developers
1. ✅ Compile and run a demo
2. → Read [ARCHITECTURE.md](ARCHITECTURE.md)
3. → Review [DEVELOPER_GUIDE.md](DEVELOPER_GUIDE.md)
4. → Study [API_REFERENCE.md](API_REFERENCE.md)
5. → Examine source code in taskmanager*.pas files
6. → Create your own program using the library

### For Contributors
1. ✅ Get code compiling
2. → Understand [ARCHITECTURE.md](ARCHITECTURE.md)
3. → Follow [DEVELOPER_GUIDE.md](DEVELOPER_GUIDE.md)
4. → Check module dependencies in source code
5. → Make changes maintaining the code style
6. → Run all solution*.pas examples to verify changes
7. → Update documentation as needed

---

## Key Files to Know

| File | Purpose |
|------|---------|
| taskmanager.pas | Core module - always used |
| solution1.pas | Basic example - start here |
| solution2.pas | Extended features example |
| solution15.pas | Boards example |
| bin/ | Compiled executable directory |
| README.md | Project overview |
| TABLE_OF_CONTENTS_AND_INDEX.md | Navigation guide |
| ARCHITECTURE.md | System design |
| API_REFERENCE.md | Complete API |

---

## Common Commands

```bash
# Navigate to project
cd solution1

# Create bin directory if missing
mkdir -p bin

# Compile basic example
fpc solution1.pas -obin/task_manager -O1 -Mobjfpc

# Run compiled program
./bin/task_manager

# Clean compilation artifacts (keeping executables)
find . -name "*.o" -delete
find . -name "*.ppu" -delete

# View source code
cat solution1.pas | head -50

# Count lines in a file
wc -l solution1.pas

# Search for a function
grep -n "function AddTask" taskmanager.pas
```

---

## What Each Example Demonstrates

**Solution 1**: Core CRUD, filtering, sorting, statistics  
**Solution 2**: Extended tasks, hierarchies, batch operations  
**Solution 3**: Advanced features, work sessions  
**Solution 4**: Work sessions, notes  
**Solution 5**: Extended analytics  
**Solution 6**: Recurring tasks  
**Solution 7**: Enhanced features  
**Solution 8**: Resource management  
**Solution 9**: AI and intelligence  
**Solution 10**: Focus mode  
**Solution 11**: Lifestyle and wellbeing  
**Solution 12**: Meeting management  
**Solution 13**: Notifications  
**Solution 14**: Team collaboration  
**Solution 15**: Kanban/Scrum boards  
**Solution 16**: Advanced search  
**Solution 17**: Smart recommendations  
**Solution 18**: Comments  
**Solution 19**: Gamification  
**Solution 20**: Templates  
**Solution 21**: Time tracking  
**Solution 22**: Knowledge base  

See [SOLUTION_FILES_REFERENCE.md](SOLUTION_FILES_REFERENCE.md) for detailed descriptions of each!

---

## Documentation Roadmap

```
You are here: QUICK_START_GUIDE.md ✓
         ↓
    README.md (project overview)
         ↓
Pick your path:
├── [Learner] → README_CORE_TASKMANAGER.md → Pick features → Deep dives
├── [Developer] → ARCHITECTURE.md → DEVELOPER_GUIDE.md → API_REFERENCE.md
└── [Contributor] → All of the above + Study source code

Navigation: Use TABLE_OF_CONTENTS_AND_INDEX.md for complete guidance
```

---

## Tips for Success

1. **Start small**: Compile and run solution1.pas first
2. **Read one doc at a time**: Don't try to learn everything at once
3. **Run the examples**: They're the best teachers
4. **Explore gradually**: Pick one feature, read about it, run its example
5. **Reference as needed**: Use API_REFERENCE.md when writing code
6. **Join the community**: Share what you build!

---

## Getting Help

1. **Can't compile?** → Check the [Troubleshooting](#troubleshooting) section
2. **Don't understand a feature?** → Read the corresponding README_*.md file
3. **Need API details?** → Check [API_REFERENCE.md](API_REFERENCE.md)
4. **Want to contribute?** → Read [DEVELOPER_GUIDE.md](DEVELOPER_GUIDE.md)
5. **Lost in documentation?** → Use [TABLE_OF_CONTENTS_AND_INDEX.md](TABLE_OF_CONTENTS_AND_INDEX.md)

---

## Version Information

- **Language**: Free Pascal (Object Pascal mode)
- **Minimum FPC Version**: 3.0.0
- **Target OS**: Linux, macOS, Windows
- **Total Code**: ~42,700 lines across 44 source files
- **Total Documentation**: ~38 markdown files

---

Happy coding! 🚀

**Next**: Run `fpc solution1.pas -obin/task_manager -O1 -Mobjfpc && ./bin/task_manager`
