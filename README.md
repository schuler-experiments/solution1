
# Free Pascal Task Manager

A comprehensive, modular task management library built with Free Pascal/Object Pascal and the mORMot 2.x framework. This library provides reusable components for task management, team collaboration, productivity tracking, and workflow automation without any direct UI dependencies.

## 🎯 Overview

The Free Pascal Task Manager is designed as a **library-first** architecture, providing clean APIs that can be integrated into:
- Desktop GUI applications (Lazarus/LCL, fpGUI, MSEide)
- Web services and REST APIs
- Console/CLI tools
- Mobile applications
- Embedded systems

**Key Principle:** No `ReadLn`, no `WriteLn`, no direct user interaction - just clean, reusable business logic.

## ✨ Features

### Core Capabilities
- ✅ **Task Management**: Create, update, delete, and organize tasks with priorities, deadlines, and statuses
- 🏷️ **Tagging System**: Flexible categorization with custom tags and colors
- 💬 **Comments & Discussions**: Thread-based commenting on tasks
- 📊 **Projects**: Group related tasks into projects with progress tracking
- 🔄 **Recurring Tasks**: Automated task generation based on schedules

### Advanced Features
- 📋 **Kanban Boards**: Visual workflow management with customizable columns
- 👥 **Team Management**: Multi-user collaboration with role-based permissions
- ⏱️ **Time Tracking**: Log time spent on tasks with billing support
- 📝 **Templates**: Reusable task and project templates
- 🔍 **Advanced Search**: Full-text search with complex filtering
- 🔔 **Notifications**: Event-driven notification system
- 🎮 **Gamification**: Points, badges, and achievement tracking
- 📚 **Knowledge Base**: Integrated documentation and notes
- 🧘 **Wellbeing**: Break reminders and wellness tracking

## 🏗️ Architecture

```
┌─────────────────────────────────────────────┐
│  Your Application (GUI/Web/Console)        │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│  Service Layer (ITaskService, etc.)        │
│  • Clean interfaces                         │
│  • Business logic encapsulation            │
│  • Event notifications                      │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│  Domain Models (TTaskModel, etc.)          │
│  • Rich domain entities                     │
│  • Validation logic                         │
│  • Type safety                              │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│  Data Layer (mORMot ORM)                   │
│  • SQLite3 persistence                      │
│  • Query optimization                       │
│  • Transaction management                   │
└─────────────────────────────────────────────┘
```

## 🚀 Quick Start

### Prerequisites

- Free Pascal Compiler (FPC) 3.2.0 or later
- mORMot 2.x framework
- SQLite3 (included with mORMot)

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/fpc-task-manager.git
cd fpc-task-manager

# Initialize mORMot submodule (if using submodules)
git submodule update --init --recursive

# Build the library
fpc -B src/taskmanager.pas
```

### Basic Usage Example

```pascal
program SimpleTaskManager;

uses
  SysUtils, mormot.core.base, mormot.orm.core, mormot.orm.rest,
  task_models, task_services, task_services_impl;

var
  RestServer: TSQLRestServerDB;
  TaskService: ITaskService;
  NewTask: TTaskModel;
  TaskID: Int64;
begin
  // Initialize the database
  RestServer := TSQLRestServerDB.Create(
    TSQLModel.Create([TTaskModel, TTagModel, TCommentModel]),
    'tasks.db'
  );
  try
    // Create the task service
    TaskService := TTaskServiceImpl.Create(RestServer);
    
    // Create a new task
    NewTask := TTaskModel.Create;
    try
      NewTask.Title := 'Implement user authentication';
      NewTask.Description := 'Add JWT-based authentication to the API';
      NewTask.Priority := tpHigh;
      NewTask.Status := tsNotStarted;
      NewTask.DueDate := Now + 7; // Due in 7 days
      
      // Save the task
      TaskID := TaskService.CreateTask(NewTask);
      WriteLn('Task created with ID: ', TaskID);
      
    finally
      NewTask.Free;
    end;
    
    // Retrieve and display the task
    NewTask := TaskService.GetTaskByID(TaskID);
    try
      WriteLn('Title: ', NewTask.Title);
      WriteLn('Status: ', GetEnumName(TypeInfo(TTaskStatus), Ord(NewTask.Status)));
      WriteLn('Priority: ', GetEnumName(TypeInfo(TTaskPriority), Ord(NewTask.Priority)));
    finally
      NewTask.Free;
    end;
    
  finally
    RestServer.Free;
  end;
end.
```

### Using with Tags

```pascal
var
  TagService: ITagService;
  Tag: TTagModel;
  TagID: Int64;
begin
  TagService := TTagServiceImpl.Create(RestServer);
  
  // Create a tag
  Tag := TTagModel.Create;
  try
    Tag.Name := 'backend';
    Tag.Color := '#FF5733';
    Tag.Description := 'Backend development tasks';
    TagID := TagService.CreateTag(Tag);
  finally
    Tag.Free;
  end;
  
  // Assign tag to task
  TagService.AssignTagToTask(TaskID, TagID);
end;
```

### Advanced: Using the Event System

```pascal
uses
  task_events;

type
  TMyTaskEventHandler = class(TInterfacedObject, ITaskEventListener)
  public
    procedure OnTaskCreated(const ATask: TTaskModel);
    procedure OnTaskUpdated(const ATask: TTaskModel);
    procedure OnTaskDeleted(ATaskID: Int64);
  end;

procedure TMyTaskEventHandler.OnTaskCreated(const ATask: TTaskModel);
begin
  WriteLn('New task created: ', ATask.Title);
  // Send notification, update UI, log event, etc.
end;

var
  EventManager: TTaskEventManager;
  EventHandler: ITaskEventListener;
begin
  EventManager := TTaskEventManager.Create;
  EventHandler := TMyTaskEventHandler.Create;
  
  // Register event listener
  EventManager.RegisterListener(EventHandler);
  
  // Now all task operations will trigger events
end;
```

## 📚 Documentation

- **[Software Specification](software-spec.md)**: Complete technical specification with architecture, data models, and implementation details
- **API Reference**: See Section 5 of the specification for detailed API documentation
- **Class Diagrams**: See Section 9 for comprehensive class diagrams
- **Integration Guide**: See Section 6 for UI integration patterns

## 🛠️ Building from Source

### Linux/Unix

```bash
# Standard build
make

# Build with debug symbols
make debug

# Run tests
make test

# Clean build artifacts
make clean
```

### Windows

```batch
rem Build the library
build.bat

rem Run tests
test.bat
```

### Custom Build Configuration

Create a `fpc.cfg` file in your project directory:

```
# Search paths
-Fu./src/models
-Fu./src/services
-Fu./src/managers
-Fu./src/infrastructure

# mORMot paths
-Fu./lib/mormot2/src/core
-Fu./lib/mormot2/src/orm

# Optimization
-O3
-CX
-XX

# Debugging (comment out for release)
-g
-gl
```

## 🧪 Testing

The project includes comprehensive unit and integration tests:

```bash
# Run all tests
./tests/AllTests

# Run specific test suite
./tests/TestTaskServices

# Generate coverage report (requires lcov)
make coverage
```

## 📦 Project Structure

```
fpc-task-manager/
├── src/
│   ├── models/              # Domain models (Task, Tag, Comment, etc.)
│   ├── services/            # Service interfaces and implementations
│   ├── managers/            # Feature modules (Boards, Teams, etc.)
│   ├── infrastructure/      # Events, validation, configuration
│   ├── data/                # Database setup and repositories
│   └── utils/               # Utility functions
├── tests/                   # Unit and integration tests
├── examples/
│   ├── console/             # Console application example
│   ├── gui/                 # GUI application example (Lazarus)
│   └── webservice/          # REST API example
├── docs/                    # Additional documentation
├── lib/                     # Third-party libraries (mORMot)
└── bin/                     # Compiled binaries
```

## 🤝 Integration Examples

### Lazarus/LCL Desktop Application

```pascal
procedure TMainForm.CreateTaskButtonClick(Sender: TObject);
var
  Task: TTaskModel;
  TaskID: Int64;
begin
  Task := TTaskModel.Create;
  try
    Task.Title := TitleEdit.Text;
    Task.Description := DescriptionMemo.Text;
    Task.Priority := TTaskPriority(PriorityComboBox.ItemIndex);
    Task.DueDate := DueDatePicker.Date;
    
    TaskID := FTaskService.CreateTask(Task);
    ShowMessage('Task created successfully!');
    RefreshTaskList;
  finally
    Task.Free;
  end;
end;
```

### REST API Server

```pascal
program TaskManagerAPI;

uses
  mormot.rest.http.server,
  task_services, task_services_impl;

var
  HTTPServer: TSQLHttpServer;
  RestServer: TSQLRestServerDB;
begin
  // Initialize database and services
  RestServer := TSQLRestServerDB.Create(...);
  
  // Create HTTP server
  HTTPServer := TSQLHttpServer.Create('8080', [RestServer]);
  try
    WriteLn('Task Manager API running on http://localhost:8080');
    WriteLn('Press [Enter] to quit');
    ReadLn;
  finally
    HTTPServer.Free;
    RestServer.Free;
  end;
end.
```

## 🔐 Security Considerations

- **Input Validation**: All user input is validated through the validation framework
- **SQL Injection Prevention**: mORMot ORM uses parameterized queries
- **Authentication**: Implement using mORMot's built-in authentication or custom solutions
- **Authorization**: Role-based access control through `TUserModel` and service-level checks

## 🚀 Performance

- **Lightweight**: Minimal memory footprint (~2-5 MB for core functionality)
- **Fast**: Optimized SQLite queries with proper indexing
- **Scalable**: Handles 10,000+ tasks efficiently on modest hardware
- **Concurrent**: Thread-safe service implementations

## 📋 Roadmap

- [x] Core task management
- [x] Tagging and filtering
- [x] Comments system
- [x] Kanban boards
- [x] Time tracking
- [ ] Calendar integration
- [ ] Email notifications
- [ ] Mobile app support
- [ ] Cloud synchronization
- [ ] Plugin system

## 🤝 Contributing

Contributions are welcome! Please read our [Contributing Guidelines](CONTRIBUTING.md) before submitting pull requests.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MPL 2.0 License - see the [LICENSE](LICENSE) file for details.

### Third-Party Licenses

- **mORMot Framework**: MPL 1.1/GPL 2.0/LGPL 2.1 triple license
- **SQLite3**: Public Domain
- **Free Pascal RTL**: LGPL with static linking exception

## 🙏 Acknowledgments

- **Synopse mORMot Team**: For the excellent mORMot framework
- **Free Pascal Team**: For the robust FPC compiler
- **Community Contributors**: For bug reports, feature requests, and contributions

## 📞 Support

- **Documentation**: See [software-spec.md](software-spec.md)
- **Issues**: [GitHub Issues](https://github.com/yourusername/fpc-task-manager/issues)
- **Discussions**: [GitHub Discussions](https://github.com/yourusername/fpc-task-manager/discussions)
- **Email**: support@example.com

## 🌟 Why Free Pascal Task Manager?

- ✅ **No Vendor Lock-in**: Open source, MIT licensed
- ✅ **Type Safe**: Leverages Pascal's strong type system
- ✅ **Fast**: Compiled to native code, no runtime overhead
- ✅ **Portable**: Cross-platform support (Windows, Linux, macOS, BSD)
- ✅ **Modular**: Use only what you need
- ✅ **Well-Tested**: Comprehensive test coverage
- ✅ **Production-Ready**: Built on battle-tested mORMot framework
- ✅ **Developer-Friendly**: Clean APIs, excellent documentation

---

**Made with ❤️ using Free Pascal and mORMot**

For detailed technical information, see the complete [Software Specification](software-spec.md).
