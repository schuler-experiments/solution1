
## 8. Deployment and Scaling Strategies

### 8.1 Overview

The Free Pascal Task Manager is designed as a **library-first architecture**, meaning it can be deployed in various configurations ranging from single-user desktop applications to multi-user server applications. This section outlines deployment strategies, scaling approaches, and operational considerations.

### 8.2 Deployment Architectures

#### 8.2.1 Embedded Library Deployment (Recommended for Desktop)

**Architecture:**
```
┌─────────────────────────────────────┐
│   Desktop Application (GUI/CLI)     │
│  ┌───────────────────────────────┐  │
│  │  Task Manager Library         │  │
│  │  (Compiled into executable)   │  │
│  │                               │  │
│  │  ┌─────────────────────────┐  │  │
│  │  │  SQLite Database File   │  │  │
│  │  │  (Local filesystem)     │  │  │
│  │  └─────────────────────────┘  │  │
│  └───────────────────────────────┘  │
└─────────────────────────────────────┘
```

**Characteristics:**
- Single executable deployment
- No network dependencies
- Local SQLite database
- Platform-specific builds (Windows .exe, Linux binary, macOS .app)
- Minimal installation requirements

**Use Cases:**
- Personal task management applications
- Offline-first applications
- Portable applications (USB deployment)

**Deployment Steps:**
1. Compile application with FPC including all task manager units
2. Package executable with runtime libraries (if using dynamic linking)
3. Include default configuration file
4. Create installation package (NSIS for Windows, DEB/RPM for Linux, DMG for macOS)

**Example Build Command:**
```bash
# Linux/macOS
fpc -O3 -XX -CX \
    -Fu../src/models \
    -Fu../src/services \
    -Fu../src/features \
    -Fu../mORMot2/src/core \
    -Fu../mORMot2/src/orm \
    -FUunits/x86_64-linux \
    -FEbin \
    myapp.lpr

# Windows (cross-compile or native)
fpc -Twin64 -O3 -XX -CX \
    -Fu../src/models \
    -Fu../src/services \
    -Fu../src/features \
    -Fu../mORMot2/src/core \
    -Fu../mORMot2/src/orm \
    -FUunits/x86_64-win64 \
    -FEbin \
    myapp.lpr
```

#### 8.2.2 Client-Server Deployment

**Architecture:**
```
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│   Client 1   │  │   Client 2   │  │   Client N   │
│  (Thin GUI)  │  │  (Thin GUI)  │  │  (Thin GUI)  │
└──────┬───────┘  └──────┬───────┘  └──────┬───────┘
       │                 │                 │
       └─────────────────┼─────────────────┘
                         │ HTTP/REST or TCP
                         ↓
              ┌──────────────────────┐
              │   mORMot REST Server │
              │  ┌────────────────┐  │
              │  │ Task Manager   │  │
              │  │ Library        │  │
              │  └────────────────┘  │
              │  ┌────────────────┐  │
              │  │ SQLite/External│  │
              │  │ Database       │  │
              │  └────────────────┘  │
              └──────────────────────┘
```

**Characteristics:**
- Centralized data management
- Multi-user support
- Network-based communication
- RESTful API or binary protocol
- Session management required

**Use Cases:**
- Team collaboration tools
- Enterprise task management
- Multi-device synchronization

**Server Deployment Steps:**
1. Compile server application with mORMot HTTP server
2. Configure listening port and security settings
3. Set up database (SQLite for small teams, PostgreSQL/MySQL for larger deployments)
4. Configure authentication and authorization
5. Set up reverse proxy (nginx/Apache) for HTTPS
6. Configure firewall rules
7. Set up systemd service (Linux) or Windows Service

**Example Server Configuration:**
```pascal
type
  TTaskManagerServer = class
  private
    FServer: TRestServerDB;
    FHttpServer: TRestHttpServer;
  public
    constructor Create(const ADatabaseFile: TFileName; APort: Integer);
    procedure Start;
    procedure Stop;
  end;

constructor TTaskManagerServer.Create(const ADatabaseFile: TFileName; APort: Integer);
begin
  // Create ORM model
  FModel := CreateTaskManagerModel;
  
  // Create database server
  FServer := TRestServerDB.Create(FModel, ADatabaseFile);
  FServer.CreateMissingTables;
  
  // Set up authentication
  FServer.AuthenticationRegister(TRestServerAuthenticationDefault);
  
  // Create HTTP server
  FHttpServer := TRestHttpServer.Create(
    IntToStr(APort),
    [FServer],
    '+',  // domain name (+ = all interfaces)
    useHttpApiRegisteringURI
  );
end;
```

**systemd Service File (Linux):**
```ini
[Unit]
Description=Task Manager REST Server
After=network.target

[Service]
Type=simple
User=taskmanager
Group=taskmanager
WorkingDirectory=/opt/taskmanager
ExecStart=/opt/taskmanager/bin/taskmanager-server
Restart=always
RestartSec=10
StandardOutput=journal
StandardError=journal

# Security hardening
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=/var/lib/taskmanager

[Install]
WantedBy=multi-user.target
```

#### 8.2.3 Microservices Deployment (Advanced)

**Architecture:**
```
┌─────────────────────────────────────────────────┐
│              Load Balancer / API Gateway        │
└───────┬─────────────┬─────────────┬─────────────┘
        │             │             │
   ┌────▼────┐   ┌────▼────┐   ┌────▼────┐
   │ Task    │   │ Notif.  │   │ Search  │
   │ Service │   │ Service │   │ Service │
   └────┬────┘   └────┬────┘   └────┬────┘
        │             │             │
        └─────────────┼─────────────┘
                      │
              ┌───────▼────────┐
              │  Database      │
              │  (PostgreSQL)  │
              └────────────────┘
```

**Characteristics:**
- Service decomposition by feature module
- Independent scaling per service
- Polyglot database support
- Container-based deployment (Docker)
- Orchestration (Kubernetes, Docker Swarm)

**Use Cases:**
- Large-scale enterprise deployments
- High availability requirements
- Geographic distribution

### 8.3 Scaling Strategies

#### 8.3.1 Vertical Scaling (Scale Up)

**Database Optimization:**
```pascal
// Configure SQLite for better performance
procedure OptimizeSQLite(AServer: TRestServerDB);
begin
  AServer.DB.Execute('PRAGMA journal_mode=WAL');
  AServer.DB.Execute('PRAGMA synchronous=NORMAL');
  AServer.DB.Execute('PRAGMA cache_size=10000');
  AServer.DB.Execute('PRAGMA temp_store=MEMORY');
  AServer.DB.Execute('PRAGMA mmap_size=268435456'); // 256MB
  AServer.DB.Execute('PRAGMA page_size=4096');
end;
```

**Connection Pooling:**
```pascal
// mORMot automatically handles connection pooling
// Configure pool size:
FServer.AcquireExecutionMode[execOrmGet] := amBackgroundThread;
FServer.AcquireExecutionMode[execOrmWrite] := amBackgroundThread;
```

**Memory Management:**
- Increase cache sizes for frequently accessed data
- Use batch operations for bulk inserts/updates
- Implement query result pagination
- Use indexes on frequently queried fields

**Limits:**
- SQLite: ~140 TB database size, excellent for <100GB workloads
- Recommended: Up to 100,000 tasks for single SQLite instance
- Server: 8-16 CPU cores, 16-64 GB RAM optimal

#### 8.3.2 Horizontal Scaling (Scale Out)

**Read Replicas (SQLite Limitations):**
SQLite doesn't natively support replication. Alternatives:
1. **Litestream** - Continuous SQLite replication to S3/cloud storage
2. **rqlite** - Distributed SQLite using Raft consensus
3. **Migration to PostgreSQL** for native replication

**Load Balancing:**
```
┌────────────────┐
│ Load Balancer  │
│   (HAProxy)    │
└───────┬────────┘
        │
   ┌────┼────┐
   ▼    ▼    ▼
┌─────┐ ┌─────┐ ┌─────┐
│App 1│ │App 2│ │App 3│
└──┬──┘ └──┬──┘ └──┬──┘
   └───────┼───────┘
           ▼
    ┌──────────────┐
    │  PostgreSQL  │
    │   (Primary)  │
    └──────┬───────┘
           │
    ┌──────┼───────┐
    ▼      ▼       ▼
┌────────┐ ┌────────┐
│Replica1│ │Replica2│
└────────┘ └────────┘
```

**Session Management:**
```pascal
// Use stateless authentication (JWT tokens)
// Store session state in Redis or database
type
  TTaskManagerAuthService = class
  private
    FRedisClient: TRedisClient; // Optional external session store
  public
    function CreateSession(AUserID: Int64): RawUtf8; // Returns JWT token
    function ValidateSession(const AToken: RawUtf8): Boolean;
  end;
```

**Caching Strategy:**
```pascal
// Implement caching layer
type
  TTaskCacheService = class
  private
    FCache: TSynDictionary; // In-memory cache
    FTaskService: ITaskService;
  public
    function GetTask(ATaskID: Int64): TTaskModel;
    procedure InvalidateTask(ATaskID: Int64);
  end;

function TTaskCacheService.GetTask(ATaskID: Int64): TTaskModel;
var
  CachedValue: RawUtf8;
begin
  if FCache.FindAndCopy(Int64ToUtf8(ATaskID), CachedValue) then
    Result := TTaskModel.CreateFrom(CachedValue)
  else
  begin
    Result := FTaskService.GetTask(ATaskID);
    FCache.Add(Int64ToUtf8(ATaskID), ObjectToJson(Result));
  end;
end;
```

#### 8.3.3 Performance Benchmarks and Capacity Planning

**Expected Performance (Single Server, SQLite):**
- Task Creation: 1,000-5,000 tasks/second
- Task Retrieval: 10,000-50,000 reads/second
- Task Updates: 500-2,000 updates/second
- Search Queries: 100-1,000 searches/second (depends on complexity)

**Capacity Planning Guidelines:**
```
Users          Database Size    Server Requirements
─────────────────────────────────────────────────────
1-10          < 100 MB         1 CPU, 512 MB RAM
10-100        < 1 GB           2 CPU, 2 GB RAM
100-1,000     < 10 GB          4 CPU, 8 GB RAM
1,000-10,000  < 100 GB         8 CPU, 16 GB RAM
10,000+       > 100 GB         16+ CPU, 32+ GB RAM (PostgreSQL)
```

### 8.4 Configuration Management

#### 8.4.1 Configuration File Format

**config.json:**
```json
{
  "database": {
    "type": "sqlite",
    "path": "./data/taskmanager.db",
    "options": {
      "journal_mode": "WAL",
      "synchronous": "NORMAL",
      "cache_size": 10000
    }
  },
  "server": {
    "port": 8080,
    "host": "0.0.0.0",
    "max_connections": 100,
    "timeout": 30
  },
  "features": {
    "enable_notifications": true,
    "enable_gamification": true,
    "enable_time_tracking": true,
    "enable_wellbeing": false
  },
  "security": {
    "require_authentication": true,
    "jwt_secret": "CHANGE_THIS_IN_PRODUCTION",
    "session_timeout": 3600,
    "password_min_length": 8
  },
  "logging": {
    "level": "info",
    "file": "./logs/taskmanager.log",
    "max_size_mb": 100,
    "max_files": 10
  }
}
```

**Configuration Loader:**
```pascal
type
  TTaskManagerConfig = class
  private
    FDatabasePath: TFileName;
    FServerPort: Integer;
    FEnableNotifications: Boolean;
    procedure LoadFromFile(const AFileName: TFileName);
  public
    constructor Create(const AConfigFile: TFileName);
    property DatabasePath: TFileName read FDatabasePath;
    property ServerPort: Integer read FServerPort;
  end;

constructor TTaskManagerConfig.Create(const AConfigFile: TFileName);
begin
  inherited Create;
  LoadFromFile(AConfigFile);
end;

procedure TTaskManagerConfig.LoadFromFile(const AFileName: TFileName);
var
  JSON: RawUtf8;
  Doc: TDocVariantData;
begin
  JSON := RawUtf8FromFile(AFileName);
  Doc.InitJson(JSON);
  
  FDatabasePath := Doc.U['database.path'];
  FServerPort := Doc.I['server.port'];
  FEnableNotifications := Doc.B['features.enable_notifications'];
  // ... load other settings
end;
```

#### 8.4.2 Environment-Specific Configuration

**Development:**
```json
{
  "database": {
    "path": "./data/dev.db"
  },
  "logging": {
    "level": "debug"
  },
  "security": {
    "require_authentication": false
  }
}
```

**Production:**
```json
{
  "database": {
    "path": "/var/lib/taskmanager/production.db"
  },
  "logging": {
    "level": "warning"
  },
  "security": {
    "require_authentication": true,
    "jwt_secret": "${JWT_SECRET_FROM_ENV}"
  }
}
```

### 8.5 Build and Packaging

#### 8.5.1 Build Scripts

**build.sh (Linux/macOS):**
```bash
#!/bin/bash
set -e

# Configuration
FPC_VERSION="3.2.2"
BUILD_TYPE="${1:-release}"  # debug or release
TARGET_OS="${2:-linux}"     # linux, win64, darwin

# Directories
SRC_DIR="./src"
BUILD_DIR="./build"
BIN_DIR="./bin"
UNITS_DIR="./units/${TARGET_OS}"

# Compiler flags
COMMON_FLAGS="-Fu${SRC_DIR}/models -Fu${SRC_DIR}/services -Fu${SRC_DIR}/features"
COMMON_FLAGS="${COMMON_FLAGS} -Fu./mORMot2/src/core -Fu./mORMot2/src/orm"
COMMON_FLAGS="${COMMON_FLAGS} -FU${UNITS_DIR} -FE${BIN_DIR}"

if [ "$BUILD_TYPE" = "release" ]; then
  OPT_FLAGS="-O3 -XX -CX -Xs"
else
  OPT_FLAGS="-g -gl -gh"
fi

# Clean previous build
rm -rf "${UNITS_DIR}"
mkdir -p "${UNITS_DIR}" "${BIN_DIR}"

# Build
echo "Building Task Manager for ${TARGET_OS} (${BUILD_TYPE})..."
fpc ${COMMON_FLAGS} ${OPT_FLAGS} \
    -T${TARGET_OS} \
    ./src/taskmanager_main.lpr

echo "Build complete: ${BIN_DIR}/taskmanager_main"
```

**build.bat (Windows):**
```batch
@echo off
setlocal

set BUILD_TYPE=%1
if "%BUILD_TYPE%"=="" set BUILD_TYPE=release

set SRC_DIR=.\src
set BIN_DIR=.\bin
set UNITS_DIR=.\units\win64

if "%BUILD_TYPE%"=="release" (
  set OPT_FLAGS=-O3 -XX -CX
) else (
  set OPT_FLAGS=-g -gl
)

if not exist "%UNITS_DIR%" mkdir "%UNITS_DIR%"
if not exist "%BIN_DIR%" mkdir "%BIN_DIR%"

echo Building Task Manager (Windows %BUILD_TYPE%)...
fpc -Twin64 ^
    -Fu%SRC_DIR%\models -Fu%SRC_DIR%\services -Fu%SRC_DIR%\features ^
    -Fu.\mORMot2\src\core -Fu.\mORMot2\src\orm ^
    -FU%UNITS_DIR% -FE%BIN_DIR% ^
    %OPT_FLAGS% ^
    %SRC_DIR%\taskmanager_main.lpr

echo Build complete: %BIN_DIR%\taskmanager_main.exe
```

#### 8.5.2 Continuous Integration

**GitHub Actions (.github/workflows/build.yml):**
```yaml
name: Build Task Manager

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  build-linux:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          submodules: recursive
      
      - name: Install FPC
        run: |
          sudo apt-get update
          sudo apt-get install -y fpc
      
      - name: Build
        run: ./build.sh release linux
      
      - name: Run Tests
        run: ./bin/taskmanager_tests
      
      - name: Upload Artifact
        uses: actions/upload-artifact@v3
        with:
          name: taskmanager-linux
          path: bin/taskmanager_main

  build-windows:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v3
        with:
          submodules: recursive
      
      - name: Install FPC
        run: choco install freepascal
      
      - name: Build
        run: .\build.bat release
      
      - name: Upload Artifact
        uses: actions/upload-artifact@v3
        with:
          name: taskmanager-windows
          path: bin\taskmanager_main.exe
```

#### 8.5.3 Packaging Formats

**Debian Package (DEB):**
```bash
# Create package structure
mkdir -p taskmanager_1.0.0/DEBIAN
mkdir -p taskmanager_1.0.0/usr/bin
mkdir -p taskmanager_1.0.0/etc/taskmanager
mkdir -p taskmanager_1.0.0/var/lib/taskmanager

# Copy files
cp bin/taskmanager_main taskmanager_1.0.0/usr/bin/
cp config.json taskmanager_1.0.0/etc/taskmanager/

# Create control file
cat > taskmanager_1.0.0/DEBIAN/control << EOF
Package: taskmanager
Version: 1.0.0
Architecture: amd64
Maintainer: Your Name <your@email.com>
Description: Free Pascal Task Manager
 A comprehensive task management system built with Free Pascal
Depends: libc6 (>= 2.31)
EOF

# Build package
dpkg-deb --build taskmanager_1.0.0
```

**Windows Installer (NSIS):**
```nsis
; taskmanager-installer.nsi
!include "MUI2.nsh"

Name "Task Manager"
OutFile "TaskManager-Setup-1.0.0.exe"
InstallDir "$PROGRAMFILES64\TaskManager"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Install"
  SetOutPath "$INSTDIR"
  File "bin\taskmanager_main.exe"
  File "config.json"
  
  CreateDirectory "$INSTDIR\data"
  CreateDirectory "$INSTDIR\logs"
  
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
  CreateShortcut "$DESKTOP\Task Manager.lnk" "$INSTDIR\taskmanager_main.exe"
SectionEnd

Section "Uninstall"
  Delete "$INSTDIR\taskmanager_main.exe"
  Delete "$INSTDIR\config.json"
  Delete "$INSTDIR\Uninstall.exe"
  Delete "$DESKTOP\Task Manager.lnk"
  RMDir "$INSTDIR"
SectionEnd
```

**Docker Container:**
```dockerfile
# Dockerfile
FROM fpcsrc/fpc:3.2.2 AS builder

WORKDIR /app
COPY . .

RUN apt-get update && apt-get install -y git
RUN ./build.sh release linux

FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/bin/taskmanager_main .
COPY --from=builder /app/config.json .

RUN mkdir -p /app/data /app/logs
VOLUME ["/app/data", "/app/logs"]

EXPOSE 8080

CMD ["./taskmanager_main"]
```

**Docker Compose:**
```yaml
# docker-compose.yml
version: '3.8'

services:
  taskmanager:
    build: .
    ports:
      - "8080:8080"
    volumes:
      - ./data:/app/data
      - ./logs:/app/logs
      - ./config.production.json:/app/config.json:ro
    environment:
      - JWT_SECRET=${JWT_SECRET}
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 30s
      timeout: 10s
      retries: 3

  nginx:
    image: nginx:alpine
    ports:
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
      - ./ssl:/etc/nginx/ssl:ro
    depends_on:
      - taskmanager
    restart: unless-stopped
```

### 8.6 Database Deployment and Migration

#### 8.6.1 Initial Database Setup

```pascal
procedure InitializeDatabase(const ADatabasePath: TFileName);
var
  Server: TRestServerDB;
  Model: TOrmModel;
begin
  Model := CreateTaskManagerModel;
  try
    Server := TRestServerDB.Create(Model, ADatabasePath);
    try
      // Create all tables
      Server.CreateMissingTables;
      
      // Create indexes for performance
      Server.DB.Execute(
        'CREATE INDEX IF NOT EXISTS idx_tasks_status ON Tasks(Status)'
      );
      Server.DB.Execute(
        'CREATE INDEX IF NOT EXISTS idx_tasks_duedate ON Tasks(DueDate)'
      );
      Server.DB.Execute(
        'CREATE INDEX IF NOT EXISTS idx_tasks_priority ON Tasks(Priority)'
      );
      Server.DB.Execute(
        'CREATE INDEX IF NOT EXISTS idx_comments_taskid ON Comments(TaskID)'
      );
      Server.DB.Execute(
        'CREATE INDEX IF NOT EXISTS idx_tasktags_taskid ON TaskTags(TaskID)'
      );
      Server.DB.Execute(
        'CREATE INDEX IF NOT EXISTS idx_tasktags_tagid ON TaskTags(TagID)'
      );
      
      // Insert default data
      InsertDefaultData(Server);
    finally
      Server.Free;
    end;
  finally
    Model.Free;
  end;
end;
```

#### 8.6.2 Database Migration System

```pascal
type
  TDatabaseMigration = class
  private
    FServer: TRestServerDB;
    FCurrentVersion: Integer;
    procedure ApplyMigration(AMigrationSQL: RawUtf8; AVersion: Integer);
  public
    constructor Create(AServer: TRestServerDB);
    function GetCurrentVersion: Integer;
    procedure MigrateTo(ATargetVersion: Integer);
  end;

procedure TDatabaseMigration.ApplyMigration(AMigrationSQL: RawUtf8; AVersion: Integer);
begin
  FServer.DB.TransactionBegin;
  try
    FServer.DB.Execute(AMigrationSQL);
    FServer.DB.Execute(
      'INSERT OR REPLACE INTO schema_version (version, applied_at) VALUES (?, ?)',
      [AVersion, NowToString]
    );
    FServer.DB.Commit;
  except
    FServer.DB.Rollback;
    raise;
  end;
end;

// Migration definitions
const
  MIGRATION_001 = 
    'ALTER TABLE Tasks ADD COLUMN EstimatedHours REAL DEFAULT 0';
  
  MIGRATION_002 = 
    'CREATE TABLE IF NOT EXISTS Attachments (' +
    '  ID INTEGER PRIMARY KEY,' +
    '  TaskID INTEGER NOT NULL,' +
    '  FileName TEXT NOT NULL,' +
    '  FilePath TEXT NOT NULL,' +
    '  FileSize INTEGER,' +
    '  UploadedAt TEXT' +
    ')';
```

#### 8.6.3 Backup and Restore

**Automated Backup Script:**
```bash
#!/bin/bash
# backup-taskmanager.sh

DB_PATH="/var/lib/taskmanager/production.db"
BACKUP_DIR="/var/backups/taskmanager"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="${BACKUP_DIR}/taskmanager_${TIMESTAMP}.db"

# Create backup directory if not exists
mkdir -p "${BACKUP_DIR}"

# SQLite backup using .backup command
sqlite3 "${DB_PATH}" ".backup '${BACKUP_FILE}'"

# Compress backup
gzip "${BACKUP_FILE}"

# Keep only last 30 days of backups
find "${BACKUP_DIR}" -name "taskmanager_*.db.gz" -mtime +30 -delete

echo "Backup completed: ${BACKUP_FILE}.gz"
```

**Cron Job for Daily Backups:**
```cron
# /etc/cron.d/taskmanager-backup
0 2 * * * taskmanager /opt/taskmanager/scripts/backup-taskmanager.sh
```

**Restore Procedure:**
```bash
#!/bin/bash
# restore-taskmanager.sh

BACKUP_FILE="$1"
DB_PATH="/var/lib/taskmanager/production.db"

if [ -z "$BACKUP_FILE" ]; then
  echo "Usage: $0 <backup_file.db.gz>"
  exit 1
fi

# Stop service
systemctl stop taskmanager

# Backup current database
cp "${DB_PATH}" "${DB_PATH}.before-restore"

# Restore from backup
gunzip -c "${BACKUP_FILE}" > "${DB_PATH}"

# Verify database integrity
sqlite3 "${DB_PATH}" "PRAGMA integrity_check"

# Start service
systemctl start taskmanager

echo "Restore completed from ${BACKUP_FILE}"
```

### 8.7 Monitoring and Maintenance

#### 8.7.1 Health Check Endpoint

```pascal
type
  THealthCheckService = class
  public
    function GetHealthStatus: THealthStatus;
  end;

  THealthStatus = record
    Overall: string;  // "healthy", "degraded", "unhealthy"
    DatabaseConnected: Boolean;
    DatabaseSize: Int64;
    TaskCount: Integer;
    UptimeSeconds: Integer;
    MemoryUsageMB: Integer;
  end;

function THealthCheckService.GetHealthStatus: THealthStatus;
begin
  Result.Overall := 'healthy';
  
  try
    Result.DatabaseConnected := FServer.DB.IsOpen;
    Result.DatabaseSize := GetFileSize(FDatabasePath);
    Result.TaskCount := FServer.TableRowCount(TTaskModel);
    Result.UptimeSeconds := GetTickCount64 div 1000;
    Result.MemoryUsageMB := GetHeapStatus.TotalAllocated div (1024 * 1024);
    
    if not Result.DatabaseConnected then
      Result.Overall := 'unhealthy'
    else if Result.MemoryUsageMB > 1024 then
      Result.Overall := 'degraded';
  except
    Result.Overall := 'unhealthy';
  end;
end;
```

#### 8.7.2 Logging Strategy

```pascal
uses
  mormot.core.log;

// Configure logging
procedure ConfigureLogging;
begin
  with TSynLog.Family do
  begin
    Level := LOG_VERBOSE;
    PerThreadLog := ptIdentifiedInOneFile;
    RotateFileCount := 10;
    RotateFileSizeKB := 10240; // 10 MB
    DestinationPath := './logs/';
    HighResolutionTimestamp := true;
  end;
end;

// Usage in code
procedure TTaskServiceImpl.CreateTask(const ATask: TTaskModel);
begin
  TSynLog.Add.Log(sllInfo, 'Creating task: %', [ATask.Title]);
  try
    // ... task creation logic
    TSynLog.Add.Log(sllDebug, 'Task created with ID: %', [ATask.ID]);
  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllError, 'Failed to create task: %', [E.Message]);
      raise;
    end;
  end;
end;
```

#### 8.7.3 Performance Monitoring

```pascal
type
  TPerformanceMetrics = class
  private
    FRequestCount: Int64;
    FTotalResponseTime: Int64;
    FSlowQueries: TStringList;
  public
    procedure RecordRequest(AResponseTimeMs: Integer);
    function GetAverageResponseTime: Double;
    property RequestCount: Int64 read FRequestCount;
  end;

procedure TPerformanceMetrics.RecordRequest(AResponseTimeMs: Integer);
begin
  InterlockedIncrement(FRequestCount);
  InterlockedExchangeAdd(FTotalResponseTime, AResponseTimeMs);
  
  if AResponseTimeMs > 1000 then // Slow query threshold
    TSynLog.Add.Log(sllWarning, 'Slow query detected: % ms', [AResponseTimeMs]);
end;
```

#### 8.7.4 Database Maintenance

**Vacuum Schedule:**
```bash
#!/bin/bash
# vacuum-database.sh

DB_PATH="/var/lib/taskmanager/production.db"

echo "Starting VACUUM on ${DB_PATH}..."
sqlite3 "${DB_PATH}" "VACUUM;"
echo "VACUUM completed"

# Analyze for query optimizer
sqlite3 "${DB_PATH}" "ANALYZE;"
echo "ANALYZE completed"
```

**Weekly Maintenance Cron:**
```cron
# /etc/cron.d/taskmanager-maintenance
0 3 * * 0 taskmanager /opt/taskmanager/scripts/vacuum-database.sh
```

### 8.8 Security Deployment Considerations

#### 8.8.1 HTTPS Configuration (nginx reverse proxy)

**nginx.conf:**
```nginx
upstream taskmanager_backend {
    server localhost:8080;
    keepalive 32;
}

server {
    listen 443 ssl http2;
    server_name taskmanager.example.com;

    ssl_certificate /etc/nginx/ssl/cert.pem;
    ssl_certificate_key /etc/nginx/ssl/key.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;

    location / {
        proxy_pass http://taskmanager_backend;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # Timeouts
        proxy_connect_timeout 60s;
        proxy_send_timeout 60s;
        proxy_read_timeout 60s;
    }

    # Security headers
    add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header X-XSS-Protection "1; mode=block" always;
    add_header Strict-Transport-Security "max-age=31536000" always;
}
```

#### 8.8.2 Firewall Configuration

**ufw (Ubuntu):**
```bash
# Allow SSH
ufw allow 22/tcp

# Allow HTTPS only (nginx reverse proxy)
ufw allow 443/tcp

# Block direct access to application port
ufw deny 8080/tcp

# Enable firewall
ufw enable
```

**firewalld (RHEL/CentOS):**
```bash
firewall-cmd --permanent --add-service=https
firewall-cmd --permanent --add-service=ssh
firewall-cmd --reload
```

### 8.9 Deployment Checklist

#### 8.9.1 Pre-Deployment Checklist

- [ ] All unit tests passing
- [ ] Integration tests passing
- [ ] Performance benchmarks meet requirements
- [ ] Security audit completed
- [ ] Database migration scripts tested
- [ ] Backup and restore procedures tested
- [ ] Configuration files reviewed (no secrets in version control)
- [ ] Dependencies versions locked
- [ ] Build artifacts generated for target platforms
- [ ] Documentation updated

#### 8.9.2 Deployment Steps

1. **Backup current production database** (if upgrading)
2. **Stop current service** (if upgrading)
3. **Deploy new binaries**
4. **Run database migrations**
5. **Update configuration files**
6. **Start service**
7. **Verify health check endpoint**
8. **Monitor logs for errors**
9. **Run smoke tests**
10. **Monitor performance metrics**

#### 8.9.3 Rollback Plan

```bash
#!/bin/bash
# rollback.sh

VERSION_TO_ROLLBACK="$1"

# Stop current service
systemctl stop taskmanager

# Restore previous binary
cp "/opt/taskmanager/backups/taskmanager_main.${VERSION_TO_ROLLBACK}" \
   /opt/taskmanager/bin/taskmanager_main

# Restore previous database
cp "/var/backups/taskmanager/before_migration.db" \
   /var/lib/taskmanager/production.db

# Start service
systemctl start taskmanager

# Verify
sleep 5
systemctl status taskmanager
```

### 8.10 Scaling Roadmap

#### 8.10.1 Small Scale (1-100 users)
- **Architecture:** Embedded library or single server
- **Database:** SQLite
- **Hosting:** Single VPS (2 CPU, 4GB RAM)
- **Cost:** $10-20/month

#### 8.10.2 Medium Scale (100-1,000 users)
- **Architecture:** Client-server with load balancer
- **Database:** PostgreSQL with read replicas
- **Hosting:** 2-3 application servers + database server
- **Cost:** $100-300/month
- **Features:** Redis caching, CDN for static assets

#### 8.10.3 Large Scale (1,000-10,000 users)
- **Architecture:** Microservices
- **Database:** PostgreSQL cluster with connection pooling (PgBouncer)
- **Hosting:** Kubernetes cluster (3-5 nodes)
- **Cost:** $500-2,000/month
- **Features:** Message queue (RabbitMQ), Elasticsearch for search, monitoring (Prometheus/Grafana)

#### 8.10.4 Enterprise Scale (10,000+ users)
- **Architecture:** Multi-region microservices
- **Database:** Distributed PostgreSQL (Citus) or move to cloud-native DB
- **Hosting:** Multi-region Kubernetes with auto-scaling
- **Cost:** $2,000+/month
- **Features:** Full observability stack, chaos engineering, blue-green deployments

---
