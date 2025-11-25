unit TaskSLAMonitor;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  Math,
  TaskTypes,
  TaskManager;

type
  { SLA Configuration }
  TSLA = record
    slaId: integer;
    category: string;
    priority: TTaskPriority;
    responseTimeHours: double;
    resolutionTimeHours: double;
    escalationHours: double;
    enabled: boolean;
  end;

  TSLAArray = array of TSLA;

  { SLA Status for a task }
  TSLAStatus = (
    slaHealthy,
    slaAtRisk,
    slaBreached,
    slaNotApplicable
  );

  TSLAAlert = record
    alertId: integer;
    taskId: integer;
    taskName: string;
    slaId: integer;
    alertType: string;
    severity: TTaskPriority;
    message: string;
    createdDate: TDateTime;
    acknowledged: boolean;
  end;

  TSLAAlertArray = array of TSLAAlert;

  TSLAMetrics = record
    totalTasksMonitored: integer;
    slaCompliantTasks: integer;
    slaBreachedTasks: integer;
    slaAtRiskTasks: integer;
    complianceRate: double;
    averageResponseTime: double;
    averageResolutionTime: double;
  end;

  { Task SLA Monitor class }
  TTaskSLAMonitor = class
  private
    slas: TSLAArray;
    alerts: TSLAAlertArray;
    nextAlertId: integer;
    nextSLAId: integer;
    lastError: string;
    manager: TTaskManager;
    
    function findSLAIndex(slaId: integer): integer;
    function findAlertIndex(alertId: integer): integer;
    function calculateTimeElapsed(createdDate, now: TDateTime): double;
    function getSLAForTask(var task: TTask): integer;
    
  public
    constructor Create(aManager: TTaskManager);
    destructor Destroy;
    
    { SLA Management }
    function addSLA(const category: string; priority: TTaskPriority;
                   responseHours, resolutionHours, escalationHours: double): integer;
    function updateSLA(slaId: integer; const category: string;
                      priority: TTaskPriority; responseHours,
                      resolutionHours, escalationHours: double): boolean;
    function deleteSLA(slaId: integer): boolean;
    function getSLA(slaId: integer; var sla: TSLA): boolean;
    function getAllSLAs: TSLAArray;
    function getSLACount: integer;
    
    { SLA Monitoring }
    function checkTaskSLAStatus(taskId: integer): TSLAStatus;
    function checkAllSLAs: integer;
    function getTaskSLACompliancePercent(taskId: integer): double;
    
    { Alert Management }
    function getAlerts: TSLAAlertArray;
    function getUnacknowledgedAlerts: TSLAAlertArray;
    function acknowledgeAlert(alertId: integer): boolean;
    function clearAlert(alertId: integer): boolean;
    function getAlertCount: integer;
    function getUnacknowledgedAlertCount: integer;
    
    { Metrics and Reporting }
    function calculateMetrics: TSLAMetrics;
    function getTasksSLAAtRisk: TTaskArray;
    function getTasksSLABreached: TTaskArray;
    function getTasksCompliantWithSLA: TTaskArray;
    function getSLAReport: string;
    function getComplianceByCategory(const category: string): double;
    function getComplianceByPriority(priority: TTaskPriority): double;
    
    { Utility }
    function getLastError: string;
    procedure clearError;
  end;

implementation

constructor TTaskSLAMonitor.Create(aManager: TTaskManager);
begin
  manager := aManager;
  nextSLAId := 1;
  nextAlertId := 1;
  setLength(slas, 0);
  setLength(alerts, 0);
  lastError := '';
end;

destructor TTaskSLAMonitor.Destroy;
begin
  setLength(slas, 0);
  setLength(alerts, 0);
  inherited Destroy;
end;

function TTaskSLAMonitor.findSLAIndex(slaId: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to length(slas) - 1 do
  begin
    if slas[i].slaId = slaId then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskSLAMonitor.findAlertIndex(alertId: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to length(alerts) - 1 do
  begin
    if alerts[i].alertId = alertId then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskSLAMonitor.calculateTimeElapsed(createdDate, now: TDateTime): double;
begin
  result := (now - createdDate) * 24; { Convert to hours }
end;

function TTaskSLAMonitor.getSLAForTask(var task: TTask): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to length(slas) - 1 do
  begin
    if (slas[i].enabled) and
       (slas[i].category = task.category) and
       (slas[i].priority = task.priority) then
    begin
      result := slas[i].slaId;
      exit;
    end;
  end;
end;

function TTaskSLAMonitor.addSLA(const category: string; priority: TTaskPriority;
                               responseHours, resolutionHours, escalationHours: double): integer;
var
  newSLA: TSLA;
  idx: integer;
begin
  if responseHours <= 0 then
  begin
    lastError := 'Response time must be positive';
    result := -1;
    exit;
  end;
  
  if resolutionHours <= 0 then
  begin
    lastError := 'Resolution time must be positive';
    result := -1;
    exit;
  end;
  
  if escalationHours < 0 then
  begin
    lastError := 'Escalation time cannot be negative';
    result := -1;
    exit;
  end;
  
  newSLA.slaId := nextSLAId;
  newSLA.category := category;
  newSLA.priority := priority;
  newSLA.responseTimeHours := responseHours;
  newSLA.resolutionTimeHours := resolutionHours;
  newSLA.escalationHours := escalationHours;
  newSLA.enabled := true;
  
  idx := length(slas);
  setLength(slas, idx + 1);
  slas[idx] := newSLA;
  
  result := nextSLAId;
  inc(nextSLAId);
  lastError := '';
end;

function TTaskSLAMonitor.updateSLA(slaId: integer; const category: string;
                                  priority: TTaskPriority; responseHours,
                                  resolutionHours, escalationHours: double): boolean;
var
  idx: integer;
begin
  idx := findSLAIndex(slaId);
  if idx < 0 then
  begin
    lastError := 'SLA not found';
    result := false;
    exit;
  end;
  
  if responseHours <= 0 then
  begin
    lastError := 'Response time must be positive';
    result := false;
    exit;
  end;
  
  slas[idx].category := category;
  slas[idx].priority := priority;
  slas[idx].responseTimeHours := responseHours;
  slas[idx].resolutionTimeHours := resolutionHours;
  slas[idx].escalationHours := escalationHours;
  
  result := true;
  lastError := '';
end;

function TTaskSLAMonitor.deleteSLA(slaId: integer): boolean;
var
  idx, i: integer;
begin
  idx := findSLAIndex(slaId);
  if idx < 0 then
  begin
    lastError := 'SLA not found';
    result := false;
    exit;
  end;
  
  for i := idx to length(slas) - 2 do
    slas[i] := slas[i + 1];
  
  setLength(slas, length(slas) - 1);
  result := true;
  lastError := '';
end;

function TTaskSLAMonitor.getSLA(slaId: integer; var sla: TSLA): boolean;
var
  idx: integer;
begin
  idx := findSLAIndex(slaId);
  if idx < 0 then
  begin
    lastError := 'SLA not found';
    result := false;
    exit;
  end;
  
  sla := slas[idx];
  result := true;
  lastError := '';
end;

function TTaskSLAMonitor.getAllSLAs: TSLAArray;
begin
  result := copy(slas);
end;

function TTaskSLAMonitor.getSLACount: integer;
begin
  result := length(slas);
end;

function TTaskSLAMonitor.checkTaskSLAStatus(taskId: integer): TSLAStatus;
var
  task: TTask;
  slaId: integer;
  idx: integer;
  timeElapsed: double;
begin
  if not manager.getTask(taskId, task) then
  begin
    result := slaNotApplicable;
    exit;
  end;
  
  slaId := getSLAForTask(task);
  if slaId < 0 then
  begin
    result := slaNotApplicable;
    exit;
  end;
  
  idx := findSLAIndex(slaId);
  if idx < 0 then
  begin
    result := slaNotApplicable;
    exit;
  end;
  
  timeElapsed := calculateTimeElapsed(task.createdDate, now);
  
  if task.status = tsCompleted then
  begin
    if timeElapsed <= slas[idx].resolutionTimeHours then
      result := slaHealthy
    else
      result := slaBreached;
  end
  else
  begin
    if timeElapsed > slas[idx].resolutionTimeHours then
      result := slaBreached
    else if timeElapsed > slas[idx].escalationHours then
      result := slaAtRisk
    else
      result := slaHealthy;
  end;
end;

function TTaskSLAMonitor.checkAllSLAs: integer;
var
  i: integer;
  status: TSLAStatus;
  task: TTask;
  newAlert: TSLAAlert;
  idx: integer;
  slaId: integer;
  alertsCreated: integer;
begin
  alertsCreated := 0;
  
  for i := 1 to manager.getTaskCount do
  begin
    if not manager.getTask(i, task) then
      continue;
    
    status := checkTaskSLAStatus(i);
    
    if (status = slaAtRisk) or (status = slaBreached) then
    begin
      slaId := getSLAForTask(task);
      if slaId < 0 then
        continue;
      
      idx := length(alerts);
      setLength(alerts, idx + 1);
      
      newAlert.alertId := nextAlertId;
      newAlert.taskId := i;
      newAlert.taskName := task.name;
      newAlert.slaId := slaId;
      newAlert.createdDate := now;
      newAlert.acknowledged := false;
      
      if status = slaBreached then
      begin
        newAlert.alertType := 'SLA Breached';
        newAlert.severity := task.priority;
        newAlert.message := 'SLA has been breached for task: ' + task.name;
      end
      else
      begin
        newAlert.alertType := 'SLA At Risk';
        newAlert.severity := task.priority;
        newAlert.message := 'SLA at risk for task: ' + task.name;
      end;
      
      alerts[idx] := newAlert;
      inc(nextAlertId);
      inc(alertsCreated);
    end;
  end;
  
  result := alertsCreated;
end;

function TTaskSLAMonitor.getTaskSLACompliancePercent(taskId: integer): double;
var
  task: TTask;
  slaId: integer;
  idx: integer;
  timeElapsed: double;
begin
  if not manager.getTask(taskId, task) then
  begin
    result := 0;
    exit;
  end;
  
  slaId := getSLAForTask(task);
  if slaId < 0 then
  begin
    result := 100;
    exit;
  end;
  
  idx := findSLAIndex(slaId);
  if idx < 0 then
  begin
    result := 100;
    exit;
  end;
  
  timeElapsed := calculateTimeElapsed(task.createdDate, now);
  
  if task.status = tsCompleted then
  begin
    if timeElapsed <= slas[idx].resolutionTimeHours then
      result := 100
    else
      result := (slas[idx].resolutionTimeHours / timeElapsed) * 100;
  end
  else
  begin
    result := (slas[idx].resolutionTimeHours - timeElapsed) / slas[idx].resolutionTimeHours * 100;
    if result < 0 then
      result := 0;
  end;
  
  if result > 100 then
    result := 100;
end;

function TTaskSLAMonitor.getAlerts: TSLAAlertArray;
begin
  result := copy(alerts);
end;

function TTaskSLAMonitor.getUnacknowledgedAlerts: TSLAAlertArray;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to length(alerts) - 1 do
  begin
    if not alerts[i].acknowledged then
      inc(count);
  end;
  
  setLength(result, count);
  count := 0;
  for i := 0 to length(alerts) - 1 do
  begin
    if not alerts[i].acknowledged then
    begin
      result[count] := alerts[i];
      inc(count);
    end;
  end;
end;

function TTaskSLAMonitor.acknowledgeAlert(alertId: integer): boolean;
var
  idx: integer;
begin
  idx := findAlertIndex(alertId);
  if idx < 0 then
  begin
    lastError := 'Alert not found';
    result := false;
    exit;
  end;
  
  alerts[idx].acknowledged := true;
  result := true;
  lastError := '';
end;

function TTaskSLAMonitor.clearAlert(alertId: integer): boolean;
var
  idx, i: integer;
begin
  idx := findAlertIndex(alertId);
  if idx < 0 then
  begin
    lastError := 'Alert not found';
    result := false;
    exit;
  end;
  
  for i := idx to length(alerts) - 2 do
    alerts[i] := alerts[i + 1];
  
  setLength(alerts, length(alerts) - 1);
  result := true;
  lastError := '';
end;

function TTaskSLAMonitor.getAlertCount: integer;
begin
  result := length(alerts);
end;

function TTaskSLAMonitor.getUnacknowledgedAlertCount: integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to length(alerts) - 1 do
  begin
    if not alerts[i].acknowledged then
      inc(count);
  end;
  result := count;
end;

function TTaskSLAMonitor.calculateMetrics: TSLAMetrics;
var
  i: integer;
  status: TSLAStatus;
  totalTime, compliantTime, breachedTime: double;
begin
  result.totalTasksMonitored := 0;
  result.slaCompliantTasks := 0;
  result.slaBreachedTasks := 0;
  result.slaAtRiskTasks := 0;
  result.complianceRate := 0;
  result.averageResponseTime := 0;
  result.averageResolutionTime := 0;
  
  for i := 1 to manager.getTaskCount do
  begin
    status := checkTaskSLAStatus(i);
    if status <> slaNotApplicable then
    begin
      inc(result.totalTasksMonitored);
      case status of
        slaHealthy: inc(result.slaCompliantTasks);
        slaAtRisk: inc(result.slaAtRiskTasks);
        slaBreached: inc(result.slaBreachedTasks);
      end;
    end;
  end;
  
  if result.totalTasksMonitored > 0 then
    result.complianceRate := (result.slaCompliantTasks / result.totalTasksMonitored) * 100;
end;

function TTaskSLAMonitor.getTasksSLAAtRisk: TTaskArray;
var
  i, count: integer;
  status: TSLAStatus;
  task: TTask;
begin
  count := 0;
  for i := 1 to manager.getTaskCount do
  begin
    status := checkTaskSLAStatus(i);
    if status = slaAtRisk then
      inc(count);
  end;
  
  setLength(result, count);
  count := 0;
  for i := 1 to manager.getTaskCount do
  begin
    if manager.getTask(i, task) then
    begin
      status := checkTaskSLAStatus(i);
      if status = slaAtRisk then
      begin
        result[count] := task;
        inc(count);
      end;
    end;
  end;
end;

function TTaskSLAMonitor.getTasksSLABreached: TTaskArray;
var
  i, count: integer;
  status: TSLAStatus;
  task: TTask;
begin
  count := 0;
  for i := 1 to manager.getTaskCount do
  begin
    status := checkTaskSLAStatus(i);
    if status = slaBreached then
      inc(count);
  end;
  
  setLength(result, count);
  count := 0;
  for i := 1 to manager.getTaskCount do
  begin
    if manager.getTask(i, task) then
    begin
      status := checkTaskSLAStatus(i);
      if status = slaBreached then
      begin
        result[count] := task;
        inc(count);
      end;
    end;
  end;
end;

function TTaskSLAMonitor.getTasksCompliantWithSLA: TTaskArray;
var
  i, count: integer;
  status: TSLAStatus;
  task: TTask;
begin
  count := 0;
  for i := 1 to manager.getTaskCount do
  begin
    status := checkTaskSLAStatus(i);
    if status = slaHealthy then
      inc(count);
  end;
  
  setLength(result, count);
  count := 0;
  for i := 1 to manager.getTaskCount do
  begin
    if manager.getTask(i, task) then
    begin
      status := checkTaskSLAStatus(i);
      if status = slaHealthy then
      begin
        result[count] := task;
        inc(count);
      end;
    end;
  end;
end;

function TTaskSLAMonitor.getSLAReport: string;
var
  metrics: TSLAMetrics;
  i: integer;
  allSLAs: TSLAArray;
begin
  metrics := calculateMetrics;
  result := '';
  result := result + '===== SLA COMPLIANCE REPORT =====' + #13#10;
  result := result + 'Total Tasks Monitored: ' + intToStr(metrics.totalTasksMonitored) + #13#10;
  result := result + 'Compliant: ' + intToStr(metrics.slaCompliantTasks) + #13#10;
  result := result + 'At Risk: ' + intToStr(metrics.slaAtRiskTasks) + #13#10;
  result := result + 'Breached: ' + intToStr(metrics.slaBreachedTasks) + #13#10;
  result := result + 'Overall Compliance Rate: ' + FloatToStrF(metrics.complianceRate, ffFixed, 5, 2) + '%' + #13#10;
  result := result + #13#10;
  
  allSLAs := getAllSLAs;
  result := result + 'Configured SLAs:' + #13#10;
  for i := 0 to length(allSLAs) - 1 do
  begin
    result := result + '  - ' + allSLAs[i].category + ' (' + intToStr(integer(allSLAs[i].priority)) + '): ' +
              FloatToStrF(allSLAs[i].responseTimeHours, ffFixed, 5, 1) + 'h response, ' +
              FloatToStrF(allSLAs[i].resolutionTimeHours, ffFixed, 5, 1) + 'h resolution' + #13#10;
  end;
end;

function TTaskSLAMonitor.getComplianceByCategory(const category: string): double;
var
  i: integer;
  task: TTask;
  compliantCount, totalCount: integer;
  status: TSLAStatus;
begin
  compliantCount := 0;
  totalCount := 0;
  
  for i := 1 to manager.getTaskCount do
  begin
    if manager.getTask(i, task) then
    begin
      if task.category = category then
      begin
        status := checkTaskSLAStatus(i);
        if status <> slaNotApplicable then
        begin
          inc(totalCount);
          if status = slaHealthy then
            inc(compliantCount);
        end;
      end;
    end;
  end;
  
  if totalCount > 0 then
    result := (compliantCount / totalCount) * 100
  else
    result := 0;
end;

function TTaskSLAMonitor.getComplianceByPriority(priority: TTaskPriority): double;
var
  i: integer;
  task: TTask;
  compliantCount, totalCount: integer;
  status: TSLAStatus;
begin
  compliantCount := 0;
  totalCount := 0;
  
  for i := 1 to manager.getTaskCount do
  begin
    if manager.getTask(i, task) then
    begin
      if task.priority = priority then
      begin
        status := checkTaskSLAStatus(i);
        if status <> slaNotApplicable then
        begin
          inc(totalCount);
          if status = slaHealthy then
            inc(compliantCount);
        end;
      end;
    end;
  end;
  
  if totalCount > 0 then
    result := (compliantCount / totalCount) * 100
  else
    result := 0;
end;

function TTaskSLAMonitor.getLastError: string;
begin
  result := lastError;
end;

procedure TTaskSLAMonitor.clearError;
begin
  lastError := '';
end;

end.
