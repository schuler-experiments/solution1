
unit TaskWorkflowApproval;

{$mode objfpc}

interface

uses
  SysUtils,
  DateUtils,
  TaskTypes;

type
  { Approval statuses }
  TApprovalStatus = (asApproved, asRejected, asPending);

  { Approval record }
  TApprovalRecord = record
    transitionId: integer;
    approverName: string;
    approvalStatus: TApprovalStatus;
    approvalDate: TDateTime;
    comments: string;
  end;

  TApprovalArray = array of TApprovalRecord;

  { Workflow approval manager }
  TWorkflowApprovalManager = class
  private
    approvals: TApprovalArray;
    
    function FindApprovalIndex(transitionId: integer): integer;
  public
    constructor Create();
    destructor Destroy(); override;
    
    { Approval operations }
    function ApproveTransition(transitionId: integer; approverName, comments: string): boolean;
    function RejectTransition(transitionId: integer; approverName, comments: string): boolean;
    function IsApprovalRequired(action: integer): boolean;
    function GetApprovalStatus(transitionId: integer): TApprovalStatus;
    function GetPendingApprovals(): TApprovalArray;
    
    { Self test }
    procedure SelfTest();
  end;

implementation

constructor TWorkflowApprovalManager.Create();
begin
  inherited Create();
  SetLength(approvals, 0);
end;

destructor TWorkflowApprovalManager.Destroy();
begin
  SetLength(approvals, 0);
  inherited Destroy();
end;

function TWorkflowApprovalManager.FindApprovalIndex(transitionId: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(approvals) - 1 do
  begin
    if approvals[i].transitionId = transitionId then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TWorkflowApprovalManager.ApproveTransition(transitionId: integer; 
                                                    approverName, comments: string): boolean;
var
  idx: integer;
  approval: TApprovalRecord;
begin
  idx := FindApprovalIndex(transitionId);
  approval.transitionId := transitionId;
  approval.approverName := approverName;
  approval.approvalStatus := asApproved;
  approval.approvalDate := Now();
  approval.comments := comments;
  
  if idx >= 0 then
  begin
    approvals[idx] := approval;
  end
  else
  begin
    SetLength(approvals, Length(approvals) + 1);
    approvals[Length(approvals) - 1] := approval;
  end;
  
  result := true;
end;

function TWorkflowApprovalManager.RejectTransition(transitionId: integer; 
                                                   approverName, comments: string): boolean;
var
  idx: integer;
  approval: TApprovalRecord;
begin
  idx := FindApprovalIndex(transitionId);
  approval.transitionId := transitionId;
  approval.approverName := approverName;
  approval.approvalStatus := asRejected;
  approval.approvalDate := Now();
  approval.comments := comments;
  
  if idx >= 0 then
  begin
    approvals[idx] := approval;
  end
  else
  begin
    SetLength(approvals, Length(approvals) + 1);
    approvals[Length(approvals) - 1] := approval;
  end;
  
  result := true;
end;

function TWorkflowApprovalManager.IsApprovalRequired(action: integer): boolean;
begin
  { Certain actions require approval }
  result := (action > 2);
end;

function TWorkflowApprovalManager.GetApprovalStatus(transitionId: integer): TApprovalStatus;
var
  idx: integer;
begin
  idx := FindApprovalIndex(transitionId);
  if idx >= 0 then
    result := approvals[idx].approvalStatus
  else
    result := asPending;
end;

function TWorkflowApprovalManager.GetPendingApprovals(): TApprovalArray;
var
  i, count: integer;
  pending: TApprovalArray;
begin
  SetLength(pending, 0);
  count := 0;
  
  for i := 0 to Length(approvals) - 1 do
  begin
    if approvals[i].approvalStatus = asPending then
    begin
      SetLength(pending, count + 1);
      pending[count] := approvals[i];
      Inc(count);
    end;
  end;
  
  result := pending;
end;

procedure TWorkflowApprovalManager.SelfTest();


begin
  WriteLn('');
  WriteLn('=== Workflow Approval Manager Self Test ===');
  
  ApproveTransition(1, 'Manager', 'Approved for deployment');
  RejectTransition(2, 'Reviewer', 'Needs more documentation');
  
  WriteLn('Created approval records');
  WriteLn(Format('Pending approvals: %d', [Length(GetPendingApprovals())]));
  
  WriteLn('=== Workflow Approval Manager Self Test Complete ===');
end;

end.
