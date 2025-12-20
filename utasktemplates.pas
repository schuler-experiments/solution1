
unit utasktemplates;

{$mode objfpc}
{$h+}

interface

uses
  SysUtils, Classes, utaskmanager;

type
  ttasktemplate = record
    name: string;
    default_title: string;
    default_description: string;
    default_category: string;
    default_priority: ttaskpriority;
  end;

  ttemplatearray = array of ttasktemplate;

  ttasktemplatemanager = class
  private
    ftemplates: ttemplatearray;
  public
    constructor create;
    destructor destroy; override;
    procedure add_template(const aname, atitle, adescription, acategory: string; apriority: ttaskpriority);
    function apply_template(const aname: string; amanager: ttaskmanager): integer;
  end;

implementation

constructor ttasktemplatemanager.create;
begin
  inherited create;
  setlength(ftemplates, 0);
end;

destructor ttasktemplatemanager.destroy;
begin
  setlength(ftemplates, 0);
  inherited destroy;
end;

procedure ttasktemplatemanager.add_template(const aname, atitle, adescription, acategory: string; apriority: ttaskpriority);
begin
  setlength(ftemplates, length(ftemplates) + 1);
  ftemplates[high(ftemplates)].name := aname;
  ftemplates[high(ftemplates)].default_title := atitle;
  ftemplates[high(ftemplates)].default_description := adescription;
  ftemplates[high(ftemplates)].default_category := acategory;
  ftemplates[high(ftemplates)].default_priority := apriority;
end;

function ttasktemplatemanager.apply_template(const aname: string; amanager: ttaskmanager): integer;
var
  i: integer;
  tags: ttagarray;
begin
  result := -1;
  setlength(tags, 0);
  for i := 0 to high(ftemplates) do
    if ftemplates[i].name = aname then
    begin
      amanager.add_task(ftemplates[i].default_title, ftemplates[i].default_description, ftemplates[i].default_category, tags, ftemplates[i].default_priority, now + 7);
      result := amanager.get_tasks()[high(amanager.get_tasks())].id;
      exit;
    end;
end;

end.
