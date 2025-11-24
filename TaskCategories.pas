
unit TaskCategories;

{$mode objfpc}

interface

uses
  SysUtils, DateUtils;

type
  { Category information }
  TTaskCategory = record
    id: integer;
    name: string;
    description: string;
    parentCategoryId: integer;     { For hierarchical categories, -1 if root }
    color: string;                 { Hex color code for UI }
    isActive: boolean;
  end;

  TCategoryArray = array of TTaskCategory;
  TCategoryIDArray = array of integer;

  { Category hierarchy management }
  TTaskCategoryManagerClass = class
  private
    categories: TCategoryArray;
    nextCategoryId: integer;
    function FindCategoryIndex(id: integer): integer;
  public
    constructor Create();
    destructor Destroy();
    
    function AddCategory(name, description: string; parentId: integer; color: string): integer;
    function GetCategory(id: integer): TTaskCategory;
    function UpdateCategory(id: integer; newCategory: TTaskCategory): boolean;
    function DeleteCategory(id: integer): boolean;
    function GetAllCategories(): TCategoryArray;
    function GetChildCategories(parentId: integer): TCategoryArray;
    function GetCategoryPath(id: integer): string;
    function GetCategoryDepth(id: integer): integer;
    function GetCategoryCount(): integer;
    function IsCategoryActive(id: integer): boolean;
    function SetCategoryActive(id: integer; active: boolean): boolean;
    function GetSubcategoryCount(parentId: integer): integer;
    function CanDeleteCategory(id: integer): boolean;
    function RenameCategory(id: integer; newName: string): boolean;
    function MoveCategory(categoryId: integer; newParentId: integer): boolean;
    function GetCategoryTree(parentId: integer; indent: string): string;
    procedure SelfTest();
  end;

implementation

constructor TTaskCategoryManagerClass.Create();
begin
  SetLength(categories, 0);
  nextCategoryId := 1;
end;

destructor TTaskCategoryManagerClass.Destroy();
begin
  SetLength(categories, 0);
  inherited Destroy();
end;

function TTaskCategoryManagerClass.FindCategoryIndex(id: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(categories) - 1 do
  begin
    if categories[i].id = id then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskCategoryManagerClass.AddCategory(name, description: string; parentId: integer; color: string): integer;
var
  idx: integer;
  parentIdx: integer;
begin
  { Validate parent exists if specified }
  if parentId <> -1 then
  begin
    parentIdx := FindCategoryIndex(parentId);
    if parentIdx = -1 then
    begin
      result := -1;
      exit;
    end;
  end;

  idx := Length(categories);
  SetLength(categories, idx + 1);
  
  categories[idx].id := nextCategoryId;
  categories[idx].name := name;
  categories[idx].description := description;
  categories[idx].parentCategoryId := parentId;
  categories[idx].color := color;
  categories[idx].isActive := true;
  
  result := nextCategoryId;
  inc(nextCategoryId);
end;

function TTaskCategoryManagerClass.GetCategory(id: integer): TTaskCategory;
var
  idx: integer;
begin
  idx := FindCategoryIndex(id);
  if idx >= 0 then
    result := categories[idx]
  else
  begin
    result.id := -1;
    result.name := '';
    result.description := '';
    result.parentCategoryId := -1;
    result.color := '';
    result.isActive := false;
  end;
end;

function TTaskCategoryManagerClass.UpdateCategory(id: integer; newCategory: TTaskCategory): boolean;
var
  idx: integer;
begin
  idx := FindCategoryIndex(id);
  if idx >= 0 then
  begin
    categories[idx] := newCategory;
    categories[idx].id := id;
    result := true;
  end
  else
    result := false;
end;

function TTaskCategoryManagerClass.DeleteCategory(id: integer): boolean;
var
  idx, i: integer;
begin
  { Cannot delete if has children }
  if GetSubcategoryCount(id) > 0 then
  begin
    result := false;
    exit;
  end;

  idx := FindCategoryIndex(id);
  if idx >= 0 then
  begin
    for i := idx to Length(categories) - 2 do
      categories[i] := categories[i + 1];
    SetLength(categories, Length(categories) - 1);
    result := true;
  end
  else
    result := false;
end;

function TTaskCategoryManagerClass.GetAllCategories(): TCategoryArray;
begin
  result := Copy(categories, 0, Length(categories));
end;

function TTaskCategoryManagerClass.GetChildCategories(parentId: integer): TCategoryArray;
var
  i, count: integer;
begin
  count := 0;
  SetLength(result, 0);
  
  for i := 0 to Length(categories) - 1 do
  begin
    if categories[i].parentCategoryId = parentId then
    begin
      SetLength(result, count + 1);
      result[count] := categories[i];
      inc(count);
    end;
  end;
end;

function TTaskCategoryManagerClass.GetCategoryPath(id: integer): string;
var
  cat: TTaskCategory;
  path: string;
begin
  path := '';
  cat := GetCategory(id);
  
  while cat.id <> -1 do
  begin
    if path = '' then
      path := cat.name
    else
      path := cat.name + ' > ' + path;
    
    if cat.parentCategoryId = -1 then
      break;
    cat := GetCategory(cat.parentCategoryId);
  end;
  
  result := path;
end;

function TTaskCategoryManagerClass.GetCategoryDepth(id: integer): integer;
var
  depth: integer;
  cat: TTaskCategory;
begin
  depth := 0;
  cat := GetCategory(id);
  
  while cat.parentCategoryId <> -1 do
  begin
    inc(depth);
    cat := GetCategory(cat.parentCategoryId);
    if cat.id = -1 then
      break;
  end;
  
  result := depth;
end;

function TTaskCategoryManagerClass.GetCategoryCount(): integer;
begin
  result := Length(categories);
end;

function TTaskCategoryManagerClass.IsCategoryActive(id: integer): boolean;
var
  idx: integer;
begin
  idx := FindCategoryIndex(id);
  if idx >= 0 then
    result := categories[idx].isActive
  else
    result := false;
end;

function TTaskCategoryManagerClass.SetCategoryActive(id: integer; active: boolean): boolean;
var
  idx: integer;
begin
  idx := FindCategoryIndex(id);
  if idx >= 0 then
  begin
    categories[idx].isActive := active;
    result := true;
  end
  else
    result := false;
end;

function TTaskCategoryManagerClass.GetSubcategoryCount(parentId: integer): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(categories) - 1 do
  begin
    if categories[i].parentCategoryId = parentId then
      inc(count);
  end;
  result := count;
end;

function TTaskCategoryManagerClass.CanDeleteCategory(id: integer): boolean;
begin
  result := GetSubcategoryCount(id) = 0;
end;

function TTaskCategoryManagerClass.RenameCategory(id: integer; newName: string): boolean;
var
  idx: integer;
begin
  idx := FindCategoryIndex(id);
  if idx >= 0 then
  begin
    categories[idx].name := newName;
    result := true;
  end
  else
    result := false;
end;

function TTaskCategoryManagerClass.MoveCategory(categoryId: integer; newParentId: integer): boolean;
var
  idx: integer;
begin
  idx := FindCategoryIndex(categoryId);
  if idx >= 0 then
  begin
    { Prevent moving to self }
    if categoryId = newParentId then
    begin
      result := false;
      exit;
    end;
    categories[idx].parentCategoryId := newParentId;
    result := true;
  end
  else
    result := false;
end;

function TTaskCategoryManagerClass.GetCategoryTree(parentId: integer; indent: string): string;
var
  children: TCategoryArray;
  i: integer;
  result_str: string;
begin
  children := GetChildCategories(parentId);
  result_str := '';
  
  for i := 0 to Length(children) - 1 do
  begin
    result_str := result_str + indent + '├─ ' + children[i].name + ' [' + children[i].color + ']' + #10;
    result_str := result_str + GetCategoryTree(children[i].id, indent + '  ');
  end;
  
  result := result_str;
end;

procedure TTaskCategoryManagerClass.SelfTest();
var
  root_id, sub1_id, sub2_id, sub1_1_id: integer;
  cats: TCategoryArray;
begin
  WriteLn('=== TaskCategories Manager Self Test ===');
  
  { Add root categories }
  root_id := AddCategory('Work', 'Work related tasks', -1, '#FF0000');
  WriteLn('Added root category: ', root_id);
  
  sub1_id := AddCategory('Development', 'Dev tasks', root_id, '#FF5500');
  sub2_id := AddCategory('Testing', 'Test tasks', root_id, '#00FF00');
  WriteLn('Added subcategories: ', sub1_id, ', ', sub2_id);
  
  sub1_1_id := AddCategory('Backend', 'Backend development', sub1_id, '#FF8800');
  WriteLn('Added nested subcategory: ', sub1_1_id);
  
  { Get category path }
  WriteLn('Category path for Backend: ', GetCategoryPath(sub1_1_id));
  WriteLn('Category depth: ', GetCategoryDepth(sub1_1_id));
  WriteLn('Subcategory count for Work: ', GetSubcategoryCount(root_id));
  
  { Test renaming }
  if RenameCategory(sub1_id, 'Software Development') then
    WriteLn('✓ Category renamed successfully');
  
  { Display hierarchy }
  WriteLn('Category hierarchy:');
  WriteLn(GetCategoryTree(-1, ''));
  
  WriteLn('=== TaskCategories Manager Self Test Complete ===');
end;

end.
