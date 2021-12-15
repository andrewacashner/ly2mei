interface
type 
  TMEIElementList = class(specialize TObjectList<TMEIElement>)
  public
    constructor CreateFromTree(Tree: TMEIElement);
    function ToString: String; override;
    function MeasureCount: Integer;
  end;

function CountMeasures(MEITree: TMEIElement): Integer;


implementation
constructor TMEIElementList.CreateFromTree(Tree: TMEIElement);
function InnerTreeToList(Tree: TMEIElement; List: TMEIElementList): TMEIElementList;
var 
  NewElement: TMEIELement;
begin
  assert(List <> nil);
  if Tree <> nil then
    with Tree do
    begin
      NewElement := TMEIElement.Create;
      NewElement.AssignNode(Tree);
      List.Add(NewElement);
      if FChild <> nil then
        List := InnerTreeToList(FChild, List);
      if FSibling <> nil then
        List := InnerTreeToList(FSibling, List);
    end;
  result := List;
end;
begin
  inherited Create;
  Self := InnerTreeToList(Tree, Self);
end;

function TMEIElementList.ToString: String;
var
  ThisElement: TMEIElement;
  ThisElementStr: String;
  OutputStr: String = '';
begin
  for ThisElement in Self do
  begin
    with ThisElement do
    begin
      ThisElementStr := FName + ' ' + FID + ' ' + IntToStr(FNum);
      if FMeasures <> nil then
        ThisElementStr := ThisElementStr + ' (' + IntToStr(FMeasures.Count) + ' measures)';
    end;
    OutputStr := OutputStr + 'ELEMENT: ' + ThisElementStr + ' | ';
  end;
  result := OutputStr;
end;

function TMEIElementList.MeasureCount: Integer;
var
  ThisElement: TMEIElement;
  PrevCount: Integer = 0;
  NewCount: Integer = 0;
begin
  for ThisElement in Self do
  begin
    if ThisElement.FMeasures <> nil then
    begin
      NewCount := ThisElement.FMeasures.Count
    end;
    if (PrevCount = 0) or (PrevCount = NewCount) then
      PrevCount := NewCount
    else
    begin
      NewCount := -1;
      break;
    end;
  end;
  result := NewCount;
end;

function CountMeasures(MEITree: TMEIElement): Integer;
var
  MEIList: TMEIELementList;
  Count: Integer;
begin
  MEIList := TMEIElementList.CreateFromTree(MEITree);
  Count := MEIList.MeasureCount;
  FreeAndNil(MEIList);
  result := Count;
end;



