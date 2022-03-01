{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}
program series(output);

{ Make a numbered series of XML siblings and select them by number like an
  array. }

uses SysUtils, MEI;

var
  Root, M1, M2, M3, M4, M5, ThisNode, SelectedNode: TMeiNode;
  ThisCount: Integer;
const
  Max: Integer = 5;

begin
  Root := TMeiNode.CreateMeiRoot();
  M1 := TMeiNode.Create('measure');
  M2 := TMeiNode.Create('measure');
  M3 := TMeiNode.Create('measure');
  M4 := TMeiNode.Create('measure');
  M5 := TMeiNode.Create('measure');

  for ThisCount := 1 to Max do
  begin
    ThisNode := nil;
    case ThisCount of
      1 : ThisNode := M1;
      2 : ThisNode := M2;
      3 : ThisNode := M3;
      4 : ThisNode := M4;
      5 : ThisNode := M5;
    end;
    ThisNode.AddAttribute('n', Format('%d', [ThisCount]));
  
    if ThisCount = 1 then
      Root.AppendChild(ThisNode)
    else if Assigned(Root.ChildTree) then
      Root.ChildTree.AppendSibling(ThisNode);
  end;

  WriteMeiDocument(Root);

  SelectedNode := TMeiNode.Create();
  SelectedNode.Assign(Root.FindElementByAttribute('measure', 'n', '3'));
  WriteMeiDocument(SelectedNode);
  
  { Need to extract just a single node, not children or siblings }

  FreeAndNil(Root);
  FreeAndNil(SelectedNode);
end.




