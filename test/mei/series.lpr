{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}
program series(output);

{ Make a numbered series of XML siblings and select them by number like an
  array. }

uses SysUtils, MEI;

function CreateMeasureN(MeasureNum: Integer): TMeiNode;
var NewMeasure: TMeiNode;
begin
  NewMeasure := TMeiNode.Create('measure');
  NewMeasure.AddAttribute('n', Format('%d', [MeasureNum]));
  result := NewMeasure;
end;

var
  Root, SelectedNode, Layer: TMeiNode;
begin
  Root := TMeiNode.CreateMeiRoot();
  Layer := TMeiNode.Create('layer');
  Layer.AddAttribute('label', 'Soprano');

  Root.AppendChild(
    CreateMeasureN(1).AppendSibling(
      CreateMeasureN(2).AppendSibling(
        CreateMeasureN(3).AppendSibling(
          CreateMeasureN(4).AppendSibling(
            CreateMeasureN(5))))));

  SelectedNode := Root.FindElementByAttribute('measure', 'n', '3');
  SelectedNode.AppendChild(Layer);

  WriteMeiDocument(Root);

  SelectedNode := TMeiNode.Create;
  SelectedNode.AssignWithoutSiblings(
    Root.FindElementByAttribute('measure', 'n', '3'));
  
  FreeAndNil(Root);
  WriteMeiDocument(SelectedNode);

  FreeAndNil(SelectedNode);
end.




