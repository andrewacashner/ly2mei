{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}
program testMEI(output);

{ Test of basic XML tree-building and writing, 
  including deep-copy of elements 
}

uses SysUtils, MEI;

var 
  Root, Node1, Node2, Node3, Node4, Node5: TMeiNode;
begin
  Root := TMeiNode.CreateMeiRoot();

  Node1 := TMeiNode.Create('measure');
  Node1.AddAttribute('n', '1');

  Node2 := TMeiNode.Create('staff');
  Node2.AddAttribute('n', '1');
  Node2.AddAttribute('label', 'Soprano');
  
  Node3 := TMeiNode.Create('staff');
  Node3.AddAttribute('n', '2');
  Node3.AddAttribute('label', 'Bass');

  Node4 := TMeiNode.Create('measure');
  Node4.AddAttribute('n', '2');

  Root.AppendChild(Node1);
  Node1.AppendChild(Node2);
  Node2.AppendSibling(Node3);
  
  Node5 := TMeiNode.Create();
  Node5.Assign(Node1);
  
  Node1.AppendSibling(Node4);
  Node1.AppendSibling(Node5);

  WriteMeiDocument(Root);
  FreeAndNil(Root);
end.
