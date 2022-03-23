program textNode(output);

{ Test of XML with text nodes }

uses SysUtils, MEI;

var
  Root, Body, P1, P2: TMeiNode;
begin
  Root := TMeiNode.Create('html');
  Root.AddAttribute('lang', 'en-US');

  Body := TMeiNode.Create('body');
  P1 := TMeiNode.Create('p');
  P1.AddAttribute('n', '1');
  P1.AddAttribute('xml:id', 'intro');
  
  P2 := TMeiNode.Create('p');
  P2.AddAttribute('n', '2');
  P2.AddAttribute('xml:id', 'conclusion');

  Root.AppendChild(Body);
  Body.AppendChild(P1);
  P1.AppendSibling(P2);

  P1.SetTextNode('This is the introductory paragraph. It has multiple sentences.');

  P2.SetTextNode('In conclusion, this is the concluding paragraph. This is the end of the statement.');

  WriteMeiDocument(Root);
  FreeAndNil(Root);
end.
