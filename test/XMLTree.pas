{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

program XMLTree;

uses SysUtils, Classes, Generics.Collections;

type
  TXMLAttributeList = class(specialize TDictionary<String, String>)
  public
    function ToString: String; override;
  end;

  TXMLNode = class
  private
    var
      FName: String;
      FAttributes: TXMLAttributeList;
      FText: String;
      FParent: TXMLNode;
      FChild: TXMLNode;
      FSibling: TXMLNode;
  public
    constructor Create(Name: String; IDStr: String = ''; 
      TextStr: String = '');
    destructor Destroy; override;
    function ToString(Indent: Integer = 0): String;
    procedure AddAttribute(Key, Value: String);
    function AddChild(Child: TXMLNode): TXMLNode;
    function AddSibling(Sibling: TXMLNode): TXMLNode;
    function AddParent(Parent: TXMLNode): TXMLNode;
  end;

function XMLAttributeString(Key, Value: String): String;
begin
  result := Format('%s="%s"', [Key, Value]);
end;

function TXMLAttributeList.ToString: String;
var
  ThisAttribute: TXMLAttributeList.TDictionaryPair;
  XML: String = '';
  Index: Integer = 0;
begin
  if Count > 0 then 
  begin
    for ThisAttribute in Self do
    begin
      if Index > 0 then 
        XML := XML + ' '; 
      
      XML := XML + XMLAttributeString(ThisAttribute.Key, 
        ThisAttribute.Value); 

      Inc(Index);
    end;
  end;
  result := XML;
end;

constructor TXMLNode.Create(Name: String; IDStr: String = ''; 
  TextStr: String = ''); 
var
  NewGUID: TGUID;
  Test: Integer;
begin
  inherited Create;
  FName := Name;
 
  if IDStr = '' then
  begin
    Test := CreateGUID(NewGUID);
    if Test = 0 then
      IDStr := GUIDToString(NewGUID).Trim(['{', '}']);
  end;

  IDStr := Format('%s_%s', [Name, IDStr]);

  FAttributes := TXMLAttributeList.Create;
  FAttributes.Add('xml:id', IDStr);

  FText := TextStr;
  FParent := nil;
  FChild := nil;
  FSibling := nil;
end;

destructor TXMLNode.Destroy;
begin
  FreeAndNil(FAttributes);

  if Assigned(FChild) then
    FChild.Destroy;

  if Assigned(FSibling) then
    FSibling.Destroy;

  inherited Destroy;
end;

function TXMLNode.ToString(Indent: Integer = 0): String;
var
  XML, Attributes, IndentStr: String;
begin
  IndentStr := Space(Indent * 2);
  Attributes := FAttributes.ToString;

  if Assigned(FChild) then
  begin
    FText := FText + LineEnding
              + FChild.ToString(Indent + 1) + LineEnding + IndentStr;
  end;

  XML := Format('%s<%s %s>%s</%s>', 
          [IndentStr, FName, Attributes, FText, FName]);
  
  if Assigned(FSibling) then
    XML := XML + LineEnding + FSibling.ToString(Indent);

  result := XML;
end;

procedure TXMLNode.AddAttribute(Key, Value: String);
begin
  FAttributes.Add(Key, Value);
end;

function TXMLNode.AddParent(Parent: TXMLNode): TXMLNode;
begin
  FParent := Parent;
  if Assigned(Parent) then
    Parent.FChild:= Self;

  result := Self;
end;

function TXMLNode.AddChild(Child: TXMLNode): TXMLNode;
begin
  FChild := Child;
  if Assigned(Child) then
    Child.FParent := Self;

  result := Self;
end;

function TXMLNode.AddSibling(Sibling: TXMLNode): TXMLNode;
begin
  FSibling := Sibling;
  if Assigned(Sibling) then
    Sibling.FParent := FParent;

  result := Self;
end;

function XMLDocument(Tree: TXMLNode): String;
const
 XMLDeclaration: String = '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding;
var
 XML: String = '';
begin
  if Assigned(Tree) then
    XML := XMLDeclaration + Tree.ToString;

  result := XML;
end;

var
  Measure, Staff, Layer, Note1, Note2, Note1Accid: TXMLNode;
begin
  Measure := TXMLNode.Create('measure');
  Measure.AddAttribute('n', '1');

  Staff := TXMLNode.Create('staff', 's-Soprano');
  Staff.AddAttribute('n', '1');

  Layer := TXMLNode.Create('layer', 'soprano');
  Layer.AddAttribute('n', '1');

  Note1 := TXMLNode.Create('note');
  Note1.AddAttribute('pname', 'c');
  Note1.AddAttribute('dur', '4');

  Note1Accid := TXMLNode.Create('accid');
  Note1Accid.AddAttribute('accid', 'f');

  Note2 := TXMLNode.Create('note');
  Note2.AddAttribute('pname', 'c');
  Note2.AddAttribute('dur', '4');

  Measure.AddChild(Staff);
  Staff.AddChild(Layer);
  Layer.AddChild(Note1);
  Note1.AddSibling(Note2);
  Note1.AddChild(Note1Accid);

  WriteLn(XMLDocument(Measure));
  FreeAndNil(Measure);

end.
