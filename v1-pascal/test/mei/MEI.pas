{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(Write MEI-XML.)
  @author(Andrew Cashner)

  Test of basic (MEI-)XML writing.
}
unit MEI;

interface

uses SysUtils, Classes, Generics.Collections;


type
  TMeiAttributeList = class(specialize TDictionary<String, String>)
  public
    function XMLString: String; 
    procedure Assign(SourceList: TMeiAttributeList);
  end;

  TMEIAttributeKeyValue = TMeiAttributeList.TDictionaryPair;

  TMeiNode = class
  private
    var
      FName: String;
      FAttributes: TMeiAttributeList;
      FChild: TMeiNode;
      FSibling: TMeiNode;
  public
    constructor Create(Name: String = 'xml');
    constructor CreateMeiRoot();
    destructor Destroy(); override;

    function GenerateID: String;
    function XMLString(IndentLevel: Integer = 0): String;

    procedure AddAttribute(Key, Value: String);
    procedure RemoveAttribute(Key: String);

    function ChildTree: TMeiNode;
    function NextSibling: TMeiNode;

    function LastChild: TMeiNode;
    function LastSibling: TMeiNode;

    function AddFirstChild(Child: TMeiNode): TMeiNode;
    function AddFirstSibling(Sibling: TMeiNode): TMeiNode;

    function AppendChild(Child: TMeiNode): TMeiNode;
    function AppendSibling(Sibling: TMeiNode): TMeiNode;

    procedure Assign(SourceNode: TMeiNode);

    function FindElementByAttribute(Name, Key, Value: String): TMeiNode;
  end;

const
  _XMLDeclaration = '<?xml version="1.0" encoding="utf-8"?>';
  _MeiNamespace = 'https://music-encoding.org/ns/mei';
  _MeiVersion = '3.0.0';

procedure WriteMeiDocument(Root: TMeiNode);


implementation

function TMeiAttributeList.XMLString: String;
var 
  ThisPair: TMeiAttributeKeyValue;
  OutputStr: String;
  KeyCount: Integer;
begin
  OutputStr := '';
  KeyCount := Self.Count;
  for ThisPair in Self do
  begin
    OutputStr := OutputStr + Format('%s="%s"', [ThisPair.Key, ThisPair.Value]);
    
    Dec(KeyCount);
    if KeyCount > 0 then
      OutputStr := OutputStr + ' ';
  end;
  result := OutputStr;
end;

procedure TMeiAttributeList.Assign(SourceList: TMeiAttributeList);
var
  ThisPair: TMeiAttributeKeyValue;
begin
  Clear;
  for ThisPair in SourceList do
    Add(ThisPair);
end;

constructor TMeiNode.Create(Name: String = 'xml');
begin
  inherited Create();
  FName := Name;
  
  FAttributes := TMeiAttributeList.Create();
  FAttributes.AddOrSetValue('xml:id', GenerateID());
 
  FChild   := nil;
  FSibling := nil;
end;

constructor TMeiNode.CreateMeiRoot();
begin
  Create('mei');
  RemoveAttribute('xml:id');
  AddAttribute('xml:ns', _MeiNamespace);
  AddAttribute('meiversion', _MeiVersion);
end;

destructor TMeiNode.Destroy();
begin
  FAttributes.Destroy;

  if Assigned(FChild) then
  begin
    FChild.Destroy;
  end;

  if Assigned(FSibling) then
  begin
    FSibling.Destroy;
  end;

  inherited Destroy;
end;

function TMeiNode.GenerateID: String;
var
  GUID: TGUID;
  IDStr: String = '';
begin
  if CreateGUID(GUID) = 0 then
  begin
    IDStr := GUIDToString(GUID);
  end;
  { Make the GUID into a valid NCname }
  result := 'mei-' + IDStr.Substring(1, Length(IDStr) - 2);
end;

function TMeiNode.XMLString(IndentLevel: Integer = 0): String; 

function Indent(Level: Integer): String;
begin
  result := StringOfChar(' ', 2 * Level);
end;

var
  OutputStr: String;
  BasicIndent: String;
begin
  BasicIndent := Indent(IndentLevel);

  if FAttributes.Count = 0 then
    OutputStr := BasicIndent + Format('<%s>', [FName])
  else
    OutputStr := BasicIndent + Format('<%s %s', [FName, FAttributes.XMLString]);
  
  if Assigned(FChild) then
    OutputStr := OutputStr + '>' + LineEnding 
                  + FChild.XMLString(IndentLevel + 1) + LineEnding
                  + BasicIndent + Format('</%s>', [FName])
  else
    OutputStr := OutputStr + Format('/>', [FName]);

  if Assigned(FSibling) then
    OutputStr := OutputStr + LineEnding + FSibling.XMLString(IndentLevel);
  
  result := OutputStr;
end;

procedure TMeiNode.AddAttribute(Key, Value: String);
begin
  Assert(Assigned(FAttributes));
  FAttributes.AddOrSetValue(Key, Value);
end;

procedure TMeiNode.RemoveAttribute(Key: String);
begin
  Assert(Assigned(FAttributes));
  FAttributes.Remove(Key);
end;

function TMeiNode.ChildTree: TMeiNode;
begin
  result := FChild;
end;

function TMeiNode.NextSibling: TMeiNode;
begin
  result := FSibling;
end;

function TMeiNode.LastChild: TMeiNode;
begin
  if not Assigned(FChild) then
    result := Self
  else
    result := FChild.LastChild;
end;

function TMeiNode.LastSibling: TMeiNode;
begin
  if not Assigned(FSibling) then
    result := Self
  else
    result := FSibling.LastSibling;
end;

function TMeiNode.AddFirstChild(Child: TMeiNode): TMeiNode;
begin
  if Assigned(FChild) then
  begin
    FChild.Destroy;
  end;
  FChild := Child;
  result := Self;
end;

function TMeiNode.AddFirstSibling(Sibling: TMeiNode): TMeiNode;
begin
  if Assigned(FSibling) then
  begin
    FSibling.Destroy;
  end;
  FSibling := Sibling;
  result := Self;
end;

function TMeiNode.AppendChild(Child: TMeiNode): TMeiNode;
begin
  result := Self.LastChild.AddFirstChild(Child);
end;

function TMeiNode.AppendSibling(Sibling: TMeiNode): TMeiNode;
begin
  result := Self.LastSibling.AddFirstSibling(Sibling);
end;

procedure TMeiNode.Assign(SourceNode: TMeiNode);
begin
  FName := SourceNode.FName;

  FAttributes.Assign(SourceNode.FAttributes);

  if Assigned(SourceNode.FChild) then
  begin
    FChild := TMeiNode.Create();
    FChild.Assign(SourceNode.FChild);
  end;

  if Assigned(SourceNode.FSibling) then
  begin
    FSibling := TMeiNode.Create();
    FSibling.Assign(SourceNode.FSibling);
  end;
end;

{ Return the first node that matches the name and attribute pair.
  Does not copy the tree, just returns the pointer to its root. }
function TMeiNode.FindElementByAttribute(Name, Key, Value: String): TMeiNode;
var
  ValueTest: String;
  FoundNode: TMeiNode = nil;
begin
  if (FName = Name) 
    and FAttributes.TryGetValue(Key, ValueTest)
    and (ValueTest = Value) then
  begin
    FoundNode := Self;
  end;

  if (not Assigned(FoundNode)) and Assigned(FChild) then
  begin
    FoundNode := FChild.FindElementByAttribute(Name, Key, Value);
  end;
  
  if (not Assigned(FoundNode)) and Assigned(FSibling) then
  begin
    FoundNode := FSibling.FindElementByAttribute(Name, Key, Value);
  end;

  result := FoundNode;
end;

procedure WriteMEIDocument(Root: TMeiNode);
var
  OutputStr: String;
begin
  OutputStr := _XMLDeclaration + LineEnding;
  OutputStr := OutputStr + Root.XMLString;
  WriteLn(OutputStr);
end;

end.
