{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(Create an internal representation of an MEI-XML tree and write it
  to XML.)
  @author(Andrew Cashner)

  We represent an XML tree as a left-child/right-sibling tree of
  @link(TMeiNode) objects. The unit defines a basic library of functions for
  creating, accessing, and manipulating the tree. All these functions are
  recursive and most return the root of the tree given as input. 
}
unit MEI;

interface

uses SysUtils, Classes, Generics.Collections;

type
  { @abstract(A list of string key-value pairs representing the attributes of
  an XML element.) }
  TMeiAttributeList = class(specialize TDictionary<String, String>)
  public
    { Render the list to an XML string (@code(key1="value1" key2="value2")) }
    function XMLString: String; 
    procedure Assign(SourceList: TMeiAttributeList);
  end;

  { A single key-value pair in the attribute list. }
  TMeiAttributeKeyValue = TMeiAttributeList.TDictionaryPair;

  { @abstract(Internal representation of an MEI-XML element.)
  Includes its name, list of attributes, text node, and (pointers to) child
  and sibling trees. }
  TMeiNode = class
  private
    var
      FName: String;                  {< Element/tag name }
      FAttributes: TMeiAttributeList; {< List of key-value attribute pairs }
      FText: String;                  {< Text node, if any }
      FChild: TMeiNode;               {< Child tree }
      FSibling: TMeiNode;             {< Sibling tree }
  public
    constructor Create(Name: String = 'xml'; TextContents: String = '');

    { Create the MEI root element with namespace and version attributes }
    constructor CreateMeiRoot();
    
    destructor Destroy(); override;

    { Render the entire tree (recursively) to formatted XML starting at this
    node }
    function XMLString(IndentLevel: Integer = 0): String;

    { Add a single attribute key-value pair to an element, or update an
      existing attribute if the key is already in the list. }
    procedure AddAttribute(Key, Value: String);

    { Delete a single attribute with the given key from an element's list. }
    procedure RemoveAttribute(Key: String);

    { Is the text node not empty? }
    function IsTextSet: Boolean;

    { Return the last child, following every child node to the end of the
    tree. }
    function LastChild: TMeiNode;

    { Return the last sibling, following every sibling node to the end of the
    tree. }
    function LastSibling: TMeiNode;

    { Set the child of this node }
    function AddFirstChild(Child: TMeiNode): TMeiNode;

    { Set the sibling of this node }
    function AddFirstSibling(Sibling: TMeiNode): TMeiNode;

    { Add another sibling at the end of this node's siblings. That is, add
    another node at the current level of the tree hierarchy. }
    function AppendSibling(Sibling: TMeiNode): TMeiNode;

    { Add another element as the last sibling to this node's child. That is,
    add another node at one level down on the tree hierarchy relative to the
    parent. } 
    function AppendChild(Child: TMeiNode): TMeiNode;

    { Add a child after the last child in the whole chain of children starting
    from this node. That is, add another node at the lowest level of the tree accessible through this parent's children. }
    function AppendLastChild(Child: TMeiNode): TMeiNode;

    { Copy the information for a single node without its children or sibling
    trees. }
    procedure AssignNodeOnly(SourceNode: TMeiNode);

    { Copy the information for a single node including its children but not
    its siblings. }
    procedure AssignWithoutSiblings(SourceNode: TMeiNode);

    { Copy the whole tree starting from the given node to this tree. }
    procedure Assign(SourceNode: TMeiNode);

    { TODO are this and the following function needed? }
    { Return the first node that matches the name.
      Does not copy the tree, just returns the pointer to its root. }
    function FindElementByName(Name: String): TMeiNode;

    { Return the first node that matches the name and attribute pair.
      Does not copy the tree, just returns the pointer to its root. }
    function FindElementByAttribute(Name, Key, Value: String): TMeiNode;
  
    { Look up the value for a given key in the attribute list. }
    function GetAttributeValue(Key: String): String;

    property Name:        String   read FName write FName; {< Element/tag name }
    property TextNode:    String   read FText write FText; {< Text node }
    property ChildTree:   TMeiNode read FChild;            {< Child node/tree }
    property NextSibling: TMeiNode read FSibling;          {< Sibling node/tree }
  end;

{ Create a randomly-generated GUID for each XML element. Each is prefaced with
  @code(mei-) so that it will be a valid @code(xml:id) value. }
function GenerateXmlID: String;

{ Given the root of an MEI tree, return the whole MEI document as a string. }
function MeiDocString(Root: TMeiNode): String;


implementation

const
  _XMLDeclaration = '<?xml version="1.0" encoding="utf-8"?>' + LineEnding
    + '<?xml-model href="https://music-encoding.org/schema/dev/mei-all.rng" ' 
    + 'type="application/xml" '
    + 'schematypens="http://relaxng.org/ns/structure/1.0"?>' + LineEnding 
    + '<?xml-model href="https://music-encoding.org/schema/dev/mei-all.rng" '  
    + 'type="application/xml" '
    + 'schematypens="http://purl.oclc.org/dsdl/schematron"?>';

  _MeiNamespace = 'https://music-encoding.org/ns/mei';
  _MeiVersion = '3.0.0';

function TMeiAttributeList.XMLString: String;
var 
  ThisPair: TMeiAttributeKeyValue;
  KeyCount: Integer;
  OutputStr: String = '';
begin
  KeyCount := Self.Count;
  for ThisPair in Self do
  begin
    OutputStr := OutputStr + Format('%s="%s"', [ThisPair.Key, ThisPair.Value]);
    { Only add space between items }
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

constructor TMeiNode.Create(Name: String = 'xml'; TextContents: String = '');
begin
  inherited Create();
  FName := Name;
  
  FAttributes := TMeiAttributeList.Create();
  FAttributes.AddOrSetValue('xml:id', GenerateXmlID());

  FText    := TextContents;
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

function GenerateXmlID: String;
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

  if not Assigned(FChild) and not IsTextSet then
    OutputStr := OutputStr + '/>'
  else 
  begin
    OutputStr := OutputStr + '>';
    if IsTextSet then
    begin
      OutputStr := OutputStr + FText;
    end;

    if not Assigned(FChild) then
    begin
      OutputStr := OutputStr + Format('</%s>', [FName]);
    end
    else
    begin
      OutputStr := OutputStr  + LineEnding 
        + FChild.XMLString(IndentLevel + 1) + LineEnding
        + BasicIndent + Format('</%s>', [FName]);
    end;
  end;

  if Assigned(FSibling) then
  begin
    OutputStr := OutputStr + LineEnding + FSibling.XMLString(IndentLevel);
  end;
  
  result := OutputStr;
end;

procedure TMeiNode.AddAttribute(Key, Value: String);
begin
  Assert(Assigned(FAttributes));
  FAttributes.AddOrSetValue(Key, Value);
end;

function TMEINode.IsTextSet: Boolean;
begin
  result := not FText.IsEmpty;
end;

procedure TMeiNode.RemoveAttribute(Key: String);
begin
  Assert(Assigned(FAttributes));
  FAttributes.Remove(Key);
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

function TMeiNode.AppendSibling(Sibling: TMeiNode): TMeiNode;
begin
  result := Self.LastSibling.AddFirstSibling(Sibling);
end;

function TMeiNode.AppendChild(Child: TMeiNode): TMeiNode;
begin
  if not Assigned(FChild) then
    result := Self.AddFirstChild(Child)
  else 
    result := Self.FChild.AppendSibling(Child);
end;

function TMeiNode.AppendLastChild(Child: TMeiNode): TMeiNode;
begin
  result := Self.LastChild.AddFirstChild(Child);
end;

procedure TMeiNode.AssignNodeOnly(SourceNode: TMeiNode);
begin
  FName := SourceNode.FName;
  FAttributes.Assign(SourceNode.FAttributes);
end;

procedure TMeiNode.AssignWithoutSiblings(SourceNode: TMeiNode);
begin
  AssignNodeOnly(SourceNode);

  if Assigned(SourceNode.FChild) then
  begin
    FChild := TMeiNode.Create();
    FChild.Assign(SourceNode.FChild);
  end;
end;

procedure TMeiNode.Assign(SourceNode: TMeiNode);
begin
  AssignNodeOnly(SourceNode);

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

function TMeiNode.FindElementByName(Name: String): TMeiNode;
var
  FoundNode: TMeiNode = nil;
begin
  if (FName = Name) then
  begin
    FoundNode := Self;
  end;

  if (not Assigned(FoundNode)) and Assigned(FChild) then
  begin
    FoundNode := FChild.FindElementByName(Name);
  end;
  
  if (not Assigned(FoundNode)) and Assigned(FSibling) then
  begin
    FoundNode := FSibling.FindElementByName(Name);
  end;

  result := FoundNode;
end;

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

function TMeiNode.GetAttributeValue(Key: String): String;
var 
  ValueTest: String = '';
begin
  FAttributes.TryGetValue(Key, ValueTest);
  result := ValueTest;
end;

function MeiDocString(Root: TMeiNode): String;
var
  OutputStr: String;
begin
  OutputStr := _XMLDeclaration + LineEnding;
  OutputStr := OutputStr + Root.XMLString;
  result := OutputStr;
end;

end.
