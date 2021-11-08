{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} {$OPTIMIZATION tailrec}

{ @abstract(Build a tree of code objects defined by Lilypond @code(\new)
  commands within a @code(\score) expression.)
  @author(Andrew Cashner)

  This unit is used to build a tree of Lilypond code objects that were
  created with @code(\new) commands within a @code(\score) expression. 
  The tree is a left-child/right-sibling tree of @link(TLyObject) instances.
  
  In the command @code(\new Voice = "Soprano" < \MusicSoprano >), this class
  would store @code("Voice") as the @code(FType), @code("Soprano") as the
  @code(FID), and the rest of the expression, up to the conclusion of a
  curly-brace delimited group, as the @code(FContents).
}
unit ScoreTree;

interface

uses SysUtils, StrUtils, Classes, StringTools, Outline;

type
  { One node in a tree of Lilypond code objects that were
  created with @code(\new) commands. }
  TLyObject = class
  public
    var
      { Type of object, e.g., Voice or Lyrics }
      FType: String;
      { ID string, as in @code(\new Voice = "Soprano"); blank if omitted in
        Lilypond source }
      FID: String;
      { Contents: If the command is followed by section delimited by double
      angle brackets, then the contents will be blank; otherwise, everything
      up to the end of a curly-brace-delimited argument }
      FContents: String;
      { Left child node (tree) }
      FChild: TLyObject;
      { Right sibling node (tree) }
      FSibling: TLyObject;
    constructor Create();
    constructor Create(TypeStr, IDStr, ContentsStr: String);
    constructor Create(TypeStr, IDStr, ContentsStr: String; 
      Child, Sibling: TLyObject);

    { Destroy the whole tree. }
    destructor Destroy; override;

    { Follow the left children all the way to the end; return the last one. }
    function LastChild: TLyObject;

    { Follow the right siblings all the way to the end; return the last one. }
    function LastSibling: TLyObject;

    { Return a string with a DIY XMl representation of the object tree, for
      testing/debugging. }
    function ToString: String; override;
  end;

{ Build an LCRS tree of Lilypond @code(\new) objects.
  
  We look for two kinds of objects:
    @orderedlist(
      @item(@code(\new) expressions followed by an expression enclosed in double angle brackets (@code(<<...>>)))
      @item(Expressions that conclude with an expression in matched curly braces)
    )
  For the first kind, we store the value and ID, if there is one; then we continue to look recursively for child elements within the angle-bracket expression.
  For the second kind, we store value, ID, and contents (everything from the ID up to the end of the matched-brace expression) as a sibling of the previous item.
}
function FindLyNewTree(Source: String; Tree: TLyObject): TLyObject;

implementation

constructor TLyObject.Create();
begin
  inherited Create;
end;

constructor TLyObject.Create(TypeStr, IDStr, ContentsStr: String);
begin
  inherited Create;
  FType     := TypeStr;
  FID       := IDStr;
  FContents := ContentsStr;
  FChild    := nil;
  FSibling  := nil;

end;

constructor TLyObject.Create(TypeStr, IDStr, ContentsStr: String;
  Child, Sibling: TLyObject);
begin
  inherited Create;
  FType     := TypeStr;
  FID       := IDStr;
  FContents := ContentsStr;
  FChild    := Child;
  FSibling  := Sibling;
end;

destructor TLyObject.Destroy;
begin
  if FChild <> nil then 
    FChild.Destroy;
  if FSibling <> nil then 
    FSibling.Destroy;
  inherited Destroy;
end;

function TLyObject.LastChild: TLyObject;
begin
  if FChild = nil then
    result := Self
  else
    result := FChild.LastChild;
end;

function TLyObject.LastSibling: TLyObject;
begin
  if FSibling = nil then
    result := Self
  else
    result := FSibling.LastSibling;
end;

function TLyObject.ToString: String; 
  function TreeToString(Parent: TLyObject; Generation: Integer): String;
  var
    Indent: String;
    ParentStr, ChildStr, SibStr, IDStr: String;
  begin
    if Parent <> nil then
    begin
      Indent := IndentStr(Generation);

      if Parent.FID <> '' then 
        IDStr := '" id="' + Parent.FID;
      
      ParentStr := '<lyobject type="' + Parent.FType + IDStr + '">' + Parent.FContents;

      if Parent.FChild <> nil then
        ChildStr := LineEnding + TreeToString(Parent.FChild, Generation + 1) + Indent;

      SibStr := LineEnding;
      if Parent.FSibling <> nil then 
        SibStr := LineEnding + TreeToString(Parent.FSibling, Generation);

      result := Indent + ParentStr + ChildStr + '</lyobject>' + SibStr;
    end;
  end;
begin
  if FType = '' then
    result := ''
  else 
    result := TreeToString(Self, 0);
end;

function FindLyNewTree(Source: String; Tree: TLyObject): TLyObject;
var
  SearchIndex: Integer;
  SearchStr, ThisType, ThisID, ThisContents: String;
  Outline: TIndexPair;
begin
  Outline := TIndexPair.Create;
  SearchStr := Source;
  try
    SearchIndex := 0;
    if (Length(Source) = 0) then
    begin
      result := Tree;
      exit;
    end;
   
    SearchIndex := SearchStr.IndexOf('\new ');
    if SearchIndex = -1 then 
    begin
      result := Tree;
      exit;
    end;
     
    { Find Type }
    SearchStr := SearchStr.Substring(SearchIndex);
    ThisType := ExtractWord(2, SearchStr, StdWordDelims);
    SearchStr := StringDropBefore(SearchStr, ThisType);
 
    { Find ID }
    if SearchStr.StartsWith(' = "') then
    begin 
      SearchStr := StringDropBefore(SearchStr, ' = "');
      ThisID := SearchStr.Substring(0, SearchStr.IndexOf('"'));
      SearchStr := StringDropBefore(SearchStr, ThisID + '"');
    end
    else
      ThisID := '';
 
    { Find Contents: Either an expression within double angle brackets or
    one within curly braces. If angle brackets, recursively look for nested
    @code(\new) expressions. }
    if SearchStr.TrimLeft.StartsWith('<<') then
    begin
      { Search within group for nested @code(\new) expressions and save them
      as children; then move on after this group. Omit content string. }
      Outline := BalancedDelimiterSubstring(SearchStr, '<', '>', Outline);
      ThisContents := CopyStringRange(SearchStr, Outline, rkInclusive);
      if Tree = nil then
        Tree := TLyObject.Create(ThisType, ThisID, '')
      else
        Tree.LastChild.FChild := TLyObject.Create(ThisType, ThisID, '');
    
      Tree.LastChild.FChild := FindLyNewTree(ThisContents, nil);
      Source := StringDropBefore(Source.Substring(SearchIndex), ThisContents);
    end
    else
    begin
      { Add this expression as a sibling, then move on to next }
      ThisContents := SearchStr.Substring(0, SearchStr.IndexOf('{'));
      ThisContents := ThisContents + CopyBraceExpr(SearchStr);
      ThisContents := ThisContents.Trim;
      if Tree = nil then
        Tree := TLyObject.Create(ThisType, ThisID, ThisContents)
      else
        Tree.LastSibling.FSibling := TLyObject.Create(ThisType, ThisID, ThisContents);
      
      Source := Source.Substring(SearchIndex + 1); 
    end;
  
    { Look for the next sibling where you left off from the last search }
    Tree.LastSibling.FSibling := FindLyNewTree(Source, nil);
  finally
    FreeAndNil(Outline);
    result := Tree;
  end;
end;

end.
