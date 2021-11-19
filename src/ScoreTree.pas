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
  TMusicTreeElement = (ekAnonymous, ekStaffGrp, ekStaff, ekLayer, ekMeasure);

  { One node in a tree of Lilypond code objects that were
  created with @code(\new) commands. }
  TLyObject = class
  public
    var
      { Label for object type, equivalent between Lilypond and MEI }
      FType: TMusicTreeElement;
      { Type of object, e.g., Voice or Lyrics }
      FName: String;
      { ID string, as in @code(\new Voice = "Soprano"); blank if omitted in
        Lilypond source }
      FID: String;
      { In-order position of elements of the same type: Staff elements are
      numbered consecutively regardless of whether they are enclosed in a
      StaffGroup. }
      FNum: Integer;
      { Contents: If the command is followed by section delimited by double
      angle brackets, then the contents will be blank; otherwise, everything
      up to the end of a curly-brace-delimited argument }
      FContents: String;
      { Left child node (tree) }
      FChild: TLyObject;
      { Right sibling node (tree) }
      FSibling: TLyObject;
    constructor Create();
    constructor Create(Name, ID: String; ContentsStr: String = ''; Num:
      Integer = 1; Child: TLyObject = nil; Sibling: TLyObject = nil); 

    { Destroy the whole tree. }
    destructor Destroy; override;

    { Follow the left children all the way to the end; return the last one. }
    function LastChild: TLyObject;

    { Follow the right siblings all the way to the end; return the last one. }
    function LastSibling: TLyObject;

    { Number the objects with a given FType label consecutively in an in-order
    traversal, regardless of relation to other elements of the tree hierarchy }
    procedure NumberElementsInOrder(ElementType: TMusicTreeElement);

    { Number ChoirStaff, StaffGroup, Staff, and Voice elements in order }
    procedure SetNumbers;

    { Return a string with a DIY XMl representation of the object tree, for
      testing/debugging. }
    function ToString: String; override;

    { Create a new stringlist containing an MEI scoreDef element, drawing the
    information from this tree. }
    function ToNewMEIScoreDef: TStringListAAC;
   
//    { Convert a Lilypond music expression to MEI in a stringlist }
//    function ToMusic(MEILines: TStringListAAC): TStringListAAC;
  end;

{ Build an LCRS tree of Lilypond @code(\new) objects.
  
  We look for two kinds of objects:
    @orderedlist(
      @item(@code(\new) expressions followed by an expression enclosed in
        double angle brackets (@code(<<...>>))) 
      @item(Expressions that conclude with an expression in matched curly
        braces)
    )
  For the first kind, we store the value and ID, if there is one; then we
  continue to look recursively for child elements within the angle-bracket
  expression. For the second kind, we store value, ID, and contents
  (everything from the ID up to the end of the matched-brace expression) as a
  sibling of the previous item.
}

function FindLyNewTree(Source: String; Tree: TLyObject): TLyObject;


implementation

constructor TLyObject.Create();
begin
  inherited Create;
end;

constructor TLyObject.Create(Name, ID: String; ContentsStr: String = ''; 
  Num: Integer = 1; Child: TLyObject = nil; Sibling: TLyObject = nil); 
function NameToType(Name: String): TMusicTreeElement;
var
  Element: TMusicTreeElement;
begin
  case Name of
    'StaffGroup', 'ChoirStaff': Element := ekStaffGrp;
    'Staff': Element := ekStaff;
    'Voice': Element := ekLayer;
  else Element := ekAnonymous;
  end;
  result := Element;
end;
begin
  inherited Create;
  FType     := NameToType(Name);
  FName     := Name;
  FID       := ID;
  FNum      := Num;
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
      IDStr := ' ' + XMLAttribute('id', Parent.FID);
    
    ParentStr := '<lyobject ' + XMLAttribute('type', Parent.FName) + IDStr 
                  + ' ' + XMLAttribute('n', IntToStr(Parent.FNum)) + '">' 
                  + Parent.FContents;

    if Parent.FChild <> nil then
      ChildStr := LineEnding + TreeToString(Parent.FChild, Generation + 1) + Indent;

    SibStr := LineEnding;
    if Parent.FSibling <> nil then 
      SibStr := LineEnding + TreeToString(Parent.FSibling, Generation);

    result := Indent + ParentStr + ChildStr + '</lyobject>' + SibStr;
  end;
end;
begin
  result := TreeToString(Self, 0);
end;

function FindLyNewTree(Source: String; Tree: TLyObject): TLyObject;
var
  SearchIndex: Integer;
  SearchStr, ThisType, ThisID, ThisContents: String;
  Outline: TIndexPair;
begin
  SearchStr := Source;
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
    Outline := BalancedDelimiterSubstring(SearchStr, '<', '>');
    ThisContents := CopyStringRange(SearchStr, Outline, rkInclusive);
    if Tree = nil then
      Tree := TLyObject.Create(ThisType, ThisID)
    else
      Tree.LastChild.FChild := TLyObject.Create(ThisType, ThisID);
  
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
  result := Tree;
end;

procedure TLyObject.NumberElementsInOrder(ElementType: TMusicTreeElement);
var
  N: Integer = 0;
function InnerNums(Node: TLyObject): TLyObject;
begin
  if Node <> nil then
  begin
    if Node.FType = ElementType then
    begin
      Inc(N);
      Node.FNum := N;
    end;
    if Node.FChild <> nil then
      Node.FChild := InnerNums(Node.FChild);
    if Node.FSibling <> nil then
      Node.FSibling := InnerNums(Node.FSibling);
  end;
  result := Node;
end;
begin
  InnerNums(Self);
end;

procedure TLyObject.SetNumbers;
begin
  { TODO treating ChoirStaff and StaffGroup as part of same series }
  NumberElementsInOrder(ekStaffGrp); 
  NumberElementsInOrder(ekStaff);
  NumberElementsInOrder(ekLayer);
end;

type
  ClefKind = (ckTreble, ckBass);
  KeyKind = (kkDurus, kkMollis);
  MeterKind = (mkDuple, mkTriple);

const StaffGrpAttributes = ' bar.thru="false" symbol="bracket"';

function StaffDefAttributes(Clef: ClefKind; Key: KeyKind; Meter: MeterKind): String;
var
  ClefStr, KeyStr, MeterStr, OutputStr: String;
begin
  OutputStr := ' lines="5" ';
  case Clef of
    ckTreble: ClefStr := 'clef.line="2" clef.shape="G" ';
    ckBass  : ClefStr := 'clef.line="4" clef.shape="F" ';
  end;
  case Key of 
    kkDurus : KeyStr := 'key.sig="0" ';
    kkMollis: KeyStr := 'key.sig="1f" ';
  end;
  case Meter of
    mkDuple : MeterStr := 'mensur.sign="C" mensur.tempus="2"';
    mkTriple: MeterStr := 'mensur.sign="C" mensur.tempus="2" proport.num="3"';
  end;
  result := OutputStr + ClefStr + KeyStr + MeterStr;
end;

function ElementNumID(Node: TLyObject; N: Integer): String;
begin
  result := XMLAttribute('n', IntToStr(N)) + XMLAttribute(' xml:id', Node.FID);
end;

function StaffNumID(Node: TLyObject): String;
begin
  result := XMLAttribute('n', IntToStr(Node.FNum)) 
            + XMLAttribute(' def', '#' + Node.FID);
end;


function TLyObject.ToNewMEIScoreDef: TStringListAAC;
function InnerScoreDef(Node: TLyObject; InnerLines: TStringListAAC): TStringListAAC;
var 
  ThisTag, SearchStr, Attributes: String;
  TempLines: TStringListAAC;
  Clef: ClefKind;
  Key: KeyKind;
  Meter: MeterKind;
begin
  assert(InnerLines <> nil);
  TempLines := TStringListAAC.Create;
  ThisTag := '';
  try
    case Node.FType of
      ekStaffGrp:
      begin
        ThisTag := 'staffGrp';
        Attributes := XMLAttribute('xml:id', Node.FID) + StaffGrpAttributes;
      end;

      ekStaff:
      begin
        ThisTag := 'staffDef';
        Attributes := XMLAttribute('n', IntToStr(Node.FNum)) 
                      + XMLAttribute(' xml:id', Node.FID);
        
        { Extract staffDef info from the first music expression in the first
        child Voice }
        if (Node.FChild <> nil) and (Node.FChild.FType = ekLayer) then
        begin
          { Search c. first 10 lines }
          SearchStr := Node.FChild.FContents.Substring(0, 800); 
          Clef := ckTreble;
          if SearchStr.Contains('\clef "bass"') then 
            Clef := ckBass;

          Key := kkDurus;
          if SearchStr.Contains('\CantusMollis') then
            Key := kkMollis;

          Meter := mkDuple;
          if SearchStr.Contains('\MeterTriple') then
            Meter := mkTriple;
        end;
        Attributes := Attributes + StaffDefAttributes(Clef, Key, Meter);
      end;
    end;

    { Create this element and its children }
    if (ThisTag <> '') and (Node.FChild <> nil) then
    begin
      TempLines.AddStrings(InnerScoreDef(Node.FChild, InnerLines));
      TempLines.EncloseInXML(ThisTag, Attributes);
    end;

    { Create its siblings }
    if Node.FSibling <> nil then
      TempLines.AddStrings(InnerScoreDef(Node.FSibling, InnerLines));
    InnerLines.Assign(TempLines);
  finally
    FreeAndNil(TempLines);
    result := InnerLines;
  end;
end;
var
  MEI: TStringListAAC;
begin
  MEI := TStringListAAC.Create;
  MEI := InnerScoreDef(Self, MEI);
  MEI.EncloseInXML('staffGrp');
  MEI.EncloseInXML('scoreDef');
  result := MEI;
end;

end.
