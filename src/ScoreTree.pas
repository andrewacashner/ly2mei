{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

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

uses SysUtils, StrUtils, Classes, StringTools, Outline, MEI;

{ Labels for keys or key signatures. }
type
  TKeyKind = (kkNone,
    kkCMaj,  kkAMin,  kkCantusDurus, 
    kkGMaj,  kkEMin,
    kkDMaj,  kkBMin,
    kkAMaj,  kkFsMin,
    kkEMaj,  kkCsMin,
    kkBMaj,  kkGsMin,
    kkFsMaj, kkDsMin,
    kkCsMaj, kkAsMin,
    kkFMaj,  kkDMin,   kkCantusMollis,
    kkBbMaj, kkGMin,
    kkEbMaj, kkCMin,
    kkAbMaj, kkFMin,
    kkDbMaj, kkBbMin,
    kkGbMaj, kkEbMin,
    kkCbMaj, kkAbMin);

{ In Lilypond input, find a @code(\key) declaration, or in Lirio format,
  @code(\CantusDurus) (no accidentals) or @code(\CantusMollis) (one flat). }
function FindLyKey(KeyStr: String): TKeyKind;

type 
  { Types of elements in the internal tree of @code(TLyObject) or
    @code(TMEIElement) objects }
    { TODO get rid of deprecated ekXML }
  TMusicTreeElement = (ekAnonymous, ekStaffGrp, ekStaff, ekLayer, ekMeasure, ekXML);

  { @abstract(One node in a tree of Lilypond code objects.)
    These objects were originally created with @code(\new) commands in the
    @code(\score) expression. }
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
      up to the end of a curly-brace-delimited argument. }
      FContents: String;
      { Left child node (tree) }
      FChild: TLyObject;
      { Right sibling node (tree) }
      FSibling: TLyObject;

    constructor Create();

    { The @code(FType) field will be created automatically based on the
    @code(Name) argument. }
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

    { Number ChoirStaff, StaffGroup, Staff, and Voice elements in order.
      We are treating ChoirStaff and StaffGroup as part of the same series.
      (Should we?) }
    procedure SetNumbers;

    { Return a string with a DIY XMl representation of the object tree, for
      testing/debugging. }
    function ToString: String; override;

    function ToMeiScoreDef: TMeiNode;
    
    function ToXMLAsIs(XmlNode: TMeiNode = nil): TMeiNode;
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
  (everything from the ID up to the end of the matched-brace expression)
  as a sibling of the previous item.
}
function BuildLyObjectTree(Source: String; Tree: TLyObject): TLyObject;


function CreateLyObjectTreeFromLy(LyInput: TStringListAAC): TLyObject;

function CreateMeiScoreDefFromLy(LyInput: TStringListAAC): TMeiNode;

function AddMeiScoreDef(Root: TMeiNode; LyInput: TStringListAAC): TMeiNode;


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
  if Assigned(FChild) then 
    FChild.Destroy;
  if Assigned(FSibling) then 
    FSibling.Destroy;
  inherited Destroy;
end;

function TLyObject.LastChild: TLyObject;
begin
  if not Assigned(FChild) then
    result := Self
  else
    result := FChild.LastChild;
end;

function TLyObject.LastSibling: TLyObject;
begin
  if not Assigned(FSibling) then
    result := Self
  else
    result := FSibling.LastSibling;
end;

function TLyObject.ToString: String; 
function TreeToString(Parent: TLyObject; Generation: Integer): String;
var
  Indent: String;
  ParentStr, ChildStr, SibStr: String;
begin
  if Assigned(Parent) then
  begin
    Indent := IndentStr(Generation);
    
    ParentStr := Format('<lyobject %s %s %s>%s', 
        [XMLAttribute('type', Parent.FName),
         XMLAttribute('id', Parent.FID),
         XMLAttribute('n', IntToStr(Parent.FNum)), 
         Parent.FContents]);

    if Assigned(Parent.FChild) then
      ChildStr := LineEnding + TreeToString(Parent.FChild, Generation + 1) + Indent;

    SibStr := LineEnding;
    if Assigned(Parent.FSibling) then 
      SibStr := LineEnding + TreeToString(Parent.FSibling, Generation);

    result := Indent + ParentStr + ChildStr + '</lyobject>' + SibStr;
  end;
end;
begin
  result := TreeToString(Self, 0);
end;

function BuildLyObjectTree(Source: String; Tree: TLyObject): TLyObject;
var
  SearchIndex: Integer = 0;
  SearchStr, ThisType, ThisID, ThisContents: String;
  Outline: TIndexPair;
begin
  SearchStr := Source;
  SearchIndex := SearchStr.IndexOf('\new ');

  if (not SearchStr.IsEmpty) and (SearchIndex <> -1) then
  begin
    { Find Type }
    SearchStr := SearchStr.Substring(SearchIndex);
    ThisType := ExtractWord(2, SearchStr, StdWordDelims);
    SearchStr := StringDropBefore(SearchStr, ThisType);

    { Find ID }
    ThisID := '';
    if SearchStr.StartsWith(' = "') then
    begin 
      SearchStr := StringDropBefore(SearchStr, ' = "');
      ThisID := StringDropAfter(SearchStr, '"');
      SearchStr := StringDropBefore(SearchStr, ThisID + '"');
    end;

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

      Tree.LastChild.FChild := BuildLyObjectTree(ThisContents, nil);
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
    Tree.LastSibling.FSibling := BuildLyObjectTree(Source, nil);
  end;
  result := Tree;
end;

procedure TLyObject.NumberElementsInOrder(ElementType: TMusicTreeElement);
var
  N: Integer = 0;
function InnerNums(Node: TLyObject): TLyObject;
begin
  if Assigned(Node) then
  begin
    if Node.FType = ElementType then
    begin
      Inc(N);
      Node.FNum := N;
    end;
    if Assigned(Node.FChild) then
      Node.FChild := InnerNums(Node.FChild);
    if Assigned(Node.FSibling) then
      Node.FSibling := InnerNums(Node.FSibling);
  end;
  result := Node;
end;
begin
  InnerNums(Self);
end;

procedure TLyObject.SetNumbers;
begin
  NumberElementsInOrder(ekStaffGrp); 
  NumberElementsInOrder(ekStaff);
  NumberElementsInOrder(ekLayer);
end;

function CreateLyObjectTreeFromLy(LyInput: TStringListAAC): TLyObject;
var 
  LyTree: TLyObject = nil;
begin
  LyTree := BuildLyObjectTree(LyInput.Text, LyTree); 
  LyTree.SetNumbers;
  result := LyTree;
end;

type
  TClefKind = (ckNone, ckTreble, ckSoprano, ckMezzoSoprano, ckAlto, ckTenor,
    ckBaritone, ckBass, ckTreble8va); 

function FindLyClef(Source: String): TClefKind;
var
  TestStr, ClefStr: String;
  Clef: TClefKind = ckNone;
begin
  if Source.Contains('\clef ') then
  begin
    TestStr := StringDropBefore(Source, '\clef ');
    ClefStr := CopyFirstQuotedString(TestStr);
    case ClefStr of
      'treble'       : Clef := ckTreble;
      'soprano'      : Clef := ckSoprano; 
      'mezzosoprano' : Clef := ckMezzoSoprano;
      'alto'         : Clef := ckAlto;
      'tenor'        : Clef := ckTenor;
      'baritone'     : Clef := ckBaritone;
      'bass'         : Clef := ckBass;
      'treble_8'     : Clef := ckTreble8va;
      else
        Clef := ckNone;
    end;
  end;
  
  DebugLn('CLEF test string: ''' + ClefStr + '''');
  DebugLn('CLEF kind: '); {$ifdef DEBUG}WriteLn(Clef);{$endif}

  result := Clef;
end;

function AddMEIClefAttribute(StaffDefNode: TMeiNode; Clef: TClefKind): TMeiNode;
var
  ClefLine: Integer;
  ClefLetter: String;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.GetName = 'staffDef');

  case Clef of
    ckNone, ckSoprano           : ClefLine := 1;
    ckTreble, ckMezzoSoprano,   
      ckTreble8va               : ClefLine := 2;
    ckAlto, ckBaritone          : ClefLine := 3;
    ckTenor, ckBass             : ClefLine := 4;
  end;
  
  case Clef of
    ckNone, ckTreble, 
      ckTreble8va               : ClefLetter := 'G';
    ckSoprano, ckMezzoSoprano,   
      ckAlto, ckTenor           : ClefLetter := 'C';
    ckBaritone, ckBass          : ClefLetter := 'F';
  end;

  StaffDefNode.AddAttribute('clef.line', IntToStr(ClefLine));
  StaffDefNode.AddAttribute('clef.shape', ClefLetter);

  if Clef = ckTreble8va then
  begin
    StaffDefNode.AddAttribute('clef.dis', '8');
    StaffDefNode.AddAttribute('clef.dis.place', 'below');
  end;
  result := StaffDefNode;
end;

function FindLyKey(KeyStr: String): TKeyKind;
type
  TKeyMode = (kMajor, kMinor);
var
  Key: TKeyKind = kkNone;
  KeyName: String;
  Mode: TKeyMode;
begin
  if KeyStr.Contains('\CantusDurus') then
    Key := kkCantusDurus
  else if KeyStr.Contains('\CantusMollis') then
    Key := kkCantusMollis
  else if KeyStr.Contains('\key ') then
  begin
    KeyStr := StringDropBefore(KeyStr, '\key ');
    DebugLn('Searching for Key in string: ''' + KeyStr + '''');

    if KeyStr.Contains('\major') then
    begin
      KeyStr := StringDropAfter(KeyStr, '\major'); 
      Mode := kMajor;
    end
    else if KeyStr.Contains('\minor') then
    begin
      KeyStr := StringDropAfter(KeyStr, '\minor'); 
      Mode := kMinor;
    end
    else 
    begin
      result := kkNone;
      exit;
    end;

    KeyName := StringDropAfter(KeyStr, '\m').Trim;
    case Mode of
      kMajor :
        case KeyName of
          'c'   : Key := kkCMaj;
          'ces' : Key := kkCbMaj;
          'cis' : Key := kkCsMaj;
          'd'   : Key := kkDMaj;
          'des' : Key := kkDbMaj;
          'e'   : Key := kkEMaj;
          'es', 'ees' : Key := kkEbMaj;
          'f'   : Key := kkFMaj;
          'fis' : Key := kkFsMaj;
          'g'   : Key := kkGMaj;
          'ges' : Key := kkGbMaj;
          'a'   : Key := kkAMaj;
          'as', 'aes' : Key := kkAbMaj;
          'b'   : Key := kkBMaj;
          'bes' : Key := kkBbMaj;
        end;

      kMinor :
        case KeyName of
          'c'   : Key := kkCMin;
          'cis' : Key := kkCsMin;
          'd'   : Key := kkDMin;
          'dis' : Key := kkDsMin;
          'e'   : Key := kkEMin;
          'es', 'ees' : Key := kkEbMin;
          'f'   : Key := kkFMin;
          'fis' : Key := kkFsMin;
          'g'   : Key := kkGMin;
          'gis' : Key := kkGsMin;
          'a'   : Key := kkAMin;
          'as', 'aes' : Key := kkAbMin;
          'ais' : Key := kkAsMin;
          'bes' : Key := kkBbMin;
          'b'   : Key := kkBMin;
        end;
    end;
  end;
  DebugLn('KEY: '); {$ifdef DEBUG}WriteLn(Key);{$endif}
  result := Key;
end;

function AddMEIKeyAttribute(StaffDefNode: TMeiNode; Key: TKeyKind): TMeiNode;
var
  KeySig: String;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.GetName = 'staffDef');

  case Key of 
    kkNone, kkCantusDurus : KeySig := '0';
    kkCantusMollis        : KeySig := '1f';
    kkCMaj,  kkAMin   : KeySig := '0';
    kkGMaj,  kkEMin   : KeySig := '1s';
    kkDMaj,  kkBMin   : KeySig := '2s';
    kkAMaj,  kkFsMin  : KeySig := '3s';
    kkEMaj,  kkCsMin  : KeySig := '4s';
    kkBMaj,  kkGsMin  : KeySig := '5s';
    kkFsMaj, kkDsMin  : KeySig := '6s';
    kkCsMaj, kkAsMin  : KeySig := '7s';
    kkFMaj,  kkDMin   : KeySig := '1f';
    kkBbMaj, kkGMin   : KeySig := '2f';
    kkEbMaj, kkCMin   : KeySig := '3f';
    kkAbMaj, kkFMin   : KeySig := '4f';
    kkDbMaj, kkBbMin  : KeySig := '5f';
    kkGbMaj, kkEbMin  : KeySig := '6f';
    kkCbMaj, kkAbMin  : KeySig := '7f';
  end;
  StaffDefNode.AddAttribute('key.sig', KeySig);
  result := StaffDefNode;
end;

type
  TMeterKind = (mkNone, mkMensuralTempusImperfectum,
    mkMensuralProportioMinor, mkModern);
  
    TMeter = record
    FKind: TMeterKind;
    FCount, FUnit: Integer;
  end;

function FindLyMeter(MeterStr: String): TMeter;
var
  Meter: TMeter;
  SearchStr, NumStr: String;
  MeterNums: TStringArray;
begin
  Meter.FKind := mkNone;
  Meter.FCount := 0;
  Meter.FUnit := 0;

  if MeterStr.Contains('\MeterDuple') then
    Meter.FKind := mkMensuralTempusImperfectum
  else if MeterStr.Contains('\MeterTriple') then
    Meter.FKind := mkMensuralProportioMinor
  else if MeterStr.Contains('\time ') then
  begin
    Meter.FKind := mkModern;
    SearchStr := StringDropBefore(MeterStr, '\time ');
    NumStr := ExtractWord(1, SearchStr, [' ', LineEnding]);
    DebugLn(Format('Looking for meter in string: ''%s''', [NumStr]));

    MeterNums := NumStr.Split(['/'], 2);
    Meter.FCount := StrToInt(MeterNums[0]);
    Meter.FUnit := StrToInt(MeterNums[1]);
  end;

  DebugLn('METER Kind: '); {$ifdef DEBUG}WriteLn(Meter.FKind);{$endif}
  DebugLn(Format('METER Count: %d, Unit: %d', [Meter.FCount, Meter.FUnit])); 

  result := Meter;
end;

function AddMEIMeterAttribute(StaffDefNode: TMeiNode; Meter: TMeter): TMeiNode;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.GetName = 'staffDef');

  with Meter do
  begin
    case FKind of
      mkMensuralTempusImperfectum : 
      begin
        StaffDefNode.AddAttribute('mensur.sign', 'C');
        StaffDefNode.AddAttribute('mensur.tempus', '2');
      end;

      mkMensuralProportioMinor : 
      begin
        StaffDefNode.AddAttribute('mensur.sign', 'C');
        StaffDefNode.AddAttribute('mensur.tempus', '2');
        StaffDefNode.AddAttribute('proport.num', '3');
      end;

      mkModern : 
      begin
        StaffDefNode.AddAttribute('meter.count', IntToStr(FCount));
        StaffDefNode.AddAttribute('meter.unit', IntToStr(FUnit));
      end;
    end;
  end;
  result := StaffDefNode;
end;

function AddStaffDefAttributes(StaffDefNode: TMeiNode; Clef: TClefKind; Key:
  TKeyKind; Meter: TMeter): TMeiNode;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.GetName = 'staffDef');

  StaffDefNode.AddAttribute('lines', '5');
  StaffDefNode := AddMEIClefAttribute(StaffDefNode, Clef);
  StaffDefNode := AddMEIKeyAttribute(StaffDefNode, Key);
  StaffDefNode := AddMEIMeterAttribute(StaffDefNode, Meter);

  result := StaffDefNode;
end;

function AddStaffGrpAttributes(StaffGrpNode: TMeiNode): TMeiNode;
begin
  Assert(Assigned(StaffGrpNode));
  Assert(StaffGrpNode.GetName = 'staffGrp');

  StaffGrpNode.AddAttribute('bar.thru', 'false');
  StaffGrpNode.AddAttribute('symbol', 'bracket');

  result := StaffGrpNode;
end;

{ TODO are we using this? }
function AddStaffNumIDAttributes(StaffNode: TMeiNode; Node: TLyObject):
  TMeiNode; 
begin
  Assert(Assigned(StaffNode));
  Assert(StaffNode.GetName = 'staff');

  StaffNode.AddAttribute('n', IntToStr(Node.FNum));
  StaffNode.AddAttribute('def', '#' + Node.FID);

  result := StaffNode;
end;

function TLyObject.ToMeiScoreDef: TMeiNode;
function InnerScoreDef(LyNode: TLyObject; MeiNode: TMeiNode): TMeiNode;
var 
  SearchStr: String;
  Clef: TClefKind;
  Key: TKeyKind;
  Meter: TMeter;
begin
  if not Assigned(MeiNode) then
  begin
    MeiNode := TMeiNode.Create();
  end;

  MeiNode.AddAttribute('n', IntToStr(LyNode.FNum));
 
  if not LyNode.FID.IsEmpty then
    MeiNode.AddAttribute('xml:id', LyNode.FID); 
  
  case LyNode.FType of
    ekStaffGrp:
    begin
      MeiNode.SetName('staffGrp');
      MeiNode := AddStaffGrpAttributes(MeiNode);
    end;

    ekStaff:
    begin
      MeiNode.SetName('staffDef');
      
      { Extract staffDef info from the first music expression in the first
      child Voice }
      if Assigned(LyNode.FChild) and (LyNode.FChild.FType = ekLayer) then
      begin
        { Search c. first 10 lines }
        SearchStr := LyNode.FChild.FContents.Substring(0, 800); 
        Clef  := FindLyClef(SearchStr);
        Key   := FindLyKey(SearchStr);
        Meter := FindLyMeter(SearchStr);
        MeiNode := AddStaffDefAttributes(MeiNode, Clef, Key, Meter); 
      end;
    end;
  end;

  { Create this element and its children }
  if (LyNode.FType = ekStaffGrp) and Assigned(LyNode.FChild) then
  begin
    MeiNode.AppendChild(InnerScoreDef(LyNode.FChild, nil));
  end;

  { Create its siblings }
  if Assigned(LyNode.FSibling) then
  begin
    MeiNode.AppendSibling(InnerScoreDef(LyNode.FSibling, nil));
  end;

  result := MeiNode;
end;

var
  ScoreDef: TMeiNode;
begin
  ScoreDef := TMeiNode.Create('scoreDef');
  ScoreDef.AppendChild(InnerScoreDef(Self, nil));
  result := ScoreDef;
end;

function TLyObject.ToXMLAsIs(XmlNode: TMeiNode = nil): TMeiNode;
var
  NodeName: String;
begin
  if not Assigned(XmlNode) then
    XmlNode := TMeiNode.Create();

  case FType of
    ekStaffGrp  : NodeName := 'staffGrp';
    ekStaff     : NodeName := 'staff';
    ekLayer     : NodeName := 'layer';
    ekMeasure   : NodeName := 'measure';
    else NodeName := 'xml';
  end;

  XmlNode.SetName(NodeName);

  if NodeName = 'xml' then
  begin
    XmlNode.AddAttribute('type', FName);
  end;

  if not FID.IsEmpty then
  begin
    XmlNode.AddAttribute('xml:id', FID);
  end;

  XmlNode.AddAttribute('n', IntToStr(FNum));
  XmlNode.SetTextNode(FContents);

  if Assigned(FChild) then
  begin
    XmlNode.AppendChild(FChild.ToXMLAsIs);
  end;

  if Assigned(FSibling) then
  begin
    XmlNode.AppendSibling(FSibling.ToXMLAsIs);
  end;

  result := XmlNode;
end;

function CreateMeiScoreDefFromLy(LyInput: TStringListAAC): TMeiNode;
var 
  LyTree: TLyObject;
  MeiScoreDef: TMeiNode = nil;
begin
  LyTree := BuildLyObjectTree(LyInput.Text, nil);
  LyTree.SetNumbers;

  MeiScoreDef := LyTree.ToMeiScoreDef;
  FreeAndNil(LyTree);
  result := MeiScoreDef;
end;

function AddMeiScoreDef(Root: TMeiNode; LyInput: TStringListAAC): TMeiNode;
var
  ScoreDef: TMeiNode = nil;
begin
  assert(Assigned(Root));
  ScoreDef := CreateMeiScoreDefFromLy(LyInput);
  if Assigned(ScoreDef) then
    Root.AppendChild(ScoreDef)
  else
    WriteLn(stderr, 'Could not create scoreDef element');

  result := Root;
end;


end.
