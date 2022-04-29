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

  The @link(TLyObject.ToMeiScoreDef) method extracts the needed information
  from the tree to create a @link(TMeiNode) tree for the MEI @code(scoreDef)
  element.
  
  To create the MEI music elements, we must convert the tree structure from
  the Lilypond hierarchy to that of MEI.  Lilypond @code(\score) expressions
  have a hierarchy of score/StaffGroup or ChoirStaff/Staff/Voice/music
  expression, where the music is a list of measures containing pitches.  MEI
  on the other hand, has a hierarchy of
  score/measure/staffGrp/staff/layer/notes.  So for each MEI measure we must
  build the path staffGrp/staff/layer/notes.

  The @link(TLyObject) object provides two methods for converting the whole
  tree to XML. @link(TLyObject.ToXMLAsIs) creates a @link(TMeiNode) tree that
  copies the structure of the original Lilypond tree; @link(TLyObject.ToMEI)
  creates a @link(TMeiNode) tree with the proper MEI structure.

  To create the MEI structure, the @link(TLyObject) tree contains, at the
  bottom level (equivalent to Lilypond Voice or MEI layer), a list of
  containers corresponding to the music for each measure (each of which
  contains a list of internal representations of a pitch). These structures
  are defined and created in the @link(MusicNotes) unit.  That means we can
  trace the path to each bottom node, copy a given measure's contents, and
  then build the path to that node for each separate measure and each separate
  voice or layer.
}
unit ScoreTree;

interface

uses SysUtils, StrUtils, Classes, StringTools, Outline, MEI, MusicNotes;


type 
  { Types of elements in the internal tree of @code(TLyObject) or
    @code(TMEIElement) objects }
  TMusicTreeElement = (ekAnonymous, ekStaffGrp, ekStaff, ekLayer, ekMeasure);

  { @abstract(A node, which may be the base of a tree, of Lilypond code
    objects.)

    These objects were originally created with @code(\new) commands in the
    @code(\score) expression. Includes fields for a type label, name (string),
    id (string), num (integer), contents (string), measurelist
    (@link(TMeasureList)), and child and sibling trees. 

    To build this left-child/right-sibling tree, we look for two kinds of
    objects: 
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
  TLyObject = class
  private
    var
      { Label for object type, equivalent between Lilypond and MEI }
      FType: TMusicTreeElement;
      { Type of object, e.g., Voice or Lyrics }
      FName: String;
      { ID string, as in @code(\new Voice = "Soprano"). If Lilypond source did
        not supply a label, we generate one randomly. }
      FID: String;
      { In-order position of elements of the same type: Staff elements are
        numbered consecutively regardless of whether they are enclosed in a
        StaffGroup. }
      FNum: Integer;
      { Contents (Lilypond text): If the command is followed by section
        delimited by double angle brackets, then the contents will be blank;
        otherwise, everything up to the end of a curly-brace-delimited
        argument. }
      FContents: String;
      { List of measures (lists of pitch objects), parsed from contents;
        ONLY for @code(ekLayer) type; otherwise empty (0 length)}
      FMeasureList: TMeasureList;
      { Left child node (tree) }
      FChild: TLyObject;
      { Right sibling node (tree) }
      FSibling: TLyObject;

    { Get the right name string for this @code(FType). }
    function NodeName: String;

    { Find the first descendant node with the given type. }
    function FindFirstDescendant(ElementType: TMusicTreeElement): TLyObject;

    { Find the first layer (@link(ekLayer) type) object in the tree in a
    pre-order traversal. }
    function FindFirstLayer: TLyObject;

    { Find the first staff (@link(ekStaff) type) object in the tree in a
    pre-order traversal. }
    function FindFirstStaff: TLyObject;

    { Create a @link(TMeiNode) tree with the path to the first layer in the
      @link(TLyObject) tree. }
    function ToMeiLayerPath(MeiNode: TMeiNode = nil): TMeiNode;
   
    { Follow the left children all the way to the end; return the last one. }
    function LastChild: TLyObject;

    { Follow the right siblings all the way to the end; return the last one. }
    function LastSibling: TLyObject;

    { Add given child to the end of the chain of children. }
    function AppendLastChild(NewChild: TLyObject): TLyObject;

    { Add given sibling to the end of the chain of siblings. }
    function AppendLastSibling(NewSibling: TLyObject): TLyObject;

    { Number the objects with a given FType label consecutively in an in-order
      traversal, regardless of relation to other elements of the tree }
    procedure NumberElementsInOrder(ElementType: TMusicTreeElement);

    { Number ChoirStaff, StaffGroup, Staff, and Voice elements in order.
      We are treating ChoirStaff and StaffGroup as part of the same series.
      (TODO Should we?) }
    procedure SetNumbers;

  public
    constructor Create();

    { The @code(FType) field will be created automatically based on the
      @code(Name) argument. }
    constructor Create(Name, ID: String; ContentsStr: String = ''; Num:
      Integer = 1; Child: TLyObject = nil; Sibling: TLyObject = nil); 

    { Destroy the whole tree. }
    destructor Destroy; override;

    { Write the tree to XML in its original, Lilypond-based structure. }
    function ToXMLAsIs(XmlNode: TMeiNode = nil): TMeiNode;

    { Create an MEI scoreDef element. }
    function ToMeiScoreDef: TMeiNode;
   
    { Create a @link(TMeiNode) tree with the proper MEI structure for the
      music data, converting from Lilypond structure. }
    function ToMEI(MeiScore: TMeiNode = nil): TMeiNode;

    property LyType:    TMusicTreeElement read FType;
    property Name:      String            read FName;
    property ID:        String            read FID;
    property Num:       Integer           read FNum           write FNum;
    property Contents:  String            read FContents;
    property Measures:  TMeasureList      read FMeasureList;
    property Child:     TLyObject         read FChild         write FChild;
    property Sibling:   TLyObject         read FSibling       write FSibling;
  end;

{ Build the MEI @code(scoreDef) element and add it to the given MEI tree. }
function AddMeiScoreDef(Root: TMeiNode; LyInput: TStringList): TMeiNode;

{ Build the MEI @code(score) element and add it to the given MEI tree. }
function AddMeiScore(Root: TMeiNode; LyInput: TStringList): TMeiNode;

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
  
  FMeasureList := TMeasureList.Create();
  if FType = ekLayer then
  begin
    FMeasureList.SetFromLy(FContents);
  end;
end;

destructor TLyObject.Destroy;
begin
  if Assigned(FChild) then 
  begin
    FChild.Destroy;
  end;

  if Assigned(FSibling) then 
  begin
    FSibling.Destroy;
  end;

  FMeasureList.Destroy;
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

function TLyObject.AppendLastChild(NewChild: TLyObject): TLyObject;
begin
  Self.LastChild.FChild := NewChild;
  result := Self;
end;

function TLyObject.AppendLastSibling(NewSibling: TLyObject): TLyObject;
begin
  Self.LastSibling.FSibling := NewSibling;
  result := Self;
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
      Node.Num := N;
    end;
    if Assigned(Node.Child) then
      Node.Child := InnerNums(Node.Child);
    if Assigned(Node.Sibling) then
      Node.Sibling := InnerNums(Node.Sibling);
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

function TLyObject.ToXMLAsIs(XmlNode: TMeiNode = nil): TMeiNode;
begin
  if not Assigned(XmlNode) then
    XmlNode := TMeiNode.Create();

  XmlNode.Name := NodeName;

  if NodeName = 'xml' then
  begin
    XmlNode.AddAttribute('type', FName);
  end;

  if not FID.IsEmpty then
  begin
    XmlNode.AddAttribute('xml:id', FID);
  end;

  XmlNode.AddAttribute('n', IntToStr(FNum));
  
  if FType = ekLayer then
  begin
    XMLNode.AppendChild(FMeasureList.ToMEI);
  end;

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



{ TODO make this a constructor? }
{ Build an LCRS tree of Lilypond @code(\new) objects. }
function BuildLyObjectTree(Source: String; Tree: TLyObject): TLyObject;
var
  SearchIndex: Integer = 0;
  SearchStr, ThisType, ThisID, ThisContents: String;
  Outline: TIndexPair;
begin
  Outline := TIndexPair.Create;
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
      Outline.MarkBalancedDelimiterSubstring(SearchStr, '<', '>');
      ThisContents := CopyStringRange(SearchStr, Outline, rkInclusive);
      if Tree = nil then
        Tree := TLyObject.Create(ThisType, ThisID)
      else
        Tree := Tree.AppendLastChild(TLyObject.Create(ThisType, ThisID));

      Tree := Tree.AppendLastChild(BuildLyObjectTree(ThisContents, nil));
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
        Tree := Tree.AppendLastSibling(TLyObject.Create(ThisType, ThisID,
          ThisContents));

      Source := Source.Substring(SearchIndex + 1); 
    end;

    { Look for the next sibling where you left off from the last search }
    Tree := Tree.AppendLastSibling(BuildLyObjectTree(Source, nil));
  end;
  FreeAndNil(Outline);
  result := Tree;
end;

{ Build the tree and number the elements. }
function CreateLyObjectTreeFromLy(LyInput: TStringList): TLyObject;
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
  
  result := Clef;
end;

function AddMEIClefAttribute(StaffDefNode: TMeiNode; Clef: TClefKind): TMeiNode;
var
  ClefLine: Integer;
  ClefLetter: String;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.Name = 'staffDef');

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

function AddMEIKeyAttribute(StaffDefNode: TMeiNode; Key: TKeyKind): TMeiNode;
var
  KeySig: String;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.Name = 'staffDef');

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

    MeterNums := NumStr.Split(['/'], 2);
    Meter.FCount := StrToInt(MeterNums[0]);
    Meter.FUnit := StrToInt(MeterNums[1]);
  end;

  result := Meter;
end;

function AddMEIMeterAttribute(StaffDefNode: TMeiNode; Meter: TMeter): TMeiNode;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.Name = 'staffDef');

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
  Assert(StaffDefNode.Name = 'staffDef');

  StaffDefNode.AddAttribute('lines', '5');
  StaffDefNode := AddMEIClefAttribute(StaffDefNode, Clef);
  StaffDefNode := AddMEIKeyAttribute(StaffDefNode, Key);
  StaffDefNode := AddMEIMeterAttribute(StaffDefNode, Meter);

  result := StaffDefNode;
end;

function AddStaffGrpAttributes(StaffGrpNode: TMeiNode): TMeiNode;
begin
  Assert(Assigned(StaffGrpNode));
  Assert(StaffGrpNode.Name = 'staffGrp');

  StaffGrpNode.AddAttribute('bar.thru', 'false');
  StaffGrpNode.AddAttribute('symbol', 'bracket');

  result := StaffGrpNode;
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

  MeiNode.AddAttribute('n', IntToStr(LyNode.Num));
 
  if not LyNode.ID.IsEmpty then
    MeiNode.AddAttribute('xml:id', LyNode.ID); 
  
  case LyNode.FType of
    ekStaffGrp:
    begin
      MeiNode.Name := 'staffGrp';
      MeiNode := AddStaffGrpAttributes(MeiNode);
    end;

    ekStaff:
    begin
      MeiNode.Name := 'staffDef';
      
      { Extract staffDef info from the first music expression in the first
      child Voice }
      if Assigned(LyNode.Child) and (LyNode.Child.LyType = ekLayer) then
      begin
        { Search c. first 10 lines }
        SearchStr := LyNode.Child.Contents.Substring(0, 800); 
        Clef  := FindLyClef(SearchStr);
        Key   := FindLyKey(SearchStr);
        Meter := FindLyMeter(SearchStr);
        MeiNode := AddStaffDefAttributes(MeiNode, Clef, Key, Meter); 
      end;
    end;
  end;

  { Create this element and its children }
  if (LyNode.LyType = ekStaffGrp) and Assigned(LyNode.Child) then
  begin
    MeiNode.AppendChild(InnerScoreDef(LyNode.Child, nil));
  end;

  { Create its siblings }
  if Assigned(LyNode.Sibling) then
  begin
    MeiNode.AppendSibling(InnerScoreDef(LyNode.Sibling, nil));
  end;

  result := MeiNode;
end;

var
  ScoreDef, MainStaffGrp: TMeiNode;
begin
  ScoreDef := TMeiNode.Create('scoreDef');
  MainStaffGrp := TMeiNode.Create('staffGrp');
  ScoreDef.AppendChild(MainStaffGrp);
  MainStaffGrp.AppendChild(InnerScoreDef(Self, nil));
  result := ScoreDef;
end;

function TLyObject.NodeName: String;
var
  NameString: String;
begin
  case FType of
    ekStaffGrp  : NameString := 'staffGrp';
    ekStaff     : NameString := 'staff';
    ekLayer     : NameString := 'layer';
    ekMeasure   : NameString := 'measure';
    else NameString := 'xml';
  end;
  result := NameString;
end;

{ TODO how many times are we building the TLyObject tree? }
{ Build the @link(TLyObject) tree and create the @code(scoreDef) element. }
function CreateMeiScoreDefFromLy(LyInput: TStringList): TMeiNode;
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


function AddMeiScoreDef(Root: TMeiNode; LyInput: TStringList): TMeiNode;
var
  ScoreDef: TMeiNode = nil;
begin
  Assert(Assigned(Root));
  ScoreDef := CreateMeiScoreDefFromLy(LyInput);
  if Assigned(ScoreDef) then
    Root.AppendChild(ScoreDef)
  else
    WriteLn(stderr, 'Could not create scoreDef element');

  result := Root;
end;


function TLyObject.FindFirstDescendant(ElementType: TMusicTreeElement): 
  TLyObject;
var
  FoundNode: TLyObject = nil;
begin
  if FType = ElementType then
    FoundNode := Self
  else if Assigned(FChild) then
  begin
    FoundNode := FChild.FindFirstDescendant(ElementType);
  end;

  if not Assigned(FoundNode) and Assigned(FSibling) then
  begin
    FoundNode := FSibling.FindFirstDescendant(ElementType);
  end;

  result := FoundNode;
end;

function TLyObject.FindFirstLayer: TLyObject;
begin
  result := FindFirstDescendant(ekLayer);
end;

function TLyObject.FindFirstStaff: TLyObject;
begin
  result := FindFirstDescendant(ekStaff);
end;

function TLyObject.ToMeiLayerPath(MeiNode: TMeiNode = nil): TMeiNode;
begin
  if not Assigned(MeiNode) then
  begin
    case FType of
      ekStaff, ekLayer :
      begin
        MeiNode := TMeiNode.Create(NodeName);
        if not FID.IsEmpty then
        begin
          MeiNode.AddAttribute('xml:id', FID);
        end;
        MeiNode.AddAttribute('n', IntToStr(FNum));
      end;
      { Ignore other node types }
    end;
  end;

  if FType = ekLayer then
    result := MeiNode
  else if Assigned(FChild) then
    result := MeiNode.AppendChild(FChild.ToMeiLayerPath)
  else 
    result := nil;
end;

{ TODO need to make sure all voices have same number of measures }
function CreateMeiMeasure(LyLayer: TLyObject; MeasureNum: Integer): TMeiNode;
var
  MeasureNode: TMeiNode = nil;
  MeasureList: TMeasureList;
begin
  if (LyLayer.LyType = ekLayer) then
  begin
    MeasureList := LyLayer.Measures;
    if MeasureNum < MeasureList.Count then
    begin
      MeasureNode := LyLayer.Measures.Items[MeasureNum].ToMEI;
    end
    else
      WriteLn(stderr, 'Measure number out of range');
  end;
  
  result := MeasureNode;
end;

function AddMeiFermatasAndLines(LyNode: TLyObject; MeiMeasure: TMeiNode; 
  MeasureNum: Integer): TMeiNode;
var
  ThisMeasureList: TMeasureList;
  ThisMeasure: TPitchList;
  LyFermatas: TFermataList;
  LyLines: TLineList;
  MeiFermatas, MeiLines: TMeiNode;
begin
  Assert(Assigned(LyNode) and Assigned(MeiMeasure));
  Assert(LyNode.LyType = ekLayer);

  ThisMeasureList := LyNode.Measures;

  if MeasureNum < ThisMeasureList.Count then
  begin
    ThisMeasure := ThisMeasureList.Items[MeasureNum];
    LyFermatas := ThisMeasure.FermataList;
    MeiFermatas := LyFermatas.ToMEI;
    MeiMeasure.AppendChild(MeiFermatas);

    LyLines := ThisMeasure.LineList;
    MeiLines := LyLines.ToMEI;
    MeiMeasure.AppendChild(MeiLines);
  end;

  result := MeiMeasure;
end;

function BuildMeiMeasureTree(LyTree: TLyObject; MeiTree: TMeiNode; 
  MeasureNum: Integer): TMeiNode;
var
  MeiLayerPath, MeiMusicNode: TMeiNode;
  LyStaff, LyLayer: TLyObject;
begin
  Assert(Assigned(MeiTree));

  if Assigned(LyTree) then
  begin
    LyStaff := LyTree.FindFirstStaff;
    LyLayer := LyStaff.FindFirstLayer;
    
    MeiLayerPath := LyStaff.ToMeiLayerPath;
    MeiMusicNode := CreateMeiMeasure(LyLayer, MeasureNum);
    MeiLayerPath.AppendLastChild(MeiMusicNode);
    MeiTree.AppendChild(MeiLayerPath);

    MeiTree := AddMeiFermatasAndLines(LyLayer, MeiTree, MeasureNum);

    if Assigned(LyStaff.Sibling) then
    begin
      MeiTree := BuildMeiMeasureTree(LyStaff.Sibling, MeiTree, MeasureNum);
    end;
  
    if (LyTree.LyType <> ekStaff) and Assigned(LyTree.Sibling) then
    begin
      MeiTree := BuildMeiMeasureTree(LyTree.Sibling, MeiTree, MeasureNum);
    end;
  end;
  
  result := MeiTree;
end;


{ Flip an @code(LyObjectTree) with a staff/voice/measure hierarchy
  to make a @code(MeiNode) tree with a measure/staff/voice hierarchy. }
  { TODO we do not support multiple \score expressions }
function TLyObject.ToMEI(MeiScore: TMeiNode = nil): TMeiNode;
var
  LayerNode: TLyObject;
  MeasureList: TMeasureList;
  MeiMeasureTree: TMeiNode;
  MeasureCount, MeasureNum: Integer;
begin
  LayerNode := Self.FindFirstLayer;
  if not Assigned(MeiScore) then
  begin
    MeiScore := TMeiNode.Create('section');
  end;

  if Assigned(LayerNode) then
  begin
    MeasureList := LayerNode.Measures;
    MeasureCount := MeasureList.Count;
    for MeasureNum := 0 to (MeasureCount - 1) do
    begin
      MeiMeasureTree := TMeiNode.Create('measure');
      MeiMeasureTree.AddAttribute('n', IntToStr(MeasureNum + 1));

      { Section heading (from MeasureList), first measure only }
      if MeasureNum = 0 then
      begin
        MeiMeasureTree := MeasureList.AddMeiSectionHead(MeiMeasureTree);
      end;

      { Barline for this measure (from PitchList = lirio:measure) }
      MeiMeasureTree := AddMeiBarlineAttr(MeiMeasureTree, 
        MeasureList.Items[MeasureNum]);

      { For this measure, staff/layer/notes }
      MeiMeasureTree := BuildMeiMeasureTree(Self, MeiMeasureTree, MeasureNum);
      MeiScore.AppendChild(MeiMeasureTree);

    end;
  end;
  result := MeiScore;
end;

function CreateEmptyMeiScore: TMeiNode;
var
  MeiMusic, MeiBody, MeiMdiv, MeiScore: TMeiNode;
begin
    MeiMusic := TMeiNode.Create('music');
    MeiBody := TMeiNode.Create('body');
    MeiMdiv := TMeiNode.Create('mdiv');
    MeiScore := TMeiNode.Create('score');
    MeiMusic.AppendChild(MeiBody);
    MeiBody.AppendChild(MeiMdiv);
    MeiMdiv.AppendChild(MeiScore);
    result := MeiMusic;
end;

function AddMeiScore(Root: TMeiNode; LyInput: TStringList): TMeiNode;
var
  LyTree: TLyObject = nil;
  Score, ScoreDef, Measures: TMeiNode;
begin
  Assert(Assigned(Root));
 
  Score := CreateEmptyMeiScore;
  Root.AppendChild(Score);

  Measures := nil;
  LyTree := CreateLyObjectTreeFromLy(LyInput);
  
  if Assigned(LyTree) then
  begin
    Measures := LyTree.ToMEI;
    FreeAndNil(LyTree);
  end;

  if Assigned(Measures) then
  begin
    ScoreDef := CreateMeiScoreDefFromLy(LyInput);
    if Assigned(ScoreDef) then
    begin
      Score.AppendLastChild(ScoreDef);
      ScoreDef.AppendSibling(Measures);
    end
    else
    begin
      WriteLn(stderr, 'Could not create scoreDef element');
    end;
  end
  else
  begin
    WriteLn(stderr, 'Could not create score element');
  end;

  result := Root;
end;

end.

