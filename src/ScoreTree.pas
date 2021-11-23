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

  TClefKind = (ckNone, ckTreble, ckSoprano, ckMezzoSoprano, ckAlto, ckTenor,
    ckBaritone, ckBass, ckTreble8va); 

  TMeterKind = (mkNone, mkMensuralTempusImperfectum,
    mkMensuralProportioMinor, mkModern);

function FindLyKey(KeyStr: String): TKeyKind;

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
    function ToMEIScoreDef: TStringListAAC;
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

function BuildLyObjectTree(Source: String; Tree: TLyObject): TLyObject;
var
  SearchIndex: Integer = 0;
  SearchStr, ThisType, ThisID, ThisContents: String;
  Outline: TIndexPair;
begin
  if Length(Source) = 0 then
  begin
    result := Tree;
    exit;
  end;
 
  SearchStr := Source;
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
  { NOTE treating ChoirStaff and StaffGroup as part of same series }
  NumberElementsInOrder(ekStaffGrp); 
  NumberElementsInOrder(ekStaff);
  NumberElementsInOrder(ekLayer);
end;

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
  DebugLn('CLEF kind: ');
  {$ifdef DEBUG} WriteLn(Clef); {$endif}

  result := Clef;
end;

function MEIClef(Clef: TClefKind): String;
var
  ClefLine: Integer;
  ClefLetter: Char;
  ClefDis: String;
  XML: String;
begin
  case Clef of
    ckSoprano                  : ClefLine := 1;
    ckTreble, ckMezzoSoprano,  
      ckTreble8va              : ClefLine := 2;
    ckAlto, ckBaritone         : ClefLine := 3;
    ckTenor, ckBass            : ClefLine := 4;
  end;
  case Clef of
    ckTreble, ckTreble8va       : ClefLetter := 'G';
    ckSoprano, ckMezzoSoprano,   
      ckAlto, ckTenor           : ClefLetter := 'C';
    ckBaritone, ckBass          : ClefLetter := 'F';
  end;
  case Clef of
    ckTreble8va : ClefDis := ' ' + XMLAttribute('clef.dis', '8') + ' '
                              + XMLAttribute('clef.dis.place', 'below');
    else
      ClefDis := '';
  end;
  XML := XMLAttribute('clef.line', IntToStr(ClefLine)) + ' '
            + XMLAttribute('clef.shape', ClefLetter) + ClefDis;
  result := XML;
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
  DebugLn('KEY: ');
  {$ifdef DEBUG}WriteLn(Key);{$endif}

  result := Key;
end;

function MEIKey(Key: TKeyKind): String;
var
  KeySig: String;
begin
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
  result := XMLAttribute('key.sig', KeySig);
end;

type
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
    DebugLn('Looking for meter in string: ''' + NumStr + '''');

    MeterNums := NumStr.Split(['/'], 2);
    Meter.FCount := StrToInt(MeterNums[0]);
    Meter.FUnit := StrToInt(MeterNums[1]);
  end;

  DebugLn('METER Kind: ');
  {$ifdef DEBUG}WriteLn(Meter.FKind);{$endif}
  DebugLn('METER Count: ' + IntToStr(Meter.FCount) + ', Unit: ' +
    IntToStr(Meter.FUnit)); 

  result := Meter;
end;

function MEIMeter(Meter: TMeter): String;
var
  MEI: String;
begin
  with Meter do
  begin
    case FKind of
      mkMensuralTempusImperfectum : 
        MEI := 'mensur.sign="C" mensur.tempus="2"';
      mkMensuralProportioMinor : 
        MEI := 'mensur.sign="C" mensur.tempus="2" proport.num="3"';
      mkModern : 
        MEI := XMLAttribute('meter.count', IntToStr(FCount)) + ' ' 
                + XMLAttribute('meter.unit', IntToStr(FUnit));
      else
        MEI := '';
    end;
  end;
  result := MEI;
end;

function StaffDefAttributes(Clef: TClefKind; Key: TKeyKind; Meter: TMeter): String;
var
  ClefStr, KeyStr, MeterStr, OutputStr: String;
begin
  OutputStr := XMLAttribute('lines', '5');
  ClefStr := MEIClef(Clef);
  KeyStr := MEIKEy(Key);
  MeterStr := MEIMeter(Meter);
  result := OutputStr + ' ' + ClefStr + ' ' + KeyStr + ' ' + MeterStr;
end;

function StaffGrpAttributes(): String;
begin
  result := ' ' + XMLAttribute('bar.thru', 'false') + ' '
            + XMLAttribute('symbol', 'bracket');
end;

function StaffNumID(Node: TLyObject): String;
begin
  result := XMLAttribute('n', IntToStr(Node.FNum)) 
            + XMLAttribute(' def', '#' + Node.FID);
end;

function TLyObject.ToMEIScoreDef: TStringListAAC;
function InnerScoreDef(Node: TLyObject; InnerLines: TStringListAAC): TStringListAAC;
var 
  ThisTag, SearchStr, Attributes: String;
  TempLines: TStringListAAC;
  Clef: TClefKind;
  Key: TKeyKind;
  Meter: TMeter;
begin
  assert(InnerLines <> nil);
  TempLines := TStringListAAC.Create;
  ThisTag := '';
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
        Clef  := FindLyClef(SearchStr);
        Key   := FindLyKey(SearchStr);
        Meter := FindLyMeter(SearchStr);
      end;
      Attributes := Attributes + ' ' + StaffDefAttributes(Clef, Key, Meter);
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
  FreeAndNil(TempLines);
  result := InnerLines;
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
