{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(Convert a Lilypond music expression to MEI.)
  @author(Andrew Cashner)
}
unit MusicNotes;

{
  ly start:
    Staff     : TVoiceList
    Voice     : TMeasureList
    | ... \n  : TPitchList
    c'4       : TPitch
  mei end:
    <measure> : TStaffList
      <staff> : TVoiceList
        <layer> : TPitchList
          <note> : TPitch
}

interface

uses SysUtils, StrUtils, Classes, Generics.Collections, StringTools, Outline,
ScoreTree;

type 
  TPitchName = (pkNone, pkC, pkD, pkE, pkF, pkG, pkA, pkB, pkRest);
  TAccidental = (akNatural, akFlat, akSharp);
  TDuration = (dkNone, dkBreve, dkSemibreve, dkMinim, dkSemiminim, dkFusa,
    dkSemifusa, dkBreveDotted, dkSemibreveDotted, dkMinimDotted,
    dkSemiminimDotted, dkFusaDotted);

function GetPitchKind(LyName: String): TPitchName;
function GetOctave(OctLy: String): Integer;
function GetDurationKind(DurLy: String): TDuration;
function GetAccidKind(LyName: String): TAccidental;

type
  TPitch = class
  public
    var
      FPitchName: TPitchName;
      FAccid: TAccidental;
      FOct: Integer;
      FDur: TDuration;
    constructor Create();
    constructor Create(Name: TPitchName; Accid: TAccidental; Oct: Integer;
      Dur: TDuration); 
    constructor CreateFromLy(Source: String);
    procedure Assign(Source: TPitch); 
    function MEIPName: String;
    function MEIAccid: String;
    function MEIOct: String;
    function MEIDurDots: String;
    function ToMEI: String;
  end;
  
  TPitchList = class(specialize TObjectList<TPitch>)
  public
    constructor CreateFromLy(Source: String);
    procedure Assign(Source: TPitchList);
    function ToMEI: TStringListAAC;
  end;

  TMeasureList = class(specialize TObjectList<TPitchList>)
  public
    constructor CreateFromLy(Source: String);
    procedure Assign(Source: TMeasureList);
  end;

  TMeasureCopyMode = (mkAllMeasures, mkOneMeasure);

  TMEIElement = class
  private
    var
      FType: TMusicTreeElement;
      FName, FID: String;
      FNum: Integer;
      FMeasures: TMeasureList;
      FChild, FSibling: TMEIElement;
  public
    constructor Create();
    constructor Create(ElementType: TMusicTreeElement; ID: String; 
      Num: Integer = 1; Measures: TMeasureList = nil; 
      Child: TMEIElement = nil; Sibling: TMEIElement = nil);
    destructor Destroy; override;
    function LastSibling: TMEIElement;
    procedure AssignNode(Source: TMEIElement; Mode: TMeasureCopyMode =
      mkAllMeasures; MeasureIndex: Integer = 0); 
    procedure AssignTree(Source: TMEIElement; Mode: TMeasureCopyMode =
      mkAllMeasures; MeasureIndex: Integer = 0); 
    procedure SetFromLyObject(LyObject: TLyObject);
    function CountMeasures: Integer;
    function ToString: String; override;
    function StaffToMeasureTree: TMEIElement;
    function ToMEI: TStringListAAC;
  end;

function LyToMEITree(LyTree: TLyObject): TMEIElement;

{ Parse a Lilypond \score expression and create an MEI music
  element including the scoreDef and music notes }
function CreateMEIMusic(SourceLines: TStringListAAC): TStringListAAC;


implementation

function GetPitchKind(LyName: String): TPitchName;
var PitchName: TPitchName;
begin
  case LyName.Substring(0, 1) of 
    'c': PitchName := pkC;
    'd': PitchName := pkD;
    'e': PitchName := pkE;
    'f': PitchName := pkF;
    'g': PitchName := pkG;
    'a': PitchName := pkA;
    'b': PitchName := pkB;
    'r': PitchName := pkRest;
    else 
      PitchName := pkNone;
  end;
  result := PitchName;
end;

function GetOctave(OctLy: String): Integer;
var
  Oct: Integer;
begin
  case OctLy of 
    ',,,'     : Oct := 0;
    ',,'      : Oct := 1;
    ','       : Oct := 2;
    ''        : Oct := 3;
    ''''      : Oct := 4; { '' }
    ''''''    : Oct := 5; { ''' }
    ''''''''  : Oct := 6; { '''' }
    else 
      Oct := -1;
  end;
  result := Oct;
end;

function GetDurationKind(DurLy: String): TDuration;
var
  Dur: TDuration;
begin
  case DurLy of
    '\breve'  : Dur := dkBreve;
    '1'       : Dur := dkSemibreve;
    '2'       : Dur := dkMinim;
    '4'       : Dur := dkSemiminim;
    '8'       : Dur := dkFusa;
    '16'      : Dur := dkSemifusa;
    '\breve.' : Dur := dkBreveDotted;
    '1.'      : Dur := dkSemibreveDotted;
    '2.'      : Dur := dkMinimDotted;
    '4.'      : Dur := dkSemiminimDotted;
    '8.'      : Dur := dkFusaDotted;
    else
      Dur := dkNone
  end;
  result := Dur;
end;

function GetAccidKind(LyName: String): TAccidental;
var
  Accid: TAccidental;
begin
  if LyName.EndsWith('is') then
    Accid := akSharp
  else if LyName.EndsWith('es') then
    Accid := akFlat
  else
    Accid := akNatural;
  result := Accid;
end;

constructor TPitch.Create();
begin
  inherited Create;
end;

constructor TPitch.Create(Name: TPitchName; Accid: TAccidental; Oct: Integer;
  Dur: TDuration);
begin
  inherited Create;
  FPitchName := Name;
  FAccid := Accid;
  FOct := Oct;
  FDur := Dur;
end;

constructor TPitch.CreateFromLy(Source: String);
var
  NoteStr, PitchNameLy, OctLy, DurLy, EtcLy: String;
begin
  inherited Create;
  NoteStr := Source;
  PitchNameLy := ExtractWord(1, NoteStr, [',', '''', '1', '2', '4', '8', '\']);
  NoteStr := StringDropBefore(NoteStr, PitchNameLy);
  
  OctLy := ExtractWord(1, NoteStr, ['1', '2', '4', '8', '\']);
  if OctLy <> '' then
    NoteStr := StringDropBefore(NoteStr, OctLy);
 
  DurLy := ExtractWord(1, NoteStr, ['(', ')', '~', '\']);
  if DurLy <> '' then
  begin
    NoteStr := StringDropBefore(NoteStr, DurLy);
    EtcLy := NoteStr; { TODO }
  end;

  FPitchName := GetPitchKind(PitchNameLy);
  FAccid     := GetAccidKind(PitchNameLy);
  FOct       := GetOctave(OctLy);
  FDur       := GetDurationKind(DurLy);
 
  { TODO how to deal with unexpected input? }
  if (FPitchName = pkNone) or (FOct = -1) or (FDur = dkNone) then
  begin
    WriteLn(Stderr, 'Error creating pitch from string ' + Source);
    halt;
  end;
end;

procedure TPitch.Assign(Source: TPitch);
begin
  if Source <> nil then
  begin
    FPitchName := Source.FPitchName;
    FAccid := Source.FAccid;
    FOct := Source.FOct;
    FDur := Source.FDur;
  end;
end;

function TPitch.MEIPname: String;
var
  Pname: String;
begin
  case FPitchName of
    pkC : Pname := 'c';
    pkD : Pname := 'd';
    pkE : Pname := 'e';
    pkF : Pname := 'f';
    pkG : Pname := 'g';
    pkA : Pname := 'a';
    pkB : Pname := 'b';
  end;
  result := XMLAttribute('pname', Pname);
end;

function TPitch.MEIAccid: String;
var
  Accid: String;
begin
  case FAccid of
    akNatural : Accid := 'n';
    akFlat    : Accid := 'f';
    akSharp   : Accid := 's';
  end;
  result := XMLAttribute('accid.ges', Accid);
  { TODO do we have to check every accidental against the key signature to
  know whether to do accid or accid.ges ? }
end;

function TPitch.MEIOct: String;
begin
  result := XMLAttribute('oct', IntToStr(FOct));
end;

function TPitch.MEIDurDots: String;
var
  DurBase, Dur: String;
  Dots: Boolean;
begin
  case FDur of 
    dkBreve           : DurBase := 'breve';
    dkSemibreve       : DurBase := '1';
    dkMinim           : DurBase := '2';
    dkSemiminim       : DurBase := '4';
    dkFusa            : DurBase := '8';
    dkSemifusa        : DurBase := '16';
    dkBreveDotted     : DurBase := 'breve';
    dkSemibreveDotted : DurBase := '1';
    dkMinimDotted     : DurBase := '2';
    dkSemiminimDotted : DurBase := '4';
    dkFusaDotted      : DurBase := '8';
  end;
  case FDur of
    dkBreve .. dkSemifusa         : Dots := False;
    dkBreveDotted .. dkFusaDotted : Dots := True;
  end;
  Dur := XMLAttribute('dur', DurBase);
  if Dots then
    Dur := Dur + ' ' + XMLAttribute('dots', '1');
  result := Dur;
end;

function TPitch.ToMEI: String;
var
  Dur: String;
begin
  Dur := MEIDurDots;
  if FPitchName = pkRest then
    result := XMLElement('rest', '', Dur)
  else
    result := XMLElement('note', '', MEIPname + ' ' + MEIAccid + ' ' + MEIOct
      + ' ' + Dur); 
end;

constructor TPitchList.CreateFromLy(Source: String);
var
  MEI, ThisNote: String;
  Notes: TStringArray;
  NewPitch: TPitch;
begin
  inherited Create;
  MEI := StringDropBefore(Source.TrimLeft, '| ');
  Notes := MEI.Split([' ']);
  for ThisNote in Notes do
  begin
    NewPitch := TPitch.CreateFromLy(ThisNote);
    { TODO test if NewPitch is valid? }
    if NewPitch <> nil then
      Self.Add(NewPitch);
  end;
end;

procedure TPitchList.Assign(Source: TPitchList);
var
  ThisPitch, NewPitch: TPitch;
begin
  if Source <> nil then
  begin
    for ThisPitch in Source do
    begin
      NewPitch := TPitch.Create;
      NewPitch.Assign(ThisPitch);
      Self.Add(NewPitch);
    end;
  end;
end;

function TPitchList.ToMEI: TStringListAAC;
var
  ThisPitch: TPitch;
  MEI: TStringListAAC;
begin
  MEI := TStringListAAC.Create;
  for ThisPitch in Self do
    MEI.Add(ThisPitch.ToMEI);
  result := MEI;
end;

constructor TMeasureList.CreateFromLy(Source: String);
var
  LyLines: TStringListAAC;
  ThisLine: String;
begin
  inherited Create;
  LyLines := TStringListAAC.Create(Source);
  for ThisLine in LyLines do
  begin
    if ThisLine.TrimLeft.StartsWith('|') 
      and (ThisLine.CountChar('|') = 1) then 
    begin
      DebugLn('Adding new TPitchList to TMeasureList...');
      Self.Add(TPitchList.CreateFromLy(ThisLine));
    end;
  end;
  FreeAndNil(LyLines);
end;

procedure TMeasureList.Assign(Source: TMeasureList);
var
  ThisMeasure, NewPitchList: TPitchList;
begin
  if Source <> nil then
  begin
    for ThisMeasure in Source do
    begin
      NewPitchList := TPitchList.Create;
      NewPitchList.Assign(ThisMeasure);
     Self.Add(NewPitchList);
    end;
  end;
end;

constructor TMEIElement.Create();
begin
  inherited Create;
end;

function TypeToName(Element: TMusicTreeElement): String;
var
  Name: String;
begin
  case Element of
    ekStaffGrp: Name := 'staffGrp';
    ekStaff:    Name := 'staff';
    ekLayer:    Name := 'layer';
    ekMeasure:  Name := 'measure';
  else Name := 'UNKNOWN';
  end;
  result := Name;
end;

constructor TMEIElement.Create(ElementType: TMusicTreeElement; ID: String;
  Num: Integer = 1; Measures: TMeasureList = nil; Child: TMEIElement = nil;
  Sibling: TMEIElement = nil); 
begin
  inherited Create;
  FType     := ElementType;
  FName     := TypeToName(ElementType);
  FID       := ID;
  FNum      := Num;
  FMeasures := Measures;
  FChild    := Child;
  FSibling  := Sibling;
end;

procedure TMEIElement.AssignNode(Source: TMEIElement; Mode: TMeasureCopyMode =
  mkAllMeasures; MeasureIndex: Integer = 0); 
var
  NewMeasure: TPitchList;
begin
  FType := Source.FType;
  FName := Source.FName;
  FID   := Source.FID;
  FNum  := Source.FNum;
  if Source.FMeasures <> nil then
  begin
    FMeasures := TMeasureList.Create;
    case Mode of 
      mkAllMeasures: FMeasures.Assign(Source.FMeasures);
      mkOneMeasure:
      begin
        if MeasureIndex < Source.FMeasures.Count then
        begin
          NewMeasure := TPitchList.Create;
          NewMeasure.Assign(Source.FMeasures[MeasureIndex]);
          FMeasures.Add(NewMeasure);
        end;
      end;
    end;
  end;
  FChild   := nil;
  FSibling := nil;
end;

procedure TMEIElement.AssignTree(Source: TMEIElement; Mode: TMeasureCopyMode =
  mkAllMeasures; MeasureIndex: Integer = 0); 
function InnerAssign(TreeA, TreeB: TMEIElement): TMEIElement;
begin
  if TreeA <> nil then
  begin
    if TreeB = nil then
      TreeB := TMEIElement.Create;
   
    TreeB.AssignNode(TreeA, Mode, MeasureIndex);

    if TreeA.FChild <> nil then
      TreeB.FChild := InnerAssign(TreeA.FChild, TreeB.FChild);
    if TreeA.FSibling <> nil then
      TreeB.FSibling:= InnerAssign(TreeA.FSibling, TreeB.FSibling);
  end;
  result := TreeB;
end;
begin
  InnerAssign(Source, Self);
end;


procedure TMEIElement.SetFromLyObject(LyObject: TLyObject);
begin
  FType     := LyObject.FType;
  FName     := TypeToName(LyObject.FType);
  FID       := LyObject.FID;
  FNum      := LyObject.FNum;
  FChild    := nil;
  FSibling  := nil;
  if LyObject.FContents = '' then
    FMeasures := nil
  else
    FMeasures := TMeasureList.CreateFromLy(LyObject.FContents);
end;

function TMEIElement.CountMeasures: Integer;
var
  MasterCount: Integer = 0;
function InnerCount(Node: TMEIElement): Integer;
var
  ThisCount: Integer = 0;
begin
  if Node <> nil then
  begin
    if (Node.FType = ekLayer) and (Node.FMeasures <> nil) then
    begin
      ThisCount := Node.FMeasures.Count;
      if MasterCount = 0 then
        MasterCount := ThisCount
      else if ThisCount <> MasterCount then
      begin
        result := -1;
        exit;
      end;
    end;
    if Node.FChild <> nil then
      ThisCount := InnerCount(Node.FChild);
    if Node.FSibling <> nil then
      ThisCount := InnerCount(Node.FSibling);
  end;
  result := ThisCount;
end;
begin
  result := InnerCount(Self)
end;

function LyToMEITree(LyTree: TLyObject): TMEIElement;
function InnerTree(LyNode: TLyObject; MEINode: TMEIElement): TMEIElement;
begin
  if LyNode <> nil then
  begin
    case LyNode.FType of
      ekStaff, ekLayer:
      begin
        if MEINode = nil then
        begin
          MEINode := TMEIElement.Create;
        end;
        MEINode.SetFromLyObject(LyNode);
      end;
    end;
    if LyNode.FChild <> nil then
    begin
      if MEINode = nil then
        MEINode := InnerTree(LyNode.FChild, MEINode)
      else if MEINode.FType = LyNode.FChild.FType then
        MEINode.FSibling := InnerTree(LyNode.FChild, MEINode.FSibling)
      else
        MEINode.FChild := InnerTree(LyNode.FChild, MEINode.FChild);
    end;
    if LyNode.FSibling <> nil then
    begin
      if MEINode = nil then
        MEINode := InnerTree(LyNode.FSibling, MEINode)
      else
        MEINode.FSibling := InnerTree(LyNode.FSibling, MEINode.FSibling);
    end;
  end;
  result := MEINode;
end;
begin
  result := InnerTree(LyTree, nil);
end;

destructor TMEIElement.Destroy;
begin
  FreeAndNil(FMeasures);
  if FChild <> nil then 
    FChild.Destroy;
  if FSibling <> nil then 
    FSibling.Destroy;
  inherited Destroy;
end;

function TMEIElement.LastSibling: TMEIElement;
begin
  if FSibling = nil then
    result := Self
  else
    result := FSibling.LastSibling;
end;


function TMEIElement.ToString: String;
var
  OutputStr: String = '';
  ThisPitchList: TPitchList;
  MEIPitchLines: TStringListAAC;
begin
  OutputStr := OutputStr + FName + ' ' + FID + ' ' + IntToStr(FNum) + LineEnding;
  if FMeasures <> nil then
  begin
    for ThisPitchList in FMeasures do
    begin
      MEIPitchLines := ThisPitchList.ToMEI;
      OutputStr := OutputStr + MEIPitchLines.Text;
      FreeAndNil(MEIPitchLines);
    end;
  end;
  if FChild <> nil then
    OutputStr := OutputStr + 'CHILD (to ' + Fname + ' ' + IntToStr(FNum) +
        '): ' + FChild.ToString; if FSibling <> nil then
    OutputStr := OutputStr + 'SIBLING (to ' + Fname + ' ' + IntToStr(FNum) +
        '): ' + FSibling.ToString; 
  result := OutputStr;
end;

function TMEIElement.StaffToMeasureTree: TMEIElement;
var
  MeasureCount, MeasureNum: Integer;
  MeasureTree, Root, Branch: TMEIElement;
begin
  MeasureCount := Self.CountMeasures;
  DebugLn('MEASURE COUNT: ' + IntToStr(MeasureCount));
  if MeasureCount <= 0 then
  begin
    result := nil;
    exit;
  end;
  for MeasureNum := 0 to MeasureCount - 1 do
  begin
    Root := TMEIElement.Create(ekMeasure, '', MeasureNum + 1);
    Branch := TMEIElement.Create;
    Branch.AssignTree(Self, mkOneMeasure, MeasureNum);
    Root.FChild := Branch;
    if MeasureNum = 0 then
      MeasureTree := Root
    else
      MeasureTree.LastSibling.FSibling := Root;
  end;
  result := MeasureTree;
end;

function TMEIElement.ToMEI: TStringListAAC;
var
  MasterMEI: TStringListAAC;

function InnerToMEI(Node: TMEIElement; MEI: TStringListAAC): TStringListAAC;

function InnerAddNewElement(Node: TMEIElement; List: TStringListAAC): TStringListAAC;
var
  NewLines: TStringListAAC;
begin
  assert(List <> nil);
  NewLines := TStringListAAC.Create;
  NewLines := InnerToMEI(Node, NewLines);
  List.AddStrings(NewLines);
  FreeAndNil(NewLines);
  result := List;
end;

var
  NewElement, NewMeasure: TStringListAAC;
begin
  if Node <> nil then
  begin
    NewElement := TStringListAAC.Create;

    if (Node.FType = ekLayer) and (Node.FMeasures <> nil) then
    begin
      NewMeasure := Node.FMeasures[0].ToMEI;
      NewElement.AddStrings(NewMeasure);
      FreeAndNil(NewMeasure);
    end;

    if Node.FChild <> nil then
      NewElement := InnerAddNewElement(Node.FChild, NewElement);
 
    NewElement.EncloseInXML(Node.FName, XMLAttributeIDNum(Node.FID, Node.FNum));

    if Node.FSibling <> nil then
      NewElement := InnerAddNewElement(Node.FSibling, NewElement);
  end;
  MEI.AddStrings(NewElement);
  FreeAndNil(NewElement);
  result := MEI;
end;

begin
  MasterMEI := TStringListAAC.Create;
  MasterMEI := InnerToMEI(Self, MasterMEI);
  result := MasterMEI;
end;

function CreateMEIMusic(SourceLines: TStringListAAC): TStringListAAC;
var
  LyScoreStr: String;
  LyObjectTree: TLyObject = nil;
  MEIMusicLines: TStringListAAC = nil;
  MEIScoreLines: TStringListAAC = nil;
  MEIStaffTree: TMEIElement = nil;
  MEIMeasureTree: TMEIElement = nil;
begin
  LyScoreStr := LyArg(SourceLines.Text, '\score');
  if not LyScoreStr.IsEmpty then
  begin
    LyObjectTree := BuildLyObjectTree(LyScoreStr, LyObjectTree);
    LyObjectTree.SetNumbers;
    DebugLn('LYOBJECT TREE, NUMBERED:' + LineEnding + LyObjectTree.ToString);
    
    MEIMusicLines := LyObjectTree.ToMEIScoreDef;
    
    MEIStaffTree := LyToMEITree(LyObjectTree);
    DebugLn('MEI TREE STAGE 1:' + LineEnding + MEIStaffTree.ToString);

    MEIMeasureTree := MEIStaffTree.StaffToMeasureTree;
    DebugLn('MEI TREE STAGE 2:' + LineEnding + MEIMeasureTree.ToString);

    MEIScoreLines := MEIMeasureTree.ToMEI;
  end;
  
  if MEIScoreLines <> nil then
    MEIScoreLines.EncloseInXML('section');
    
  MEIMusicLines.AddStrings(MEIScoreLines);
  MEIMusicLines.EncloseInXML('score');
  MEIMusicLines.EncloseInXML('mdiv');
  MEIMusicLines.EncloseInXML('body');
  MEIMusicLines.EncloseInXML('music');

  FreeAndNil(MEIMeasureTree);
  FreeAndNil(MEIStaffTree);
  FreeAndNil(MEIScoreLines);
  FreeAndNil(LyObjectTree);
  result := MEIMusicLines;
end;


end.

{ TODO find and replace all the commands that have a simple one-to-one match }
{
function ReplaceCommand(Source: String): String;
var
  OutputStr: String;
begin
  case Source of
    '\clef "treble"'  :
    '\clef "bass"'    :
    '\CantusMollis'   :
    '\MeterDuple'     :
    '\MeterTriple'    :
    '\MiddleBar'      :
    '\FinalBar'       :
    '\RepeatBarStart' :
    '\RepeatBarEnd'   :
    '\Fine'           :
    '\FineEd'         :
    '\break'          :
    '\fermata'        :
  else
  end;
  
  result := OutputStr;
end;
}


