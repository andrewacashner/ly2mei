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
  TPitchName = (pkC, pkD, pkE, pkF, pkG, pkA, pkB, pkRest);
  TAccidental = (akNatural, akFlat, akSharp);
  TDuration = (dkBreve, dkSemibreve, dkMinim, dkSemiminim, dkFusa, dkSemifusa,
    dkBreveDotted, dkSemibreveDotted, dkMinimDotted, dkSemiminimDotted,
    dkFusaDotted);

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
    constructor CreateFromLy(Source: String);
    function MEIPName: String;
    function MEIAccid: String;
    function MEIOct: String;
    function MEIDurDots: String;
    function ToMEI: String;
  end;
  
  TPitchList = class(specialize TObjectList<TPitch>)
  public
    constructor CreateFromLy(Source: String);
    function ToMEI: TStringListAAC;
  end;

  TMeasureList = class(specialize TObjectList<TPitchList>)
  public
    constructor CreateFromLy(Source: String);
  end;

  TMEIElement = class
  private
    var
      FName, FID: String;
      FNum: Integer;
      FMeasures: TMeasureList;
      FChild, FSibling: TMEIElement;
  public
    constructor Create();
    constructor Create(Name, ID: String; Num: Integer = 1; 
      Measures: TMeasureList = nil; Child: TMEIElement = nil; 
      Sibling: TMEIElement = nil);
    destructor Destroy; override;
    function ToString: String; override;
    function LastChild: TMEIElement;
    function LastSibling: TMEIElement;
    function ToMeasures: TMEIElement;
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
    WriteLn(Stderr, 'Could not extract pitch from input "' + LyName + '"');
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
    WriteLn(Stderr, 'Could not extract octave from input "' + OctLy + '"');
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
    WriteLn(Stderr, 'Could not extract duration from input "' + DurLy + '"');
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
  result := XMLAttribute('accid', Accid);
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
  Notes: Array of String = ('');
  NewPitch: TPitch;
begin
  inherited Create;
  MEI := StringDropBefore(Source.TrimLeft, '| ');
  Notes := MEI.Split([' ']);
  for ThisNote in Notes do
  begin
    NewPitch := TPitch.CreateFromLy(ThisNote);
    Self.Add(NewPitch);
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

constructor TMEIElement.Create();
begin
  inherited Create;
end;

constructor TMEIElement.Create(Name, ID: String; Num: Integer = 1; 
  Measures: TMeasureList = nil; Child: TMEIElement = nil; 
  Sibling: TMEIElement = nil); 
begin
  inherited Create;
  FName := Name;
  FID := ID;
  FNum := Num;
  FMeasures := Measures;
  FChild := Child;
  FSibling := Sibling;
end;

function LyToMEITree(LyTree: TLyObject): TMEIElement;
function InnerTree(LyNode: TLyObject; MEINode: TMEIElement): TMEIElement;
var
  NewNode: TMEIElement = nil;
begin
  assert(MEINode <> nil);
  if LyNode <> nil then
  begin
    case LyNode.FType of
      'Staff' : 
      begin
        NewNode := TMEIElement.Create('staff', LyNode.FID, LyNode.FNum);
      end;
      'Voice' :
      begin
        NewNode := TMEIElement.Create('layer', LyNode.FID, LyNode.FNum);
        NewNode.FMeasures := TMeasureList.CreateFromLy(LyNode.FContents); 
      end;
    end;
    if NewNode <> nil then
      MEINode.LastChild.FChild := NewNode;
    
    if LyNode.FChild <> nil then
      MEINode := InnerTree(LyNode.FChild, MEINode);
    if LyNode.FSibling <> nil then
      MEINode := InnerTree(LyNode.FSibling, MEINode);
  end;
  result := MEINode;
end;
var
  MEITree: TMEIElement;
begin
  MEITree := TMEIElement.Create('score', '');
  MEITree := InnerTree(LyTree, MEITree);
  result := MEITree;
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
    OutputStr := OutputStr + FChild.ToString;
  if FSibling <> nil then
    OutputStr := OutputStr + FSibling.ToString;
  result := OutputStr;
end;

function TMEIElement.LastChild: TMEIElement;
begin
  if FChild = nil then
    result := Self
  else
    result := FChild.LastChild;
end;

function TMEIElement.LastSibling: TMEIElement;
begin
  if FSibling = nil then
    result := Self
  else
    result := FSibling.LastSibling;
end;



function TMEIElement.ToMeasures: TMEIElement;
function BuildTree(NodeA, NodeB: TMEIElement): TMEIElement;
var
  ThisStaff, ThisLayer: TMEIElement;
  NewMeasure, NewStaff, NewLayer, NewPitchList: TMEIElement;
  ThisPitchList: TPitchList;
  ThisMeasureList: TMeasureList;
  MeasureNum: Integer;
begin
  if NodeA <> nil then
  begin
    DebugLn('Visiting Tree A Node FName ' + NodeA.FName);
    if (NodeA.FName = 'staff') then
    begin
      ThisStaff := NodeA;
      if (ThisStaff.FChild <> nil) and (ThisStaff.FChild.FName = 'layer') then 
      begin
        ThisLayer := ThisStaff.FChild;
        DebugLn('Visiting layer n ' + IntToStr(ThisLayer.FNum));
        ThisMeasureList := ThisLayer.FMeasures;
        MeasureNum := 0;
        DebugLn('Measure count = ' + IntToStr(ThisMeasureList.Count));
        NewMeasure := TMEIElement.Create('measure', '', MeasureNum + 1);
        { TODO START
        for ThisPitchList in ThisMeasureList do
        begin
          //BuildTree again from root?
          NewStaff := TMEIElement.Create('staff', ThisStaff.FID, ThisStaff.FNum);
          if NewMeasure.FChild = nil then
            NewMeasure.FChild := NewStaff
          else
            NewMeasure.FChild.FSibling := NewStaff;
          
          NewLayer := TMEIElement.Create('layer', ThisLayer.FID, ThisLayer.FNum);
          NewStaff.FChild := NewLayer;
          NewLayer.FMeasures := TMeasureList.Create;
          NewLayer.FMeasures.Add(ThisLayer.FMeasures[MeasureNum]);
          Inc(MeasureNum);
        end;
        }
        if NewMeasure <> nil then
          NodeB.LastChild.FChild := NewMeasure;
      end;
    end;
    if NodeA.FChild <> nil then
      NodeB := BuildTree(NodeA.FChild, NodeB);
    if NodeA.FSibling <> nil then
      NodeB := BuildTree(NodeA.FSibling, NodeB);
  end;
  result := NodeB;
end;
var
  Root: TMEIElement;
begin
  Root := TMEIElement.Create('score', '', 1);
  Root := BuildTree(Self, Root);
  result := Root;
end;

function CreateMEIMusic(SourceLines: TStringListAAC): TStringListAAC;
var
  LyScoreStr: String;
  LyObjectTree: TLyObject = nil;
  MEIMusicLines: TStringListAAC = nil;
  MEIScoreLines: TStringListAAC = nil;
  MEITree: TMEIElement = nil;
  MEIMeasures: TMEIElement = nil;
begin
  LyScoreStr := LyArg(SourceLines.Text, '\score');
  if not LyScoreStr.IsEmpty then
  begin
    LyObjectTree := FindLyNewTree(LyScoreStr, LyObjectTree);
    if LyObjectTree <> nil then
    begin
      LyObjectTree.SetNumbers;
      MEIMusicLines := LyObjectTree.ToNewMEIScoreDef;
      { process music }
      MEITree := LyToMEITree(LyObjectTree);
      DebugLn(MEITree.ToString);
      { TODO START here }
      MEIMeasures := MEITree.ToMeasures;
      DebugLn(MEIMeasures.ToString);
    end;
  end;
  
  if MEIScoreLines = nil then
  begin
    MEIScoreLines := TStringListAAC.Create;
    MEIScoreLines.EncloseInXML('score');
  end;
  MEIMusicLines.AddStrings(MEIScoreLines);
  MEIMusicLines.EncloseInXML('mdiv');
  MEIMusicLines.EncloseInXML('body');
  MEIMusicLines.EncloseInXML('music');

  FreeAndNil(MEIMeasures);
  FreeAndNil(MEITree);
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


