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
    constructor CreateFromLyMeasure(Source: String);
    function ToMEI(OutputLines: TStringListAAC): TStringListAAC;
  end;

  TMeasureList = specialize TObjectList<TPitchList>;

  TMeasureListPath = class
  private
    var
      FParentStaff: TLyObject;
      FParentVoice: TLyObject;
      FMeasures: TMeasureList;
  public
    constructor Create(Staff, Voice: TLyObject; Measures: TMeasureList);
    destructor Destroy; override;
  end;

  TMusicList = class(specialize TObjectList<TMeasureListPath>)
  public
    constructor Create(Tree: TLyObject);
  end;

function ParsePitchesInScoreTree(Tree: TLyObject): TLyObject;

{ Parse a Lilypond \score expression and create an MEI music
  element including the scoreDef and music notes }
function CreateMEIScore(SourceLines: TStringListAAC): TStringListAAC;


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
    result := XMLElement('note', '', MEIPname + MEIAccid + MEIOct + Dur);
end;

constructor TPitchList.CreateFromLyMeasure(Source: String);
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

{ TODO create outputlines instead of receive as arg? }
function TPitchList.ToMEI(OutputLines: TStringListAAC): TStringListAAC;
var
  ThisPitch: TPitch;
begin
  assert(OutputLines <> nil);
  OutputLines.Clear;
  for ThisPitch in Self do
    OutputLines.Add(ThisPitch.ToMEI);
  result := OutputLines;
end;

constructor TMeasureListPath.Create(Staff, Voice: TLyObject; Measures:
  TMeasureList);
begin
  inherited Create;
  FParentStaff := TLyObject.Create(Staff.FType, Staff.FID, Staff.FNum);
  FParentVoice := TLyObject.Create(Voice.FType, Voice.FID, Voice.FNum);
  FMeasures := Measures;
end;

destructor TMeasureListPath.Destroy;
begin
  FreeAndNil(FParentStaff);
  FreeAndNil(FParentVoice);
  FreeAndNil(FMeasures);
  inherited Destroy;
end;

constructor TMusicList.Create(Tree: TLyObject);
function TraverseBuildList(Node: TLyObject; MusicList: TMusicList): TLyObject;
var
  LyLines: TStringListAAC;
  MeasureList: TMeasureList;
  Contents, MusicLine: String;
begin
  if Node <> nil then
  begin
    if Node.FType = 'Staff' then
    begin
      if (Node.FChild <> nil) and (Node.FChild = 'Voice') then
      begin
        Contents := Tree.FContents;
        if Contents.TrimLeft.StartsWith('|') 
          and (Contents.CountChar('|') = 1) then 
        begin
          LyLines := TStringListAAC.Create(ContentsStr);
          MeasureList := TMeasureList.Create;
          for MusicLine in LyLines do
          begin
            MeasureList.Add(TPitchList.CreateFromLyMeasure(MusicLine));
          end;
          MusicList.Add(TMeasureListPath.Create(Node, Node.FChild, MeasureList);
          FreeAndNil(LyLines);
        end;
      end;
    end;
    if Node.FChild <> nil then
      Node.FChild := Traverse(Node.FChild, MusicList);
    if Node.FSibling <> nil then
      Node.FSibling := Traverse(Node.FSibling, MusicList);
  end;
  result := Node;
end;
begin
  inherited Create;
  Node := TraverseBuildList(Node, Self);
end;

{ TODO START work through TLyObject tree to make PitchLists for each measure, then
make new tree with rearranged MEI hierarchy, then convert to MEI strings }

{
constructor TLyVoice.CreateFromLy(LyInput: TStringListAAC);
var
  ThisString: String;
begin
  assert(LyInput <> nil);
  inherited Create;
  for ThisString in LyInput do
  begin
    if ThisString.TrimLeft.StartsWith('|') and (ThisString.CountChar('|') = 1) then
      Self.Add(TPitchList.CreateFromLyMeasure(ThisString));
  end;
end;

constructor TLyStaff.Create(ObjectTree: TLyObject);
function Traverse(Node: TLyObject): TLyObject;
var
  LyMusicLines: TStringListAAC;
begin
  if Node <> nil then
  begin
    if Node.FType = 'Voice' then
    begin
      LyMusicLines := TStringListAAC.Create(Node.FContents);
      Self.Add(TLyVoice.CreateFromLy(LyMusicLines));
      FreeAndNil(LyMusicLines);
    end;
    if Node.FChild <> nil then
      Node.FChild := Traverse(Node.FChild);
    if Node.FSibling <> nil then
      Node.FSibling := Traverse(Node.FSibling);
  end;
  result := Node;
end;

begin
  assert(ObjectTree <> nil);
  inherited Create;
  ObjectTree := Traverse(ObjectTree);
end;

constructor TLyMusic.Create(ObjectTree: TLyObject);
function Traverse(Node: TLyObject): TLyObject;
begin
  if Node <> nil then
  begin
    if Node.FType = 'Staff' then
      Self.Add(TLyStaff.Create(Node));
    if Node.FChild <> nil then
      Node.FChild := Traverse(Node.FChild);
    if Node.FSibling <> nil then
      Node.FSibling := Traverse(Node.FSibling);
  end;
  result := Node
end;

begin
  assert(ObjectTree <> nil);
  inherited Create;
  ObjectTree := Traverse(ObjectTree);
end;

function TLyMusic.ToString: String;
var
  LyStaff: TLyStaff;
  LyVoice: TLyVoice;
  PitchList: TPitchList;
  Pitch: TPitch;
  OutputStr: String = '';
  ThisPitchStr: String;
begin
  for LyStaff in Self do
  begin
    OutputStr := OutputStr + LineEnding + 'staff: ';
    for LyVoice in LyStaff do
    begin
      OutputStr := OutputStr + LineEnding + '  voice: ';
      for PitchList in LyVoice do
      begin
        OutputStr := OutputStr + ', measure: ';
        for Pitch in PitchList do
        begin
          WriteStr(ThisPitchStr, Pitch.FPitchName);
          OutputStr := OutputStr + ThisPitchStr + IntToStr(Pitch.FOct) + ' ';
        end;
      end;
    end;
  end;
  result := OutputStr;
end;

function MakeMEIMusic(LyMusic: TLyMusic): TMEIMusic;
var
  LyStaff: TLyStaff;
  LyVoice: TLyVoice;
  MEILayer: TMEILayer;
  MEIStaff: TMEIStaff;
  MEIMeasure: TMEIMeasure;
  MEIMusic: TMEIMusic;
  MeasureNum: Integer;
begin
  MEIMusic := TMEIMusic.Create;
  for MeasureNum := 0 to LyMusic[0][0][0].Count - 1 do
  begin
    MEIMeasure := TMEIMeasure.Create;
    for LyStaff in LyMusic do
    begin
      MEIStaff := TMEIStaff.Create;
      for LyVoice in LyStaff do
      begin
        MEILayer := TMEILayer.Create;
        MEILayer.Add(LyVoice[MeasureNum]);
      end;
      MEIStaff.Add(MEILayer);
      MEIMeasure.Add(MEIStaff);
    end;
    MEIMusic.Add(MEIMeasure);
  end;
  result := MEIMusic;
end;
}

function CreateMEIScore(SourceLines: TStringListAAC): TStringListAAC;
var
  LyScoreStr: String;
  LyObjectTree: TLyObject = nil;
  MEIScoreLines: TStringListAAC = nil;
  MusicList: TMusicList;
begin
  LyScoreStr := LyArg(SourceLines.Text, '\score');
  if not LyScoreStr.IsEmpty then
  begin
    LyObjectTree := FindLyNewTree(LyScoreStr, LyObjectTree);
    if LyObjectTree <> nil then
    begin
      LyObjectTree.SetStaffNums;
      MEIScoreLines := LyObjectTree.ToNewMEIScoreDef;
      { process music }
      MusicList := TMusicList.Create(LyObjectTree);
    end;
  end;
  
  if MEIScoreLines = nil then
    MEIScoreLines := TStringListAAC.Create;

  MEIScoreLines.EncloseInXML('score');
  MEIScoreLines.EncloseInXML('mdiv');
  MEIScoreLines.EncloseInXML('body');
  MEIScoreLines.EncloseInXML('music');

  FreeAndNil(MusicList);
  FreeAndNIl(LyObjectTree);
  result := MEIScoreLines;
end;


end.
{
function LyMeasuresToMEI(LyInput, MEIOutput: TStringListAAC): TStringListAAC;
var
  ThisString: String;
  PitchList: TPitchList;
  TempLines: TStringListAAC;
  N: Integer;
begin
  assert(LyInput <> nil);
  assert(MEIOutput <> nil);
  TempLines := TStringListAAC.Create;
  try
    MEIOutput.Clear;
    N := 0;
    for ThisString in LyInput do
    begin
      TempLines.Clear;
      if ThisString.TrimLeft.StartsWith('|') and (ThisString.CountChar('|') = 1) then
      begin
        PitchList := TPitchList.CreateFromLy(ThisString);
        try
          TempLines := PitchList.ToMEI(TempLines);
          Inc(N);
          TempLines := XMLElementLines(TempLines, 'measure', 'n="' + IntToStr(N) + '"');
        finally
          MEIOutput.AddStrings(TempLines);
          FreeAndNil(PitchList);
        end;
      end;
    end;
  finally
    FreeAndNil(TempLines);
    result := MEIOutput;
  end;
end;
}

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


