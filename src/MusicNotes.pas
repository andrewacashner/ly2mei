{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(Convert a Lilypond music expression to MEI.)
  @author(Andrew Cashner)
}
unit MusicNotes;

interface

uses SysUtils, StrUtils, Classes, Generics.Collections, StringTools, ScoreTree;

type 
  TPitchName = (pkC, pkD, pkE, pkF, pkG, pkA, pkB, pkRest);
  TAccidental = (akNatural, akFlat, akSharp);
  TDuration = (dkBreve, dkSemibreve, dkMinim, dkSemiminim, dkFusa, dkSemifusa, 
    dkBreveDotted, dkSemibreveDotted, dkMinimDotted, dkSemiminimDotted, dkFusaDotted);

type
  TPitch = class
  public
    var
      FPitchName: TPitchName;
      FAccid: TAccidental;
      FOct: Integer;
      FDur: TDuration;
    constructor CreateFromLy(Source: String);
    function ToMEI: String;
  end;

  TPitchList = class(specialize TObjectList<TPitch>)
  public
    constructor CreateFromLy(Source: String);
    function ToMEI(OutputLines: TStringListAAC): TStringListAAC;
  end;

  TLyMeasure  = TPitchList;
  TLyVoice    = specialize TObjectList<TPitchList>;
  
  TLyStaff    = class(specialize TObjectList<TLyVoice>)
  public
    constructor Create(ObjectTree: TLyObject);
  end;

  TLyMusic    = class(specialize TObjectList<TLyStaff>)
  public
    constructor Create(ObjectTree: TLyObject);
  end;

  TMEILayer   = specialize TObjectList<TPitchList>;
  TMEIStaff   = specialize TObjectList<TMEILayer>;
  TMEIMeasure = specialize TObjectList<TMEIStaff>;
  TMEIMusic   = specialize TObjectList<TMEIMeasure>;

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

function LyMeasureToMEI(LyInput: String): TPitchList;
function LyMusicTextToVoice(LyInput: TStringListAAC): TLyVoice;

{
function LyMeasureListToMEI(LyInput: TStringListAAC; MeasureList: TLyVoice):
  TLyVoice;

function LyMeasuresToMEI(LyInput, MEIOutput: TStringListAAC): TStringListAAC;
}

implementation

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
    EtcLy := NoteStr;
  end;

  if PitchNameLy.EndsWith('is') then
    FAccid := akSharp
  else if PitchNameLy.EndsWith('es') then
    FAccid := akFlat
  else
    FAccid := akNatural;
  
  case PitchNameLy.Substring(0, 1) of
    'c': FPitchName := pkC;
    'd': FPitchName := pkD;
    'e': FPitchName := pkE;
    'f': FPitchName := pkF;
    'g': FPitchName := pkG;
    'a': FPitchName := pkA;
    'b': FPitchName := pkB;
    'r': FPitchName := pkRest;
  else
    WriteLn(Stderr, 'Could not extract pitch from input "' + PitchNameLy + '"');
  end;

  case OctLy of
    ',,,'     : FOct := 0;
    ',,'      : FOct := 1;
    ','       : FOct := 2;
    ''        : FOct := 3;
    ''''      : FOct := 4; { '' }
    ''''''    : FOct := 5; { ''' }
    ''''''''  : FOct := 6; { '''' }
  else
    WriteLn(Stderr, 'Could not extract octave from input "' + OctLy + '"');
  end;

  case DurLy of
    '\breve'  : FDur := dkBreve;
    '1'       : FDur := dkSemibreve;
    '2'       : FDur := dkMinim;
    '4'       : FDur := dkSemiminim;
    '8'       : FDur := dkFusa;
    '16'      : FDur := dkSemifusa;
    '\breve.' : FDur := dkBreveDotted;
    '1.'      : FDur := dkSemibreveDotted;
    '2.'      : FDur := dkMinimDotted;
    '4.'      : FDur := dkSemiminimDotted;
    '8.'      : FDur := dkFusaDotted;
  else
    WriteLn(Stderr, 'Could not extract duration from input "' + DurLy + '"');
  end;
end;

function TPitch.ToMEI: String;
var
  Pnum, Oct, Accid, DurBase, Dur: String;
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
  Dur := 'dur="' + DurBase + '"';
  if Dots then
    Dur := Dur + ' dots="1"';

  if FPitchName = pkRest then
    result := '<rest dur="' + Dur + '"/>'
  else
  begin
    case FPitchName of
      pkC : Pnum := 'c';
      pkD : Pnum := 'd';
      pkE : Pnum := 'e';
      pkF : Pnum := 'f';
      pkG : Pnum := 'g';
      pkA : Pnum := 'a';
      pkB : Pnum := 'b';
    end;
    case FAccid of
      akNatural : Accid := 'n';
      akFlat    : Accid := 'f';
      akSharp   : Accid := 's';
    end;
    Oct := IntToStr(FOct);
    result := '<note pname="' + Pnum + '" accid="' + Accid 
              + '" oct="' + Oct + '" ' + Dur + '></note>';
  end;
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

function LyMeasureToPitchList(LyInput: String): TPitchList;
var
  PitchList: TPitchList;
begin
  if LyInput.TrimLeft.StartsWith('|') and (LyInput.CountChar('|') = 1) then
    PitchList := TPitchList.CreateFromLy(LyInput);
  result := PitchList;
end;

function LyMusicTextToVoice(LyInput: TStringListAAC): TLyVoice;
var
  LyVoice: TLyVoice;
  ThisString: String;
begin
  assert(LyInput <> nil);
  LyVoice := TLyVoice.Create;
  for ThisString in LyInput do
    LyVoice.Add(LyMeasureToPitchList(ThisString));
  result := LyVoice;
end;


function LyMeasureToMEI(LyInput: String): TPitchList;
var
  PitchList: TPitchList;
begin
  if LyInput.TrimLeft.StartsWith('|') and (LyInput.CountChar('|') = 1) then
    PitchList := TPitchList.CreateFromLy(LyInput);
  result := PitchList;
end;


{
function LyMeasureListToMEI(LyInput: TStringListAAC; MeasureList: TMeasureList):
  TMeasureList; 
var
  ThisString: String;
begin
  assert(LyInput <> nil);
  assert(MeasureList <> nil);
  for ThisString in LyInput do
    MeasureList.Add(LyMeasureToMEI(ThisString));
  result := MeasureList;
end;

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

constructor TLyStaff.Create(ObjectTree: TLyObject);
var
  Node: TLyObject;
begin
  assert(ObjectTree <> nil);
  inherited Create;
  Node := ObjectTree.FChild;
  while Node <> nil do
  begin
    if Node.FType = 'Voice' then
    begin
      Self.Add(LyMusicTextToVoice(TStringListAAC.Create(Node.FContents)));
    end;
    Node := Node.FSibling;
  end;
end;

constructor TLyMusic.Create(ObjectTree: TLyObject);
begin
  assert(ObjectTree <> nil);
  inherited Create;
  { TODO this doesn't work as recursive any more:
  possibly an inner function to create staves? }
  if ObjectTree.FType = 'Staff' then
    Self.Add(TLyStaff.Create(ObjectTree.FChild));
  {  
  if (ObjectTree.FChild <> nil) then
    LyMusic(ObjectTree.FChild));
  if (ObjectTree.FSibling <> nil) then
    Self.Add(MakeLyMusic(ObjectTree.FSibling));
  }
end;

end.
