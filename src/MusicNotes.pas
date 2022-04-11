{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(Convert a Lilypond music expression to MEI.)
  @author(Andrew Cashner)

  We parse a tree of Lilypond objects (@code(TLyObject), from the ScoreTree unit)
  into an internal representation of the music data. We then convert that
  representation from Lilypond's hierarchy of data into that needed for MEI.

  Lilypond (in our specification) is organized score/staff/voice/measures/notes.
  MEI requires score/section/measure/staff/layer/notes.
}
unit MusicNotes;

interface

uses SysUtils, StrUtils, Classes, Generics.Collections, StringTools,
ScoreTree, MEI;

type 
  { Labels for pitch classes }
  TPitchName = (pkNone, pkC, pkD, pkE, pkF, pkG, pkA, pkB, pkRest,
    pkMeasureRest);

  { Labels for accidentals }
  TAccidental = (akFlat, akNatural, akSharp);

  { Labels for types of accidentals }
  TAccidType = (
    akImplicit, {< Played but not written if part of the scale in its key }
    akExplicit, {< Played and written out, not part of the scale }
    akFicta     {< Suggested editorially for historical performance practice
                    (TODO not implemented) }
  ); 

  { Labels for rhythmic durations }
  TDuration = (
    dkNone,               {< None set or unrecognized }
    dkBreve,              {< Double whole note }
    dkSemibreve,          {< Whole }
    dkMinim,              {< Half }
    dkSemiminim,          {< Quarter }
    dkFusa,               {< Eighth }
    dkSemifusa,           {< Sixteenth}
    dkBreveDotted,        {< Double whole, dotted }
    dkSemibreveDotted,    {< Whole, dotted }
    dkMinimDotted,        {< Half, dotted }
    dkSemiminimDotted,    {< Quarter, dotted }
    dkFusaDotted          {< Eigth, dotted }
  );

  { Label for position of a note in a markup such a tie or slur }
  TMarkupPosition = (
    mkNone,
    mkStart,
    mkMiddle,
    mkEnd,
    mkEndStart {< end one and start another on same note }
  );

  { Label for types of lines extending from one note to another }
  TLineKind = (lkNone, lkSlur, lkColoration, lkLigature);

  { Records which articulations a pitch has (correspond to MEI
    @code(data.ARTICULATION) }
  TArticulationSpec = record
    FFermata, FAccent, FStaccato, FTenuto, FStaccatissimo, FMarcato: Boolean;
  end; 

  { Types of bar lines }
  TBarline = (bkNormal, bkMiddle, bkFinal, bkRepeatEnd, bkRepeatStart);
  
  { @abstract(Internal data structure for a single pitch or rest.)

    Our internal structure for storing data for a pitch, using the labels
    above except for octave, which is an integer in the Helmholtz system (middle
    C is C4). 

    This class also represents rests, when the @code(FPitchName) field is set
    to @code(pkRest) or @code(pkMeasureRest). } 
  TPitch = class
  private 
    var
      { From automatically generated GUID }
      FID: String;

      { Label for pitch name, e.g., @link(pkC) or if rest, @link(pkRest) }
      FPitchName: TPitchName;
      
      { Label for accidental, e.g., @link(akNatural) }
      FAccid: TAccidental;
     
      { Label for accidental type (explicitly written out or implied by key
      signature) }
      FAccidType: TAccidType;

      { Helmholtz octave number }
      FOct: Integer;

      { Label for duration, e.g., @link(dkMinim) }
      FDur: TDuration;

      { Label indicates whether tied or not, and if so, what is the position
        (start/middle/end)? }
      FTie: TMarkupPosition;

      { Label indicates whether a slur is connected to this note, and if so,
         its position }
      FSlur: TMarkupPosition;

      { Label indicates position of note in a coloration bracket, if any }
      FColoration: TMarkupPosition;

      { Label indicates position of note in a ligature bracket, if any }
      FLigature: TMarkupPosition;

      { Record with boolean flags for possible articulation labels }
      FArticulations: TArticulationSpec;

      { A string with additional text paired with this pitch. }
      FAnnotation: String;

      
  public
    constructor Create(); 

    { Create from a Lilypond input string; set the accidental relative to the
      given key. }
    constructor CreateFromLy(Source: String; Key: TKeyKind);

    { Copy all the fields from an existing pitch to this one. }
    procedure Assign(Source: TPitch); 

    { When we find invalid input, we construct invalid pitches, with the
      fields set to "none" or negative values. Did we find a valid pitch? }
    function IsValid: Boolean;

    { A rest is a @code(TPitch) with the pitch name set to @code(pkRest) and
      only the duration. }
    function IsRest: Boolean;

    { Is the pitch class and accidental the same as another? (Ignoring
      duration and other fields) }
    function PitchEq(P2: TPitch): Boolean;
  end;

      

  { @abstract(A list of @link(TPitch) objects, corresponding to one measure of
     music.) }
  TPitchList = class(specialize TObjectList<TPitch>)
  private
    var
      FLines: String; { TODO make a slur class }
      FFermata: String; { TODO make a fermata class }
      FBarlineRight: TBarline;
  public
    { Create a new list of pitches from the Lilypond input string for a single
      measure of music, and the key relevant to this music. Recursively create
      all the pitches contained in the list. }
    constructor CreateFromLy(Source: String; Key: TKeyKind);

    { Deep copy of all pitches from another list to this one. }
    procedure Assign(Source: TPitchList);

    procedure SetBarlineRight(Source: String);

    { Generate an MEI @code(measure) element, recursively generating the
      @code(note) elements it contains. }
    function ToMEI: TMeiNode;
  end;


  { @abstract(A list of @link(TPitchList) objects for a single voice/layer.)

    This list contains a list of all the measures of music for a single voice
    (Lilypond) or layer (MEI). }
  TMeasureList = class(specialize TObjectList<TPitchList>)
  private
    var
      FPrefix: TMeiNode;
  public
    { Deep copy of all measures and pitches in another list to this one. }
    procedure Assign(Source: TMeasureList);

    { Set the contents of a list from the Lilypond input string for a
      single voice. Find the key for this music and then recursively create
      all the measures, and in turn all the pitches, it contains. }
    procedure SetFromLy(Source: String);

    { Go through measure list in which only tie starts have been set (from
      Lilypond input), and set attributes for notes in the middle and ending
      of the tie. }
    procedure AddTies;
    
    { Go through measure list in which line starts and ends have been
      marked in the @link(TPitch) elements, and create MEI line elements
      connecting those notes. Used for slurs, coloration brackets, and
      ligature brackets (MEI @code(slur) and @code(bracketSpan) elements). }
    procedure AddLines(LineKind: TLineKind);

    { Go through measure list and add fermata elements within the MEI
      @code(measure) linked to their notes by the @code(startid). }
    procedure AddFermatas;
  end;

  TLirioVoice = class(TMeiNode)
  private
    var 
      FMeasureList: TMeasureList;
  public
    constructor Create(); 
    constructor Create(LySource: String);
    destructor Destroy; override;
    function GetMeasure(Index: Integer): TPitchList;
  end;


    { First we copy a @link(TLyObject) tree to a @link(TMEIElement) tree,
      preserving its structure (score/staff/voice/measures). With this
      function we create a new tree that is organized in the MEI hierarchy
      (section/measure/staff/layer/notes).

      We first count the measures and make sure all voices have the same
      number of measures. Then for each measure, we create a new tree for that
      measure, where the child element is the entire original tree
      (staff/layer), but we include only a single measure in the list at the
      bottom, corresponding to the music for this measure. }

{ Convert a @code(TLyObject) tree to a @code(TMEIElement) tree, preserving the
  same hierarchy from the Lilypond input. 
function LyToMEITree(LyNode: TLyObject; MEINode: TMEIElement): TMEIElement;
}

  TMeiNoteRest = class(TMeiNode)
  private
    function IsNote: Boolean;
    function IsRest: Boolean;

    { Generate the MEI @code(pname) attribute for the pitch name. }
    procedure AddMeiPnameAttribute(Pitch: TPitch);

    { Generate the MEI @code(accid) and/or @code(accid.ges) attributes for the
      accidental, depending on whether the accidental is to be written
      explicitly in this key. }
    procedure AddMeiAccidAttribute(Pitch: TPitch);

    { Generate the MEI @code(oct) attribute for the octave. }
    procedure AddMeiOctAttribute(Pitch: TPitch);

    { Generate the MEI @code(dur) and @code(dots) attributes for the rhythmic
      duration. }
    procedure AddMeiDurDotsAttributes(Pitch: TPitch);

    { Generate the MEI @code(tie) attribute. }
    procedure AddMeiTieAttribute(Pitch: TPitch);

    { Generate one or more MEI @code(artic) elements within a @code(note). }
    procedure AddMeiArticAttribute(Pitch: TPitch);

  public
    constructor CreateFromPitch(Pitch: TPitch);
  end;

function AddMeiBarlineAttr(MeiMeasure: TMeiNode; PitchList: TPitchList):
  TMeiNode;

function ParseLyMusic(Tree: TMeiNode): TMeiNode;

implementation

function GetPitchName(LyName: String): TPitchName;
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
    'R': PitchName := pkMeasureRest;
    '' : PitchName  := pkNone;
    else 
    begin
      WriteLn(StdErr, 'Unrecognized pitch name: ' + LyName);
      PitchName := pkNone;
    end;
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
    ''        : Dur := dkNone;
    else
    begin
      WriteLn(StdErr, 'Unrecognized duration: ' + DurLy);
      Dur := dkNone;
    end;
  end;
  result := Dur;
end;

function GetAccid(LyName: String): TAccidental;
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

const
  Gamut: Array [1..15, 1..7] of TAccidental = (
     { pkC        pkD         pkE         pkF       pkG       pkA         pkB } 
{ C  } (akNatural, akNatural, akNatural, akNatural, akNatural, akNatural, akNatural),
{ G  } (akNatural, akNatural, akNatural, akSharp,   akNatural, akNatural, akNatural),
{ D  } (akSharp,   akNatural, akNatural, akSharp,   akNatural, akNatural, akNatural),
{ A  } (akSharp,   akNatural, akNatural, akSharp,   akSharp,   akNatural, akNatural),
{ E  } (akSharp,   akSharp,   akNatural, akSharp,   akSharp,   akNatural, akNatural),
{ B  } (akSharp,   akSharp,   akNatural, akSharp,   akSharp,   akSharp,   akNatural),
{ F# } (akSharp,   akSharp,   akSharp,   akSharp,   akSharp,   akSharp,   akNatural),
{ C# } (akSharp,   akSharp,   akSharp,   akSharp,   akSharp,   akSharp,   akSharp),
{ F  } (akNatural, akNatural, akNatural, akNatural, akNatural, akNatural, akFlat),
{ Bb } (akNatural, akNatural, akFlat,    akNatural, akNatural, akNatural, akFlat),
{ Eb } (akNatural, akNatural, akFlat,    akNatural, akNatural, akFlat,    akFlat),
{ Ab } (akNatural, akFlat,    akFlat,    akNatural, akNatural, akFlat,    akFlat),
{ Db } (akNatural, akFlat,    akFlat,    akNatural, akFlat,    akFlat,    akFlat),
{ Gb } (akFlat,    akFlat,    akFlat,    akNatural, akFlat,    akFlat,    akFlat),
{ Cb } (akFlat,    akFlat,    akFlat,    akFlat,    akFlat,    akFlat,    akFlat));
{ mensa tonographica in memoriam P. Athanasii Kircheri }

function AccidInKey(PitchName: TPitchName; Key: TKeyKind): TAccidental;
var
  KeyIndex, PitchIndex: Integer;
begin
  case Key of
    kkCMaj,  kkAMin,  
       kkCantusDurus : KeyIndex := 1;
    kkGMaj,  kkEMin  : KeyIndex := 2;
    kkDMaj,  kkBMin  : KeyIndex := 3;
    kkAMaj,  kkFsMin : KeyIndex := 4;
    kkEMaj,  kkCsMin : KeyIndex := 5;
    kkBMaj,  kkGsMin : KeyIndex := 6;
    kkFsMaj, kkDsMin : KeyIndex := 7;
    kkCsMaj, kkAsMin : KeyIndex := 8;
    kkFMaj,  kkDMin,  
      kkCantusMollis : KeyIndex := 9;
    kkBbMaj, kkGMin  : KeyIndex := 10;
    kkEbMaj, kkCMin  : KeyIndex := 11;
    kkAbMaj, kkFMin  : KeyIndex := 12;
    kkDbMaj, kkBbMin : KeyIndex := 13;
    kkGbMaj, kkEbMin : KeyIndex := 14;
    kkCbMaj, kkAbMin : KeyIndex := 15;
    else
    begin
      WriteLn(StdErr, 'Unrecognized key, substituting CMajor');
      KeyIndex := 1;
    end;
  end;

  case PitchName of
    pkC : PitchIndex := 1;
    pkD : PitchIndex := 2;
    pkE : PitchIndex := 3;
    pkF : PitchIndex := 4;
    pkG : PitchIndex := 5;
    pkA : PitchIndex := 6;
    pkB : PitchIndex := 7;
    else
    begin
      WriteLn(StdErr, 'Unrecognized pitch name, substituting C');
      PitchIndex := 1;
    end;
  end;

  result := Gamut[KeyIndex, PitchIndex]
end;


function GetAccidType(PitchName: TPitchName; Accid: TAccidental; 
  Key: TKeyKind): TAccidType; 
var
  AccidType: TAccidType;
begin
  AccidType := akExplicit;
  if Accid = AccidInKey(PitchName, Key) then 
    AccidType := akImplicit;

  DebugLn('ACCID TYPE:'); {$ifdef DEBUG}WriteLn(AccidType);{$endif}
  result := AccidType;
end;

function GetTie(Source: String): TMarkupPosition;
var
  Position: TMarkupPosition = mkNone;
begin
  if Source.Contains('~') then
    Position := mkStart;

  result := Position;
end;

function GetLinePosition(LineKind: TLineKind; Source: String): TMarkupPosition;
var
  Position: TMarkupPosition = mkNone;
  StartMark, EndMark: String;
begin
  case LineKind of
    lkSlur :
    begin
      StartMark := '(';
      EndMark   := ')';
    end;

    lkColoration :
    begin
      StartMark := '\color';
      EndMark   := '\endcolor';
    end;

    lkLigature :
    begin
      StartMark := '\[';
      EndMark   := '\]';
    end;
    { TODO deal with 'prefix' notation in Lilypond }
  end;
  if Source.Contains(StartMark) and Source.Contains(EndMark) then
    Position := mkEndStart
  else if Source.Contains(StartMark) then 
    Position := mkStart
  else if Source.Contains(EndMark) then
    Position := mkEnd
  else
    Position := mkNone;

  result := Position;
end;


{ TODO this does not enable us to deal with multiple layers of slurs }
function GetSlur(Source: String): TMarkupPosition;
begin
  result := GetLinePosition(lkSlur, Source);
end;

function GetColoration(Source: String): TMarkupPosition;
begin
  result := GetLinePosition(lkColoration, Source);
end;

function GetLigature(Source: String): TMarkupPosition;
begin
  result := GetLinePosition(lkLigature, Source);
end;

function GetArticulations(Source: String): TArticulationSpec;
var
  Spec: TArticulationSpec = (
    FFermata:   False; 
    FAccent:    False; 
    FStaccato:  False; 
    FTenuto:    False; 
    FStaccatissimo: False; 
    FMarcato:   False; 
  ); 
begin
  with Spec do
  begin
    FFermata  := Source.Contains('\fermata');
    FAccent   := Source.Contains('\accent')   or Source.Contains('->');
    FStaccato := Source.Contains('\staccato') or Source.Contains('-.');
    FTenuto   := Source.Contains('\tenuto')   or Source.Contains('--');
    FStaccatissimo := Source.Contains('\staccatissimo') or Source.Contains('-!');
    FMarcato := Source.Contains('\marcato')   or Source.Contains('-^');
  end;
  result := Spec;
end;

constructor TPitch.Create();
var
 Spec: TArticulationSpec = (
    FFermata:   False; 
    FAccent:    False; 
    FStaccato:  False; 
    FTenuto:    False; 
    FStaccatissimo: False; 
    FMarcato:   False; 
  ); 
begin
  inherited Create;
  FID := GenerateID;
  FArticulations := Spec;
end;

constructor TPitch.CreateFromLy(Source: String; Key: TKeyKind);
var
  NoteStr, PitchNameLy, OctLy, DurLy, EtcLy, Test: String;
begin
  Self.Create;
  NoteStr := Source;

  { Move ligatures to after note (where Lilypond docs admit they should be!) }
  if NoteStr.StartsWith('\[') then
    NoteStr := NoteStr.Substring(2) + '\[';

  PitchNameLy := ExtractWord(1, NoteStr, 
                  [',', '''', '1', '2', '4', '8', '\']);
  NoteStr := StringDropBefore(NoteStr, PitchNameLy);
  
  OctLy := '';
  Test := NoteStr.Substring(0, 1);
  case Test of
    '''', ',' :
    begin
      OctLy := ExtractWord(1, NoteStr, ['1', '2', '4', '8', '\']);
      NoteStr := StringDropBefore(NoteStr, OctLy);
    end;
  end;

  DurLy := '';
  Test := NoteStr.Substring(0, 1);
  case Test of
    '1', '2', '4', '8' :
    begin
      DurLy := ExtractWord(1, NoteStr, 
                ['(', ')', '~', '\', '[', ']', '*', '<', '-']);
      NoteStr := StringDropBefore(NoteStr, DurLy);
      EtcLy := NoteStr; 
    end;
  end;

  FPitchName := GetPitchName(PitchNameLy);
  FOct       := GetOctave(OctLy);
  FDur       := GetDurationKind(DurLy);
    
  FAccid     := akNatural;
  FAccidType := akImplicit;
  if Self.IsValid and not Self.IsRest then
  begin
    FAccid     := GetAccid(PitchNameLy);
    FAccidType := GetAccidType(FPitchName, FAccid, Key);
  end;

  FTie           := GetTie(EtcLy);
  FSlur          := GetSlur(EtcLy);
  FColoration    := GetColoration(EtcLy);
  FLigature      := GetLigature(EtcLy);
  FArticulations := GetArticulations(EtcLy); 

  FAnnotation := EtcLy; { TODO just holding surplus text in case we need it }
end;

procedure TPitch.Assign(Source: TPitch);
begin
  if Assigned(Source) then
  begin
    FID            := Source.FID;
    FPitchName     := Source.FPitchName;
    FAccid         := Source.FAccid;
    FAccidType     := Source.FAccidType;
    FOct           := Source.FOct;
    FDur           := Source.FDur;
    FTie           := Source.FTie;
    FSlur          := Source.FSlur;
    FColoration    := Source.FColoration;
    FLigature      := Source.FLigature;
    FArticulations := Source.FArticulations;
    FAnnotation    := Source.FAnnotation;
  end;
end;

function TPitch.IsValid: Boolean;
begin
  result := not ((FPitchName = pkNone) or (FOct = -1) or (FDur = dkNone));
end;

function TPitch.IsRest: Boolean;
begin
  result := FPitchName >= pkRest;
end;

function TMeiNoteRest.IsNote: Boolean;
begin
  result := GetName = 'note';
end;

function TMeiNoteRest.IsRest: Boolean;
begin
  result := (GetName = 'rest') or (GetName = 'mRest');
end;


procedure TMeiNoteRest.AddMeiPnameAttribute(Pitch: TPitch);
var
  Pname: String;
begin
  assert(IsNote);
  case Pitch.FPitchName of
    pkC : Pname := 'c';
    pkD : Pname := 'd';
    pkE : Pname := 'e';
    pkF : Pname := 'f';
    pkG : Pname := 'g';
    pkA : Pname := 'a';
    pkB : Pname := 'b';
  end;
  AddAttribute('pname', Pname);
end;

procedure TMeiNoteRest.AddMeiAccidAttribute(Pitch: TPitch);
var
  AccidSounded: String;
begin
  assert(IsNote);

  case Pitch.FAccid of
    akNatural : AccidSounded := 'n';
    akFlat    : AccidSounded := 'f';
    akSharp   : AccidSounded := 's';
  end;

  AddAttribute('accid.ges', AccidSounded);

  if Pitch.FAccidType = akExplicit then
    AddAttribute('accid', AccidSounded);
end;

procedure TMeiNoteRest.AddMeiOctAttribute(Pitch: TPitch);
begin
  assert(IsNote);
  AddAttribute('oct', IntToStr(Pitch.FOct));
end;

procedure TMeiNoteRest.AddMeiDurDotsAttributes(Pitch: TPitch);
var
  DurBase: String;
  Dots: Boolean;
begin
  assert(IsNote or IsRest);
  
  case Pitch.FDur of 
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

  case Pitch.FDur of
    dkBreve .. dkSemifusa         : Dots := False;
    dkBreveDotted .. dkFusaDotted : Dots := True;
  end;
  
  AddAttribute('dur', DurBase);

  if Dots then
    AddAttribute('dots', '1');
end;

procedure TMeiNoteRest.AddMeiTieAttribute(Pitch: TPitch);
var 
  Position: String;
begin
  assert(IsNote);

  if Pitch.FTie <> mkNone then
  begin
    case Pitch.FTie of
      mkStart    : Position := 'i';
      mkMiddle   : Position := 'm';
      mkEnd      : Position := 't';
    end;
    AddAttribute('tie', Position);
  end;
end;

procedure TMeiNoteRest.AddMeiArticAttribute(Pitch: TPitch);
var
  ArticKind: String = '';
  MeiArticNode: TMeiNode;
begin
  assert(IsNote);

  { Fermata is handled separately in TMeasureList.AddFermatas }
  with Pitch.FArticulations do
  begin
    if FAccent or FStaccato or FTenuto or FStaccatissimo or FMarcato then
    begin
      if FAccent then
        ArticKind := 'acc';
      if FStaccato then
        ArticKind := 'stacc';
      if FTenuto then
        ArticKind := 'ten';
      if FStaccatissimo then
        ArticKind := 'stacciss';
      if FMarcato then
        ArticKind := 'marc';
      
      MeiArticNode := TMeiNode.Create('artic');
      MeiArticNode.AddAttribute('artic', ArticKind);
      AppendChild(MeiArticNode);
    end;
  end;
end;

function TPitch.PitchEq(P2: TPitch): Boolean;
begin
  result := (FPitchName = P2.FPitchName) and (FAccid = P2.FAccid);
end;

constructor TMeiNoteRest.CreateFromPitch(Pitch: TPitch);
begin
  inherited Create();

  case Pitch.FPitchName of
    pkRest        : SetName('rest');
    pkMeasureRest : SetName('mRest');
    else
    begin
      SetName('note');
      AddMeiPnameAttribute(Pitch);
      AddMeiAccidAttribute(Pitch);
      AddMeiOctAttribute(Pitch);
      AddMeiTieAttribute(Pitch);
      AddMeiArticAttribute(Pitch);
    end;
  end;

  AddMeiDurDotsAttributes(Pitch);
end;

function ReplaceLyCommands(Source: String): String;
var 
  Translation: Array[0..3, 0..1] of String = ( 
    ('\break', ''),  { ignore breaks }
    ('\[ ', '\['),   { group start ligature with next note }
    (' \]', '\]'),   { group end ligature with previous note }
    ('\colorOne', '') { TODO for now; not supported by MEI? }
  );
  PairIndex: Integer;
begin
  for PairIndex := 0 to Length(Translation) - 1 do
  begin
    Source := Source.Replace(Translation[PairIndex][0], 
                Translation[PairIndex][1]);
  end;
  result := Source;
end;

constructor TPitchList.CreateFromLy(Source: String; Key: TKeyKind);
var
  ThisNote: String;
  Notes: TStringArray;
  NewPitch: TPitch;
begin
  inherited Create;
  DebugLn('Trying to make new pitch list from string: ' + Source);

  { TODO replace full command strings }
  Source := ReplaceLyCommands(Source);

  Notes := Source.Split([' ']);
  for ThisNote in Notes do
  begin
    NewPitch := TPitch.CreateFromLy(ThisNote, Key);
    if NewPitch.IsValid then
      Self.Add(NewPitch)
    else
    begin
      DebugLn('Invalid Pitch found in source ''' + ThisNote + '''');
      FreeAndNil(NewPitch);
    end;
  end;
end;

procedure TPitchList.Assign(Source: TPitchList);
var
  ThisPitch, NewPitch: TPitch;
begin
  if Assigned(Source) then
  begin
    for ThisPitch in Source do
    begin
      NewPitch := TPitch.Create;
      NewPitch.Assign(ThisPitch);
      Self.Add(NewPitch);
    end;
    FLines := Source.FLines;
    FFermata := Source.FFermata;
    FBarlineRight := Source.FBarlineRight;
  end;
end;

procedure TPitchList.SetBarlineRight(Source: String);
begin
  FBarlineRight := bkNormal;
  if Source.Contains('\FinalBar') or Source.Contains('\bar "|."') then
    FBarlineRight := bkFinal
  else if Source.Contains('\MiddleBar') or Source.Contains('\bar "||"') then
    FBarlineRight := bkMiddle
  else if Source.Contains('\RepeatBarEnd') or Source.Contains('\bar ":|."') then
    FBarlineRight := bkRepeatEnd;
  DebugLn('Set BARLINE to :');
  {$ifdef DEBUG}WriteLn(FBarlineRight);{$endif}
end;

function AddMeiBarlineAttr(MeiMeasure: TMeiNode; PitchList: TPitchList):
  TMeiNode; 
var 
  Attr: String;
begin
  assert(Assigned(PitchList));
  assert(Assigned(MeiMeasure) and (MeiMeasure.GetName = 'measure'));

  case PitchList.FBarlineRight of
    bkNormal    : Attr := '';
    bkMiddle    : Attr := 'dbl';
    bkFinal     : Attr := 'end';
    bkRepeatEnd : Attr := 'rptend';
  end;

  if not Attr.IsEmpty then 
    MeiMeasure.AddAttribute('right', Attr);
 
  DebugLn('Made MEI Barline type: ' + Attr);
  result := MeiMeasure;
end;

function TPitchList.ToMEI: TMeiNode;
var
  ThisPitch: TPitch;
  ThisMeiNote: TMeiNoteRest;
  MeiTree: TMeiNode;
begin
  MeiTree := TMeiNode.Create('lirio:measure');
  for ThisPitch in Self do
  begin
    ThisMeiNote := TMeiNoteRest.CreateFromPitch(ThisPitch);
    MeiTree.AppendChild(ThisMeiNote);
  end;

  MeiTree := AddMeiBarlineAttr(MeiTree, Self);
  
  result := MeiTree;
end;

function CreateMeiSectionHead(Source: String): TMeiNode;
var
  HeadingText: String;
  MeiTempo: TMeiNode = nil;
begin
  HeadingText := CopyStringBetween(Source, '\Section "', '"');

  if not HeadingText.IsEmpty then
  begin
    MeiTempo := TMeiNode.Create('tempo');

    MeiTempo.AddAttribute('place', 'above');
    MeiTempo.AddAttribute('staff', '1');
    MeiTempo.AddAttribute('tstamp', '1');

    MeiTempo.SetTextNode(HeadingText);
  end;

  result := MeiTempo;
end;

{ TODO START }
procedure TMeasureList.SetFromLy(Source: String);
var
  Key: TKeyKind;
  LyLines: TStringListAAC;
  SearchStr, ThisLine, TestLine, MeasureStr: String;
begin
  { Find the key signature for this voice }
  SearchStr := Source.Substring(0, 800); 
  Key := FindLyKey(SearchStr);

  { Find measures and parse the notes in them }
  LyLines := TStringListAAC.Create(Source);
  for ThisLine in LyLines do
  begin
    TestLine := ThisLine.TrimLeft;
    if TestLine.StartsWith('\Section ') then
    begin
      DebugLn('Section heading found: ' + ThisLine);
      FPrefix := CreateMeiSectionHead(ThisLine);
    end
    
    else if TestLine.StartsWith('\bar') or TestLine.Contains('Bar') then
    begin
      DebugLn('Barline found: ' + ThisLine);
      Self.Last.SetBarlineRight(ThisLine);
    end

    else if TestLine.StartsWith('|') then
    begin
      MeasureStr := StringDropBefore(ThisLine, '| ');

      DebugLn('Adding new TPitchList to TMeasureList...');
      Self.Add(TPitchList.CreateFromLy(MeasureStr, Key));
    end;
  end;
  FreeAndNil(LyLines);
end;

procedure TMeasureList.Assign(Source: TMeasureList);
var
  ThisMeasure, NewPitchList: TPitchList;
begin
  if Assigned(Source) then
  begin
    for ThisMeasure in Source do
    begin
      NewPitchList := TPitchList.Create;
      NewPitchList.Assign(ThisMeasure);
      Self.Add(NewPitchList);
    end;
    
    FPrefix.Assign(Source.FPrefix);
  end;
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
    ekXML:      Name := 'XML';
  else Name := 'UNKNOWN';
  end;
  result := Name;
end;

procedure TMeasureList.AddFermatas;
var
  ThisMeasure: TPitchList;
  ThisPitch: TPitch;
  PitchIndex: Integer;
begin
  for ThisMeasure in Self do
  begin
    PitchIndex := 0;
    for PitchIndex := 0 to ThisMeasure.Count - 1 do
    begin
      ThisPitch := ThisMeasure[PitchIndex];
      if ThisPitch.FArticulations.FFermata then
      begin
        ThisMeasure.FFermata:= ThisMeasure.FFermata + XMLElement('fermata',
          XMLAttribute('startid', ThisPitch.FID)); 
      end;
    end;
  end;
end;


procedure TMeasureList.AddTies;
var
  ThisMeasure: TPitchList;
  ThisPitch, TiedPitch: TPitch;
  PitchIndex: Integer;
  FoundTie: Boolean = False;
begin
  for ThisMeasure in Self do
  begin
    PitchIndex := 0;
    while PitchIndex < ThisMeasure.Count do
    begin
      ThisPitch := ThisMeasure[PitchIndex];

      if ThisPitch.FTie = mkStart then
      begin
        TiedPitch := ThisPitch; 
        FoundTie := True;
      end
      else if FoundTie then
      begin
        if ThisPitch.PitchEq(TiedPitch) then
          ThisPitch.FTie := mkMiddle
        else
        begin
          ThisMeasure[PitchIndex - 1].FTie := mkEnd;
          FoundTie := False;
          Dec(PitchIndex);
        end;
      end;
      Inc(PitchIndex);
    end;
  end;
end;


procedure TMeasureList.AddLines(LineKind: TLineKind);
var 
  ThisMeasure: TPitchList;
  ThisPitch: TPitch;
  FoundStart, FoundEnd, FoundEndStart: Boolean;
  StartID, EndID, NextStartID: String;
  MeasureNum: Integer;
  LinePosition: TMarkupPosition;
  ElementName, LocationAttr, FunctionAttr, MEI: String;
begin
  DebugLn('Looking for line type:'); {$ifdef DEBUG}WriteLn(LineKind);{$endif}
  FoundStart    := False;
  FoundEnd      := False;
  FoundEndStart := False;

  for MeasureNum := 0 to Self.Count - 1 do
  begin
    ThisMeasure := Self[MeasureNum];
    
    for ThisPitch in ThisMeasure do 
    begin
      case LineKind of
        lkSlur : 
        begin
          LinePosition := ThisPitch.FSlur;
          ElementName := 'slur';
          FunctionAttr := '';
        end;

        lkColoration : 
        begin
          LinePosition := ThisPitch.FColoration;
          ElementName := 'bracketSpan';
          FunctionAttr := ' ' + XMLAttribute('func', 'coloration');
        end;

        lkLigature : 
        begin
          LinePosition := ThisPitch.FLigature;
          ElementName := 'bracketSpan';
          FunctionAttr := Format(' %s %s', [XMLAttribute('func', 'ligature'),
                                           XMLAttribute('lform', 'solid')]);
        end;
      end;

      case LinePosition of
        mkStart :
        begin
          if not FoundStart then
          begin
            FoundStart := True;
            StartID := ThisPitch.FID;
            DebugLn('Found line start on ID ' + StartID);
          end;
        end;

        mkEnd :
        begin
          if FoundStart then
          begin
            FoundEnd := True;
            EndID := ThisPitch.FID;
            FoundStart := False;
            DebugLn('Found line end on ID ' + EndID);
          end;
        end;

        mkEndStart :
        begin
          if FoundStart then
          begin
            FoundEndStart := True;
            FoundEnd := True;
            EndID := ThisPitch.FID;
          end;
          
          FoundStart := True;
          NextStartID := ThisPitch.FID;
          DebugLn('Found line end and start on ID ' + EndID);
        end;
      end;

      if FoundEnd then
      begin
        DebugLn('FOUND A LINE');
        with ThisMeasure do
        begin
          { TODO use an object instead of XML string? }
          LocationAttr := Format('%s %s', [XMLAttribute('startid', '#' + StartID), 
                                           XMLAttribute('endid', '#' + EndID)]);
         
          MEI := XMLElement(ElementName, LocationAttr + FunctionAttr);
          FLines := Flines + MEI;
        end;
        FoundEnd := False;
      end;

      if FoundEndStart then
        StartID := NextStartID;
    end;
  end;
end;

constructor TLirioVoice.Create();
begin
  inherited Create('lirio:voice');
  FMeasureList := TMeasureList.Create();
end;

constructor TLirioVoice.Create(LySource: String);
begin
  Create();
  FMeasureList.SetFromLy(LySource);
end;

destructor TLirioVoice.Destroy();
begin
  FMeasureList.Destroy();
  inherited Destroy;
end;

function TLirioVoice.GetMeasure(Index: Integer): TPitchList;
begin
  Assert(Index < FMeasureList.Count);
  result := FMeasureList.Items[Index];
end;

{ TODO use this function to parse the contents of a Lilypond music expression
into measures and notes
Need to think through data structures for conversion: previously we created a list of measure types, which was a list of pitch types, and then worked with those; are we ready at this stage to convert these to TMeiNodes? }
function LyToMeasures(Tree: TMeiNode): TMeiNode;
var
  LyText: String;
  Voice: TLirioVoice;
begin
  Assert(Tree.GetName = 'layer');
  if Assigned(Tree) then
  begin
    LyText := Tree.GetText;
    Tree.SetTextNode('');

    Voice := TLirioVoice.Create(LyText);
    Tree.AppendChild(Voice);
  end;
  result := Tree;
end;


function ParseLyMusic(Tree: TMeiNode): TMeiNode;
var
  Child: TMeiNode = nil;
  Sibling: TMeiNode = nil;
begin
  if Tree.GetName = 'layer' then
  begin
    Tree := LyToMeasures(Tree);
  end;

  with Tree do
  begin
    Child := ChildTree;
    if Assigned(Child) then
    begin
      Child := ParseLyMusic(Child); 
    end;

    Sibling := NextSibling;
    if Assigned(Sibling) then
    begin
      Sibling := ParseLyMusic(Sibling);
    end;
  end;
  
  result := Tree;
end;

{
TODO need to do something like copy the tree (either TLyObject or TMeiNode) but expand the text fields into measure lists. e.g., copy the tree into a tree of TLirioVoice nodes that include TMeasureList members.

constructor TLirioVoice(MeiTree: TMeiNode);
var
  NextVoice: TLirioVoice;
begin
  if Assigned(MeiTree) then
  begin
    if MeiTree.GetName = 'layer' then
    begin
      NextVoice := TLirioVoice.Create(MeiTree.GetText);
    end
    else
    begin
      NextVoice := TLirioVoice.Create();
      NextVoice.Assign(MeiTree);
    end;
    FChild := NextVoice;
    if Assigned(MeiTree.ChildTree) then
    begin


  end;
end;
}

end.
