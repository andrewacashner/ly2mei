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

uses SysUtils, StrUtils, Classes, Generics.Collections, StringTools, MEI;

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

  { Labels for keys or key signatures. }
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
  { Label for position of a note in a markup such a tie or slur }
  TMarkupPosition = (
    mkNone,
    mkStart,
    mkMiddle,
    mkEnd,
    mkEndStart {< end one and start another on same note }
  );

  { Label for types of lines extending from one note to another }
  TLineKind = (lkNone, lkTie, lkSlur, lkColoration, lkLigature);

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

    { When we find invalid input, we construct invalid pitches, with the
      fields set to "none" or negative values. Did we find a valid pitch? }
    function IsValid: Boolean;

    { A rest is a @code(TPitch) with the pitch name set to @code(pkRest) and
      only the duration. }
    function IsRest: Boolean;

    { Is the pitch class and accidental the same as another? (Ignoring
      duration and other fields) }
    function PitchEq(P2: TPitch): Boolean;

    property ID:         String      read FID;
    property Name:       TPitchName  read FPitchName;
    property Accid:      TAccidental read FAccid;
    property AccidType:  TAccidType  read FAccidType;
    property Oct:        Integer     read FOct;
    property Dur:        TDuration   read FDur;

    property Tie:        TMarkupPosition read FTie write FTie;
    property Slur:       TMarkupPosition read FSlur;
    property Coloration: TMarkupPosition read FColoration;
    property Ligature:   TMarkupPosition read FLigature;

    property HasFermata:       Boolean read FArticulations.FFermata;
    property HasAccent:        Boolean read FArticulations.FAccent;
    property HasStaccato:      Boolean read FArticulations.FStaccato;
    property HasTenuto:        Boolean read FArticulations.FTenuto;
    property HasStaccatissimo: Boolean read FArticulations.FStaccatissimo;
    property HasMarcato:       Boolean read FArticulations.FMarcato;
  end;

  TFermata = class
  private
    var
      FStartID: String;
  public
    constructor Create(ID: String);
    function ToMEI: TMeiNode;
  end;

  TFermataList = class(specialize TObjectList<TFermata>)
  public
    function ToMEI: TMeiNode;
  end;


  TLine = class
  private
    var
      FName, FStartID, FEndID, FLineFunction, FLineForm: String;
  public
    constructor Create(); 
    constructor Create(Name: String;
      StartID: String = ''; 
      EndID: String = '';
      LineFunction: String = '';
      LineForm: String = '');
    procedure Assign(OrigLine: TLine);
    function ToMEI: TMeiNode;

    property Name:          String read FName;
    property StartID:       String read FStartID;
    property EndID:         String read FEndID;
    property LineFunction:  String read FLineFunction;
    property LineForm:      String read FLineForm;
  end;

  TLineList = class(specialize TObjectList<TLine>)
  public
    function ToMEI: TMeiNode;
  end;


  { @abstract(A list of @link(TPitch) objects, corresponding to one measure of
     music.) }
  TPitchList = class(specialize TObjectList<TPitch>)
  private
    var
      FBarlineRight: TBarline;
      FFermataList: TFermataList;
      FLineList: TLineList;
  public
    constructor Create(); 

    { Create a new list of pitches from the Lilypond input string for a single
      measure of music, and the key relevant to this music. Recursively create
      all the pitches contained in the list. }
    constructor CreateFromLy(Source: String; Key: TKeyKind);

    destructor Destroy(); override;

    procedure SetBarlineRight(Source: String);

    { Generate an MEI @code(measure) element, recursively generating the
      @code(note) elements it contains. }
    function ToMEI: TMeiNode;

    property BarlineRight:  TBarline      read FBarlineRight;
    property FermataList:   TFermataList  read FFermataList;
    property LineList:      TLineList     read FLineList;
  end;


  { @abstract(A list of @link(TPitchList) objects for a single voice/layer.)

    This list contains a list of all the measures of music for a single voice
    (Lilypond) or layer (MEI). }
  TMeasureList = class(specialize TObjectList<TPitchList>)
  private
    var
      FHeaderText: String; { section heading text }
  public
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

    procedure AddAllLines;

    { Go through measure list and add fermata elements within the MEI
      @code(measure) linked to their notes by the @code(startid). }
    procedure AddFermatas;
    
    function AddMeiSectionHead(MeiMeasure: TMeiNode): TMeiNode;
    
    function ToMEI: TMeiNode;
  end;

  TLinePosition = class
  public
    var
      FStartID, FEndID: String;
  public
    property StartID: String read FStartID write FStartID;
    property EndID:   String read FEndID   write FEndID;
  end;

  TLinePositionList = class(specialize TObjectList<TLinePosition>)
  public
    constructor Create(MeasureList: TMeasureList; LineKind: TLineKind);
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

    { Generate one or more MEI @code(artic) elements within a @code(note). }
    procedure AddMeiArticulation(Pitch: TPitch);

  public
    constructor CreateFromPitch(Pitch: TPitch);
  end;

{ TODO make this a class function of TMeiNode or descendant }
function AddMeiBarlineAttr(MeiMeasure: TMeiNode; PitchList: TPitchList):
  TMeiNode;


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
  result := AccidType;
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
  result := Key;
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
  FID := GenerateXmlID;
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
  case Pitch.Name of
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

  case Pitch.Accid of
    akNatural : AccidSounded := 'n';
    akFlat    : AccidSounded := 'f';
    akSharp   : AccidSounded := 's';
  end;

  AddAttribute('accid.ges', AccidSounded);

  if Pitch.AccidType = akExplicit then
    AddAttribute('accid', AccidSounded);
end;

procedure TMeiNoteRest.AddMeiOctAttribute(Pitch: TPitch);
begin
  assert(IsNote);
  AddAttribute('oct', IntToStr(Pitch.Oct));
end;

procedure TMeiNoteRest.AddMeiDurDotsAttributes(Pitch: TPitch);
var
  DurBase: String;
  Dots: Boolean;
begin
  assert(IsNote or IsRest);
  
  case Pitch.Dur of 
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

  case Pitch.Dur of
    dkBreve .. dkSemifusa         : Dots := False;
    dkBreveDotted .. dkFusaDotted : Dots := True;
  end;
  
  AddAttribute('dur', DurBase);

  if Dots then
    AddAttribute('dots', '1');
end;

procedure TMeiNoteRest.AddMeiArticulation(Pitch: TPitch);
  procedure AddArticNode(Value: String);
  var
    Artic: TMeiNode;
  begin
    Artic := TMeiNode.Create('artic');
    Artic.AddAttribute('artic', Value);
    AppendChild(Artic);
  end;

begin
  assert(IsNote);
  { Fermata is handled separately in TMeasureList.AddFermatas }
  with Pitch do
  begin
    { Add as many articulation attributes as needed to the artic element }
    if HasAccent then
      AddArticNode('acc');
    if HasStaccato then
      AddArticNode('stacc');
    if HasTenuto then
      AddArticNode('ten');
    if HasStaccatissimo then
      AddArticNode('stacciss');
    if HasMarcato then
      AddArticNode('marc');
  end;
end;

function TPitch.PitchEq(P2: TPitch): Boolean;
begin
  result := (Name = P2.Name) and (Accid = P2.Accid);
end;

constructor TMeiNoteRest.CreateFromPitch(Pitch: TPitch);
begin
  inherited Create();

  case Pitch.Name of
    pkRest        : SetName('rest');
    pkMeasureRest : SetName('mRest');
    else
    begin
      SetName('note');
      AddAttribute('xml:id', Pitch.ID);
      AddMeiPnameAttribute(Pitch);
      AddMeiAccidAttribute(Pitch);
      AddMeiOctAttribute(Pitch);
      AddMeiArticulation(Pitch);
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

constructor TFermata.Create(ID: String);
begin
  inherited Create;
  FStartID := ID;
end;

function TFermata.ToMEI: TMeiNode;
var
  FermataNode: TMeiNode = nil;
begin
  if not FStartID.IsEmpty then
  begin
    FermataNode := TMeiNode.Create('fermata');
    FermataNode.AddAttribute('startid', FStartID);
  end;
  result := FermataNode;
end;

function TFermataList.ToMEI: TMeiNode;
var
  FermataListNode: TMeiNode = nil;
  NewFermataNode: TMeiNode;
  ThisFermata: TFermata;
begin
  if Self.Count > 0 then
  begin
    for ThisFermata in Self do
    begin
      NewFermataNode := ThisFermata.ToMEI;
      if Assigned(FermataListNode) then
        FermataListNode.AppendSibling(NewFermataNode)
      else
        FermataListNode := NewFermataNode;
    end;
  end;
  result := FermataListNode;
end;

constructor TLine.Create();
begin
  inherited Create();
end;

constructor TLine.Create(Name: String;
  StartID: String = ''; 
  EndID: String = '';
  LineFunction: String = '';
  LineForm: String = '');
begin
  inherited Create();
  FName := Name;
  FStartID := StartID;
  FEndID := EndID;
  FLineFunction := LineFunction;
  FLineForm := LineForm;
end;

procedure TLine.Assign(OrigLine: TLine);
begin
  FName         := OrigLine.Name;
  FStartID      := OrigLine.StartID;
  FEndID        := OrigLine.EndID;
  FLineFunction := OrigLine.LineFunction;
  FLineForm     := OrigLine.LineForm;
end;


function TLine.ToMEI: TMeiNode;
var
  MeiLine: TMeiNode;
begin
  MeiLine := TMeiNode.Create(FName);
  MeiLine.AddAttribute('startid', FStartID);
  MeiLine.AddAttribute('endid', FEndID);

  if not FLineFunction.IsEmpty then
  begin
    MeiLine.AddAttribute('func', FLineFunction);
  end;

  if not FLineForm.IsEmpty then
  begin
    MeiLine.AddAttribute('lform', FLineForm);
  end;
  
  result := MeiLine;
end;

{ makes a chain of sibling elements (no parent element enclosing them all) }
function TLineList.ToMEI: TMeiNode;
var
  Root: TMeiNode = nil;
  ThisLine: TLine;
begin
  for ThisLine in Self do
  begin
    if not Assigned(Root) then
      Root := ThisLine.ToMEI
    else
      Root.AppendSibling(ThisLine.ToMEI);
  end;
  result := Root;
end;

constructor TPitchList.Create();
begin
  FFermataList := TFermataList.Create();
  FLineList := TLineList.Create();
  inherited Create;
end;

constructor TPitchList.CreateFromLy(Source: String; Key: TKeyKind);
var
  ThisNote: String;
  Notes: TStringArray;
  NewPitch: TPitch;
begin
  Create;
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
      FreeAndNil(NewPitch);
    end;
  end;
end;

destructor TPitchList.Destroy();
begin
  FFermataList.Destroy;
  FLineList.Destroy;
  inherited Destroy;
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
end;

function AddMeiBarlineAttr(MeiMeasure: TMeiNode; PitchList: TPitchList):
  TMeiNode; 
var 
  Attr: String;
begin
  Assert(Assigned(PitchList));
  Assert(Assigned(MeiMeasure));
  Assert((MeiMeasure.GetName = 'lirio:measure') 
    or (MeiMeasure.GetName = 'measure'));

  case PitchList.BarlineRight of
    bkNormal    : Attr := '';
    bkMiddle    : Attr := 'dbl';
    bkFinal     : Attr := 'end';
    bkRepeatEnd : Attr := 'rptend';
  end;

  if not Attr.IsEmpty then 
    MeiMeasure.AddAttribute('right', Attr);
 
  result := MeiMeasure;
end;

function TPitchList.ToMEI: TMeiNode;
var
  ThisPitch: TPitch;
  ThisMeiNote: TMeiNoteRest;
  MeiTree: TMeiNode = nil;
begin
  for ThisPitch in Self do
  begin
    ThisMeiNote := TMeiNoteRest.CreateFromPitch(ThisPitch);
    
    if not Assigned(MeiTree) then
      MeiTree := ThisMeiNote
    else
      MeiTree.AppendSibling(ThisMeiNote);
  end;
  
  result := MeiTree;
end;

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
      FHeaderText := CopyStringBetween(ThisLine, '\Section "', '"');
    end
    
    else if TestLine.StartsWith('\bar') or TestLine.Contains('Bar') then
    begin
      Self.Last.SetBarlineRight(ThisLine);
    end

    else if TestLine.StartsWith('|') then
    begin
      MeasureStr := StringDropBefore(ThisLine, '| ');

      Self.Add(TPitchList.CreateFromLy(MeasureStr, Key));
    end;
  end;

  Self.AddTies;
  Self.AddFermatas;
  Self.AddAllLines;

  FreeAndNil(LyLines);
end;

procedure TMeasureList.AddFermatas;
var
  ThisMeasure: TPitchList;
  ThisPitch: TPitch;
  NewFermata: TFermata;
begin
  for ThisMeasure in Self do
  begin
    for ThisPitch in ThisMeasure do 
    begin
      if ThisPitch.HasFermata then
      begin
        NewFermata := TFermata.Create(ThisPitch.ID);
        ThisMeasure.FermataList.Add(NewFermata);
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
      ThisPitch := ThisMeasure.Items[PitchIndex];

      if ThisPitch.Tie = mkStart then
      begin
        TiedPitch := ThisPitch; 
        FoundTie := True;
      end
      else if FoundTie then
      begin
        if ThisPitch.PitchEq(TiedPitch) then
          ThisPitch.Tie := mkEndStart
        else
        begin
          ThisMeasure.Items[PitchIndex - 1].Tie := mkEnd;
          FoundTie := False;
          Dec(PitchIndex);
        end;
      end;
      Inc(PitchIndex);
    end;
  end;
end;

constructor TLinePositionList.Create(MeasureList: TMeasureList; 
  LineKind: TLineKind);

  function SelectLineField(Pitch: TPitch; LineKind: TLineKind): TMarkupPosition;
  var 
    LineField: TMarkupPosition;
  begin
    case LineKind of
      lkTie        : LineField := Pitch.Tie;
      lkSlur       : LineField := Pitch.Slur;
      lkColoration : LineField := Pitch.Coloration;
      lkLigature   : LineField := Pitch.Ligature;
    end;
    result := LineField;
  end;

  function CreateStartPosition(ID: String): TLinePosition;
  var
    Position: TLinePosition;
  begin
    Position := TLinePosition.Create();
    Position.StartID := ID;
    result := Position;
  end;

  function AddCompletePosition(PositionList: TLinePositionList; 
    ThisPosition: TLinePosition; ID: String): TLinePositionList;
  begin
    ThisPosition.EndID := ID;
    PositionList.Add(ThisPosition);
    result := PositionList;
  end;

var
  ThisMeasure: TPitchList;
  ThisPitch: TPitch;
  LineField: TMarkupPosition;
  ThisPitchID: String;
  NewLinePosition: TLinePosition;
begin
  Assert(Assigned(MeasureList));
  inherited Create();

  for ThisMeasure in MeasureList do
  begin
    for ThisPitch in ThisMeasure do
    begin
      LineField := SelectLineField(ThisPitch, LineKind);
      ThisPitchID := ThisPitch.ID;
      case LineField of
        mkStart : NewLinePosition := CreateStartPosition(ThisPitchID);
        mkEnd : AddCompletePosition(Self, NewLinePosition, ThisPitchID);
        mkEndStart :
        begin
          AddCompletePosition(Self, NewLinePosition, ThisPitchID);
          NewLinePosition := CreateStartPosition(ThisPitchID);
          { TODO what if NewLinePosition is incomplete: we have a start but no
          end? or an end but no start? or overlapping, nested? }
        end;
      end;
    end;
  end;
end;


procedure TMeasureList.AddLines(LineKind: TLineKind);

  function GetLineName(LineKind: TLineKind): String;
  var
    LineName: String = '';
  begin
    case LineKind of
      lkTie         : LineName := 'tie';
      lkSlur        : LineName := 'slur';
      lkColoration, 
        lkLigature  : LineName := 'bracketSpan';
    end;
    result := LineName;
  end;

  function GetLineFunction(LineKind: TLineKind): String;
  var
    LineFunction: String = '';
  begin
    case LineKind of
      lkColoration : LineFunction := 'coloration';
      lkLigature   : LineFunction := 'ligature';
    end;
    result := LineFunction;
  end;

  function GetLineForm(LineKind: TLineKind): String;
  var
    LineForm: String = '';
  begin
    case LineKind of
      lkLigature : LineForm := 'solid';
    end;
    result := LineForm;
  end;

var
  LinePositionList: TLinePositionList;
  ThisLinePosition: TLinePosition;
  NewLine, ThisLine: TLine;
  LineList: TLineList;
  ThisMeasure: TPitchList;
  ThisPitch: TPitch;
begin
  if LineKind <> lkNone then
  begin
    LineList := TLineList.Create();
    LinePositionList := TLinePositionList.Create(Self, LineKind);

    for ThisLinePosition in LinePositionList do
    begin
      NewLine := TLine.Create(GetLineName(LineKind), 
        ThisLinePosition.StartID,
        ThisLinePosition.EndID,
        GetLineFunction(LineKind),
        GetLineForm(LineKind));

      LineList.Add(NewLine);
    end;

    for ThisLine in LineList do
    begin
      for ThisMeasure in Self do
      begin
        for ThisPitch in ThisMeasure do
        begin
          if ThisPitch.ID = ThisLine.StartID then
          begin
            NewLine := TLine.Create();
            NewLine.Assign(ThisLine);
            ThisMeasure.LineList.Add(NewLine);
          end;
        end;
      end;
    end;

    FreeAndNil(LineList);
    FreeAndNil(LinePositionList);
  end;
end;

procedure TMeasureList.AddAllLines;
begin
  AddLines(lkTie);
  AddLines(lkSlur);
  AddLines(lkColoration);
  AddLines(lkLigature);
end;

function TMeasureList.AddMeiSectionHead(MeiMeasure: TMeiNode): TMeiNode;
var
  SectionHead: TMeiNode = nil;
begin
  Assert(Assigned(MeiMeasure));
  Assert((MeiMeasure.GetName = 'lirio:voice') 
    or (MeiMeasure.GetName = 'measure'));

  if not FHeaderText.IsEmpty then
  begin
    SectionHead := TMeiNode.Create('tempo');
    SectionHead.AddAttribute('place', 'above');
    SectionHead.AddAttribute('staff', '1');
    SectionHead.AddAttribute('tstamp', '1');
    SectionHead.SetTextNode(FHeaderText);
    MeiMeasure.AppendChild(SectionHead);
  end;
  result := MeiMeasure;
end;

function TMeasureList.ToMEI: TMeiNode;
var
  MeiRoot, MeiMeasure, MeiLines: TMeiNode;
  ThisMeasure: TPitchList;
  MeasureNum: Integer;
begin
  MeiRoot := TMeiNode.Create('lirio:voice');
  MeiRoot := AddMeiSectionHead(MeiRoot);

  MeasureNum := 1;
  for ThisMeasure in Self do
  begin
    MeiMeasure := ThisMeasure.ToMEI;
    MeiMeasure.AddAttribute('n', IntToStr(MeasureNum));
    
    MeiLines := ThisMeasure.LineList.ToMEI;
    MeiMeasure.AppendChild(MeiLines);

    MeiRoot.AppendChild(MeiMeasure);
    Inc(MeasureNum);
  end;
  result := MeiRoot;
end;

end.
