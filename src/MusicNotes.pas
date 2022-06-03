{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(Convert a Lilypond music expression to MEI.)
  @author(Andrew Cashner)

  We parse a tree of Lilypond objects (@code(TLyObject), from the ScoreTree
  unit) into an internal representation of the music data. We then convert that
  representation from Lilypond's hierarchy of data into that needed for MEI.

  Lilypond (in our specification) is organized score/staff/voice/measures/notes.
  MEI requires score/section/measure/staff/layer/notes.
}
unit MusicNotes;

interface

uses SysUtils, StrUtils, Classes, Generics.Collections, StringTools, Outline,
  MEI;

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

  { Types of bar lines }
  TBarline = (bkNormal, bkMiddle, bkFinal, bkRepeatEnd, bkRepeatStart);

  { @abstract(Records which articulations a pitch has (values correspond to
    MEI @code(data.ARTICULATION)).) }
  TArticulationSpec = record
    FFermata, FAccent, FStaccato, FTenuto, FStaccatissimo, FMarcato: Boolean;
  end; 

  TSyllablePosition = (skSingle, skBeginning, skMiddle, skEnd, skExtend,
    skEndExtend, skSingleExtend);

  TSyllable = class 
  private
    var
      FText: String;
      FPosition: TSyllablePosition;
  public
    constructor Create();
    constructor Create(SylText: String; SylPosition: TSyllablePosition =
      skSingle);
    procedure Assign(OtherSyl: TSyllable);
    
    function ToMEI: TMeiNode;

    property SylText:     String            read FText      write FText;
    property SylPosition: TSyllablePosition read FPosition  write FPosition;
  end;

  TSyllableList = class(specialize TObjectList<TSyllable>)
  public
    constructor Create();
    constructor Create(LyInput: String);
    function ToMEI: TMeiNode;
  end;

  
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

      { One syllable of text to be sung to this note, with indication of
      syllable position }
      FSyllable: TSyllable;

  public
    constructor Create(); 

    { Create from a Lilypond input string; set the accidental relative to the
      given key. }
    constructor Create(LyInput: String; Key: TKeyKind);

    destructor Destroy(); override;

    { When we find invalid input, we construct invalid pitches, with the
      fields set to "none" or negative values. Did we find a valid pitch? }
    function IsValid: Boolean;

    { A rest is a @code(TPitch) with the pitch name set to @code(pkRest) and
      only the duration. }
    function IsRest: Boolean;

    function CanHaveSyllable: Boolean;

    { Is the pitch class and accidental the same as another? (Ignoring
      duration and other fields) }
    function PitchEq(P2: TPitch): Boolean;

    property ID:         String           read FID;
    property Name:       TPitchName       read FPitchName;
    property Accid:      TAccidental      read FAccid;
    property AccidType:  TAccidType       read FAccidType;
    property Oct:        Integer          read FOct;
    property Dur:        TDuration        read FDur;

    property Tie:        TMarkupPosition  read FTie         write FTie;
    property Slur:       TMarkupPosition  read FSlur        write FSlur;
    property Coloration: TMarkupPosition  read FColoration;
    property Ligature:   TMarkupPosition  read FLigature;
    property Syllable:   TSyllable        read FSyllable    write FSyllable;

    property HasFermata:       Boolean    read FArticulations.FFermata;
    property HasAccent:        Boolean    read FArticulations.FAccent;
    property HasStaccato:      Boolean    read FArticulations.FStaccato;
    property HasTenuto:        Boolean    read FArticulations.FTenuto;
    property HasStaccatissimo: Boolean    read FArticulations.FStaccatissimo;
    property HasMarcato:       Boolean    read FArticulations.FMarcato;
  end;

  { @abstract(Internal representation of MEI @code(fermata) element.)
    Records ID of note the fermata is attached to.) }
  TFermata = class
  private
    var
      FStartID: String;
  public
    constructor Create(ID: String);

    { Create an MEI @code(fermata) element. }
    function ToMEI: TMeiNode;
  end;

  { @abstract(List of fermatas, used to gather all the fermatas needed in a
    measure.) }
  TFermataList = class(specialize TObjectList<TFermata>)
  public

    { Create a tree of MEI @code(fermata) elements as siblings. }
    function ToMEI: TMeiNode;
  end;


  { @abstract(Internal representation of an MEI line, for ties, slurs,
    coloration brackets, and ligatures.) }
  TLine = class
  private
    var
      FName, FStartID, FEndID, FLineFunction, FLineForm: String;
  public
    constructor Create(); 

    constructor Create(Name: String; StartID: String = ''; EndID: String = '';
      LineFunction: String = ''; LineForm: String = ''); 

    procedure Assign(OrigLine: TLine);

    { Create an MEI line element: a @code(tie), @code(slur), or
    @code(bracketSpan). }
    function ToMEI: TMeiNode;

    property Name:          String read FName;
    property StartID:       String read FStartID;
    property EndID:         String read FEndID;
    property LineFunction:  String read FLineFunction;
    property LineForm:      String read FLineForm;
  end;

  { @abstract(A list of lines; used to gather all the lines (slurs, ties,
  etc.) needed in a measure.) }
  TLineList = class(specialize TObjectList<TLine>)
  public
    { Create a tree of MEI line elements (of different kinds, potentially),
      as siblings. }
    function ToMEI: TMeiNode;
  end;

  TFiguredBass = class
  private
    var
      FStaffNum: Integer;
      FTimeStamp: Integer;
      FFigures: TStringList;
  public
    constructor Create();
    destructor Destroy(); override;

    function ToMEI: TMeiNode;

    property StaffNum:  Integer     read FStaffNum;
    property TimeStamp: Integer     read FTimeStamp;
    property Figures:   TStringList read FFigures;
  end;

  TFiguredBassList = class(specialize TObjectList<TFiguredBass>)
  public
    { list of siblings }
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
      FFigureList: TFiguredBassList; 
   
    { Set the data needed for the right barline attribute. }
    procedure SetBarlineRight(Source: String);

  public
    constructor Create(); 

    { Create a new list of pitches from the Lilypond input string for a single
      measure of music, and the key relevant to this music. Recursively create
      all the pitches contained in the list. }
    constructor Create(LyInput: String; Key: TKeyKind);

    destructor Destroy(); override;

    { Create an MEI @code(measure) element, recursively generating the
      @code(note) elements it contains. }
    function ToMEI: TMeiNode;

    { What kind of barline follows this measure? }
    property BarlineRight:  TBarline      read FBarlineRight;

    { List of all the fermatas in this measure }
    property FermataList:   TFermataList  read FFermataList;

    { List of all the lines in this  measure }
    property LineList:      TLineList     read FLineList;
  end;

  { @abstract(A list of @link(TPitchList) objects for a single voice/layer.)

    This list contains a list of all the measures of music for a single voice
    (Lilypond) or layer (MEI). }
  TMeasureList = class(specialize TObjectList<TPitchList>)
  private
    var
      FHeaderText: String; { section heading text }

    { Go through measure list in which only tie starts have been set (from
      Lilypond input), and set attributes for notes in the middle and ending
      of the tie. }
    function AddTies: TMeasureList;

    { Go through measure list in which line starts and ends have been
      marked in the @link(TPitch) elements, and create MEI line elements
      connecting those notes. Used for ties, slurs, coloration brackets, and
      ligature brackets (MEI @code(slur) and @code(bracketSpan) elements). }
    function AddLines(LineKind: TLineKind): TMeasureList;

    { Add all the kinds of lines (ties, slurs, coloration brackets, and
      ligatures). }
    function AddAllLines: TMeasureList;

    { Go through measure list and add fermata elements within the MEI
      @code(measure) linked to their notes by the @code(startid). }
    function AddFermatas: TMeasureList;

    function MarkBetweenSlurs: TMeasureList;

  public
    constructor Create();

    { Create a list from the Lilypond input string for a
      single voice. Find the key for this music and then recursively create
      all the measures, and in turn all the pitches, it contains. }
    constructor Create(LyInput: String);

    { Create an MEI @code(tempo) element, used for a section heading, and add
      it to the MEI @code(measure) element. }
    function AddMeiSectionHead(MeiMeasure: TMeiNode): TMeiNode;
    
    function AddLyrics(SyllableList: TSyllableList): TMeasureList;

    { Create a @code(lirio:voice) element representing all the music for one
      voice/layer. Used for testing. } 
    function ToMEI: TMeiNode;
  end;

  { @abstract(Records the position of a line element (start and end IDs of the
    notes the line is attached to).) }
  TLinePosition = class
  public
    var
      FStartID, FEndID: String;
  public
    property StartID: String read FStartID write FStartID;
    property EndID:   String read FEndID   write FEndID;
  end;

  { @abstract(List of all the positions for lines in a measure.) }
  TLinePositionList = class(specialize TObjectList<TLinePosition>)
  public
    constructor Create(MeasureList: TMeasureList; LineKind: TLineKind);
  end;
     
  { @abstract(Internal representation of an MEI @code(note), @code(rest), or
    @code(mRest) (multimeasure rest) element.) }
  TMeiNoteRest = class(TMeiNode)
  private
    function IsNote: Boolean;
    function IsRest: Boolean;

    { Add the MEI @code(pname) attribute for the pitch name. }
    procedure AddMeiPnameAttribute(Pitch: TPitch);

    { Add the MEI @code(accid) and/or @code(accid.ges) attributes for the
      accidental, depending on whether the accidental is to be written
      explicitly in this key. }
    procedure AddMeiAccidAttribute(Pitch: TPitch);

    { Add the MEI @code(oct) attribute for the octave. }
    procedure AddMeiOctAttribute(Pitch: TPitch);

    { Add the MEI @code(dur) and @code(dots) attributes for the rhythmic
      duration. }
    procedure AddMeiDurDotsAttributes(Pitch: TPitch);

    { Create one or more MEI @code(artic) elements within a @code(note). }
    procedure AddMeiArticulation(Pitch: TPitch);
    
    procedure AddMeiSyllable(Pitch: TPitch);

  public
    { Create from internal @link(TPitch) structure. }
    constructor Create(Pitch: TPitch);
  end;

{ In Lilypond input, find a @code(\key) declaration, or in Lirio format,
  @code(\CantusDurus) (no accidentals) or @code(\CantusMollis) (one flat). }
function FindLyKey(KeyStr: String): TKeyKind;

{ Set information for right barline type in this measure. } 
function AddMeiBarlineAttr(MeiMeasure: TMeiNode; PitchList: TPitchList):
  TMeiNode; 

implementation

function GetPitchName(LyName: String): TPitchName;
var 
  PitchName: TPitchName;
begin
  case FirstCharStr(LyName) of 
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
      WriteLn(stderr, 'Unrecognized pitch name: ' + LyName);
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
      WriteLn(stderr, 'Unrecognized duration: ' + DurLy);
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

function AccidInKey(PitchName: TPitchName; Key: TKeyKind): TAccidental;

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
{ Mensa tonographica in memoriam P. Athanasii Kircheri }

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
      WriteLn(stderr, 'Unrecognized key, substituting CMajor');
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
      WriteLn(stderr, 'Unrecognized pitch name, substituting C');
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

  function GetKeyKind(Mode: TKeyMode; KeyName: String): TKeyKind;
  var
    Key: TKeyKind;
  begin
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
    result := Key;
  end;

var
  Key: TKeyKind = kkNone;
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
      KeyStr := StringDropAfter(KeyStr, '\major').Trim; 
      Key := GetKeyKind(kMajor, KeyStr);
    end
    else if KeyStr.Contains('\minor') then
    begin
      KeyStr := StringDropAfter(KeyStr, '\minor').Trim; 
      Key := GetKeyKind(kMinor, KeyStr);
    end
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
  Spec: TArticulationSpec;
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

constructor TSyllable.Create();
begin
  inherited Create;
end;

constructor TSyllable.Create(SylText: String; 
  SylPosition: TSyllablePosition = skSingle);
begin
  inherited Create;
  FText := SylText;
  FPosition := SylPosition;
end;

procedure TSyllable.Assign(OtherSyl: TSyllable);
begin
  if Assigned(OtherSyl) then
  begin
    FText := OtherSyl.SylText;
    FPosition := OtherSyl.SylPosition;
  end;
end;

function TSyllable.ToMEI: TMeiNode;
var
  Syl: TMeiNode;
begin
  Syl := TMeiNode.Create('syl');
  Syl.TextNode := SylText;

  if SylPosition > skSingle then
  begin
    case SylPosition of
      skBeginning        : Syl.AddAttribute('wordpos', 'i');
      skMiddle           : Syl.AddAttribute('wordpos', 'm');
      skEnd, skEndExtend : Syl.AddAttribute('wordpos', 't');
    end;

    case SylPosition of
      skBeginning, skMiddle       : Syl.AddAttribute('con', 'd');
      skEndExtend, skSingleExtend : Syl.AddAttribute('con', 'u');
    end;
  end;
  result := Syl;
end;

constructor TSyllableList.Create();
begin
  inherited Create();
end;

{ TODO deal with \EdLyrics and other markup }
{ TODO elided lyrics, e.g., 'a~un' or 'a_un' -> two syllables on one note }
{ given input of \lyrics expression }
constructor TSyllableList.Create(LyInput: String);

  function ValidSyllable(Syl: String): Boolean;
  begin
    result := (Syl <> '{') and (Syl <> '}') and not Syl.StartsWith('\');
  end;

var
  TokenizedInput: TStringArray;
  ThisString: String;
  NewSyllable: TSyllable;
begin
  inherited Create();

  LyInput := LyArg(LyInput, '\lyricmode', rkExclusive);

  TokenizedInput := LyInput.Split([' ', LineEnding], 
    TStringSplitOptions.ExcludeEmpty);

  for ThisString in TokenizedInput do
  begin
    { Found a hyphen, this means the next item will be the continuation of the
      previous. Change previous item's position marker to indicate the word
      continues. A single word becomes a beginning; a word ending becomes a
      middle. Don't add the hyphen to the syllable list.
    }
    if (Self.Count > 0) and (ThisString = '--') then
    begin
      case Last.SylPosition of
        skSingle : Last.SylPosition := skBeginning;
        skEnd    : Last.SylPosition := skMiddle;
      end;
    end
    { Mark lyric extensions after the end of a word.
      NB: Lyric extenders must be explicitly specified in Lilypond source }
    else if (Self.Count > 0) and (ThisString = '__') then
    begin
      case Last.SylPosition of
        skEnd    : Last.SylPosition := skEndExtend;
        skSingle : Last.SylPosition := skSingleExtend;
      end;
    end
    else if ValidSyllable(ThisString) then
    begin
      { Found a string, not a hyphen or markup instruction.
        NB We are ignoring all commands (starting with backslashes) in lyrics.
        We do make syllables for '_' so that we have placeholders to align
        with text. If we are adding to an open (continuing) word, mark this as
        the ending; otherwise just add a single independent syllable. If we
        find another hyphen after this, we'll change this to be a middle. 
      }
      NewSyllable := TSyllable.Create(ThisString);
      if Self.Count > 0 then
      begin
        case Last.SylPosition of
          skBeginning, skMiddle : NewSyllable.SylPosition := skEnd;
        end;
      end;
      Add(NewSyllable);
    end;
  end;
end;

function TSyllableList.ToMEI: TMeiNode;
var
  Verse, NewSyl: TMeiNode;
  ThisSyllable: TSyllable;
begin
  Verse := TMeiNode.Create('verse');
  for ThisSyllable in Self do
  begin
    NewSyl := ThisSyllable.ToMEI;
    Verse.AppendChild(NewSyl);
  end;
  result := Verse;
end;



constructor TPitch.Create();
begin
  inherited Create;
  FID := GenerateXmlID;
  FSyllable := TSyllable.Create;
end;

constructor TPitch.Create(LyInput: String; Key: TKeyKind);
var
  NoteStr, PitchNameLy, OctLy, DurLy, EtcLy, Test: String;
begin
  Create;
  NoteStr := LyInput;

  { Move ligatures to after note (where Lilypond docs admit they should be!) }
  if NoteStr.StartsWith('\[') then
  begin
    NoteStr := NoteStr.Substring(2) + '\[';
  end;

  PitchNameLy := ExtractWord(1, NoteStr, 
                  [',', '''', '1', '2', '4', '8', '\']);
  NoteStr := StringDropBefore(NoteStr, PitchNameLy);
  
  OctLy := '';
  Test := FirstCharStr(NoteStr);
  case Test of
    '''', ',' :
    begin
      OctLy := ExtractWord(1, NoteStr, ['1', '2', '4', '8', '\']);
      NoteStr := StringDropBefore(NoteStr, OctLy);
    end;
  end;

  DurLy := '';
  Test := FirstCharStr(NoteStr);
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
  if IsValid and not IsRest then
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

destructor TPitch.Destroy();
begin
  FSyllable.Free;
  inherited Destroy;
end;

function TPitch.IsValid: Boolean;
begin
  result := not ((FPitchName = pkNone) or (FOct = -1) or (FDur = dkNone));
end;

function TPitch.IsRest: Boolean;
begin
  result := FPitchName >= pkRest;
end;

function TPitch.CanHaveSyllable: Boolean;
begin
  { DEBUG this doesn't work because we are not setting mkMiddle for notes
  between slurs }
  result := (not IsRest) and (Tie <= mkStart) and (Slur <= mkStart);
end;

function TMeiNoteRest.IsNote: Boolean;
begin
  result := Name = 'note';
end;

function TMeiNoteRest.IsRest: Boolean;
begin
  result := (Name = 'rest') or (Name = 'mRest');
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

procedure TMeiNoteRest.AddMeiSyllable(Pitch: TPitch);
var
  ThisSyllable: TSyllable;
  Verse, Syl: TMeiNode;
begin
  ThisSyllable := Pitch.Syllable;
  if IsNote and not ThisSyllable.SylText.IsEmpty then
  begin
    Verse := TMeiNode.Create('verse');
    Syl := ThisSyllable.ToMEI;
    Verse.AppendChild(Syl);
    AppendChild(Verse);
  end;
end;


function TPitch.PitchEq(P2: TPitch): Boolean;
begin
  result := (Name = P2.Name) and (Accid = P2.Accid);
end;

constructor TMeiNoteRest.Create(Pitch: TPitch);
begin
  inherited Create();

  case Pitch.Name of
    pkRest        : Name := 'rest';
    pkMeasureRest : Name := 'mRest';
    else
    begin
      Name := 'note';
      AddAttribute('xml:id', Pitch.ID);
      AddMeiPnameAttribute(Pitch);
      AddMeiAccidAttribute(Pitch);
      AddMeiOctAttribute(Pitch);
      AddMeiArticulation(Pitch);
    end;
  end;

  AddMeiDurDotsAttributes(Pitch);
  AddMeiSyllable(Pitch);
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

{ Makes a chain of sibling elements (no parent element enclosing them all) }
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

constructor TFiguredBass.Create();
begin
  inherited Create();
  FFigures := TStringList.Create();
end;

destructor TFiguredBass.Destroy();
begin
  FFigures.Free;
  inherited Destroy();
end;

function TFiguredBass.ToMEI: TMeiNode;
var
  HarmNode: TMeiNode = nil;
  FiguredBassNode: TMeiNode = nil;
  ThisFigureNode: TMeiNode = nil;
  ThisFigure: String;
begin
  HarmNode := TMeiNode.Create('harm');
  HarmNode.AddAttribute('staff', IntToStr(FStaffNum));
  HarmNode.AddAttribute('tstamp', IntToStr(FTimeStamp));

  FiguredBassNode := TMeiNode.Create('fb');
  for ThisFigure in FFigures do
  begin
    ThisFigureNode := TMeiNode.Create('f');
    ThisFigureNode.TextNode := ThisFigure;
    FiguredBassNode := FiguredBassNode.AppendChild(ThisFigureNode);
  end;
  HarmNode := HarmNode.AppendChild(FiguredBassNode);
  result := HarmNode;
end;

{ list of siblings }
function TFiguredBassList.ToMEI: TMeiNode;
var
  Root: TMeiNode = nil;
  NewHarmNode: TMeiNode = nil;
  ThisFiguredBass: TFiguredBass;
begin
  for ThisFiguredBass in Self do
  begin
    NewHarmNode := ThisFiguredBass.ToMEI;
    if not Assigned(Root) then
      Root := NewHarmNode
    else
      Root := Root.AppendSibling(NewHarmNode);
  end;
  result := Root;
end;

constructor TPitchList.Create();
begin
  inherited Create;
  FFermataList := TFermataList.Create();
  FLineList := TLineList.Create();
  FFigureList := TFiguredBassList.Create();
end;

{ TODO add all commands to be replaced (i.e., ignored) }
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

constructor TPitchList.Create(LyInput: String; Key: TKeyKind);
var
  ThisNote: String;
  Notes: TStringArray;
  NewPitch: TPitch;
begin
  Create;
  LyInput := ReplaceLyCommands(LyInput);

  Notes := LyInput.Split([' ']);
  for ThisNote in Notes do
  begin
    NewPitch := TPitch.Create(ThisNote, Key);
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
  FFermataList.Free;
  FLineList.Free;
  FFigureList.Free;
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
  Assert(Assigned(PitchList) and Assigned(MeiMeasure));
  Assert((MeiMeasure.Name = 'lirio:measure') or (MeiMeasure.Name = 'measure'));

  case PitchList.BarlineRight of
    bkNormal    : Attr := '';
    bkMiddle    : Attr := 'dbl';
    bkFinal     : Attr := 'end';
    bkRepeatEnd : Attr := 'rptend';
  end;

  if not Attr.IsEmpty then 
  begin
    MeiMeasure.AddAttribute('right', Attr);
  end;
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
    ThisMeiNote := TMeiNoteRest.Create(ThisPitch);
    
    if not Assigned(MeiTree) then
      MeiTree := ThisMeiNote
    else
      MeiTree.AppendSibling(ThisMeiNote);
  end;
  result := MeiTree;
end;

function TMeasureList.AddFermatas: TMeasureList;
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
  result := Self;
end;

constructor TMeasureList.Create();
begin
  inherited Create;
end;

constructor TMeasureList.Create(LyInput: String);
var
  Key: TKeyKind;
  LyLines: TStringArray;
  SearchStr, ThisLine, TestLine, MeasureStr: String;
begin
  inherited Create;
  { Find the key signature for this voice }
  SearchStr := LyInput.Substring(0, 800); 
  Key := FindLyKey(SearchStr);

  { Find measures and parse the notes in them }
  LyLines := LyInput.Split([' ¶ '], TStringSplitOptions.ExcludeEmpty);
  WriteLn(stderr, 'LyLines[0]:' + LyLines[0]);
  for ThisLine in LyLines do
  begin
    WriteLn(stderr, 'ThisLine: ' + ThisLine);
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
      MeasureStr := SubstringAfter(ThisLine, '| ');
      WriteLn(stderr, 'Try to create measure from input: ' + MeasureStr);

      Self.Add(TPitchList.Create(MeasureStr, Key));
    end;
  end;

  Self := AddTies;
  Self := AddFermatas;
  Self := AddAllLines;

end;

{ TODO would be nice to have an algorithm without the boolean switching }
function TMeasureList.AddTies: TMeasureList;
var
  ThisMeasure: TPitchList;
  ThisPitch, TiedPitch: TPitch;
  FoundTie: Boolean = False;
begin
  for ThisMeasure in Self do
  begin
    for ThisPitch in ThisMeasure do 
    begin
      if ThisPitch.Tie = mkStart then
      begin
        TiedPitch := ThisPitch; 
        if FoundTie then
        begin
          ThisPitch.Tie := mkEndStart;
        end;
        FoundTie := True;
      end
      else if FoundTie then
      begin
        if ThisPitch.PitchEq(TiedPitch) then
        begin
          ThisPitch.Tie := mkEnd;
          FoundTie := False;
        end;
      end;
    end;
  end;
  result := Self;
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
  ThisID: String;
  NewLinePosition: TLinePosition = nil;
  FinishedLine: Boolean = False;
begin
  Assert(Assigned(MeasureList));
  inherited Create();

  for ThisMeasure in MeasureList do
  begin
    for ThisPitch in ThisMeasure do
    begin
      LineField := SelectLineField(ThisPitch, LineKind);
      ThisID := ThisPitch.ID;
      case LineField of
        mkStart : 
        begin
          NewLinePosition := CreateStartPosition(ThisID);
          FinishedLine := False;
        end;

        mkEnd : 
        begin
          if Assigned(NewLinePosition) then
          begin
            AddCompletePosition(Self, NewLinePosition, ThisID);
            FinishedLine := True;
          end;
        end;

        mkEndStart :
        begin
          if Assigned(NewLinePosition) then
          begin
            AddCompletePosition(Self, NewLinePosition, ThisID);
          end;
          NewLinePosition := CreateStartPosition(ThisID);
          FinishedLine := False;
        end;
      end;
    end;
  end;
  if Assigned(NewLinePosition) and not FinishedLine then
    FreeAndNil(NewLinePosition);
end;


function TMeasureList.AddLines(LineKind: TLineKind): TMeasureList;

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
  result := Self;
end;

function TMeasureList.MarkBetweenSlurs: TMeasureList;
var
  ThisMeasure: TPitchList;
  ThisPitch: TPitch;
  InSlur: Boolean;
begin
  InSlur := False;
  for ThisMeasure in Self do
  begin
    for ThisPitch in ThisMeasure do
    begin
      case ThisPitch.Slur of
        mkStart, mkMiddle, mkEndStart : 
          InSlur := True;
        mkEnd : 
          InSlur := False;
        mkNone : 
          if InSlur then
            ThisPitch.Slur := mkMiddle;
      end;
    end;
  end;
  result := Self;
end;

function TMeasureList.AddAllLines: TMeasureList;
begin
  Self := AddLines(lkTie);
  Self := AddLines(lkSlur);
  Self := AddLines(lkColoration);
  Self := AddLines(lkLigature);
  Self := MarkBetweenSlurs;
  result := Self;
end;

function TMeasureList.AddMeiSectionHead(MeiMeasure: TMeiNode): TMeiNode;
var
  SectionHead: TMeiNode = nil;
begin
  Assert(Assigned(MeiMeasure));
  Assert((MeiMeasure.Name = 'lirio:voice') 
    or (MeiMeasure.Name = 'measure'));

  if not FHeaderText.IsEmpty then
  begin
    SectionHead := TMeiNode.Create('tempo');
    SectionHead.AddAttribute('place', 'above');
    SectionHead.AddAttribute('staff', '1');
    SectionHead.AddAttribute('tstamp', '1');
    SectionHead.TextNode := FHeaderText;
    MeiMeasure.AppendChild(SectionHead);
  end;
  result := MeiMeasure;
end;

function TMeasureList.AddLyrics(SyllableList: TSyllableList): TMeasureList;
var
  ThisSyllable: TSyllable;
  ThisMeasure: TPitchList;
  ThisPitch: TPitch;
  SyllableIndex: Integer;
begin
  SyllableIndex := 0;
  for ThisMeasure in Self do
  begin
    for ThisPitch in ThisMeasure do
    begin
      if ThisPitch.CanHaveSyllable 
        and (SyllableIndex < SyllableList.Count) then
      begin
        ThisSyllable := SyllableList[SyllableIndex];
        if ThisSyllable.SylText <> '_' then
        begin
          ThisPitch.Syllable.Assign(ThisSyllable);
        end;
        Inc(SyllableIndex);
      end
      else
        continue;
    end;
  end;

  result := Self;
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
