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

uses SysUtils, StrUtils, Classes, Generics.Collections, StringTools, Outline,
ScoreTree, MEI;

type 
  { Labels for pitch classes }
  TPitchName = (pkNone, pkC, pkD, pkE, pkF, pkG, pkA, pkB, pkRest, pkMeasureRest);

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

    { Generate the MEI @code(pname) attribute for the pitch name. }
    function MEIPName: String;

    { Generate the MEI @code(accid) and/or @code(accid.ges) attributes for the
      accidental, depending on whether the accidental is to be written
      explicitly in this key. }
    function MEIAccid: String;

    { Generate the MEI @code(oct) attribute for the octave. }
    function MEIOct: String;

    { Generate the MEI @code(dur) and @code(dots) attributes for the rhythmic
      duration. }
    function MEIDurDots: String;

    { Generate the MEI @code(tie) attribute. }
    function MEITie: String;

    { Generate one or more MEI @code(artic) elements within a @code(note). }
    function MEIArtic: String;

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

    { Generate a complete MEI @code(note) element for this pitch. }
    function ToMEI: String;
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

    procedure  SetBarlineRight(Source: String);

    function MEIBarlineAttr: String;

    { Generate an MEI @code(measure) element, recursively generating the
      @code(note) elements it contains. }
    function ToMEI: TStringListAAC;

    { Return the results of @link(TPitchList.ToMEI) as a string instead of a
      list. }
    function ToMEIString: String;
  end;

  { @abstract(A list of @link(TPitchList) objects for a single voice/layer.)

    This list contains a list of all the measures of music for a single voice
    (Lilypond) or layer (MEI). }
  TMeasureList = class(specialize TObjectList<TPitchList>)
  private
    var
      FPrefix, FSuffix: String;
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

  { Label to indicate whether we should assign all measures or just one. Used
  when converting a Lilypond tree to an MEI structure, where there are just
  the notes for one measure at the bottom of the tree.}
  TMeasureCopyMode = (mkAllMeasures, mkOneMeasure);

  { @abstract(A node in an LCRS tree used to represent MEI structure.)

    Like @link(TLyObject), this class functions as nodes in an
    left-child/right-sibling tree. Each node represents one XML element. We
    will convert the @link(TLyObject) tree taken from Lilypond input into a
    new tree. }
  TMEIElement = class
  private
    var
      { The element must be one of a standard set of types. }
      FType: TMusicTreeElement;

      { Name and ID strings }
      FName, FID: String;

      { Any kind of text }
      FText: String;
      
      { Position in series of same type }
      FNum: Integer;

      { Only elements with @code(ekLayer) type contain a list of measures; 
        for all other types this will be @code(nil). }
      FMeasures: TMeasureList;

      { Links to rest of tree }
      FChild, FSibling: TMEIElement;
  public
    constructor Create();

    { Create from all fields (default is to create a single node with
      @code(nil) relations) }
    constructor Create(ElementType: TMusicTreeElement; ID: String = '';
      TextStr: String = ''; Num: Integer = 1);

    { Destroy entire tree }
    destructor Destroy; override;

    { Return the last left child in the tree. }
    function LastChild: TMEIElement;

    { Return the last right sibling in the tree. }
    function LastSibling: TMEIElement;

    { Deep copy of all fields of a single element node to this one, including
      the measure list (and its contents recursively) if there is one. }
    procedure AssignNode(Source: TMEIElement; Mode: TMeasureCopyMode =
      mkAllMeasures; MeasureIndex: Integer = 0); 

    { Deep copy of the entire tree starting at this node, including any
      measure lists it contains and all their contents recursively. }
    procedure AssignTree(Source: TMEIElement; Mode: TMeasureCopyMode =
      mkAllMeasures; MeasureIndex: Integer = 0); 

    { Copy data to convert from a @link(TLyObject). }
    procedure SetFromLyObject(LyObject: TLyObject);

    { How many measures are in the measure list within this element? Return -1
      if not all measure lists have the same count. }
    function CountMeasures: Integer;

    { Convert the tree to a simple string representation for debugging. }
    function ToString: String; override;

    { Fill in ties, slurs, and fermatas from the markers taken from Lilypond
      input. }
    procedure AddAnalyticalMarkup;
   
    { Combine all the @link(TMeasureList) suffixes in each measure element. }
    function ConcatMeasureSuffixes(MeasureIndex: Integer): String;

    { First we copy a @link(TLyObject) tree to a @link(TMEIElement) tree,
      preserving its structure (score/staff/voice/measures). With this
      function we create a new tree that is organized in the MEI hierarchy
      (section/measure/staff/layer/notes).

      We first count the measures and make sure all voices have the same
      number of measures. Then for each measure, we create a new tree for that
      measure, where the child element is the entire original tree
      (staff/layer), but we include only a single measure in the list at the
      bottom, corresponding to the music for this measure. }
    function StaffToMeasureTree: TMEIElement;

    { Generate the MEI output for the whole tree starting at this node,
      including recursively all its subelements and relations. Return a
      stringlist with one MEI node and its contents. }
    function ToMEI: TStringListAAC;
  end;

{ Convert a @code(TLyObject) tree to a @code(TMEIElement) tree, preserving the
  same hierarchy from the Lilypond input. }
function LyToMEITree(LyNode: TLyObject; MEINode: TMEIElement): TMEIElement;

{ For now, replace section command with MEI equivalent. 
  Eventually (TODO) replace multiple commands.

  If no commands are found, return the original string.
 
  @table(
    @rowHead( @cell(Lilypond) @cell(MEI) )
    @row( @cell(@code(\Section "[ESTRIBILLO] SOLO"))
          @cell(@code(<tempo tstamp="1">[ESTRIBILLO] SOLO</tempo>)) ) 
  )
}
function MEISectionHead(Source: String): String;

{ Parse a Lilypond \score expression and create an MEI music
  element including the scoreDef and music notes. 
function CreateMEIMusic(SourceLines: TStringListAAC): TStringListAAC;
}

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

  PitchNameLy := ExtractWord(1, NoteStr, [',', '''', '1', '2', '4', '8', '\']);
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
      DurLy := ExtractWord(1, NoteStr, ['(', ')', '~', '\', '[', ']', '*', '<', '-']);
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
  AccidSounded, AccidWritten: String;
begin
  case FAccid of
    akNatural : AccidSounded := 'n';
    akFlat    : AccidSounded := 'f';
    akSharp   : AccidSounded := 's';
  end;

  AccidWritten := '';
  if FAccidType = akExplicit then
    AccidWritten := ' ' + XMLAttribute('accid', AccidSounded);
  
  AccidSounded := XMLAttribute('accid.ges', AccidSounded);

  result := AccidSounded + AccidWritten;
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
    Dur := Format('%s %s', [Dur, XMLAttribute('dots', '1')]);

  result := Dur;
end;

function TPitch.MEITie: String;
var 
  Position: String;
  MEI: String = '';
begin
  if FTie <> mkNone then
  begin
    case FTie of
      mkStart    : Position := 'i';
      mkMiddle   : Position := 'm';
      mkEnd      : Position := 't';
    end;
    MEI := ' ' + XMLAttribute('tie', Position);
  end;
  result := MEI;
end;

function TPitch.MEIArtic: String;
  function Artic(Kind: String): String;
  begin
    result := XMLElement('artic', XMLAttribute('artic', Kind));
  end;

var
  MEI: String = '';
begin
  { Fermata is handled separately in TMeasureList.AddFermatas }
  with FArticulations do
  begin
    if FAccent then
      MEI := MEI + Artic('acc');
    if FStaccato then
      MEI := MEI + Artic('stacc');
    if FTenuto then
      MEI := MEI + Artic('ten');
    if FStaccatissimo then
      MEI := MEI + Artic('stacciss');
    if FMarcato then
      MEI := MEI + Artic('marc');
  end; 

  result := MEI;
end;

function TPitch.PitchEq(P2: TPitch): Boolean;
begin
  result := (FPitchName = P2.FPitchName) and (FAccid = P2.FAccid);
end;

function TPitch.ToMEI: String;
var
  Dur, ID, MEI: String;
begin
  Dur := MEIDurDots;
  ID  := XMLAttribute('xml:id', FID);

  case FPitchName of
    pkRest:        MEI := XMLElement('rest', Format('%s %s', [ID, Dur]));
    pkMeasureRest: MEI := XMLElement('mRest', Format('%s %s', [ID, Dur]));
    else
    begin
      MEI := XMLElement('note', Format('%s %s %s %s %s%s', 
                [ID, MEIPname, MEIAccid, MEIOct, Dur, MEITie]),
                MEIArtic); 
    end;
  end;
  result := MEI;
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
    Source := Source.Replace(Translation[PairIndex][0], Translation[PairIndex][1]);

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

function TPitchList.MEIBarlineAttr: String;
var 
  Attr, MEI: String;
begin
  case FBarlineRight of
    bkNormal    : Attr := '';
    bkMiddle    : Attr := 'dbl';
    bkFinal     : Attr := 'end';
    bkRepeatEnd : Attr := 'rptend';
  end;

  if Attr.IsEmpty then 
    MEI := ''
  else
    MEI := XMLAttribute('right', Attr);
 
  DebugLn('Made MEI Barline: ' + MEI);
  result := MEI;
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

function TPitchList.ToMEIString: String;
var
  TempLines: TStringListAAC;
  OutputStr: String;
begin
  TempLines := Self.ToMEI;
  OutputStr := TempLines.Text;
  FreeAndNil(TempLines);
  result := OutputStr;
end;

function MEISectionHead(Source: String): String;
var
  HeadingText: String;
begin
  HeadingText := CopyStringBetween(Source, '\Section "', '"');
  result := XMLElement('tempo', Format('%s %s %s',
              [XMLAttribute('place', 'above'), 
               XMLAttribute('staff', '1'), 
               XMLAttribute('tstamp', '1')]),
              HeadingText);
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
      DebugLn('Section heading found: ' + ThisLine);
      FPrefix := MEISectionHead(ThisLine);
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
    
    FPrefix := Source.FPrefix;
    FSuffix := Source.FSuffix;
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

constructor TMEIElement.Create();
begin
  inherited Create;
  FID := GenerateID;
end;

constructor TMEIElement.Create(ElementType: TMusicTreeElement; ID: String = '';
  TextStr: String = ''; Num: Integer = 1);
begin
  inherited Create;
  FType        := ElementType;
  FName        := TypeToName(ElementType);

  if ID <> '' then
    FID := ID;

  FText        := TextStr;
  FNum         := Num;
  FMeasures    := nil;
  FChild       := nil;
  FSibling     := nil;
end;

procedure TMEIElement.AssignNode(Source: TMEIElement; Mode: 
  TMeasureCopyMode = mkAllMeasures; MeasureIndex: Integer = 0); 
var
  NewMeasure: TPitchList;
begin
  FType := Source.FType;
  FName := Source.FName;
  FID   := Source.FID;
  FText := Source.FText;
  FNum  := Source.FNum;
  if Assigned(Source.FMeasures) then
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

          {
          if MeasureIndex = 0 then
            FMeasures.FPrefix := Source.FMeasures.FPrefix
          else if MeasureIndex = Source.FMeasures.Count - 1 then
            FMeasures.FSuffix := Source.FMeasures.FSuffix;
          }
          FMeasures.FPrefix := Source.FMeasures.FPrefix;
          FMeasures.FSuffix := Source.FMeasures.FSuffix;
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
  if Assigned(TreeA) then
  begin
    if not Assigned(TreeB) then
      TreeB := TMEIElement.Create;
  
    TreeB.AssignNode(TreeA, Mode, MeasureIndex);

    if Assigned(TreeA.FChild) then
      TreeB.FChild := InnerAssign(TreeA.FChild, TreeB.FChild);
    if Assigned(TreeA.FSibling) then
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

  if LyObject.FID <> '' then
    FID       := LyObject.FID;

  FNum      := LyObject.FNum;
  FChild    := nil;
  FSibling  := nil;
  if LyObject.FContents.IsEmpty then
    FMeasures := nil
  else
  begin
    FMeasures := TMeasureList.Create;
    FMeasures.SetFromLy(LyObject.FContents);
  end;
end;

function TMEIElement.CountMeasures: Integer;
var
  MasterCount: Integer = 0;
function InnerCount(Node: TMEIElement): Integer;
var
  ThisCount: Integer = 0;
begin
  if Assigned(Node) then
  begin
    if (Node.FType = ekLayer) and Assigned(Node.FMeasures) then
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
    if Assigned(Node.FChild) then
      ThisCount := InnerCount(Node.FChild);
    if Assigned(Node.FSibling) then
      ThisCount := InnerCount(Node.FSibling);
  end;
  result := ThisCount;
end;
begin
  result := InnerCount(Self)
end;

function LyToMEITree(LyNode: TLyObject; MEINode: TMEIElement): TMEIElement;
begin
  if Assigned(LyNode) then
  begin
    DebugLn('LyToMEITree: visiting non-empty LyObjectNode with FType: ');
    {$ifdef DEBUG}WriteLn(LyNode.FType);{$endif}
    
    case LyNode.FType of
      ekStaff, ekLayer :
      begin
        if not Assigned(MEINode) then
        begin
          MEINode := TMEIElement.Create;
          MEINode.SetFromLyObject(LyNode);
        end;
      end;
    end;
    { If we haven't made a new node yet, then we are skipping a parent node }
    if not Assigned(MEINode) then
    begin
      if Assigned(LyNode.FChild) then
        MEINode := LyToMEITree(LyNode.FChild, MEINode);
      if Assigned(LyNode.FSibling) then
        MEINode := LyToMEITree(LyNode.FSibling, MEINode);
    end
    else 
    begin
      if Assigned(LyNode.FChild) then
      begin
        { Because we are skipping some parent nodes, we may find child nodes
        in the original tree that need to become sibling nodes in the new tree }
        if MEINode.FType = LyNode.FChild.FType then
        begin
          MEINode.LastSibling.FSibling := 
            LyToMEITree(LyNode.FChild, MEINode.LastSibling.FSibling);
        end
        else
          MEINode.FChild := LyToMEITree(LyNode.FChild, MEINode.FChild);
      end;

      if Assigned(LyNode.FSibling) then
        MEINode.FSibling := LyToMEITree(LyNode.FSibling, MEINode.FSibling);
    end;
  end;

  result := MEINode;
end;

destructor TMEIElement.Destroy;
begin
  FreeAndNil(FMeasures);
  if Assigned(FChild) then 
    FChild.Destroy;
  if Assigned(FSibling) then 
    FSibling.Destroy;
  inherited Destroy;
end;

function TMEIElement.LastChild: TMEIElement;
begin
  if not Assigned(FChild) then
    result := Self
  else
    result := FChild.LastChild;
end;


function TMEIElement.LastSibling: TMEIElement;
begin
  if not Assigned(FSibling) then
    result := Self
  else
    result := FSibling.LastSibling;
end;


function TMEIElement.ToString: String;
var
  OutputStr: String = '';
  ThisPitchList: TPitchList;
begin
  OutputStr := OutputStr + Format('%s %s %d', [FName, FID, FNum]) + LineEnding;
  if Assigned(FMeasures) then
  begin
    OutputStr := OutputStr + Format('Measurelist prefix: ''%s''', 
                    [FMeasures.FPrefix]) + LineEnding; 

    for ThisPitchList in FMeasures do
    begin
      OutputStr := OutputStr + ThisPitchList.ToMEIString;
      OutputStr := OutputStr + Format('PitchList line SUFFIX: ''%s''', 
                    [ThisPitchList.FLines]) + LineEnding; 
      OutputStr := OutputStr + Format('PitchList fermata SUFFIX: ''%s''', 
                    [ThisPitchList.FFermata]) + LineEnding; 

    end;

    OutputStr := OutputStr + Format('Measurelist suffix: ''%s''',
                  [FMeasures.FSuffix]) + LineEnding;
  end;
  if Assigned(FChild) then
    OutputStr := OutputStr + Format('CHILD (to %s %d): %s', 
                  [Fname, FNum, FChild.ToString]); 

  if Assigned(FSibling) then
    OutputStr := OutputStr + Format('SIBLING (to %s %d): %s',
                  [Fname, FNum, FSibling.ToString]); 
  result := OutputStr;
end;

function TMEIElement.ConcatMeasureSuffixes(MeasureIndex: Integer): String;
function InnerConcat(Node: TMEIElement; SuffixStr: String = ''): String;
var
  ThisMeasure: TPitchList;
begin
  if Assigned(Node) then
  begin
    if (Node.FType = ekLayer) and Assigned(Node.FMeasures) then
    begin
      ThisMeasure := Node.FMeasures[MeasureIndex];
      if ThisMeasure.FLines <> '' then
          SuffixStr := Format('%s %s', [SuffixStr, ThisMeasure.FLines]);
      if ThisMeasure.FFermata <> '' then
          SuffixStr := Format('%s %s', [SuffixStr, ThisMeasure.FFermata]);
      { TODO using a stringlist would be better, but using data objects for
      slur elements would be better yet }
    end;

    if Assigned(Node.FChild) then
      SuffixStr := InnerConcat(Node.FChild, SuffixStr);
    
    if Assigned(Node.FSibling) then
      SuffixStr := InnerConcat(Node.FSibling, SuffixStr);
  end;
  DebugLn('PitchList SUFFIX string currently: ' + SuffixStr);
  result := SuffixStr;
end;
begin
  result := InnerConcat(Self);
end;

function TMEIElement.StaffToMeasureTree: TMEIElement;
var
  MeasureCount, MeasureNum: Integer;
  MeasureTree, Root, Branch, Prefix, Suffix, Leaf: TMEIElement;
  ThisMeasure: TPitchList;
  PrefixStr: String;
  SuffixStr: String = '';
  BeforeEndMeasureStr: String = '';
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
    Root := TMEIElement.Create(ekMeasure, '', '', MeasureNum + 1);

    Branch := TMEIElement.Create;
    Branch.AssignTree(Self, mkOneMeasure, MeasureNum);

    Leaf := Branch.LastChild;
    if Assigned(Leaf.FMeasures) then
    begin
      ThisMeasure := Leaf.FMeasures.First;
      if ThisMeasure.FBarlineRight <> bkNormal then
        Root.FText := ThisMeasure.MEIBarlineAttr;
        { TODO there must a better way }
    end;

    BeforeEndMeasureStr := Self.ConcatMeasureSuffixes(MeasureNum);
    Branch.LastSibling.FSibling := TMEIElement.Create(ekXML, '', BeforeEndMeasureStr);

    Root.FChild := Branch;

    if MeasureNum = 0 then
    begin
      if Assigned(Branch.LastChild.FMeasures) then
      begin
        PrefixStr := Branch.LastChild.FMeasures.FPrefix;
        if PrefixStr <> '' then
        begin
          DebugLn('Found measurelist prefix' + PrefixStr);
          Prefix := TMEIElement.Create(ekXML, '', PrefixStr);
          Prefix.FSibling := Branch;
          Root.FChild := Prefix;
        end;
      end;

      MeasureTree := Root
    end
    else if MeasureNum = MeasureCount - 1 then
    begin
      if Assigned(Branch.LastChild.FMeasures) then
      begin
        SuffixStr := Branch.LastChild.FMeasures.FSuffix;
        if SuffixStr <> '' then
        begin
          DebugLn('Found measurelist suffix' + SuffixStr);
          Suffix := TMEIElement.Create(ekXML, '', SuffixStr);
          Root.LastSibling.FSibling := Suffix;
        end;
      end;
      
      MeasureTree.LastSibling.FSibling := Root;
    end
    else
      MeasureTree.LastSibling.FSibling := Root;
  end;
  result := MeasureTree;
end;

function TMEIElement.ToMEI: TStringListAAC;
function InnerToMEI(Node: TMEIElement; MEI: TStringListAAC): TStringListAAC;
  function InnerAddNewElement(Node: TMEIElement; List: TStringListAAC): TStringListAAC;
  var
    NewLines: TStringListAAC;
  begin
    assert(Assigned(List));
    NewLines := TStringListAAC.Create;
    NewLines := InnerToMEI(Node, NewLines);
    List.AddStrings(NewLines);
    FreeAndNil(NewLines);
    result := List;
  end;

var
  NewElement, NewMeasure: TStringListAAC;
  Attributes: String = '';
begin
  if Assigned(Node) then
  begin
    NewElement := TStringListAAC.Create;

    if (Node.FType = ekLayer) and Assigned(Node.FMeasures) then
    begin
      NewMeasure := Node.FMeasures.First.ToMEI;
      NewElement.AddStrings(NewMeasure);
      FreeAndNil(NewMeasure);
    end
    else
    begin
      if (Node.FType = ekMeasure) and (Node.FText <> '') then
      begin
        Attributes := Format('%s %s', [Attributes, Node.FText]);
        DebugLn('Add Barline attribute to Measure: ' + Attributes);
      end;
    end;

    if Assigned(Node.FChild) then
      NewElement := InnerAddNewElement(Node.FChild, NewElement);

    if Node.FType = ekXML then
      NewElement.Add(Node.FText)
    else
    begin
      Attributes := XMLAttribute('n', IntToStr(Node.FNum));
      if Node.FType = ekStaff then
      begin
        Attributes := Format('%s %s', 
          [Attributes,  XMLAttribute('def', '#' + Node.FID)]);
      end
      else if Node.FID <> '' then
      begin
        Attributes := Format('%s %s', 
          [Attributes, XMLAttribute('xml:id', Node.FID)]);
      end;
     
      if (Node.FType = ekMeasure) and (Node.FText <> '') then
      begin
        Attributes := Format('%s %s', [Attributes, Node.FText]);
        DebugLn('Add Barline attribute to Measure: ' + Attributes);
      end;
      

      NewElement.EncloseInXML(Node.FName, Attributes);
    end;

    if Assigned(Node.FSibling) then
      NewElement := InnerAddNewElement(Node.FSibling, NewElement);
  end;
  MEI.AddStrings(NewElement);
  FreeAndNil(NewElement);
  result := MEI;
end;

var
  MasterMEI: TStringListAAC;
begin
  MasterMEI := TStringListAAC.Create;
  MasterMEI := InnerToMEI(Self, MasterMEI);
  result := MasterMEI;
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

procedure TMEIElement.AddAnalyticalMarkup;
procedure MeasureListDo(List: TMeasureList);
begin
  List.AddTies;
  List.AddLines(lkSlur);
  List.AddLines(lkColoration);
  List.AddLines(lkLigature);
  List.AddFermatas;
end;

function InnerMarkup(Tree: TMEIElement): TMEIElement;
begin
  if Assigned(Tree) then
  begin
    if (Tree.FType = ekLayer) and Assigned(Tree.FMeasures) then
      MeasureListDo(Tree.FMeasures);

    if Assigned(Tree.FChild) then
      Tree.FChild := InnerMarkup(Tree.FChild);

    if Assigned(Tree.FSibling) then
      Tree.FSibling := InnerMarkup(Tree.FSibling);
  end;
  result := Tree;
end;

begin
  InnerMarkup(Self);
end;
{
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

    MEIStaffTree := LyToMEITree(LyObjectTree, MEIStaffTree);
    
    if Assigned(MEIStaffTree) then
    begin
      DebugLn('MEI TREE STAGE 1:' + LineEnding + MEIStaffTree.ToString);

      MEIStaffTree.AddAnalyticalMarkup;
      DebugLn('MEI TREE STAGE 1.5, analytical markup:' 
          + LineEnding + MEIStaffTree.ToString);

      MEIMeasureTree := MEIStaffTree.StaffToMeasureTree;
    end;
    if Assigned(MEIMeasureTree) then
    begin
      DebugLn('MEI TREE STAGE 2:' + LineEnding + MEIMeasureTree.ToString);
      
      MEIScoreLines := MEIMeasureTree.ToMEI;
    end;
  end;

  if not Assigned(MEIMusicLines) then
    MEIMusicLines := TStringListAAC.Create;

  if Assigned(MEIScoreLines) then
  begin
    MEIScoreLines.EncloseInXML('section');
    MEIMusicLines.AddStrings(MEIScoreLines);
  end;

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
}

end.

{ TODO find and replace all the commands that have a simple one-to-one match 
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
x    '\break'          :
x    '\fermata'        :

x  \color
x  \endcolor
x  \[ \] (ligatures)
}
