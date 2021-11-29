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
ScoreTree;

type 
  { Labels for pitch classes }
  TPitchName = (pkNone, pkC, pkD, pkE, pkF, pkG, pkA, pkB, pkRest, pkMeasureRest);

  { Labels for accidentals }
  TAccidental = (akFlat, akNatural, akSharp);

  { Labels for types of accidentals }
  TAccidType = (
    { Played but not written if part of the scale in its key }
    akImplicit, 
    { Played and written out, not part of the scale }
    akExplicit, 
    { Suggested editorially for historical performance practice 
      (TODO not implemented) }
    akFicta); 

  { Labels for rhythmic durations }
  TDuration = (dkNone, 
    { Double whole note }
    dkBreve, 
    { Whole }
    dkSemibreve, 
    { Half }
    dkMinim, 
    { Quarter }
    dkSemiminim, 
    { Eighth }
    dkFusa,
    { Sixteenth}
    dkSemifusa, 
    { Double whole, dotted }
    dkBreveDotted, 
    { Whole, dotted }
    dkSemibreveDotted, 
    { Half, dotted }
    dkMinimDotted,
    { Quarter, dotted }
    dkSemiminimDotted, 
    { Eigth, dotted }
    dkFusaDotted);

type
  { Our internal structure for storing data for a pitch, using the labels
    above except for octave, which is an integer in the Helmholtz system (middle
    C is C4). } 
  TPitch = class
  public
    var
      FPitchName: TPitchName;
      FAccid: TAccidental;
      FAccidType: TAccidType;
      FOct: Integer;
      FDur: TDuration;

    constructor Create();
    
    { Create from all the fields }
    constructor Create(Name: TPitchName; Accid: TAccidental; AccidType:
      TAccidType; Oct: Integer; Dur: TDuration); 

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

    { Generate a complete MEI @code(note) element for this pitch. }
    function ToMEI: String;
  end;
 
  { A list of @link(TPitch) objects, corresponding to one measure of music. }
  TPitchList = class(specialize TObjectList<TPitch>)
  public
    { Create a new list of pitches from the Lilypond input string for a single
      measure of music, and the key relevant to this music. Recursively create
      all the pitches contained in the list. }
    constructor CreateFromLy(Source: String; Key: TKeyKind);

    { Deep copy of all pitches from another list to this one. }
    procedure Assign(Source: TPitchList);

    { Generate an MEI @code(measure) element, recursively generating the
      @code(note) elements it contains. }
    function ToMEI: TStringListAAC;
  end;

  { A list of @link(TPitchList) objects, corresponding to a list of all the
  measures of music for a single voice (Lilypond) or layer (MEI). }
  TMeasureList = class(specialize TObjectList<TPitchList>)
  public
    { Create a new list from the Lilypond input string for a single voice.
      Find the key for this music and then recursively create all the
      measures, and in turn all the pitches, it contains. }
    constructor CreateFromLy(Source: String);

    { Deep copy of all measures and pitches in another list to this one. }
    procedure Assign(Source: TMeasureList);
  end;

  { Label to indicate whether we should assign all measures or just one. Used
  when converting a Lilypond tree to an MEI structure, where there are just
  the notes for one measure at the bottom of the tree.}
  TMeasureCopyMode = (mkAllMeasures, mkOneMeasure);

  { Like @link(TLyObject), this class functions as nodes in an LCRS tree. Each
    node represents one XML element. We will convert the @link(TLyObject) tree
    taken from Lilypond input into a new tree. }
  TMEIElement = class
  private
    var
      { The element must be one of a standard set of types. }
      FType: TMusicTreeElement;
      { Name and ID strings }
      FName, FID: String;
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
    constructor Create(ElementType: TMusicTreeElement; ID: String; 
      Num: Integer = 1; Measures: TMeasureList = nil; 
      Child: TMEIElement = nil; Sibling: TMEIElement = nil);

    { Destroy entire tree }
    destructor Destroy; override;

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

{ Parse a Lilypond \score expression and create an MEI music
  element including the scoreDef and music notes. }
function CreateMEIMusic(SourceLines: TStringListAAC): TStringListAAC;


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
    else 
    begin
      WriteLn(StdErr, 'Unrecognized pitch name, substituting C: input was ' + LyName);
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

constructor TPitch.Create();
begin
  inherited Create;
end;

constructor TPitch.Create(Name: TPitchName; Accid: TAccidental; 
  AccidType: TAccidType; Oct: Integer; Dur: TDuration);
begin
  inherited Create;
  FPitchName := Name;
  FAccid := Accid;
  FAccidType := AccidType;
  FOct := Oct;
  FDur := Dur;
end;

constructor TPitch.CreateFromLy(Source: String; Key: TKeyKind);
var
  NoteStr, PitchNameLy, OctLy, DurLy, EtcLy, Test: String;
begin
  inherited Create;
  NoteStr := Source;
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
      DurLy := ExtractWord(1, NoteStr, ['(', ')', '~', '\', '[', ']', '*']);
      NoteStr := StringDropBefore(NoteStr, DurLy);
      EtcLy := NoteStr; { TODO }
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
end;

procedure TPitch.Assign(Source: TPitch);
begin
  if Source <> nil then
  begin
    FPitchName := Source.FPitchName;
    FAccid := Source.FAccid;
    FAccidType := Source.FAccidType;
    FOct := Source.FOct;
    FDur := Source.FDur;
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
    Dur := Dur + ' ' + XMLAttribute('dots', '1');
  result := Dur;
end;

function TPitch.ToMEI: String;
var
  Dur, MEI: String;
begin
  Dur := MEIDurDots;
  case FPitchName of
    pkRest:        MEI := XMLElement('rest', '', Dur);
    pkMeasureRest: MEI := XMLElement('mRest', '', Dur);
    else
      MEI := XMLElement('note', '', MEIPname + ' ' + MEIAccid + ' ' 
        + MEIOct + ' ' + Dur); 
  end;
  result := MEI;
end;

constructor TPitchList.CreateFromLy(Source: String; Key: TKeyKind);
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
    NewPitch := TPitch.CreateFromLy(ThisNote, Key);
    if NewPitch.IsValid then
      Self.Add(NewPitch)
    else
    begin
      WriteLn(StdErr, 'Invalid Pitch found in source ''' + ThisNote + '''');
      FreeAndNil(NewPitch);
    end;
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
  Key: TKeyKind;
  LyLines: TStringListAAC;
  SearchStr, ThisLine: String;
begin
  inherited Create;
  { Find the key signature for this voice }
  SearchStr := Source.Substring(0, 800); 
  Key := FindLyKey(SearchStr);

  { Find measures and parse the notes in them }
  LyLines := TStringListAAC.Create(Source);
  for ThisLine in LyLines do
  begin
    if ThisLine.TrimLeft.StartsWith('|') 
      and (ThisLine.CountChar('|') = 1) then 
    begin
      DebugLn('Adding new TPitchList to TMeasureList...');
      Self.Add(TPitchList.CreateFromLy(ThisLine, Key));
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

function LyToMEITree(LyNode: TLyObject; MEINode: TMEIElement): TMEIElement;
begin
  if LyNode <> nil then
  begin
    DebugLn('LyToMEITree: visiting non-empty LyObjectNode with FType: ');
    WriteLn(LyNode.FType);
    
    case LyNode.FType of
      ekStaff, ekLayer :
      begin
        MEINode := TMEIElement.Create;
        MEINode.SetFromLyObject(LyNode);
      end;
    end;

    { If we haven't made a new node yet, then we are skipping a parent node }
    if MEINode = nil then
    begin
      if LyNode.FChild <> nil then
        MEINode := LyToMEITree(LyNode.FChild, MEINode);
      if LyNode.FSibling <> nil then
        MEINode := LyToMEITree(LyNode.FSibling, MEINode);
    end
    else 
    begin
      if LyNode.FChild <> nil then
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
        
      if LyNode.FSibling <> nil then
        MEINode.FSibling := LyToMEITree(LyNode.FSibling, MEINode.FSibling);
    end;
  end;
  result := MEINode;
end;

{
var
  NewMEINode: TMEIElement = nil;
begin
  if LyNode <> nil then
  begin
    case LyNode.FType of
      ekStaff :
      begin
        NewMEINode := TMEIElement.Create;
        NewMEINode.SetFromLyObject(LyNode);
        
        if LyNode.FChild <> nil then
          NewMEINode.FChild := LyToMEITree(LyNode.FChild, NewMEINode.FChild);

        if LyNode.FSibling <> nil then
          NewMEINode.FSibling := LyToMEITree(LyNode.FSibling, NewMEINode.FSibling);
      end;

      ekLayer :
      begin
        NewMEINode := TMEIElement.Create;
        NewMEINode.SetFromLyObject(LyNode);
        
        if LyNode.FSibling <> nil then
          NewMEINode.FSibling := LyToMEITree(LyNode.FSibling, NewMEINode.FSibling);
      end;

      else
      begin
        if LyNode.FChild <> nil then
          NewMEINode := LyToMEITree(LyNode.FChild, NewMEINode);
        if LyNode.FSibling <> nil then
          NewMEINode := LyToMEITree(LyNode.FSibling, NewMEINode);
      end;
    end;

    if MEINode = nil then
      MEINode := NewMEINode
    else if NewMEINode <> nil then
    begin
      if MEINode.FType = NewMEINode.FType then
        MEINode.FSibling := NewMEINode
      else
        MEINode.FChild := NewMEINode;
    end;

  end; 

  result := MEINode;
end;
}
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
  Attributes: String;
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

    Attributes := XMLAttribute('n', IntToStr(Node.FNum));
    if Node.FType = ekStaff then
      Attributes := Attributes + ' ' + XMLAttribute('def', '#' + Node.FID)
    else if Node.FID <> '' then
      Attributes := Attributes + ' ' + XMLAttribute('xml:id', Node.FID);
    
    NewElement.EncloseInXML(Node.FName, Attributes);

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

    MEIStaffTree := LyToMEITree(LyObjectTree, MEIStaffTree);
    if MEIStaffTree <> nil then
    begin
      DebugLn('MEI TREE STAGE 1:' + LineEnding + MEIStaffTree.ToString);

      MEIMeasureTree := MEIStaffTree.StaffToMeasureTree;
    end;
    if MEIMeasureTree <> nil then
    begin
      DebugLn('MEI TREE STAGE 2:' + LineEnding + MEIMeasureTree.ToString);
      MEIScoreLines := MEIMeasureTree.ToMEI;
    end;
  end;

  if MEIScoreLines <> nil then
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


