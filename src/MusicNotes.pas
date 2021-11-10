{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(Convert a Lilypond music expression to MEI.)
  @author(Andrew Cashner)
}
unit MusicNotes;

interface

uses SysUtils, StrUtils, Classes, StringTools;


implementation

type 

  PitchKind = (pkC, pkD, pkE, pkF, pkG, pkA, pkB, pkRest);
  AccidentalKind = (akNatural, akFlat, akSharp);
  DurationKind = (dkBreve, dkSemibreve, dkMinim, dkSemiminim, dkFusa, dkSemifusa, 
    dkBreveDotted, dkSemibreveDotted, dkMinimDotted, dkSemiminimDotted, dkFusaDotted);

  TPitch = class
  public
    var
      FPitchName: PitchKind;
      FAccid: AccidentalKind;
      FOct: Integer;
      FDur: DurationKind;
    function ToMEI: String; 
  end;

function LyToPitches(Source: String): String;
var
  MEI, ThisNote, NoteTemp: String;
  PitchNameLy, OctLy, DurLy, EtcLy: String;
  PitchNameMEI, OctMEI, DurMEI, EtcMEI, AccidMEI: String;
  Notes: Array of String;
  NewPitch: TPitch;
begin
  SetLength(Notes, 0);
  SetLength(NoteParts, 0);
  MEI := StringDropBefore(Source.TrimLeft, '| ');
  Notes := MEI.Split([' ']);
  for ThisNote in Notes do
  begin
    NoteTemp := ThisNote;
    PitchNameLy := ExtractWord(1, NoteTemp, [',', '''', '1', '2', '4', '8', '\']);
    NoteTemp := StringDropBefore(ThisNote, PitchNameLy);
    
    OctLy := ExtractWord(1, NoteTemp, ['1', '2', '4', '8', '\']);
    if OctLy <> '' then
      NoteTemp := StringDropBefore(NoteTemp, OctLy);
   
    DurLy := ExtractWord(1, NoteTemp, '(', ')', '~', '\');

    if DurLy <> '' then
    begin
      NoteTemp := StringDropBefore(NoteTemp, DurLy);
      EtcLy := NoteTemp;
    end;

    NewPitch := TPitch.CreateFromLy(PitchNameLy, OctLy, DurLy);
    { TODO EtcLy? }

    { START add to list of pitches}

    { Move below to TPitch.CreateFromLy }
    { In separate function convert pitches to MEI, TPitch.ToMei }
    if PitchNameLy.EndsWith('is') then
      NewPitch.Accid := akSharp
    else if PitchNameLy.EndsWith('es') then
      NewPitch.Accid := akFlat
    else
      NewPitch.Accid := akNatural;
    
    case PitchNameLy[0] of
      'c': NewPitch.PitchName := pkC;
      'd': NewPitch.PitchName := pkD;
      'e': NewPitch.PitchName := pkE;
      'f': NewPitch.PitchName := pkF;
      'g': NewPitch.PitchName := pkG;
      'a': NewPitch.PitchName := pkA;
      'b': NewPitch.PitchName := pkB;
      'r': NewPitch.PitchName := pkRest;
    else
      WriteLn(Stderr, 'Could not extract pitch from input "' + PitchNameLy + '"');
    end;

    case OctLy of
      ',,,'     : NewPitch.Oct := 0;
      ',,'      : NewPitch.Oct := 1;
      ','       : NewPitch.Oct := 2;
      ''        : NewPitch.Oct := 3;
      ''''      : NewPitch.Oct := 4; { '' }
      ''''''    : NewPitch.Oct := 5; { ''' }
      ''''''''  : NewPitch.Oct := 6; { '''' }
    else
      WriteLn(Stderr, 'Could not extract octave from input "' + OctLy + '"');
    end;

    case DurLy of
      '\breve'  : NewPitch.Dur := dkBreve;
      '1'       : NewPitch.Dur := dkSemibreve;
      '2'       : NewPitch.Dur := dkMinim;
      '4'       : NewPitch.Dur := dkSemiminim;
      '8'       : NewPitch.Dur := dkFusa;
      '16'      : NewPitch.Dur := dkSemifusa;
      '\breve.' : NewPitch.Dur := dkBreveDotted;
      '1.'      : NewPitch.Dur := dkSemibreveDotted;
      '2.'      : NewPitch.Dur := dkMinimDotted;
      '4.'      : NewPitch.Dur := dkSemiminimDotted;
      '8.'      : NewPitch.Dur := dkFusaDotted;
    else
      WriteLn(Stderr, 'Could not extract duration from input "' + DurLy + '"');
    end;

  end;
  
  result := MEI;
end;

function LyCmdToMEI(LyInput, MEIOutput: TStringList): TStringList;
var
  ThisString, MEIString: String;
begin
  assert(LyInput <> nil);
  assert(MEIOutput <> nil);
  MEIOutput.Clear;

  for ThisString in LyInput do
  begin
    if ThisString.TrimLeft.StartsWith('|') and (ThisString.CountChar('|') = 1) then
      MEIString := LyToPitches(ThisString);
    { TODO make pitch list, then convert to MEI }

    MEIOutput.Add(MEIString);
  end;

  result := MEIOutput;
end;

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


end.
