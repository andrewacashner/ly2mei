{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}
{ uggh, don't do this }
{ test objects for pitches, 2021/11/10 }
program PitchObjects;

uses SysUtils, Classes;

type
  TPitchKind = (pkC, pkD, pkE, pkF, pkG, pkA, pkB, pkRest);
const
  PitchNames: String = 'cdefgabr';

function ToEnum(Name: Char): TPitchKind;
begin
  result := TPitchKind(PitchNames.IndexOf(Name) + 1);
end;

function FromEnum(Kind: TPitchKind): Char;
begin
  result := PitchNames[Ord(Kind)];
end;

type
  TAccidentalKind = (akNatural, akFlat, akSharp);
const
  Accidentals = 'nfs';

function ToEnum(Name: Char): TAccidentalKind;
begin
  result := TAccidentalKind(Accidentals.IndexOf(Name) + 1);
end;

function FromEnum(Kind: TAccidentalKind): Char;
begin
  result := Accidentals[Ord(Kind)];
end;


type
  TPitch = class
  private
    var
      FPname: TPitchKind;
      FAccid: TAccidentalKind;
  public
    function ToMEI: String;
  end;

function TPitch.ToMEI: String;
begin
  result := '<note pname="' + FromEnum(FPname) 
              + ' accid = "' + FromEnum(FAccid) + '"/>';
end;

var
  Pitch: TPitch;
begin
  Pitch := TPitch.Create;
  Pitch.FPname := ToEnum('d');
  Pitch.FAccid := ToEnum('s');
  WriteLn(Pitch.ToMEI);
  FreeAndNil(Pitch);
end.


