{ `Family Tree`

  Test of implementing an LCRS tree as a nested list of a class

  DOES NOT WORK - no way to make links between generations

  Andrew Cashner, 2021/11/04
}

{$mode objfpc}{$H+}{$J-}

program FamilyTree(output);

uses SysUtils, Classes, Generics.Collections;

type 
  TBio = class
  private
    var
      FFirstName, FLastName: String;
      FBirthDate, FDeathDate: Integer;
  public
    constructor Create(First, Last: String; Birth, Death: Integer);
    function ToString: String; override;
  end;

constructor TBio.Create(First, Last: String; Birth, Death: Integer);
begin
  FFirstName  := First;
  FLastName   := Last;
  FBirthDate  := Birth;
  FDeathDate  := Death;
end;

function TBio.ToString: String;
  function YearToString(Date: Integer): String;
  begin
    if Date = -1 then
      result := '*'
    else
      result := IntToStr(Date);
  end;
var
  DateString: String;
begin
  DateString := '(' + YearToString(FBirthDate) + '-' 
                + YearToString(FDeathDate) + ')'; 
  result :=  FFirstName + ' ' + FLastName + ' ' + DateString;
end;

type
  TBioList = specialize TObjectList<TBio>;
  TFamilyTree = specialize TList<TBioList>;


{ MAIN }
var
  CashnersRochester: TBioList;
  CashnersRichmond: TBioList;
  Cashners: TFamilyTree;
begin
  CashnersRochester := TBioList.Create;
  CashnersRichmond := TBioList.Create;
  Cashners := TFamilyTree.Create;

  try
    CashnersRochester.Add(TBio.Create('Benjamin', 'Cashner', 2011, -1));
    CashnersRochester.Add(TBio.Create('Joy', 'Cashner', 2014, -1));

    CashnersRichmond.Add(TBio.Create('Matthew', 'Cashner', 1978, -1));
    CashnersRichmond.Add(TBio.Create('Andrew', 'Cashner', 1981, -1));

    Cashners.Add(CashnersRochester);
    Cashners.Add(CashnersRichmond);

  finally
    FreeAndNil(Cashners);
    FreeAndNil(CashnersRichmond);
    FreeAndNil(CashnersRochester);
  end;
end.
