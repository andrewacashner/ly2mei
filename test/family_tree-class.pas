{ `Family Tree`

  Test of implementing an LCRS tree with a CLASS

  Andrew Cashner, 2021/11/04
}

{ TODO make this into a generic tree that could be used with any class }
{ OR do the same with nested lists? }

{$mode objfpc}{$H+}{$J-}

program FamilyTree(output);

uses SysUtils, Classes;

type 
  TBio = class
  private
    var
      FFirstName, FLastName: String;
      FBirthDate, FDeathDate: Integer;
      FChild: TBio;
      FSibling: TBio;
  public
    constructor Create(First, Last: String; Birth, Death: Integer; 
      Child, Sib: TBio); 
    function ToString: String; override;
  end;

constructor TBio.Create(First, Last: String; Birth, Death: Integer;
  Child, Sib: TBio);
begin
  FFirstName  := First;
  FLastName   := Last;
  FBirthDate  := Birth;
  FDeathDate  := Death;
  FChild      := Child;
  FSibling    := Sib;
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

function NewPerson(First, Last: String; Birth, Death: Integer): TBio;
begin
  result := TBio.Create(First, Last, Birth, Death, nil, nil);
end;

function AddChildSib(Parent, Child, Sibling: TBio): TBio;
begin
  assert(Parent <> nil);
  Parent.FChild := Child;
  Parent.FSibling := Sibling;
  result := Parent; 
end;

function FamilyTreeToString(Parent: TBio; Generation: Integer): String;
var
  Indent: String;
begin
  if Parent <> nil then
  begin
    Indent := StringOfChar(' ', 2 * Generation);
    result := Indent + Parent.ToString + LineEnding 
              + FamilyTreeToString(Parent.FChild, Generation + 1) 
              + FamilyTreeToString(Parent.FSibling, Generation);
  end;
end;

procedure FreeTree(Tree: TBio);
begin
  if Tree <> nil then
  begin
    FreeTree(Tree.FChild);
    FreeTree(Tree.FSibling);
    FreeAndNil(Tree);
  end;
end;

{ MAIN }
var
  Cashners: TBio = nil;
begin
  Cashners := 
    AddChildSib(NewPerson('Ray', 'Cashner', 1925, 1975),
      AddChildSib(NewPerson('Randall', 'Cashner', 1950, -1),
        AddChildSib(NewPerson('Matthew', 'Cashner', 1978, -1),
          nil,
          AddChildSib(NewPerson('Andrew', 'Cashner', 1981, -1),
            AddChildSib(NewPerson('Benjamin', 'Cashner', 2011, -1),
              nil,
              NewPerson('Joy', 'Cashner', 2014, -1)),
            nil)),
        AddChildSib(NewPerson('Terrence', 'Cashner', 1952, -1),
          AddChildSib(NewPerson('Kyle', 'Cashner', 1975, -1),
            NewPerson('Little Kyle', 'Cashner', 1990, -1),
            NewPerson('Darchelle', 'Cashner', 1977, -1)),
          nil)),
      nil);
    
  Write(FamilyTreeToString(Cashners, 0));
  FreeTree(Cashners);

  { second method }
  Cashners := nil;
  Cashners :=
    TBio.Create('Ray', 'Cashner', 1925, 1975, 
      TBio.Create('Randall', 'Cashner', 1950, -1,
        TBio.Create('Matthew', 'Cashner', 1978, -1, 
          nil,
          TBio.Create('Andrew', 'Cashner', 1981, -1, 
            TBio.Create('Benjamin', 'Cashner', 2011, -1, nil,
              TBio.Create('Joy', 'Cashner', 2014, -1, nil, nil)),
            nil)),
        TBio.Create('Terrence', 'Cashner', 1952, -1,
          TBio.Create('Kyle', 'Cashner', 1975 , -1,
            TBio.Create('Little Kyle', 'Cashner', 1990, -1, nil, nil),
            TBio.Create('Darchelle', 'Cashner', 1977, -1, nil, nil)),
          nil)),
      nil);
  
  Write(FamilyTreeToString(Cashners, 0));
  FreeTree(Cashners);
end.
