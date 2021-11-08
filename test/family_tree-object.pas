{ `Family Tree`

  Test of implementing an LCRS tree with objects

  Andrew Cashner, 2021/11/03
}

{$mode objfpc}{$H+}{$J-}

program FamilyTree(output);

uses SysUtils, Classes;

type 
  TPBio = ^TBio;

  TBio = object
  private
    var
      FFirstName, FLastName: String;
      FBirthDate, FDeathDate: Integer;
      FChild: TPBio;
      FSibling: TPBio;
  public
    constructor Create(First, Last: String; Birth, Death: Integer; Child, Sib:
      TPBio); 
    destructor Destroy;
    function DateString: String;
    function ToString: String; 
  end;

constructor TBio.Create(First, Last: String; Birth, Death: Integer; Child,
  Sib: TPBio); 
begin
  FFirstName  := First;
  FLastName   := Last;
  FBirthDate  := Birth;
  FDeathDate  := Death;
  FChild      := Child;
  FSibling    := Sib;
end;

destructor TBio.Destroy;
begin
  inherited;
end;

function TBio.DateString: String;
  function SingleDateToString(Date: Integer): String;
  begin
    if Date = -1 then
      result := '*'
    else
      result := IntToStr(Date);
  end;
begin
  result := '(' + SingleDateToString(FBirthDate) + '-' +
            SingleDateToString(FDeathDate) + ')'; 
end;

function TBio.ToString: String;
begin
  result :=  FFirstName + ' ' + FLastName + ' ' + Self.DateString;
end;

function NewPerson(First, Last: String; Birth, Death: Integer): TPBio;
var
  Person: TPBio;
begin
  new(Person, Create(First, Last, Birth, Death, nil, nil));
  result := Person;
end;

function AddChildSib(Parent, Child, Sibling: TPBio): TPBio;
begin
  assert(Parent <> nil);
  Parent^.FChild := Child;
  Parent^.FSibling := Sibling;
  result := Parent;
end;

function FamilyTreeToString(Parent: TPBio; Generation: Integer): String;
var
  Indent: String;
begin
  if Parent <> nil then
  begin
    Indent := StringOfChar(' ', 2 * Generation);
    result := Indent + Parent^.ToString + LineEnding 
              + FamilyTreeToString(Parent^.FChild, Generation + 1) 
              + FamilyTreeToString(Parent^.FSibling, Generation);
  end;
end;

procedure FreeTree(Tree: TPBio);
begin
  if Tree <> nil then
  begin
    FreeTree(Tree^.FChild);
    FreeTree(Tree^.FSibling);
    dispose(Tree, Destroy);
  end;
end;

{ MAIN }
var
  Cashners: TPBio = nil;
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
end.
