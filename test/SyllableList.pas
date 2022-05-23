{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

unit SyllableList;

interface

uses SysUtils, Classes, Generics.Collections, MEI;

type

  TSyllablePosition = (skSingle, skBeginning, skMiddle, skEnd);

  TSyllable = class 
  private
    var
      FText: String;
      FPosition: TSyllablePosition;
  public
    constructor Create();
    constructor Create(SylText: String; SylPosition: TSyllablePosition =
      skSingle);

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

implementation

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

function TSyllable.ToMEI: TMeiNode;
var
  Syl: TMeiNode;
  WordPos: String;
begin
  Syl := TMeiNode.Create('syl');
  Syl.TextNode := SylText;

  if SylPosition > skSingle then
  begin
    case SylPosition of
      skBeginning : WordPos := 'i';
      skMiddle    : WordPos := 'm';
      skEnd       : WordPos := 'f';
    end;
    Syl.AddAttribute('con', 'd');
    Syl.AddAttribute('wordpos', WordPos);
  end;
  result := Syl;
end;

constructor TSyllableList.Create();
begin
  inherited Create();
end;

{ TODO should there be a syllablelist in each measurelist, or what? }
{ given input of \lyrics expression }
constructor TSyllableList.Create(LyInput: String);
var
  TokenizedInput: TStringArray;
  ThisString: String;
  NewSyllable: TSyllable;
begin
  inherited Create();

  TokenizedInput := LyInput.Split([' ', LineEnding]);

  for ThisString in TokenizedInput do
  begin
    if (Self.Count > 0) and (ThisString = '--') then
    begin
      case Last.SylPosition of
        skSingle : Last.SylPosition := skBeginning;
        skEnd    : Last.SylPosition := skMiddle;
      end;
    end
    else
    begin
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

end.
