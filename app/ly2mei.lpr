{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} {$OPTIMIZATION tailrec}

{ @abstract(@code(ly2mei), A converter from Lilypond to MEI-XML)
  @author(Andrew Cashner, <andrewacashner@gmail.com>)
}
program ly2mei(input, output, stderr);

uses SysUtils, Classes, StringTools, Outline, Macro, Header, ScoreTree;

{ MAIN }
const
  XMLversion = '<?xml version="1.0" encoding="UTF-8"?>';
  MeiNamespace = 'xmlns="http://www.music-encoding.org/ns/mei"';
var
  InputText, TempText, OutputText, MEIHeaderLines, MEIScoreLines: TStringList;
  ScoreInput: String;
  HeaderValues: THeader;
  CommandArg: TCommandArg;
  LyObjectTree: TLyObject;
begin
  InputText      := TStringList.Create;
  TempText       := TStringList.Create;
  OutputText     := TStringList.Create;
  MEIHeaderLines := TStringList.Create;
  MEIScoreLines  := TStringList.Create;
  HeaderValues   := THeader.Create;
  CommandArg     := TCommandArg.Create;
  LyObjectTree   := nil;

  try
    if ParamCount <> 1 then
    begin
      WriteLn(stderr, 'Usage: header INFILE.ly');
      exit;
    end
    else
      InputText.LoadFromFile(ParamStr(1));

    { Process macros: Find and cut defs, expand macro commands. }
    InputText := RemoveComments(InputText);
    InputText := RemoveBlankLines(InputText);
    InputText := ExpandMacros(InputText);

    { Process header, convert to MEI. }
    HeaderValues := ParseHeader(InputText, HeaderValues);
    MEIHeaderLines := HeaderValues.ToMEI(MEIHeaderLines);

    { Process score, convert to MEI. }
    { TODO for now, convert to DIY XML. }
    ScoreInput := LyArg(InputText.Text, '\score');
    OutputText.Clear;
    if not ScoreInput.IsEmpty then
    begin
      LyObjectTree := FindLyNewTree(ScoreInput, LyObjectTree);
      if LyObjectTree <> nil then
        TempText := Lines(LyObjectTree.ToString, TempText);
    end;
    MEIScoreLines := XMLElementLines(TempText, MEIScoreLines, 'score');

    { Write output. }
    TempText.Clear;
    TempText.Assign(MEIHeaderLines);
    TempText.AddStrings(MEIScoreLines);

    OutputText := XMLElementLines(TempText, OutputText, 'mei', MEINamespace);
    OutputText.Insert(0, XMLversion);

    WriteLn(OutputText.Text);

  finally
    FreeAndNil(LyObjectTree);
    FreeAndNil(CommandArg);
    FreeAndNil(HeaderValues);
    FreeAndNil(MEIHeaderLines);
    FreeAndNil(MEIScoreLines);
    FreeAndNil(TempText);
    FreeAndNil(OutputText);
    FreeAndNil(InputText);
  end;
end.

