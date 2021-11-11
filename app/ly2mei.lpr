{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} {$OPTIMIZATION tailrec}

{ @abstract(@code(ly2mei), A converter from Lilypond to MEI-XML)
  @author(Andrew Cashner, <andrewacashner@gmail.com>)
}
program ly2mei(input, output, stderr);

uses SysUtils, Classes, StringTools, Outline, Macro, Header, ScoreTree;

{ MAIN }
const
  XMLversion = '<?xml version="1.0" encoding="UTF-8"?>';
  MeiNamespace = 'xmlns="http://www.music-encoding.org/ns/mei" meiVersion="4.0.0"';
var
  InputText, OutputText, MEIHeaderLines, MEIScoreLines, MEIMusicLines: TStringList;
  ScoreInput: String;
  HeaderValues: THeader;
  CommandArg: TCommandArg;
  LyObjectTree: TLyObject;
begin
  InputText      := TStringList.Create;
  OutputText     := TStringList.Create;
  MEIHeaderLines := TStringList.Create;
  MEIScoreLines  := TStringList.Create;
  MEIMusicLines  := TStringList.Create;
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
    ScoreInput := LyArg(InputText.Text, '\score');
    if not ScoreInput.IsEmpty then
    begin
      LyObjectTree := FindLyNewTree(ScoreInput, LyObjectTree);
      LyObjectTree := SetStaffNums(LyObjectTree);
      if LyObjectTree <> nil then
      begin
        MEIScoreLines := LyObjectTree.ToScoreDef(MEIScoreLines);
        MEIMusicLines := LyObjectTree.ToMusic(MEIMusicLines);
        MEIScoreLines.AddStrings(MEIMusicLines);
      end;
    end;
    MEIScoreLines := XMLElementLines(MEIScoreLines, 'score');
    MEIScoreLines := XMLElementLines(MEIScoreLines, 'mdiv');
    MEIScoreLines := XMLElementLines(MEIScoreLines, 'body');
    MEIScoreLines := XMLElementLines(MEIScoreLines, 'music');


    { Write output. }
    OutputText.Clear;
    OutputText.Assign(MEIHeaderLines);
    OutputText.AddStrings(MEIScoreLines);

    OutputText := XMLElementLines(OutputText, 'mei', MEINamespace);
    OutputText.Insert(0, XMLversion);

    WriteLn(OutputText.Text);

  finally
    FreeAndNil(LyObjectTree);
    FreeAndNil(CommandArg);
    FreeAndNil(HeaderValues);
    FreeAndNil(MEIMusicLines);
    FreeAndNil(MEIScoreLines);
    FreeAndNil(MEIHeaderLines);
    FreeAndNil(OutputText);
    FreeAndNil(InputText);
  end;
end.

