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
  InputLines, OutputLines, MEIHeaderLines, MEIScoreLines, MEIMusicLines: TStringListAAC;
  ScoreInput: String;
  HeaderValues: THeader;
  CommandArg: TCommandArg;
  LyObjectTree: TLyObject;
begin
  InputLines     := TStringListAAC.Create;
  OutputLines    := TStringListAAC.Create;
  MEIHeaderLines := TStringListAAC.Create;
  MEIScoreLines  := TStringListAAC.Create;
  MEIMusicLines  := TStringListAAC.Create;
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
      InputLines.LoadFromFile(ParamStr(1));

    { Process macros: Find and cut defs, expand macro commands. }
    InputLines.RemoveComments;
    InputLines.RemoveBlankLines;
    InputLines := ExpandMacros(InputLines);

    { Process header, convert to MEI. }
    HeaderValues := ParseHeader(InputLines, HeaderValues);
    MEIHeaderLines := HeaderValues.ToMEI(MEIHeaderLines);

    { Process score, convert to MEI. }
    ScoreInput := LyArg(InputLines.Text, '\score');
    if not ScoreInput.IsEmpty then
    begin
      LyObjectTree := FindLyNewTree(ScoreInput, LyObjectTree);
      LyObjectTree := SetStaffNums(LyObjectTree);
      if LyObjectTree <> nil then
      begin
        MEIScoreLines := LyObjectTree.ToScoreDef(MEIScoreLines);
        // MEIMusicLines := LyObjectTree.ToMusic(MEIMusicLines);
        MEIScoreLines.AddStrings(MEIMusicLines);
      end;
    end;
    MEIScoreLines.EncloseInXML('score');
    MEIScoreLines.EncloseInXML('mdiv');
    MEIScoreLines.EncloseInXML('body');
    MEIScoreLines.EncloseInXML('music');


    { Write output. }
    OutputLines.Clear;
    OutputLines.Assign(MEIHeaderLines);
    OutputLines.AddStrings(MEIScoreLines);

    OutputLines.EncloseInXML('mei', MEINamespace);
    OutputLines.Insert(0, XMLversion);

    WriteLn(OutputLines.Text);

  finally
    FreeAndNil(LyObjectTree);
    FreeAndNil(CommandArg);
    FreeAndNil(HeaderValues);
    FreeAndNil(MEIMusicLines);
    FreeAndNil(MEIScoreLines);
    FreeAndNil(MEIHeaderLines);
    FreeAndNil(OutputLines);
    FreeAndNil(InputLines);
  end;
end.

