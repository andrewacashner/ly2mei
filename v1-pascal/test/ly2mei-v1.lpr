{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} {$OPTIMIZATION tailrec}

{ @abstract(@code(ly2mei), A converter from Lilypond to MEI-XML)
  @author(Andrew Cashner, <andrewacashner@gmail.com>)
}
program ly2mei(input, output, stderr);

uses SysUtils, Classes, StringTools, Macro, Header, MusicNotes;

{ MAIN }
var
  InputLines, HeaderLines, ScoreLines, OutputLines: TStringListAAC;
begin
  InputLines  := TStringListAAC.Create;
  OutputLines := TStringListAAC.Create;
  try
    if ParamCount <> 1 then
    begin
      WriteLn(stderr, 'Usage: ly2mei INFILE.ly');
      exit;
    end
    else
      InputLines.LoadFromFile(ParamStr(1));

    InputLines  := ExpandMacros(InputLines);
    InputLines  := ExpandMultiRests(InputLines);

    HeaderLines := CreateMEIHeader(InputLines);
    ScoreLines  := CreateMEIMusic(InputLines);

    OutputLines.AddStrings(HeaderLines);
    OutputLines.AddStrings(ScoreLines);
    Write(OutputLines.MEIDocStr);

  finally
    FreeAndNil(ScoreLines);
    FreeAndNil(HeaderLines);
    FreeAndNil(OutputLines);
    FreeAndNil(InputLines);
  end;
end.

