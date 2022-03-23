{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} {$OPTIMIZATION tailrec}

{ @abstract(@code(ly2mei), A converter from Lilypond to MEI-XML)
  @author(Andrew Cashner, <andrewacashner@gmail.com>)
}
program ly2mei(input, output, stderr);

uses SysUtils, Classes, StringTools, Macro, Header, MEI;

{ MAIN }
var
  InputLines: TStringListAAC;
  Root, MeiHead: TMeiNode;
begin
  InputLines  := TStringListAAC.Create;
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
  
    Root := TMeiNode.CreateMeiRoot();
    MeiHead := CreateMeiHeadFromLy(InputLines);
    if Assigned(MeiHead) then
      Root.AppendChild(MeiHead);

    WriteMeiDocument(Root);

  finally
    FreeAndNil(Root);
    FreeAndNil(InputLines);
  end;
end.

