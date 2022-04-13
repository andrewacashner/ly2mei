{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(@code(ly2mei), A converter from Lilypond to MEI-XML)
  @author(Andrew Cashner, <andrewacashner@gmail.com>)
}
program ly2mei(input, output, stderr);

uses SysUtils, Classes, StringTools, Macro, MEI, Header, ScoreTree;

{ MAIN }
var
  InputLines: TStringListAAC;
  Root: TMeiNode;
begin
  InputLines  := TStringListAAC.Create;
  Root := TMeiNode.CreateMeiRoot();

  try
    if ParamCount <> 1 then
    begin
      WriteLn(stderr, 'Usage: ly2mei INFILE.ly');
      exit;
    end;
    
    InputLines.LoadFromFile(ParamStr(1));
    InputLines := ExpandMacros(InputLines);
    Root := AddMeiHead(Root, InputLines);
    Root := AddMeiScore(Root, InputLines); 
    
    WriteMeiDocument(Root);
   
  finally
    FreeAndNil(Root);
    FreeAndNil(InputLines);
  end;
end.

