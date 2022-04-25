{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(@code(ly2mei), A converter from Lilypond to MEI-XML)
  @author(Andrew Cashner, <andrewacashner@gmail.com>)
}
program ly2mei(input, output, stderr);

uses SysUtils, Classes, StringTools, Macro, MEI, Header, ScoreTree;

{ MAIN }
var
  InputLines, ExpandedMacros: TStringList;
  InputStr: String;
  Root: TMeiNode;
begin
  InputLines  := TStringList.Create;
  Root := TMeiNode.CreateMeiRoot();

  try
    if ParamCount <> 1 then
    begin
      WriteLn(stderr, 'Usage: ly2mei INFILE.ly');
      exit;
    end;
    
    InputLines.LoadFromFile(ParamStr(1));
    InputStr := InputLines.Text;
    InputStr := ExpandMacros(InputStr);
    ExpandedMacros := Lines(InputStr); { TODO for now }
    Root := AddMeiHead(Root, ExpandedMacros);
    Root := AddMeiScore(Root, ExpandedMacros); 
    
    WriteMeiDocument(Root);
   
  finally
    FreeAndNil(InputLines);
    FreeAndNil(ExpandedMacros);
    FreeAndNil(Root);
  end;
end.

