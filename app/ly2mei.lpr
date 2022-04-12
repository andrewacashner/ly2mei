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
  LyTree: TLyObject = nil;
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
    Root := AddMeiScoreDef(Root, InputLines);
    { TODO
    Root := AddMeiScore(Root, InputLines); 
    }

    LyTree := CreateLyObjectTreeFromLy(InputLines);
    Root.AppendChild(LyTree.ToXMLAsIs);
    WriteMeiDocument(Root);
   
  finally
    FreeAndNil(LyTree);
    FreeAndNil(Root);
    FreeAndNil(InputLines);
  end;
end.

