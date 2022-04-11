{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(@code(ly2mei), A converter from Lilypond to MEI-XML)
  @author(Andrew Cashner, <andrewacashner@gmail.com>)
}
program ly2mei(input, output, stderr);

uses SysUtils, Classes, StringTools, Macro, MEI, Header, ScoreTree, MusicNotes;

{ MAIN }
var
  InputLines: TStringListAAC;
  Root: TMeiNode;
  LyTree: TLyObject = nil;
  {
  Branch: TMeiNode;
  Layer: TLirioVoice;
  Measure: TPitchList;
  }
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
    Root := ParseLyMusic(Root);
    WriteMeiDocument(Root);
   
    {
    Branch := Root.FindElementByName('layer');
    Layer := Branch.ChildTree;
    Measure := Layer.GetMeasure(1);
    WriteLn(Measure.ToMEI);
    }

  finally
    FreeAndNil(LyTree);
    FreeAndNil(Root);
    FreeAndNil(InputLines);
  end;
end.

