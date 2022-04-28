{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(@code(ly2mei), A converter from Lilypond to MEI-XML)
  @author(Andrew Cashner, <andrewacashner@gmail.com>)
}
program ly2mei(input, output, stderr);

uses SysUtils, Classes, StringTools, Macro, MEI, Header, ScoreTree;

{ MAIN }
var
  InputLines, OutputLines: TStringListPlus;
  Root: TMeiNode;
  OutputStr: String;
begin
  InputLines  := TStringListPlus.Create;
  Root := TMeiNode.CreateMeiRoot();

  try
    if (ParamCount < 1) or (ParamCount > 2) then
    begin
      WriteLn(stderr, 'Usage: ly2mei INFILE.ly [OUTFILE.mei]');
      exit;
    end;
    
    InputLines.LoadFromFile(ParamStr(1));
    InputLines := ExpandMacros(InputLines);
    Root := AddMeiHead(Root, InputLines);
    Root := AddMeiScore(Root, InputLines); 
    OutputStr := MeiDocString(Root);
   
    if ParamCount = 2 then
    begin
      OutputLines := TStringListPlus.Create(OutputStr);
      OutputLines.SaveToFile(ParamStr(2));
    end
    else
      WriteLn(OutputStr);
   
  finally
    FreeAndNil(InputLines);
    FreeAndNil(OutputLines);
    FreeAndNil(Root);
  end;
end.

