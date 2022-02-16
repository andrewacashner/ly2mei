{$mode objfpc}{$H+}{$J-}
program FindCommand(output);

uses SysUtils, StrUtils, Classes;

function StringDropBefore(InputStr: String; Delim: String): String;
var
  BreakPoint: Integer;
begin
  if InputStr.Contains(Delim) then
  begin
    BreakPoint := InputStr.IndexOf(Delim);
    InputStr := InputStr.Substring(BreakPoint, Length(InputStr) - BreakPoint);
  end;
  result := InputStr;
end;

{ `CensorString`

  Remove the first instance of a substring from a string }
function ReplaceString(Source, Cut, Add: String): String;
var
  CutFrom, CutTo: Integer;
begin
  CutFrom := Source.IndexOf(Cut);

  if CutFrom = -1 then
    result := Source { no substring found }
  else
  begin
    CutTo := CutFrom + Length(Cut) + 1;
    result := Source.Substring(0, CutFrom) + Add + Source.Substring(CutTo);
  end;
end;

var 
  InputText: TStringList;
  BufferStr, Command, AfterCommand: String;
  OutputStr: String;
begin
  InputText := TStringList.Create();
  try
    InputText.LoadFromFile('commandtest.ly');
    OutputStr := InputText.Text;
    BufferStr := OutputStr;
    while BufferStr.Contains('\') do
    begin
      BufferStr := StringDropBefore(BufferStr, '\');
      Command := ExtractWord(1, BufferStr, [' ', LineEnding]);
      WriteLn(Command);
      BufferStr := BufferStr.Substring(Length(Command));
      AfterCommand := BufferStr[1];
      OutputStr := ReplaceString(OutputStr, Command, '[Command deleted]' + AfterCommand);
    end;
    WriteLn(OutputStr);
  finally
    FreeAndNil(InputText);
  end;
end.
