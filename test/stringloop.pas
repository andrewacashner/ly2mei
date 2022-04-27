{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} 
program stringloop;

uses SysUtils, Classes, Generics.Collections;

type 
  TIndexList = specialize TList<Integer>;

var
  InputLines: TStringList;
  InputStr, ThisLine: String;
  DefLineIndexList: TIndexList;
  LineIndex: Integer;
begin
  DefLineIndexList := TIndexList.Create;
  InputLines := TStringList.Create;
  InputLines.LoadFromFile('stringloop.txt');
  LineIndex := 0;
  for ThisLine in InputLines do
  begin
    if (not ThisLine.StartsWith(' ')) and ThisLine.Contains(' = ') then
    begin
      DefLineIndexList.Add(LineIndex);
    end;
    Inc(LineIndex);
  end;
  for LineIndex in DefLineIndexList do
  begin
    WriteLn(InputLines[LineIndex]);
  end;

  FreeAndNil(InputLines);
  FreeAndNil(DefLineIndexList);
end.
