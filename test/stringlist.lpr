{$mode objfpc}{$H+}{$J-}

program stringlist;

uses SysUtils, Classes, StringTools;

var
  InputLines: TStringList;
  OutputLines: TStringListPlus;
begin
  InputLines := TStringListPlus.Create;
  OutputLines := TStringListPlus.Create;
  InputLines.LoadFromFile('stringloop.txt');
  OutputLines.AssignAfterIndex(InputLines, 3);
  WriteLn(OutputLines.Text);
  FreeAndNil(InputLines);
  FreeAndNil(OutputLines);
end.
