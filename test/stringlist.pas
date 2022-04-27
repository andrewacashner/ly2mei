{$mode objfpc}{$H+}{$J-}

program stringlist;

uses SysUtils, StringTools;

var
  InputLines: TStringListPlus;
begin
  InputLines := TStringListPlus.Create;
  InputLines.LoadFromFile('stringloop.txt');
  InputLines.DropBeforeIndex(3);
  WriteLn(InputLines.Text);
  FreeAndNil(InputLines);
end.
