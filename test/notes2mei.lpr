{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}
program Notes2MEI;

uses SysUtils, Classes, MusicNotes;

var
  InputLines, OutputLines: TStringList;
begin
  InputLines := TStringList.Create;
  OutputLines := TStringList.Create;
  try
    InputLines.Add('  | c''4 d''4 es''4. d''8');
    InputLines.Add('  | g''''2( aes''''2)');
    InputLines.Add('  | g''''4 fis''''4 g''''8 g''8');
    InputLines.Add('  | g1');
    InputLines.Add('  | gis\breve');

    OutputLines := LyMeasuresToMEI(InputLines, OutputLines); 

  finally
    Write(OutputLines.Text);
    FreeAndNil(OutputLines);
    FreeAndNil(InputLines);
  end;
end.


