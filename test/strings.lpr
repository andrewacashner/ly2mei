{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

program strings(output);

uses SysUtils, StringTools;

var
  InputText: String = '\key c\major';
  OutputText: String = '';
begin
  OutputText := StringDropBefore(InputText, '\key ');
  OutputText := StringDropAfter(OutputText, '\major');
  OutputText := StringDropAfter(OutputText, '\m').Trim;
  WriteLn(OutputText);
end.

  
