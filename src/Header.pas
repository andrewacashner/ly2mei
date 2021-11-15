{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} {$OPTIMIZATION tailrec}

{ @abstract(Parse a Lilypond header and convert it to MEI)
  @author(Andrew Cashner) }
unit Header;

interface

uses SysUtils, Classes, StringTools, Outline;

const ProgramName: String = 'ly2mei';

{ This record stores all the fields required in the Lilypond source file and
  can write them to MEI.  }
type
  THeader = record
    FTitle, FSubtitle, FComposer, FDates, 
    FPoet, FEditor, FCopyright, FSource: String;
    FValid: Boolean;
  end;

{ Return a new string list containing the MEI expression of the header data. }
function NewMEIFromHeader(Header: THeader): TStringListAAC;

{ Find a header definition and parse it into a @link(THeader) record. }
function ParseHeader(InputText: TStringListAAC): THeader;


implementation

function NewMEIFromHeader(Header: THeader): TStringListAAC;
var
  MEI: TStringListAAC;
begin
  MEI := TStringListAAC.Create;
  if not Header.FValid then
  begin
    MEI.Add('<meiHead>');
    MEI.Add('</meiHead>');
    result := MEI;
    exit;
  end;

  with Header do
  begin
    MEI.Add('<meiHead>');
    MEI.Add(IndentStr(1) + '<fileDesc>');
    MEI.Add(IndentStr(2) + '<titleStmt>');
    MEI.Add(IndentStr(3) + '<title type="main">' + FTitle + '</title>');

    if not FSubtitle.IsEmpty then
      MEI.Add(IndentStr(3) + '<title type="subtitle">' + FSubtitle + '</title>');

    MEI.Add(IndentStr(3) + '<respStmt>');

    if not FDates.IsEmpty then
      MEI.Add(IndentStr(4) + '<composer>' + FComposer + ' ' + FDates + '</composer>')
    else
      MEI.Add(IndentStr(4) + '<composer>' + FComposer + '</composer>');

    if not FPoet.IsEmpty then
      MEI.Add(IndentStr(4) + '<lyricist>' + FPoet + '</lyricist>');

    if not FEditor.IsEmpty then
      MEI.Add(IndentStr(4) + '<editor>' + FEditor + '</editor>');

    MEI.Add(IndentStr(3) + '</respStmt>');
    MEI.Add(IndentStr(2) + '</titleStmt>');
    
    if not FEditor.IsEmpty then
    begin
      MEI.Add(IndentStr(2) + '<editionStmt>');
      MEI.Add(IndentStr(3) + '<respStmt><p>Edited by ' + FEditor + '</p></respStmt>');
      MEI.Add(IndentStr(2) + '</editionStmt>');
    end;

    if not FCopyright.IsEmpty then
    begin
      MEI.Add(IndentStr(2) + '<pubStmt>');
      MEI.Add(IndentStr(3) + '<availability><p>' + FCopyright + '</p></availability>');
      MEI.Add(IndentStr(2) + '</pubStmt>');
    end;

    MEI.Add(IndentStr(1) + '</fileDesc>');
    MEI.Add(IndentStr(1) + '<encodingDesc>');
    MEI.Add(IndentStr(2) + '<appInfo>');
    MEI.Add(IndentStr(3) + '<application><name>' + ProgramName + '</name></application>');
    MEI.Add(IndentStr(2) + '</appInfo>');
    MEI.Add(IndentStr(1) + '</encodingDesc>');
    
    if not FSource.IsEmpty then
      MEI.Add(IndentStr(1) + '<sourceDesc><source>' + FSource + '</source></sourceDesc>');

    MEI.Add('</meiHead>'); 
  end;
  result := MEI;
end;

function ParseHeader(InputText: TStringListAAC): THeader;
var
  Header: THeader;
  LyHeaderLines: TStringListAAC;
  SearchStr, ThisString, Key, Value, MarkupStr: String;
  LineIndex: Integer;
  FoundThis, FoundAny: Boolean;
begin
  SearchStr := LyArg(InputText.Text, '\header');
  if not SearchStr.IsEmpty then
  begin
    LyHeaderLines := TStringListAAC.Create(SearchStr);
    LineIndex := 0;
    FoundAny := False;
    for ThisString in LyHeaderLines do
    begin
      FoundThis := False;
      if ThisString.Contains('=') then
      begin
        LyHeaderLines.GetNameValue(LineIndex, Key, Value);
        Key := Key.Trim;
        Value := Value.Trim;
       
        if IsASingleQuotedString(Value) then 
        begin
          Value := Value.DeQuotedString('"');
          FoundThis := True;
        end
        else
        begin
          MarkupStr := LyArg(Value, '\markup');
          Value := ExtractQuotedStrings(MarkupStr);
          FoundThis := True;
        end; 
      end; { if contains @code('=') }

      if FoundThis then
      with Header do
      begin
        case Key of
          'title':     FTitle := Value;
          'subtitle':  FSubtitle := Value;
          'composer':  FComposer := Value;
          'dates':     FDates := Value;
          'poet':      FPoet := Value;
          'editor':    FEditor := Value;
          'copyright': FCopyright := Value;
          'source':    FSource := Value;
        end;
        FoundAny := True;
      end;
      Inc(LineIndex);
    end;
    Header.FValid := FoundAny;
  end;
  FreeAndNil(LyHeaderLines);
  result := Header;
end;

end.
