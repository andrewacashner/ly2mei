{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} {$OPTIMIZATION tailrec}

{ @abstract(Parse a Lilypond header and convert it to MEI)
  @author(Andrew Cashner) }
unit Header;

interface

uses SysUtils, StrUtils, Classes, StringTools, Outline;

{ This class stores all the fields required in the Lilypond source file and
  can write them to MEI.  }
type
  THeader = class
  private
    const
      FProgramName: String = 'ly2mei';
    var
      FTitle, FSubtitle, FComposer, FDates, 
      FPoet, FEditor, FCopyright, FSource: String;
      FValid: Boolean;
  public
    function IsValid: Boolean;
    
    { Read a Lilypond header and extract specific values. }
    function FromLily(LyHeader: TStringList): THeader; 

    { Return a string list containing the MEI expression of the header data. }
    function ToMEI(MEI: TStringList): TStringList;
  end;

{ Find all the quoted portions of a given string and return them as a single
  string, delimited by spaces. }
function ExtractQuotedStrings(Source: String): String;

{ Find a header definition and parse it into a @link(THeader) object. }
function ParseHeader(InputText: TStringList; HeaderValues: THeader): THeader;


implementation

function THeader.IsValid: Boolean;
begin
  result := FValid;
end;

function THeader.FromLily(LyHeader: TStringList): THeader; 
var 
  ThisString, Key, Value, MarkupStr: String;
  Outline: TIndexPair;
  LineIndex: Integer;
  FoundThis, FoundAny: Boolean;
begin
  Outline := TIndexPair.Create;
  try
    LineIndex := 0;
    FoundAny := False;
    for ThisString in LyHeader do
    begin
      FoundThis := False;
      if ThisString.Contains('=') then
      begin
        LyHeader.GetNameValue(LineIndex, Key, Value);
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

  Self.FValid := FoundAny;
  finally
    FreeAndNil(Outline);
    result := Self;
  end;
end;

function THeader.ToMEI(MEI: TStringList): TStringList;
begin
  assert(MEI <> nil);
  MEI.Clear;
  if not Self.IsValid then
  begin
    MEI.Add('<meiHead>');
    MEI.Add('</meiHead>');
    result := MEI;
    exit;
  end;

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
  MEI.Add(IndentStr(3) + '<application><name>' + FProgramName + '</name></application>');
  MEI.Add(IndentStr(2) + '</appInfo>');
  MEI.Add(IndentStr(1) + '</encodingDesc>');
  
  if not FSource.IsEmpty then
    MEI.Add(IndentStr(1) + '<sourceDesc><source>' + FSource + '</source></sourceDesc>');

  MEI.Add('</meiHead>'); 

  result := MEI;
end;

function ExtractQuotedStrings(Source: String): String;
var
  MarkupStrings: TStringList;
  Markup: String;
  Outline: TIndexPair;
begin
  MarkupStrings := TStringList.Create;
  Outline := TIndexPair.Create;
  try
    while Source.CountChar('"') > 1 do
    begin
      Outline := Outline.FindRangeInString(Source, '"', '"');
      if Outline.IsValid then
      begin
        Markup := CopyStringRange(Source, Outline, rkExclusive);
        MarkupStrings.Add(Markup);
        Source := Source.Substring(Outline.FEnd + 2);
      end
      else
        break;
    end;
    MarkupStrings.StrictDelimiter := True;
    MarkupStrings.Delimiter := ' ';
    Source := DelChars(MarkupStrings.DelimitedText, '"');

  finally
    FreeAndNil(MarkupStrings);
    FreeAndNil(Outline);
    result := Source;
  end;
end;

function ParseHeader(InputText: TStringList; HeaderValues: THeader): THeader;
var
  LyHeader: TStringList;
  SearchStr: String;
  Outline: TIndexPair;
begin
  LyHeader := TStringList.Create;
  Outline := TIndexPair.Create;
  HeaderValues.FValid := False;
  try
    SearchStr := LyArg(InputText.Text, '\header');
    if not SearchStr.IsEmpty then
    begin
      LyHeader := Lines(SearchStr, LyHeader);
      HeaderValues := HeaderValues.FromLily(LyHeader);
    end;
  finally
    FreeAndNil(Outline);
    FreeAndNil(LyHeader);
    result := HeaderValues;
  end;
end;

end.
