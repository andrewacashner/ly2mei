{ `header`

  Andrew Cashner, 2021/10/13

  Translate a Lilypond header expression to MEI-XML.

  The header must be marked with a `\header` command with the argument in
  curly braces.  Within the braces there must be a set of fields in the format
  `key = "Value"`. The value must be a single string within quotation marks.
  
  They may also take the form `key = \markup < "Value" >` where `<>` are curly
  braces.  The `\markup` command *must* be followed by a single argument in
  curly braces.  If there is a string like `\markup \italic "Name"`, the
  string will be ignored. 

  Within a markup argument, only the quoted strings will be extracted: 
  `\markup < \column < \line "one" \line \italic "two" > >` will be saved as
  `one two`.

  Most of the fields translate obviously, for example `title = "Music"`
  becomes `meiHead/fileDesc/titleStmt/title[@type=main]/Music`.
}
{$mode objfpc}{$H+}{$J-}
program header(input, output, stderr);

uses SysUtils, Classes, StrUtils;

type
  TRangeMode = (rkInclusive, rkExclusive);

{ CLASS: `TIndexPair`

  Stores start and end positions in a sequence (as in a string).
}
type
  TIndexPair = class
  private
    var
      FStart, FEnd: Integer;
      FValid: Boolean;
  public
    function Clear: TIndexPair;
    function IsValid: Boolean;
    function Span: Integer;
    function SnipString(Source: String; ModeFlag: TRangeMode): String;
    function FindRange(Source, StartDelim, EndDelim: String): TIndexPair;
  end;

{ `TIndexPair` class methods }
function TIndexPair.Clear: TIndexPair;
begin
  FStart := 0;
  FEnd   := 0;
  FValid := False;
  result := Self;
end;

function TIndexPair.IsValid: Boolean;
begin
  result := FValid;
end;

function TIndexPair.Span: Integer;
begin
  if FValid then
    result := FEnd - FStart
  else
    result := -1;
end;

function TIndexPair.SnipString(Source: String; ModeFlag: TRangeMode): String;
begin
  case ModeFlag of
    rkInclusive: result := Source.Substring(FStart, Span);
    rkExclusive: result := Source.Substring(FStart + 1, Span - 2);
  end;
end;

function TIndexPair.FindRange(Source, StartDelim, EndDelim: String): TIndexPair;
begin
  Self.Clear;
  FStart := Source.IndexOf(StartDelim);
  FEnd := Source.Substring(FStart + 1).IndexOf(EndDelim);
  FValid := not ((FStart = -1) or (FEnd = -1));
  FEnd := FStart + FEnd;
  result := Self;
end;

{ `MatchBraces`

  In a string, mark the start and end indices of a single expression delimited
  by balanced curly braces, which may including other such expressions. 
}
function MatchBraces(Source: String; Outline: TIndexPair): TIndexPair;
type
  TReadMode = (rkNormal, rkBrace);
var
  BraceLevel, SIndex: Integer;
  Current: String;
  ReadMode: TReadMode;
begin
  Outline := Outline.Clear;
  BraceLevel := 0;
  SIndex := 0;
  ReadMode := rkNormal;

  for Current in Source do
  begin
    case Current of
      '{':
      begin
        if ReadMode = rkNormal then
        begin
          Outline.FStart := SIndex;
          ReadMode := rkBrace;
        end;
        Inc(BraceLevel);
      end;

      '}':
      begin
        if ReadMode = rkBrace then
        begin
          Dec(BraceLevel);
          if BraceLevel = 0 then
          begin
            Outline.FEnd := SIndex + 1; { include closing brace }
            Outline.FValid := True;
            break;
          end;
        end;
      end;
    end; { case }
    Inc(SIndex);
  end; { for }
  result := Outline;
end;

{ `StringDropBefore`
  
  Cut out everything up to a given substring from a larger string.
}
function StringDropBefore(Source, Cut: String): String;
begin
  result := Source.Substring(Source.IndexOf(Cut) + Length(Cut));
end;

{ `ListToStringFromIndex`
  
  Return a string consisting of the text of a stringlist starting at a given
  index.
}
function StringListFromIndex(List: TStringList; Index: Integer): String;
begin
    result := List.Text.Substring(List.Text.IndexOf(List[Index]));
end;

{ `Lines`

  Split a string at newlines to make a `TStringList` }
function Lines(InputStr: String; OutputList: TStringList): TStringList;
begin
  OutputList.Clear;
  OutputList.Delimiter := LineEnding;
  OutputList.StrictDelimiter := True;
  OutputList.DelimitedText := InputStr;
  result := OutputList;
end;



{ CLASS: `THeader`

  Stores all the fields required in the Lilypond source file and can write
  them to MEI.
}
type
  THeader = class
  private
    const
      FProgramName: String = 'ly2mei';
    var
      FTitle, FSubtitle, FComposer, FDates, 
      FPoet, FEditor, FCopyright, FSource: String;
  public
    function FromLily(LyHeader: TStringList): THeader; 
    function ToMEI(MEI: TStringList): TStringList;
  end;

{ `THeader.FromLily`

  Read a Lilypond header and extract specific values.
}
function THeader.FromLily(LyHeader: TStringList): THeader; 
var 
  ThisString, Key, Value, Markup: String;
  Outline: TIndexPair;
  LineIndex: Integer;
  Found: Boolean;

function IsASingleQuotedString(Source: String): Boolean;
begin
  result := (Source.CountChar('"') = 2)
            and Source.StartsWith('"') 
            and Source.EndsWith('"');
end;

function ExtractQuotedStrings(Source: String): String;
var
  MarkupStrings: TStringList;
begin
  MarkupStrings := TStringList.Create;
  try
    while Source.CountChar('"') > 1 do
    begin
      Outline := Outline.FindRange(Source, '"', '"');
      if Outline.IsValid then
      begin
        Markup := Outline.SnipString(Source, rkExclusive);
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
    result := Source;
  end;
end;


begin
  Outline := TIndexPair.Create;

  try
    LineIndex := 0;
    for ThisString in LyHeader do
    begin
      Found := False;
      if ThisString.Contains('=') then
      begin
        LyHeader.GetNameValue(LineIndex, Key, Value);
        Key := Key.Trim;
        Value := Value.Trim;
       
        if IsASingleQuotedString(Value) then 
        begin
          Value := Value.DeQuotedString('"');
          Found := True;
        end
        else
        begin
          if Value.StartsWith('\markup') then
          begin
            Value := StringListFromIndex(LyHeader, LineIndex);
            Value := StringDropBefore(Value, '\markup');
            
            if not Value.TrimLeft.StartsWith('{') then
              Value := ''
            else
            begin
              Outline := MatchBraces(Value, Outline);
              Value := Outline.SnipString(Value, rkExclusive);
              Value := ExtractQuotedStrings(Value);
              Found := True;
            end; { braces }
          end; { \markup }
        end;
      end; { if contains '=' }

      if Found then
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
      Inc(LineIndex);
    end;
  finally
    FreeAndNil(Outline);
    result := Self;
  end;
end;


{ `THeader class functions }
function THeader.ToMEI(MEI: TStringList): TStringList;
begin
  if MEI = nil then
  begin
    WriteLn(stderr, 'Function THeader requires an initialized TStringList');
    exit;
  end;

  MEI.Add('<meiHead xmlns="http://www.music-encoding.org/ns/mei">');
  MEI.Add('  <fileDesc>');
  MEI.Add('    <titleStmt>');
  MEI.Add('      <title type="main">' + FTitle + '</title>');

  if not FSubtitle.IsEmpty then
    MEI.Add('      <title type="subtitle">' + FSubtitle + '</subtitle>');

  MEI.Add('      <respStmt>');
  MEI.Add('        <composer>' + FComposer + ' ' + FDates + '</composer>');

  if not FPoet.IsEmpty then
    MEI.Add('        <lyricist>' + FPoet + '</lyricist>');

  if not FEditor.IsEmpty then
    MEI.Add('        <editor>' + FEditor + '</editor>');

  MEI.Add('      </respStmt>');
  MEI.Add('    </titleStmt>');
  
  if not FEditor.IsEmpty then
  begin
    MEI.Add('    <editionStmt>');
    MEI.Add('      <respStmt><p>Edited by ' + FEditor + '</p></respStmt>');
    MEI.Add('    </editionStmt>');
  end;

  if not FCopyright.IsEmpty then
  begin
    MEI.Add('    <pubStmt>');
    MEI.Add('      <availability><p>' + FCopyright + '</p></availability>');
    MEI.Add('    </pubStmt>');
  end;

  MEI.Add('  </fileDesc>');
  MEI.Add('  <encodingDesc>');
  MEI.Add('    <appInfo>');
  MEI.Add('      <application><name>' + FProgramName + '</name></application>');
  MEI.Add('    </appInfo>');
  MEI.Add('  </encodingDesc>');
  MEI.Add('  <sourceDesc><source>' + FSource + '</source></sourceDesc>');
  MEI.Add('</meiHead>'); 

  result := MEI;
end;



{ MAIN }
var
  InputText, LyHeader, OutputText: TStringList;
  SearchStr, LyHeaderStr: String;
  Outline: TIndexPair;
  HeaderValues: THeader;
begin
  { setup }
  InputText := TStringList.Create;
  LyHeader := TStringList.Create;
  OutputText := TStringList.Create;
  Outline := TIndexPair.Create;
  HeaderValues := THeader.Create;

  try
    { read input }
    if ParamCount <> 1 then
    begin
      WriteLn('Usage: header INFILE.ly');
      exit;
    end;
    InputText.LoadFromFile(ParamStr(1));
    
    { find header }
    if InputText.Text.Contains('\header') then
    begin
      SearchStr := StringDropBefore(InputText.Text, '\header');
      Outline := MatchBraces(SearchStr, Outline);
      LyHeaderStr := Outline.SnipString(SearchStr, rkExclusive);
    end;

    { find subfields and store }
    LyHeader := Lines(LyHeaderStr, LyHeader);
    HeaderValues := HeaderValues.FromLily(LyHeader);
    
    { make output }
    OutputText := HeaderValues.ToMei(OutputText); 
    WriteLn(OutputText.Text);

  finally
    FreeAndNil(HeaderValues);
    FreeAndNil(Outline);
    FreeAndNil(OutputText);
    FreeAndNil(LyHeader);
    FreeAndNil(InputText);
  end;
end.

