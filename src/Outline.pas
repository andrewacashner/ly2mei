{$mode objfpc}{$H+}{$J-}{$ASSERTIONS+}

{ @abstract(This unit makes it possible to find and extract sections of
  strings and stringlists by marking the indices, and to find Lilypond
  commands and their brace-delimited arguments.)
  @author(Andrew Cashner) }
unit Outline;

interface

uses SysUtils, Classes, StrUtils, StringTools;

type 
  { @abstract(Stores the start and end positions in a string or other sequence.) }
  TIndexPair = record
    FStart: Integer;  {< Start index }
    FEnd: Integer;    {< End index }
    FSpan: Integer;   {< Distance from start to end }
    FValid: Boolean;  {< Was a valid pair found? }
  end;

{ Given a string, return a new instance containing the start and end
  indices of the range between the given delimiters. If not found, mark as
  invalid.  }
function FindDelimitedRange(Source, StartDelim, EndDelim: String): TIndexPair; 

type
  { Mode flags for marking delimited substrings }
  TRangeMode = (
    rkInclusive,  {< Include delimiters in the range. }
    rkExclusive   {< Exclude delimiters. }
  );

{ Return the portion of the string between the indices in a @link(TIndexPair).
  The start and end characters of the range are included by default
  (@link(rkInclusive)). Return the original string if the index pair is
  invalid. }
function CopyStringRange(Source: String; Outline: TIndexPair; ModeFlag:
  TRangeMode): String;

{ In a string, mark the start and end indices of a single expression between
  given delimiter characters, ignoring any nested groups with the same
  delimiters in between. The delimiters must not be identical, otherwise it is
  impossible to determine nesting. }
function BalancedDelimiterSubstring(Source: String; StartDelim, EndDelim:
  Char): TIndexPair; 

{ Mark the outline (@link(TIndexPair)) of a substring delimited by matched
curly braces. }
function FindMatchedBraces(Source: String): TIndexPair;

{ Return a substring that is a complete matched-brace expression,
including the braces. }
function CopyBraceExpr(Source: String): String;

type
  { Parsing commands and arguments: These flags indicate the type of
    @link(TCommandArg) found. }
  TStatusCommandArg = (
    skCommand,    {< Found just a command }
    skCommandArg, {< Found a command and an argument }
    skInvalid     {< Found neither }
);
 
type
  { @abstract(A command and its argument.) 
    A flag indicates whether it actually holds both command and argument, just
    one or the other, or neither. }
  TCommandArg = record
    FCommand, FArg: String;
    FStatus: TStatusCommandArg;
  end;
    
{ In a string, find the first instance of command that starts with a given
  control character (e.g., backslash). If it is followed by an argument
  delimited by given strings (e.g., curly braces), return an object with
  both the command and the argument. If not return the object marked
  invalid. The delimiters are included in the string. }
function FindCommandArg(Source: String; ControlChar, ArgStartDelim,
  ArgEndDelim: Char): TCommandArg;

{ Find the first occurence of a given Lilypond command in a string and return
  its brace-delimited argument. Return an empty string if not found. }
function LyArg(Source, Command: String): String;

{ Find all the quoted portions of a given string and return them as a single
  string, delimited by spaces. }
function ExtractQuotedStrings(Source: String): String;

implementation

function FindDelimitedRange(Source, StartDelim, EndDelim: String): TIndexPair;
var 
  Pair: TIndexPair;
begin
  with Pair do
  begin
    FStart := Source.IndexOf(StartDelim);
    FSpan  := Source.Substring(FStart + 1).IndexOf(EndDelim);
    FEnd   := FStart + FSpan;
    FValid := not ((FStart = -1) or (FSpan = -1));
  end;
  result := Pair;
end;

function CopyStringRange(Source: String; Outline: TIndexPair; 
                              ModeFlag: TRangeMode): String;
begin
  with Outline do
  begin
    if not FValid then
      result := Source
    else
      case ModeFlag of
        rkInclusive: result := Source.Substring(FStart, FSpan);
        rkExclusive: result := Source.Substring(FStart + 1, FSpan - 2);
      end;
  end;
end;

function BalancedDelimiterSubstring(Source: String; StartDelim, EndDelim:
  Char): TIndexPair; 
var
  BraceLevel, SIndex: Integer;
  ThisChar: Char;
  Outline: TIndexPair;
begin
  assert(StartDelim <> EndDelim);
  BraceLevel := 0;
  SIndex := 0;
  for ThisChar in Source do
  begin
    if ThisChar = StartDelim then
    begin
      if BraceLevel = 0 then
      begin
        Outline.FStart := SIndex;
      end;
      Inc(BraceLevel);
    end
    else
    begin 
      if ThisChar = EndDelim then
      begin
        if BraceLevel > 0 then
        begin
          Dec(BraceLevel);
          if BraceLevel = 0 then
            with Outline do
            begin
              FEnd := SIndex + 1; { include closing brace }
              FSpan := FEnd - FStart;
              FValid := True;
              break;
            end;
        end;
      end;
    end;
    Inc(SIndex);
  end; { for }
  result := Outline;
end;

function FindMatchedBraces(Source: String): TIndexPair;
begin
  result := BalancedDelimiterSubstring(Source, '{', '}');
end;

function CopyBraceExpr(Source: String): String;
var
  Outline: TIndexPair;
  TempStr: String;
begin
  Outline := FindMatchedBraces(Source);
  if Outline.FValid then
    TempStr := CopyStringRange(Source, Outline, rkInclusive)
  else 
    TempStr := '';
  result := TempStr;
end;


function FindCommandArg(Source: String; ControlChar, ArgStartDelim,
  ArgEndDelim: Char): TCommandArg;
var
  TestStr, Command: String;
  Outline: TIndexPair;
  CommandArg: TCommandArg;
begin
  { Find command }
  TestStr := Source.Substring(Source.IndexOf(ControlChar));
  Command := ExtractWord(1, TestStr, [' ', LineEnding, ArgStartDelim]);
  if not Command.IsEmpty then
  begin
    CommandArg.FCommand := Command;
    CommandArg.FStatus := skCommand;

    { Find arg within delimiters }
    TestStr := TestStr.Substring(Length(Command));
    if TestStr.TrimLeft.StartsWith(ArgStartDelim) then
    begin
      Outline := BalancedDelimiterSubstring(TestStr, ArgStartDelim,
                  ArgEndDelim); 
      if Outline.FValid then
      begin
        CommandArg.FArg := CopyStringRange(TestStr, Outline, rkInclusive);
        CommandArg.FStatus := skCommandArg;
      end;
    end;
  end;
  result := CommandArg;
end;

function LyArg(Source, Command: String): String;
var
  CommandArg: TCommandArg;
  Arg: String;
begin
  if Source.Contains(Command) then
  begin
    Source := Source.Substring(Source.IndexOf(Command));
    CommandArg := FindCommandArg(Source, '\', '{', '}');
    if (CommandArg.FStatus = skCommandArg) 
      and (CommandArg.FCommand = Command) then 
      Arg := CommandArg.FArg;
  end;
  result := Arg;
end;

function ExtractQuotedStrings(Source: String): String;
var
  MarkupStrings: TStringListAAC;
  Markup: String;
  Outline: TIndexPair;
begin
  MarkupStrings := TStringListAAC.Create;
  try
    while Source.CountChar('"') > 1 do
    begin
      Outline := FindDelimitedRange(Source, '"', '"');
      if Outline.FValid then
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
    result := Source;
  end;
end;

end.

