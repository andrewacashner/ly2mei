{$mode objfpc}{$H+}{$J-}{$ASSERTIONS+}

{ @abstract(This unit makes it possible to find and extract sections of
  strings and stringlists by marking the indices, and to find Lilypond
  commands and their brace-delimited arguments.)
  @author(Andrew Cashner) }
unit Outline;

interface

uses SysUtils, Classes, StrUtils;

type 
  { This class stores start and end positions in a string or other sequence. }
  TIndexPair = class
  public
    var
      FStart, FEnd: Integer;
      FValid: Boolean;
    procedure Clear;
    function IsValid: Boolean;
    function Span: Integer;

    { Given a string, return a new instance containing the start and end
      indices of the range between the given delimiters. If not found, mark as
      invalid.  }
    function FindRangeInString(Source, StartDelim, EndDelim: String):
      TIndexPair; 
  end;

{ Mode flags for marking delimited substrings }
type
  TRangeMode = (
    { Include delimiters in the range. }
    rkInclusive, 
    { Exclude delimiters. }
    rkExclusive);

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
  Char; Outline: TIndexPair): TIndexPair; 

{ Mark the outline (@link(TIndexPair)) of a substring delimited by matched
curly braces. }
function FindMatchedBraces(Source: String; Outline: TIndexPair): TIndexPair;

{ Return a substring that is a complete matched-brace expression,
including the braces. }
function CopyBraceExpr(Source: String): String;

{ Parsing commands and arguments: These flags indicate the type of
@link(TCommandArg) found. }
type
  TStatusCommandArg = (
    { Found just a command }
    skCommand, 
    { Found a command and an argument }
    skCommandArg, 
    { Found neither }
    skInvalid);
 
type
  { This class holds a command and its argument. }
  TCommandArg = class
  public
    var
      FCommand, FArg: String;
      FStatus: TStatusCommandArg;
    procedure Clear;
    function IsValid: Boolean;
    function ToString: String; override;
    
    { In a string, find the first instance of command that starts with a given
      control character (e.g., backslash). If it is followed by an argument
      delimited by given strings (e.g., curly braces), return an object with
      both the command and the argument. If not return the object marked
      invalid. The delimiters are included in the string. }
    function ExtractFromString(Source: String; ControlChar, ArgStartDelim,
      ArgEndDelim: Char): TCommandArg;
  end;

{ Find the first occurence of a given Lilypond command in a string and return
  its brace-delimited argument. Return an empty string if not found. }
function LyArg(Source, Command: String): String;


implementation

procedure TIndexPair.Clear;
begin
  FStart := 0;
  FEnd   := 0;
  FValid := False;
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

function TIndexPair.FindRangeInString(Source, StartDelim, 
                                          EndDelim: String): TIndexPair;
begin
  FStart := Source.IndexOf(StartDelim);
  FEnd   := Source.Substring(FStart + 1).IndexOf(EndDelim);
  FValid := not ((FStart = -1) or (FEnd = -1));
  FEnd   := FStart + FEnd;
  result := Self;
end;

function CopyStringRange(Source: String; Outline: TIndexPair; 
                              ModeFlag: TRangeMode): String;
begin
  assert(Outline <> nil);
  if not Outline.IsValid then
    result := Source
  else
  begin
    case ModeFlag of
      rkInclusive: result := Source.Substring(Outline.FStart, Outline.Span);
      rkExclusive: result := Source.Substring(Outline.FStart + 1, 
                                Outline.Span - 2);
    end;
  end;
end;

function BalancedDelimiterSubstring(Source: String; StartDelim, EndDelim:
  Char; Outline: TIndexPair): TIndexPair; 
var
  BraceLevel, SIndex: Integer;
  ThisChar: Char;
begin
  assert(StartDelim <> EndDelim);
  Outline.Clear;
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
          begin
            Outline.FEnd := SIndex + 1; { include closing brace }
            Outline.FValid := True;
            break;
          end;
        end;
      end;
    end;
    Inc(SIndex);
  end; { for }

  result := Outline;
end;

function FindMatchedBraces(Source: String; Outline: TIndexPair): TIndexPair;
begin
  assert(Outline <> nil);
  result := BalancedDelimiterSubstring(Source, '{', '}', Outline);
end;

function CopyBraceExpr(Source: String): String;
var
  Outline: TIndexPair;
  TempStr: String;
begin
  Outline := TIndexPair.Create;
  try
    Outline := FindMatchedBraces(Source, Outline);
    if Outline.IsValid then
      TempStr := CopyStringRange(Source, Outline, rkInclusive)
    else 
      TempStr := '';
  finally
    FreeAndNil(Outline);
    result := TempStr;
  end;
end;

procedure TCommandArg.Clear;
begin
  FCommand := '';
  FArg := '';
  FStatus := skInvalid;
end;

function TCommandArg.IsValid: Boolean;
begin
  result := not (FStatus = skInvalid);
end;

function TCommandArg.ToString: String;
begin
  if Self.IsValid then
    result := FCommand + ' ' + FArg
  else
    result := '';
end;

function TCommandArg.ExtractFromString(Source: String; ControlChar, ArgStartDelim,
  ArgEndDelim: Char): TCommandArg;
var
  TestStr, Command: String;
  Outline: TIndexPair;
begin
  Outline := TIndexPair.Create;
  try
    Self.Clear;
    { Find command }
    TestStr := Source.Substring(Source.IndexOf(ControlChar));
    Command := ExtractWord(1, TestStr, [' ', LineEnding, ArgStartDelim]);
    if not Command.IsEmpty then
    begin
      FCommand := Command;
      FStatus := skCommand;

      { Find arg within delimiters }
      TestStr := TestStr.Substring(Length(FCommand));
      if TestStr.TrimLeft.StartsWith(ArgStartDelim) then
      begin
        Outline := BalancedDelimiterSubstring(TestStr, ArgStartDelim,
                    ArgEndDelim, Outline); 
        if Outline.IsValid then
        begin
          FArg := CopyStringRange(TestStr, Outline, rkInclusive);
          FStatus := skCommandArg;
        end;
      end;
    end;
  finally
    FreeAndNil(Outline);
    result := Self;
  end;
end;

function LyArg(Source, Command: String): String;
var
  CommandArg: TCommandArg;
  Arg: String;
begin
  CommandArg := TCommandArg.Create;
  try
    if Source.Contains(Command) then
    begin
      Source := Source.Substring(Source.IndexOf(Command));
      CommandArg := CommandArg.ExtractFromString(Source, '\', '{', '}');
      if CommandArg.IsValid and (CommandArg.FCommand = Command) then
        Arg := CommandArg.FArg;
    end;
  finally
    FreeAndNil(CommandArg);
    result := Arg;
  end;
end;

end.

