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
  TIndexPair = class 
  private
    var
      FStart: Integer;  {< Start index }
      FEnd: Integer;    {< End index }
      FSpan: Integer;   {< Distance from start to end }
      FValid: Boolean;  {< Was a valid pair found? }
  public
    constructor Create();
    constructor Create(StartIndex, EndIndex: Integer);

    { Given a string, return a new instance containing the start and end
      indices of the range between the given delimiters. If not found, mark as
      invalid.  }
    constructor Create(Source, StartDelim, EndDelim: String);

    { Set the end index, and if the start index has also been set, calculate
      the span and mark as valid }
    procedure SetEndSpan(Index: Integer);
 
    { In a string, mark the start and end indices of a single expression
      between given delimiter characters, ignoring any nested groups with the
      same delimiters in between. The delimiters must not be identical,
      otherwise it is impossible to determine nesting. }
    procedure MarkBalancedDelimiterSubstring(Source: String; StartDelim, 
      EndDelim: Char);

   { Mark the outline of a substring delimited by matched curly braces. }
    procedure MarkMatchedBraces(Source: String);

    property StartIndex: Integer read FStart  write FStart;
    property EndIndex:   Integer read FEnd    write SetEndSpan;
    property IsValid:    Boolean read FValid; 
  end;

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

{ Return a substring that is a complete matched-brace expression,
including the braces. }
function CopyBraceExpr(Source: String): String;

{ Find the first occurence of a given Lilypond command in a string and return
  its brace-delimited argument. Return an empty string if not found. }
function LyArg(Source, Command: String): String;

{ Find all the quoted portions of a given string and return them as a single
  string, delimited by spaces. }
function ExtractQuotedStrings(Source: String): String;

implementation

constructor TIndexPair.Create();
begin
  inherited Create;
  FStart := -1;
  FEnd   := -1;
  FValid := False;
end;

constructor TIndexPair.Create(StartIndex, EndIndex: Integer);
begin
  inherited Create;
  FStart := StartIndex;
  FEnd   := EndIndex;
  FSpan  := EndIndex - StartIndex;
  FValid := True;
end;

procedure TIndexPair.SetEndSpan(Index: Integer);
begin
  FEnd := Index;
  if FStart <> -1 then
  begin
    FSpan := FEnd - FStart;
    FValid := True;
  end; 
end;

constructor TIndexPair.Create(Source, StartDelim, EndDelim: String);
begin
  inherited Create;
  FStart := Source.IndexOf(StartDelim);
  FSpan  := Source.Substring(FStart + 1).IndexOf(EndDelim);
  FEnd   := FStart + FSpan;
  FValid := not ((FStart = -1) or (FSpan = -1));
end;

function CopyStringRange(Source: String; Outline: TIndexPair; 
  ModeFlag: TRangeMode): String;

  function InclusiveRange(Source: String; Outline: TIndexPair): String;
  begin
    result := Source.Substring(Outline.FStart, Outline.FSpan);
  end;

  function ExclusiveRange(Source: String; Outline: TIndexPair): String;
  begin
    result := Source.Substring(Outline.FStart + 1, Outline.FSpan - 2);
  end;

var
  OutputStr: String;
begin
  OutputStr := Source;
  if Outline.IsValid then
  begin
    case ModeFlag of
      rkInclusive: OutputStr := InclusiveRange(Source, Outline);
      rkExclusive: OutputStr := ExclusiveRange(Source, Outline);
    end;
  end;
  result := OutputStr;
end;

procedure TIndexPair.MarkBalancedDelimiterSubstring(Source: String;
  StartDelim, EndDelim: Char);
var
  BraceLevel, SIndex: Integer;
  ThisChar: Char;
begin
  Assert(StartDelim <> EndDelim);
  BraceLevel := 0;
  for SIndex := 0 to Length(Source) - 1 do
  begin
    ThisChar := Source.Chars[SIndex];
    
    if ThisChar = StartDelim then
    begin
      if BraceLevel = 0 then
      begin
        Self.StartIndex := SIndex;
      end;
      Inc(BraceLevel);
    end
    else if ThisChar = EndDelim then
    begin
      if BraceLevel > 0 then
      begin
        Dec(BraceLevel);
        if BraceLevel = 0 then
        begin
          { include closing brace }
          Self.EndIndex := SIndex + 1; 
          break;
        end;
      end;
    end;
  end; 
end;

procedure TIndexPair.MarkMatchedBraces(Source: String);
begin
  Self.MarkBalancedDelimiterSubstring(Source, '{', '}');
end;

function CopyBraceExpr(Source: String): String;
var
  Outline: TIndexPair;
  TempStr: String = '';
begin
  Outline := TIndexPair.Create;
  Outline.MarkMatchedBraces(Source);
  if Outline.IsValid then
  begin
    TempStr := CopyStringRange(Source, Outline, rkInclusive);
  end;
  FreeAndNil(Outline);
  result := TempStr;
end;

function LyArg(Source, Command: String): String;
var
  SearchStr: String = '';
  Arg: String = '';
begin
  if Source.Contains(Command) then
  begin
    SearchStr := StringDropBefore(Source, Command);
    Arg := CopyBraceExpr(SearchStr);
  end;
  result := Arg;
end;

function ExtractQuotedStrings(Source: String): String;

  function ConcatDequotedStrings(InputLines: TStringList): String; 
  var 
    OutputStr: String;
  begin
    InputLines.StrictDelimiter := True;
    InputLines.Delimiter := ' ';
    OutputStr := InputLines.DelimitedText;
    OutputStr := DelChars(OutputStr, '"');
    result := OutputStr;
  end;

var
  MarkupStrings: TStringList;
  Markup, OutputStr: String;
  Outline: TIndexPair;
begin
  MarkupStrings := TStringList.Create;
  while Source.CountChar('"') > 1 do
  begin
    Outline := TIndexPair.Create(Source, '"', '"');
    if Outline.IsValid then
    begin
      Markup := CopyStringRange(Source, Outline, rkExclusive);
      MarkupStrings.Add(Markup);
      OutputStr := Source.Substring(Outline.EndIndex + 2);
    end
    else
      break;
  end;
  OutputStr := ConcatDequotedStrings(MarkupStrings);

  FreeAndNil(Outline);
  FreeAndNil(MarkupStrings);
  result := OutputStr;
end;

end.

