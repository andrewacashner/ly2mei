{$mode objfpc}{$H+}{$J-}

{ @abstract(Utilities for handling strings and stringlists, all functional.)
  @author(Andrew Cashner) }
unit StringTools;

interface

uses SysUtils, Classes;

{ Write notes to standard error if compiled with @code(-dDEBUG) }
procedure DebugLn(Msg: String);

{ Return a string containing just the first character of the given string. }
function FirstCharStr(Source: String): String;

{ Copy the portion of a string that follows the end of a given substring. }
function StringDropBefore(Source, StartAfter: String): String;

function SubstringAfter(Source, StartAfter: String): String;

{ Return the portion of a string before a given delimiter. }
function StringDropAfter(Source, EndBefore: String): String;

function SubstringBefore(Source, EndBefore: String): String;

{ Return the portion of a string between two substrings (exclusive); if the
  substrings are not found, return the original string unchanged. }
function CopyStringBetween(Source, StartAfter, EndBefore: String): String;

{ Return the first quoted portion of a string (enclosed in @code(") marks),
  omitting the quotation marks }
function CopyFirstQuotedString(Source: String): String;

{ Is this a single quoted string without any other quotes within it? }
function IsASingleQuotedString(Source: String): Boolean;

type
  TStringListPlus = class(TStringList)
  public
    constructor Create();

    { Create a new stringlist containing the text of an input string, separated at
      newlines. }
    constructor Create(InputStr: String);
   
    { Copy the lines after a given index from a string list. }
    function AssignAfterIndex(InputLines: TStringList; 
      StartIndex: Integer):TStringListPlus;

    { On each line, strip out everything between a comment char (@code('%'))
      and the next newline. }
    function RemoveComments: TStringListPlus;

    { Remove empty lines or lines with only whitespace. }
    function RemoveBlankLines: TStringListPlus;
  end;

{ Return a string consisting of the text of a stringlist starting at a
  given index. }
function ToStringFromIndex(InputLines: TStringList; Index: Integer): String; 

function StringToWordArray(InputStr: String): TStringArray;
function WordArrayToString(StringArray: TStringArray): String;

function BalancedDelimiterSubstringWords(InputStr, StartDelim, EndDelim:
  String): String; 

function BalancedDelimiterSubarrayWords(InputWords: TStringArray; 
  StartDelim, EndDelim: String): TStringArray; 

function CommandArg(InputStr, Command, StartDelim, EndDelim: String): String;

function CommandArgBraces(InputStr, Command: String): String;
function CommandArgAngleBrackets(InputStr, Command: String): String;

implementation

procedure DebugLn(Msg: String);
begin
  {$ifdef DEBUG}
  WriteLn(stderr, '> ' + Msg);
  {$endif}
end;

function FirstCharStr(Source: String): String;
begin
  result := Source.Substring(0, 1);
end;

function StringDropBefore(Source, StartAfter: String): String;
var
  OutputStr: String;
begin
  OutputStr := Source;
  if Source.Contains(StartAfter) then
  begin
    OutputStr := Source.Substring(Source.IndexOf(StartAfter) 
                  + Length(StartAfter));
  end;
  result := OutputStr;
end;

function StringDropAfter(Source, EndBefore: String): String;
var
  OutputStr: String;
begin
  OutputStr := Source;
  if Source.Contains(EndBefore) then
  begin
    OutputStr := Source.Substring(0, Source.IndexOf(EndBefore));
  end;
  result := OutputStr;
end;

function SubstringAfter(Source, StartAfter: String): String;
begin
  result := StringDropBefore(Source, StartAfter);
end;

function SubstringBefore(Source, EndBefore: String): String;
begin
  result := StringDropAfter(Source, EndBefore);
end;

function CopyStringBetween(Source, StartAfter, EndBefore: String): String;
var
  TempStr, OutputStr: String;
begin
  OutputStr := Source;
  TempStr := StringDropBefore(Source, StartAfter);
  OutputStr := StringDropAfter(TempStr, EndBefore);
  result := OutputStr;
end;

function CopyFirstQuotedString(Source: String): String;
var
  OutputStr: String = '';
begin
  if Source.Contains('"') and (Source.CountChar('"') > 1) then
  begin
    OutputStr := CopyStringBetween(Source, '"', '"');
    result := OutputStr;
  end;
end;

function IsASingleQuotedString(Source: String): Boolean;
begin
  result := (Source.CountChar('"') = 2)
            and Source.StartsWith('"') 
            and Source.EndsWith('"');
end;

constructor TStringListPlus.Create;
begin
  inherited Create;
end;

constructor TStringListPlus.Create(InputStr: String);
begin
  inherited Create;
  Delimiter := LineEnding;
  StrictDelimiter := True;
  DelimitedText := InputStr;
end;

function TStringListPlus.AssignAfterIndex(InputLines: TStringList;
  StartIndex: Integer): TStringListPlus;
var
  ThisIndex: Integer;
begin
  for ThisIndex := StartIndex to InputLines.Count - 1 do
  begin
    Self.Add(InputLines[ThisIndex]);
  end;
  result := Self;
end;

function ToStringFromIndex(InputLines: TStringList; Index: Integer): String; 
var
  TempLines: TStringListPlus;
  OutputStr: String;
begin
  TempLines := TStringListPlus.Create;
  TempLines.AssignAfterIndex(InputLines, Index);
  OutputStr := TempLines.Text;
  FreeAndNil(TempLines);
  result := OutputStr;
end;

function TStringListPlus.RemoveComments: TStringListPlus;
var
  ThisString, CleanString: String;
  TempLines: TStringList;
  CommentDelim: String = '%';
begin
  TempLines := TStringList.Create;
  for ThisString in Self do
  begin
    if not ThisString.StartsWith(CommentDelim) then
    begin
      CleanString := StringDropAfter(ThisString, CommentDelim);
      TempLines.Add(CleanString);
    end;
  end;
  Assign(TempLines);
  FreeAndNil(TempLines);
  result := Self;
end;

function TStringListPlus.RemoveBlankLines: TStringListPlus;
var
  ThisString: String;
  TempLines: TStringList;
begin
  TempLines := TStringList.Create;
  for ThisString in Self do
  begin
    if not ThisString.Trim.IsEmpty then
      TempLines.Add(ThisString);
  end;
  Assign(TempLines);
  FreeAndNil(TempLines);
  result := Self;
end;

function StringToWordArray(InputStr: String): TStringArray;
begin
  result := InputStr.Split([' ', LineEnding], TStringSplitOptions.ExcludeEmpty);
end;

function WordArrayToString(StringArray: TStringArray; StartIndex: Integer = -1;
  EndIndex: String = -1): String); 
var
  OutputStr: String = '';
begin
  if StartIndex = -1 then
    OutputStr := OutputStr.Join(' ', StringWords)
  else if EndIndex = -1 then
    OutputStr := OutputStr.Join(' ', StringWords, StartIndex)
  else
    OutputStr := OutputStr.Join(' ', StringWords, StartIndex, EndIndex - StartIndex);
  
  result := OutputStr;
end;

function BalancedDelimiterSubarrayWords(InputWords: TStringArray; 
  StartDelim, EndDelim: String): TStringArray; 
var
  OutputWords: TStringArray;
  ThisWord: String;
  WordIndex, StartIndex, EndIndex, DelimLevel: Integer;
  InsideDelim: Boolean;
begin
  WordIndex := 0;
  DelimLevel := 0;
  for ThisWord in InputWords do
  begin
    if ThisWord = StartDelim then
    begin
      if DelimLevel = 0 then
      begin
        StartIndex := WordIndex + 1;
      end;
      Inc(DelimLevel);
    end
    else if ThisWord = EndDelim then
    begin
      Dec(DelimLevel);
      if DelimLevel = 0 then
      begin
        EndIndex := WordIndex - 1;
        break;
      end;
    end;
    Inc(WordIndex);
  end;

  if EndIndex > StartIndex then
  begin
    OutputWords := Copy(InputWords, StartIndex, EndIndex - StartIndex);
  end
  else
  begin
    SetLength(OutputWords, 0);
  end;

  result := OutputWords;
end;

function BalancedDelimiterSubstringWords(InputStr, StartDelim, EndDelim:
  String): String; 
var
  OutputStr: String = '';
  StringWords, WordsInsideDelims: TStringArray;
begin
  StringWords := StringToWordArray(InputStr);
  WordsInsideDelims := BalancedDelimiterSubarrayWords(StringWords, 
                        StartDelim, EndDelim);
  if WordsInsideDelims.Count > 0 then
  begin
    OutputStr := WordArrayToString(WordsInsideDelims);
  end;

  result := OutputStr;

  FreeAndNil(StringWords);
  FreeAndNil(WordsInsideDelims);
end;

function BracketedSubarray(InputWords: TStringArray): TStringArray;
begin
  result := BalancedDelimiterSubarrayWords(InputWords, LBracket, RBracket);
end;

function BracedSubarray(InputWords: TStringArray): TStringArray;
begin
  result := BalancedDelimiterSubarrayWords(InputWords, LBrace, RBrace);
end;

function BracketedSubstring(InputStr: String): String;
begin
  result := BalancedDelimiterSubstringWords(InputStr, LBracket, RBracket);
end;

function BracketedSubstring(InputStr: String): String;
begin
  result := BalancedDelimiterSubstringWords(InputStr, LBrace, RBrace);
end;



function CommandArg(InputStr, Command, StartDelim, EndDelim: String): String;
var
  OutputStr: String = '';
  TestStr: String;
begin
  if InputStr.Contains(Command) then
  begin
    TestStr := SubstringAfter(InputStr, Command);
    OutputStr := BalancedDelimiterSubstringWords(TestStr, StartDelim, EndDelim);
  end;
  result := OutputStr;
end;

function CommandArgBraces(InputStr, Command: String): String;
begin
  result := CommandArg(InputStr, Command, '{', '}');
end;

function CommandArgAngleBrackets(InputStr, Command: String): String;
begin
  result := CommandArg(InputStr, Command, '<<', '>>');
end;

end.
