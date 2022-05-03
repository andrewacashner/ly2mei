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

{ Return the portion of a string before a given delimiter. }
function StringDropAfter(Source, EndBefore: String): String;

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

end.
