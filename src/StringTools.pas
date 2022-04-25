{$mode objfpc}{$H+}{$J-}

{ @abstract(Utilities for handling strings and stringlists, all functional.)
  @author(Andrew Cashner) }
unit StringTools;

interface

uses SysUtils, Classes;

{ Write notes to standard error if compiled with @code(-dDEBUG) }
procedure DebugLn(Msg: String);

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

{ Return a new stringlist containing the text of an input string, separated at
  newlines. }
function Lines(InputStr: String): TStringList;

{ Return a new string list that is a copy of another string list, starting at
  a given index }
function CopyFromIndex(SourceList: TStringList; StartIndex: Integer): TStringList;

{ Return a string consisting of the text of a stringlist starting at a
  given index. }
function ToStringFromIndex(SourceLines: TStringList; Index: Integer): String; 

{ Return a copy of the given stringlist with comments removed. Strip out
  everything between a comment char (@code('%')) and the next newline. }
function RemoveComments(InputStr: String): String;

{ Return a copy of the given stringlist with blank lines removed. Blank lines
  are those that are empty or contain only whitespace. }
function RemoveBlankLines(InputStr: String): String;

implementation

procedure DebugLn(Msg: String);
begin
  {$ifdef DEBUG}
  WriteLn(stderr, '> ' + Msg);
  {$endif}
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

function Lines(InputStr: String): TStringList;
var
  OutputLines: TStringList;
begin
  OutputLines := TStringList.Create;
  with OutputLines do
  begin
    Delimiter := LineEnding;
    StrictDelimiter := True;
    DelimitedText := InputStr;
  end;
  result := OutputLines;
end;

function CopyFromIndex(SourceList: TStringList; StartIndex: Integer): TStringList;
var
  ThisIndex: Integer;
  OutputLines: TStringList;
begin
  OutputLines := TStringList.Create;
  for ThisIndex := StartIndex to SourceList.Count - 1 do
  begin
    OutputLines.Add(SourceList[ThisIndex]);
  end;
  result := OutputLines;
end;

function ToStringFromIndex(SourceLines: TStringList; Index: Integer): String; 
var
  NewLines: TStringList;
  OutputStr: String;
begin
  NewLines := CopyFromIndex(SourceLines, Index);
  OutputStr := NewLines.Text;
  FreeAndNil(NewLines);
  result := OutputStr;
end;

function RemoveComments(InputStr: String): String;
var
  OutputStr, ThisString, CleanString: String;
  InputLines, OutputLines: TStringList;
begin
  OutputLines := TStringList.Create;
  InputLines := Lines(InputStr);
  for ThisString in InputLines do
  begin
    if not ThisString.StartsWith('%') then
    begin
      CleanString := StringDropAfter(ThisString, '%');
      OutputLines.Add(CleanString);
    end;
  end;
  OutputStr := OutputLines.Text;
  FreeAndNil(InputLines);
  FreeAndNil(OutputLines);
  result := OutputStr;
end;

function RemoveBlankLines(InputStr: String): String;
var
  ThisString, OutputStr: String;
  InputLines, OutputLines: TStringList;
begin
  OutputLines := TStringList.Create;
  InputLines := Lines(InputStr);
  for ThisString in InputLines do
  begin
    if not ThisString.Trim.IsEmpty then
      OutputLines.Add(ThisString);
  end;
  OutputStr := OutputLines.Text;
  FreeAndNil(InputLines);
  FreeAndNil(OutputLines);
  result := OutputStr;
end;


end.
