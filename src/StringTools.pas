{$mode objfpc}{$H+}{$J-}{$ASSERTIONS+}

{ @abstract(Utilities for handling strings including XML generation.)
  @author(Andrew Cashner) }
unit StringTools;

interface

uses SysUtils, StrUtils, Classes;

{ Write notes to standard error if compiled with @code(-dDEBUG) }
procedure DebugLn(Msg: String);

{ Copy the portion of a string that follows the end of a given substring. }
function StringDropBefore(Source, Cut: String): String;

{ Return the portion of a string before a given delimiter. }
function StringDropAfter(InputStr: String; Delim: String): String;

{ Is this a single quoted string without any other quotes within it? }
function IsASingleQuotedString(Source: String): Boolean;

{ Return a string of spaces for indenting, 2 spaces for each degree of
indentation. }
function IndentStr(Degree: Integer = 1): String;

{ Split a string at newlines to make a @code(TStringList) }
function Lines(InputStr: String; OutputList: TStringList): TStringList;

{ Return a string consisting of the text of a stringlist starting at a given
  index. }
function ListToStringFromIndex(List: TStringList; Index: Integer): String;

{ Modify a stringlist to strip out everything between a comment char and the
next newline. }
function RemoveComments(InputLines: TStringList): TStringList;

{ Modify a stringlist to delete lines that are empty or contain only
whitespace. }
function RemoveBlankLines(InputLines: TStringList): TStringList;


{ Enclose the contents of a given @code(TStringList) inside a given XML tag
  and optional attributes; put the result into the secong given stringlist. 

  TODO Can we do this (and other similar functions) without needing to create
  a new stringlist and pass it as an argument? 
}
function XMLElementLines(
  { Text to be enclosed in XML element }
  InputLines: TStringList;
  { List to store new text }
  OutputLines: TStringList; 
  { Text of XML tag }
  Tag: String;
  { @bold(Optional): Attributes to be included in opening tag }
  Attributes: String = ''): TStringList; 


implementation

procedure DebugLn(Msg: String);
begin
  {$ifdef DEBUG}
  WriteLn(stderr, '> ' + Msg);
  {$endif}
end;

function StringDropBefore(Source, Cut: String): String;
begin
  result := Source.Substring(Source.IndexOf(Cut) + Length(Cut));
end;

function StringDropAfter(InputStr: String; Delim: String): String;
begin
  if InputStr.Contains(Delim) then
    InputStr := InputStr.Substring(0, InputStr.IndexOf(Delim));
  result := InputStr;
end;

function IsASingleQuotedString(Source: String): Boolean;
begin
  result := (Source.CountChar('"') = 2)
            and Source.StartsWith('"') 
            and Source.EndsWith('"');
end;

function IndentStr(Degree: Integer = 1): String;
begin
  result := StringOfChar(' ', 2 * Degree);
end;

function Lines(InputStr: String; OutputList: TStringList): TStringList;
begin
  OutputList.Clear;
  OutputList.Delimiter := LineEnding;
  OutputList.StrictDelimiter := True;
  OutputList.DelimitedText := InputStr;
  result := OutputList;
end;

function ListToStringFromIndex(List: TStringList; Index: Integer): String;
begin
    result := List.Text.Substring(List.Text.IndexOf(List[Index]));
end;

function RemoveComments(InputLines: TStringList): TStringList;
var
  ThisString: String;
  TempLines: TStringList;
begin
  assert(InputLines <> nil);
  TempLines := TStringList.Create;
  try
    for ThisString in InputLines do
    begin
      if not ThisString.StartsWith('%') then
        TempLines.Add(StringDropAfter(ThisString, '%'));
    end;
    InputLines.Assign(TempLines);
  finally
    FreeAndNil(TempLines);
    result := InputLines;
  end;
end;

function RemoveBlankLines(InputLines: TStringList): TStringList;
var 
  ThisString: String;
  TempLines: TStringList;
begin
  assert(InputLines <> nil);
  TempLines := TStringList.Create;
  try
    for ThisString in InputLines do
    begin
      if not ThisString.Trim.IsEmpty then
        TempLines.Add(ThisString);
    end;
    InputLines.Assign(TempLines);
  finally
    FreeAndNil(TempLines);
    result := InputLines;
  end;
end;

function XMLElementLines(InputLines: TStringList; OutputLines: TStringList;
  Tag: String; Attributes: String = ''): TStringList; 
var
  HeadTag: String;
  ThisLine: String;
begin
  assert(InputLines <> nil);
  assert(OutputLines <> nil);
  if Attributes = '' then
    HeadTag := Tag
  else 
    HeadTag := Tag + ' ' + Attributes;

  OutputLines.Clear;
  OutputLines.Add('<' + HeadTag + '>');
  for ThisLine in InputLines do
    OutputLines.Add(IndentStr + ThisLine);
  OutputLines.Add('</' + Tag + '>');
  result := OutputLines;
end;

end.
