{$mode objfpc}{$H+}{$J-}

{ @abstract(Utilities for handling strings including XML generation.)
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

type 
  { @abstract(Custom string-list class with added methods) }
  TStringListAAC = class(TStringList)
  public
    constructor Create;

    { Create a string list from a string by splitting at newlines }
    constructor Create(InputStr: String);

    { Create a string list that is a copy of another string list, starting at
    a given index }
    constructor CreateCopyFromIndex(SourceList: TStringList;
      StartIndex: Integer); 

    { Return a string consisting of the text of a stringlist starting at a
      given index. }
    function ToStringFromIndex(Index: Integer): String;
    
    { Modify a stringlist to strip out everything between a comment char and
      the next newline. }
    procedure RemoveComments;
    
    { Modify a stringlist to delete lines that are empty or contain only
      whitespace. }
    procedure RemoveBlankLines;
  end;


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

{ TODO evaluate if we really need stringlists like this }
constructor TStringListAAC.Create;
begin
  inherited Create;
end;

constructor TStringListAAC.Create(InputStr: String);
begin
  Delimiter := LineEnding;
  StrictDelimiter := True;
  DelimitedText := InputStr;
end;

constructor TStringListAAC.CreateCopyFromIndex(SourceList: TStringList;
  StartIndex: Integer); 
var
  ThisIndex: Integer;
begin
  inherited Create;
  for ThisIndex := StartIndex to SourceList.Count - 1 do
  begin
    Self.Add(SourceList[ThisIndex]);
  end;
end;

function TStringListAAC.ToStringFromIndex(Index: Integer): String;
var
  NewStringList: TStringListAAC;
  OutputStr: String;
begin
  NewStringList := TStringListAAC.CreateCopyFromIndex(Self, Index);
  OutputStr := NewStringList.Text;
  FreeAndNil(NewStringList);
  result := OutputStr;
end;

procedure TStringListAAC.RemoveComments;
var
  ThisString: String;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do 
  begin
    ThisString := Self[Index];
    if not ThisString.StartsWith('%') then
      Self[Index] := ThisString.Remove(ThisString.IndexOf('%'));
  end;
end;

procedure TStringListAAC.RemoveBlankLines;
var 
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    if Self[Index].Trim.IsEmpty then
      Self.Delete(Index);
  end;
end;

end.
