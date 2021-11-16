{$mode objfpc}{$H+}{$J-}{$ASSERTIONS+}

{ @abstract(Utilities for handling strings including XML generation.)
  @author(Andrew Cashner) }
unit StringTools;

interface

uses SysUtils, Classes;

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

{ Create an XML attribute string: @code(key = "value") }
function XMLAttribute(Tag, Value: String): String;

{ Create an XML element string, with optional attributes }
function XMLElement(Tag, Contents: String; Attributes: String = ''): String;

type 
  { Custom string list }
  TStringListAAC = class(TStringList)
  public
    constructor Create;

    { Create a string list from a string by splitting at newlines }
    constructor Create(InputStr: String);

    { Return a string consisting of the text of a stringlist starting at a
      given index. }
    function ToStringFromIndex(Index: Integer): String;
    
    { Modify a stringlist to strip out everything between a comment char and
      the next newline. }
    procedure RemoveComments;
    
    { Modify a stringlist to delete lines that are empty or contain only
      whitespace. }
    procedure RemoveBlankLines;

    { Enclose list contents inside a given XML tag and optional attributes }
    procedure EncloseInXML(
      { Text of XML tag }
      Tag: String;
      { @bold(Optional): Attributes to be included in opening tag }
      Attributes: String = '');

    function XMLDocStr(RootElement: String = 'xml';
      RootAttributes: String = ''): String;

    function MEIDocStr: String;
  end;


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

function XMLAttribute(Tag, Value: String): String;
begin
  result := Tag + '="' + Value + '"';
end;

function XMLElement(Tag, Contents: String; Attributes: String = ''): String;
begin
  result := '<' + Tag + Attributes + '>' + Contents + '</' + Tag + '>';
end;


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

function TStringListAAC.ToStringFromIndex(Index: Integer): String;
begin
  result := Self.Text.Substring(Self.Text.IndexOf(Self[Index]));
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
    if Self[Index].Trim.IsEmpty then
      Self.Delete(Index);
end;

procedure TStringListAAC.EncloseInXML(Tag: String; Attributes: String = '');
var
  HeadTag: String;
  Index: Integer;
begin
  HeadTag := Tag;
  if Attributes <> '' then
    HeadTag := Tag + ' ' + Attributes;
  
  for Index := Count -1 downto 0 do
    Self[Index] := IndentStr + Self[Index];
 
  Self.Insert(0, '<' + HeadTag + '>');
  Self.Add('</' + Tag + '>');
end;

function TStringListAAC.XMLDocStr(RootElement: String = 'xml';
  RootAttributes: String = ''): String;
const
  XMLversion = '<?xml version="1.0" encoding="UTF-8"?>';
begin
  Self.EncloseInXML(RootElement, RootAttributes);
  Self.Insert(0, XMLversion);
  result := Self.Text;
end;

function TStringListAAC.MEIDocStr: String;
const
  MeiNamespace = 'xmlns="http://www.music-encoding.org/ns/mei" meiVersion="4.0.0"';
begin
  result := Self.XMLDocStr('mei', MEINamespace);
end;

end.
