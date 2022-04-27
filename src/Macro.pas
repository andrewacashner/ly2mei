{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} 

{ @abstract(Find, store, and expand Lilypond macros)
  @author(Andrew Cashner)

  Treat Lilypond variable definitions as simple text macros. Find them, make a
  dictionary of them, and then expand them in the text, including nested
  macros. }
unit Macro;

interface

uses SysUtils, StrUtils, Classes, Generics.Collections, StringTools, Outline;

type
  TIndexList = specialize TList<Integer>;

  { @abstract(A macro dictionary of key-value pairs.) }
  TMacroDict = specialize TDictionary<String, String>;

  { A single key-value pair used in the dictionary. }
  TMacroKeyValue = TMacroDict.TDictionaryPair;

{ Find, parse, and save macro definitions in a stringlist. Return a macro
  dictionary; if no valid macros are found, it will be empty. Values may
  contain unexpanded macros.

  The function also modifies the given stringlist to remove macro definitions.

  A macro must have the form @code(label = < arg >) or 
  @code(label = \command < arg >) where @code(<>) are curly braces. We
  don't accept @code(label = "string") or other formats. The label must be
  at the beginning of a line. }
function ProcessMacros(InputLines: TStringList): TStringList;

{ In a given stringlist, replace all macro commands (@code(\command))
  with the corresponding definition in a macro dictionary. Repeat as necessary
  until all known macros are expanded.

  A macro call must be followed by a space or newline. Otherwise we could not
  have commands like @code(\SopranoI) and @code(\SopranoII). }
function FindReplaceMacros(InputStr: String; Dict: TMacroDict): String; 

{ Write out Lilypond multimeasure rests as individual rests: @code(| R1*2)
becomes @code(| R1\n| R1). }
function ExpandMultiRests(InputLines: TStringList): TStringList;

{ Process macros in the source text: modify the stringlist by expanding all
  macros, including nested ones. Also expand multimeasure rests. }
function ExpandMacros(InputLines: TStringList): TStringList;

implementation

function ContainsMacros(TestStr: String; Dict: TMacroDict): Boolean;
var
  MacroTest: Boolean = False;
  Macro: TMacroKeyValue;
begin
  for Macro in Dict do
  begin
    MacroTest := TestStr.Contains(Macro.Key);
  end;
  result := MacroTest;
end;

function FindReplaceMacros(InputStr: String; Dict: TMacroDict): String; 
var
  OutputStr: String;
  Macro: TMacroKeyValue;
begin
  OutputStr := InputStr;
  while ContainsMacros(OutputStr, Dict) do
  begin
    for Macro in Dict do
    begin
      OutputStr := OutputStr.Replace(Macro.Key + ' ', 
        Macro.Value + ' ', [rfReplaceAll]);
      OutputStr := OutputStr.Replace(Macro.Key + LineEnding, 
        Macro.Value + LineEnding, [rfReplaceAll]);
    end;
  end;
  result := OutputStr;
end;

function GetMacroDefKey(InputStr: String): String;
var
  Key: String = '';
begin
  if (not InputStr.StartsWith(' ')) and InputStr.Contains('=') then
  begin
    Key := StringDropAfter(InputStr, '=');
  end;
  result := Key;
end;


function ListMacroDefinitionLines(LineList: TStringList): TIndexList;
var
  IndexList: TIndexList;
  ThisLine: String;
  LineIndex: Integer;
begin
  IndexList := TIndexList.Create;
  LineIndex := 0;
  for ThisLine in LineList do
  begin
    if not ThisLine.StartsWith(' ') and ThisLine.Contains(' = ') then
    begin
      IndexList.Add(LineIndex);
    end;
    Inc(LineIndex);
  end;
  result := IndexList;
end;

function CopyBraceExpression(InputStr: String): String;
var
  IndexPair: TIndexPair;
  OutputStr: String = '';
begin
  IndexPair := FindMatchedBraces(InputStr);
  if IsValid(IndexPair) then
  begin
    OutputStr := InputStr.Substring(IndexPair.FStart, IndexPair.FSpan);
  end;
  result := OutputStr;
end;

function DeleteMacroDefinitions(InputStr: String; Dict: TMacroDict): String;
var
  OutputStr, Definition: String;
  ThisKeyValue: TMacroKeyValue;
begin
  OutputStr := InputStr;
  for ThisKeyValue in Dict do
  begin
    Definition := ThisKeyValue.Key + ' = ' + ThisKeyValue.Value;
    OutputStr := OutputStr.Replace(Definition, '');
  end;
  result := OutputStr;
end;

function ProcessMacros(InputLines: TStringList): TStringList;
var
  OutputLines: TStringList;
  IndexList: TIndexList;
  ThisIndex: Integer;
  InputStr, OutputStr, TestStr, Key, Value, Command, Arg: String;
  Dict: TMacroDict;
begin
  Dict := TMacroDict.Create;
  IndexList := ListMacroDefinitionLines(InputLines);
  for ThisIndex in IndexList do
  begin
    Key := StringDropAfter(InputLines[ThisIndex], ' = ');
    Value := '';

    TestStr := ToStringFromIndex(InputLines, ThisIndex);
    TestStr := TestStr.Substring(Length(Key + ' = '));
    
    if TestStr.StartsWith('{') then
    begin
      Value := CopyBraceExpression(TestStr);
    end
    else if TestStr.StartsWith('\') then
    begin
      Command := StringDropAfter(TestStr, '{');
      Arg := StringDropBefore(TestStr, Command);
      Value := Command + CopyBraceExpression(Arg);
    end;

    if not Key.IsEmpty and not Value.IsEmpty then
      Dict.Add(Key, Value);
  end;

  InputStr := InputLines.Text;
  { TODO make these handle stringlists too? }
  OutputStr := DeleteMacroDefinitions(InputStr, Dict);
  OutputStr := FindReplaceMacros(OutputStr, Dict);
  OutputLines := Lines(OutputStr);
  
  InputLines.Assign(OutputLines);

  FreeAndNil(IndexList);
  FreeAndNil(OutputLines);
  FreeAndNil(Dict);
  result := InputLines;
end;

function IsBarRest(InputStr: String): Boolean;
begin
  result := InputStr.TrimLeft.StartsWith('|') and InputStr.Contains(' R');
end;

function CopyMultiRest(InputStr: String): String;
var
  RestStr: String;
begin
  RestStr := StringDropBefore(InputStr, ' R');
  RestStr := ExtractWord(1, RestStr, [' ']);
  result := RestStr;
end;

function RestCount(InputStr: String): Integer;
var
  RestData: Array of String;
  Count: Integer = 0;
begin
  RestData := InputStr.Split('*', 2);
  if (Length(RestData) = 2) and not RestData[1].IsEmpty then
  begin
    Count := RestData[1].ToInteger;
  end;
  result := Count;
end;

function ExpandMultiRests(InputLines: TStringList): TStringList;
var
  ThisLine, RestStr, DurStr: String;
  Repeats, ThisRest: Integer;
  OutputLines: TStringList;
begin
  OutputLines := TStringList.Create;
  for ThisLine in InputLines do
  begin
    if IsBarRest(ThisLine) then
    begin
      RestStr := CopyMultiRest(ThisLine);
      Repeats := RestCount(RestStr);
      DurStr := CopyStringBetween(RestStr, 'R', '*');

      for ThisRest := 0 to Repeats - 1 do
        OutputLines.Add('| R' + DurStr);
    end
    else
      OutputLines.Add(ThisLine);
  end;
  InputLines.Assign(OutputLines);
  FreeAndNil(OutputLines);
  result := InputLines;
end;

function ExpandMacros(InputLines: TStringList): TStringList;
begin
  InputLines := RemoveComments(InputLines);
  InputLines := ProcessMacros(InputLines);
  InputLines := RemoveBlankLines(InputLines);
  InputLines := ExpandMultiRests(InputLines);
  result := InputLines;
end;

   
end.
