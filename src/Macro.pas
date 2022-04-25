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
  { @abstract(A macro dictionary of key-value pairs.) }
  TMacroDict = specialize TDictionary<String, String>;

{ Find, parse, and save macro definitions in a stringlist. Return a macro
  dictionary; if no valid macros are found, it will be empty. Values may
  contain unexpanded macros.

  The function also modifies the given stringlist to remove macro definitions.

  A macro must have the form @code(label = < arg >) or 
  @code(label = \command < arg >) where @code(<>) are curly braces. We
  don't accept @code(label = "string") or other formats. The label must be
  at the beginning of a line. }
function ExtractMacros(InputStr: String; Dict: TMacroDict): String;

type
  { A single key-value pair used in the dictionary. }
  TMacroKeyValue = TMacroDict.TDictionaryPair;

{ In a given stringlist, replace all macro commands (@code(\command))
  with the corresponding definition in a macro dictionary. Repeat as necessary
  until all known macros are expanded.

  A macro call must be followed by a space or newline. Otherwise we could not
  have commands like @code(\SopranoI) and @code(\SopranoII). }
function FindReplaceMacros(InputStr: String; Dict: TMacroDict): String; 

{ Write out Lilypond multimeasure rests as individual rests: @code(| R1*2)
becomes @code(| R1\n| R1). }
function ExpandMultiRests(InputStr: String): String;

{ Process macros in the source text: modify the stringlist by expanding all
  macros, including nested ones. Also expand multimeasure rests. }
function ExpandMacros(InputStr: String): String;


implementation

function FindReplaceMacros(InputStr: String; Dict: TMacroDict): String; 
var
  OutputStr: String;
  Macro: TMacroKeyValue;
  HasMacros: Boolean;
begin
  OutputStr := InputStr;
  HasMacros := True;
  while HasMacros do
  begin
    for Macro in Dict do
    begin
      OutputStr := OutputStr.Replace(Macro.Key + ' ', 
        Macro.Value + ' ', [rfReplaceAll]);
      OutputStr := OutputStr.Replace(Macro.Key + LineEnding, 
        Macro.Value + LineEnding, [rfReplaceAll]);
    end;
    HasMacros := False;
    for Macro in Dict do
      if OutputStr.Contains(Macro.Key) then
        HasMacros := True;
  end;
  result := OutputStr;
end;

function ExtractMacros(InputStr: String; Dict: TMacroDict): String;
var
  FindOutline, CopyOutline: TIndexPair;
  CommandArg: TCommandArg;
  OutputStr, ThisString, NextStr, Key, Value, TestStr: String;
  BufferStr: String = '';
  LineIndex: Integer = 0;
  Found: Boolean;
  InputLines: TStringList;
begin
  Assert(Assigned(Dict));
  CopyOutline.FStart := 0;
  CopyOutline.FValid := True;

  InputLines := Lines(InputStr);
  { Look for a @code(key = value) pair at the start of each line }
  for ThisString in InputLines do
  begin
    Found := False;
    if ThisString.Contains('=') and not ThisString.StartsWith(' ') then
    begin
      InputLines.GetNameValue(LineIndex, Key, Value);
      if Key.IsEmpty or Value.IsEmpty then
        continue;
      
      { Found key, mark start location }
      Key := Key.Trim;
      CopyOutline := SetEndSpan(CopyOutline, InputStr.IndexOf(Key + ' '));

      { Parse value }
      Value := Value.Trim;
      case Value.Substring(0, 1) of
      '{':
        { Value is a brace-delimited argument }
        begin
          TestStr := ToStringFromIndex(InputLines, LineIndex);
          FindOutline := FindMatchedBraces(TestStr);
          if FindOutline.FValid then
          begin
            Value := CopyStringRange(TestStr, FindOutline, rkInclusive);
            Found := True;
          end;
        end;

      '\':
        { Value is a command, possibly followed by argument }
        begin
          TestStr := ToStringFromIndex(InputLines, LineIndex);
          CommandArg := FindCommandArg(TestStr, '\', '{', '}');
          case CommandArg.FStatus of
          { Found only a command }
          skCommand:
            begin
              Value := CommandArg.FCommand;
              Found := True;
            end;
          { Found a command and a brace-delimited argument }
          skCommandArg:
            begin
              Value := Format('%s %s', [CommandArg.FCommand, CommandArg.FArg]);
              Found := True;
            end;
          end;
        end;
      end;
      { Add found key/value pair to dictionary; copy text from last ending
      position to next start position to output; mark new start position }
      if Found then
      begin
        Dict.Add('\' + Key, Value);
        NextStr := InputStr.Substring(CopyOutline.FStart, CopyOutline.FSpan);
        if not NextStr.IsEmpty then
        begin
          BufferStr := BufferStr + InputStr.Substring(CopyOutline.FStart,
            CopyOutline.FSpan);
        end;
        CopyOutline.FStart := InputStr.IndexOf(Value) + Length(Value);
      end;
    end;
    Inc(LineIndex);
  end;
  { Add remaining text after last macro definition to output }
  BufferStr := BufferStr + InputStr.Substring(CopyOutline.FStart);
  OutputStr := BufferStr;

  FreeAndNil(InputLines);

  result := OutputStr;
end;

function ExpandMultiRests(InputStr: String): String;
var
  OutputStr, ThisLine, RestStr, DurStr: String;
  RestData: Array of String;
  Repeats, RestCount: Integer;
  InputLines, OutputLines: TStringList;
begin
  OutputLines := TStringList.Create;
  InputLines := Lines(InputStr);

  for ThisLine in InputLines do
  begin
    if ThisLine.TrimLeft.StartsWith('|') and ThisLine.Contains(' R') then
    begin
      RestStr := StringDropBefore(ThisLine, ' R');
      RestStr := ExtractWord(1, RestStr, [' ']);
      RestData := RestStr.Split('*', 2);
      DurStr := RestData[0];
      
      if (Length(RestData) = 2) and not RestData[1].IsEmpty then
      begin
          Repeats := RestData[1].ToInteger;
      
          for RestCount := Repeats - 1 downto 0 do
          begin
            OutputLines.Add('| R' + DurStr);
          end;
      end;
    end
    else
      OutputLines.Add(ThisLine);
  end;
  OutputStr := OutputLines.Text;

  FreeAndNil(InputLines);
  FreeAndNil(OutputLines);

  result := OutputStr;
end;

function ExpandMacros(InputStr: String): String;
var
  Macros: TMacroDict;
  OutputStr: String;
begin
  Macros := TMacroDict.Create;
  OutputStr := RemoveComments(InputStr);

  { TODO procedure modifies both arguments }
  OutputStr := ExtractMacros(OutputStr, Macros);
  
  OutputStr := FindReplaceMacros(OutputStr, Macros);
  OutputStr := ExpandMultiRests(RemoveBlankLines(OutputStr));

  FreeAndNil(Macros);
  result := OutputStr;
end;

   
end.
