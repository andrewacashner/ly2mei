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
  { This class is a macro dictionary of key-value pairs. }
  TMacroDict = class(specialize TDictionary<String, String>)
  public

    { Return the contents of a macro dictionary as a string, for
    testing/debugging purposes. }
    function ToString: String; override;
    
  end;

{ Find, parse, and save macro definitions in a stringlist. Return a macro
  dictionary; if no valid macros are found, it will be empty. Values may
  contain unexpanded macros.

  The function also modifies the given stringlist to remove macro definitions.

  A macro must have the form @code(label = < arg >) or 
  @code(label = \command < arg >) where @code(<>) are curly braces. We
  don't accept @code(label = "string") or other formats. The label must be
  at the beginning of a line. }
procedure ExtractMacros(SourceLines: TStringListAAC; Dict: TMacroDict);

type
  { A single key-value pair used in the dictionary. }
  TMacroKeyValue = TMacroDict.TDictionaryPair;


{ In a given string (not a list), replace all macro commands (@code(\command))
  with the corresponding definition in a macro dictionary. Repeat as necessary
  until all known macros are expanded.

  A macro call must be followed by a space or newline. Otherwise we could not
  have commands like @code(\SopranoI) and @code(\SopranoII). }
function FindReplaceMacros(Source: String; Dict: TMacroDict): String;

{ Process macros in the source text and return a stringlist with all macros
  expanded, including nested ones. }
function ExpandMacros(SourceLines: TStringListAAC): TStringListAAC;


implementation

function TMacroDict.ToString: String;
var 
  Macro: TMacroKeyValue;
  OutputStr, MacroStr: String;
  N: Integer;
begin
  OutputStr := '';
  N := 1;
  for Macro in Self do
  begin
    MacroStr := IntToStr(N) + '. ' + Macro.Key + ': ' + Macro.Value + LineEnding;
    OutputStr := OutputStr + MacroStr;
    Inc(N);
  end;
  result := OutputStr;
end;
    
function FindReplaceMacros(Source: String; Dict: TMacroDict): String;
var
  OutputStr: String;
  Macro: TMacroKeyValue;
  HasMacros: Boolean;
begin
  OutputStr := Source;
  HasMacros := True;
  while HasMacros do
  begin
    for Macro in Dict do
    begin
      OutputStr := StringReplace(OutputStr, 
        Macro.Key + ' ', Macro.Value + ' ', [rfReplaceAll]);
      OutputStr := StringReplace(OutputStr, 
        Macro.Key + LineEnding, Macro.Value + LineEnding, [rfReplaceAll]);
    end;
    HasMacros := False;
    for Macro in Dict do
      if OutputStr.Contains(Macro.Key) then
        HasMacros := True;
  end;
  result := OutputStr;
end;

procedure ExtractMacros(SourceLines: TStringListAAC; Dict: TMacroDict);
var
  InputStr, BufferStr, ThisString, NextStr, Key, Value, TestStr: String;
  LineIndex: Integer;
  FindOutline, CopyOutline: TIndexPair;
  CommandArg: TCommandArg;
  Found: Boolean;
begin
  assert(SourceLines <> nil);
  FindOutline := TIndexPair.Create;
  CopyOutline := TIndexPair.Create;
  CommandArg := TCommandArg.Create;
  try
    LineIndex := 0;
    CopyOutline.FStart := 0;
    CopyOutline.FValid := True;
    BufferStr := '';
    InputStr := SourceLines.Text;
    { Look for a @code(key = value) pair at the start of each line }
    for ThisString in SourceLines do
    begin
      Found := False;
      if ThisString.Contains('=') and not ThisString.StartsWith(' ') then
      begin
        SourceLines.GetNameValue(LineIndex, Key, Value);
        if Key.IsEmpty or Value.IsEmpty then
          continue;
        
        { Found key, mark start location }
        Key := Key.Trim;
        CopyOutline.FEnd := InputStr.IndexOf(Key + ' ');

        { Parse value }
        Value := Value.Trim;
        case Value.Substring(0, 1) of
        '{':
          { Value is a brace-delimited argument }
          begin
            TestStr := SourceLines.ToStringFromIndex(LineIndex);
            FindOutline := FindMatchedBraces(TestStr, FindOutline);
            if FindOutline.IsValid then
            begin
              Value := CopyStringRange(TestStr, FindOutline, rkInclusive);
              Found := True;
            end;
          end;

        '\':
          { Value is a command, possibly followed by argument }
          begin
            TestStr := SourceLines.ToStringFromIndex(LineIndex);
            CommandArg := CommandArg.ExtractFromString(TestStr, '\', '{', '}');
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
                Value := CommandArg.ToString;
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
          NextStr := InputStr.Substring(CopyOutline.FStart, CopyOutline.Span);
          if not NextStr.IsNullOrWhitespace(NextStr) then
          begin
            BufferStr := BufferStr + InputStr.Substring(CopyOutline.FStart,
              CopyOutline.Span);
          end;
          CopyOutline.FStart := InputStr.IndexOf(Value) + Length(Value);
        end;
      end;
      Inc(LineIndex);
    end;
    { Add remaining text after last macro definition to output }
    BufferStr := BufferStr + InputStr.Substring(CopyOutline.FStart);
    FreeAndNil(SourceLines);
    SourceLines := TStringListAAC.Create(BufferStr);

  finally
    FreeAndNil(CommandArg);
    FreeAndNil(CopyOutline);
    FreeAndNil(FindOutline);
  end;
end;

function ExpandMacros(SourceLines: TStringListAAC): TStringListAAC;
var
  Macros: TMacroDict;
  TempStr: String;
begin
  Macros := TMacroDict.Create;
  try
    ExtractMacros(SourceLines, Macros);
    TempStr := FindReplaceMacros(SourceLines.Text, Macros);
    FreeAndNil(SourceLines);
    SourceLines := TStringListAAC.Create(TempStr);
    SourceLines.RemoveBlankLines;
  finally
    FreeAndNil(Macros);
    result := SourceLines;
  end;
end;

end.
