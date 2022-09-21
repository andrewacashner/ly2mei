{$mode objfpc}{$H+}{$J-}

{ @abstract(Find, store, and expand Lilypond macros)
  @author(Andrew Cashner)

  Treat Lilypond variable definitions as simple text macros. Find them, make a
  dictionary of them, and then expand them in the text, including nested
  macros. 
}
unit Macro;

interface

uses SysUtils, StrUtils, Classes, Generics.Collections, StringTools, Outline;

type
  { @abstract(A list of integer indices corresponding to lines that start with
    a macro definition.) }
  TIndexList = class(specialize TList<Integer>)
  public
    { Macro definitions must start at the beginning of the line, and be in the
    format @code(label = ❴ value ❵) or @code(label = \command ❴ value ❵). }
    constructor Create(InputLines: TStringListPlus);
  end;

  { @abstract(A macro dictionary of key-value pairs.) }
  TMacroDict = class(specialize TDictionary<String, String>)
  public
    constructor Create(InputLines: TStringListPlus);
  end;

  { A single key-value pair used in the dictionary. }
  TMacroKeyValue = TMacroDict.TDictionaryPair;

{ Find all commands to include other files, of the form @code(\include
  "file.ly"), and insert the content of those files at that point in the
  string list. }
function IncludeFiles(InputLines: TStringListPlus): TStringListPlus;

{ Find, parse, and save macro definitions in a stringlist. Delete
  the definition expressions and expand the macro calls.

  A macro must have the form @code(label = ❴ arg ❵) or 
  @code(label = \command ❴ arg ❵). We don't accept @code(label = "string") or
  other formats. The label must be at the beginning of a line. 

  (TODO: The program actually does accept @code(label = "string"), but was
  this intentional or a byproduct?)

  A macro call must be followed by a space or newline. Otherwise we could not
  have commands like @code(\SopranoI) and @code(\SopranoII). }
function ProcessMacros(InputLines: TStringListPlus): TStringListPlus;

{ Write out Lilypond multimeasure rests as individual rests: @code(| R1*2)
  becomes @code(| R1\n| R1). }
function ExpandMultiRests(InputLines: TStringListPlus): TStringListPlus;

{ Modify the stringlist: Clean up text (remove comments, blank lines) and
  expand macros and multirests. }
function ExpandMacros(InputLines: TStringListPlus): TStringListPlus;

implementation

constructor TIndexList.Create(InputLines: TStringListPlus);
var
  ThisLine: String;
  LineIndex: Integer;
begin
  inherited Create;
  LineIndex := 0;
  for ThisLine in InputLines do
  begin
    if not ThisLine.StartsWith(' ') and ThisLine.Contains(' = ') then
    begin
      Self.Add(LineIndex);
    end;
    Inc(LineIndex);
  end;
end;

constructor TMacroDict.Create(InputLines: TStringListPlus);
  
  function CopyCommandArg(InputStr: String): String;
  var
    Command, Arg: String;
    OutputStr: String = '';
  begin
    Command := StringDropAfter(InputStr, '{');
    Arg := StringDropBefore(InputStr, Command);
    OutputStr := Command + CopyBraceExpr(Arg);
    result := OutputStr;
  end;

  function CopyKey(InputStr, Delim: String): String;
  begin
    result := StringDropAfter(InputStr, Delim);
  end;

  function CopyAfterKey(InputStr, Key, Delim: String): String;
  begin
    result := InputStr.Substring(Length(Key + Delim));
  end;

  function FindMacroValue(InputStr: String): String;
  var
    Value: String = '';
  begin
    if InputStr.StartsWith('{') then
      Value := CopyBraceExpr(InputStr)
    else if InputStr.StartsWith('\') then
      Value := CopyCommandArg(InputStr);

    result := Value;
  end;

const
  MacroDefDelim: String = ' = ';
var
  IndexList: TIndexList;
  ThisIndex: Integer;
  TestStr, Key, Value: String;
begin
  inherited Create;
  IndexList := TIndexList.Create(InputLines);
  for ThisIndex in IndexList do
  begin
    Key := CopyKey(InputLines[ThisIndex], MacroDefDelim);

    TestStr := ToStringFromIndex(InputLines, ThisIndex);
    TestStr := CopyAfterKey(TestStr, Key, MacroDefDelim);
    Value := FindMacroValue(TestStr);

    if not Key.IsEmpty and not Value.IsEmpty then
      Self.Add(Key, Value);
  end;
  FreeAndNil(IndexList);
end;

function IncludeFiles(InputLines: TStringListPlus): TStringListPlus;
var
  FileLines, OutputLines: TStringListPlus;
  ThisStr, TestStr, FileName: String;
begin
  OutputLines := TStringListPlus.Create();
  FileLines := TStringListPlus.Create();

  for ThisStr in InputLines do
  begin
    { Copy the input file, but replace lines with command 
      @code(\include "file.ly") with contents of named file.
      If the file can't be found, just skip this line. }
    if not ThisStr.Contains('\include') then
    begin
      OutputLines.Add(ThisStr);
    end
    else
    begin
      TestStr := SubstringAfter(ThisStr, '\include ').Trim;
      FileName := CopyFirstQuotedString(TestStr);
      if not FileExists(FileName) then
      begin
        WriteLn(stderr, 'File not found: Could not include file "' + FileName + '"');
      end
      else
      begin
        try
          FileLines.LoadFromFile(FileName);
          OutputLines.AddStrings(FileLines);
        except
          on Error: EInOutError do
            WriteLn(stderr, 'File handling error occurred. Reason: ', Error.Message);
        end;
      end;
    end;
  end;
 
  InputLines.Assign(OutputLines);
  FreeAndNil(OutputLines);
  FreeAndNil(FileLines);
  result := InputLines;
end;


function ProcessMacros(InputLines: TStringListPlus): TStringListPlus;

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

  { In a given string, replace all macro commands with the corresponding
  definition in a macro dictionary. Repeat as necessary
    until all known macros are expanded. } 
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
        OutputStr := OutputStr.Replace('\' + Macro.Key + ' ', 
          Macro.Value + ' ', [rfReplaceAll]);
        OutputStr := OutputStr.Replace('\' + Macro.Key + LineEnding, 
          Macro.Value + LineEnding, [rfReplaceAll]);
      end;
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

var
  Dict: TMacroDict;
  TempLines: TStringListPlus;
  InputStr, OutputStr: String;
begin
  Dict := TMacroDict.Create(InputLines);
  InputStr := InputLines.Text;
  OutputStr := DeleteMacroDefinitions(InputStr, Dict);
  OutputStr := FindReplaceMacros(OutputStr, Dict);
  
  TempLines := TStringListPlus.Create(OutputStr);
  InputLines.Assign(TempLines);
  FreeAndNil(TempLines);
  FreeAndNil(Dict);
  result := InputLines;
end;

function ExpandMultiRests(InputLines: TStringListPlus): TStringListPlus;

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

var
  ThisLine, RestStr, DurStr: String;
  Repeats, ThisRest: Integer;
  TempLines: TStringListPlus;
begin
  TempLines := TStringListPlus.Create;
  for ThisLine in InputLines do
  begin
    if IsBarRest(ThisLine) then
    begin
      RestStr := CopyMultiRest(ThisLine);
      Repeats := RestCount(RestStr);
      DurStr := CopyStringBetween(RestStr, 'R', '*');

      for ThisRest := 0 to Repeats - 1 do
        TempLines.Add('| R' + DurStr);
    end
    else
      TempLines.Add(ThisLine);
  end;
  InputLines.Assign(TempLines);
  FreeAndNil(TempLines);
  result := InputLines;
end;

function ExpandMacros(InputLines: TStringListPlus): TStringListPlus;
begin
  DebugLn('(Macro.ExpandMacros) Initial input text:');
  DebugLines(InputLines);

  if InputLines.Count > 0 then
  begin
    InputLines := IncludeFiles(InputLines);
    InputLines := InputLines.RemoveComments;
    InputLines := ProcessMacros(InputLines);
    InputLines := InputLines.RemoveBlankLines;
    InputLines := ExpandMultiRests(InputLines);
  end;

  DebugLn('(Macro.ExpandMacros) Input text with macros expanded:');
  DebugLines(InputLines);

  result := InputLines;
end;
   
end.
