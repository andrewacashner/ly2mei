{ `lymacro`

  Andrew Cashner, 2021/10/07--12
 
  A basic macro-substitution program targeting Lilypond code.
}

{ NOTE: To avoid the appearance of nested comments and therefore bad
    highlighting and compiler warnings, we are writing `< arg >` instead of
    the TeX-style curly braces that Lilypond actually uses (`{ arg }`). }

{  Lilypond allows users to define macros like this:
  
  ~~~~
  MusicSoprano = < c'4 d'4 ees'4 >
  LyricsSoprano = \lyricmode < ly -- ric text >
  ~~~~
  
  Then they can call them like `\MusicSoprano` or `\LyricsSoprano`.
  Instead of actually processing the arguments like Lilypond does, we just
  want to do simple textual substitution of the argument for the macro label.
}

{$mode objfpc}{$H+}{$J-}
program lymacro(input, output, stderr);

uses SysUtils, StrUtils, Classes, Generics.Collections;

{ `DebugLn`

  Write notes to standard error if compiled with `-dDEBUG` }
procedure DebugLn(Msg: String);
begin
  {$ifdef DEBUG}
  WriteLn(stderr, '> ' + Msg);
  {$endif}
end;

{ `ReplaceString`

  Replace the first instance of one substring with another }
function ReplaceString(Source, Cut, Add: String): String;
var
  CutFrom, CutTo: Integer;
begin
  CutFrom := Source.IndexOf(Cut);

  if CutFrom = -1 then
    result := Source { no substring found }
  else
  begin
    CutTo := CutFrom + Length(Cut) + 1;
    result := Source.Substring(0, CutFrom) + Add + Source.Substring(CutTo);
  end;
end;

function StringDropBefore(InputStr: String; Delim: String): String;
var
  BreakPoint: Integer;
begin
  if InputStr.Contains(Delim) then
  begin
    BreakPoint := InputStr.IndexOf(Delim);
    InputStr := InputStr.Substring(BreakPoint, Length(InputStr) - BreakPoint);
  end;
  result := InputStr;
end;

function StringDropAfter(InputStr: String; Delim: String): String;
begin
  if InputStr.Contains(Delim) then
    InputStr := InputStr.Substring(0, InputStr.IndexOf(Delim));
  result := InputStr;
end;

function RemoveComments(InputLines: TStringList): TStringList;
var
  I: Integer;
begin
  for I := InputLines.Count - 1 downTo 0 do
  begin
    if InputLines[I].StartsWith('%') then
      InputLines.Delete(I)
    else
      InputLines[I] := StringDropAfter(InputLines[I], '%');
  end;
  result := InputLines;
end;

function RemoveBlankLines(InputLines: TStringList): TStringList;
var
  I: Integer;
begin
  for I := InputLines.Count - 1 downTo 0 do
  begin
    if InputLines[I].Trim.IsEmpty then
      InputLines.Delete(I);
  end;
  result := InputLines;
end;

{ `Lines`

  Split a string at newlines to make a `TStringList` }
function Lines(InputStr: String; OutputList: TStringList): TStringList;
begin
  OutputList.Clear;
  OutputList.Delimiter := LineEnding;
  OutputList.StrictDelimiter := True;
  OutputList.DelimitedText := InputStr;
  result := OutputList;
end;


{ Dictionary }
type
  TMacroDict     = specialize TDictionary<String, String>;
  TMacroKeyValue = TMacroDict.TDictionaryPair;

{ `TMacroOutline`

  Stores string indices that mark the positions of keys and values, and a
  flag to indicate if they are valid. }
type
  TMacroOutline = class 
  private
    FKeyStart, FKeyEnd, FValueStart, FValueEnd: Integer;
    FValid: Boolean;
  public
    procedure Clear;
    function KeyLength: Integer;
    function ValueLength: Integer;
    function MacroLength: Integer;
  end;

{ `TMacroOutline` class functions }
procedure TMacroOutline.Clear;
begin
  FKeyStart := 0;
  FKeyEnd := 0;
  FValueStart := 0;
  FValueEnd := 0;
  FValid := False;
end;

function TMacroOutline.KeyLength: Integer;
begin
  result := FKeyEnd - FKeyStart;
end;

function TMacroOutline.ValueLength: Integer;
begin
  result := FValueEnd - FValueStart;
end;

function TMacroOutline.MacroLength: Integer;
begin
  result := FValueEnd - FKeyStart;
end;

{ `MarkKey`

  Mark the start and end location of the first key in a string, with syntax
  `key = ...`. The key must be the first word in the line. Return an updated
  `TMacroOutline`; if no key found, mark the outline invalid. }
function MarkKey(Source: String; Outline: TMacroOutline): TMacroOutline;
var
  SplitPoint: Integer;
  Key: String;
begin
  SplitPoint := Source.IndexOf('=');
  Key := Source.Substring(0, SplitPoint - 1);
  DebugLn('testing possible key: ' + Key.Substring(0, 20));

  if Key = ExtractWord(1, Source, StdWordDelims) then
  begin
    Outline.FKeyStart := 0;
    Outline.FKeyEnd := SplitPoint;
    Outline.FValid := True;
    DebugLn('found key: start ' + IntToStr(Outline.FKeyStart) 
    + ', end: ' + IntToStr(Outline.FKeyEnd));
  end
  else
  begin
    Outline.FValid := False;
  end;
  result := Outline;
end;

{ `MarkMacro`

  Find a macro definition and return a structure with the four indices for the
  start and end of the key and of the value; if none found, return the object
  with `FValid` marked `False`. 

  - The KEY must be a single alphabetic word, starting at the beginning fo the
    line.  
  - The DELIMITER is `'='`, optionally surrounded by whitespace.
  - The VALUE can be any of the following:
      - A music expression enclosed in curly braces: `key = < music >`
      - A macro command with no arguments: `key = \macro`
      - A macro command with arguments: `key = \lyricmode < lyrics >` 

  NOTE we are not accepting `MarkupMacro = \markup "string"` as a valid
  definition, only `MarkupMacro = \markup < "string" >`.  }
function MarkMacro(Outline: TMacroOutline; Source:String): TMacroOutline;
type
  TReadMode = (rkNormal, rkCommand, rkBraceArgument);
var
  C, Value: String;
  SIndex, CommandStart, CommandEnd, ArgumentStart, ArgumentEnd,
  BraceLevel: Integer;
  CommandFound, ArgumentFound: Boolean;
  ReadMode: TReadMode;
begin
  Outline.Clear;
  if Source.Contains('=') then
  begin
    Outline := MarkKey(Source, Outline);
    if Outline.FValid then
    begin
      { Find expression in matched curly braces }
      ReadMode := rkNormal;
      BraceLevel := 0;
      CommandFound := False;
      ArgumentFound := False;

      SIndex := Outline.FKeyEnd + 2;
      while SIndex < Length(Source) do
      begin
        C := Source[SIndex];
        DebugLn('test source char: ' + C);
        case C of
          '\':
          begin
            if ReadMode = rkNormal then
            begin
              CommandStart := SIndex - 1;
              DebugLn('Found \ at index ' + IntToStr(CommandStart));
              ReadMode := rkCommand;
              Value := ExtractWord(1, Source.Substring(CommandStart), StdWordDelims);
              DebugLn('Found command \' + Value);
              CommandEnd := CommandStart + Length(Value) + 1;
              CommandFound := True;

              { search for argument immediately after command (with optional
              whitespace between) }
              SIndex := CommandEnd + 1;
              DebugLn('checking for arg in substring starting: ' + Source.Substring(SIndex, 20));
              if Source.Substring(SIndex).TrimLeft.StartsWith('{') then
              begin
                DebugLn('looking for argument after command');
              end
              else
              begin
                ArgumentFound := False;
                DebugLn('no argument found after command; ending the search');
                break;
              end;
            end; { if ReadMode }
          end;

          '{': 
          begin
            DebugLn('Found { at index ' + IntToStr(SIndex));
            if (ReadMode = rkNormal) and (BraceLevel = 0) then
            begin
              ArgumentStart := SIndex - 1; { include opening bracket in value string }
            end;
            Inc(BraceLevel);
            DebugLn('Going to bracelevel ' + IntToStr(BraceLevel));
            ReadMode := rkBraceArgument;
          end;

          '}': 
          begin
            if ReadMode = rkBraceArgument then
            begin
              DebugLn('Found } at index ' + IntToStr(SIndex));
              Dec(BraceLevel);
              DebugLn('Going to bracelevel ' + IntToStr(BraceLevel));
              if BraceLevel = 0 then
              begin
                ArgumentEnd := SIndex; { include close bracket }
                ArgumentFound := True;
                DebugLn('Found an expression, ending the search');
                break;
              end;
            end;
          end; 
        end; { case }
        Inc(SIndex);
      end; { for }

      if CommandFound then
      begin { `\command < argument >` }
        if ArgumentFound then
        begin
          Outline.FValueStart := CommandStart;
          Outline.FValueEnd   := ArgumentEnd;
          Outline.FValid      := True;
        end
        else { `\command` }
        begin
          Outline.FValueStart := CommandStart;
          Outline.FValueEnd   := CommandEnd;
          Outline.FValid      := True;
        end
      end
      else { `< argument >` }
      begin
        if ArgumentFound then
        begin
          Outline.FValueStart := ArgumentStart;
          Outline.FValueEnd   := ArgumentEnd;
          Outline.FValid      := True;
        end;
      end;
       
      {$ifdef DEBUG}
      if CommandFound or ArgumentFound then
      begin
        DebugLn('found value: start ' + IntToStr(Outline.FValueStart) 
        + ', end: ' + IntToStr(Outline.FValueEnd));
      end;
      {$endif}
    end;
  end;

  {$ifdef DEBUG}
  if Outline.FValid = False then
  begin 
    DebugLn('no macro definition found');
  end;
  {$endif}

  result := Outline;
end;

{ `FindReplaceMacros`

  Find a command starting with backslash like `\Music`, look up the key in
  dictionary and if found, replace it with the corresponding value; if nothing
  is found, just leave the text alone.  Expand any macros in the stored values
  before expanding them in the source text. } 
function FindReplaceMacros(Source: String; Dict: TMacroDict): String;
var
  BufferStr, Command, AfterCommand, OutputStr: String;
  ThisValue: String;
begin
  OutputStr := Source;
  BufferStr := OutputStr;
  while BufferStr.Contains('\') do
  begin
    BufferStr := StringDropBefore(BufferStr, '\');
    Command := ExtractWord(1, BufferStr, [' ', LineEnding]);

    { Continue searching after this command }
    BufferStr := BufferStr.Substring(Length(Command));

    { Replace command and add back the space or newline that followed it }
    AfterCommand := BufferStr.Substring(0, 1);
    if Dict.TryGetValue(Command, ThisValue) then
    begin
      OutputStr := ReplaceString(OutputStr, Command, ThisValue + AfterCommand);
    end;
  end;
  result := OutputStr;
end;

{ `ExpandDictMacros`
  
  Expand all the nested macros stored within macro dictionary values. }
function ExpandDictMacros(Dict: TMacroDict): TMacroDict;
var
  MacroPairI, MacroPairJ, MacroPairEdit: TMacroKeyValue;
begin
  for MacroPairI in Dict do
  begin
    MacroPairEdit := MacroPairI;
    for MacroPairJ in Dict do
    begin
      MacroPairEdit.Value := FindReplaceMacros(MacroPairEdit.Value, Dict);
      Dict.AddOrSetValue(MacroPairI.Key, MacroPairEdit.Value);
    end;
  end;
  result := Dict;
end;

{ `ExtractMacro` 

  Find a macro in a string from its outline (indices of key and value) and add
  it to a dictionary; return the updated dictionary. }
function ExtractMacro(Source: String; Outline: TMacroOutline; Dict:
  TMacroDict): TMacroDict;
var
  Key, Value: String;
begin
  if Outline.FValid then
  begin
    Key   := '\' + Source.Substring(Outline.FKeyStart, Outline.KeyLength).Trim;
    Value := Source.Substring(Outline.FValueStart, Outline.ValueLength).Trim;
    Dict.AddOrSetValue(Key, Value);
    DebugLn('SUCCESS: added macro key: ' + Key + ', value: ' + Value);
  end;
  result := Dict;
end;


{ MAIN }
var
  InputText, OutputText: TStringList;
  Macros: TMacroDict;
  Outline: TMacroOutline;
  FileName, ThisStr, BufferStr, CutStr: String;
  NewStart: Integer;
begin
  InputText := TStringList.Create();
  OutputText := TStringList.Create();
  Outline := TMacroOutline.Create();
  Macros := TMacroDict.Create();

  try
    { Process input file }
    if ParamCount <> 1 then
    begin
      WriteLn('Usage: lymacro INFILE.ly');
      exit;
    end
    else
    begin
      FileName := ParamStr(1);
      DebugLn('Loading input file ' + FileName);
    end;

    InputText.LoadFromFile(FileName);
    InputText := RemoveComments(InputText);

    { Find and store macro definitions in the dictionary and delete 
    them from the output text. If none found in this line, go to the next.  }
    BufferStr := InputText.Text;
    ThisStr := BufferStr;
    while not ThisStr.IsEmpty do
    begin
      Outline := MarkMacro(Outline, ThisStr);
      if Outline.FValid then
      begin
        Macros := ExtractMacro(ThisStr, Outline, Macros);
        CutStr := ThisStr.Substring(Outline.FKeyStart, Outline.MacroLength);
        BufferStr := ReplaceStr(BufferStr, CutStr, '');

        NewStart := Outline.FValueEnd;
        ThisStr := ThisStr.Substring(NewStart);
      end
      else
      begin
        DebugLn('did not find a macro on this line');
        NewStart := ThisStr.IndexOf(LineEnding) + 1;
        ThisStr := ThisStr.Substring(NewStart);
      end;
    end;

    {$ifdef DEBUG}
    for MacroPair in Macros do
      DebugLn('MACRO: key = ' + MacroPair.Key + ', value = ' + MacroPair.Value);
    {$endif}

    Macros := ExpandDictMacros(Macros);
    BufferStr := FindReplaceMacros(BufferStr, Macros);

    OutputText := RemoveBlankLines(Lines(BufferStr, OutputText));
    WriteLn(OutputText.Text);

  finally
    FreeAndNil(Macros);
    FreeAndNil(Outline);
    FreeAndNil(OutputText);
    FreeAndNil(InputText);
  end;
end.

