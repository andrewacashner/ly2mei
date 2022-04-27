function ProcessMacros(InputStr: String): String;
var
  FindOutline, CopyOutline: TIndexPair;
  CommandArg: TCommandArg;
  OutputStr, ThisString, NextStr, Key, Value, TestStr: String;
  BufferStr: String = '';
  LineIndex: Integer = 0;
  Found: Boolean;
  InputLines: TStringList;
  Dict: TMacroDict;
begin
  Dict := TMacroDict.Create;
  
  CopyOutline.FStart := 0;
  CopyOutline.FValid := True;

  { TODO just loop through one index at a time, instead of looping through a
  stringlist AND a string }
  InputLines := Lines(InputStr);
  { Look for a @code(key = value) pair at the start of each line }
  for ThisString in InputLines do
  begin
    Found := False;
    Key := GetMacroDefKey(ThisString);
    if not Key.IsEmpty then
    begin
      { Found key, mark start location }
      Key := Key.Trim;
      CopyOutline := SetEndSpan(CopyOutline, InputStr.IndexOf(Key + ' '));

      { Parse value }
      Value := StringDropBefore(ThisString, '=').Trim;
      case Value.Chars[0] of
      '{':
        { Value is a brace-delimited argument }
        begin
          TestStr := ToStringFromIndex(InputLines, LineIndex);
          FindOutline := FindMatchedBraces(TestStr);
          if IsValid(FindOutline) then
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

  OutputStr := FindReplaceMacros(BufferStr, Dict);

  FreeAndNil(InputLines);
  FreeAndNil(Dict);

  result := OutputStr;
end;

