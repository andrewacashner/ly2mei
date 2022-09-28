{$mode objfpc}{$H+}{$J-}

{ @abstract(Module for testing improved parsing of music expression)
  @author(Andrew Cashner, 2022/09/27)
}
program parseMusic(input, output, stderr);

uses SysUtils, Classes, Generics.Collections;

type
  TIndexList = class(specialize TList<Integer>)
  public
    constructor CreateFromBarlines(Tokens: TStringArray);
  end;

constructor TIndexList.CreateFromBarlines(Tokens: TStringArray);
var
  Index: Integer;
  ThisWord: String;
begin
  inherited Create;
  Index := 0;
  for ThisWord in Tokens do
  begin
    if ThisWord = '|' then
    begin
      Add(Index);
    end;
    Inc(Index);
  end;
end;

function IsCommand(TestStr: String): Boolean;
begin
  result := TestStr.StartsWith('\');
end;

function IsQuoteString(TestStr: String): Boolean;
begin
  result := TestStr.StartsWith('"') and TestStr.EndsWith('"');
end;


function FindCloseBrace(InputWords: TStringList; StartIndex: Integer): Integer;
var
  EndIndex: Integer;
begin
  if InputWords[StartIndex] = '}' then
    EndIndex := StartIndex + 1
  else
    EndIndex := FindCloseBrace(InputWords, StartIndex + 1);
  
  result := EndIndex;
end;

function FindArgEnd(InputWords: TStringList; StartIndex: Integer): Integer;
var
  ThisIndex, EndIndex: Integer;
  ThisWord, NextWord: String;
begin
  EndIndex := StartIndex;
  if StartIndex < InputWords.Count - 1 then
  begin
    ThisWord := InputWords[StartIndex];
    if IsCommand(ThisWord) then
    begin
      NextWord := InputWords[StartIndex + 1];
      if IsQuoteString(NextWord) then
        EndIndex := StartIndex + 1
      else if NextWord = '{' then
        EndIndex := FindCloseBrace(InputWords, StartIndex + 1)
      else if IsCommand(NextWord) then
        EndIndex := FindArgEnd(InputWords, StartIndex + 1)
    end;
  end;
  result := EndIndex;
end;

function StringListRangeToString(StringList: TStringList; 
  Start, Stop: Integer): String;
var
  OutputStr: String = '';
  ThisIndex: Integer;
begin
  for ThisIndex := Start to Stop do
    OutputStr := OutputStr + StringList[ThisIndex] + ' ';
  
  result := OutputStr;
end;

function GroupCommandWithArg(InputWords: TStringList): TStringList;
var
  NewWords: TStringList;
  ThisWord, NextWord: String;
  WordIndex, ArgEndIndex, CopyIndex: Integer;
  Command: String;
begin
  NewWords := TStringList.Create;
  WordIndex := 0;
  while WordIndex < InputWords.Count do
  begin
    ThisWord := InputWords[WordIndex];
    if IsCommand(ThisWord) then
    begin
      ArgEndIndex := FindArgEnd(InputWords, WordIndex);
      Command := StringListRangeToString(InputWords, WordIndex, ArgEndIndex);
      NewWords.Add(Command);
      WordIndex := ArgEndIndex;
    end
    else
    begin
      NewWords.Add(ThisWord);
    end;
    Inc(WordIndex);
  end;

  InputWords.Assign(NewWords);
  FreeAndNil(NewWords);
  result := InputWords;
end;


type
  TInputMeasure = TStringList;

  TInputSection = class(specialize TObjectList<TStringList>)
  public
    constructor CreateByMeasure(InputTokens: TStringArray);
  end;

constructor TInputSection.CreateByMeasure(InputTokens: TStringArray);
var
  ThisInputMeasure: TInputMeasure;
  StartPositions: TIndexList;
  PositionCount, ThisIndex, ThisStart, ThisStop, ThisTokenIndex: Integer;
  CurrentStr: String;
begin
  inherited Create;

  StartPositions := TIndexList.CreateFromBarlines(InputTokens);

  PositionCount := 0;
  for ThisIndex in StartPositions do
  begin
    ThisStart := ThisIndex;
    if (PositionCount < StartPositions.Count - 1) then
      ThisStop := StartPositions[PositionCount + 1]
    else
      ThisStop := Length(InputTokens) - 1;
    
    ThisInputMeasure := TInputMeasure.Create;
    
    for ThisTokenIndex := ThisStart + 1 to ThisStop - 1 do
    begin
      CurrentStr := InputTokens[ThisTokenIndex];
      ThisInputMeasure.Add(CurrentStr);
    end;

    ThisInputMeasure := GroupCommandWithArg(ThisInputMeasure);
   
    Add(ThisInputMeasure);
    Inc(PositionCount);
  end;

  FreeAndNil(StartPositions);
end;


type
  TPitch = class
  private 
    var
      RawStr : String;
  public 
    constructor Create(InputStr: String);
    function ToString: String; override;
  end;

  TPitchList = class(specialize TObjectList<TPitch>)
  public
    constructor CreateFromInputMeasure(InputMeasure: TInputMeasure);
  end;

  TMeasureList = class(specialize TObjectList<TPitchList>)
  public
    constructor CreateFromInputSection(InputSection: TInputSection);
    function ToStringList(OutputLines: TStringList): TStringList;
    function ToString: String; override;
  end;

constructor TPitch.Create(InputStr: String);
begin
  inherited Create;
  RawStr := InputStr;
end;

function TPitch.ToString: String;
begin
  result := RawStr;
end;

constructor TPitchList.CreateFromInputMeasure(InputMeasure: TInputMeasure);
var
  ThisWord: String;
  NewPitch: TPitch;
begin
  inherited Create;
  for ThisWord in InputMeasure do
  begin
    NewPitch := TPitch.Create(ThisWord);
    Add(NewPitch);
  end;
end;

constructor TMeasureList.CreateFromInputSection(InputSection: TInputSection);
var
  ThisMeasure: TInputMeasure;
  NewPitchList: TPitchList;
begin
  inherited Create;
  for ThisMeasure in InputSection do
  begin
    NewPitchList := TPitchList.CreateFromInputMeasure(ThisMeasure);
    Add(NewPitchList);
  end;
end;

function TMeasureList.ToStringList(OutputLines: TStringList): TStringList;
var
  ThisPitchList: TPitchList;
  ThisPitch: TPitch;
begin
  OutputLines.Clear;
  for ThisPitchList in Self do
  begin
    for ThisPitch in ThisPitchList do
    begin
      OutputLines.Add(ThisPitch.ToString);
    end;
  end;
  result := OutputLines;
end;

function TMeasureList.ToString: String;
var
  OutputStr: String;
  OutputLines: TStringList;
begin
  OutputLines := TStringList.Create;
  OutputLines := Self.ToStringList(OutputLines);
  OutputStr := OutputLines.Text;
  FreeAndNil(OutputLines);
  result := OutputStr;
end;

function SplitAtSpaces(InputStr: String): TStringArray;
begin
  result := InputStr.Split([' ', LineEnding], 
              TStringSplitOptions.ExcludeEmpty);
end;

{ MAIN }
var
  InputLines: TStringList;
  Words: TStringArray;
  Section: TInputSection;
  MeasureList: TMeasureList;
begin
  { setup }
  InputLines := TStringList.Create;

  try
    { input }
    if (ParamCount <> 1) then
    begin
      WriteLn(stderr, 'Usage: parseMusic INFILE.ly');
      exit;
    end;

    InputLines.LoadFromFile(ParamStr(1));

    { parse }
    Words := SplitAtSpaces(InputLines.Text);
    Section := TInputSection.CreateByMeasure(Words);
    MeasureList := TMeasureList.CreateFromInputSection(Section);

    { output }
    WriteLn(MeasureList.ToString);

  finally
    { cleanup }
    FreeAndNil(Section);
    FreeAndNil(MeasureList);
    FreeAndNil(InputLines);
  end;
end.

