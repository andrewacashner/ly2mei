{$mode objfpc}{$H+}{$J-}

{ @abstract(Module for testing improved parsing of music expression)
  @author(Andrew Cashner, 2022/09/27)
}
program parseMusic(input, output, stderr);

uses SysUtils, Classes, Generics.Collections;

type
  TPitch = class
  private 
    var
      RawStr : String;
  public 
    constructor Create(InputStr: String);
    function Show: String;
  end;

  TPitchList = specialize TObjectList<TPitch>;

  TMeasureList = specialize TObjectList<TPitchList>;

constructor TPitch.Create(InputStr: String);
begin
  inherited Create;
  RawStr := InputStr;
end;

function TPitch.Show: String;
begin
  result := RawStr;
end;

type
  TIndexList = specialize TList<Integer>;

function BarStartPositions(Tokens: Array of String): TIndexList;
var
  Index: Integer;
  ThisWord: String;
  StartPositions: TIndexList;
begin
  StartPositions := TIndexList.Create;
  Index := 0;
  for ThisWord in Tokens do
  begin
    if ThisWord = '|' then
    begin
      StartPositions.Add(Index);
    end;
    Inc(Index);
  end;
  result := StartPositions;
end;

type
  TInputMeasure = TStringList;
  TInputSection = specialize TObjectList<TStringList>;

function GroupByMeasure(InputTokens: Array of String): TInputSection;
var
  Section: TInputSection;
  ThisInputMeasure: TInputMeasure;
  StartPositions: TIndexList;
  PositionCount, ThisIndex, ThisStart, ThisStop, ThisTokenIndex: Integer;
  CurrentStr: String;
begin
  Section := TInputSection.Create;

  StartPositions := BarStartPositions(InputTokens);

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
   
    Section.Add(ThisInputMeasure);

    Inc(PositionCount);
  end;

  result := Section;
  FreeAndNil(StartPositions);
end;

var
  InputLines, OutputLines: TStringList;
  Words: Array of String;
  ThisWord: String;
  Section: TInputSection;
  ThisInputMeasure: TInputMeasure;
  
  ThisPitch: TPitch;
  ThisPitchList: TPitchList;
  MeasureList: TMeasureList;
begin
  { setup }
  InputLines := TStringList.Create;
  OutputLines := TStringList.Create;
  MeasureList := TMeasureList.Create;

  try
    { input }
    if (ParamCount <> 1) then
    begin
      WriteLn(stderr, 'Usage: parseMusic INFILE.ly');
      exit;
    end;

    InputLines.LoadFromFile(ParamStr(1));

    { parse }
    Words := InputLines.Text.Split([' ', LineEnding], 
              TStringSplitOptions.ExcludeEmpty);

    Section := GroupByMeasure(Words);

    for ThisInputMeasure in Section do
    begin
      ThisPitchList := TPitchList.Create;
      for ThisWord in ThisInputMeasure do
      begin
        ThisPitch := TPitch.Create(ThisWord);
        ThisPitchList.Add(ThisPitch);
      end;
      MeasureList.Add(ThisPitchList);
    end;

    { output }
    for ThisPitchList in MeasureList do
    begin
      for ThisPitch in ThisPitchList do
      begin
        OutputLines.Add(ThisPitch.Show);
      end;
    end;

    WriteLn(OutputLines.Text);

  finally
    { cleanup }
    FreeAndNil(Section);
    FreeAndNil(MeasureList);
    FreeAndNil(InputLines);
    FreeAndNil(OutputLines);
  end;
end.

