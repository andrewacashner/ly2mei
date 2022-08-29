{ fragments for adding figured bass to MusicNotes.pas;
  abandoned 2022/08/29
}

uses Character;

interface 

type
  TFiguredBass = class
  private
    var
      FStaffNum: Integer;
      FTimeStamp: Single;
      FFigures: TStringArray;
  public
    constructor Create();
    constructor Create(LyInput: String);
    destructor Destroy(); override;

    function ToMEI: TMeiNode;

    property StaffNum:  Integer      read FStaffNum;
    property TimeStamp: Single       read FTimeStamp;
    property Figures:   TStringArray read FFigures;
  end;

  TFiguredBassList = class(specialize TObjectList<TFiguredBass>)
  public
    constructor Create();
    constructor Create(StaffNum: Integer; Meter: TMeter; LyInput: String);

    { return tree that is a list of siblings }
    function ToMEI: TMeiNode;
  end;

  { add field to TPitchList: @(FFigureList: TFiguredBassList) }

implementation 
constructor TFiguredBass.Create();
begin
  inherited Create();
end;

function IsValidFiguredBassInput(LyInput: String): Boolean;
begin
  result := not LyInput.IsEmpty
              and IsNumber(LyInput.Chars[Length(LyInput) - 1])
              and ((LyInput.StartsWith('<') 
                      and LyInput.Substring(1).Contains('>'))
                    or LyInput.StartsWith('s'));
end;


function CalcTimeStamp(Meter: TMeter; LyInput: String): Single;

  function DotCount(LyInput: String): Integer;
  var
    CharIndex: Integer;
    Dots: Integer = 0;
  begin
    if LyInput.EndsWith('.') then
    begin
      CharIndex := Length(LyInput) - 1;
      while (CharIndex > 0) and (LyInput[CharIndex] = '.') do
      begin
        Inc(Dots);
      end;
    end;
    result := Dots;
  end;

var
  TimeStamp: Single = 0.0;
  ThisDur: Integer;
  DurBase: Integer;
  Dots: Integer = 0;
begin
  case Meter.Kind of
    mkModern :
      DurBase := Meter.BeatUnit;
    mkMensuralTempusImperfectum, mkMensuralProportioMinor :
      DurBase := 2;
  end;

  if LyInput.Contains('.') then
  begin
    LyInput := SubstringBefore(LyInput, '.');
    Dots := DotCount(LyInput);
  end;

  ThisDur := StrToInt(LyInput);

  TimeStamp := ThisDur / DurBase;
  { TODO this is not enough. You need this number, a time length relative to
  the base beat count of the meter, but you also need the offset from the start of the measure, which you can only get by looking at the whole measure. }
  result := TimeStamp;
end;

{ TODO START }
constructor TFiguredBassList.Create(StaffNum: Integer; Meter: TMeter; LyInput: String);
var
  Duration: Integer;
begin
  Create();
  FStaffNum := StaffNum;

  if LyInput.StartsWith('s') then
  begin
    FFigures := nil;
    FTimeStamp := CalcTimeStamp(Meter, LyInput.Substring(1));
  end
  else
  begin
    { parse < ... > expression }
    FTimeStamp := {} ;
    FFigures := {} ;
  end;
end;

destructor TFiguredBass.Destroy();
begin
  FFigures.Free;
  inherited Destroy();
end;

function TFiguredBass.ToMEI: TMeiNode;
var
  HarmNode: TMeiNode = nil;
  FiguredBassNode: TMeiNode = nil;
  ThisFigureNode: TMeiNode = nil;
  ThisFigure: String;
begin
  HarmNode := TMeiNode.Create('harm');
  HarmNode.AddAttribute('staff', IntToStr(FStaffNum));
  HarmNode.AddAttribute('tstamp', IntToStr(FTimeStamp));

  FiguredBassNode := TMeiNode.Create('fb');
  for ThisFigure in FFigures do
  begin
    ThisFigureNode := TMeiNode.Create('f');
    ThisFigureNode.TextNode := ThisFigure;
    FiguredBassNode := FiguredBassNode.AppendChild(ThisFigureNode);
  end;
  HarmNode := HarmNode.AppendChild(FiguredBassNode);
  result := HarmNode;
end;

function TFiguredBassList.Create()
begin
  inherited Create();
end;

function TFiguredBassList.Create(StaffNum: Integer; Meter: TMeter; LyInput: String);
var
  TokenizedInput: TStringArray;
  ThisString: String;
  NewFigure: TFiguredBass;
begin
  Create();

  LyInput := LyArg(LyInput, '\figuremode', rkExclusive);

  TokenizedInput := LyInput.Split([' ', LineEnding], 
    TStringSplitOptions.ExcludeEmpty);

  for ThisString in TokenizedInput do
  begin
    if IsValidFiguredBassInput(ThisString) then
    begin
      NewFigure := TFiguredBass.Create(StaffNum, Meter, ThisString);
      Add(NewFigure);
    end;
  end;
end;

{ list of siblings }
function TFiguredBassList.ToMEI: TMeiNode;
var
  Root: TMeiNode = nil;
  NewHarmNode: TMeiNode = nil;
  ThisFiguredBass: TFiguredBass;
begin
  for ThisFiguredBass in Self do
  begin
    NewHarmNode := ThisFiguredBass.ToMEI;
    if not Assigned(Root) then
      Root := NewHarmNode
    else
      Root := Root.AppendSibling(NewHarmNode);
  end;
  result := Root;
end;

end.





