{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} {$OPTIMIZATION tailrec}

{ @abstract(Parse a Lilypond header and convert it to MEI)
  @author(Andrew Cashner) }
unit Header;

interface

uses SysUtils, Classes, StringTools, Outline, MEI;

const ProgramName: String = 'ly2mei';

type
  { @abstract(All the data needed for an MEI header.) }
  THeader = class
  private
    var
      FTitle, FSubtitle, FComposer, FDates, 
      FPoet, FEditor, FCopyright, FSource: String;
      FValid: Boolean;
  public
    constructor CreateFromLy(InputLines: TStringList);
    function ToMEI(): TMeiNode;
  end;

function CreateMeiHeadFromLy(LyInput: TStringList): TMeiNode;

function AddMeiHead(Root: TMeiNode; LyInput: TStringList): TMeiNode;

implementation

constructor THeader.CreateFromLy(InputLines: TStringList);
var
  LyHeaderLines: TStringList;
  SearchStr, ThisString, Key, Value, MarkupStr: String;
  LineIndex: Integer;
  FoundThis, FoundAny: Boolean;
begin
  inherited Create();

  SearchStr := LyArg(InputLines.Text, '\header');
  if not SearchStr.IsEmpty then
  begin
    LyHeaderLines := Lines(SearchStr);
    LineIndex := 0;
    FoundAny := False;
    for ThisString in LyHeaderLines do
    begin
      FoundThis := False;
      if ThisString.Contains('=') then
      begin
        LyHeaderLines.GetNameValue(LineIndex, Key, Value);
        Key := Key.Trim;
        Value := Value.Trim;
       
        if IsASingleQuotedString(Value) then 
        begin
          Value := Value.DeQuotedString('"');
          FoundThis := True;
        end
        else
        begin
          MarkupStr := LyArg(Value, '\markup');
          Value := ExtractQuotedStrings(MarkupStr);
          FoundThis := True;
        end; 
      end; { if contains @code('=') }

      if FoundThis then
      begin
        case Key of
          'title':     FTitle := Value;
          'subtitle':  FSubtitle := Value;
          'composer':  FComposer := Value;
          'dates':     FDates := Value;
          'poet':      FPoet := Value;
          'editor':    FEditor := Value;
          'copyright': FCopyright := Value;
          'source':    FSource := Value;
        end;
        FoundAny := True;
      end;
      Inc(LineIndex);
    end;
    FValid := FoundAny;
  end;
  FreeAndNil(LyHeaderLines);
end;

function THeader.ToMEI(): TMeiNode;
var 
  HeaderTree, FileDesc, TitleStmt, Title, Subtitle, TitleRespStmt, Composer,
  Lyricist, EditionStmt, Editor, EditionRespStmt, PubStmt, Availability,
  EncodingDesc, AppInfo, Application, SourceDesc, Source:
    TMeiNode; 
begin
  HeaderTree := TMeiNode.Create('meiHead');
  if FValid then
  begin
    { 1 - File Description }
    FileDesc := TMeiNode.Create('fileDesc');
    HeaderTree.AppendChild(FileDesc);

    { 1a - Title Statement }
    TitleStmt := TMeiNode.Create('titleStmt');
    FileDesc.AppendChild(TitleStmt);

    Title := TMeiNode.Create('title');
    Title.AddAttribute('type', 'main');
    Title.SetTextNode(FTitle);
    TitleStmt.AppendChild(Title);

    if not FSubtitle.IsEmpty then 
    begin
      Subtitle := TMeiNode.Create('title');
      Subtitle.AddAttribute('type', 'subtitle');
      Subtitle.SetTextNode(FSubtitle);
      TitleStmt.AppendChild(Subtitle);
    end;

    TitleRespStmt := TMeiNode.Create('respStmt');
    TitleStmt.AppendChild(TitleRespStmt);

    Composer := TMeiNode.Create('composer');
    if FDates.IsEmpty then
      Composer.SetTextNode(FComposer)
    else
    begin
      Composer.SetTextNode(Format('%s %s', [FComposer, FDates]));
    end;
    TitleRespStmt.AppendChild(Composer);
    
    if not FPoet.IsEmpty then
    begin
      Lyricist := TMeiNode.Create('lyricist', FPoet);
      TitleRespStmt.AppendChild(Lyricist);
    end;

    if not FEditor.IsEmpty then
    begin
      Editor := TMeiNode.Create('editor', FEditor);
      TitleRespStmt.AppendChild(Editor);
    end;
    
    { Edition Statement }
    if not FEditor.IsEmpty then
    begin
      EditionStmt := TMeiNode.Create('editionStmt');
      EditionRespStmt := TMeiNode.Create('respStmt');
      EditionRespStmt.AppendChild(
        TMeiNode.Create('p', Format('Edited by %s', [FEditor])));
      EditionStmt.AppendChild(EditionRespStmt);
      FileDesc.AppendChild(EditionStmt);
    end;
    
    if not FCopyright.IsEmpty then
    begin
      PubStmt := TMeiNode.Create('pubStmt');
      Availability := TMeiNode.Create('availability').AppendChild(
        TMeiNode.Create('p', FCopyright));
      PubStmt.AppendChild(Availability);
      FileDesc.AppendChild(PubStmt);
    end;
    
    if not FSource.IsEmpty then
    begin
      SourceDesc := TMeiNode.Create('sourceDesc');
      Source := TMeiNode.Create('source', FSource);
      SourceDesc.AppendChild(Source);
      HeaderTree.AppendChild(SourceDesc);
    end;

    EncodingDesc := TMeiNode.Create('encodingDesc');
    AppInfo := TMeiNode.Create('appInfo');
    Application := TMeiNode.Create('application');
    Application.AddAttribute('name', ProgramName);
    EncodingDesc.AppendChild(AppInfo);
    AppInfo.AppendChild(Application);
    HeaderTree.AppendChild(EncodingDesc);
  end;
  result := HeaderTree;
end;

function CreateMeiHeadFromLy(LyInput: TStringList): TMeiNode;
var
  HeaderData: THeader;
  MeiTree: TMeiNode = nil;
begin
  HeaderData := THeader.CreateFromLy(LyInput);
  MeiTree := HeaderData.ToMei;
  FreeAndNil(HeaderData);
  result := MeiTree;
end;

function AddMeiHead(Root: TMeiNode; LyInput: TStringList): TMeiNode;
var
  MeiHead: TMeiNode = nil;
begin
  assert(Assigned(Root));
  MeiHead := CreateMeiHeadFromLy(LyInput);
  if Assigned(MeiHead) then
    Root.AppendChild(MeiHead)
  else
    WriteLn(stderr, 'Could not create meiHead element');

  result := Root;
end;

end.
