{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} 

{ @abstract(Parse a Lilypond header and convert it to MEI)
  @author(Andrew Cashner) }
unit Header;

interface

uses SysUtils, Classes, StringTools, Outline, MEI;

type
  { @abstract(All the data needed for an MEI header.) 

  Includes title, subtitle, composer, dates, poet, editor, copyright, and
  source (all strings) and a flag for validity. } 
  THeader = class
  private
    var
      FTitle, FSubtitle, FComposer, FDates, 
      FPoet, FEditor, FCopyright, FSource: String;
      FValid: Boolean;

    { Generate all needed MEI elements }
    { titleStmt sub-elements }
    function MeiTitle: TMeiNode;
    function MeiSubtitle: TMeiNode;
    function MeiComposer: TMeiNode;
    function MeiLyricist: TMeiNode;
    function MeiEditor:TMeiNode;
   
    { fileDesc sub-elements }
    function MeiTitleStmt: TMeiNode;
    function MeiEditionStmt: TMeiNode;
    function MeiPubStmt: TMeiNode;

    { meiHead sub-elements }
    function MeiFileDesc: TMeiNode;
    function MeiSourceDesc: TMeiNode;
    function MeiEncodingDesc: TMeiNode;

  public
    { Create a header structure from the Lilypond @code(\header) block }
    constructor Create(InputLines: TStringList);

    { Set the value for a given key; match the key string to the field }
    procedure SetValue(Key, Value: String);

    { Create a new MEI @code(meiHead) element (tree) }
    function ToMEI(): TMeiNode;
  end;

{ Create an MEI tree for a @code(meiHead) element from a Lilypond
  @code(\header) expression. }
function CreateMeiHead(LyInput: TStringList): TMeiNode;

implementation

constructor THeader.Create(InputLines: TStringList);

  { Find either a single quoted string or a @code(\markup) expression. }
  function FindHeaderValue(InputStr, Delim: String): String;
  var
    Value: String = '';
    MarkupStr: String;
  begin
    Value := CopyStringBetween(InputStr, Delim, LineEnding).Trim;
    if IsASingleQuotedString(Value) then 
      Value := Value.DeQuotedString('"')
    else
    begin
      MarkupStr := LyArg(Value, '\markup');
      Value := ExtractQuotedStrings(MarkupStr);
    end; 
    result := Value;
  end; 

const
  FieldDelim: String = ' = ';
var
  LyHeaderLines: TStringListPlus;
  SearchStr, TestStr, ThisString, Key, Value: String;
  LineIndex: Integer;
begin
  inherited Create();
  FValid := False;

  SearchStr := LyArg(InputLines.Text, '\header');
  if not SearchStr.IsEmpty then
  begin
    LyHeaderLines := TStringListPlus.Create(SearchStr);

    LineIndex := 0;
    for ThisString in LyHeaderLines do
    begin
      Value := '';
      if ThisString.Contains(FieldDelim) then
      begin
        Key := StringDropAfter(ThisString, FieldDelim).Trim;
        if not Key.IsEmpty then
        begin
          TestStr := ToStringFromIndex(InputLines, LineIndex);
          Value := FindHeaderValue(TestStr, FieldDelim);
        end;
      end;

      if not Value.IsEmpty then
      begin
        Self.SetValue(Key, Value);
      end;
      Inc(LineIndex);
    end;
  end;
  FreeAndNil(LyHeaderLines);
end;

procedure THeader.SetValue(Key, Value: String);
begin
  case Key of
    'title':     FTitle     := Value;
    'subtitle':  FSubtitle  := Value;
    'composer':  FComposer  := Value;
    'dates':     FDates     := Value;
    'poet':      FPoet      := Value;
    'editor':    FEditor    := Value;
    'copyright': FCopyright := Value;
    'source':    FSource    := Value;
  end;
  FValid := True;
end;

function THeader.MeiTitle: TMeiNode;
var
  Title: TMeiNode;
begin
  Title := TMeiNode.Create('title');
  Title.AddAttribute('type', 'main');
  Title.TextNode := FTitle;
  result := Title;
end;

function THeader.MeiSubtitle: TMeiNode;
var
  Subtitle: TMeiNode = nil;
begin
  if not FSubtitle.IsEmpty then 
  begin
    Subtitle := TMeiNode.Create('title');
    Subtitle.AddAttribute('type', 'subtitle');
    Subtitle.TextNode := FSubtitle;
  end;
  result := Subtitle
end;

function THeader.MeiComposer: TMeiNode;
var
  Composer: TMeiNode;
begin
  Composer := TMeiNode.Create('composer');
  if FDates.IsEmpty then
    Composer.TextNode := FComposer
  else
    Composer.TextNode := Format('%s %s', [FComposer, FDates]);

  result := Composer;
end;

function THeader.MeiLyricist: TMeiNode;
var
  Lyricist: TMeiNode = nil;
begin
  if not FPoet.IsEmpty then
  begin
    Lyricist := TMeiNode.Create('lyricist', FPoet);
  end;
  result := Lyricist;
end;

function THeader.MeiEditor:TMeiNode;
var
  Editor: TMeiNode = nil;
begin
  if not FEditor.IsEmpty then
  begin
    Editor := TMeiNode.Create('editor', FEditor);
  end;
  result := Editor;
end;

function THeader.MeiTitleStmt: TMeiNode;
var 
  TitleStmt, TitleRespStmt: TMeiNode;
begin
  TitleStmt := TMeiNode.Create('titleStmt');
  TitleStmt.AppendChild(MeiTitle);
  TitleStmt.AppendChild(MeiSubtitle);

  TitleRespStmt := TMeiNode.Create('respStmt');
  TitleStmt.AppendChild(TitleRespStmt);

  TitleRespStmt.AppendChild(MeiComposer);
  TitleRespStmt.AppendChild(MeiLyricist);
  TitleRespStmt.AppendChild(MeiEditor);
  
  result := TitleStmt;
end;

function THeader.MeiEditionStmt: TMeiNode;
var
  EditionStmt: TMeiNode = nil;
  EditionRespStmt: TMeiNode;
begin
  if not FEditor.IsEmpty then
  begin
    EditionStmt := TMeiNode.Create('editionStmt');
    EditionRespStmt := TMeiNode.Create('respStmt');
    EditionRespStmt.AppendChild(
      TMeiNode.Create('p', Format('Edited by %s', [FEditor])));
    EditionStmt.AppendChild(EditionRespStmt);
  end;
  result := EditionStmt;
end;

function THeader.MeiPubStmt: TMeiNode;
var
  PubStmt: TMeiNode = nil;
  Availability: TMeiNode;
begin
  if not FCopyright.IsEmpty then
  begin
    PubStmt := TMeiNode.Create('pubStmt');
    Availability := TMeiNode.Create('availability').AppendChild(
      TMeiNode.Create('p', FCopyright));
    PubStmt.AppendChild(Availability);
  end;
  result := PubStmt;
end;

function THeader.MeiFileDesc: TMeiNode;
var
  FileDesc: TMeiNode;
begin
  FileDesc := TMeiNode.Create('fileDesc');
  FileDesc.AppendChild(MeiTitleStmt);
  FileDesc.AppendChild(MeiEditionStmt);
  FileDesc.AppendChild(MeiPubStmt);
  result := FileDesc;
end;

function THeader.MeiSourceDesc: TMeiNode;
var
  SourceDesc: TMeiNode = nil;
  Source: TMeiNode;
begin
  if not FSource.IsEmpty then
  begin
    SourceDesc := TMeiNode.Create('sourceDesc');
    Source := TMeiNode.Create('source', FSource);
    SourceDesc.AppendChild(Source);
  end;
  result := SourceDesc;
end;

function THeader.MeiEncodingDesc: TMeiNode;
const 
  ProgramName: String = 'ly2mei';
var
  EncodingDesc, AppInfo, Application: TMeiNode;
begin
  EncodingDesc := TMeiNode.Create('encodingDesc');
  AppInfo := TMeiNode.Create('appInfo');
  Application := TMeiNode.Create('application');
  Application.AddAttribute('name', ProgramName);
  EncodingDesc.AppendChild(AppInfo);
  AppInfo.AppendChild(Application);
  result := EncodingDesc;
end;

function THeader.ToMEI: TMeiNode;
var 
  HeaderTree: TMeiNode;
begin
  HeaderTree := TMeiNode.Create('meiHead');
  if FValid then
  begin
    HeaderTree.AppendChild(MeiFileDesc);
    HeaderTree.AppendChild(MeiSourceDesc);
    HeaderTree.AppendChild(MeiEncodingDesc);
  end;
  result := HeaderTree;
end;

function CreateMeiHead(LyInput: TStringList): TMeiNode;
var
  HeaderData: THeader;
  MeiHead: TMeiNode;
begin
  HeaderData := THeader.Create(LyInput);
  MeiHead := HeaderData.ToMei;
  FreeAndNil(HeaderData);
  result := MeiHead;
end;

end.
