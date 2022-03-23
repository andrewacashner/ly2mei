{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+} {$OPTIMIZATION tailrec}

{ @abstract(Parse a Lilypond header and convert it to MEI)
  @author(Andrew Cashner) }
unit Header;

interface

uses SysUtils, Classes, StringTools, Outline, MEI;

const ProgramName: String = 'ly2mei';

type
  { @abstract(All the data needed for an MEI header.) }
  THeader = record
    FTitle, FSubtitle, FComposer, FDates, 
    FPoet, FEditor, FCopyright, FSource: String;
    FValid: Boolean;
  end;

{ Return a new string list containing the MEI expression of the header data. }
function NewMEIFromHeader(Header: THeader): TStringListAAC;

{ Find a header definition and parse it into a @link(THeader) record. }
function ParseHeader(InputLines: TStringListAAC): THeader;

{ All together: Parse Lilypond output to MEI header }
function CreateMEIHeader(SourceLines: TStringListAAC): TStringListAAC;

implementation

function THeader.ToMEI: TMeiNode;
var 
  HeaderTree, FileDesc, TitleStmt, Title, Subtitle, TitleRespStmt, Composer,
  Lyricist, Editor, EditionRespStmt, PubStmt, Availability, Copyright,
  EncodingDesc, AppInfo, Application, ApplicationName, SourceDesc, Source:
    TMeiNode; 
begin
  HeaderTree := TMeiNode.Create('meiHead');
  if Header.FValid then
  begin
    with Header do
    begin
      FileDesc := TMeiNode.Create('fileDesc');
      HeaderTree.AppendChild(FileDesc);

      TitleStmt := TMeiNode.Create('titleStmt');
      FileDesc.AppendChild(TitleStmt);

      Title := TMeiNode.Create('title');
      Title.AddAttribute('type', 'main');
      Title.SetTextNode(FTitle);
      TitleStmt := AppendChild(Title);

      if not FSubtitle.IsEmpty then 
      begin
        Subtitle := TMeiNode.Create('title');
        Subtitle.AddAttribute('type', 'subtitle');
        Subtitle.SetTextNode(FSubtitle);
        TitleStmt := AppendChild(Subtitle);
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
        Lyricist := TMeiNodeCreate('lyricist');
        Lyricist.SetTextNode(FPoet);
        TitleRespStmt.AppendChild(Lyricist):
      end;

      if not FEditor.IsEmpty then
      begin
        Editor := TMeiNodeCreate('editor');
        Editor.SetTextNode(FEditor);
        TitleRespStmt.AppendChild(Editor);
      end;


      if not FEditor.IsEmpty then
      begin
        EditionStmt := TMeiNodeCreate('editionStmt');
        EditionRespStmt := TMeiNodeCreate('respStmt');
        EditionRespStmt.AppendChild(
          TMeiNodeCreate('p').SetTextNode(Format('Edited by %s', [FEditor])));
        EditionStmt.AppendChild(EditionRespStmt);
        FileDesc.AppendChild(EditionStmt);
      end;

      if not FCopyright.IsEmpty then
      begin
        PubStmt := TMeiNodeCreate('pubStmt');
        Availability := TMeiNodeCreate('availability').AppendChild(
          TMeiNodeCreate('p').SetTextNode(FCopyright));
        PubStmt.AppendChild(Availability);
        FileDesc.AppendChild(PubStmt);
      end;

      EncodingDesc := TMeiNodeCreate('encodingDesc').AppendChild(
        TMeiNodeCreate('appInfo').AppendChild(
          TMeiNodeCreate('application').AddAttribute('name', ProgramName)));
      HeaderTree.AppendChild(EncodingDesc);

      if not FSource.IsEmpty then
      begin
        Source := TMeiNode.Create('sourceDesc').AppendChild(
          TMeiNode.Create('source').SetTextNode(FSource));
        HeaderTree.AppendChild(Source);
      end;
    end;
  end;
  result := HeaderTree;
end;

{ TODO debug function above, test XMLString output from it }

function NewMEIFromHeader(Header: THeader): TStringListAAC;
var
  MEI: TStringListAAC;
begin
  MEI := TStringListAAC.Create;
  if not Header.FValid then
  begin
    MEI.Add('<meiHead>');
    MEI.Add('</meiHead>');
    result := MEI;
    exit;
  end;

  with Header do
  begin
    MEI.Add('<meiHead>');
    MEI.Add(IndentStr(1) + '<fileDesc>');
    MEI.Add(IndentStr(2) + '<titleStmt>');
    MEI.Add(IndentStr(3) + XMLElement('title',
      XMLAttribute('type', 'main'), FTitle));

    if not FSubtitle.IsEmpty then
      MEI.Add(IndentStr(3) + XMLElement('title',
        XMLAttribute('type', 'subtitle'), FSubtitle));

    MEI.Add(IndentStr(3) + '<respStmt>');

    if not FDates.IsEmpty then
    begin
      MEI.Add(IndentStr(4) + XMLElement('composer', '', 
        Format('%s %s', [FComposer, FDates])));
    end
    else
      MEI.Add(IndentStr(4) + XMLElement('composer', '', FComposer));

    if not FPoet.IsEmpty then
      MEI.Add(IndentStr(4) + XMLElement('lyricist', '', FPoet));

    if not FEditor.IsEmpty then
      MEI.Add(IndentStr(4) + XMLElement('editor', '', FEditor));

    MEI.Add(IndentStr(3) + '</respStmt>');
    MEI.Add(IndentStr(2) + '</titleStmt>');
    
    if not FEditor.IsEmpty then
    begin
      MEI.Add(IndentStr(2) + '<editionStmt>');
      MEI.Add(IndentStr(3) + XMLElement('respStmt', '',
        XMLElement('p', '', Format('Edited by %s', [FEditor]))));
      MEI.Add(IndentStr(2) + '</editionStmt>');
    end;

    if not FCopyright.IsEmpty then
    begin
      MEI.Add(IndentStr(2) + '<pubStmt>');
      MEI.Add(IndentStr(3) + XMLElement('availability', '',
        XMLElement('p', '', FCopyright)));
      MEI.Add(IndentStr(2) + '</pubStmt>');
    end;

    MEI.Add(IndentStr(1) + '</fileDesc>');
    MEI.Add(IndentStr(1) + '<encodingDesc>');
    MEI.Add(IndentStr(2) + '<appInfo>');
    MEI.Add(IndentStr(3) + XMLElement('application', '',
      XMLElement('name', '', ProgramName)));
    MEI.Add(IndentStr(2) + '</appInfo>');
    MEI.Add(IndentStr(1) + '</encodingDesc>');
    
    if not FSource.IsEmpty then
      MEI.Add(IndentStr(1) + XMLElement('sourceDesc', '',
        XMLElement('source', '', FSource)));

    MEI.Add('</meiHead>'); 
  end;
  result := MEI;
end;

function ParseHeader(InputLines: TStringListAAC): THeader;
var
  Header: THeader;
  LyHeaderLines: TStringListAAC;
  SearchStr, ThisString, Key, Value, MarkupStr: String;
  LineIndex: Integer;
  FoundThis, FoundAny: Boolean;
begin
  SearchStr := LyArg(InputLines.Text, '\header');
  if not SearchStr.IsEmpty then
  begin
    LyHeaderLines := TStringListAAC.Create(SearchStr);
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
      with Header do
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
    Header.FValid := FoundAny;
  end;
  FreeAndNil(LyHeaderLines);
  result := Header;
end;

function CreateMEIHeader(SourceLines: TStringListAAC): TStringListAAC;
begin
  result := NewMEIFromHeader(ParseHeader(SourceLines));
end;

end.
