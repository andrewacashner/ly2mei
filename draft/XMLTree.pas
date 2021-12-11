{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

program XMLTree;

uses SysUtils, Classes, Generics.Collections;

type
  TXMLAttributeList = class(specialize TDictionary<String, String>)
  public
    function ToString: String; override;
  end;

  TXMLNode = class
  private
    var
      FName: String;
      FID: String;
      FText: String;
      FAttributes: TXMLAttributeList;
      FParent: TXMLNode;
      FChild: TXMLNode;
      FSibling: TXMLNode;
  public
    constructor Create(Name: String; IDStr: String = ''; TextStr: String = '');
    destructor Destroy; override;
    function ToString: String; override;
    procedure AddAttribute(Key, Value: String);
  end;

function XMLAttributeString(Key, Value: String): String;
begin
  result := Format('%s="%s"', [Key, Value]);
end;

function TXMLAttributeList.ToString: String;
var
  ThisAttribute: TXMLAttributeList.TDictionaryPair;
  XML: String = '';
begin
  if Count > 0 then 
  begin
    for ThisAttribute in Self do
      XML := Format('%s %s', [XML, 
        XMLAttributeString(ThisAttribute.Key, ThisAttribute.Value)]);
  end;
  result := XML;
end;

constructor TXMLNode.Create(Name: String; IDStr: String = ''; 
  TextStr: String = ''); 
var
  NewGUID: TGUID;
  Test: Integer;
begin
  inherited Create;
  FName := Name;
  FText := TextStr;
 
  FID := IDStr;
  if FID = '' then
  begin
    Test := CreateGUID(NewGUID);
    if Test = 0 then
      FID := GUIDToString(NewGUID);
  end;

  FAttributes := TXMLAttributeList.Create;

  FParent := nil;
  FChild := nil;
  FSibling := nil;
end;

destructor TXMLNode.Destroy;
begin
  FreeAndNil(FAttributes);

  if FChild <> nil then
    FChild.Destroy;

  if FSibling <> nil then
    FSibling.Destroy;

  inherited Destroy;
end;

function TXMLNode.ToString: String;
var
  XML, ID, Attributes: String;
begin
  ID := XMLAttributeString('xml:id', FID);

  Attributes := FAttributes.ToString;
  if Attributes = '' then
    Attributes := ID
  else
    Attributes := Format('%s %s', [ID, Attributes]);

  if FChild <> nil then
    FText := FText + FChild.ToString;

  if FSibling <> nil then
    FText := FText + FSibling.ToString;

  XML := Format('<%s %s>%s</%s>', [FName, Attributes, FText, FName]);
  result := XML;
end;

procedure TXMLNode.AddAttribute(Key, Value: String);
begin
  FAttributes.Add(Key, Value);
end;

var
  Root: TXMLNode;
begin
  Root := TXMLNode.Create('score');
  Root.AddAttribute('n', '1');
  Root.AddAttribute('type', 'test');

  WriteLn(Root.ToString);
  FreeAndNil(Root);
end.
