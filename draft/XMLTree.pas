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
    constructor Create(Name: String; IDStr: String = ''; 
      TextStr: String = '');
    destructor Destroy; override;
    function ToString: String; override;
    procedure AddAttribute(Key, Value: String);
    function AddChild(Child: TXMLNode): TXMLNode;
    function AddParent(Parent: TXMLNode): TXMLNode;
  end;

function XMLAttributeString(Key, Value: String): String;
begin
  result := Format('%s="%s"', [Key, Value]);
end;

function TXMLAttributeList.ToString: String;
var
  ThisAttribute: TXMLAttributeList.TDictionaryPair;
  XML: String = '';
  Index: Integer = 0;
begin
  if Count > 0 then 
  begin
    for ThisAttribute in Self do
    begin
      if Index > 1 then 
        XML := XML + ' ';
      
      XML := XML + XMLAttributeString(ThisAttribute.Key, 
        ThisAttribute.Value); 

      Inc(Index);
    end;
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

function TXMLNode.AddChild(Child: TXMLNode): TXMLNode;
begin
  FChild := Child;
  if Assigned(Child) then
    Child.FParent := Self;

  result := Self;
end;

function TXMLNode.AddParent(Parent: TXMLNode): TXMLNode;
begin
  FParent := Parent;
  if Assigned(Parent) then
    Parent.FChild:= Self;

  result := Self;
end;


var
  Measure, Staff, Layer, Note: TXMLNode;
begin
  Measure := TXMLNode.Create('measure');
  Measure.AddAttribute('n', '1');

  Staff := TXMLNode.Create('staff', 's-Soprano');
  Staff.AddAttribute('n', '1');

  Layer := TXMLNode.Create('layer', 'soprano');
  Layer.AddAttribute('n', '1');

  Note := TXMLNode.Create('note');
  Note.AddAttribute('pname', 'c');
  Note.AddAttribute('dur', '4');

  Note.AddParent(Layer.AddParent(Staff.AddParent(Measure)));
//  Measure := Measure.AddChild(Staff.AddChild(Layer.AddChild(Note)));

  WriteLn(Measure.ToString);
  FreeAndNil(Measure);
end.
