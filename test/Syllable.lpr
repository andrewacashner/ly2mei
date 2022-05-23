{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

program syllable(output);

uses SysUtils, SyllableList, MEI;

const Lyrics: String = 
  'Sus -- pen -- ded, cie -- los, vues -- tro dul -- ce can -- to.';

var
  SylList: TSyllableList;
  MeiTree: TMeiNode;
begin
  SylList := TSyllableList.Create(Lyrics);
  MeiTree := SylList.ToMEI;

  Write(MeiTree.XMLString);

  FreeAndNil(SylList);
  FreeAndNil(MeiTree);
end.
