{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}
program pivot(output);

{ Rearrange an XML tree }

uses SysUtils, MEI;

{ TODO not quite the full model even for mockup

Lilypond: root/score/staff/voice/measure/notes
MEI:      root/score/measure/staff/layer/notes
}

var
  LyRoot, 
  LyMeasure1Soprano, LyMeasure2Soprano,
  LyMeasure1Bass, LyMeasure2Bass, 
  LyStaff1, LyStaff2, 
  LyMusicSopranoM1, LyMusicSopranoM2, 
  LyMusicBassM1, LyMusicBassM2,
  MeiRoot, 
  MeiMeasure1, MeiMeasure2,
  MeiStaff1M1, MeiStaff2M1, 
  MeiStaff1M2, MeiStaff2M2,
  MeiMusicSopranoM1, MeiMusicSopranoM2, 
  MeiMusicBassM1, MeiMusicBassM2: TMeiNode;
begin
  LyRoot := TMeiNode.Create('lirio:score');

  LyStaff1 := TMeiNode.Create('staff');
  LyStaff1.AddAttribute('n', '1');
  LyStaff1.AddAttribute('label', 'Soprano');
  
  LyStaff2 := TMeiNode.Create('staff');
  LyStaff2.AddAttribute('n', '2');
  LyStaff2.AddAttribute('label', 'Bass');

  LyMeasure1Soprano := TMeiNode.Create('lirio:measure');
  LyMeasure1Soprano.AddAttribute('n', '1');
  
  LyMeasure2Soprano := TMeiNode.Create('lirio:measure');
  LyMeasure2Soprano.AddAttribute('n', '2');
  
  LyMeasure1Bass := TMeiNode.Create('lirio:measure');
  LyMeasure1Bass.AddAttribute('n', '1');
  
  LyMeasure2Bass := TMeiNode.Create('lirio:measure');
  LyMeasure2Bass.AddAttribute('n', '2');

  LyMusicSopranoM1 := TMeiNode.Create('note');
  LyMusicSopranoM1.AddAttribute('pname', 'b');
  LyMusicSopranoM1.AddAttribute('oct', '4');
  LyMusicSopranoM1.AddAttribute('dur', '1');
  
  LyMusicSopranoM2 := TMeiNode.Create('note');
  LyMusicSopranoM2.AddAttribute('pname', 'c');
  LyMusicSopranoM2.AddAttribute('oct', '5');
  LyMusicSopranoM2.AddAttribute('dur', '1');
 
  LyMusicBassM1 := TMeiNode.Create('note');
  LyMusicBassM1.AddAttribute('pname', 'g');
  LyMusicBassM1.AddAttribute('oct', '3');
  LyMusicBassM1.AddAttribute('dur', '1');

  LyMusicBassM2 := TMeiNode.Create('note');
  LyMusicBassM2.AddAttribute('pname', 'c');
  LyMusicBassM2.AddAttribute('oct', '3');
  LyMusicBassM2.AddAttribute('dur', '1');

  { Lilypond hierarchy }
  { score -> staves }
  LyRoot.AppendChild(LyStaff1);
  LyStaff1.AppendSibling(LyStaff2);

  { staff [1] -> (voice -> ) measures }
  LyStaff1.AppendChild(LyMeasure1Soprano);
  LyMeasure1Soprano.AppendSibling(LyMeasure2Soprano);

  { measure -> notes }
  LyMeasure1Soprano.AppendChild(LyMusicSopranoM1);
  LyMeasure2Soprano.AppendChild(LyMusicSopranoM2);

  { staff [2] -> (voice -> ) measures }
  LyStaff2.AppendChild(LyMeasure1Bass);
  LyMeasure1Bass.AppendSibling(LyMeasure2Bass);

  { measure -> notes }
  LyMeasure1Bass.AppendChild(LyMusicBassM1);
  LyMeasure2Bass.AppendChild(LyMusicBassM2);

  WriteMeiDocument(LyRoot);


  { MEI hierarchy }
  MeiRoot := TMeiNode.CreateMeiRoot();

  MeiMeasure1 := TMeiNode.Create('measure');
  MeiMeasure1.AddAttribute('n', '1');

  MeiMeasure2 := TMeiNode.Create('measure');
  MeiMeasure2.AddAttribute('n', '2');

  MeiStaff1M1 := TMeiNode.Create('staff');
  MeiStaff1M1.AddAttribute('n', '1');
  MeiStaff1M1.AddAttribute('label', 'Soprano');
  
  MeiStaff2M1 := TMeiNode.Create('staff');
  MeiStaff2M1.AddAttribute('n', '2');
  MeiStaff2M1.AddAttribute('label', 'Bass');

  { Copy MEI measure 1 staves for measure 2 }
  MeiStaff1M2 := TMeiNode.Create();
  MeiStaff1M2.Assign(MeiStaff1M1);
  
  MeiStaff2M2 := TMeiNode.Create();
  MeiStaff2M2.Assign(MeiStaff2M1);

  { Copy measure 1 notes from ly tree }
  MeiMusicSopranoM1 := TMeiNode.Create();
  MeiMusicSopranoM1.Assign(LyMusicSopranoM1);
  
  MeiMusicBassM1 := TMeiNode.Create();
  MeiMusicBassM1.Assign(LyMusicBassM1);

  { Copy measure 2 notes from ly tree }
  MeiMusicSopranoM2 := TMeiNode.Create();
  MeiMusicSopranoM2.Assign(LyMusicSopranoM2);

  MeiMusicBassM2 := TMeiNode.Create();
  MeiMusicBassM2.Assign(LyMusicBassM2);
  
  { root -> (score -> ) measures }
  MeiRoot.AppendChild(MeiMeasure1);
  MeiMeasure1.AppendSibling(MeiMeasure2);

  { measure [1] -> staves }
  MeiMeasure1.AppendChild(MeiStaff1M1);
  MeiStaff1M1.AppendSibling(MeiStaff2M1);
 
  { measure [2] -> staves }
  MeiMeasure2.AppendChild(MeiStaff1M2);
  MeiStaff1M2.AppendSibling(MeiStaff2M2);

  { measure 1 staff 1 -> (layer -> ) notes }
  MeiStaff1M1.AppendChild(MeiMusicSopranoM1);
  { measure 1 staff 2 -> (layer -> ) notes }
  MeiStaff2M1.AppendChild(MeiMusicBassM1);

  { measure 2 staff 1 -> (layer -> ) notes }
  MeiStaff1M2.AppendChild(MeiMusicSopranoM2);
  { measure 2 staff 2 -> (layer -> ) notes }
  MeiStaff2M2.AppendChild(MeiMusicBassM2);

  WriteMeiDocument(MeiRoot);

  FreeAndNil(LyRoot);

  { Find parts of tree. Not copying, just getting pointers to nodes. }
  LyRoot := MeiRoot.FindElementByAttribute('staff', 'n', '2');
  if Assigned(LyRoot) then
    WriteMeiDocument(LyRoot);
  
  LyRoot := MeiRoot.FindElementByAttribute('staff', 'label', 'Soprano');
  if Assigned(LyRoot) then
    WriteMeiDocument(LyRoot);

  FreeAndNil(MeiRoot);
end.
{
for measure n: extract staff attributes
  for each staff, copy tree: voices/measure[n]/notes
}
