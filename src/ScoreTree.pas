{$mode objfpc}{$H+}{$J-} {$ASSERTIONS+}

{ @abstract(Build a tree of code objects defined by Lilypond @code(\new)
  commands within a @code(\score) expression.)
  @author(Andrew Cashner)

  This unit is used to build a tree of Lilypond code objects that were
  created with @code(\new) commands within a @code(\score) expression. 
  The tree is a left-child/right-sibling tree of @link(TLyObject) instances.
  
  In the command @code(\new Voice = "Soprano" < \MusicSoprano >), this class
  would store @code("Voice") as the @code(FType), @code("Soprano") as the
  @code(FID), and the rest of the expression, up to the conclusion of a
  curly-brace delimited group, as the @code(FContents).

  The @link(TLyObject.ToMeiScoreDef) method extracts the needed information
  from the tree to create a @link(TMeiNode) tree for the MEI @code(scoreDef)
  element.
  
  To create the MEI music elements, we must convert the tree structure from
  the Lilypond hierarchy to that of MEI.  Lilypond @code(\score) expressions
  have a hierarchy of score/StaffGroup or ChoirStaff/Staff/Voice/music
  expression, where the music is a list of measures containing pitches.  MEI
  on the other hand, has a hierarchy of
  score/measure/staffGrp/staff/layer/notes.  So for each MEI measure we must
  build the path staffGrp/staff/layer/notes.

  The @link(TLyObject) object provides two methods for converting the whole
  tree to XML. @link(TLyObject.ToXMLAsIs) creates a @link(TMeiNode) tree that
  copies the structure of the original Lilypond tree;
  @link(TLyObject.ToMeiSection) creates a @link(TMeiNode) tree with the proper
  MEI structure.

  To create the MEI structure, the @link(TLyObject) tree contains, at the
  bottom level (equivalent to Lilypond Voice or MEI layer), a list of
  containers corresponding to the music for each measure (each of which
  contains a list of internal representations of a pitch). These structures
  are defined and created in the @link(MusicNotes) unit.  That means we can
  trace the path to each bottom node, copy a given measure's contents, and
  then build the path to that node for each separate measure and each separate
  voice or layer.
}
unit ScoreTree;

interface

uses SysUtils, StrUtils, Classes, Generics.Collections, StringTools, Outline,
  MEI, Header, MusicNotes;

type 
  { Types of elements in the internal tree of @code(TLyObject) or
    @code(TMEIElement) objects }
  TLyObjectType = (ekAnonymous, ekStaffGrp, ekStaff, ekLayer, ekMeasure, 
    ekLyrics, ekFigures);

  { @abstract(A node, which may be the base of a tree, of Lilypond code
    objects.)

    These objects were originally created with @code(\new) commands in the
    @code(\score) expression. Includes fields for a type label, name (string),
    id (string), num (integer), contents (string), measurelist
    (@link(TMeasureList)), and child and sibling trees. 

    To build this left-child/right-sibling tree, we look for two kinds of
    objects: 
      @orderedlist(
          @item(@code(\new) expressions followed by an expression enclosed in
            double angle brackets (@code(<<...>>))) 
          @item(Expressions that conclude with an expression in matched curly
            braces)
        )
    For the first kind, we store the value and ID, if there is one; then we
    continue to look recursively for child elements within the angle-bracket
    expression. For the second kind, we store value, ID, and contents
    (everything from the ID up to the end of the matched-brace expression)
    as a sibling of the previous item.
  }
  TLyObject = class
  private
    var
      { Label for object type, equivalent between Lilypond and MEI }
      FType: TLyObjectType;
      { Type of object, e.g., Voice or Lyrics }
      FName: String;
      { ID string, as in @code(\new Voice = "Soprano"). If Lilypond source did
        not supply a label, we generate one randomly. }
      FID: String;
      { In-order position of elements of the same type: Staff elements are
        numbered consecutively regardless of whether they are enclosed in a
        StaffGroup. }
      FNum: Integer;
      { Contents (Lilypond text): If the command is followed by section
        delimited by double angle brackets, then the contents will be blank;
        otherwise, everything up to the end of a curly-brace-delimited
        argument. }
      FContents: String;
      { List of measures (lists of pitch objects), parsed from contents;
        ONLY for @code(ekLayer) type; otherwise empty (0 length)}
      FMeasureList: TMeasureList;
      
      { If this item is linked to another (as in Lyrics that match up with a
      Voice via @code(\lyricsto) then this is the ID of the other item. }
      FLinkID: String;

      { Left child node (tree) }
      FChild: TLyObject;
      { Right sibling node (tree) }
      FSibling: TLyObject;

    { Get the right name string for this @code(FType). }
    function NodeName: String;

    { Create a single MEI node for @code(scoreDef) from a @link(TLyObject)
      node, not including its children or siblings. }
    function ToMeiScoreDefNode: TMeiNode;

    { Find the first descendant node with the given type (and also
    the ID if given). }
    function FindElement(ElementType: TLyObjectType; ElementID:
      String = ''): TLyObject;

    { Find the first layer (@link(ekLayer) type) object in the tree in a
    pre-order traversal. }
    function FindFirstLayer: TLyObject;

    { Find the first staff (@link(ekStaff) type) object in the tree in a
    pre-order traversal. }
    function FindFirstStaff: TLyObject;

    { Create a @link(TMeiNode) tree with the path to the first layer in the
      @link(TLyObject) tree. }
    function ToMeiLayerPath(MeiNode: TMeiNode = nil): TMeiNode;
   
    { Follow the left children all the way to the end; return the last one. }
    function LastChild: TLyObject;

    { Follow the right siblings all the way to the end; return the last one. }
    function LastSibling: TLyObject;

    { Add given child to the end of the chain of children. }
    function AppendLastChild(NewChild: TLyObject): TLyObject;

    { Add given sibling to the end of the chain of siblings. }
    function AppendLastSibling(NewSibling: TLyObject): TLyObject;

    { Number ChoirStaff, StaffGroup, Staff, and Voice elements consecutively
      in a pre-order traversal, regardless of relation to other elements of the
      tree. }
    function NumberElementsInOrder: TLyObject;

    function AddLyrics: TLyObject;

  public
    constructor Create();

    { The @code(FType) field will be created automatically based on the
      @code(Name) argument. }
    constructor Create(Name, ID: String; LinkID: String = '';
      ContentsStr: String = ''; Num: Integer = 1; Child: TLyObject = nil; 
      Sibling: TLyObject = nil); 

    { Create a whole tree from Lilypond input as stringlist. }
    constructor Create(Source: String);

    { Create a whole tree from Lilypond input as stringlist, and number the
      elements consecutively. }
    constructor Create(LyInput: TStringList);

    { Destroy the whole tree. }
    destructor Destroy; override;

    { Write the tree to XML in its original, Lilypond-based structure. }
    function ToXMLAsIs(XmlNode: TMeiNode = nil): TMeiNode;

    { Create an MEI scoreDef element. }
    function ToMeiScoreDef: TMeiNode;
   
    { Create a @link(TMeiNode) tree with the proper MEI structure for the
      music data, converting from Lilypond structure. }
    function ToMeiSection: TMeiNode;

    property LyType:    TLyObjectType read FType;
    property Name:      String        read FName;
    property ID:        String        read FID;
    property Num:       Integer       read FNum           write FNum;
    property Contents:  String        read FContents;
    property Measures:  TMeasureList  read FMeasureList   write FMeasureList;
    property LinkID:    String        read FLinkID;
    property Child:     TLyObject     read FChild         write FChild;
    property Sibling:   TLyObject     read FSibling       write FSibling;
  end;

  TLyricsDict = class(specialize TObjectDictionary<String, TSyllableList>)
  public
    constructor Create(); override;
    constructor Create(LyTree: TLyObject);
    destructor Destroy(); override;
  end;

{ Create a new MEI tree consisting of the MEI @code(score) element, including
  @code(scoreDef) and @code(section) with music contents, and add it to the
  given MEI tree. }
function CreateMeiScore(LyInput: TStringList): TMeiNode;

{ Create a whole MEI document tree. }
function CreateMeiDocument(LyInput: TStringList): TMeiNode;

implementation

constructor TLyObject.Create();
begin
  inherited Create;
end;

constructor TLyObject.Create(Name, ID: String; LinkID: String = '';
  ContentsStr: String = ''; Num: Integer = 1; Child: TLyObject = nil; 
  Sibling: TLyObject = nil); 
  
  function NameToType(Name: String): TLyObjectType;
  var
    Element: TLyObjectType = ekAnonymous;
  begin
    case Name of
      'StaffGroup', 
        'ChoirStaff' : Element := ekStaffGrp;
      'Staff'        : Element := ekStaff;
      'Voice'        : Element := ekLayer;
      'Lyrics'       : Element := ekLyrics;
      'FiguredBass'  : Element := ekFigures;
    end;
    result := Element;
  end;

begin
  inherited Create;
  FType     := NameToType(Name);
  FName     := Name;
  FID       := ID;
  FNum      := Num;
  FContents := ContentsStr;
  FLinkID   := LinkID;
  FChild    := Child;
  FSibling  := Sibling;
  
  if FType = ekLayer then
    FMeasureList := TMeasureList.Create(FContents)
  else
    FMeasureList := TMeasureList.Create();
end;

destructor TLyObject.Destroy;
begin
  FChild.Free;
  FSibling.Free;
  FMeasureList.Free;
  inherited Destroy;
end;

function TLyObject.LastChild: TLyObject;
begin
  if not Assigned(FChild) then
    result := Self
  else
    result := FChild.LastChild;
end;

function TLyObject.LastSibling: TLyObject;
begin
  if not Assigned(FSibling) then
    result := Self
  else
    result := FSibling.LastSibling;
end;

function TLyObject.AppendLastChild(NewChild: TLyObject): TLyObject;
begin
  Self.LastChild.FChild := NewChild;
  result := Self;
end;

function TLyObject.AppendLastSibling(NewSibling: TLyObject): TLyObject;
begin
  Self.LastSibling.FSibling := NewSibling;
  result := Self;
end;

function TLyObject.NumberElementsInOrder: TLyObject;
{ The counters need to be outside the scope of this function so that we can
  number consecutively regardless of the recursive traversal. }
var
  StaffGroupCount: Integer = 0;
  StaffCount:      Integer = 0;
  LayerCount:      Integer = 0;
 
  function InnerNums(Node: TLyObject): TLyObject;
  begin
    if Assigned(Node) then
    begin
      case Node.LyType of
        ekStaffGrp :
        begin
          Inc(StaffGroupCount);
          Node.Num := StaffGroupCount;
        end;

        ekStaff :
        begin
          Inc(StaffCount);
          Node.Num := StaffCount;
        end;

        ekLayer :
        begin
          Inc(LayerCount);
          Node.Num := LayerCount;
        end;
      end;

      if Assigned(Node.Child) then
      begin
        Node.Child := InnerNums(Node.Child);
      end;
      if Assigned(Node.Sibling) then
      begin
        Node.Sibling := InnerNums(Node.Sibling);
      end;
    end;
    result := Node;
  end;

begin
  result := InnerNums(Self);
end;

function TLyObject.ToXMLAsIs(XmlNode: TMeiNode = nil): TMeiNode;
begin
  if not Assigned(XmlNode) then
  begin
    XmlNode := TMeiNode.Create();
  end;

  XmlNode.Name := NodeName;
  { Use @code(@type) to label nodes with unrecognized names }
  if NodeName = 'xml' then
  begin
    XmlNode.AddAttribute('type', FName);
  end;

  if not FID.IsEmpty then
  begin
    XmlNode.AddAttribute('xml:id', FID);
  end;

  XmlNode.AddAttribute('n', IntToStr(FNum));

  if FType = ekLayer then
  begin
    XmlNode := XmlNode.AppendChild(FMeasureList.ToMEI);
  end;

  if Assigned(FChild) then
  begin
    XmlNode := XmlNode.AppendChild(FChild.ToXMLAsIs);
  end;

  if Assigned(FSibling) then
  begin
    XmlNode := XmlNode.AppendSibling(FSibling.ToXMLAsIs);
  end;

  result := XmlNode;
end;

{ Build an LCRS tree of Lilypond @code(\new) objects. }
constructor TLyObject.Create(Source: String);
var
  SearchIndex: Integer = 0;
  SearchStr, ThisType, ThisID, ThisLinkID, ThisContents: String;
  Outline: TIndexPair;
begin
  Outline := TIndexPair.Create;
  SearchStr := Source;
  ThisType := '';
  ThisID := '';
  ThisLinkID := '';

  if SearchStr.Contains('\new ') then
  begin
    SearchIndex := SearchStr.IndexOf('\new ');
    
    { Find Type }
    ThisType := CopyStringBetween(SearchStr, '\new ', ' ');
    SearchStr := StringDropBefore(SearchStr, ThisType);

    { Find ID }
    if SearchStr.StartsWith(' = "') then
    begin 
      ThisID := CopyStringBetween(SearchStr, ' = "', '"');
      SearchStr := StringDropBefore(SearchStr, ThisID + '"');
    end;

    { Find Link ID for Lyrics }
    if (ThisType = 'Lyrics') and SearchStr.Contains('\lyricsto') then
    begin
      ThisLinkID := CopyStringBetween(SearchStr, '\lyricsto "', '"');
    end;

    { Find Contents: Either an expression within double angle brackets or
      one within curly braces. If angle brackets, recursively look for nested
      @code(\new) expressions. }
    if SearchStr.TrimLeft.StartsWith('<<') then
    begin
      { Search within group for nested @code(\new) expressions and save them
        as children; then move on after this group. Omit content string. }
      Outline.MarkBalancedDelimiterSubstring(SearchStr, '<', '>');
      ThisContents := CopyStringRange(SearchStr, Outline, rkInclusive);

      Create(ThisType, ThisID, ThisLinkID);
      if ThisContents.Contains('\new ') then
      begin
        FChild := TLyObject.Create(ThisContents);
      end;
    end
    else
    begin
      { Save brace expression in this node }
      ThisContents := CopyBraceExpr(SearchStr);
      Create(ThisType, ThisID, ThisLinkID, ThisContents);
    end;

    { Look for the next sibling where you left off from the last search }
    Source := StringDropBefore(Source.Substring(SearchIndex), ThisContents);
    if Source.Contains('\new ') then
    begin
      FSibling := TLyObject.Create(Source);
    end;
  end;
  FreeAndNil(Outline);
end;

{ Parse a Lilypond @code(\score) expression into a tree of @link(TLyObject)
nodes. A score expression consists of the command @code(\score) followed by a single argument in curly braces. That argument contains (in our system) a single "simultaneous event" group enclosed in double angle brackets (@code(<<...>>)). 

Within the simultaneous group there is a series of @code(\new) expressions.
These can define a @code(Staff), and groups of staves may be enclosed in a
@code(StaffGroup) or @code(ChoirStaff). The contents of each staff or
staff-group type are enclosed in simultaneous event group brackets. 
A staff, in turn, includes @code(\new) expressions to define a @code(Voice),
@code(Lyrics), or @code(FiguredBass).

So we will see this structure: 
@longcode(#
  \score ❴ 
    << 
      \new ChoirStaff 
      << 
        \new Staff = "s-Soprano" 
          << 
            \new Voice = "Soprano" ❴ c'2 d'2 ... ❵  
            \new Lyrics \lyricsto "Soprano" ❴ do re ... ❵ 
          >>
         \new Staff = "s-Alto" 
          << ... >> 
        ...
      >>
      ...
    >> 
  ❵
#)

We may define a @code(\new) command, then, as follows:
  @orderedlist(
    @item(The command @code('\new '),)
    @item(followed by one of the following type labels: @code('ChoirStaff'),
    @code('StaffGroup'), @code('Staff'), @code('Voice'), @code('Lyrics'), or
    @code('FiguredBass').) 
    @item(Optionally, the type label may be followed by an ID label, denoted by an equals sign and an ID string in double quotation marks, e.g., @code('\new Voice = "Soprano"').)
    @item(A @code(Lyrics) item must (in our system) include a command that links the lyrics to a particular @code(Voice): @code('\lyricsto "ID"') where ID matches the ID label of a voice.)
    @item(@code(StaffGroup), @code(ChoirStaff), and @code(Staff) elements are followed by an expression in double angle brackets (@code(<< ... >>)), which will include more @code(\new) expressions.)
    @item(@code(Voice), @code(Lyrics), and @code(FiguredBass) items are followed by an expression in curly braces (@code(❴ ... ❵)). In our system, these arguments may not include further @code(\new) expressions.)
    @item(A @code(Voice) item's argument must be a music expression (the default mode); a @code(Lyrics) item's argument must be in lyrics mode (@code(\lyricmode ❴ ... ❵)); a @code(FiguredBass) item's argument must be in figures mode (@code(\figuremode)).)
  )

Summary: 
@longcode(#
\score ❴ << [(one or more:) \new ... ] >> ❵

\new [Label] [= "ID"] [(if Label = Lyrics:) \lyricsto "VoiceID"] 
  | (if ChoirStaff, StaffGroup, or Staff:) [ << [(one or more:) \new] >> ]
  | (if Voice, Lyrics, or FiguredBass:)    [ ❴ [contents in appropriate mode] ❵ ]
#)

To parse the score: look for the command, find the matched brace argument,
find the matched angle-brackets expression within. Parse contents, reading
only @code(\new) expressions as defined, ignoring rest.

To parse a @code(\new) expression: find the command, extract the label, look
for an ID and extract if there is one; if Lyrics, look for @code(\lyricsto)
command.  If staff or staff group type, look for matched angle-brackets
expression; otherwise look for matched curly-brace expression.  If staff or
staff group type, search argument for more @code(\new) expressions.

From the score expression, construct a tree of @link(TLyObject) elements. Each
contains a type label, an ID (automatically assigned if none is specified), a
link ID (for lyrics, connecting to another item's ID; blank if none is
  specified), and links to children and siblings.


}
  { START HERE TODO }
  { pattern matching approach 
possible valid inputs:
1. \new Lyrics = "[ID]" \lyricsto "[LinkID]" ❴ [Contents] ❵ 
2. \new [LABEL] = "[ID]" ❴ [Contents] ❵
3. \new [LABEL] = "[ID]" << [Contents] >>
4. \new [LABEL] ❴ [Contents] ❵
5. \new [LABEL] << [Contents] >>

subtests:
IsNew x: x = '\new'
IsEquals x: x = '='
IsQuoted x: x.StartsWith('"') and x.EndsWith('"')
HasID Input: IsNew(Input[0]) && IsEquals(Input[2]) && IsQuoted(Input[3])
HasBraceArg Input N: FindBalancedDelimiter(Str, '❴', '❵') <> ''
HasBracketArg Str: FindBalancedDelimiter(Str, '<<', '>>') <> ''

tests: 
1. HasID(Input) and (Input[1] = 'Lyrics') and (Input[4] = '\lyricsto') and (IsQuoted(Input[5])) and HasBracketArg(Unwords(Input, 6));
2. HasID(Input) and HasBraceArg(Unwords(Input, 4));
3. HasID(Input) and HasBracketArg(Unwords(Input, 4));
4. not HasID(Input) and HasBraceArg(Unwords(Input, 2);
5. not HasID(Input) and HasBracketArg(Unwords(Input, 2);

assignments: 
1. Label := Input[1], ID := Input[3], LinkID := Input[5], Contents := BraceArg(Input);
2. Label := Input[1], ID := Input[3], LinkID := '', Contents := BraceArg(Input);
3. Label := Input[1], ID := Input[3], LinkID := '', Contents := BracketArg(Input);
4. Label := Input[1], ID := auto, LinkID := '', Contents := BraceArg(Input);
5. Label := Input[1], ID := auto, LinkID := '', Contents := BracketArg(Input);

}
function IsNew(InputStr: String): Boolean;
begin
  result := InputStr = '\new';
end;

function IsEqualSign(InputStr: String): Boolean;
begin
  result := InputStr = '=';
end;

function IsOpenBrace(InputStr: String): Boolean;
begin
  result := InputStr = '{';
end;

function IsOpenBracket(InputStr: String): Boolean;
begin
  result := InputStr = '<<';
end;

function IsQuoted(InputStr: String): Boolean;
begin
  result := InputStr.StartsWith('"') and InputStr.EndsWith('"');
end;

function StartsNew(Words: Array of String): Boolean;
begin
  result := IsNew(Words[0]);
end;

function HasID(Words: Array of String): Boolean;
var
  Answer: Boolean = False;
begin
  if Words.Count >= 4 then
  begin
    Answer := IsNew(Words[0]) and IsEqualSign(Words[1]) and IsQuoted(Words[2]);
  end;
  result :=  Answer;
end;

function IndexAfterLabelOrID(Words: Array of String): Integer;
var
  Index: Integer;
begin
  if StartsNew(Words) then
  begin
    if HasID(Words) then
      Index := 4
    else 
      Index := 2;
  end
  else 
    Index := -1;

  result := Index;
end;

function IsNewLyrics(Words: Array of String): Boolean;
var
  Answer: Boolean = False;
  Index: Integer;
begin
  Index := IndexAfterLabelOrID;
  if (Index <> -1) and (Words.Count >= Index + 3) then
  begin
    Answer := (Words[Index] = '\lyricsto') 
      and IsQuoted(Words[Index + 1])
      and IsOpenBrace(Words[Index + 2]);
  end;
  result := Answer;
end;

function IsNewWithBraceArg(Words: Array of String; TypeLabel: String): Boolean;
var
  Index: Integer;
begin
  Index := IndexAfterLabelOrID;
  result := (Index <> -1) and (Words[1] = TypeLabel) 
              and IsOpenBrace(Words[Index])
end;

function IsNewVoiceOrFigureType(Words: Array of String): Boolean;
var
  Index: Integer;
begin
  Index := IndexAfterLabelOrID;
  result := (Index <> -1) 
            and ((Words[1] = 'Voice') 
              or (Words[1] = 'FiguredBass'))
            and IsOpenBrace(Words[Index]);
end;

function IsNewVoice(Words: Array of String): Boolean;
begin
  result := IsNewWithBraceArg(Words, 'Voice');
end;

function IsNewFiguredBass(Words: Array of String): Boolean;
begin
  result := IsNewWithBraceArg(Words, 'FiguredBass');
end;

function IsNewWithBracketArg(Words: Array of String; TypeLabel: String): Boolean;
var
  Index: Integer;
begin
  Index := IndexAfterLabelOrID;
  result := (Index <> -1) and (Words[1] = TypeLabel) 
              and IsOpenBracket(Words[Index])
end;

function IsNewStaffType(Words: Array of String): Boolean;
begin
  Index := IndexAfterLabelOrID;
  result := (Index <> -1) 
              and ((Words[1] = 'StaffGroup') 
                or (Words[1] = 'ChoirStaff') 
                or (Words[1] = 'Staff'))
              and IsOpenBracket(Words[Index]);
end;

function ArgIndex(Words: Array of String): Integer;
var
  Index: Integer = -1;
begin
  if IsNewVoiceorFigureType(Words) or IsNewStaffType(Words) then
    Index := 2
  else if IsNewLyrics(Words) then
    Index := 4;

  if (Index <> -1) and HasID(Words) then
  begin
    Index := Index + 2;
  end;
  result := Index;
end;

function IDIndex(Words: Array of String): Integer;
var
  Index: Integer = -1;
begin
  if HasID(Words) then
  begin
    Index := 3;
  end;
  result := Index;
end;

function LinkIDIndex(Words: Array of String): Integer;
var
  Index: Integer = -1;
begin
  if IsNewLyrics(Words) then
  begin
    if HasId(Words) then
      Index := 5
    else
      Index := 3;
  end;
  result := Index;
end;

function TestNewExpression(Words: Array of String; StartIndex: Integer): Boolean;
var
  Answer: Boolean = False;
begin
 { Do tests }
end;


function ReadScore(LyInput: String): String;
var
  Score: String;
  OutputStr: String = '';
begin
  Score := CommandArgBraces(LyInput, '\score');
  OutputStr := BalancedDelimiterSubstringWords(Score, '<<', '>>');
  result := OutputStr;
end;

function ReadNewExpr(LyInput: String): TLyObject;
var
  OutputStr: String = '';
  TestWords: TStringArray;
  TestStr, ThisWord: String;
  WordIndex, StartIndex, EndIndex, Level: Integer;
  TypeLabel, ID, LinkID, Contents: String;
  NewObject: TLyObject = nil;
begin
  WordIndex := 0;
  for ThisWord in Words do
  begin
    if IsNew(ThisWord) then
    begin
      if TestNewExpression(Words, WordIndex) then
      begin
        NewNode := TLyObject.Create(ThisWord, WordIndex); { TODO }
      end;
    end;
    Inc(WordIndex);
  end;
  result := NewNode;
end;


{ long way  
  if LyInput.Contains('\new ') then 
  begin
    TestWords := Words(InputStr);

    
    WordIndex := 0;
    Level := 0;
    for ThisWord in TestWords do
    begin
      if ThisWord = '\new' then
      begin
        if Level = 0 then
        begin
          Inc(WordIndex);
          if WordIndex < TestWords.Count then
          begin
            TypeLabel := TestWords[WordIndex];
            Inc(WordIndex);
            if WordIndex < TestWords.Count then
            begin
              if (TestWords[WordIndex] = '=') 
                and (WordIndex + 1 < TestWords.Count) then
              begin
                Inc(WordIndex);
                ID := TestWords[WordIndex];
              end;
              Inc(WordIndex);
              if (WordIndex < TestWords.Count) then
              begin
                if (TypeLabel = 'Lyrics') and (TestWords[WordIndex] = 'lyricsto') then
                begin
                  Inc(WordIndex);
                  if (WordIndex < TestWords.Count) then
                  begin
                    LinkID := TestWords[WordIndex];
                  end
                  else LinkID := '';
                end;
                Inc(WordIndex);
                if WordIndex < TestWord.Count then
                begin
                  ContentsTest := Unwords(TestWords, WordIndex);

                  case TypeLabel of
                  'StaffGroup', 'ChoirStaff', 'Staff' :
                    Contents := BalancedDelimiterSubstringWords(ContentsTest, '<<', '>>');

                  'Voice', 'Lyrics', 'FiguredBass' :
                    Contents := BalancedDelimiterSubstringWords(ContentsTest, '{', '}');
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  if not Contents.IsEmpty then
  begin
    NewObject := TLyObject.Create(TypeLabel, ID, LinkID, Contents);
  end;

  result := NewObject;
  FreeAndNil(TestWords);
end;
}

constructor TLyObject.Create(LyInput: TStringList);
begin
  Create(LyInput.Text);
  Self := NumberElementsInOrder;
  Self := AddLyrics;
end;


function TLyObject.AddLyrics: TLyObject;
var
  LyricsDict: TLyricsDict;
  ThisLyricEntry: TLyricsDict.TDictionaryPair;
  ThisVoice: TLyObject = nil;
begin
  LyricsDict := TLyricsDict.Create(Self);
  for ThisLyricEntry in LyricsDict do
  begin
    ThisVoice := Self.FindElement(ekLayer, ThisLyricEntry.Key);
    if Assigned(ThisVoice) then
    begin
      ThisVoice.Measures := ThisVoice.Measures.AddLyrics(ThisLyricEntry.Value);
    end;
  end;
  FreeAndNil(LyricsDict);
  result := Self;
end;

type
  TClefKind = (ckNone, ckTreble, ckSoprano, ckMezzoSoprano, ckAlto, ckTenor,
    ckBaritone, ckBass, ckTreble8va); 

function FindLyClef(Source: String): TClefKind;
var
  TestStr, ClefStr: String;
  Clef: TClefKind = ckNone;
begin
  if Source.Contains('\clef ') then
  begin
    TestStr := StringDropBefore(Source, '\clef ');
    ClefStr := CopyFirstQuotedString(TestStr);
    case ClefStr of
      'treble'       : Clef := ckTreble;
      'soprano'      : Clef := ckSoprano; 
      'mezzosoprano' : Clef := ckMezzoSoprano;
      'alto'         : Clef := ckAlto;
      'tenor'        : Clef := ckTenor;
      'baritone'     : Clef := ckBaritone;
      'bass'         : Clef := ckBass;
      'treble_8'     : Clef := ckTreble8va;
    end;
  end;
  result := Clef;
end;

function AddMEIClefAttribute(StaffDefNode: TMeiNode; Clef: TClefKind): TMeiNode;
var
  ClefLine: Integer;
  ClefLetter: String;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.Name = 'staffDef');

  case Clef of
    ckNone, ckSoprano           : ClefLine := 1;
    ckTreble, ckMezzoSoprano,   
      ckTreble8va               : ClefLine := 2;
    ckAlto, ckBaritone          : ClefLine := 3;
    ckTenor, ckBass             : ClefLine := 4;
  end;
  
  case Clef of
    ckNone, ckTreble, 
      ckTreble8va               : ClefLetter := 'G';
    ckSoprano, ckMezzoSoprano,   
      ckAlto, ckTenor           : ClefLetter := 'C';
    ckBaritone, ckBass          : ClefLetter := 'F';
  end;

  StaffDefNode.AddAttribute('clef.line', IntToStr(ClefLine));
  StaffDefNode.AddAttribute('clef.shape', ClefLetter);

  if Clef = ckTreble8va then
  begin
    StaffDefNode.AddAttribute('clef.dis', '8');
    StaffDefNode.AddAttribute('clef.dis.place', 'below');
  end;
  result := StaffDefNode;
end;

function AddMEIKeyAttribute(StaffDefNode: TMeiNode; Key: TKeyKind): TMeiNode;
var
  KeySig: String;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.Name = 'staffDef');

  case Key of 
    kkNone, 
      kkCantusDurus   : KeySig := '0';
    kkCantusMollis    : KeySig := '1f';
    kkCMaj,  kkAMin   : KeySig := '0';
    kkGMaj,  kkEMin   : KeySig := '1s';
    kkDMaj,  kkBMin   : KeySig := '2s';
    kkAMaj,  kkFsMin  : KeySig := '3s';
    kkEMaj,  kkCsMin  : KeySig := '4s';
    kkBMaj,  kkGsMin  : KeySig := '5s';
    kkFsMaj, kkDsMin  : KeySig := '6s';
    kkCsMaj, kkAsMin  : KeySig := '7s';
    kkFMaj,  kkDMin   : KeySig := '1f';
    kkBbMaj, kkGMin   : KeySig := '2f';
    kkEbMaj, kkCMin   : KeySig := '3f';
    kkAbMaj, kkFMin   : KeySig := '4f';
    kkDbMaj, kkBbMin  : KeySig := '5f';
    kkGbMaj, kkEbMin  : KeySig := '6f';
    kkCbMaj, kkAbMin  : KeySig := '7f';
  end;

  StaffDefNode.AddAttribute('key.sig', KeySig);
  result := StaffDefNode;
end;

type
  TMeterKind = (mkNone, mkMensuralTempusImperfectum,
    mkMensuralProportioMinor, mkModern);
  
  TMeter = class 
  private
    var
      FKind: TMeterKind;
      FCount, FUnit: Integer;
  public
    constructor Create(LyMeterStr: String);
    property Kind:      TMeterKind read FKind;
    property BeatCount: Integer    read FCount;
    property BeatUnit:  Integer    read FUnit;
  end;

constructor TMeter.Create(LyMeterStr: String);
var
  SearchStr, NumStr: String;
  MeterNums: TStringArray;
begin
  inherited Create;

  if LyMeterStr.Contains('\MeterDuple') then
    FKind := mkMensuralTempusImperfectum
  else if LyMeterStr.Contains('\MeterTriple') then
    FKind := mkMensuralProportioMinor
  else if LyMeterStr.Contains('\time ') then
  begin
    FKind := mkModern;
    SearchStr := StringDropBefore(LyMeterStr, '\time ');
    NumStr := ExtractWord(1, SearchStr, [' ', LineEnding]);

    MeterNums := NumStr.Split(['/'], 2);
    FCount := StrToInt(MeterNums[0]);
    FUnit := StrToInt(MeterNums[1]);
  end;
end;

function AddMEIMeterAttribute(StaffDefNode: TMeiNode; Meter: TMeter): TMeiNode;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.Name = 'staffDef');

  with Meter do
  begin
    case Kind of
      mkMensuralTempusImperfectum : 
      begin
        StaffDefNode.AddAttribute('mensur.sign', 'C');
        StaffDefNode.AddAttribute('mensur.tempus', '2');
      end;

      mkMensuralProportioMinor : 
      begin
        StaffDefNode.AddAttribute('mensur.sign', 'C');
        StaffDefNode.AddAttribute('mensur.tempus', '2');
        StaffDefNode.AddAttribute('proport.num', '3');
      end;

      mkModern : 
      begin
        StaffDefNode.AddAttribute('meter.count', IntToStr(BeatCount));
        StaffDefNode.AddAttribute('meter.unit', IntToStr(BeatUnit));
      end;
    end;
  end;
  result := StaffDefNode;
end;

function AddStaffDefAttributes(StaffDefNode: TMeiNode; Clef: TClefKind; Key:
  TKeyKind; Meter: TMeter): TMeiNode;
begin
  Assert(Assigned(StaffDefNode));
  Assert(StaffDefNode.Name = 'staffDef');

  StaffDefNode.AddAttribute('lines', '5');
  StaffDefNode := AddMEIClefAttribute(StaffDefNode, Clef);
  StaffDefNode := AddMEIKeyAttribute(StaffDefNode, Key);
  StaffDefNode := AddMEIMeterAttribute(StaffDefNode, Meter);

  result := StaffDefNode;
end;

function AddStaffGrpAttributes(StaffGrpNode: TMeiNode): TMeiNode;
begin
  Assert(Assigned(StaffGrpNode));
  Assert(StaffGrpNode.Name = 'staffGrp');

  StaffGrpNode.AddAttribute('bar.thru', 'false');
  StaffGrpNode.AddAttribute('symbol', 'bracket');

  result := StaffGrpNode;
end;

function TLyObject.ToMeiScoreDefNode: TMeiNode;
var 
  SearchStr: String;
  Clef: TClefKind;
  Key: TKeyKind;
  Meter: TMeter;
  NewNode: TMeiNode;
begin
  NewNode := TMeiNode.Create();
  NewNode.AddAttribute('n', IntToStr(FNum));
 
  if not FID.IsEmpty then
  begin
    NewNode.AddAttribute('xml:id', FID); 
  end;
  
  case FType of
    ekStaffGrp:
    begin
      NewNode.Name := 'staffGrp';
      NewNode := AddStaffGrpAttributes(NewNode);
    end;

    ekStaff:
    begin
      NewNode.Name := 'staffDef';
      
      { Extract staffDef info from the first music expression in the first
      child Voice }
      if Assigned(FChild) and (FChild.LyType = ekLayer) then
      begin
        { Search c. first 10 lines }
        SearchStr := FChild.Contents.Substring(0, 800); 
        Clef  := FindLyClef(SearchStr);
        Key   := FindLyKey(SearchStr);
        Meter := TMeter.Create(SearchStr);
        NewNode := AddStaffDefAttributes(NewNode, Clef, Key, Meter); 
        FreeAndNil(Meter);
      end;
    end;
  end;
  result := NewNode;
end;

function TLyObject.ToMeiScoreDef: TMeiNode;

  function InnerScoreDef(LyNode: TLyObject): TMeiNode;
  var
    MeiTree: TMeiNode;
  begin
    MeiTree := LyNode.ToMeiScoreDefNode;

    if (LyNode.LyType = ekStaffGrp) and Assigned(LyNode.Child) then
    begin
      MeiTree := MeiTree.AppendChild(InnerScoreDef(LyNode.Child));
    end;
    
    if Assigned(LyNode.Sibling) then
    begin
      MeiTree := MeiTree.AppendSibling(InnerScoreDef(LyNode.Sibling));
    end;
    result := MeiTree;
  end;

var
  ScoreDef, MainStaffGrp: TMeiNode;
begin
  ScoreDef := TMeiNode.Create('scoreDef');
  MainStaffGrp := TMeiNode.Create('staffGrp');
  MainStaffGrp := MainStaffGrp.AppendChild(InnerScoreDef(Self));
  ScoreDef := ScoreDef.AppendChild(MainStaffGrp);
  result := ScoreDef;
end;

function TLyObject.NodeName: String;
var
  NameString: String;
begin
  case FType of
    ekStaffGrp  : NameString := 'staffGrp';
    ekStaff     : NameString := 'staff';
    ekLayer     : NameString := 'layer';
    ekMeasure   : NameString := 'measure';
    else 
      NameString := 'xml';
  end;
  result := NameString;
end;

function TLyObject.FindElement(ElementType: TLyObjectType; 
  ElementID: String = ''): TLyObject;
var
  FoundNode: TLyObject = nil;
  NodeTest: Boolean;
begin
  if ElementID.IsEmpty then
    NodeTest := FType = ElementType
  else
    NodeTest := (FType = ElementType) and (FID = ElementID);

  if NodeTest then
    FoundNode := Self
  else if Assigned(FChild) then
  begin
    FoundNode := FChild.FindElement(ElementType, ElementID);
  end;

  if not Assigned(FoundNode) and Assigned(FSibling) then
  begin
    FoundNode := FSibling.FindElement(ElementType, ElementID);
  end;

  result := FoundNode;
end;


function TLyObject.FindFirstLayer: TLyObject;
begin
  result := FindElement(ekLayer);
end;

function TLyObject.FindFirstStaff: TLyObject;
begin
  result := FindElement(ekStaff);
end;

function TLyObject.ToMeiLayerPath(MeiNode: TMeiNode = nil): TMeiNode;
begin
  if not Assigned(MeiNode) then
  begin
    case FType of
      ekStaff, ekLayer :
      begin
        MeiNode := TMeiNode.Create(NodeName);
        if not FID.IsEmpty then
        begin
          MeiNode.AddAttribute('xml:id', FID);
        end;
        MeiNode.AddAttribute('n', IntToStr(FNum));
      end;
      { Ignore other node types }
    end;
  end;

  if FType = ekLayer then
    result := MeiNode
  else if Assigned(FChild) then
    result := MeiNode.AppendChild(FChild.ToMeiLayerPath)
  else 
    result := nil;
end;

constructor TLyricsDict.Create();
begin
  inherited Create();
end;

constructor TLyricsDict.Create(LyTree: TLyObject);

  function AddNewLyrics(Dict: TLyricsDict; LyTree: TLyObject): TLyricsDict;
  var
    SyllableList: TSyllableList = nil;
  begin
    if LyTree.LyType = ekLyrics then
    begin
      SyllableList := TSyllableList.Create(LyTree.Contents);

      Dict.Add(LyTree.LinkID, SyllableList);
    end;
    if Assigned(LyTree.Child) then
    begin
      Dict := AddNewLyrics(Dict, LyTree.Child);
    end;
    if Assigned(LyTree.Sibling) then
    begin
      Dict := AddNewLyrics(Dict, LyTree.Sibling);
    end;
    result := Dict;
  end;

begin
  inherited Create();
  Self := AddNewLyrics(Self, LyTree);
end;

destructor TLyricsDict.Destroy;
var
  ThisEntry: TLyricsDict.TDictionaryPair;
begin
  for ThisEntry in Self do
  begin
    ThisEntry.Value.Free;
  end;
  inherited Destroy;
end;

{ TODO need to make sure all voices have same number of measures }
function CreateMeiMeasure(LyLayer: TLyObject; MeasureNum: Integer): TMeiNode;
var
  MeasureNode: TMeiNode = nil;
  MeasureList: TMeasureList;
begin
  if (LyLayer.LyType = ekLayer) then
  begin
    MeasureList := LyLayer.Measures;
    if MeasureNum < MeasureList.Count then
    begin
      MeasureNode := LyLayer.Measures.Items[MeasureNum].ToMEI;
    end
    else
      WriteLn(stderr, 'Measure number out of range');
  end;
  
  result := MeasureNode;
end;

function AddMeiFermatasAndLines(LyNode: TLyObject; MeiMeasure: TMeiNode; 
  MeasureNum: Integer): TMeiNode;
var
  ThisMeasureList: TMeasureList;
  ThisMeasure: TPitchList;
  LyFermatas: TFermataList;
  LyLines: TLineList;
  MeiFermatas, MeiLines: TMeiNode;
begin
  Assert(Assigned(LyNode) and Assigned(MeiMeasure));
  Assert(LyNode.LyType = ekLayer);

  ThisMeasureList := LyNode.Measures;

  if MeasureNum < ThisMeasureList.Count then
  begin
    ThisMeasure := ThisMeasureList.Items[MeasureNum];
    LyFermatas := ThisMeasure.FermataList;
    MeiFermatas := LyFermatas.ToMEI;
    MeiMeasure := MeiMeasure.AppendChild(MeiFermatas);

    LyLines := ThisMeasure.LineList;
    MeiLines := LyLines.ToMEI;
    MeiMeasure := MeiMeasure.AppendChild(MeiLines);
  end;

  result := MeiMeasure;
end;

function BuildMeiMeasureTree(LyTree: TLyObject; MeiTree: TMeiNode; 
  MeasureNum: Integer): TMeiNode;
var
  MeiLayerPath, MeiMusicNode: TMeiNode;
  LyStaff, LyLayer: TLyObject;
begin
  Assert(Assigned(MeiTree));

  if Assigned(LyTree) then
  begin
    LyStaff := LyTree.FindFirstStaff;
    LyLayer := LyStaff.FindFirstLayer;
    
    MeiLayerPath := LyStaff.ToMeiLayerPath;
    MeiMusicNode := CreateMeiMeasure(LyLayer, MeasureNum);
    MeiLayerPath := MeiLayerPath.AppendLastChild(MeiMusicNode);
    
    MeiTree := MeiTree.AppendChild(MeiLayerPath);
    MeiTree := AddMeiFermatasAndLines(LyLayer, MeiTree, MeasureNum);
  { TODO
    MeiTree := AddFiguredBass(LyLayer, MeiTree, MeasureNum); }

    if Assigned(LyStaff.Sibling) then
    begin
      MeiTree := BuildMeiMeasureTree(LyStaff.Sibling, MeiTree, MeasureNum);
    end;
  
    if (LyTree.LyType <> ekStaff) and Assigned(LyTree.Sibling) then
    begin
      MeiTree := BuildMeiMeasureTree(LyTree.Sibling, MeiTree, MeasureNum);
    end;
  end;
  result := MeiTree;
end;


{ Flip an @code(LyObjectTree) with a staff/voice/measure hierarchy
  to make a @code(MeiNode) tree with a measure/staff/voice hierarchy. }
  { TODO we do not support multiple \score expressions }
function TLyObject.ToMeiSection: TMeiNode;
var
  LayerNode: TLyObject;
  MeasureList: TMeasureList;
  MeiSection, MeiMeasureTree: TMeiNode;
  MeasureCount, MeasureNum: Integer;
begin
  MeiSection := TMeiNode.Create('section');
  
  LayerNode := Self.FindFirstLayer;
  if Assigned(LayerNode) then
  begin
    MeasureList := LayerNode.Measures;
    MeasureCount := MeasureList.Count;
    for MeasureNum := 0 to MeasureCount - 1 do
    begin
      MeiMeasureTree := TMeiNode.Create('measure');
      MeiMeasureTree.AddAttribute('n', IntToStr(MeasureNum + 1));

      { Section heading (from MeasureList), first measure only }
      if MeasureNum = 0 then
      begin
        MeiMeasureTree := MeasureList.AddMeiSectionHead(MeiMeasureTree);
      end;

      { Barline for this measure (from PitchList = lirio:measure) }
      MeiMeasureTree := AddMeiBarlineAttr(MeiMeasureTree, 
        MeasureList.Items[MeasureNum]);

      { For this measure, staff/layer/notes }
      MeiMeasureTree := BuildMeiMeasureTree(Self, MeiMeasureTree, MeasureNum);
      MeiSection := MeiSection.AppendChild(MeiMeasureTree);
    end;
  end;
  result := MeiSection;
end;

function CreateMeiScore(LyInput: TStringList): TMeiNode;
  
  function CreateEmptyMeiScore: TMeiNode;
  var
    MeiMusic, MeiBody, MeiMdiv, MeiScore: TMeiNode;
  begin
      MeiMusic  := TMeiNode.Create('music');
      MeiBody   := TMeiNode.Create('body');
      MeiMdiv   := TMeiNode.Create('mdiv');
      MeiScore  := TMeiNode.Create('score');
      MeiMusic  := MeiMusic.AppendChild(MeiBody);
      MeiBody   := MeiBody.AppendChild(MeiMdiv);
      MeiMdiv   := MeiMdiv.AppendChild(MeiScore);
      result := MeiMusic;
  end;

var
  LyTree: TLyObject;
  MeiScore, MeiScoreDef, MeiSection: TMeiNode;
begin
  LyTree := TLyObject.Create(LyInput);
 
  MeiScore    := CreateEmptyMeiScore;
  MeiScoreDef := LyTree.ToMeiScoreDef;
  MeiSection  := LyTree.ToMeiSection;

  MeiScore := MeiScore.AppendLastChild(MeiScoreDef);
  MeiScoreDef := MeiScoreDef.AppendSibling(MeiSection);
  
  FreeAndNil(LyTree);
  result := MeiScore;
end;

function CreateMeiDocument(LyInput: TStringList): TMeiNode;
var
  Root, MeiHeader, MeiScore: TMeiNode;
begin
  Root      := TMeiNode.CreateMeiRoot;
  MeiHeader := CreateMeiHead(LyInput);
  MeiScore  := CreateMeiScore(LyInput);

  Root := Root.AppendChild(MeiHeader);
  Root := Root.AppendChild(MeiScore);
  result := Root;
end;

end.
