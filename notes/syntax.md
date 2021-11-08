# Ignore

% ...
: comment, ignore

\version :: String
: ignore

\layout {}
: ignore

\lyricmode { a~|y }
: add space before y (ignore)

\break
: ignore

## Possibly ignore, or read file

\include :: String
: input filename in string with its definitions

    If it is a local include, input file contents directly; if it is a library
    include, ignore

# Metadata, header
\header :: newline-delimited list of key/value pairs, in form: label = String
: store metadata for header (may include markup which must be processed)

\markup :: String, {} delimited Markup group, or Markup command, or series of
strings
: format text according to command; concatenate list of strings

# Define macros

VariableLabel = { ... } :: ly-music
: define macro that expands to arg

    In Lilypond, process the argument as a music expression and define the
    given symbol to refer to that

IncipitXX = {}
: store macro that will be used in incipit staff for voice XX (perhaps distinction
unnecessary)

MusicXX = {}
: store macro that will be used in music expression for voice XX

LyricsXX = \lyricmode {}
: define macro for lyrics for voice XX

    Lilypond: process lyric expression and assign to variable

# Already defined macros

\MSclefCi, \MSclefCii, \MSclefFiv, \MSclefGii
: translate to clef: C1, C2, F4, G2, etc.

\CantusMollis
: set key signature to 1 flat on B

\MeterZ, MeterCZ, \MeterCThree, etc. 
: set meter signature *symbol* to Z, CZ, C3, etc.

\clef :: String
: set clef

\MeterTriple, \MeterDuple
: set 3/2 or 2/2 meter

\fermata
: add fermata

\SectionBreak
: new section

\MiddleBar, \FinalBar, \RepeatBarEnd
: insert those kinds of bars

## Macros with arguments
\Section :: String
: create section heading (rehearsal mark) with text of string before the note
that follows

\Fine, \FineEd, \DSalCoda, etc; \RepeatMsg :: String
: add text "Fine", "[Fine]", "D. S. al coda" or arbitrary string in
appropriate place

# Pitches

e''4 e''4. es'4 fis,2 c,,\breve
: make music note

    - alphabetic chars up to ,' or number = letter name
    - first char = pitch class
    - -es = flat
    - -is = sharp (accidentals always explicit, natural if none)
    - no , or ' = octave 3; ' = octave 4; '' = 5 etc; , = oct 2; ,, = 1; etc.
    - number = duration: 4 = quarter, 2 = half, etc.
    - dot = add dot

|
: barline, contents between two are one measure

a'2~ | a'2
: tie across barline

`R1.*2`
: full-bar rest of given numeric+dot value * factor given
    
    treat as macro for r1. r1.

# Articulations, lines

f'4( 
: start slur 

g'4)
: end slur

a'2\color
: start coloration bracket

f'1\endcolor
: end coloration bracket

bes'8[
: start ligature

a'8]
: end ligature

# Figured bass

`<< { c4 }  \figures { <_->4 } >>`
: `<<...>>` align these two things simultaneously (vertically)

\figures {}
: add figured bass expression 

`<_->4`
: figured bass: b3 of duration 4

# Lyrics

\lyricmode{ Ve -- nid, que~hoy a -- ten -- ded. __ }
: lyric text, space delimits words, -- delimits syllables; ~ or _ mark
elision; __ marks extender (ignore)

\StanzaI, \StanzaII
: show given stanza number in lyrics

\EdLyrics { }
: put these lyrics in italics

\NextLyricsLine "thislabel" "prevlabel" "thisvoice" { \LyricsXX }
: add lyrics labeled "thislabel" to "thisvoice" below lyrics with "prevlabel"


# Building score

\score { }
: put music into score

\new ChoirStaff = "label" << ... >>
: put contents of angle brackets into a choirstaff with label

\new StaffGroup = "label" << ... >>
: same but for staff group

\new Staff = "label" << ... >>
: put contents of angle brackets into staff with label

\IncipitStaff "long" "short" { \IncipitXX }
: make an incipit staff with music in \IncipitXX; set the long and short instrument-name labels

\new Voice = "XX" { \MusicXX }
: put the music on the staff for voice XX

\new Lyrics = "label" \lyricsto "XX" { \LyricsXX } 
: add lyrics to align with voice XX

\ShowChoirStaffBracket
: show bracket around choir staff even if only one voice in group

\TwoLineName "top" "bottom" "short"
: make instrument name where long name is two rows with "top" over "bottom";
and short name is "short"

-------------------------------------

# Thoughts

- Defining and substituting textual macros is one separate problem
    - Substituting ones pre-defined (e.g., `\FinalBar`)
    - Reading new macro definitions and expanding them 
        (e.g., `MusicA = {}, ... \MusicA`)
- Parsing header is fairly simple (though markup can be complex)
- Parsing score is ok except it's presented on each staff as full voice and
  full lyrics (+ full figures), rather than MEI barwise
    - but bars are delimited in this input format so they could be parsed
    - incipt staves?
- Matching lyrics with notes is difficult
- Can't handle arbitrary commands, Ly or Scheme: must be strict subset
- parsing Pitch elements is fairly easy

- hardest part is where input structure doesn't match (e.g., voicewise vs barwise)
     - but, e.g.: for each voice, use delimiters to make list of bars; then
       pivot the lists

## Macro definitions

### Syntax
Label = { }
Label = \lyricmode { }
Label = \figuremode { }

=> \Label

where Label is start of line (?) in "regular" mode
where Label is not preceded by \new
where Label is not inside \header {}

### Procedure
1. identify definition expression (label = {})
2. in assoc-list, store label as key and contents of {} (plain text) as value
3. identify macro expression (\label), look up key and replace with value

## Converting \score

### MEI
- mei @xmlns
    - meiHead [...]
    - music
        - mdiv: @n @type
            - score:
                - scoreDef: @key.sig [@midi.bpm]
                    - staffGrp: @n @xml:id @symbol="bracket"
                        - staffDef: @n @xml:id @lines="5" @clef.shape @clef.line
                                @mensur.sign @mensur.slash @mensur.tempus
                                @proport.num 
                            - label
                            - labelAbbr
                            - [layerDef: @n @xml:id]
                - measure: @n
                    - staff: @n @corresp="#{$xml:id}" (or @def?)
                        - layer: @n [@corresp="#{@xml:id}"]
                            - note: @pname @dur @dots 
                                - accid: @accid.ges
                                - verse
                                    - syl: @con @wordpos
                            - OR rest: @dur @dots

### TBD
- incipit staves??
- figured bass?

### Source of MEI data coming from Lilypond

music/mdiv/score 
    <- \score {}

score/scoreDef/@key.sig 
    <- first music expression, \CantusMollis or not (or \key )

score/scoreDef/staffGrp 
    <- \new ChoirStaff or StaffGroup (or none)

//staffDef/@clef.shape, @clef.line 
    <- first music expression on that staff, \clef ""

//staffDef/@mensur.sign, @mensur.slash, @mensur.tempus, @proport.num
    <- first music expr on that staff, \MeterTriple or \MeterDuple or \time

//staffDef/label, labelAbbr 
    <- \InstrumentName "LONG" "SHORT" or \IncipitStaff "LONG" "SHORT"

//score/measure/staff/layer/note, rest 
    <- \score { << ... \new Staff << \new Voice { | note note | }

//score/measure/staff/layer/note/verse/syl 
    <- \new Lyrics \lyricsto "label" { Ly -- rics }



Ly:
\score { << \new ChoirStaff << \new Staff << \new Voice { note1 note2 } \new
Lyrics { syl1 syl2 } >> >> >> }
-> music/mdiv/score/scoreDef
-> music/mdiv/score/measure(1)/staff/layer(1)/note{$note1}/verse/syl{$syl1}
                              /staff/layer(2)/...
                   /measure(2)/staff/layer(1)/note{$note2}/verse/syl{$syl2}
                   /measure(2)/staff/layer(2)/...

ly: staff/voice/(measure/)note
    staff/lyrics/syllable

mei: measure/staff/layer/note/verse/syl


- read through whole ly \score{} and store all needed info:
    - number and labels of staff groups or choir staves
    - for each group: number of staves
    - for each staff: labels(instrument names), number of voices, voiceIDs
    - for each voice: 
        - clef, key, meter
        - measures/notes, expression marks, section heads, barlines
        - lyric syllables
        - bass figures

- then assemble MEI structure pulling out this data as needed
- e.g., for each measure:
    - get that measure from each staff/each voice
    - get those syllables (!) 

intermediate form for music/lyrics:

voice:soprano, measure:1, notes:(c'4, d'4, es'4), lyrics:(one, two, three)
voice:soprano, measure:2, notes:(e'4, fis'4, g'2), lyrics:(ev-, -en, more)

