\version "2.23"
\include "early-music.ly"
\header {
  title = "Test of Lirio Score Reading"
  composer = "Andrew Cashner"
}

MusicS = {
  \clef "treble"
  \MeterDuple
  \CantusMollis
  \Section "DEMONSTRACIÓN"
  | c'4 d'4( e'4 f'4~\color
  | f'4 e'4)\endcolor d'4 c'4
}
LyricsS = \lyricmode {
  Me -- lis -- ma -- ta 
}

MusicSII = {
  \clef "treble"
  \MeterDuple
  \CantusMollis
  | R1*2
}

MusicA = {
  \clef "alto"
  \MeterDuple
  \CantusMollis
  | \[ c'4( d'4 \] e'4) f'4~
  | f'4 e'4\color d'4 c'4\endcolor
}
LyricsA = \lyricmode {
  Can -- ción de no -- tas
}

MusicT = {
  \clef "treble_8"
  \MeterDuple
  \CantusMollis
  | a4 b4 c'4 d'4
  | \[ d'4 c'4\color bes4\endcolor( a4) \]
}
LyricsT = \lyricmode {
  mun -- dum sub -- ter -- ra -- ne -- um
}

MusicB = {
  \clef "bass"
  \MeterDuple
  \CantusMollis
  | c2. r4
  | c1
}

FiguresB = \figuremode {
  | <6>1
  | s1
}

\score {
  <<
    \new ChoirStaff = "voices"
    <<
      \new Staff = "s-Soprano"
      <<
        \new Voice = "Soprano" { \MusicS }
        \new Lyrics \lyricsto "Soprano" { \LyricsS }
      >>
      \new Staff = "s-SopranoII"
      <<
        \new Voice = "SopranoII" { \MusicSII }
      >>
      \new Staff = "s-Alto"
      <<
        \new Voice = "Alto" { \MusicA }
        \new Lyrics \lyricsto "Alto" { \LyricsA }
      >>
      \new Staff = "s-Tenor"
      <<
        \new Voice = "Tenor" { \MusicT }
        \new Lyrics \lyricsto "Tenor" { \LyricsT }
      >>
    >>
    \new ChoirStaff = "accompaniment"
    <<
      \new Staff = "s-continuo"
      <<
        \new Voice = "Bass" { \MusicB }
        \new FiguredBass { \FiguresB }
      >>
    >>
  >>
}

