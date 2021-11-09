\version "3.0"
\header {
  title = "Test of Score Reading"
  composer = "Andrew Cashner"
}

MusicS = {
  \clef "treble"
  \DupleMeter
  \CantusMollis
  | c'4 d'4 e'4 f'4
  | f'4 e'4 d'4 c'4
}
LyricsS = \lyricmode {
  One two three four
  du -- ple me -- ter
}

MusicA = {
  \clef "treble"
  \DupleMeter
  \CantusMollis
  | a4 b4 c'4 d'4
  | d'4 c'4 b4 a4
}
LyricsA = \lyricmode {
  Un deux trois quatre
  im -- per -- fec -- tum
}

MusicB = {
  \clef "bass"
  \DupleMeter
  \CantusMollis
  | c1
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
      \new Staff = "s-Alto"
      <<
        \new Voice = "Alto" { \MusicS }
        \new Lyrics \lyricsto "Alto" { \LyricsS }
      >>
    >>
    \new ChoirStaff = "accompaniment"
    <<
      \new Staff = "s-continuo"
      <<
        \new Voice = "Bass" { \MusicB }
        \new Figures { \FiguresB }
      >>
    >>
  >>
}

