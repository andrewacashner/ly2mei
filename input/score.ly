\version "2.23"
\header {
  title = "Simple Lilypond Score with Default Syntax"
  composer = "Andrew Cashner"
}

MusicS = {
  \clef "treble"
  \time 4/4
  \key cis\minor
  | c'4 d'4 e'4 f'4
  | f'4 e'4 d'4 c'4
}
LyricsS = \lyricmode {
  One two three four
  du -- ple me -- ter
}

MusicT = {
  \clef "treble_8"
  \time 4/4
  \key ges \major
  | a4 b4 c'4 d'4
  | d'4 c'4 bes4 a4
}
LyricsT = \lyricmode {
  Un deux trois quatre
  im -- per -- fec -- tum
}

MusicB = {
  \clef "bass"
  \time 4/4
  \key as \minor
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

