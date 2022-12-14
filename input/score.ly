\version "2.22"
\header {
  title = "Simple Lilypond Score with Default Syntax"
  composer = "Andrew Cashner"
}

MusicS = {
  \clef "treble"
  \time 4/4
  \key f\major
  | c''4(-^ c''4 a'2() \break
  | bes'4) a'2-- c''4\fermata
  \bar "|."
}
LyricsS = \lyricmode {
  syn -- co -- pa
}

MusicSII = {
  \clef "treble"
  \time 4/4
  \key f\major
  | R1*2
}

MusicA = {
  \clef "treble"
  \time 4/4
  \key f\major
  | c'4(\tenuto d'4-!) e'4 \[ f'4->(~
  | f'4 e'4)(\marcato \] d'4 c'4\fermata)
}
LyricsA = \lyricmode {
  Ma -- ny slurs __
}

MusicT = {
  \clef "treble_8"
  \time 4/4
  \key f\major
  | a4\staccato b4-. c'4 d'4~
  | d'4 d'4 bes4-.--\marcato a4\fermata
}
LyricsT = \lyricmode {
  Ri -- sing \markup { \italic { notes } } tied _ back down
}

MusicB = {
  \clef "bass"
  \time 4/4
  \key f\major
  | c2. r4
  | c1\fermata
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

