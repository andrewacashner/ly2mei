\version "2.2"

\include "input/melodia-data.ly"

\header {
    title = \title
    composer = \composer
    poet = \poet
    source = \source 
}

\score {
    <<
        \new Staff 
        <<
            \new Voice = "mel" { \clef "treble" \melody }
            \new Lyrics \lyricsto "mel" { \lyricmode { \words } }
        >>
    >>
}
