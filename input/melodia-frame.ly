\version "2.2"

\input "melodia-data.ly"

\header {
    title = \title
    composer = \composer
    poet = \poet
    source = \source 
}

\score {
    <<
        \new staff 
        <<
            \new voice = "mel" { \clef "treble" \melody }
            \new lyrics \lyricsto "mel" { \lyricmode { \words } }
        >>
    >>
}
