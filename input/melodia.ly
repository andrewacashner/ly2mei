\version "2.2"

\header {
    title = "Canción inventada"
    composer = "Cantor desconocido"
    poet = "Autor desconocido"
    source = "Barcelona"
}

Melodia = {
    \clef "treble"
    \time 3/4
    \key bes\minor

    | f'4 ges'4 aes'4
    | aes'4( ges'2)
    | f'2 r4
    \FinalBar
}

Palabras = \lyricmode {
    ¡can -- ta,
    can -- tan -- te!
}

\score {
    <<
        \new Staff 
        <<
            \new Voice = "mel" { \Melodia }
            \new Lyrics \lyricsto "mel" { \Palabras }
        >>
    >>
}
