\version "2.28"

% Mozart Clarinet Quintet
\header {
  subtitle = "[Fragment]"
  title = "Missa da Requiem"
  composer = \markup { 
    \column {
      \line { "Wolfgang" \italic "Amadé" }
      \line { "Mozart" }
      "(1756-1791)"
    }
  }
  poet = "Roman Catholic Liturgy"
  editor = "Werner von Braun"
  source = \markup { "Vienna," 
    \italic "Österreichische Staatsbibliothek" }
  copyright = "Public domain"
}

MusicViolinI = {
  \clef "treble"
  \time 4/4
  \key a\major
  | e''2 cis''2
  | b''2 a''2
}

\score {
  <<
    \new Staff
    <<
      \new Voice { \MusicViolinI }
    >>
  >>
}

