MusicSoprano = { c''4 d''4 es''4 }
This is not a macro % this is a comment
LyricsSoprano = \lyricmode { ly -- ric text }
LyricsAlto = \lyricmode { some \EdLyrics { more brac -- kets } }
LyricsTenor = \lyricmode { some \EdLyrics { more brac -- kets } and more text }

\new Voice = "S" { \MusicSoprano }
\new Lyrics \lyricsto "S" { \LyricsSoprano }
\new Lyrics \lyricsto "A" { \LyricsAlto }
MusicBass = { c8 d8 e8 f8 g2 g,2
  c4 e4 f4 g4 c1 }

% {{{1
MusicAc = { c1 c1 \MusicBass }
\new Staff = "Basses"
  <<
    % {{{2
    \new Voice = "B1" { \voiceOne \MusicBass }
    \new Voice = "B2" { \voiceTwo \MusicAc }
    % }}}2
  >>
% }}}1

LevelOne = { \MusicSoprano }
LevelTwo = \LevelOne % { no argument
LevelThree = \LevelTwo
\LevelThree

