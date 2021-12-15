MarkupI = \markup "Hello"
MarkupII = \markup { "World" }
% ignore
% {{{1 even if there are braces
MusicSoprano = { c'4 d'4 e'2 }
% }}}1
\new Staff = "Melody" { \MusicSoprano }
LyricsSoprano = \lyricmode { ly -- ric text^\MarkupII }
MusicAlto = \MusicSoprano % { what about braces that start midline? }
LyricsAlto = \LyricsSoprano
<<
\MusicAlto
\LyricsAlto
>>

