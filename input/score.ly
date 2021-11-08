\version "3.0"
\header {
  title = "Test of Score Reading"
  composer = "Andrew Cashner"
}

MusicS = {
  c'4 d'4 e'4
}
LyricsS = \lyricmode {
  One Two Three
}

\score {
  <<
    \new ChoirStaff = "voices"
    <<
      \new Staff 
      <<
        \new Voice = "Soprano" { \MusicS }
        \new Lyrics = "lyrics-Soprano" \lyricsto "Soprano" { \LyricsS }
      >>
      \new Staff 
      <<
        \new Voice = "Alto" { \MusicS }
        \new Lyrics = "lyrics-Alto" \lyricsto "Alto" { \LyricsS }
      >>
    >>
  >>
}

% <score>
%     <choirStaff>
%         <staff>
%             <voice></voice>
%             <lyrics></lyrics>
%         </staff>
%         <staff>
%             <voice></voice>
%             <lyrics</lyrics>
%         </staff>
%     </choirStaff>
% </score>
