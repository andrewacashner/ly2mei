% vim: set foldmethod=marker :

% Juan Hidalgo, Venid querubines alados (D-Mbs: Mus. mss. 2897)
% Edited by Andrew A. Cashner, 2019

% {{{1 history
% 2019-09-18    Begun in Lilypond
% }}}1

\version "2.23"
\include "villancico.ly"

% {{{1 header
\header {
  title     = "Venid, querubines alados"
  subtitle  = "De nuestra Señora a 4"
  composer  = "JUAN HIDALGO"
  dates     = "(1614–1685)"
  poet      = "Anonymous"
  editor    = "Andrew A. Cashner"
  copyright = "Copyright © 2021 Andrew A. Cashner"
  source    = \markup { \italic "D-Mbs:" "Mus. mss. 2897" }
}
% }}}1

% {{{1 music
% {{{2 incipits
IncipitSI = {
  \MSclefCi
  \CantusMollis
  \MeterZ
  bes'2
}

IncipitSII = {
  \MSclefCi
  \CantusMollis
  \MeterZ
  bes'2
}

IncipitSIII = {
  \MSclefCi
  \CantusMollis
  \MeterZ
  d'2
}

IncipitT = {
  \MSclefCiv
  \CantusMollis
  \MeterZ
  bes2
}

IncipitAc = {
  \MSclefFiv
  \CantusMollis
  \MeterZ
  bes2
}
% }}}2

% {{{2 SI
MusicSI = {
  \clef "treble"
  \CantusMollis
  \MeterTriple

  % {{{3 estribillo
  \Section "[ESTRIBILLO] SOLO"
  | r2 r2 bes'2
  | d''2 d''2. e''4
  | f''2. g''4 f''2
  | es''2 d''2 d''2
  | c''1 c''2
  | c''2 d''2 es''2
  | d''2. c''4 bes'2
  | bes'2 a'2 c''2
  | bes'1.

  % m. 10
  | bes'2 bes'2 bes'2
  | bes'2. bes'4 a'2
  | a'2 g'2 bes'2
  | a'2 f'2 f''2 
  | f''2 es''2 es''2
  | es''2. es''4 d''2
  | d''2 c''2 es''2
  | d''1.
  | R1.*2

  % m. 20
  | R1.*10

  % m. 30 
  | R1. \break
  | R1.*2

  % m. 33 a 4
  | r2 r2 
  \Section "A 4"
  f'2
  | bes'2 bes'2 bes'2
  | c''2. c''4 d''2
  | bes'2 bes'2 bes'2
  | a'1 a'2
  | a'2 a'2 a'2
  | bes'2. c''4 bes'2

  % m. 40
  | bes'2 a'2 c''2
  | bes'1.
  | bes'2 bes'2 bes'2
  | bes'2. bes'4 a'2
  | a'2 g'2 bes'2
  | a'2 a'2 d''2
  | d''2 c''2 c''2
  | c''2. c''4 bes'2
  | bes'2 bes'2 a'2
  | bes'1.

  % m. 50 Escuchad
  | r2 bes'2 d''2
  | c''1.
  | r2 bes'2 d''2
  | c''1.~
  | c''1.
  \MiddleBar
  \break

  % m. 55 Duo
  \Section "DÚO"
  | r2 d''2 e''2 
  | f''1.
  | r2 a'2 bes'2
  | c''1.
  | r2 f'4( g'4) a'4( bes'4)

  % m. 60
  | c''2 a'4( bes'4) c''4( d''4)
  | c''2 f'4( g'4) a'4( bes'4)
  | c''2 a'4( bes'4) c''4( d''4)
  | a'2\color f'1\endcolor
  | r2 r2 a'2
  | bes'1\color a'2~
  | a'2\endcolor g'4( a'4 bes'4 c''4)
  | f'1.
  | R1.
  | r2 bes'4( c''4) d''4( e''4)

  % m. 70
  | f''2 d''4( e''4) f''4( g''4)
  | d''2 bes'4( c''4) d''4( e''4)
  | f''2 d''4( e''4) f''4( g''4)
  | d''2\color bes'1\endcolor
  | r2 g'2\color bes'2~
  | bes'2 bes'2. a'4\endcolor
  | bes'1.
  | R1.
  | r2 d''2 e''2
  | f''1.

  % m. 80
  | r2 a'2 bes'2
  | c''1.~

  % m. 82 end estribillo
  | c''1.\fermata
  \FineEd
  \FinalBar
  % }}}3

  % {{{3 coplas
  % m. 83 COPLAS odd
  \SectionBreak
  \MeterDuple
  \Section "COPLAS SOLO (El medio es el golpe del pasa calle)"
  | r2 r4 bes'4
  | d''8 d''16 d''16 c''8 es''8 d''8( c''8) bes'8 bes'8
  \break
  | bes'4 a'4 bes'8[ a'8] g'8 a'8
  | f'2 r8 a'8 a'8 a'16 bes'16
  | c''4 b'4 c''8 d''8 es''8 es''8
  | es''4 d''4 es''8[ d''8] c''8[ d''8]
  | bes'2 r8 a'8 a'8 a'16 bes'16

  % m. 90
  | c''4 b'4 c''8 d''8 es''8 es''8
  | es''4 d''4 es''8[ d''8] c''8[ d''8]
  | bes'1
  \MiddleBar

  % m. 93 Tag
  \MeterTriple
  | r2 d''2 e''2 
  | f''1.
  | r2 a'2 bes'2
  | c''1.
  | r2 f'4( g'4) a'4( bes'4)
  | c''2 a'4( bes'4) c''4( d''4)
  | c''2 f'4( g'4) a'4( bes'4)

  % m. 100
  | c''2 a'4( bes'4) c''4( d''4)
  | a'2\color f'1\endcolor
  | r2 r2 a'2
  | bes'1\color a'2~
  | a'2\endcolor g'4( a'4 bes'4 c''4)
  | f'1.
  | R1.
  | r2 bes'4( c''4) d''4( e''4)
  | f''2 d''4( e''4) f''4( g''4)
  | d''2 bes'4( c''4) d''4( e''4)

  % m. 110
  | f''2 d''4( e''4) f''4( g''4)
  | d''2\color bes'1\endcolor
  | r2 g'2\color bes'2~
  | bes'2 bes'2. a'4\endcolor
  | bes'1.~
  | bes'1.\fermata
  \MiddleBar
 
  % m. 116
  \SectionBreak
  \MeterDuple
  | R1*10
  \MiddleBar

  % m. 126
  \MeterTriple
  | R1.*23
  \RepeatBarEnd
  % }}}3
}
% }}}2

% {{{2 SII
MusicSII = {
  \clef "treble"
  \CantusMollis
  \MeterTriple

  % {{{3 estribillo
  | R1.*16
  
  % m. 17
  | r2 r2 bes'2
  | d''2 d''2 e''2
  | f''2. g''4 f''2

  % m. 20
  | es''2 d''2 d''2
  | c''1 c''2
  | c''2 d''2 es''2
  | d''2. c''4 bes'2 
  | bes'2 a'2 c''2
  | bes'1. 
  | bes'2 bes'2 bes'2
  | bes'2. bes'4 a'2
  | a'2 g'2 bes'2
  | a'2 f'2 f''2

  % m. 30
  | f''2 es''2 es''2
  | es''2. es''4 d''2
  | d''2 c''2 es''2
  | d''1 bes'2
  | d''2 d''2 e''2
  | f''2. g''4 f''2
  | es''2 d''2 d''2
  | c''1 c''2
  | c''2 c''2 c''2
  | es''2 d''2. d''4

  % m. 40
  | d''2 d''2 d''2 
  | d''1.
  | d''2 d''2 d''2 
  | c''2. c''4 c''2
  | c''2 c''2 c''2
  | c''2 c''2 f''2
  | f''2 es''2 es''2
  | es''2. es''4 d''2
  | d''2 c''2 es''2
  | d''1.

  % m. 50
  | r2 d''2 f''2 
  | f''1.
  | r2 d''2 e''2
  | f''1.~
  | f''1.
  \MiddleBar

  % m. 55 Duo
  | R1.
  | r2 d''2 e''2 
  | f''1.
  | r2 a'2 bes'2
  | c''1.

  % m. 60
  | r2 f'4( g'4) a'4( bes'4)
  | c''2 a'4( bes'4) c''4( d''4)
  | c''2 f'4( g'4) a'4( bes'4)
  | c''2 a'4( bes'4) c''4( d''4)
  | a'2\color f'1\endcolor
  | r2 d''2\color f''2~
  | f''2 f''2. e''4\endcolor
  | f''1.
  | r2 bes'4( c''4) d''4( e''4)
  | f''2 d''4( e''4) f''4( g''4)

  % m. 70
  | d''2 bes'4( c''4) d''4( e''4)
  | f''2 d''4( e''4) f''4( g''4)
  | d''2\color bes'1\endcolor
  | r2 r2 d''2
  | es''1\color d''2~
  | d''2 c''1\endcolor
  | d''1.
  | r2 d''2 e''2
  | f''1.
  | r2 a'2 bes'2

  % m. 80
  | c''2 f'2 g'2
  | a'1.~
  | a'1.\fermata
  \FinalBar
  % }}}3

  % {{{3 coplas
  % m. 83 odd coplas SI
  \SectionBreak
  \MeterDuple
  | R1*10
  \MiddleBar

  % m. 93 Tag
  \MeterTriple
  | R1.*23
  \MiddleBar

  % m. 116 even coplas 
  \SectionBreak
  \MeterDuple
  | r2 r4 bes'8 c''8
  | d''8 d''8 c''8 e''8 d''8 c''8 bes'8 bes'8
  \break
  | bes'4 a'4 bes'8[ a'8] g'8[ a'8]
  | f'2 r8 a'8 a'8 bes'8

  % m. 120
  | c''4 b'8 b'8 c''8[ d''8] es''8 es''8
  | es''4 d''4 es''8[ d''8] c''8 d''8
  | bes'2 r8 a'8 a'8 bes'8
  | c''4 b'8 b'8 c''8[ d''8] es''8 es''8
  | es''4 d''4 es''8[ d''8] c''8 d''8
  | bes'1
  \MiddleBar

  % m. 126 Tag
  \MeterTriple
  | R1.
  | r2 d''2 e''2 
  | f''1.
  \break
  | r2 a'2 bes'2

  % m. 130
  | c''1.
  | r2 f'4( g'4) a'4( bes'4)
  | c''2 a'4( bes'4) c''4( d''4)
  | c''2 f'4( g'4) a'4( bes'4)
  | c''2 a'4( bes'4) c''4( d''4)
  | a'2\color f'1\endcolor
  | r2 d''2\color f''2~
  | f''2 f''2. e''4\endcolor
  | f''1.
  | r2 bes'4( c''4) d''4( e''4)

  % m. 140
  | f''2 d''4( e''4) f''4( g''4)
  | d''2 bes'4( c''4) d''4( e''4)
  | f''2 d''4( e''4) f''4( g''4)
  | d''2\color bes'1\endcolor
  | r2 r2 d''2
  | es''1\color d''2~
  | d''2 c''1\endcolor
  | d''1.~

  % m. 148
  | d''1.\fermata
  \RepeatBarEnd
  \DCalFineAfterLastCopla
  % }}}3
}
% }}}2

% {{{2 SIII
MusicSIII = {
  \clef "treble"
  \CantusMollis
  \MeterTriple
  | R1.*32
  
  % m. 33 a 4
  | r2 r2 d'2
  | f'2 f'2 g'2
  | a'2. a'4 bes'2
  | g'2 f'2 f'2
  | f'1 f'2 
  | f'2 f'2 f'2
  | f'2. f'4 g'2

  % m. 40
  | g'2 g'2 fis'2
  | g'1.
  | g'2 g'2 f'2
  | e'2. d'4 f'2
  | f'2 f'2 e'2
  | f'2. g'4 a'2
  | a'2 g'2 bes'2
  | a'2. g'4 f'2
  | bes2 f'2 f'2
  | f'1.

  % m. 50 Escuchad
  | r2 f'2 bes'2
  | a'1.
  | r2 f'2 bes'2
  | a'1.~
  | a'1.
  \MiddleBar

  % m. 55 Duo
  | R1.*28
  \FinalBar

  % Coplas tacet
}
% }}}2

% {{{2 T
MusicT = {
  \clef "treble_8"
  \CantusMollis
  \MeterTriple
  | R1.*32

  % m. 33 a 4
  | r2 r2 bes2
  | bes2 a2 g2
  | f2. es4 d2 % CN MS: E\na
  | es2 bes,2 bes,2
  | f1 f2
  | f2 f2 f2
  | bes2. a4 g2

  % m. 40
  | g2 d2 d2 
  | g1.
  | g2 g2 g2 
  | c'2. c'4 f2
  | f2 c2 c2
  | f2. es4 d2
  | d2 es2 es2
  | f2. f4 bes,2
  | bes,2 f2 f2
  | bes,1.

  % m. 50
  | r2 bes2 bes2
  | f'1.
  | r2 bes2 bes2
  | f'1.~
  | f'1.
  \MiddleBar

  % m. 55 Duo
  | R1.*28
  \FinalBar

  % Coplas tacet
}
% }}}2

% {{{2 Ac
% {{{3 coplas repeated music
MusicCoplasAc = {
  % m. 83 
  \SectionBreak
  \MeterDuple
  | r2 r4 bes,4
  | bes4 a4 bes8.[ a16 g8. f16]
  | es4 f4 bes,4 c4 % CN MS: E\na
  | f,2 f2
%  | es4 d4 << { c4 } \figures { <_->4 } >> c'4
  | es4 d4 c4 c'4
  | a4 bes4 es4 f4
  | bes,8 d8 d8 e8 f2

  % m. 90
%  | es4 d4 << { c2 } \figures { <_->2 } >>
  | es4 d4 c2 
  | a,4 bes,4 es4 f4
  | bes,1
  \MiddleBar

  % m. 93 Tag
  \MeterTriple
  | bes,1.
  | bes1.
  | f1.
  | f,1.
  | f2 f2 f2
  | f1 f,2
  | f2 f2 f2

  % m. 100
  | f1 f,2
  | f2 f2 f2
  | f1 f,2
  | bes,1\color f,2~
  | f,2 c1\endcolor
  | f,1.
  | bes2 bes2 bes2
  | bes1 bes,2
  | bes2 bes2 bes2
  | bes1 bes,2

  % m. 110
  | bes2 bes2 bes2
  | bes1 bes,2
  | es1\color bes,2~
  | bes,2 f1\endcolor
  | bes,1.~
  | bes,1.\fermata
}
% }}}3

MusicAc = {
  \clef "bass"
  \CantusMollis
  \MeterTriple

  % {{{3 estribillo
  | r2 r2 bes2
  | bes2 a2 g2
  | f2. es4 d2
  | es2 bes,2 bes,2
  | f1 f2
  | f2 f2 f2 
  | bes2. a4 g2
  | g2 d2 d2
  | g1.

  % m. 10
  | g2 a2 bes2
  | c'2. c'4 f2
  | f2 c2 c2 
  | f2. es4 d2
  | d2\color es1\endcolor
  | f1\color bes,2~
  | bes,2 f,1\endcolor
  | bes,1 bes2
  | bes2 a2 g2 
  | f2. es4 d2

  % m. 20
  | es2\color bes,1\endcolor
  | f1 f2
  | f2 f2 f2
  | bes2. a4 g2
  | g2\color c1\endcolor
  | g1.
  | g2 a2 bes2
  | c'1\color f2~
  | f2 c1\endcolor
  | f2. es4 d2

  % m. 30
  | d2\color es1\endcolor
  | f1\color bes,2~
  | bes,2 f,1\endcolor

  % m. 33 a 4
  | bes,1 bes2
  | bes2 a2 g2
  | f2. es4 d2
  | es2\color bes,1\endcolor
  | f1 f2
  | f2 f2 f2 
  | bes2. a4 g2

  % m. 40
  | g2\color d1\endcolor
  | g1.
  | g2 a2 bes2
  | c'1\color f2~
  | f2 c1\endcolor
  | f2. es4 d2
  | d2\color es1\endcolor
  | f1\color bes,2~
  | bes,2 f,1\endcolor
  | bes,1.

  % m. 50
  | r2 bes,2 bes,2
  | f1.
  | r2 bes,2 bes,2
  | f1.~
  | f1.
  \MiddleBar

  % m. 55 Duo
  | bes1.
  | bes,1.
  | f1.
  | f,1.
  | f2 f2 f2 

  % m. 60
  | f1 f,2
  | f2 f2 f2 
  | f1 f,2
  | f2 f2 f2
  | f1 f,2
  | bes,1\color f,2~
  | f,2 c1\endcolor
  | f,1.
  | bes2 bes2 bes2
  | bes1 bes,2

  % m. 70
  | bes2 bes2 bes2
  | bes1 bes,2
  | bes2 bes2 bes2
  | bes1 bes,2
  | es1\color bes,2~
  | bes,2 f1\endcolor
  | bes,1.
  | bes1.
  | bes,1.
  | f1.

  % m. 80
  | f,1.~
  | f,1.~
  | f,1.\fermata
  \FinalBar
  % }}}3

  % m. 83 coplas odd
  \MusicCoplasAc

  % m. 116 coplas even 
  \MusicCoplasAc
  \RepeatBarEnd
}
% }}}2
% }}}1

% {{{1 lyrics
% {{{2 estribillo a 4
LyricsQuartet = \lyricmode { 
  Ve -- nid, que -- ru -- bi -- nes a -- la -- dos, ve -- nid,
  co -- rred, mo -- ra -- do -- res del cie -- lo, co -- rred,
  que~hoy a la tie -- rra sus lu -- zes re -- par -- te
  un sol que sin som -- bras sa -- be~a -- ma -- ne -- cer.
  Es -- cu -- chad, a -- ten -- ded. __
}
% }}}2

% {{{2 coplas
LyricsCoplasI = \lyricmode {
  \StanzaI
  Tan lle -- no de luz a -- so -- ma el sol 
  de gra -- cia fe -- liz,
  que des -- de~el na -- dir ob -- sten -- ta 
  cla -- ri -- da -- des del ce -- nit,
  que des -- de~el na -- dir ob -- sten -- ta 
  cla -- ri -- da -- des del ce -- nit.

  Es -- cu -- chad, a -- ten -- ded,
  que~el __ cla -- rín de __ su~au -- ro -- ra, __
  cla -- rín de __ su~au -- ro -- ra
  mi voz ha __ de __ ser,
  que~el __ cla -- rín de __ su~au -- ro -- ra, __
  cla -- rín de __ su~au -- ro -- ra
  mi voz __ ha de ser. __
}

LyricsCoplasII = \lyricmode {
  \StanzaII
  Sus fe -- cun -- dos ra -- yos ha -- cen 
  flo -- re -- cer y pro -- du -- cir
  co -- mo~en su~au -- ro -- ra pub -- li -- ca
  la más es -- té -- ril ra -- íz,
  co -- mo~en su~au -- ro -- ra pub -- li -- ca
  la más es -- té -- ril ra -- íz.

  Es -- cu -- chad, a -- ten -- ded,
  que~el __ cla -- rín de __ su~au -- ro -- ra, __
  cla -- rín de __ su~au -- ro -- ra
  mi voz __ ha de ser,
  que~el __ cla -- rín de __ su~au -- ro -- ra, __
  cla -- rín de __ su~au -- ro -- ra
  mi voz ha __ de ser. __
}

% Omit stanzas 3-6
LyricsCoplasOdd = \LyricsCoplasI

LyricsCoplasEven = \LyricsCoplasII
% }}}3
% }}}2

% {{{2 SI
LyricsSI = \lyricmode {
  Ve -- nid, que -- ru -- bi -- nes a -- la -- dos, ve -- nid, 
  co -- rred, mo -- ra -- do -- res del cie -- lo, co -- rred,
  que~hoy a la tie -- rra sus lu -- zes re -- par -- te
  un sol que sin som -- bras sa -- be~a -- ma -- ne -- cer.

  \LyricsQuartet

  Es -- cu -- chad, a -- ten -- ded,
  que~el __ cla -- rín de __ su~au -- ro -- ra, __
  cla -- rín de __ su~au -- ro -- ra
  mi voz ha __ de __ ser.
  que~el __ cla -- rín de __ su~au -- ro -- ra, __ 
  cla -- rín de __ su~au -- ro -- ra
  mi voz __ ha de ser.
  
  Es -- cu -- chad, a -- ten -- ded. __

  \LyricsCoplasOdd
}
% }}}2

% {{{2 SII
LyricsSII = \lyricmode {
  Ve -- nid a la mís -- ti -- ca fies -- ta, ve -- nid,
  co -- rred, cor -- te -- sa -- nos del cie -- lo, co -- rred,
  que~hoy de Ma -- rí -- a~el al -- bor re -- ve -- ren -- cia
  la cán -- di -- da~au -- ro -- ra en su~a -- ma -- ne -- cer.

  \LyricsQuartet

  Es -- cu -- chad, a -- ten -- ded,
  que~el __ cla -- rín de __ su~au -- ro -- ra, __
  cla -- rín de __ su~au -- ro -- ra
  mi voz __ ha de ser.
  que~el __ cla -- rín de __ su~au -- ro -- ra, __ 
  cla -- rín de __ su~au -- ro -- ra
  mi voz ha __ de ser.
  
  Es -- cu -- chad, a -- ten -- ded, \EdLyrics { a -- ten -- ded. __ }

  \LyricsCoplasEven
}
% }}}2

% {{{2 SIII, T
LyricsSIII = \LyricsQuartet

LyricsT = \LyricsQuartet
% }}}2
% }}}1

% {{{1 figures
FiguresCoplasAc = \figuremode {
  \SectionBreak
  \MeterDuple
  | R1*4
  | s2 <_->4 s4
  | R1*2
  | s2 <_->2
  | R1*2
  \MeterTriple
  | R1.*23
}
FiguresAc = \figuremode {
  \MeterTriple
  | R1.*82
  \FiguresCoplasAc
  \FiguresCoplasAc
}


% {{{1 score
\score {
  <<
    \new ChoirStaff = "voices"
    <<
      \new Staff = "s-SI"
      <<
        \IncipitStaff "[TIPLE 1]" "Ti. 1" { \IncipitSI }
        \new Voice = "SI" { \MusicSI }
        \new Lyrics = "LineISI" \lyricsto "SI" { \LyricsSI }
      >>
      \new Staff = "s-SII"
      <<
        \IncipitStaff "[TIPLE 2]" "Ti. 2" { \IncipitSII }
        \new Voice = "SII" { \MusicSII }
        \new Lyrics = "LineISII" \lyricsto "SII" { \LyricsSII }
      >>
      \new Staff = "s-SIII"
      <<
        \IncipitStaff "[TIPLE 3]" "Ti. 3" { \IncipitSIII }
        \new Voice = "SIII" { \MusicSIII }
        \new Lyrics \lyricsto "SIII" { \LyricsSIII }
      >>
      \new Staff = "s-T"
      <<
        \IncipitStaff "[TENOR]" "T." { \IncipitT }
        \new Voice = "T" { \MusicT }
        \new Lyrics \lyricsto "T" { \LyricsT }
      >>
    >>
    \new ChoirStaff = "continuo"
    <<
      \ShowChoirStaffBracket
      \new Staff = "s-Ac"
      <<
        \IncipitStaff 
        \TwoLineName "ACOMP." "ARPA"
        "Ac."
        { \IncipitAc }
        \new Voice = "Ac" { \MusicAc }
        \new FiguredBass { \FiguresAc }
      >>
    >>
  >>
}
% }}}1
