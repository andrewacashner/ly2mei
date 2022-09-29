# `ly2mei`

Andrew A. Cashner, <andrewacashner@gmail.com>

# Description

Converts files written in a strict dialect of Lilypond to MEI-XML.
This program is primarily a proof of concept.

The program is basically a compiler that reads a subset of the Lilypond
language and produces MEI.
(It does not use the Lilypond program at all.)
The program can only work with a predictable, rule-bound portion of the
Lilypond language and includes no support at all for Scheme.

Its main feature is that it can read files written in the one-voice-at-a-time,
measure-wise format of Lilypond and transform the data to the
one-measure-at-a-time format of MEI.

It also implements a basic macro expander that actually allows more
flexibility than Lilypond itself. 
With the definition `macro = "string"` or `macro = { expression in braces }`
starting on a new line, you can use `\macro` anywhere, even before the
definition, without worrying about the mode or type of expression.

## Aspirations and Conclusions

My goal was to evaluate the feasibility of this approach while experimenting
with programming techniques that were relatively new to me: object-oriented
programming in a lower-level language than I had been using (having just come
from a large project in Haskell).
I wanted to write clear, well-documented code and after working in a
pure-functional language I wanted to use something close to a functional style
while getting practice managing memory.

I hoped to be able to convert my own Lilypond projects (editions of
17th-century Spanish villancicos) to MEI.
I had written them in a LaTeX-like interface for Lilypond I had developed (my
`lirio` "package"), which aimed to eliminate mixing semantic or
content-oriented code with low-level overrides and other Lilypond ugliness.
Nevertheless, some complexity was just unavoidable when I had to make both
parts and scores (using `\keepWithTags` for example), add figured bass, and
many other features.
In the end (so far anyway) this goal remains out of reach but I learned a lot
about programming and music encoding.

If you find some part of this project useful or would like to pick it up where
I've left off, please reach out.

## Limitations

The program is not really intended for general use although it can do a lot.

- No abbreviations or shortcuts. There must be an explicit octave mark and
  rhythmic value after every single pitch.
- Every bar must be marked with `|`. 
- No Scheme functions.
- Lilypond commands not hard-coded into this program will be ignored. 
- Including files works if they just contain simple `key = value` macro
  definitions but not if they contain elaborate Lilypond configuration, Scheme
  code, etc.
- No `<<...>>` expressions within a music expression.
- No figured bass or chord mode.

## Bonus features

The macro expander module in this program actually allows you to define simple
macros that Lilypond doesn't allow.
Lilypond macros are typed (music mode, lyric mode, etc.), where these  macros
are simple text substitutions.
There is no problem with writing `Text = { a -- ny -- thing }` and then later
writing `\lyricmode \Text`, where with Lilypond you would have to use
`\lyricmode` to create the macro to begin with.

The program supports many of the commands I created for my own `lirio`
Lilypond "package", which I used for typesetting editions of 17th-century
Spanish choral music.
So in addition to `\time 3/2` you can write `\MeterTriple` and instead of `key
f\major` you can write `\CantusMollis`.
Coloration brackets work with `\color` and `\endcolor`, e.g, `a'2\color
b'2\endcolor`.
Section headings, instrument names, and incipit staves are not (yet)
supported, unfortunately. (Not for lack of trying...)

# Implementation

Written in Object Pascal for the Free Pascal Compiler. Documentation generated
by PasDoc. 

# Requirements

- [GNU Make](https://www.gnu.org/software/make/)
- [`fpc`](https://www.freepascal.org/)
- [PasDoc](https://pasdoc.github.io/index)
- For PDF documentation: [TeXLive](https://tug.org/texlive/) and my `code2pdf`
  script (see [below](#code2pdf))

# Build 

- Executable: `make`
- HTML documentation of unit interfaces: `make doc` and `make view` to read the docs in the
  browser
- PDF documentation, including source code: `make pdfdoc` and `make view-pdf`
- Clean up: `make clean`

## Testing and Debugging

- Build in test mode (show all compiler messages and report on memory usage):
  `make TEST=1`
- Build in debug mode (show all verbose debug messages): `make DEBUG=1`

# Read documentation

- View HTML documentation: `make view`
- View PDF documentation: `make view-pdf`

# License

Copyright © 2021 Andrew A. Cashner. All rights reserved.


