# `ly2mei`

Andrew A. Cashner, <andrewacashner@gmail.com>

# Description

Converts files written in a strict dialect of Lilypond to MEI-XML.

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

# code2pdf

````
#! /bin/sh
set -e
INFILE="$1"
pdflatex -jobname="$INFILE" \"\\def\\infile\{"$INFILE"\}\\input\{prettyprintframe\}\"
rm "$INFILE".log "$INFILE".aux
````

# License

Copyright © 2021 Andrew A. Cashner. All rights reserved.


