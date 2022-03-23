# Lilypond header (with "villancico.ly")

```` lilypond
\header {
  title     = "Venid, querubines alados"
  subtitle  = "De nuestra Señora a 4"
  composer  = "JUAN HIDALGO"
  dates     = "(1614–1685)"
  poet      = "Anonymous"
  source    = \markup { \italic "D-Mbs:" "Mus. mss. 2897" }
%   (these lines added in the "style file")
%  editor     = "Andrew A. Cashner"
%  copyright  = Copyright © 2021 Andrew A. Cashner
}
````

# MEI header

```` xml
<meiHead xmlns="http://www.music-encoding.org/ns/mei">
  <fileDesc>
    <titleStmt>
      <title type="main">Venid, querubines alados</title>
      <title type="subtitle">De nuestra Señora. A 4.</title>
      <respStmt>
        <composer><name>Juan Hidalgo</name> (1614–1685)</composer>
        <lyricist>Anonymous</lyricist>
        <editor><name xml:id="AAC">Andrew A. Cashner</name></editor>
      </respStmt>
    </titleStmt>

    <editionStmt>
      <respStmt><p>Edited by Andrew A. Cashner</p></respStmt>
    </editionStmt>

    <pubStmt>
      <unpub/>
      <availability><p>Copyright © 2018 Andrew A. Cashner</p></availability>
      <date isodate="2018"/>
    </pubStmt>

    <sourceDesc>
      <source label="ms">
        <identifier>
          <repository>D-Mbs</repository>
          <idno>Mus. mss. 2897</idno>
        </identifier>
      </source>
    </sourceDesc>

  </fileDesc>

  <encodingDesc>
    <appInfo>
      <application type="render" label="lilypond" version="2.19"/>
      <application type="transform" subtype="xslt" label="lirio"/>
    </appInfo>
  </encodingDesc>

  <revisionDesc>
    <change isodate="2018-10-01" resp="#AAC">
      <changeDesc>
        <p>First attempt in custom MEI</p>
      </changeDesc>
    </change>
    <change isodate="2018-09-24" resp="#AAC">
      <changeDesc>
        <p>Transcription by hand</p>
      </changeDesc>
    </change>
  </revisionDesc>
</meiHead>

````

# Translation

````
\header { ... } => meiHead
title           => meiHead/fileDesc/titleStmt/title[@type=main]
subtitle        => meiHead/fileDesc/titleStmt/title[@type=subtitle]
composer, dates => meiHead/fileDesc/titleStmt/composer/${composer + dates}
poet            => meiHead/fileDesc/titleStmt/lyricist
editor          => meiHead/fileDesc/titleStmt/editor/
editor          => meiHead/editionStmt/respStmt/p/"Edited by ${editor}"
copyright       => meiHead/pubStmt/availability/p/
source          => meiHead/sourceDesc/source/
````

## Optional:

````
today, version, software-id, software-name 
    => meiHead/encodingDesc/appInfo/
            application[@isodate=${today},@version=${version},
                xml:id={$software-id}]/name/${software-name}
````

## Namespace 

`meiHead` element has namespace:

````
<meiHead xmlns="http://www.music-encoding.org/ns/mei">
````

# Steps

- Read input file
- Find `\header{}`
- Find subfields
    - Store in dictionary or list (because we know the keys already
- Open output file (or use stdout)
- Write XML plugging in values from list 

## Parsing value strings and markup expressions

1. Basic header field is `key = "value"`
2. But they can also be `key = \markup { "value" }` 
3. or really anything could go inside the markup expression:
    - `key = \markup { \column { \line { "First" } \line { \italic "Second" } } }`
4. Notation without braces is possible in ly:
    - `key = \markup \italic "Emphasised"`

- We will accept (1) as is (but remove quotes).
- For (2) or (3), we only keep the quoted strings inside a markup expression,
  at any level of nesting, separated by spaces. 
- For (4), the argument to `\markup` must be enclosed in curly braces,
  otherwise we reject the whole thing

- (1) => 'value'
- (2) => 'value'
- (3) => 'First Second'
- (4) => (no data stored)
