- (DONE) deal section headings
- (DONE) deal with barlines
- (DONE) deal with ties
    - (DONE) Verovio gives warning message about ties: need to make separate tie element
- (DONE) deal with lines:  slurs, lines (coloration) 
    - (DONE) Avoid creating duplicates
    - (DONE) Need to encode lines in the measure of their startID note
- (DONE) deal with multiple articulations on one note
- (DONE) deal with Lyrics
    - NB we are ignoring markup or other \commands within lyrics

- section headings mid-voice
- meter, key changes mid-voice

- pitch input!: support "as" not just "aes"
- (DONE) enable input files

## Figured Bass
- deal with FiguredBass
    - (DONE) add basic classes, MEI output
    - add constructors to parse input and create those classes
        - need to store staff number enclosing the figured bass
        - need to calculate time stamp from durations
        - need to parse ly text commands

- Lilypond figured bass looks like this, in its own input layer (in lirio we
  are not allowing ad hoc figured bass within music input):

````
\new FiguredBass { \figuremode { \time 4/4 | <6>1 | <+>2 <7->2 | <5 4>2 } }
````

- MEI figured bass looks like this, within `<measure>`:

````
<harm staff="5" place="below" tstamp="1">
  <fb>
    <f>6</f>
    <f>♯</f>
  </fb>
</harm>
````

- parse figure series
    - start with contents of `\new FiguredBass { }` for a particular staff (need
      staff ID)
    - get contents of `\figuremode { }`
    - read time signature if there is one, or import from rest of program
    - split at spaces
    - read one measure at a time (as we did for pitch input, from `|` to `|` or `}`
    - match pattern: `<[[1-7_].[+ - ! -- ++ \+ / \\ \!].*] ].*>[1 2 3 8 16 \breve].[.].*`
    - for each: parse, convert, store
        -for each number+mod separated by spaces:
            - number: 
                - numeral -> numeral
                - `_` -> no numeral (just accidental following)
            - modifier
                - `+`   -> `♯`
                - `-`   -> `♭`
                - `!`   -> `♮`
                - `--`  -> `` (U+E264)
                - `++`  -> `` (U+E263)
                - `\+`  -> `+`
                - `/`   -> `/`
                - `\\`  -> `\`
                - `\!`  -> look back for previous figure with same number and
                                add @extender="true" to it, don't add anything
                                to this one
        - rhythm value (number + dot) -> tstamp based on time signature
        



# also
- deal with differing numbers of measures in diff. voices
- support \IncipitStaff and \InstrumentName in \score

# Optimize, refactor

(DONE for current code 5/3)

- Consistency with private variables vs. public getters/setters (properties?)
- Consistency with class functions vs independent
- Consistency with constructors vs other functions that create items
    - especially class methods of one class that create another class
    - (TRIED, GAVE UP) change from Class.ToMEI to MeiNode.Create(Class)
        - current system works and is consistent
        - using constructors does not work when the function needs to be able
          to return nil (e.g., List.ToMEI => nil when List.Count = 0)
- try/finally more?
- Remove unused code

- Improve basic parsing functions (e.g., TLyObject\.Create) to reduce number
  of states, conditions, loops; too confusing at present
