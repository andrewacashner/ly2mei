- (DONE) deal section headings
- (DONE) deal with barlines
- deal with (DONE) ties
- deal with lines:  slurs, lines (coloration) (IN PROGRESS)
    - ? Verovio gives warning message about ties
        - need to make separate tie element for Verovio?
    - Avoid creating duplicates
        - = problem using same TPitchList.FLineList for all voices/layers
          after conversion to MEI structure
    - Need to encode lines in the measure of their startID note
- (DONE) deal with multiple articulations on one note
- deal with Lyrics
- deal with FiguredBass

# Optimize, refactor

- Consistency with private variables vs. public getters/setters (properties?)
- Consistency with class functions vs independent
- Consistency with constructors vs other functions that create items
    - especially class methods of one class that create another class
