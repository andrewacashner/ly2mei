- (DONE) deal section headings
- (DONE) deal with barlines
- (DONE?) deal with ties
    - Verovio gives warning message about ties: need to make separate tie element?
- (DONE) deal with lines:  slurs, lines (coloration) 
    - (DONE) Avoid creating duplicates
    - (DONE) Need to encode lines in the measure of their startID note
- (DONE) deal with multiple articulations on one note
- deal with Lyrics
- deal with FiguredBass

# Optimize, refactor

- Consistency with private variables vs. public getters/setters (properties?)
- Consistency with class functions vs independent
- Consistency with constructors vs other functions that create items
    - especially class methods of one class that create another class
- Remove unused code
