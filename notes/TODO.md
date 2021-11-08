- (DONE) find `key = value` expressions in list of strings and store keys and
  values in dictionary
    - looking for *last* delimiter not the best way to do this
- substitute values
    - after storing definitions, remove definition expressions from list
    - find \key expressions in list and substitute values (on second pass;
      ignore them on first pass)
        - Q: is a two-pass system enough (or necessary?) 

- find whole, multiline value expressions: match \value or { value text ... }
  or \lyricmode { value ... }
    - match braces 
