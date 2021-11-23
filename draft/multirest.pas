    if ThisNote.StartsWith('R') then
    begin
      NoteStr := ExtractWord(1, ThisNote, [' ']);
      DebugLn('MULTIREST - found in string: ''' + NoteStr + '''');

      MultiRestStr := StringDropBefore(NoteStr, '*');
      DebugLn('MULTIREST duration string: ''' + MultiRestStr + '''');
      
      MultiRestCount := MultiRestStr.ToInteger;
      DebugLn('MULTIREST count: ' + IntToStr(MultiRestCount));
      
      NoteStr := StringDropAfter(NoteStr, '*');
      NoteStr := NoteStr.Replace('R', 'r');
      DebugLn('MULTIREST will repeat this rest string: ' + NoteStr);
     
      for I := MultiRestCount - 1 downto 0 do
        AddNewPitch(NewPitch, NoteStr, Key);

