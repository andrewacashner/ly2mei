constructor TLyObject.Create(Source: String);
var
  SearchIndex: Integer = 0;
  SearchStr, ThisType, ThisID, ThisLinkID, ThisContents: String;
  Outline: TIndexPair;
begin
  Outline := TIndexPair.Create;
  SearchStr := Source;
  ThisType := '';
  ThisID := '';
  ThisLinkID := '';

  if SearchStr.Contains('\new ') then
  begin
    SearchIndex := SearchStr.IndexOf('\new ');
    
    { Find Type }
    ThisType := CopyStringBetween(SearchStr, '\new ', Space);
    SearchStr := StringDropBefore(SearchStr, ThisType);

    { Find ID }
    if SearchStr.StartsWith(' = "') then
    begin 
      ThisID := CopyStringBetween(SearchStr, ' = "', '"');
      SearchStr := StringDropBefore(SearchStr, ThisID + '"');
    end;

    { Find Link ID for Lyrics }
    if (ThisType = 'Lyrics') and SearchStr.Contains('\lyricsto') then
    begin
      ThisLinkID := CopyStringBetween(SearchStr, '\lyricsto "', '"');
    end;

    { Find Contents: Either an expression within double angle brackets or
      one within curly braces. If angle brackets, recursively look for nested
      @code(\new) expressions. }
    if SearchStr.TrimLeft.StartsWith('<<') then
    begin
      { Search within group for nested @code(\new) expressions and save them
        as children; then move on after this group. Omit content string. }
      Outline.MarkBalancedDelimiterSubstring(SearchStr, '<', '>');
      ThisContents := CopyStringRange(SearchStr, Outline, rkInclusive);

      Create(ThisType, ThisID, ThisLinkID);
      if ThisContents.Contains('\new ') then
      begin
        FChild := TLyObject.Create(ThisContents);
      end;
    end
    else
    begin
      { Save brace expression in this node }
      ThisContents := CopyBraceExpr(SearchStr);
      Create(ThisType, ThisID, ThisLinkID, ThisContents);
    end;

    { Look for the next sibling where you left off from the last search }
    Source := StringDropBefore(Source.Substring(SearchIndex), ThisContents);
    if Source.Contains('\new ') then
    begin
      FSibling := TLyObject.Create(Source);
    end;
  end;
  FreeAndNil(Outline);
end;

