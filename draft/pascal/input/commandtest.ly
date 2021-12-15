This is not a macro.
Definition = { a b c}
DefWithCommand = \command
DefWithCommandArg = \command { a b c}

CommandI = { "1" }
CommandII = { "2" }
CommandIII =  { "3" }

\Definition
\new Thing with { \DefWithCommand }

\CommandI
\CommandII
\CommandI
\CommandIII


