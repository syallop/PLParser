# PLParser - experimental

A Parser Combinator library evolving with the [Programming Language](https://github.com/syallop/PL) project.

What?
- Combinator interface
- Leftover input (including after successful parses)
- Resuming/ incremental input
- Backtracking on failure with explicit 'try's
- Position tracking across/ within lines
- Questionable reporting about possible causes of failure

Why?
- Because the requirements of PL are in flux and adapting this is easier than switching.
- To allow experimenting with combinations of features that don't exist in established libraries.
- To allow closer integration with PL without having to write general solutions.
- Fun!

Anticipated future changes:
- Compile Parsers from [Grammars](https://github.com/syallop/PLGrammar)
- Stream multiple values as output.
- Improve Space & time complexity of the 'Cursor' abstraction (that holds the input and
  position).
- Replace the 'Expectation' mechanism used to suggest causes of failure as it does not retain
  enough information to provide more than guesses.

