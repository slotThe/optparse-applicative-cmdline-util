# optparse-applicative-cmdline-util

Utility functions for working with `optparse-applicative`.

Much of the module revolves around easily building options that can take
"multiple arguments" in the form of separated inputs (e.g. `program
--option one,two,three,four`).  This still honours the POSIX standard
for options only taking a single argument (by not using spaces to
separate the different inputs), while also being very convenient to
enter (as opposed to, say, wrapping everything inside quotes).

Another focus involves connecting the `attoparsec` library with
`optparse-applicative` (this is often useful when options involve more
complex parsing patterns).

# Example

Consider the following pseudo-EBNF:

``` ebnf
  start        ∷= prefix , ":" , options ;
  prefix       ∷= "a" | "b" | "c" ;
  options      ∷= { option-chars , "," } | option-chars ;
  option-chars ∷= { ?all-characters? - "," } ;
```

I.e., we would like an option that parses strings like
`a:this,that,these`.  The following Haskell code builds an option
`--ignore` that does exactly that:

``` haskell
  import Control.Applicative ((<|>))
  import Data.Text (Text)
  import Options.Applicative (Parser)
  import Options.Applicative.CmdLine.Util (optionA, splitOn, AttoParser)

  data Example = A [Text] | B [Text] | C [Text]

  pIgnore :: Parser [Example]
  pIgnore = many $ optionA (parse "a:" A <|> parse "b:" B <|> parse "c:" C)
     ( long "ignore"
    <> short 'i'
    <> help "Explanation of the feature."
    <> value []  -- default
     )
   where
    parse :: AttoParser Text -> ([Text] -> Example) -> AttoParser Example
    parse s d = s *> (d <$> splitOn sep)

    sep :: [Char]
    sep = ","
```

Note that, due to invocation of `many`, it would be possible to call
`--ignore` multiple times—e.g., `my-cli-program --ignore a:a,b,c
--ignore c:d,e`.
