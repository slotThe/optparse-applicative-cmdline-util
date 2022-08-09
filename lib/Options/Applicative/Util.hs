{- |
   Module      : Options.Applicative.Util
   Description : Utility functions for working with optparse-applicative
   Copyright   : (c) slotThe, 2020
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable

This module contains utility functions for working with the
'optparse-applicative' library.

Much of the module revolves around easily building options that can take
"multiple arguments" in the form of separated inputs (e.g. program
--option one,two,three,four).  This still honours the POSIX standard for
options only taking a single argument (by not using spaces to separate
the different inputs), while also being very convenient to enter (as
opposed to, say, wrapping everything inside quotes).

Another focus involves connecting the 'attoparsec' library with
'optparse-applicative' (this is often useful when options involve more
complex parsing patterns).
-}
module Options.Applicative.Util
    ( -- * Types
      AttoParser    -- type alias: Data.Attoparsec.Parser

      -- * Interfacing with parsing libraries
    , attoReadM     -- :: AttoParser a -> ReadM a
    , optionA       -- :: AttoParser a -> Mod OptionFields a -> Parser a

      -- * Parsing a list of things
    , splitWith     -- :: AttoParser p -> String -> ReadM [p]
    , splitOn       -- :: String -> ReadM [Text]

      -- * Parsing one thing out of a list of things
    , anyOf         -- :: [(a, [Text])] -> AttoParser a
    , anyOfSkip     -- :: (Char -> Bool) -> [(a, [Text])] -> AttoParser a
    , anyOfRM       -- :: [(a, [Text])] -> ReadM a

      -- * Easier parsing for a thing
    , aliases       -- :: Foldable t => t Text -> AttoParser Text

      -- * Misc
    , showSepChars  -- :: Foldable t => t Char -> [Char]
    ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T

import Data.Text (Text)
import Options.Applicative (Mod, OptionFields, Parser, ReadM, eitherReader, option)


-- | Less confusion as to which 'Parser' one is referring to.
type AttoParser = A.Parser

-- | Attoparsec <--> optparse-applicative interface.
attoReadM :: AttoParser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . T.pack)

-- | Like 'option', but takes an 'AttoParser' instead of a 'ReadM'.
optionA :: AttoParser a -> Mod OptionFields a -> Parser a
optionA = option . attoReadM
-- | Parse a collection of things, separated by some specific characters.
splitWith
    :: AttoParser p  -- ^ Parser for a single entry
    -> String        -- ^ Characters that may be used to separate different entries
    -> ReadM [p]
splitWith p sepChars = attoReadM $ A.choice
    [ [] <$ A.endOfInput
    , p `A.sepBy` (A.skipSpace *> foldMap A.char sepChars <* A.skipSpace)
    ]

-- | Like 'splitWith', but the parser is just taking everything it can until the
-- next separation character.
splitOn :: String -> ReadM [Text]
splitOn sepChars = A.takeWhile (`notElem` sepChars) `splitWith` sepChars

-- | Create a parser that matches case-insensitively for all elements of a given
-- list.
aliases :: Foldable t => t Text -> AttoParser Text
aliases = foldMap A.asciiCI
{-# INLINE aliases #-}

-- | Create a parser that matches any of the given 'a', with the given aliases.
anyOf :: [(a, [Text])] -> AttoParser a
anyOf = A.choice . map (\(a, ts) -> a <$ aliases ts)

-- | Like 'anyOf' but, after having found a match, skip all remaining
-- text as long as the given predicate is true.
anyOfSkip :: (Char -> Bool) -> [(a, [Text])] -> AttoParser a
anyOfSkip p xs = anyOf xs <* A.skipWhile p

-- | Like 'anyOf', but return a 'ReadM a' instead of an 'AttoParser a'.
anyOfRM :: [(a, [Text])] -> ReadM a
anyOfRM = attoReadM . anyOf

-- | Pretty print some container of separation characters, inserting a space
-- between each item.
showSepChars :: Foldable t => t Char -> String
showSepChars = concatMap ((' ' :) . (:[]))
{-# INLINE showSepChars #-}
