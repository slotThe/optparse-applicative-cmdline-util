{- |
   Module      : Options.Applicative.Util
   Description : Utility functions for working with optparse-applicative
   Copyright   : (c) slotThe, 2020
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable
-}
module Options.Applicative.Util
    ( -- * Types
      AttoParser  -- type alias: Data.Attoparsec.Parser

      -- * Interfacing with parsing libraries
    , attoReadM   -- :: AttoParser a -> ReadM a

      -- * Parsing a list of things
    , splitWith   -- :: AttoParser p -> String -> ReadM [p]
    , splitOn     -- :: String -> ReadM [Text]

      -- * Parsing one thing out of a list of things
    , anyOf       -- :: [(a, [Text])] -> AttoParser a
    , anyOfRM     -- :: [(a, [Text])] -> ReadM a

      -- * Easier parsing for a thing
    , aliases     -- :: [Text] -> AttoParser Text
    ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T

import Data.Text (Text)
import Options.Applicative (ReadM, eitherReader)


-- | Less confusion as to which 'Parser' one is referring to.
type AttoParser = A.Parser

-- | Attoparsec <--> optparse-applicative interface.
attoReadM :: AttoParser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . T.pack)

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
aliases :: [Text] -> AttoParser Text
aliases = foldMap A.asciiCI

-- | Create a parser that matches any of the given 'a', with the given aliases.
anyOf :: [(a, [Text])] -> AttoParser a
anyOf = A.choice . map (\(a, ts) -> a <$ aliases ts)

-- | Like 'anyOf', but return a 'ReadM a' instead of an 'AttoParser a'.
anyOfRM :: [(a, [Text])] -> ReadM a
anyOfRM = attoReadM . anyOf
