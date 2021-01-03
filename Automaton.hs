{-|
Module Automaton contains definitions and methods for
working with a finite moore's automaton defined as tabl
-}
module Automaton where

import qualified Data.List  as List (concatMap, head, lines, map, nub, tail,
                                     words, (++))
import           Data.Tuple as Tuple (fst, snd)
import           GHC.Base   (Eq, String, (.), (/=))
import qualified Text.Read  as Read (lex)


-- | Automaton's transition function
type TransitionFunction state word = [((state, word), state)]

-- | Automaton main type
type Automaton state word = TransitionFunction state word

-- | Read automaton's transition function defined in a table format
readAutomatonFromTable :: String -> F2 String String String
readAutomatonFromTable str = let -- list with table's head elements (function's domain)
                                 functionDomainElements = (List.words . List.head . List.lines) str
                                 -- list with remaining table's rows (they encode function's map)
                                 tableRowsStr = (List.tail . List.lines) str
                             in List.concatMap (readTableRow functionDomainElements) tableRowsStr
    where
        -- | Parse one row from the table
        readTableRow :: [String] -> String -> F2 String String String
        readTableRow domainElements tableRowStr = let (image, tailStr) = (List.head . Read.lex) tableRowStr
                                                  in f0 domainElements image (List.words tailStr)
        -- | Bound 
        f0 :: [String] -> String -> [String] -> F2 String String String
        f0 [] _ _ = []
        f0 _ _ [] = []
        f0 (x:xs) image (m:ms) = if m /= "_"
                             then ((x, image), m): f0 xs image ms
                             else f0 xs image ms