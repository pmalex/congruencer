{-# LANGUAGE NoImplicitPrelude #-}

module Func
    ( F0, F, F1, F2(..),
      im, dom, dom1, dom2,
      (#), (##), (#*),
      readF2fromTable
    ) where

import Data.Eq (Eq, (==))
import Data.List
import Data.Int (Int)
import Data.Tuple (fst, snd)
import Text.Show (Show)
import Text.Read
import Data.Maybe (Maybe(..))
import GHC.Base


data F0 a = F0 [a] deriving (Show)
type F a b = [(a, b)]
type F1 a b = F a b
type F2 a b c = F (a, b) c
type F3 a b c d = F (a, b, c) d


im :: Eq b => F a b -> [b]
im fs = nub (map snd fs)

dom :: Eq a => F a b -> [a]
dom fs = nub (map fst fs)

dom1 :: Eq a => F1 a b -> [a]
dom1 fs = dom fs

dom2 :: (Eq a, Eq b) => F2 a b c -> ([a], [b])
dom2 fs = let as = nub (map (fst . fst) fs)
              bs = nub (map (snd . fst) fs)
          in (as, bs)


{- Apply function to one argument -}
(#) :: Eq a => F a b -> a -> Maybe b
[] # _ = Nothing
((t,q):fs) # arg = if arg == t then Just q else fs # arg

{- Apply function to a set of arguments -}
(##) :: Eq a => F a b -> [a] -> [b]
[] ## _ = []
_ ## [] = []
fs ## (x:xs) = (f0 (fs # x)) ++ (fs ## xs)
            where f0 (Just t) = [t]
                  f0 Nothing = []

{- Apply function to a set of arguments and glue equals -}
(#*) :: (Eq a, Eq b) => F a b -> [a] -> [b]
(#*) fs xs = nub (fs ## xs)



-- readF2fromTable_ :: (Read a, Read b, Read c) => String -> F2 a b c
-- readF2fromTable_ str = let rs = map read ((words . head . lines) str)
--                            qs = (tail . lines) str
--                        in concat (map (\r -> readTableLine rs r) qs)
--         where
--             readTableLine :: (Read a, Read b, Read c) => [a] -> String -> F2 a b c
--             readTableLine ts str = let (y, s) = (head . lex) str
--                                     in f0 ts (read y) (words s)
--                     where
--                         f0 :: (Read b, Read c) => [a] -> b -> [String] -> [((a,b),c)]
--                         f0 [] _ _ = []
--                         f0 _ _ [] = []
--                         f0 (x:xs) t (m:ms) = if m /= "_"
--                                             then ((x, t),(read m)): f0 xs t ms
--                                             else f0 xs t ms

{- read function in table format -}
readF2fromTable :: String -> F2 String String String
readF2fromTable str = let rs = (words . head . lines) str
                          qs = (tail . lines) str
                      in concat (map (readTableLine rs) qs)
        where
            readTableLine :: [String] -> String -> F2 String String String
            readTableLine ts st = let (y, s) = (head . lex) st
                                  in f0 ts y (words s)

            f0 :: [String] -> String -> [String] -> F2 String String String
            f0 [] _ _ = []
            f0 _ _ [] = []
            f0 (x:xs) t (m:ms) = if m /= "_"
                                then ((x, t), m): f0 xs t ms
                                else f0 xs t ms
