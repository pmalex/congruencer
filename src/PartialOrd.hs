{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PartialOrd (
        PartialOrd(..),
        Ordering(..),
        elem, intersect,
        minimums, minimum,
        maximums, maximum,
        elemIndex
        ) where

import qualified Data.List as List (filter)
import Data.Bool (Bool(..), (&&), not, otherwise)
import qualified Data.Foldable as Foldable (any, foldl)
import Text.Show (Show)
import qualified Data.Eq as Eq (Eq, (==))
import Data.Maybe (Maybe(..))
import Data.Int (Int)
import GHC.Num ((+))


-- LT    Less Than
-- GT    Greater Than
-- EQ    Equal
-- NC    Not Comparable
data Ordering = LT | GT | EQ | NC deriving (Show, Eq.Eq)

class PartialOrd a where
    (<=) :: a -> a -> Bool
    x <= y = y >= x

    (>=) :: a -> a -> Bool
    x >= y = y <= x

    (==) :: a -> a -> Bool
    x == y = x <= y && y <= x

    (/=) :: a -> a -> Bool
    x /= y = not (x == y)

    (<) :: a -> a -> Bool
    x < y = (x <= y) && (x /= y)

    (>) :: a -> a -> Bool
    x > y = (x >= y) && (x /= y)

    compare :: a -> a -> Ordering
    compare x y = if | x == y    -> EQ
                     | x < y     -> LT
                     | x > y     -> GT
                     | otherwise -> NC

    {-# MINIMAL (<=) | (>=) #-}

elem :: PartialOrd a => a -> [a] -> Bool
elem x xs = Foldable.any (==x) xs

intersect :: PartialOrd a => [a] -> [a] -> [a]
intersect xs ys = List.filter (\r -> r `elem` ys) xs

elemIndex :: PartialOrd a => a -> [a] -> Maybe Int
elemIndex x xs = elemIndex' x xs 0
            where
                elemIndex' :: PartialOrd a => a -> [a] -> Int -> Maybe Int
                elemIndex' y [] n = Nothing
                elemIndex' y (t:ts) n = if y == t then Just n else elemIndex' y ts (n + 1)

{- List of minimal non-comparable elements -}
minimums :: PartialOrd a => [a] -> [a]
minimums [] = []
minimums xs = Foldable.foldl (\r q -> dive r q) [] xs
        where
            dive :: PartialOrd a => [a] -> a -> [a]
            dive ys x = if Foldable.any (\r -> compare x r Eq.== GT) ys
                            then ys
                            else if Foldable.any (\r -> compare x r Eq.== LT) ys
                                    then x: List.filter (\r -> compare x r Eq.== NC) ys
                                    else x:ys



{- Some minimal element from list -}
minimum :: PartialOrd a => [a] -> Maybe a
minimum [] = Nothing
minimum (x:xs) = Just (Foldable.foldl (\r q -> if r >= q then q else r) x xs)

{- List of maximal non-comparable elements -}
maximums :: PartialOrd a => [a] -> [a]
maximums [] = []
maximums xs = Foldable.foldl (\r q -> dive r q) [] xs
        where
            dive :: PartialOrd a => [a] -> a -> [a]
            dive ys x = if Foldable.any (\r -> compare x r Eq.== LT) ys
                            then ys
                            else if Foldable.any (\r -> compare x r Eq.== GT) ys
                                    then x: List.filter (\r -> compare x r Eq.== NC) ys
                                    else x:ys

{- Some maximal element from list -}
maximum :: PartialOrd a => [a] -> Maybe a
maximum [] = Nothing
maximum (x:xs) = Just (Foldable.foldl (\r q -> if r <= q then q else r) x xs)
