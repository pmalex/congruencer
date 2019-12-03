{-# LANGUAGE NoImplicitPrelude #-}

module Partitions
    ( Partition,
      partitionSet, toCongruences,
      getElements
    ) where

import PartialOrd
import Lattice
import Func

import Data.List
import Text.Show
import Data.Int (Int)
import Text.Read
import qualified Data.Eq as Eq
import qualified Data.Ord as Ord
import Data.Bool (Bool(..), otherwise, (&&))
import Data.String (String)
import Data.Foldable (foldr, foldl, any, all, elem)
import Control.Monad ((>>=))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)

data Partition a = Partition [[a]]

instance Eq.Eq a => PartialOrd (Partition a) where
        (Partition p) <= (Partition s) = all (\t -> t `isSubset1` s) p
            where
                isSubset1 :: Eq.Eq a => [a] -> [[a]] -> Bool
                isSubset1 x ys = any (isSubset0 x) ys

                isSubset0 :: Eq.Eq a => [a] -> [a] -> Bool
                isSubset0 xs ys = all (\t -> t `Data.Foldable.elem` ys) xs

instance Show a => Show (Partition a) where
    show (Partition []) = ""
    show (Partition [[]]) = ""
    show (Partition p) | length zs Ord.> 0 = foldl (\x y -> x ++ showSubPartition y) "" zs
                       | otherwise = "Delta"
                    where
                        zs = filter (\x -> length x Ord.> 1) p

                        showSubPartition :: Show a => [a] -> String
                        showSubPartition ts = "(" ++ filter (Eq./='\"')
                                        (foldl (\x y -> x ++ show y) "" ts) ++ ")"


instance Eq.Eq a => CompleteLattice (Partition a)


{- Generate all possible partitions of a set -}
partitionSet :: [a] -> [Partition a]
partitionSet ms = map (Partition) (foldr (\x r -> r >>= expandBy x) [[]] ms)
    where
        expandBy :: a -> [[a]] -> [[[a]]]
        expandBy k part = zipWith (++) (inits part) (map (insertFront k) (tails part))

        insertFront :: a -> [[a]] -> [[a]]
        insertFront k (h:t) = [k:h] ++ t
        insertFront k _ = [[k]]


toCongruences :: (Eq.Eq x, Eq.Eq s) => [Partition x] -> F2 x s x -> [Partition x]
toCongruences ps fs = filter (\r -> isCongruence r fs) ps


isCongruence :: (Eq.Eq x, Eq.Eq s) => Partition x -> F2 x s x -> Bool
isCongruence p fs = f2 p (snd (dom2 fs)) fs <= p
        where
            {- Multiply partition on set of elements -}
            f2 :: (Eq.Eq x, Eq.Eq s) => Partition x -> [s] -> F2 x s x -> Partition x
            f2 (Partition ps) ss fs = Partition (concat (map (\r -> f0 ps r fs) ss))

            {- Multiply partition on element -}
            f0 :: (Eq.Eq x, Eq.Eq s) => [[x]] -> s -> F2 x s x -> [[x]]
            f0 ps t fs = map (\r -> f1 r t fs) ps

            {- Multiply set on an element -}
            f1 :: (Eq.Eq a, Eq.Eq b, Eq.Eq c) => [a] -> b -> F2 a b c -> [c]
            f1 xs y fs = fs #* (map (\r -> (r, y)) xs)


-- makeConLattice :: (Eq.Eq x, Eq.Eq s) => F2 x s x -> Lattice (Partition x)
-- makeConLattice ft = toLattice (toCongruences (partitionSet (getElements ft)) ft)

getElements :: (Eq.Eq x, Eq.Eq s) => F2 x s x -> [x]
getElements fs = (fst (dom2 fs)) `union` (im fs)
