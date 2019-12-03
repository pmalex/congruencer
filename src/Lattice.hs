{-# LANGUAGE NoImplicitPrelude #-}

module Lattice
    (
        Lattice, CompleteLattice,
        toLattice, showDotLattice,
        isModular, isCommutative,
        isDistributive
    ) where

import PartialOrd
import Data.Int (Int)
import Data.String (String)
import qualified Data.List as List (filter, nubBy, (++), concat,
             map, intersect, (!!), minimum, maximum, deleteBy)
import Data.Bool (Bool(..), (&&), (||))
import qualified Data.Eq as Eq
import qualified Data.Foldable as Foldable (all)
import Data.Maybe (Maybe(..))
import Text.Show (Show, show)


data Node a = Node [Int] a [Int] (Lattice a)
type Lattice a = [Node a]

instance PartialOrd a => PartialOrd (Node a) where
    (Node _ x _ _) <= (Node _ y _ _) = x <= y

instance Show a => Show (Node a) where
    show (Node _ x _ _) = show x

infixr 5 \/
infixr 6 /\

class PartialOrd a => CompleteLattice a where
    (\/) :: Node a -> Node a -> Node a
    (\/) (Node _ _ us nl) (Node _ _ us' _) = (\(Node a b c d) -> Node a b c nl) ts
                        where Just ts = minimum (List.map (nl List.!!) (us `List.intersect` us'))

    (/\) :: Node a -> Node a -> Node a
    (/\) (Node ls _ _ nl) (Node ls' _ _ _) = (\(Node a b c d) -> Node a b c nl) ts
                        where Just ts = maximum (List.map (nl List.!!) (ls `List.intersect` ls'))


{- Convert partial ordered set to lattice structure -}
toLattice :: PartialOrd a => [a] -> Lattice a
toLattice xs = List.map (\(Node ls n us d) -> Node ls n us ts) ts
                where ts = List.map (\r -> Node (ls r) r (us r) []) xs
                        where ls n = List.map (toIdx) (List.filter (<=n) xs)
                              us m = List.map (toIdx) (List.filter (>=m) xs)
                              toIdx a = (\(Just r) -> r) (elemIndex a xs)


isModular :: CompleteLattice a => Lattice a -> Bool
isModular nl = Foldable.all (Eq.==True) [modularId n m l | n <- nl, m <- nl, l <- nl]
                where modularId x y z = (x /\ (y \/ z)) == (x /\ ((y /\ (x \/ z)) \/ z))

isDistributive :: CompleteLattice a => Lattice a -> Bool
isDistributive nl = Foldable.all (Eq.==True) [distributiveId n m l | n <- nl, m <- nl, l <- nl]
                where distributiveId x y z = (x /\ (y \/ z)) == ((x /\ y) \/ (x /\ z))

isCommutative :: CompleteLattice a => Lattice a -> Bool
isCommutative nl = Foldable.all (Eq.==True) [commId n m | n <- nl, m <- nl]
                where commId x y = (x /\ y) == (y /\ x)



{- show lattice in dot format -}

data Edge a = Edge a a
type LatticeGraph a = [Edge a]

instance Show a => Show (Edge a) where
    show (Edge a b) = "\t\"" List.++ show a List.++ "\" -- \"" List.++ show b List.++ "\";\n"

toLatticeGraph :: PartialOrd a => Lattice a -> LatticeGraph (Node a)
toLatticeGraph ll = List.nubBy (\x y -> cmp x y)
                        (List.concat (List.map f0 ll List.++ List.map f1 ll))
    where
        cmp :: PartialOrd a => Edge a -> Edge a -> Bool
        cmp (Edge a b) (Edge c d) = (a == c && b == d) || (a == d && b == c)

        f0 :: PartialOrd a => Node a -> [Edge (Node a)]
        f0 nd = List.map (Edge nd) (PartialOrd.maximums (lowers nd))
            where
                lowers :: PartialOrd a => Node a -> [Node a]
                lowers (Node ls nd us llt) = List.deleteBy (PartialOrd.==) (Node ls nd us llt)
                                                (List.map (llt List.!!) ls)

        f1 :: PartialOrd a => Node a -> [Edge (Node a)]
        f1 nd = List.map (Edge nd) (PartialOrd.minimums (uppers nd))
            where
                uppers :: PartialOrd a => Node a -> [Node a]
                uppers (Node ls nd us llt) = List.deleteBy (PartialOrd.==) (Node ls nd us llt)
                                                (List.map (llt List.!!) us)


showDotLattice :: (Show a, PartialOrd a) => Lattice a -> String
showDotLattice ll = header List.++
                List.concat (List.map (show) (toLatticeGraph ll)) List.++
                footer
        where header = "graph lattice {\n"
                    List.++ "\trankdir = TB;\n\tratio = 0.75;\n"
                    List.++ "\tnode[shape = none];\n"
              footer = "}"
