module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Data.List (nub, find)
import Data.Foldable (all, elem)

import Partitions
import Lattice
import Func (F2, readF2fromTable)

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset xs ys = all (\t -> t `elem` ys) xs

data Flag
    = Version               -- -v
    | PrintPartitions       -- -p
    | PrintCongruences
    | PrintLattice          -- -l
    | ModularId
    | DistributiveId
    | CommutativeId
    | OutFile String        -- -o FILE
    | Help                  -- -h
    deriving (Eq,Show)

findOutputFile :: [Flag] -> Maybe String
findOutputFile [] = Nothing
findOutputFile (t:ts) = case t of
                        OutFile filename -> Just filename
                        otherwise -> findOutputFile ts

options :: [OptDescr Flag]
options =
   [Option ['p'] ["print-partitions"] (NoArg PrintPartitions)
        "Print all partitions (all possible equivalence relations)."
   ,Option ['t'] ["print-congruences"] (NoArg PrintCongruences)
        "Print congruences."
   ,Option ['l'] ["print-dot-lattice"] (NoArg PrintLattice)
         "Print congruence lattice (in DOT format)."
   ,Option ['o'] ["output"]       (ReqArg OutFile "FILE")
         "Write lattice to a file."
   ,Option ['v'] ["version"]    (NoArg Version)
        "Print version and exit."
   ,Option ['m'] ["modular"]    (NoArg ModularId)
        "Check holding modular identity: x /\\ (y \\/ z) = x /\\ ((y /\\ (x \\/ z)) \\/ z)"
   ,Option ['d'] ["distributive"]    (NoArg DistributiveId)
        "Check holding distributive identity: x /\\ (y \\/ z) = (x /\\ y) \\/ (x /\\ z)"
   ,Option ['c'] ["commutative"]    (NoArg CommutativeId)
         "Check holding commutative identity: x /\\ y = y /\\ x (for time testing)"
   ,Option ['h','?'] ["help"]    (NoArg Help)
        "Print this help message."
   ]

parse :: [String] -> IO ([Flag],[String])
parse argv = case getOpt Permute options argv of
    (args,nargs,[]) ->
                    return (nub args, nargs)
    (_,_,errs) -> do
                    hPutStrLn stderr (concat errs)
                    exitWith (ExitFailure 1)

main :: IO a
main = do
    args <- getArgs >>= parse
    {- print arguments -}
    -- hPutStrLn stdout (show args)

    {- open output file -}
    fhdl <- case findOutputFile (fst args) of
        Just fname -> openFile fname WriteMode
        otherwise -> return (stdout)

    handle args fhdl

    hClose fhdl
    exitWith ExitSuccess

    where handle args fhdl
            | Version `elem` (fst args) = do
                    hPutStrLn stdout "0.0.1"
            | Help `elem` (fst args) = do
                    appname <- getProgName
                    hPutStrLn stdout (usageInfo (header appname) options)
                    hPutStrLn stdout footnote
            | snd args == [] = do
                    contents <- hGetContents stdin
                    process fhdl (fst args) contents
            | snd args /= [] = do
                    handle <- openFile (head $ snd args) ReadMode
                    contents <- hGetContents handle
                    process fhdl (fst args) contents
                    hClose handle
            | otherwise = do
                    hPutStrLn stdout "Bad options, see help"
            where
                process fhdl args contents = do
                        -- let fs = read contents :: [((String,String),String)]
                        let fs = readF2fromTable contents
                        -- hPutStrLn stdout (show (getElements fs))
                        let eq = partitionSet (getElements fs)
                        if PrintPartitions `elem` args
                            then output stdout (show eq)
                            else return ()
                        -- let cl = makeConLattice fs
                        let cl = toLattice (toCongruences eq fs)
                        if PrintCongruences `elem` args
                            then output stdout (show cl)
                            else return ()
                        hPutStrLn stdout ("Congruences: " ++ show (length cl))
                        if ModularId `elem` args
                            then output stdout ("Modularity: " ++ show (isModular cl))
                            else return ()
                        if DistributiveId `elem` args
                            then output stdout ("Distributivity: " ++ show (isDistributive cl))
                            else return ()
                        if CommutativeId `elem` args
                            then output stdout ("Commutativity: " ++ show (isCommutative cl))
                            else return ()
                        if PrintLattice `elem` args
                            then output fhdl (showDotLattice cl)
                            else return ()

                output fhdl res = hPutStrLn fhdl res

                header appname = "Usage: " ++ appname ++ "file [-ptlovmdch]"
                        ++ "\n\nThis utility check holding lattice identities in the congruence lattice.\n"
                footnote = "Author: genary@ya.ru"
