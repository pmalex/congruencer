module PartialFunc where

import qualified Data.Bool  as Bool (Bool (False, True), otherwise)
import qualified Data.List  as List (concatMap, head, length, lines, map, nub,
                                     tail, words, (++))
import qualified Data.Tuple as Tuple (fst, snd)
-- import           GHC.Base   (Eq, String, error, (.), (/=), (==))
import           GHC.Base   (error)
import qualified Text.Read  as Read (Read, lex)
-- import           Text.Show  (show)



-- | Abstract partial function type
type PartialFunc arg value = [(arg, value)]

-- | Returns domain set of a partial function
dom :: Eq arg => PartialFunc arg value -> [arg]
dom fs = List.nub (List.map Tuple.fst fs)

-- | Function of two arguments
newtype PartialFunc2 arg1 arg2 value
    = PartialFunc2 (PartialFunc (arg1, arg2) value)
    deriving Show

-- instance (Read arg1, Read arg2, Read value) => Read (PartialFunc2 arg1 arg2 value) where
--     readsPrec d str = buildPartialFunc str
--         where
--             -- | Build a function of two arguments from a columns of a row of the table
--             buildPartialFunc
--             :: [String]   -- ^ First function's arguments list, denote it by `A1`.
--             -> String     -- ^ Second argument (atomic element), denote it by `a2`.
--             -> [String]   -- ^ Values of the function on the set `A1 x a2` (cartesian product).
--             -> PartialFunc (arg1, arg2) value
--             buildPartialFunc [] _ _ = []
--             buildPartialFunc _ _ [] = []
--             buildPartialFunc (firstArg : firstArgs) secondArg (funcVal : funcValues)
--                 | funcVal /= "_" = ((firstArg, secondArg), funcVal) : buildPartialFunc firstArgs secondArg funcValues
--                 | Bool.otherwise  = buildPartialFunc firstArgs secondArg funcValues

newtype Test a = Test a deriving Show
instance (Read a) => Read (Test a) where
    readsPrec d str = [(Test x, s) |
                            ("{", s) <- Read.lex str,
                            (x, s) <- readsPrec d s,
                            ("}", s) <- Read.lex s
                        ]


-- | Read a partial function of two arguments defined in a table format.
readPartialFunc2FromTable :: String -> PartialFunc2 String String String
readPartialFunc2FromTable str =
    let firstArgs = -- list with first function arguments
            (List.words . List.head . List.lines) str
        tableRows = -- list with remaining table's rows (they encode function's map)
            (List.tail . List.lines) str
    in PartialFunc2 (List.concatMap (buildPartialFuncFromRow firstArgs) tableRows)

    where
        -- | Parse elements of a row and bind it with the first arguments.
        buildPartialFuncFromRow :: [String] -> String -> PartialFunc (String, String) String
        buildPartialFuncFromRow firstArgs tableRow =
                let (secondArg, tailStr) = (List.head . Read.lex) tableRow
                    funcValues = List.words tailStr
                in if List.length firstArgs /= List.length funcValues
                   then
                       error ("You specified not all arguments in a row `"
                                List.++ tableRow List.++ "`."
                                List.++ " Maybe you forget specify `_`?")
                   else
                       buildPartialFunc firstArgs secondArg funcValues

        -- | Build a function of two arguments from a columns of a row of the table
        buildPartialFunc
           :: [String]   -- ^ First function's arguments list, denote it by `A1`.
           -> String     -- ^ Second argument (atomic element), denote it by `a2`.
           -> [String]   -- ^ Values of the function on the set `A1 x a2` (cartesian product).
           -> PartialFunc (String, String) String
        buildPartialFunc [] _ _ = []
        buildPartialFunc _ _ [] = []
        buildPartialFunc (firstArg : firstArgs) secondArg (funcVal : funcValues)
            | funcVal /= "_" = ((firstArg, secondArg), funcVal) : buildPartialFunc firstArgs secondArg funcValues
            | Bool.otherwise  = buildPartialFunc firstArgs secondArg funcValues
