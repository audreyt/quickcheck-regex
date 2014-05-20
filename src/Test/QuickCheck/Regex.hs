{-|

This module exports a 'matching' function that turns a regular expression
into a generator for strings matching that regex. For example:

>>> import Test.QuickCheck.Regex (matching)
>>> import Test.QuickCheck (generate)
>>> generate (matching "[-a-z0-9._%]+@[-a-z0-9.]+\\.[a-z]{3,18}\\.(asia|eu|today)")
"9%az4rmek@rar1d8qvo04jkd1.agzy.asia"

-}
module Test.QuickCheck.Regex (matching) where

import Test.QuickCheck (Gen, oneof, arbitrary, listOf, listOf1, choose, generate)
import Data.List ((\\))
import Control.Monad (replicateM)
import Data.Monoid (mempty)
import Regex.Genex.Normalize (normalize)
import Text.Regex.TDFA.Pattern (Pattern(..), PatternSet(..))
import Text.Regex.TDFA.ReadRegex (parseRegex)
import qualified Data.Set as Set

minChar, maxChar :: Char
minChar = ' '
maxChar = '~'

matching :: String -> Gen String
matching regex = case parseRegex regex of
    Left x -> fail $ show x
    Right (pattern, _) -> go $ normalize mempty pattern
    where
    go :: Pattern -> Gen String
    go pat = case pat of
        PEmpty      -> return ""
        POr ps      -> oneof (map go ps)
        PConcat ps  -> concat `fmap` mapM go ps
        PQuest p    -> oneof [return "", go p]
        PDot{}      -> do
            n <- choose (fromEnum minChar, fromEnum maxChar)
            return [toEnum n]
        PPlus p     -> concat `fmap` listOf (go p)
        PStar _ p   -> concat `fmap` listOf1 (go p)
        PBound low high p -> do
            n <- choose (low, maybe 10 id high)
            concat `fmap` replicateM n (go p)
        PChar{ getPatternChar = ch } -> return [ch]
        PEscape{ getPatternChar = ch } -> oneChar $ expandEscape ch
        PAny{ getPatternSet = PatternSet (Just cset) _ _ _ } -> oneChar $ Set.toList cset
        PAnyNot{ getPatternSet = PatternSet (Just cset) _ _ _ } -> oneChar $ charExclude (Set.toList cset)
        _           -> fail $ "Invalid pattern: " ++ show pat
    oneChar = oneof . map (return . (:[]))
    charExclude = ([minChar .. maxChar] \\)
    expandEscape ch = case ch of
        'n' -> "\n"
        't' -> "\t"
        'r' -> "\r"
        'f' -> "\f"
        'a' -> "\a"
        'e' -> "\ESC"
        'd' -> ['0'..'9']
        'w' -> ['0'..'9'] ++ '_' : ['a'..'z'] ++ ['A'..'Z']
        's' -> "\9\32"
        'D' -> charExclude $ ['0'..'9']
        'W' -> charExclude $ ['0'..'9'] ++ '_' : ['a'..'z'] ++ ['A'..'Z']
        'S' -> charExclude "\9\32"
        _   -> [ch]
