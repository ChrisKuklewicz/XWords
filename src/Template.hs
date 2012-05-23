module Template where

import Data.Char(toUpper)
import Data.Ix
import Data.List
import qualified Data.Set as S

data Square = Normal | DoubleLetter | TripleLetter | DoubleWord | TripleWord deriving Show
data Entry = Blank Square | Filled Bool Int deriving Show
type Template = [Entry]

f `on` g = (\a b -> (g a) `f` (g b))

parseTemplate :: String -> Template
parseTemplate = map parseEntry
parseEntry '-' = Blank Normal
parseEntry '2' = Blank DoubleLetter
parseEntry '3' = Blank TripleLetter
parseEntry '4' = Blank DoubleWord
parseEntry '5' = Blank TripleWord
parseEntry x | inRange ('A','Z') x = Filled False (fromEnum x)
             | inRange ('a','z') x = Filled True (fromEnum (toUpper x))
             | otherwise = error ("Unknown character "++(x:" in the template"))

isBlank (Blank {}) = True
isBlank _ = False

blankTemplates :: Int -> [Template]
blankTemplates inHand = take inHand (iterate (Blank Normal:) [Blank Normal])

allTemplates :: Template -> [Template]
allTemplates template = sortBy (flip compare `on` length) $
  let leading = length (takeWhile isBlank template)
      backwards = reverse template
      trailing = length (takeWhile isBlank backwards)
  in if all isBlank template || trailing==0
       then take (succ leading) (tails template)
       else concatMap (take (succ leading) . tails . reverse) 
            . take (succ trailing) . tails $ backwards
