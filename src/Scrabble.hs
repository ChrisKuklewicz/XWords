{-# LANGUAGE BangPatterns #-}
module Scrabble (makeDict,process,validHand,prettySC
                ,ScrabbleCommand(..),Sort(..),Dict,FilePath,Score) where

import Data.Array.Unboxed((!))
import Data.Ix(inRange)
import Data.List(group,sort)
import Data.Char(toUpper)
import Tiles(freqArray)
import Dict(Dict,build)
import Lookup(HandTiles(..),Hist(..),Score,lookupTiles,withTemplate,withTemplates)
--import Best(best)

import Control.OldException(catchJust,ioErrors)
import Data.List(sortBy)

hist :: [Int] -> Hist -- [(Int,Int)]
hist = Hist . map hist' . group . sort
  where hist' xs = (head xs, length xs)

makeDict :: FilePath -> IO (Either String Dict)
makeDict file = catchJust ioErrors
                          (fmap (Right . build . lines) (readFile file))
                          (\e -> return (Left (show e)))

data Sort = Sort {iMethod :: Int, bReversed :: Bool} deriving Show

sortMethod :: Sort -> [(Score,String)] -> [(Score,String)]
sortMethod (Sort 1 False) = sortBy (flip compare)
sortMethod (Sort 1 True) = sort
sortMethod (Sort 2 False) = sortBy (flip compare `on` (length . snd))
sortMethod (Sort 2 True) = sortBy (compare `on` (length . snd))
sortMethod (Sort 3 False) = sortBy (compare `on` snd)
sortMethod (Sort 3 True) = sortBy (flip compare `on` snd)
sortMethod err = error $ "Scrabble.sortMethod failed "++show err

data ScrabbleCommand = LookupCommand {sTiles :: String, getSort :: Sort}
                     | TemplateCommand {sTiles :: String, sTemplate :: String, getSort :: Sort}
                     | TemplatesCommand {sTiles :: String, sTemplate :: String, getSort :: Sort}
  deriving Show

prettySC :: ScrabbleCommand -> Score -> String
prettySC (LookupCommand {sTiles=hand}) bestScore = (show bestScore) ++ " " ++ hand
prettySC (TemplateCommand {sTiles=hand,sTemplate=template}) bestScore = (show bestScore) ++ " " ++ hand ++ " " ++ template
prettySC (TemplatesCommand {sTiles=hand,sTemplate=template}) bestScore = (show bestScore) ++ " " ++ hand ++ " " ++ template

parseTiles :: String -> HandTiles
--parseTiles :: String -> (Int, [(Int,Int)], Int)
parseTiles tiles = HandTiles wilds h (length tiles)
    where wilds = length (filter (dot ==)  tiles)
          h = hist . map fromEnum . map toUpper . filter (dot /=) $ tiles
          dot = '.'

-- Returns sorted (and perhaps limited) results
process :: Dict -> ScrabbleCommand -> [(Score,String)]
process t command = post . sortMethod (getSort command) $ results where
  hand = parseTiles (sTiles command)
  post | validHand hand = id
       | otherwise = take 100
  results = case command of
              LookupCommand {} -> lookupTiles t hand
              TemplateCommand {} -> withTemplate t hand (sTemplate command)
              TemplatesCommand {} -> withTemplates t hand (sTemplate command)

on :: (b->b->c) -> (a->b) -> (a -> a -> c)
f `on` g = (\a b -> (g a) `f` (g b))

validHand :: HandTiles -> Bool
validHand (HandTiles wilds (Hist h) n) = inRange (0,2) wilds && inRange (1,7) n && and (map validCharFreq h)
  where validCharFreq (c,f) = inRange (1,freqArray ! c) f

