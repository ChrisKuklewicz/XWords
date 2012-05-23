{-# LANGUAGE BangPatterns #-}
module Lookup(lookupTiles,withTemplate,withTemplates,Score,Val,Count,Hist(..),HandTiles(..)) where

import qualified Data.IntMap as M
import Data.Array.IArray((!))
--import Data.List(group,sort)
import Data.Monoid(Monoid(..))
--import Data.Ix(inRange)
import Data.Char (toLower)
import Data.List(group)

import Dict(Dict(Dict))
import Tiles(valArray)
import Template

-- imports for testing
import Dict(buildInts,Dictionary(..))
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Batch
import Data.List(nub,sort,nubBy,unfoldr)
import Data.Maybe(listToMaybe)
import Control.Monad(replicateM,foldM,liftM2)
--
import Common
import Debug.Trace

type IsWild = Bool
type Val = Int
type Count = Int
type Score = Int
newtype Hist = Hist {unHist :: [(Val,Count)]} deriving Show

data HandTiles = HandTiles Count Hist Count -- # wilds, Hist of A..Z, # of tiles including wilds

data SM = SM !Int !Int -- First is running total of tiles, the second is multipler

hist :: [Val] -> Hist
hist = Hist . map hist' . group . sort
  where hist' xs = (head xs, length xs)

fromHist :: Hist -> [Val]
fromHist = concatMap (uncurry (flip replicate)) . unHist

pullTile :: Hist -> [(Val,Hist)]
pullTile = helper id . unHist
  where helper _ [] = []
        helper f (pair@(x,1):xs) = (x,Hist $ f             xs)  : helper (f . (pair:)) xs
        helper f (pair@(x,n):xs) = (x,Hist $ f ((x,pred n):xs)) : helper (f . (pair:)) xs

addBonus :: (Score,String) -> (Score,String)
addBonus (score,str) = (score+50,str)

lookupTiles :: Dict -> HandTiles -> [(Score,String)]
lookupTiles !t (HandTiles wilds s n) =
  let scoreBonus | n/=7 = id
                 | otherwise = map scoreBonus'
      scoreBonus' x@(_,str) | length str == 7 = addBonus x
                            | otherwise = x
  in scoreBonus $ lookupAll t 0 wilds s

withTemplate :: Dict -> HandTiles -> String  -> [(Score,String)]
withTemplate t (HandTiles wilds s n) sTemplate =
  let template = parseTemplate sTemplate
      m = length (filter isBlank template)
      --msg = show(wilds,s,template,(m,n,m<=n))
      scoreBonus | m == 7 = map addBonus
                 | otherwise = id
  in if m <= n then scoreBonus . mapFst total . lookupTemplate t mempty wilds s $ template
       else []

withTemplates :: Dict -> HandTiles -> String  -> [(Score,String)]
withTemplates t (HandTiles wilds s n) sTemplate = 
  let templates = allTemplates (parseTemplate sTemplate)
      templateLens = map (\template -> (template,length (filter isBlank template))) templates
      validTemplates = filter (\(_,m)-> 1<=m && m<=n) templateLens
      oneTemplateLen (template,m) | m==7 = map addBonus . oneTemplate $ template
                                  | otherwise = oneTemplate template
      oneTemplate template = mapFst total . lookupTemplate t mempty wilds s $ template
  in concatMap oneTemplateLen $ validTemplates

lookupAll :: Dict -> Score -> Count -> Hist -> [(Score,String)]
lookupAll !(Dict here m) !score !wilds !s =
    if here then (score,[]) : rest else rest
  where rest :: [(Score,String)]
        rest = concatMap choose (pullTile s) ++ wild
        choose :: (Val,Hist) -> [(Score,String)]
        choose (y,s') = case M.lookup y m of
                          Nothing -> []
                          Just t -> let c = toEnum y
                                        score' = score + valArray!y
                                    in mapSnd (c:) (lookupAll t score' wilds s')
        wild :: [(Score,String)]
        wild = if wilds>0
                 then let wilds' = pred wilds
                          f (y,t) = let c = toLower (toEnum y)
                                    in mapSnd (c:) (lookupAll t score wilds' s)
                      in concatMap f (M.assocs m)
                 else []

mapFst :: (a1 -> a2) -> [(a1,b)] -> [(a2,b)]
mapFst f = go where
  go [] = []
  go ((a,b):rest) = (f a,b) : go rest

mapSnd :: (b1 -> b2) -> [(a,b1)] -> [(a,b2)]
mapSnd f = go where
  go [] = []
  go ((a,b):rest) = (a,f b) : go rest

scoreOf :: Square -> IsWild -> Val -> SM
scoreOf square isWild val =
  case square of
    Normal       -> SM base 1
    DoubleLetter -> SM (2*base) 1
    TripleLetter -> SM (3*base) 1
    DoubleWord   -> SM base 2
    TripleWord   -> SM base 3
 where base | isWild = 0
            | otherwise = valArray!val

instance Monoid SM where
  mempty = SM 0 1
  mappend (SM score1 mult1) (SM score2 mult2) = SM (score1+score2) (mult1 * mult2)

addTile :: SM -> Square -> IsWild -> Val -> SM
addTile sm s i v = mappend sm (scoreOf s i v)

total :: SM -> Score
total (SM t m) = m*t

lookupTemplate :: Dict -> SM -> Count -> Hist  -> Template -> [(SM,String)]
lookupTemplate !(Dict here _m) !score !_wilds !_s [] =
  if here then [(score,[])] else []

lookupTemplate !(Dict _here m) !score !wilds !s (Filled isWild val : template') = 
   case M.lookup val m of
     Nothing -> []
     Just t -> let c = (if isWild then toLower else id) (toEnum val)
                   score' = addTile score Normal isWild val
               in mapSnd (c:) (lookupTemplate t score' wilds s template')

lookupTemplate !(Dict _here m) !score !wilds !s (Blank square:template') = 
  concatMap choose (pullTile s) ++ useWild
 where
  choose :: (Val,Hist) -> [(SM,String)]
  choose (val,s') =
    case M.lookup val m of
      Nothing -> []
      Just t -> let c = toEnum val
                    score' = addTile score square False val
                in mapSnd (c:) (lookupTemplate t score' wilds s' template')
  useWild ::[(SM,String)]
  useWild | 0 == wilds = []
          | otherwise =
    let wilds' = pred wilds
        useAs (val,t) = let c = toLower (toEnum val)
                            score' = addTile score square True val
                        in mapSnd (c:) (lookupTemplate t score' wilds' s template')
    in concatMap useAs (M.assocs m)

-- testing

instance Arbitrary Hist where
  arbitrary = do
    n <- choose (1,7)
    vals <- replicateM n (choose (fromEnum 'A',fromEnum 'E'))
    return (hist vals)

test_hist_fromHist :: [Val] -> Bool
test_hist_fromHist x = sort x == (fromHist (hist x))

test_pullTile :: Hist -> Bool
test_pullTile h@(Hist x) = let each = map fst x == nub (sort (map fst (pullTile h)))
                               y0 = (fromHist h)
                               y1 = (sort $ unfoldr (listToMaybe . pullTile) h)
                               y2 = (sort $ unfoldr (listToMaybe . reverse . pullTile) h)
                           in each && (y0==y1) && (y0==y2)

instance Arbitrary HandTiles where
  arbitrary = do
    w <- choose (0,2)
    n <- choose (0,7-w)
    vals <- replicateM n (choose (1,26))
    return (HandTiles w (hist vals) (w+n))

test_lookup :: Dictionary -> Bool
test_lookup (Dictionary ds) =
  let dict = buildInts ds
      isOk :: [Int] -> Bool
      isOk h = (score==score')&&(sort str==str')
        where score' = (sum $ map (valArray!) h) + (if length h == 7 then 50 else 0)
              str' = sort $ map toEnum h
              (score,str) = last . sort $ lookupTiles dict $ HandTiles 0 (hist h) (length h)
  in all isOk ds
