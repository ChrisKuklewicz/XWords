{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
module Dict(Dict(Dict),Map,build,buildInts,buildASCII,seqHack
           ,dict_hit,dict_map,inDict,notInDict,pretty,fromDict
           ,run_all_tests,Dictionary(..)) where

import qualified Data.IntMap as M
import Data.List(groupBy)
import Data.Char (toUpper)
import qualified Data.ByteString as B
import Common(on)

-- imports for testing
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Batch
import Data.List(group,sort)
import Control.Monad(replicateM,foldM)

type Map k = M.IntMap k

data Dict = Dict {-# UNPACK #-} !Bool {-# UNPACK #-} !(Map Dict) deriving (Show,Eq)

dict_hit :: Dict -> Bool
dict_hit (Dict b _) = b

dict_map :: Dict -> Map Dict
dict_map (Dict _ m) = m

build :: [String] -> Dict
build = buildInts . map (map (fromEnum . toUpper))

inDict :: [Int] -> Dict -> Bool
inDict is root = maybe False dict_hit $ foldM (\t i -> M.lookup i (dict_map t)) root is

notInDict :: [Int] -> Dict -> Bool
notInDict is root = not (inDict is root)

fromDict :: (Enum a) => Dict -> [[a]]
fromDict tIn = go id tIn [] where
  go f t =  (if dict_hit t then (f []:) else id) . (M.foldWithKey (\x t' g -> go (f . (toEnum x:)) t' . g) id (dict_map t))

pretty :: Dict -> [String]
pretty (Dict b m) = (if b then ([]:) else id) (concatMap (\(x,t) -> pre1 x (pretty t)) (M.assocs m))
  where pre1 _ [] = []
        pre1 x (y:ys) = (toEnum x : y) : map ('.':) ys

-- More simply: What if there is only one String to "build"? Then we
-- can share that list of Dict's.

seqHack :: Dict -> ()
seqHack (Dict _ m) = case M.elems m of
                       [] -> ()
                       xs -> seqHack (last xs)

-- buildInts requires a (possibly empty) list of distinct ascending
-- lists, which means the null list as a member can only be the head,
-- if present.
buildInts :: [[Int]] -> Dict
buildInts xs = case xs of
                 ([]:ys) -> Dict True (makeMap ys)
                 _ -> Dict False (makeMap xs)
  where -- makeMap takes a possibly empty list of non-empty lists which are
        -- distinct and in ascending order
        makeMap :: [[Int]] -> Map Dict
        makeMap = M.fromDistinctAscList . map pullHead . groupBy ((==) `on` head)
          where pullHead ys@((x:_):_) = let t = buildInts (map tail ys) -- ys is not null
                                        in seq t (x,t) -- add strictness
                pullHead ys = error ("Cannot happen in IntDict.makeMap.pullHead:"++show ys)

buildASCII :: [B.ByteString] -> Dict
buildASCII [] = Dict False M.empty
buildASCII ys@(x:xs) | B.null x = Dict True (makeMapASCII xs)
                     | otherwise = Dict False (makeMapASCII ys)
  where makeMapASCII :: [B.ByteString] -> Map Dict
        makeMapASCII = M.fromDistinctAscList . map pullHead . groupBy ((==) `on` B.head)
          where pullHead zs@(z:_) = let t = buildASCII (map B.tail zs)
                                    in seq t (fromEnum (B.head z),t)
                pullHead _ = error "Cannot happen in Dict.makeMapASCII.pullHead"

-- Testing

run_all_tests :: IO ()
run_all_tests = run_HUnit >> run_QuickCheck where
  run_QuickCheck = runTests "Dict with QuickCheck" defOpt tests_QuickCheck
  run_HUnit = runTestTT tests_HUnit

newtype Dictionary = Dictionary {unDictionary :: [[Int]]} deriving Show

newtype English = English {unEnglish :: [Int]} deriving Show
newtype ShortEnglish = ShortEnglish [Int] deriving Show

instance Arbitrary Dictionary where
  coarbitrary (Dictionary (ds)) = variant 0 . (foldr (.) id  . map (coarbitrary . English) $ ds)
  arbitrary = do
    n <- oneof [choose (0,10),choose (5*5*5,5*5*5*5)]
    ss <- (map head . group . sort . map unEnglish) `fmap` (vector n)
    return (Dictionary ss)

instance Arbitrary English where
  coarbitrary (English e) = variant 0 . coarbitrary e
  arbitrary = do
    n <- choose (0,15)
    s <- replicateM n (choose (fromEnum 'A',fromEnum 'E'))
    return (English s)

instance Arbitrary ShortEnglish where
  coarbitrary (ShortEnglish e) = variant 0 . coarbitrary e
  arbitrary = do
    n <- choose (0,5)
    s <- replicateM n (choose (fromEnum 'A',fromEnum 'E'))
    return (ShortEnglish s)

tests_QuickCheck :: [TestOptions -> IO TestResult]
tests_QuickCheck = [run prop_hasNull
                   ,run prop_hasAll
                   ,run prop_hasFirst
                   ,run prop_hasLast
                   ,run prop_hasRandom
                   ]
  where prop_hasNull (Dictionary d) = case d of ([]:_) -> [] `inDict` (buildInts d)
                                                _   -> [] `notInDict` (buildInts d)
        prop_hasAll :: Dictionary -> Bool
        prop_hasAll (Dictionary d) = d == fromDict (buildInts d)
        prop_hasFirst (Dictionary d) = (not (null d)) ==> ((head d) `inDict` (buildInts d))
        prop_hasLast (Dictionary d)  = (not (null d)) ==> ((last d) `inDict` (buildInts d))
        prop_hasRandom (Dictionary d) (ShortEnglish e) =
          let b = elem e d
          in collect (length e) $ collect b $ b == inDict e (buildInts d)

tests_HUnit :: Test
tests_HUnit = TestList $ concat
  [[ "null Dict" ~: (buildInts []) ~?= (Dict False (M.empty))
   , "one Dict" ~: (buildInts [[1..10]]) ~?= (foldr (\c t -> Dict False (M.singleton c t)) (Dict True M.empty) [1..10])
   ]
  ,map (\e -> "d1t1" ~: e `inDict` t1 ~? show e) d1
  ,map (\e -> "d1t2" ~: e `notInDict` t2 ~? show e) d1
  ,map (\e -> "d2t1" ~: e `notInDict` t1 ~? show e) d2
  ,map (\e -> "d2t2" ~: e `inDict` t2 ~? show e) d2
  ]
 where
  -- d1 is an intersting set of [Int]
  d1 = sort $ map (\n -> [1..n]) [1..10] ++ [[2,3,6,7,8],[2,3,9,10,11],[3]]
  -- d2 is the tails of d1, and has no common members
  d2 = sort $ map (\n -> [2..n]) [1..10] ++ [[3,6,7,8],[3,9,10,11]]
  t1 = buildInts d1
  t2 = buildInts d2
