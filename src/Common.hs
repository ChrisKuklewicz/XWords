module Common where

import System.Random
import Test.QuickCheck

on :: (b->b->c) -> (a->b) -> (a -> a -> c)
f `on` g = (\a b -> (g a) `f` (g b))

gen :: (Arbitrary a) => IO a
gen = do
  x <- randomIO
  s <- newStdGen
  let a = generate x s arbitrary
  return a
