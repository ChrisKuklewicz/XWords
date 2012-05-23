module Tiles(valArray,freqArray,azBounds) where

import Data.Array.Unboxed

freqArray :: UArray Int Int
freqArray = listArray azBounds $
  [9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1]

azBounds :: (Int,Int)
azBounds = (fromEnum 'A',fromEnum 'Z')

valArray :: UArray Int Int
valArray = listArray azBounds $
  [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]
