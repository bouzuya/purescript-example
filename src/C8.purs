module C8 where

import Prelude

import Control.Plus (empty)
import Data.Array ((..), head, tail)
import Data.Maybe

countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n
    then pure [x, y]
    else empty

-- head :: forall a. Array a -> Maybe a
-- tail :: forall a. Array a -> Maybe (Array a)
third :: forall a. Array a -> Maybe a
third xs = do -- [1,2,3,4]
  xs1 <- tail xs -- [2, 3, 4]
  xs2 <- tail xs1 -- [3, 4]
  head xs2

-- TODO: 8.7
