module Test.Euler where

import Prelude

import Euler (answer)
import Test.Assert (assert)

main = do
  assert (answer == 233168)
