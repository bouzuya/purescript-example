module Main where

import Prelude (Unit, show, bind, (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM, random)

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM | e) Unit
main = do
  n <- random
  log ("Hello " <> show n)
