module Data.C4 where

import Prelude

import Data.Array (null, filter, concatMap, (..), foldl)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

countEven :: Array Int -> Int
countEven arr = f arr 0
  where
  f :: Array Int -> Int -> Int
  f a c =
    if null a
    then c
    else f (unsafePartial tail a)
      if isEven (unsafePartial head a)
      then c + 1
      else c

squares :: Array Int -> Array Int
squares a = map (\n -> n * n) a

squares' :: Array Int -> Array Int
squares' a = (\n -> n * n) <$> a

removeNegative :: Array Int -> Array Int
removeNegative a = filter (\n -> n >= 0) a

infixl 4 filter as <$?>

pairs :: Int -> Array (Array Int)
pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. (n - 1)
  b <- a .. (n - 1)
  c <- b .. (n - 1)
  guard $ a * a + b * b == c * c
  pure [a, b, c]

-- TODO
-- factorizations :: Int -> Array (Array Int)

everyTrue :: Array Boolean -> Boolean
everyTrue [] = false
everyTrue a = foldl (&&) true a
