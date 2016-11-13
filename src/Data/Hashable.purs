module Data.Hashable where

import Prelude

import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary })
    = "(Complex { real: " <> show real <>
      ", imaginary: " <> show imaginary <> " })"

instance eqComplex :: Eq Complex where
  eq (Complex { real: r1, imaginary: i1 })
     (Complex { real: r2, imaginary: i2 })
    = (r1 == r2) && (i1 == i2)

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty ah at) = show $ [ah] <> at

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty ah at) (NonEmpty bh bt) = NonEmpty ah (at <> [bh] <> bt)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty h t) = (NonEmpty (f h) (map f t))

instance foldableNonEmpty :: Foldable NonEmpty where
  -- foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldr f b (NonEmpty h t) = foldr f (f h b) t
  -- foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldl f b (NonEmpty h t) = foldl f (f b h) t
  -- foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
  foldMap f (NonEmpty h t) = f h <> (foldMap f t)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  -- eq :: a -> a -> Boolean
  eq (NonEmpty h1 t1) (NonEmpty h2 t2) = h1 == h2 && t1 == t2


threeAreEqual :: forall a. Eq a => a -> a -> a -> Boolean
threeAreEqual a1 a2 a3 = a1 == a2 && a2 == a3

showCompare :: forall a. (Ord a, Show a) => a -> a -> String
showCompare a1 a2 | a1 < a2 =
  show a1 <> " is less than " <> show a2
showCompare a1 a2 | a1 > a2 =
  show a1 <> " is greater than " <> show a2
showCompare a1 a2 =
  show a1 <> " is equal to " <> show a2

data Extended a = Finite a | Infinite

-- TODO 6.7
-- instance ordExtended :: Ord (Extended a) where
  -- compare :: a -> a -> Ordering

-- TODO 6.7
-- data OneMore f a = OneMore a (f a)

-- TODO 6.10


newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
  hash :: a -> HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance equalHashCode :: Eq HashCode where
  -- eq :: a -> a -> Boolean
  eq (HashCode h1) (HashCode h2) = h1 `eq` h2

instance showHashCode :: Show HashCode where
  -- show :: a -> String
  show (HashCode h) = show h

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

-- TODO: 6.11
