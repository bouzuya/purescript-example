module Data.AddressBook where

import Prelude
import Control.Apply (lift2)
import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book
-- insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street

hasEntry :: String -> String -> AddressBook -> Boolean
hasEntry firstName lastName book = not $ null $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy (\a b -> a.firstName == b.firstName && a.lastName == b.lastName) book

address1 =
  {
    street: "street 1"
  , city: "city 1"
  , state: "state 1"
  }
address2 =
  {
    street: "street 2"
  , city: "city 2"
  , state: "state 2"
  }
entry1 =
  {
    firstName: "first name 1"
  , lastName: "last name 1"
  , address: address1
  }
entry2 =
  {
    firstName: "first name 2"
  , lastName: "last name 2"
  , address: address2
  }

book1 = insertEntry entry1 emptyBook
book2 = insertEntry entry2 book1

printEntry firstName lastName book =
  showEntry <$> findEntry firstName lastName book

printEntryByStreet street book =
  showEntry <$> findEntryByStreet street book

-- findEntry firstName lastName = head <<< filter filterEntry
--   where
--     filterEntry :: Entry -> Boolean
--     filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

address :: String -> String -> String -> Address
address street city state = { street: street, city: city, state: state }

combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs

add' :: forall f a. Applicative f => Semiring a => f a -> f a -> f a
add' x y = lift2 (+) x y

sub' :: forall f a. Applicative f => Ring a => f a -> f a -> f a
sub' x y = lift2 (-) x y

mul' :: forall f a. Applicative f => Semiring a => f a -> f a -> f a
mul' x y = lift2 (*) x y

div' :: forall f a. Applicative f => EuclideanRing a => f a -> f a -> f a
div' x y = lift2 (/) x y

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) = Just <$> x

-- NOTE
-- combineMaybe (Just ([1, 2])) -- => [Just 1, Just 2]

