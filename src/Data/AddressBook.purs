module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

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
