module Data.AddressBook where

import Prelude

newtype Address = Address
  { street :: String
  , city   :: String
  , state  :: String
  }

address :: String -> String -> String -> Address
address street city state = Address { street, city, state }

data PhoneType
  = HomePhone
  | WorkPhone
  | CellPhone
  | OtherPhone

newtype PhoneNumber = PhoneNumber
  { "type" :: PhoneType
  , number :: String
  }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber ty number = PhoneNumber
  { "type": ty
  , number: number
  }

newtype Person = Person
  { firstName   :: String
  , lastName    :: String
  , homeAddress :: Address
  , phones      :: Array PhoneNumber
  }

person :: String -> String -> Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones =
  Person { firstName, lastName, homeAddress, phones }

examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake St." "FakeTown" "CA")
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
         ]

instance showAddress :: Show Address where
  show (Address o) = "Address " <>
    "{ street: " <> show o.street <>
    ", city: "   <> show o.city <>
    ", state: "  <> show o.state <>
    " }"

instance showPhoneType :: Show PhoneType where
  show HomePhone = "HomePhone"
  show WorkPhone = "WorkPhone"
  show CellPhone = "CellPhone"
  show OtherPhone = "OtherPhone"

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber o) = "PhoneNumber " <>
    "{ type: "   <> show o."type" <>
    ", number: " <> show o.number <>
    " }"

instance showPerson :: Show Person where
  show (Person o) = "Person " <>
    "{ firstName: "   <> show o.firstName <>
    ", lastName: "    <> show o.lastName <>
    ", homeAddress: " <> show o.homeAddress <>
    ", phones: "      <> show o.phones <>
    " }"

-- module Data.AddressBook where

-- import Prelude
-- import Control.Apply (lift2)
-- import Control.Plus (empty)
-- import Data.List (List(..), filter, head, null, nubBy)
-- import Data.Maybe
-- import Data.Either
-- import Data.Validation.Semigroup
-- import Data.String
-- import Data.String.Regex as R
-- import Data.String.Regex.Flags (noFlags)
-- import Partial.Unsafe (unsafePartial)

-- -- type Entry =
-- --   { firstName :: String
-- --   , lastName  :: String
-- --   , address   :: Address
-- --   }

-- newtype Address = Address
--   { street :: String
--   , city   :: String
--   , state  :: String
--   }

-- -- type AddressBook = List Entry

-- -- showEntry :: Entry -> String
-- -- showEntry entry = entry.lastName <> ", " <>
-- --                   entry.firstName <> ": " <>
-- --                   showAddress entry.address

-- -- showAddress :: Address -> String
-- -- showAddress addr = addr.street <> ", " <>
-- --                    addr.city <> ", " <>
-- --                    addr.state

-- -- emptyBook :: AddressBook
-- -- emptyBook = empty

-- -- insertEntry :: Entry -> AddressBook -> AddressBook
-- -- insertEntry entry book = Cons entry book
-- -- insertEntry = Cons

-- -- findEntry :: String -> String -> AddressBook -> Maybe Entry
-- -- findEntry firstName lastName book = head $ filter filterEntry book
-- --   where
-- --     filterEntry :: Entry -> Boolean
-- --     filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- -- findEntryByStreet :: String -> AddressBook -> Maybe Entry
-- -- findEntryByStreet street book = head $ filter filterEntry book
-- --   where
-- --     filterEntry :: Entry -> Boolean
-- --     filterEntry entry = entry.address.street == street

-- -- hasEntry :: String -> String -> AddressBook -> Boolean
-- -- hasEntry firstName lastName book = not $ null $ filter filterEntry book
-- --   where
-- --     filterEntry :: Entry -> Boolean
-- --     filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- -- removeDuplicates :: AddressBook -> AddressBook
-- -- removeDuplicates book = nubBy (\a b -> a.firstName == b.firstName && a.lastName == b.lastName) book

-- -- address1 =
-- --   {
-- --     street: "street 1"
-- --   , city: "city 1"
-- --   , state: "state 1"
-- --   }
-- -- address2 =
-- --   {
-- --     street: "street 2"
-- --   , city: "city 2"
-- --   , state: "state 2"
-- --   }
-- -- entry1 =
-- --   {
-- --     firstName: "first name 1"
-- --   , lastName: "last name 1"
-- --   , address: address1
-- --   }
-- -- entry2 =
-- --   {
-- --     firstName: "first name 2"
-- --   , lastName: "last name 2"
-- --   , address: address2
-- --   }

-- -- book1 = insertEntry entry1 emptyBook
-- -- book2 = insertEntry entry2 book1

-- -- printEntry firstName lastName book =
-- --   showEntry <$> findEntry firstName lastName book

-- -- printEntryByStreet street book =
-- --   showEntry <$> findEntryByStreet street book

-- -- findEntry firstName lastName = head <<< filter filterEntry
-- --   where
-- --     filterEntry :: Entry -> Boolean
-- --     filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- address :: String -> String -> String -> Address
-- address street city state = Address { street: street, city: city, state: state }

-- -- combineList :: forall f a. Applicative f => List (f a) -> f (List a)
-- -- combineList Nil = pure Nil
-- -- combineList (Cons x xs) = Cons <$> x <*> combineList xs

-- -- add' :: forall f a. Applicative f => Semiring a => f a -> f a -> f a
-- -- add' x y = lift2 (+) x y

-- -- sub' :: forall f a. Applicative f => Ring a => f a -> f a -> f a
-- -- sub' x y = lift2 (-) x y

-- -- mul' :: forall f a. Applicative f => Semiring a => f a -> f a -> f a
-- -- mul' x y = lift2 (*) x y

-- -- div' :: forall f a. Applicative f => EuclideanRing a => f a -> f a -> f a
-- -- div' x y = lift2 (/) x y

-- -- combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
-- -- combineMaybe Nothing = pure Nothing
-- -- combineMaybe (Just x) = Just <$> x

-- -- NOTE
-- -- combineMaybe (Just ([1, 2])) -- => [Just 1, Just 2]

-- data PhoneType = HomePhone | WorkPhone | CellPhone | OtherPhone

-- newtype PhoneNumber = PhoneNumber
--   { "type" :: PhoneType
--   , number :: String
--   }

-- phoneNumber :: PhoneType -> String -> PhoneNumber
-- phoneNumber ty number = PhoneNumber
--   { "type": ty
--   , number: number
--   }

-- newtype Person = Person
--   { firstName   :: String
--   , lastName    :: String
--   , homeAddress :: Address
--   , phones      :: Array PhoneNumber
--   }

-- person :: String -> String -> Address -> Array PhoneNumber -> Person
-- person firstName lastName homeAddress phones =
--   Person { firstName, lastName, homeAddress, phones }

-- examplePerson :: Person
-- examplePerson =
--   person "John" "Smith"
--          (address "123 Fake St." "FakeTown" "CA")
--          [ phoneNumber HomePhone "555-555-5555"
--          , phoneNumber CellPhone "555-555-0000"
--          ]

-- instance showAddress :: Show Address where
--   show (Address o) = "Address " <>
--     "{ street: " <> show o.street <>
--     ", city: "   <> show o.city <>
--     ", state: "  <> show o.state <>
--     " }"

-- instance showPhoneType :: Show PhoneType where
--   show HomePhone = "HomePhone"
--   show WorkPhone = "WorkPhone"
--   show CellPhone = "CellPhone"
--   show OtherPhone = "OtherPhone"

-- instance showPhoneNumber :: Show PhoneNumber where
--   show (PhoneNumber o) = "PhoneNumber " <>
--     "{ type: "   <> show o."type" <>
--     ", number: " <> show o.number <>
--     " }"

-- instance showPerson :: Show Person where
--   show (Person o) = "Person " <>
--     "{ firstName: "   <> show o.firstName <>
--     ", lastName: "    <> show o.lastName <>
--     ", homeAddress: " <> show o.homeAddress <>
--     ", phones: "      <> show o.phones <>
--     " }"

-- -- nonEmpty :: String -> Either String Unit
-- -- nonEmpty "" = Left "Field cannot be empty"
-- -- nonEmpty _  = Right unit

-- -- validatePerson :: Person -> Either String Person
-- -- validatePerson (Person o) =
-- --   person <$> (nonEmpty o.firstName *> pure o.firstName)
-- --          <*> (nonEmpty o.lastName  *> pure o.lastName)
-- --          <*> pure o.address
-- --          <*> pure o.phones

-- type Errors = Array String

-- nonEmpty :: String -> String -> V Errors Unit
-- nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
-- nonEmpty _     _  = pure unit

-- lengthIs :: String -> Int -> String -> V Errors Unit
-- lengthIs field len value | length value /= len =
--   invalid ["Field '" <> field <> "' must have length " <> show len]
-- lengthIs _     _   _     =
--   pure unit

-- validateAddress :: Address -> V Errors Address
-- validateAddress (Address o) =
--   address <$> (nonEmpty "Street" o.street *> pure o.street)
--           <*> (nonEmpty "City"   o.city   *> pure o.city)
--           <*> (lengthIs "State" 2 o.state *> pure o.state)

-- matches :: String -> R.Regex -> String -> V Errors Unit
-- matches _     regex value | R.test regex value =
--   pure unit
-- matches field _     _     =
--   invalid ["Field '" <> field <> "' did not match the required format"]

-- phoneNumberRegex :: R.Regex
-- phoneNumberRegex =
--   unsafePartial
--     case R.regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
--       Right r -> r

-- validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
-- validatePhoneNumber (PhoneNumber o) =
--   phoneNumber <$> pure o."type"
--               <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

-- stateRegex :: R.Regex
-- stateRegex =
--   unsafePartial
--     case R.regex "^[A-Za-z]{2}$" noFlags of
--       Right r -> r

-- validateState :: String -> V Errors String
-- validateState st = matches "State" stateRegex st *> pure st

-- -- TODO: 7.10
-- -- TODO: 7.11


