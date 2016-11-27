module C8AddressBook(addressBook) where

import Prelude (($))
import Control.Applicative (pure)
import React (ReactClass, Render, createClass, spec)
import React.DOM as D
import React.DOM.Props as P

import Data.AddressBook (Person, examplePerson)
import Data.AddressBook.Validation (Errors)

newtype AppState = AppState
  { person :: Person
  , errors :: Errors
  }

initialState :: AppState
initialState = AppState
  { person: examplePerson
  , errors: []
  }

-- type Render props state eff =
--   ReactThis props state ->
--   Eff
--     ( props :: ReactProps
--     , refs :: ReactRefs Disallowed
--     , state :: ReactState ReadOnly
--     | eff
--     ) ReactElement
render :: forall props state eff. Render props state eff
render _ctx = do
  pure $
    D.div
      [ P.className "address-book" ]
      [ D.text "AAAAAAAAAA!!!" ]

-- createClass :: forall props state eff.
--   ReactSpec props state eff -> ReactClass props
-- spec :: forall props state eff.
--   state -> Render props state eff -> ReactSpec props state eff

addressBook :: forall props. ReactClass props
addressBook = createClass $ spec state render
  where
    state = initialState
