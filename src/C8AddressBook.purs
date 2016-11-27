module C8AddressBook(addressBook) where

import Prelude (($), (<>))
import Control.Monad (bind, pure)
import React (ReactClass, Render, createClass, readState, spec)
import React.DOM as D
import React.DOM.Props as P

import Data.AddressBook (Address(..), Person(..), examplePerson)
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
render :: forall props eff. Render props AppState eff
render ctx = do
  AppState { person: Person person@{ homeAddress: Address address }
           , errors
           } <- readState ctx
  pure $
    D.div [ P.className "container" ]
          [ D.div [ P.className "row" ]
                  [] -- TODO
          , D.div [ P.className "row" ]
                  [ D.form [ P.className "form-horizontal" ]
                           [ D.h3' [ D.text "Basic Information" ]
                           , D.text ("First Name:" <> person.firstName) -- TODO
                           , D.text ("Last Name:" <> person.lastName) -- TODO
                           , D.h3' [ D.text "Address" ]
                           , D.text ("Street:" <> address.street) -- TODO
                           , D.text ("City:" <> address.city) -- TODO
                           , D.text ("State" <> address.state) -- TODO
                           , D.h3' [ D.text "Contact Information" ]
                           ]
                  ]
          ]


-- createClass :: forall props state eff.
--   ReactSpec props state eff -> ReactClass props
-- spec :: forall props state eff.
--   state -> Render props state eff -> ReactSpec props state eff

addressBook :: forall props. ReactClass props
addressBook = createClass $ spec state render
  where
    state = initialState
