module C8AddressBook(addressBook) where

import Prelude (($), (<>), (<$>))
import Control.Monad (bind, pure)
import React (ReactClass, ReactElement, Render, createClass, readState, spec)
import React.DOM as D
import React.DOM.Props as P

import Data.AddressBook
  ( Address(..)
  , Person(..)
  , PhoneNumber(..)
  , examplePerson
  )
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

formField :: String -> String -> ReactElement
formField name value =
  D.div [ P.className "form-group" ]
        [ D.label [ P.className "col-sm-2 control-label" ]
                  [ D.text name ]
        , D.div [ P.className "col-sm-3" ]
                [ D.input [ P._type "text"
                          , P.className "form-control"
                          , P.placeholder name
                          , P.value value
                          -- TODO
                          ] []
                ]
        ]

phoneField :: PhoneNumber -> ReactElement
phoneField (PhoneNumber { number }) = formField "Phone:" number

form :: Person -> ReactElement
form (Person person@{ homeAddress: Address address }) =
  D.form [ P.className "form-horizontal" ] $
         [ D.h3' [ D.text "Basic Information" ]
         , formField "First Name" person.firstName
         , formField "Last Name:" person.lastName
         , D.h3' [ D.text "Address" ]
         , formField "Street:" address.street
         , formField "City:" address.city
         , formField "State:" address.state
         , D.h3' [ D.text "Contact Information" ]
         ]
         <> (phoneField <$> person.phones)

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
  AppState { person, errors } <- readState ctx
  pure $
    D.div [ P.className "container" ]
          [ D.div [ P.className "row" ]
                  [] -- TODO
          , D.div [ P.className "row" ]
                  [ form person ]
          ]

-- createClass :: forall props state eff.
--   ReactSpec props state eff -> ReactClass props
-- spec :: forall props state eff.
--   state -> Render props state eff -> ReactSpec props state eff

addressBook :: forall props. ReactClass props
addressBook = createClass $ spec state render
  where
    state = initialState
