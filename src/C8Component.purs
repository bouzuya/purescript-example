module C8Component(component) where

import Prelude (unit)
import React (ReactElement, createElement)
import React.DOM as D
import React.DOM.Props as P
import C8AddressBook (addressBook)

component :: ReactElement
component =
  D.div
    [ P.className "main" ]
    [ D.text "Hello, World!"
    , createElement addressBook unit []
    ]

