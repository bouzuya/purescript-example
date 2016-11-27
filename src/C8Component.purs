module C8Component(component) where

import React (ReactElement)
import React.DOM as D
import React.DOM.Props as P

component :: ReactElement
component =
  D.div
    [ P.className "main" ]
    [ D.text "Hello, World!" ]

