module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (HTMLDocument, htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), NonElementParentNode)
import Partial.Unsafe (unsafePartial)
import React.DOM as D
import ReactDOM (render)

getMainElement :: forall eff. HTMLDocument -> Eff (dom :: DOM | eff) Element
getMainElement htmlDocument = do
  mainElement <- getElementById elementId nonElementParentNode
  pure $ unsafePartial fromJust $ toMaybe mainElement
  where
  elementId :: ElementId
  elementId = ElementId "main"
  nonElementParentNode :: NonElementParentNode
  nonElementParentNode = htmlDocumentToNonElementParentNode htmlDocument

main :: Eff ( console :: CONSOLE
            , dom :: DOM
            ) Unit
main = void do
  log "Rendering address book component"
  let component = D.div [] [D.text "Hello, World!"]
  w <- window
  d <- document w
  e <- getMainElement d
  render component e
