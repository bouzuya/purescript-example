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
import ReactDOM (render)

import C8Component (component)

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
  w <- window
  d <- document w
  e <- getMainElement d
  render component e
