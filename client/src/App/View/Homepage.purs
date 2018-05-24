module App.View.Homepage where

import App.Events (Event(..))
import App.State (State)
import Control.Bind (discard)
import Data.Function (($), const)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h1, h2)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!), (#!), text)

view :: State -> HTML Event
view s =
  div do
    h1 $ text "Cube Draft"
    a ! className "host"
      #! onClick (const Host)
        $ h2 $ text "Host"
    a ! className "join"
      #! onClick (const Join)
        $ h2 $ text "Join"
