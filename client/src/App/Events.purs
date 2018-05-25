module App.Events where

import App.Routes (Route)
import App.State (State(..))
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Data.Function (($))
import Data.Functor (($>))
import Data.Maybe (Maybe(Nothing))
import Network.HTTP.Affjax (AJAX, get)
import Prelude (bind, discard, (<>), pure)
import Pux (EffModel, noEffects, onlyEffects)

data Event = PageView Route
           | Host
           | Join

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

foldp :: forall fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp e (State st) = case e of
  PageView route -> noEffects $ State st { route = route, loaded = true }
  Host           -> echoServer "http://localhost:8000/host"
  Join           -> echoServer "http://localhost:8000/join"
  where echoServer url =  { state: State st
                          , effects: [do res <- get url
                                         log ("Server says: " <> res.response)
                                         pure Nothing]
                          }
