module App.Events where

import App.Routes (Route)
import App.State (State(..))
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Data.Function (($))
import Data.Functor (($>))
import Data.Maybe (Maybe(Nothing))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)

data Event = PageView Route
           | Host
           | Join

type AppEffects fx = (console :: CONSOLE | fx)

foldp :: forall fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
foldp Host             (State st) = { state: State st
                                    , effects: [log "hosted" $> Nothing]
                                    }
foldp Join             (State st) = { state: State st
                                    , effects: [log "joined" $> Nothing]
                                    }