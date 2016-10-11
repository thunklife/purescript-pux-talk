module App.Types where

import DOM (DOM)
import Control.Monad.Eff.Random (RANDOM)
import Network.HTTP.Affjax (AJAX)

type AppEffects = (dom :: DOM, random :: RANDOM, ajax :: AJAX)
