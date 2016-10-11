module Main where

import App.Effects (AppEffects)
import App.Routes (match)
import App.Layout (Action(PageView), State, view, update)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Prelude (bind, pure)
import Pux (App, CoreEffects, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)
import Signal ((~>))

-- | App configuration
-- config :: forall e.
--           State ->
--           Eff (CoreEffects AppEffects)
--               (Config State Action (ajax :: AJAX | e))
config state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  pure
    { initialState: state
    , update: update
    , view: view
    , inputs: [routeSignal] }

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  c <- config state
  app <- start c

  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  c <- config state
  app <- Pux.Devtool.start c

  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app
