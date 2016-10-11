module App.Layout where

import App.Counter as Counter
import App.Home as Home
import App.RockPaperScissors as RPS
import App.Github as GH
import App.Routes (Route(Home, Counter, Github, Game))
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude (($), (#), map)
import Pux (mapEffects, mapState, noEffects, EffModel)
import Pux.Html (Html, div)
import Pux.Html.Attributes (className)

data Action
  = ChildCounter Counter.Action
  | ChildGame RPS.Action
  | ChildGithub GH.Action
  | PageView Route

type State =
  { route :: Route
  , count :: Counter.State
  , game :: RPS.State
  , github :: GH.State
  }

init :: State
init =
  { route: Home
  , count: Counter.init
  , game: RPS.init
  , github: GH.init
  }

update :: forall eff. Action -> State -> EffModel State Action (dom :: DOM, random :: RANDOM, ajax :: AJAX | eff)
update (PageView route) state = noEffects $ state { route = route }
update (ChildCounter action) state = noEffects $ state { count = Counter.update action state.count }
update (ChildGame action) state = RPS.update action state.game
  # mapState (state { game = _ })
  # mapEffects ChildGame
update (ChildGithub action) state = GH.update action state.github
  # mapState (state { github = _ })
  # mapEffects ChildGithub

view :: State -> Html Action
view state =
  div
    [ className "container" ]
    [ case state.route of
        Home -> Home.view
        Counter -> map ChildCounter $ Counter.view state.count
        Github -> map ChildGithub $ GH.view state.github
        Game -> map ChildGame $ RPS.view state.game
    ]
