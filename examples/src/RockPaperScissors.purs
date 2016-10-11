module App.RockPaperScissors where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (randomInt, RANDOM)
import Data.Function (const)
import Data.Show (class Show)
import Prelude (($), (>>=), (<<<), pure, bind, show)
import Pux (noEffects, EffModel)
import Pux.Html (Html, div, h1, span, p, button, text, i)
import Pux.Html.Attributes (className)
import Pux.Html.Elements (small)
import Pux.Html.Events (onClick)
import Pux.Router (link)

data Hand = Rock | Paper | Scissors | Empty

instance showHand :: Show Hand  where
  show Rock = "Rock"
  show Paper = "Paper"
  show Scissors = "Scissors"
  show Empty = ""

data Result = P1Wins | P2Wins | Draw | NotStarted

instance showResult :: Show Result where
  show P1Wins = "Player One Wins!"
  show P2Wins = "Aw. You lose."
  show Draw = "Tis a draw. Play Again."
  show NotStarted = ""

type State =
  { p1Hand :: Hand
  , p2Hand :: Hand
  , result :: Result
  }

init :: State
init =
  { p1Hand: Empty
  , p2Hand: Empty
  , result: NotStarted
  }

data Action = Throw Hand | ReceiveRandom Hand | Reset

update :: forall eff. Action -> State -> EffModel State Action (random :: RANDOM | eff)
update (Throw h) state =
  { state: state { p1Hand = h }
  , effects: [ do
      p2Hand <- liftEff randomHand
      pure $ ReceiveRandom p2Hand
    ]
  }
update (ReceiveRandom h) state = noEffects $ state { result = declare state.p1Hand  h, p2Hand =  h }
update Reset _ = noEffects $ init

randomHand :: forall eff. Eff (random :: RANDOM | eff) Hand
randomHand = do
  n <- randomInt 1 3
  pure $ case n of
    1 -> Rock
    2 -> Paper
    _ -> Scissors

declare :: Hand -> Hand -> Result
declare Rock Paper = P2Wins
declare Rock Scissors = P1Wins
declare Paper Rock = P1Wins
declare Paper Scissors = P2Wins
declare Scissors Paper = P1Wins
declare Scissors Rock = P2Wins
declare Empty Empty = NotStarted
declare _ _ = Draw

view :: State -> Html Action
view state = div [ ]
                 [ h1 [ className "page-title" ]
                      [ text "Rock Paper Scissors"
                      , small []
                              [ link "/" [] [ text "Home" ] ]
                      ]
                 , div [ className "rps-container" ]
                       [ p [] [ text "Choose wisely: " ]
                       , button [ className "hand-button", onClick $ const $ Throw Rock ]
                                [ i [ className $ cssClass Rock ] [] ]
                       , button [ className "hand-button", onClick $ const $ Throw Paper ]
                                [ i [ className $ cssClass Paper ] [] ]
                       , button [ className "hand-button", onClick $ const $ Throw Scissors ]
                                [ i [ className $ cssClass Scissors ] [] ]
                       ]
                 , handsContainer state.p1Hand state.p2Hand
                 , resultContainer state.result
                 ]

handsContainer :: forall action. Hand -> Hand -> Html action
handsContainer Empty Empty = span [] []
handsContainer p1 Empty = div [ ] [ hand p1 ]
handsContainer p1 p2 = div [ className "hands-container" ]
                           [ hand p1
                           , span [] [ text "vs" ]
                           , hand p2
                           ]

hand :: forall action. Hand -> Html action
hand h = span [ className "hand" ] [ i [ className $ cssClass h] [] ]

cssClass :: Hand -> String
cssClass Rock = "fa fa-hand-rock-o"
cssClass Paper = "fa fa-hand-paper-o"
cssClass Scissors = "fa fa-hand-scissors-o"
cssClass _ = ""

resultContainer :: forall action. Result -> Html action
resultContainer NotStarted = span [] []
resultContainer res = div [ className "result-container" ]
                          [ span []
                                 [ text $ show res ]
                          ]
