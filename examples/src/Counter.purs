module App.Counter where

import Prelude ((+), (-), const, show)
import Pux.Html (Html, h1, div, span, small, button, text)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
import Pux.Router (link)

data Action = Increment | Decrement

type State = Int

init :: State
init = 0

update :: Action -> State -> State
update Increment state = state + 1
update Decrement state = state - 1

view :: State -> Html Action
view state =
  div
    []
    [ h1 [ className "page-title" ] [ text "Counter"
            , small []
                    [ link "/" [] [ text "Home" ] ]
            ]
    , button [ onClick (const Increment) ] [ text "Increment" ]
    , span [] [ text (show state) ]
    , button [ onClick (const Decrement) ] [ text "Decrement" ]
    ]
