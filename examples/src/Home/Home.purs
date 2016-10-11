module App.Home where

import Pux.Html (Html)
import Pux.Html.Elements (h1, div, ul, li, text)
import Pux.Router (link)

view :: forall action. Html action
view = div []
           [ h1 [] [ text "Pux Examples" ]
           , ul []
                [ li [] [ link "/counter" [] [ text "Ye Olde Counter" ] ]
                , li [] [ link "/game" [] [ text "Rock, Paper, Scissors" ] ]
                , li [] [ link "/github" [] [ text "Github Repository Search" ] ]
                ]
           ]
