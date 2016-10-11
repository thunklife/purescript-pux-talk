module App.Github where
import App.Types.Repo (RepoSearch(RepoSearch), Repo(Repo), Owner(Owner))
import Control.Monad.Aff (attempt, Aff)
import Data.Argonaut (decodeJson)
import Data.Either (either, Either(Left, Right))
import Data.Maybe (fromMaybe, Maybe(Just, Nothing))
import Network.HTTP.Affjax (get, AJAX)
import Prelude (bind, const, pure, show, map, ($), (<<<), (<>))
import Pux (noEffects, EffModel)
import Pux.Html (Html)
import Pux.Html.Attributes (name, className, type_, value, href, src)
import Pux.Html.Elements (small, h2, button, i, div, img, a, form, h1, span, text, p, input)
import Pux.Html.Events (onSubmit, FormEvent, onChange)
import Pux.Router (link)

type State =
  { searchTerm :: String
  , fetching :: Boolean
  , results :: Maybe RepoSearch
  , message :: String
  }


init :: State
init = { searchTerm: "", fetching: false, results: Nothing, message: "" }

data Action
  = SubmitSearch
  | SearchTermChange FormEvent
  | ReceiveSearchResults (Either String RepoSearch)

update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff )
update (SearchTermChange e) state = noEffects $ state { searchTerm = e.target.value }
update SubmitSearch state =
  { state: state { fetching = true }
  , effects: [ runSearch state.searchTerm ]
  }
update (ReceiveSearchResults (Left s)) state = noEffects $ state { message = s }
update (ReceiveSearchResults (Right results)) state = noEffects $ state { results = Just results }

runSearch :: forall eff. String -> Aff (ajax :: AJAX | eff) Action
runSearch term = do
  res <- attempt $ get $ "https://api.github.com/search/repositories?per_page=25&q=" <> term
  let decode r = decodeJson r.response :: Either String RepoSearch
  let repos = either (Left <<< show) decode res
  pure $ ReceiveSearchResults repos

view :: State -> Html Action
view state = div []
                 [ div [ className "search-container" ]
                       [ h1 [ className "page-title"]
                            [ text "Github Repo Search"
                            , small []
                                    [ link "/" [] [ text "Home" ] ]
                            ]
                       , form [ name "Github Search"
                              , onSubmit (const SubmitSearch)
                              , className "search-form"
                              ]
                              [ input
                                [ type_ "text"
                                , onChange SearchTermChange
                                , value state.searchTerm
                                ] []
                              , button [] [ text "Search" ]
                              ]
                       ]
                 , renderResults state.results
                 ]

renderResults :: forall action. Maybe RepoSearch -> Html action
renderResults Nothing = span [] []
renderResults (Just (RepoSearch repos)) = div [ className "search-results"]
                                              $ map renderSingleResult repos.items


renderSingleResult :: forall action. Repo -> Html action
renderSingleResult (Repo repo) = div [ className "repo-card" ]
                                     [ h2 [ className "repo-name" ]
                                          [ a [ href repo.html_url ]
                                              [ text repo.name ]
                                          ]

                                     , (ownerDetails repo.owner)
                                     , (gazers repo.stargazers_count)
                                     , (issues repo.open_issues)
                                     , (description repo.description)
                                     ]
ownerDetails :: forall action. Owner -> Html action
ownerDetails (Owner owner) = div [ className "repo-owner" ]
                             [ img [ src owner.avatar_url ] []
                             , a [ href owner.html_url ] [ text owner.login ]
                             ]

gazers :: forall action. Int -> Html action
gazers n = div [ className "star-gazers" ]
               [ i [ className "fa fa-star"]
                   []
               , text $ show n
               ]

issues :: forall action. Int -> Html action
issues n = div [ className "open-issues" ]
               [ i [ className "fa fa-exclamation-circle" ]
                   []
               ,   text $ show n
               ]
description :: forall action. Maybe String -> Html action
description desc = p [ className "repo-description"  ]
                     [ text $ fromMaybe "No Description" desc ]
