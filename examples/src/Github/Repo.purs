module App.Types.Repo where

import Data.Maybe (Maybe)
import Data.Argonaut (decodeJson, class DecodeJson, (.?))
import Prelude (bind, pure, ($), (=<<))

newtype RepoSearch =
  RepoSearch { total_count :: Int
             , incomplete_results :: Boolean
             , items :: Repos
             }

instance decodeJsonRepoSearch :: DecodeJson RepoSearch where
  decodeJson json = do
    obj                <- decodeJson json
    items              <- decodeJson =<< obj .? "items"
    total_count        <- obj .? "total_count"
    incomplete_results <- obj .? "incomplete_results"
    pure $ RepoSearch { total_count: total_count, incomplete_results: incomplete_results, items: items }

newtype Repo =
  Repo { name :: String
       , owner :: Owner
       , html_url :: String
       , stargazers_count :: Int
       , open_issues :: Int
       , description :: Maybe String
       }

instance decodeJsonRepo :: DecodeJson Repo where
  decodeJson json = do
    obj              <- decodeJson json
    name             <- obj .? "name"
    owner            <- decodeJson =<< obj .? "owner"
    html_url         <- obj .? "html_url"
    description      <- obj .? "description"
    open_issues      <- obj .? "open_issues"
    stargazers_count <- obj .? "stargazers_count"
    pure $ Repo { name, owner, html_url, stargazers_count, open_issues, description }

type Repos = Array Repo

newtype Owner =
  Owner { login :: String
        , avatar_url :: String
        , html_url :: String
        }

instance decodeJsonOwner :: DecodeJson Owner where
  decodeJson json = do
    obj        <- decodeJson json
    login      <- obj .? "login"
    html_url   <- obj .? "html_url"
    avatar_url <- obj .? "avatar_url"
    pure $ Owner { login, avatar_url, html_url }
