module App.Routes where

import Control.Alternative ((<|>))
import Control.Applicative ((<*))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Prelude (($))
import Pux.Router (lit, end, router)

data Route = Home | Counter | Github | Game

match :: String -> Route
match url = fromMaybe Home $ router url $
  Home <$ end
  <|>
  Counter <$ (lit "counter") <* end
  <|>
  Github <$ (lit "github") <* end
  <|>
  Game <$ (lit "game") <* end
