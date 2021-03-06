module PackageSet
  ( Query(..)
  , packageSet
  ) where

import Control.Applicative (pure)
import Control.Bind (discard)
import Control.Monad.State.Class (put)
import Control.Semigroupoid ((<<<))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Filterable (filter)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Newtype (un, wrap)
import Data.String (contains)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen.Component as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PackageSet.Name as Name
import PackageSet.Package (Package(..), renderResults)

data Query a
  = Search String a

search :: String -> Query Unit
search str = Search str unit

type Message = Void

type State = Array Package

packageSet :: forall m. Name.Set -> State -> H.Component HH.HTML Query Unit Message m
packageSet setName packages' =
  H.component { eval, initialState, receiver, render }
  where
  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Search str x -> do
      put $ filter (contains (wrap str) <<< un Name.Package <<< _.name <<< un Package) packages'
      pure x

  initialState :: forall a. a -> State
  initialState _ = packages'

  receiver :: forall a f. a -> Maybe (f a)
  receiver _ = Nothing

  render :: State -> H.ComponentHTML Query
  render packages =
    HH.article_
      [ Name.renderSet setName
      , renderSearch
      , renderResults packages
      ]

renderSearch :: H.ComponentHTML Query
renderSearch =
  HH.input
    [ HP.class_ $ wrap "search"
    , HE.onValueInput (pure <<< search)
    , HP.placeholder "Search"
    , HP.type_ InputText
    ]
