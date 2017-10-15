module PackageSet
  ( Data(..)
  , Name(..)
  , Query(..)
  , packageSet
  , parseSetURI
  ) where

import Control.Applicative (pure)
import Control.Bind (discard)
import Control.Monad.State.Class (put)
import Control.Semigroupoid ((<<<))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Either (Either)
import Data.Filterable (filter)
import Data.Function (($))
import Data.Functor (class Functor, (<#>))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Newtype (un, wrap)
import Data.Semigroup ((<>))
import Data.String (contains)
import Data.URI (runParseURI)
import Data.URI.Types (URI)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen.Component as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Package (Package(..), renderResults)
import Text.Parsing.StringParser (ParseError)

newtype Data set
  = Data
    { name :: Name
    , set :: set
    }

derive instance functorData :: Functor Data

parseSetURI :: Data String -> Either ParseError (Data URI)
parseSetURI (Data {name, set}) =
  runParseURI url <#> Data <<< {name, set: _}
  where
  url =
    "https://cdn.rawgit.com/joneshf/purescript-package-sets/"
      <> set
      <> "/packages.json"

newtype Name
  = Name String

newtype Version
  = Version String

renderName :: forall f. Name -> H.ComponentHTML f
renderName (Name setName) =
  HH.h2
    [ HP.class_ $ wrap "set" ]
    [ HH.text setName ]

data Query a
  = Search String a

search :: String -> Query Unit
search str = Search str unit

type Message = Void

type State = Array Package

packageSet :: forall m. Name -> State -> H.Component HH.HTML Query Unit Message m
packageSet setName packages' =
  H.component { eval, initialState, receiver, render }
  where
  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Search str x -> do
      put $ filter (contains (wrap str) <<< _.name <<< un Package) packages'
      pure x

  initialState :: forall a. a -> State
  initialState _ = packages'

  receiver :: forall a f. a -> Maybe (f a)
  receiver _ = Nothing

  render :: State -> H.ComponentHTML Query
  render packages =
    HH.article_
      [ renderName setName
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
