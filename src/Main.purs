module Main (main) where

import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.State.Class (put)
import Control.Semigroupoid ((<<<))
import DOM.Classy.Element (fromElement)
import DOM.HTML (window)
import DOM.HTML.Indexed.InputType (InputType(..))
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), NonElementParentNode, documentToNonElementParentNode)
import Data.Either (Either(..), either)
import Data.Filterable (filter)
import Data.Foldable (for_, traverse_)
import Data.Function (($))
import Data.Functor ((<#>), (<$>))
import Data.Maybe (Maybe(Nothing))
import Data.NaturalTransformation (type (~>))
import Data.Newtype (class Newtype, un, wrap)
import Data.Semigroup ((<>))
import Data.StrMap (StrMap, foldMap)
import Data.String (contains)
import Data.Traversable (traverse)
import Data.URI (URI, printURI, runParseURI)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import Simple.JSON (class ReadForeign, readJSON)
import Text.Parsing.StringParser (ParseError)

main :: Eff (HA.HalogenEffects (ajax :: AJAX, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  doc <- liftEff $ window >>= document
  let parent = documentToNonElementParentNode $ htmlDocumentToDocument doc
  either logShow (traverse_ $ loadData parent) $ traverse parsePackageSet sets

sets :: Array (PackageSetData String)
sets =
  [ PackageSetData
    { name: SetName "psc"
    , set: "psc-0.11.6"
    }
  , PackageSetData
    { name: SetName "purerl"
    , set: "purerl-0.11.6"
    }
  ]

loadData
  :: forall e
  . NonElementParentNode
  -> PackageSetData URI
  -> Aff (HA.HalogenEffects (ajax :: AJAX | e)) Unit
loadData parent (PackageSetData x ) = do
  container <- liftEff $ getElementById (ElementId "sets") parent
  json <- _.response <$> Affjax.get (printURI x.set)
  let packages = case readJSON json of
        Right (PackageSetPackage y) ->
          foldMap (\name { repo, version } -> [wrap { name, repo, version }]) y
        _ -> []

  for_ (container >>= fromElement) $ runUI (packageSet x.name packages) unit

parsePackageSet :: PackageSetData String -> Either ParseError (PackageSetData URI)
parsePackageSet (PackageSetData {name, set}) =
  runParseURI url <#> PackageSetData <<< {name, set: _}
  where
  url =
    "https://cdn.rawgit.com/joneshf/purescript-package-sets/"
      <> set
      <> "/packages.json"

newtype PackageSetData set
  = PackageSetData
    { name :: SetName
    , set :: set
    }

newtype SetName
  = SetName String

newtype SetVersion
  = SetVersion String

data Query a
  = Search String a

search :: String -> Query Unit
search str = Search str unit

type Message = Void

type State = Array Package

newtype Package
  = Package
    { name :: String
    , repo :: String
    , version :: String
    }

derive instance newtypePackage :: Newtype Package _

newtype PackageSetPackage
  = PackageSetPackage
    (StrMap
      { dependencies :: Array String
      , repo :: String
      , version :: String
      }
    )

derive instance newtypePackageSetPackage :: Newtype PackageSetPackage _
derive newtype instance readForeignPackageSetPackage
  :: ReadForeign PackageSetPackage

packageSet :: forall m. SetName -> State -> H.Component HH.HTML Query Unit Message m
packageSet (SetName setName) packages' =
  H.component
    { eval
    , initialState
    , receiver
    , render
    }
    where
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Search str x -> do
        put $ filter (contains (wrap str) <<< _.name <<< un Package) packages'
        pure x

    initialState :: forall a. a -> State
    initialState _ = packages'

    receiver :: forall a. a -> Maybe (Query a)
    receiver _ = Nothing

    render :: State -> H.ComponentHTML Query
    render packages =
      HH.article_
        [ HH.h2
          [ HP.class_ $ wrap "set" ]
          [ HH.text setName ]
        , HH.input
          [ HP.class_ $ wrap "search"
          , HE.onValueInput (pure <<< search)
          , HP.placeholder "Search"
          , HP.type_ InputText
          ]
        , HH.table
          [ HP.class_ $ wrap "packages" ]
          [ HH.thead_
            [ HH.tr
              [ HP.classes [ wrap "package", wrap "package-header" ] ]
              [ HH.th
                [ HP.class_ $ wrap "package-name" ]
                [ HH.text "name" ]
              , HH.th
                [ HP.class_ $ wrap "package-version" ]
                [ HH.text "version" ]
              ]
            ]
          , HH.tbody_
            (packageRow <$> packages)
          ]
        ]

    packageRow :: Package -> H.ComponentHTML Query
    packageRow (Package { name, repo, version }) =
      HH.tr
        [ HP.class_ $ wrap "package" ]
        [ HH.td
          [ HP.class_ $ wrap "package-name" ]
          [ HH.a
            [ HP.href repo ]
            [ HH.text name ]
          ]
        , HH.td
          [ HP.class_ $ wrap "package-version" ]
          [ HH.text version ]
        ]
