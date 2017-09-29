module Main (main) where

import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
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
import Data.Foldable (for_, intercalate)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing))
import Data.NaturalTransformation (type (~>))
import Data.Newtype (class Newtype, un, wrap)
import Data.Semigroup ((<>))
import Data.StrMap (StrMap, keys)
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

  case traverse packageSetURL ["psc-0.11.6", "purerl-0.11.6"] of
    Right [pscURI, purerlURI] -> do
      loadData parent pscURI (ElementId "psc-container")
      loadData parent purerlURI (ElementId "purerl-container")
    _ -> log "whoops"

loadData
  :: forall e
  . NonElementParentNode
  -> URI
  -> ElementId
  -> Aff (HA.HalogenEffects (ajax :: AJAX | e)) Unit
loadData parent url ident = do
  container <- liftEff $ getElementById ident parent
  json <- _.response <$> Affjax.get (printURI url)
  let packages = either (const []) (keys <<< un PackageSetPackage) $ readJSON json

  for_ (container >>= fromElement) $ runUI (packageSet packages) unit

packageSetURL :: String -> Either ParseError URI
packageSetURL set =
  runParseURI url
  where
  url =
    "https://cdn.rawgit.com/joneshf/purescript-package-sets/"
      <> set
      <> "/packages.json"

data Query a
  = Search String a

search :: String -> Query Unit
search str = Search str unit

type Message = Void

type State = Array String

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

packageSet :: forall m. State -> H.Component HH.HTML Query Unit Message m
packageSet packages' =
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
        put $ filter (contains $ wrap str) packages'
        pure x

    initialState :: forall a. a -> State
    initialState _ = packages'

    receiver :: forall a. a -> Maybe (Query a)
    receiver _ = Nothing

    render :: State -> H.ComponentHTML Query
    render packages =
      HH.div_
        [ HH.input
          [ HE.onValueInput (pure <<< search)
          , HP.type_ InputText
          ]
        , HH.pre_ [HH.text $ intercalate "\n" packages]
        ]
