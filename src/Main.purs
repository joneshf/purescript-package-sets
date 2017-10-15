module Main (main) where

import Control.Applicative (class Applicative, pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Semigroupoid ((<<<))
import DOM.Classy.Element (fromElement)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), NonElementParentNode, documentToNonElementParentNode)
import Data.Either (Either, either)
import Data.Foldable (foldMap, for_, traverse_)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Newtype (wrap)
import Data.Record as Record
import Data.Semigroup ((<>))
import Data.StrMap as StrMap
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.URI (URI, printURI, runParseURI)
import Data.Unit (Unit, unit)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import PackageSet (packageSet)
import PackageSet.Name as Name
import PackageSet.Package (Package)
import Raw as Raw
import Simple.JSON (readJSON)
import Text.Parsing.StringParser (ParseError)

main :: Eff (HA.HalogenEffects (ajax :: AJAX, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  doc <- liftEff $ window >>= document
  let parent = documentToNonElementParentNode $ htmlDocumentToDocument doc
  either logShow (traverse_ $ loadData parent) $ traverse (traverse parseSetURI) sets

sets :: Array (Raw.Data String)
sets =
  [ Raw.Data
    { name: "psc"
    , set: "psc-0.11.6"
    }
  , Raw.Data
    { name: "purerl"
    , set: "purerl-0.11.6"
    }
  ]

loadData
  :: forall e
  . NonElementParentNode
  -> Raw.Data URI
  -> Aff (HA.HalogenEffects (ajax :: AJAX | e)) Unit
loadData parent (Raw.Data x) = do
  container <- liftEff $ getElementById (ElementId "sets") parent
  json <- _.response <$> Affjax.get (printURI x.set)
  let packages = foldMap (StrMap.foldMap convert) $ readJSON json

  for_ (container >>= fromElement) $ runUI (packageSet (Name.Set x.name) packages) unit

convert :: forall f. Applicative f => String -> Raw.Package -> f Package
convert name =
  pure
    <<< wrap
    <<< Record.modify (SProxy :: SProxy "version") wrap
    <<< Record.modify (SProxy :: SProxy "repo") wrap
    <<< Record.modify (SProxy :: SProxy "dependencies") (map wrap)
    <<< Record.modify (SProxy :: SProxy "name") wrap
    <<< Record.insert (SProxy :: SProxy "name") name

parseSetURI :: String -> Either ParseError URI
parseSetURI set = runParseURI url
  where
  url =
    "https://cdn.rawgit.com/joneshf/purescript-package-sets/"
      <> set
      <> "/packages.json"
