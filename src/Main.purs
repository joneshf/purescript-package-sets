module Main (main) where

import Control.Bind (bind, (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM.Classy.Element (fromElement)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), NonElementParentNode, documentToNonElementParentNode)
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Newtype (class Newtype, wrap)
import Data.Record as Record
import Data.StrMap (foldMap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.URI (URI, printURI)
import Data.Unit (Unit, unit)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import PackageSet (packageSet)
import PackageSet.Name as Name
import PackageSet.Repo (Repo)
import PackageSet.Version (Version)
import Raw as Raw
import Simple.JSON (readJSON)
import Type.Row (class RowLacks)

main :: Eff (HA.HalogenEffects (ajax :: AJAX, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  doc <- liftEff $ window >>= document
  let parent = documentToNonElementParentNode $ htmlDocumentToDocument doc
  either logShow (traverse_ $ loadData parent) $ traverse Raw.parseSetURI sets

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
  let packages = case readJSON json of
        Right (Raw.Package y) ->
          foldMap (\name package -> [insertName name $ wrapRepo $ wrapVersion package]) y
        _ -> []

  for_ (container >>= fromElement) $ runUI (packageSet (Name.Set x.name) packages) unit

insertName
  :: forall a b r t
  . Newtype b a
  => Newtype t { name :: b | r }
  => RowLacks "name" r
  => a
  -> { | r }
  -> t
insertName name package =
  wrap $ Record.insert (SProxy :: SProxy "name") (wrap name) package

wrapRepo :: forall r. { repo :: String | r } -> { repo :: Repo | r }
wrapRepo = Record.modify (SProxy :: SProxy "repo") wrap

wrapVersion :: forall r. { version :: String | r } -> { version :: Version | r }
wrapVersion = Record.modify (SProxy :: SProxy "version") wrap
