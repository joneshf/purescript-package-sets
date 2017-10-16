module Main (main) where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import DOM.Classy.Element (fromElement)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(ElementId), documentToNonElementParentNode)
import Data.Array (sortWith)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array)
import Data.Either (Either(Right, Left))
import Data.Foldable (for_, traverse_)
import Data.Foreign (ForeignError(..), fail, renderForeignError)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Map as Map
import Data.Newtype (un, wrap)
import Data.Semigroup ((<>))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import PackageSet (packageSet)
import PackageSet.Package (Package(..))
import SQLJS as SQLJS
import SQLJS.Typed (SQL(..))
import SQLJS.Typed as SQLJS.Typed
import Simple.JSON (read)

main :: Eff (HA.HalogenEffects (SQLJS.Effects (ajax :: AJAX, console :: CONSOLE))) Unit
main = HA.runHalogenAff do
  doc <- liftEff $ window >>= document
  let parent = documentToNonElementParentNode $ htmlDocumentToDocument doc
  { response } <- Affjax.get sqliteURI
  result <- liftEff' do
    db <- SQLJS.new $ asUint8Array $ whole response
    { values } <- SQLJS.Typed.queryResult packageSQL db
    pure $ runExcept $ for values case _ of
      [name', repo', set', version'] -> do
        let dependencies = []
        name <- wrap <$> read name'
        repo <- wrap <$> read repo'
        set <- wrap <$> read set'
        version <- wrap <$> read version'
        pure $ Tuple set [Package { dependencies, name, repo, version }]
      _ -> fail $ ForeignError "expected four fields"
  case result of
    Left error -> logShow error
    Right (Left errors) -> traverse_ (log <<< renderForeignError) errors
    Right (Right packages') -> do
      let packageSets = Map.fromFoldableWith (<>) packages'
      container <- liftEff $ getElementById (ElementId "sets") parent
      for_ (container >>= fromElement) \el ->
        for_ (Map.toUnfoldable packageSets :: Array _) \(Tuple name packages) ->
          runUI (packageSet name (sortWith (_.name <<< un Package) packages)) unit el

packageSQL :: SQL
packageSQL = SQL
  """
  SELECT name, repo, package_set, version
  FROM package;
  """

sqliteURI :: String
sqliteURI =
  "https://cdn.rawgit.com/joneshf/purescript-package-sets/master/package_sets.sqlite3"
