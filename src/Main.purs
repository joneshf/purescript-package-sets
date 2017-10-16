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
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array)
import Data.Either (Either(Right, Left))
import Data.Foldable (for_, traverse_)
import Data.Foreign (ForeignError(..), fail, renderForeignError)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Map as Map
import Data.Newtype (wrap)
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
import SQLJS (SQLJS)
import SQLJS as SQLJS
import Simple.JSON (read)

main :: Eff (HA.HalogenEffects (ajax :: AJAX, console :: CONSOLE, sqljs :: SQLJS)) Unit
main = HA.runHalogenAff do
  doc <- liftEff $ window >>= document
  let parent = documentToNonElementParentNode $ htmlDocumentToDocument doc
  { response } <- Affjax.get sqliteURI
  result <- liftEff' do
    db <- SQLJS.new $ asUint8Array $ whole response
    results <- SQLJS.exec "SELECT name, repo, package_set, version FROM package;" db
    pure $ runExcept case results of
      [{ values }] ->
        for values case _ of
          [name', repo', set', version'] -> do
            let dependencies = []
            name <- wrap <$> read name'
            repo <- wrap <$> read repo'
            set <- wrap <$> read set'
            version <- wrap <$> read version'
            pure $ Tuple set [Package { dependencies, name, repo, version }]
          _ -> fail $ ForeignError "expected four fields"
      _ -> fail $ ForeignError "expected one result"
  case result of
    Left error -> logShow error
    Right (Left errors) -> traverse_ (log <<< renderForeignError) errors
    Right (Right packages') -> do
      let packageSets = Map.fromFoldableWith (<>) packages'
      container <- liftEff $ getElementById (ElementId "sets") parent
      for_ (container >>= fromElement) \el ->
        for_ (Map.toUnfoldable packageSets :: Array _) \(Tuple name packages) ->
          runUI (packageSet name packages) unit el

sqliteURI :: String
sqliteURI =
  "https://cdn.rawgit.com/joneshf/purescript-package-sets/package_sets.sqlite3"
