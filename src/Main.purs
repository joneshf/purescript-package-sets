module Main (main) where

import Control.Applicative (class Applicative, pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except.Trans (ExceptT, mapExceptT, runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Safely (safely)
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
import Data.Foreign (F, ForeignError(..), MultipleErrors, fail, renderForeignError)
import Data.Function (($))
import Data.Map as Map
import Data.Newtype (un, wrap)
import Data.Semigroup ((<>))
import Data.Traversable (class Traversable, for)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import PackageSet (packageSet)
import PackageSet.Name as Name
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
  db <- liftEff $ SQLJS.new $ asUint8Array $ whole response
  packs <- liftEff $ SQLJS.Typed.queryResult packageSQL db
  result <- runExceptT $ for' packs.values case _ of
    [name', repo', package_set', version'] -> do
      name <- toF' $ read name'
      repo <- toF' $ read repo'
      package_set <- toF' $ read package_set'
      version <- toF' $ read version'
      deps <- liftEff $ SQLJS.Typed.queryResult (dependencySQL { name, package_set}) db
      dependencies <- for' deps.values case _ of
        [dependent'] -> do
          dependent <- toF' $ read dependent'
          pure $ wrap dependent
        _ -> toF' $ fail $ ForeignError "Expected one field"
      let package =
            { dependencies: dependencies
            , name: wrap name
            , repo: wrap repo
            , version: wrap version
            }
      pure $ Tuple (Name.Set package_set) [Package package]
    _ -> toF' $ fail $ ForeignError "expected four fields"
  case result of
    Left errors -> traverse_ (log <<< renderForeignError) errors
    Right packages' -> do
      let packageSets = Map.fromFoldableWith (<>) packages'
      container <- liftEff $ getElementById (ElementId "sets") parent
      for_ (container >>= fromElement) \el ->
        for_ (Map.toUnfoldable packageSets :: Array _) \(Tuple name packages) ->
          runUI (packageSet name (sortWith (_.name <<< un Package) packages)) unit el

dependencySQL :: { name :: String, package_set :: String } -> SQL
dependencySQL { name, package_set } = SQL $
  """
  SELECT dependent
  FROM dependencies
  WHERE independent = '""" <> name <> """'
    AND package_set = '""" <> package_set <> """';
  """

packageSQL :: SQL
packageSQL = SQL
  """
  SELECT name, repo, package_set, version
  FROM package;
  """

sqliteURI :: String
sqliteURI =
  "https://cdn.rawgit.com/joneshf/purescript-package-sets/master/package_sets.sqlite3"

type F' = ExceptT MultipleErrors

toF' :: forall a f. Applicative f => F a -> F' f a
toF' = mapExceptT (pure <<< un wrap)

for' :: forall a b f m. MonadRec m => Traversable f => f a -> (a -> m b) -> m (f b)
for' x f = safely \lift _ -> for x (lift <<< f)
