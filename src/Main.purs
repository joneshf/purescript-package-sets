module Main (main) where

import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM.Classy.Element (fromElement)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), NonElementParentNode, documentToNonElementParentNode)
import Data.Argonaut.Core (toObject)
import Data.Either (Either(..))
import Data.Foldable (for_, intercalate)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NaturalTransformation (type (~>))
import Data.Semigroup ((<>))
import Data.StrMap (keys)
import Data.Traversable (traverse)
import Data.URI (URI, printURI, runParseURI)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX, get)
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
  json <- _.response <$> get (printURI url)
  let packages = fromMaybe [] $ keys <$> toObject json

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
  = Nope a

type Message = Void

type State = Array String

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
      Nope x -> pure x

    initialState :: forall a. a -> State
    initialState _ = packages'

    receiver :: forall a. a -> Maybe (Query a)
    receiver _ = Nothing

    render :: State -> H.ComponentHTML Query
    render packages =
      HH.pre_
        [ HH.text $ intercalate "\n" packages
        ]
