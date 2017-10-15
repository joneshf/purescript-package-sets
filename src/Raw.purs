module Raw
  ( Data(..)
  , Package(..)
  , parseSetURI
  ) where

import Control.Semigroupoid ((<<<))
import Data.Either (Either)
import Data.Functor (class Functor, (<#>))
import Data.Newtype (class Newtype)
import Data.Semigroup ((<>))
import Data.StrMap (StrMap)
import Data.URI (runParseURI)
import Data.URI.Types (URI)
import Simple.JSON (class ReadForeign)
import Text.Parsing.StringParser (ParseError)

newtype Package
  = Package
    (StrMap
      { dependencies :: Array String
      , repo :: String
      , version :: String
      }
    )

derive instance newtypePackage :: Newtype Package _
derive newtype instance readForeignPackage :: ReadForeign Package

newtype Data set
  = Data
    { name :: String
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
