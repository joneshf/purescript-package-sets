module Raw
  ( Data(..)
  , Package(..)
  , parseSetURI
  ) where

import Control.Semigroupoid ((<<<))
import Data.Either (Either)
import Data.Functor (class Functor, (<#>))
import Data.Semigroup ((<>))
import Data.URI (runParseURI)
import Data.URI.Types (URI)
import Text.Parsing.StringParser (ParseError)

type Package =
  { dependencies :: Array String
  , repo :: String
  , version :: String
  }

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
