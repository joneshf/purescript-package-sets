module Raw
  ( Package(..)
  ) where

import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Simple.JSON (class ReadForeign)

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
