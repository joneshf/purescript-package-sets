module Raw
  ( Data(..)
  , Package(..)
  ) where

import Control.Semigroupoid ((<<<))
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Functor (class Functor, (<$>))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable, sequenceDefault)

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

derive instance newtypeData :: Newtype (Data a) _

instance foldableData :: Foldable Data where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f = f <<< _.set <<< unwrap

instance traversableData :: Traversable Data where
  traverse f (Data x) = Data <<< x { set = _ } <$> f x.set
  sequence = sequenceDefault
