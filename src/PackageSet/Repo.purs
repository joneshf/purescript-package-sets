module PackageSet.Repo
  ( Repo(..)
  , href
  ) where


import Data.Newtype (class Newtype)
import Halogen.HTML.Properties as HP

newtype Repo
  = Repo String

derive instance newtypeRepo :: Newtype Repo  _

href :: forall i r. Repo -> HP.IProp ( href :: String | r ) i
href (Repo repo) = HP.href repo
