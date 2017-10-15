module PackageSet.Version
  ( Version(..)
  , render
  ) where


import Data.Newtype (class Newtype)
import Halogen as H
import Halogen.HTML as HH

newtype Version
  = Version String

derive instance newtypeVersion :: Newtype Version  _

render :: forall f. Version -> H.ComponentHTML f
render (Version version) = HH.text version
