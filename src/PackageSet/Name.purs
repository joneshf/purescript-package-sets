module PackageSet.Name
  ( Name(..)
  , renderName
  ) where

import Data.Function (($))
import Data.Newtype (wrap)
import Halogen.Component as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

newtype Name
  = Name String

renderName :: forall f. Name -> H.ComponentHTML f
renderName (Name setName) =
  HH.h2
    [ HP.class_ $ wrap "set" ]
    [ HH.text setName ]
