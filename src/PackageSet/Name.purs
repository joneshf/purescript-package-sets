module PackageSet.Name
  ( Package(..)
  , renderPackage
  , Set(..)
  , renderSet
  ) where

import Data.Function (($))
import Data.Newtype (class Newtype, wrap)
import Halogen.Component as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

newtype Package
  = Package String

derive instance newtypePackage :: Newtype Package _

renderPackage :: forall f. Package -> H.ComponentHTML f
renderPackage (Package name) = HH.text name

newtype Set
  = Set String

derive instance newtypeSet :: Newtype Set _

renderSet :: forall f. Set -> H.ComponentHTML f
renderSet (Set name) =
  HH.h2
    [ HP.class_ $ wrap "set" ]
    [ HH.text name ]
