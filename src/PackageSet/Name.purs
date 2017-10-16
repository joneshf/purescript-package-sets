module PackageSet.Name
  ( Package(..)
  , renderPackage
  , Set(..)
  , renderSet
  ) where

import Data.Eq (class Eq)
import Data.Function (($))
import Data.Newtype (class Newtype, wrap)
import Data.Ord (class Ord)
import Halogen.Component as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

newtype Package
  = Package String

derive instance eqPackage :: Eq Package

derive instance newtypePackage :: Newtype Package _

derive instance ordPackage :: Ord Package


renderPackage :: forall f. Package -> H.ComponentHTML f
renderPackage (Package name) = HH.text name

newtype Set
  = Set String

derive instance eqSet :: Eq Set

derive instance newtypeSet :: Newtype Set _

derive instance ordSet :: Ord Set

renderSet :: forall f. Set -> H.ComponentHTML f
renderSet (Set name) =
  HH.h2
    [ HP.class_ $ wrap "set" ]
    [ HH.text name ]
