module PackageSet.Package
  ( Package(..)
  , renderResults
  ) where

import Data.Function (($))
import Data.Functor ((<#>), (<$>))
import Data.Newtype (class Newtype, wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

newtype Package
  = Package
    { dependencies :: Array String
    , name :: String
    , repo :: String
    , version :: String
    }

derive instance newtypePackage :: Newtype Package _

renderResults :: forall f. Array Package -> H.ComponentHTML f
renderResults packages =
  HH.table
    [ HP.class_ $ wrap "packages" ]
    [ HH.thead_
      [ HH.tr
        [ HP.classes [ wrap "package", wrap "package-header" ] ]
        [ HH.th
          [ HP.class_ $ wrap "package-name" ]
          [ HH.text "name" ]
        , HH.th
          [ HP.class_ $ wrap "package-version" ]
          [ HH.text "version" ]
        ]
      ]
    , HH.tbody_
      (packageRow <$> packages)
    ]

packageRow :: forall f. Package -> H.ComponentHTML f
packageRow (Package { dependencies, name, repo, version }) =
  HH.tr
    [ HP.class_ $ wrap "package" ]
    [ HH.td
      [ HP.class_ $ wrap "package-name" ]
      [ HH.details
        []
        [ HH.summary
          []
          [ HH.a
            [ HP.href repo ]
            [ HH.text name ]
          ]
        , HH.div_
          [ HH.text "Dependencies" ]
        , HH.ul
          [ HP.class_ $ wrap "dependencies" ]
          (dependencies <#> \dep -> HH.li_ [HH.text dep])
        ]
      ]
    , HH.td
      [ HP.class_ $ wrap "package-version" ]
      [ HH.text version ]
    ]
