module PackageSet.Package
  ( Package(..)
  , renderResults
  ) where

import Data.Function (($))
import Data.Functor ((<$>))
import Data.Newtype (class Newtype, wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PackageSet.Name as Name
import PackageSet.Repo (Repo)
import PackageSet.Repo as Repo
import PackageSet.Version (Version)
import PackageSet.Version as Version

newtype Package
  = Package
    { dependencies :: Array Name.Package
    , name :: Name.Package
    , repo :: Repo
    , version :: Version
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
            [ Repo.href repo ]
            [ Name.renderPackage name ]
          ]
        , HH.div_
          [ HH.text "Dependencies" ]
        , HH.ul
          [ HP.class_ $ wrap "dependencies" ]
          (renderDependency <$> dependencies)
        ]
      ]
    , HH.td
      [ HP.class_ $ wrap "package-version" ]
      [ Version.render version ]
    ]

renderDependency :: forall f. Name.Package -> H.ComponentHTML f
renderDependency dep = HH.li_ [ Name.renderPackage dep ]
