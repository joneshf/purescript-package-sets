module Main where

import Control.Bind (discard)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Semigroup ((<>))
import Data.Unit (Unit)

main :: Eff (dom :: DOM) Unit
main = do
  runFn2 loadData (packageSet "psc-0.11.6") "psc-container"
  runFn2 loadData (packageSet "purerl-0.11.6") "purerl-container"

foreign import loadData
  :: forall e. Fn2 String String (Eff (dom :: DOM | e) Unit)

packageSet :: String -> String
packageSet set =
  "https://cdn.rawgit.com/joneshf/purescript-package-sets/"
    <> set
    <> "/packages.json"
