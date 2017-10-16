module SQLJS.Typed where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throw)
import Data.Foldable (length)
import Data.Function (($))
import Data.Newtype (class Newtype, un)
import Data.Semigroup ((<>))
import Data.Show (show)
import SQLJS (Database, Effects, QueryResult, exec)

newtype SQL
  = SQL String

derive instance newtypeSQL :: Newtype SQL _

queryResult :: forall e. SQL -> Database -> Eff (Effects e) QueryResult
queryResult sql db = do
  results <- exec (un SQL sql) db
  case results of
    [result] -> pure result
    _ -> throw $ "Expected exactly one result. Got " <> show (length results :: Int)
