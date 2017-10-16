module SQLJS.Typed where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throw)
import Data.Foldable (length)
import Data.Function (($))
import Data.Newtype (class Newtype)
import Data.Semigroup ((<>))
import Data.Show (show)
import SQLJS (Database, Effects, QueryResult, exec)

newtype SQL
  = SQL String

derive instance newtypeSQL :: Newtype SQL _

queryResult :: forall e. SQL -> Database -> Eff (Effects e) QueryResult
queryResult (SQL sql) db = do
  results <- exec sql db
  case results of
    [] -> pure { columns: [], values: [] }
    [result] -> pure result
    _ ->
      throw
        $ "Expected exactly one result. Got "
        <> show (length results :: Int)
        <> ". Query was: "
        <> show sql
