module SQLJS
  ( Database
  , SQLJS
  , close
  , exec
  , new
  ) where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Foreign (Foreign)
import Data.Unit (Unit, unit)

foreign import data Database :: Type

foreign import data SQLJS :: Effect

foreign import close_ :: forall a e. EffFn2 ( sqljs :: SQLJS | e ) a Database a

foreign import exec_
  :: forall e
  . EffFn2 ( sqljs :: SQLJS | e ) String Database (Array QueryResult)

foreign import new_ :: forall e. EffFn1 ( sqljs :: SQLJS | e ) Uint8Array Database

type QueryResult
  = { columns :: Array String
    , values :: Array (Array Foreign)
    }

close :: forall e. Database -> Eff ( sqljs :: SQLJS | e ) Unit
close = runEffFn2 close_ unit

exec :: forall e. String -> Database -> Eff ( sqljs :: SQLJS | e ) (Array QueryResult)
exec = runEffFn2 exec_

new :: forall e. Uint8Array -> Eff ( sqljs :: SQLJS | e ) Database
new = runEffFn1 new_
