module SQLJS
  ( Database
  , QueryResult
  , SQLJS
  , Effects
  , close
  , exec
  , new
  ) where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Foreign (Foreign)
import Data.Unit (Unit, unit)

foreign import data Database :: Type

foreign import data SQLJS :: Effect

foreign import close_ :: forall a e. EffFn2 (Effects e) a Database a

foreign import exec_
  :: forall e
  . EffFn2 (Effects e) String Database (Array QueryResult)

foreign import new_ :: forall e. EffFn1 (Effects e) Uint8Array Database

type QueryResult
  = { columns :: Array String
    , values :: Array (Array Foreign)
    }

type Effects e = ( exception :: EXCEPTION, sqljs :: SQLJS | e )

close :: forall e. Database -> Eff (Effects e) Unit
close = runEffFn2 close_ unit

exec :: forall e. String -> Database -> Eff (Effects e) (Array QueryResult)
exec = runEffFn2 exec_

new :: forall e. Uint8Array -> Eff (Effects e) Database
new = runEffFn1 new_
