module Control.Monad.Eff.Worker where

import Prelude (Unit)
import Control.Monad.Eff (Eff, kind Effect)

foreign import data WORKER :: Effect

-- TODO accept only Transferable types
foreign import data Worker :: Type -> Type -> Type
foreign import data WorkerModule :: Type -> Type -> Type

type MessageCallback a e = a -> Eff e Unit
