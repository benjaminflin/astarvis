module Worker where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect)

foreign import data Worker :: Type

foreign import data DedicatedGlobalScope :: Type

type URL
  = String

foreign import worker :: URL -> Worker

foreign import postMessage :: forall a. Worker -> a -> Effect Unit

foreign import globalScope :: Effect DedicatedGlobalScope

foreign import postMessageGlobal :: forall a. DedicatedGlobalScope -> a -> Effect Unit

foreign import onMessage :: forall a m. MonadEffect m => Worker -> (a -> m Unit) -> Effect Unit

foreign import onMessageGlobal :: forall a m. MonadEffect m => DedicatedGlobalScope -> (a -> m Unit) -> Effect Unit

foreign import terminate :: Worker -> Effect Unit
