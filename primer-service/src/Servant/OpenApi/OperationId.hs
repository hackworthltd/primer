{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.OpenApi.OperationId (OperationId, OpId) where

import Foreword

import Data.Kind (Type)
import Data.OpenApi (allOperations)
import Data.Text (pack)
import Optics (traversalVL, (%), (?~))
import Servant (HasServer (hoistServerWithContext, route), ServerT, Verb)
import Servant.OpenApi (HasOpenApi (toOpenApi))

-- | A wrapped 'Verb'
-- Servant treats @OperationId id m s ct a@  the same as @Verb m s ct a@.
-- For generating OpenAPI documentation, they behave the same except for the
-- addition of an @operationId@ field on the corresponding operation.
--
-- The @id@s must be unique throughout any 'API', otherwise 'toOpenApi' will
-- generate an invalid OpenAPI spec.
data
  OperationId
    (id :: Symbol)
    (method :: k1)
    (statusCode :: Nat)
    (contentTypes :: [Type])
    (a :: Type)

instance
  HasServer (Verb method status ctypes a) context =>
  HasServer (OperationId id method status ctypes a) context
  where
  type
    ServerT (OperationId id method status ctypes a) m =
      ServerT (Verb method status ctypes a) m
  route _ = route (Proxy @(Verb method status ctypes a))
  hoistServerWithContext _ =
    hoistServerWithContext
      (Proxy @(Verb method status ctypes a))

instance
  (KnownSymbol id, HasOpenApi (Verb method status ctypes a)) =>
  HasOpenApi (OperationId id method status ctypes a)
  where
  toOpenApi _ =
    toOpenApi (Proxy @(Verb method status ctypes a))
      & traversalVL allOperations % #operationId
      ?~ pack (symbolVal (Proxy @id))

-- | Similar to 'OperationId', but for Servant-provided convenience synonyms,
-- like 'Get' or 'Post'
data
  OpId
    (id :: Symbol)
    (verb :: [Type] -> Type -> Type)
    (contentTypes :: [Type])
    (a :: Type)

instance
  HasServer (verb ctypes a) context =>
  HasServer (OpId id verb ctypes a) context
  where
  type
    ServerT (OpId id verb ctypes a) m =
      ServerT (verb ctypes a) m
  route _ = route (Proxy @(verb ctypes a))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(verb ctypes a))

instance (KnownSymbol id, HasOpenApi (verb ctypes a)) => HasOpenApi (OpId id verb ctypes a) where
  toOpenApi _ =
    toOpenApi (Proxy @(verb ctypes a))
      & traversalVL allOperations % #operationId
      ?~ pack (symbolVal (Proxy @id))
