{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.OpenApi.OperationId (OperationId) where

import Foreword

import Data.OpenApi (allOperations)
import Data.Text (pack)
import Optics (traversalVL, (%), (%~))
import Servant (
  HasServer (ServerT, hoistServerWithContext, route),
  type (:>),
 )
import Servant.Client.Core (
  HasClient (
    Client,
    clientWithRoute,
    hoistClientMonad
  ),
 )
import Servant.OpenApi (HasOpenApi (toOpenApi))

-- | Add an openapi operation id for endpoints in an api.
-- Servant treats @OperationId id :> api@  the same as @api@.
-- For generating OpenAPI documentation, they behave the same except for the
-- addition of an @operationId@ field on the corresponding operation.
-- All @OperationId@s for a particular endpoint will be concatenated.
--
-- The @id@s must be unique throughout any 'API', otherwise 'toOpenApi' will
-- generate an invalid OpenAPI spec.
data OperationId (id :: Symbol)

instance
  HasServer api context =>
  HasServer (OperationId id :> api) context
  where
  type ServerT (OperationId id :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance
  HasClient m api =>
  HasClient m (OperationId id :> api)
  where
  type Client m (OperationId id :> api) = Client m api
  clientWithRoute pm _ = clientWithRoute pm (Proxy @api)
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)

instance
  (KnownSymbol id, HasOpenApi api) =>
  HasOpenApi (OperationId id :> api)
  where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      & traversalVL allOperations % #operationId %~ (Just (pack (symbolVal (Proxy @id))) <>)
