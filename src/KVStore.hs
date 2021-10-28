
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module KVStore where

import           Control.Algebra
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Monad.IO.Class
import           Data.Kind
import           Types

data KVStore k v (m :: Type -> Type) a where
  LookupKV :: k -> KVStore k v m (Maybe v)
  UpdateKV :: k -> v -> KVStore k v m ()

lookupKV :: HasLabelled KVStore (KVStore k v) sig m => k -> m (Maybe v)
lookupKV = sendLabelled @KVStore  . LookupKV

updateKV :: HasLabelled KVStore (KVStore k v) sig m => k -> v -> m ()
updateKV k v = sendLabelled @KVStore (UpdateKV k v)

