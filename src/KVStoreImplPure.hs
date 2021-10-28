{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module KVStoreImplPure where

import           Control.Algebra
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Monad.IO.Class
import           Data.Map.Strict              as Map
import           KVStore


newtype KVStoreC k v m a = KVStoreC { runKVStoreC :: StateC (Map k v) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Ord k) => Algebra (KVStore k v :+: sig ) (KVStoreC k v m) where
  alg hdl sig ctx = KVStoreC $ case sig of
    L (LookupKV k)   -> (<$ ctx) <$> gets (Map.lookup k)
    L (UpdateKV k v) -> modify (Map.insert k v) >> pure ctx
    R other          -> alg (runKVStoreC . hdl) (R other) ctx

runKVStore :: (Monad m, Ord k)
           => Labelled KVStore (KVStoreC k v) m a
           -> m (Map k v, a)
runKVStore = runState mempty . runKVStoreC . runLabelled

