{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module KVStoreImplSQLite where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Labelled
import           Control.Monad.IO.Class
import           Data.Map.Strict                  as Map
import           Data.Maybe
import           Database.SQLite.Simple           (NamedParam ((:=)))
import qualified Database.SQLite.Simple           as SQL
import           KVStore

import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField   (ToField)
import           KVStoreImplPure                  (runKVStore)

newtype KVStoreC k v m a = KVStoreC { runKVStoreC :: ReaderC SQL.Connection m a }
  deriving (Functor, Applicative, Monad, MonadIO)


instance (Algebra sig m, MonadIO m, ToField k, ToField v, FromField v) =>
 Algebra (KVStore k v :+: sig ) (KVStoreC k v m) where
  alg hdl sig ctx = KVStoreC $ case sig of
    L (LookupKV k)   -> do
      conn <- ask
      hashes <- liftIO $ SQL.queryNamed conn
                   "SELECT hash FROM passwords WHERE username = :username"
                     [":username" := k]
      pure ((SQL.fromOnly <$> listToMaybe hashes) <$ ctx)
    L (UpdateKV k v) -> do
      let (query, params) = ("INSERT INTO passwords (username, hash) VALUES (:username, :hash) "
                               <> "ON CONFLICT (username) DO UPDATE SET hash = excluded.hash"
                            ,[":username" := k, ":hash" := v])
      conn <- ask
      liftIO $ SQL.executeNamed conn query params
      pure ctx
    R other          -> alg (runKVStoreC . hdl) (R other) ctx

runKVStore :: Monad m
           => SQL.Connection
           -> Labelled KVStore (KVStoreC k v) m a
           -> m a
runKVStore conn f = runReader conn $ runKVStoreC $ runLabelled f
