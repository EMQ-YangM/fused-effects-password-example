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
module CryptoHash where

import           Control.Algebra
import           Control.Carrier.State.Strict
import           Control.Monad.IO.Class
import qualified Crypto.KDF.BCrypt            as BCrypt
import qualified Crypto.Random                as CR
import           Data.ByteString              (ByteString)
import           Data.Kind
import           Types


data CryptoHash (m :: Type -> Type) a where
  MakeHash :: Password  -> CryptoHash m PasswordHash
  ValidateHash :: Password -> PasswordHash -> CryptoHash m Bool

makeHash :: Has CryptoHash sig m => Password -> m PasswordHash
makeHash  = send . MakeHash

validateHash :: Has CryptoHash sig m => Password -> PasswordHash -> m Bool
validateHash ps psh = send (ValidateHash ps psh)

newtype CryptoHashC gen m a = CryptoHashC { runCryptoHashC :: StateC gen m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, CR.DRG gen) => Algebra (CryptoHash :+: sig ) (CryptoHashC gen m) where
  alg hdl sig ctx = CryptoHashC $ case sig of
    L (ValidateHash password hash) -> pure (BCrypt.validatePassword password hash  <$ ctx )
    L (MakeHash password) -> do
      drg <- get @gen
      let (hash, drg') = CR.withDRG drg (BCrypt.hashPassword 5 password)
      put drg'
      pure (hash <$ ctx)
    R other -> alg (runCryptoHashC . hdl) (R other) ctx

runCryptoHash :: forall gen m a. Functor m => gen -> CryptoHashC gen m a -> m a
runCryptoHash gen f = evalState @gen gen $ runCryptoHashC f
