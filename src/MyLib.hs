{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module MyLib where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Effect.Labelled
import           Control.Monad.IO.Class
import           Crypto.Random           (seedFromInteger)
import qualified Crypto.Random           as CR
import           CryptoHash
import           Data.Map                (Map)
import qualified Database.SQLite.Simple  as SQL
import           KVStore
import qualified KVStoreImplPure         as KVP
import qualified KVStoreImplSQLite       as KVS
import           Types

-- addUser username password = do
--     hashedPassword <- makeHash password
--     insertInStore username hashedPassword

-- validatePassword username password = do
--     hashInStore <- lookupInStore username
--     case hashInStore of
--       Just h  -> validateHash password h
--       Nothing -> return False

addUser ::(Has CryptoHash sig m,
           HasLabelled KVStore
             (KVStore Username PasswordHash) sig m)
        => Username
        -> Password
        -> m ()
addUser username password = do
  hashedPassword <- makeHash password
  updateKV username hashedPassword

validatePassword ::(Has CryptoHash sig m,
                    HasLabelled KVStore
                      (KVStore Username PasswordHash) sig m)
                 => Username
                 -> Password
                 -> m Bool
validatePassword username password = do
  hashed <- lookupKV username
  case hashed of
    Nothing -> pure False
    Just h  -> validateHash password h

regist ::(Has CryptoHash sig m,
          HasLabelled KVStore
             (KVStore Username PasswordHash) sig m)
       => Username
       -> Password
       -> m (Either String ())
regist username password = do
  v <- lookupKV username
  case v of
    Just _  -> pure (Left $ show username ++  " exist")
    Nothing -> do
      passwordHash <- makeHash password
      updateKV username passwordHash
      return (Right ())

loggin :: (Has CryptoHash sig m,
           HasLabelled KVStore
             (KVStore Username PasswordHash) sig m)
       => Username
       -> Password
       -> m Bool
loggin username password =  do
  v <- lookupKV username
  case v of
    Nothing           -> pure False
    Just passwordHash -> validateHash password passwordHash

example1 :: (Has CryptoHash sig m,
             HasLabelled KVStore
               (KVStore Username PasswordHash) sig m)
         => m Bool
example1 = do
  regist (Username "yang") (Password "12345")
  regist (Username "yang1") (Password "12345678")
  loggin (Username "yang") (Password "12345")

runExample1 :: (Map Username PasswordHash, Bool)
runExample1  = do
  let seed = seedFromInteger 10
      cdrg = CR.drgNewSeed seed
  run $ runCryptoHash cdrg $ KVP.runKVStore example1

dbFile :: FilePath
dbFile = "password.db"

withPasswordDBConnection :: (SQL.Connection -> IO a) -> IO a
withPasswordDBConnection f = SQL.withConnection dbFile $ \conn -> do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS passwords (username TEXT PRIMARY KEY, hash TEXT)"
  f conn

runExample2 :: IO ()
runExample2 = do
  let seed = seedFromInteger 10
      cdrg = CR.drgNewSeed seed
  withPasswordDBConnection $ \conn -> do
    res <- runCryptoHash cdrg $ KVS.runKVStore conn example1
    print res
