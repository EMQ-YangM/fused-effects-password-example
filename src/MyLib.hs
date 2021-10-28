{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module MyLib where

import           Control.Algebra
import           Control.Effect.Labelled
import           Control.Monad.IO.Class
import qualified Crypto.Random           as CR
import           CryptoHash
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
               (KVStore Username PasswordHash) sig m,
             MonadIO m)
         => m ()
example1 = do
  regist (Username "yang") (Password "12345") >>= liftIO . print
  regist (Username "yang1") (Password "12345678") >>= liftIO . print
  loggin (Username "yang") (Password "12345") >>= liftIO . print

runExample1  = do
  gen <- CR.getSystemDRG
  runCryptoHash gen $ KVP.runKVStore example1

dbFile :: FilePath
dbFile = "password.db"

withPasswordDBConnection :: (SQL.Connection -> IO a) -> IO a
withPasswordDBConnection f = SQL.withConnection dbFile $ \conn -> do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS passwords (username TEXT PRIMARY KEY, hash TEXT)"
  f conn

runExample2 :: IO ()
runExample2 = do
  gen <- CR.getSystemDRG
  withPasswordDBConnection $ \conn -> do
    runCryptoHash gen $ KVS.runKVStore conn example1
