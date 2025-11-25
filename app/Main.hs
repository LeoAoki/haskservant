{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (..))
import Network.Wai.Handler.Warp
import Server.Routes
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

runMigration :: Connection -> FilePath -> IO ()
runMigration conn fp = do
  sql <- readFile fp
  void $ execute_ conn (Query $ BS.pack sql)

getConnectionString :: IO String
getConnectionString = do
  env <- lookupEnv "DATABASE_URL"
  let defaultConn = "host=localhost port=5432 dbname=veiculos user=postgres password=postgres"
  pure (fromMaybe defaultConn env)

main :: IO ()
main = do 
    putStrLn "Servidor rodando na porta 8080"

    connStr <- getConnectionString
    conn <- connectPostgreSQL (BS.pack connStr)

    runMigration conn "migration.sql"

    run 8080 (app conn)
