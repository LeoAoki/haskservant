{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Server.Routes where

import Api.Model
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Proxy
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Only (..))
import Network.Wai
import Servant.API
import Servant.Server

type API =
       "veiculos" :> Get '[JSON] VehicleList
  :<|> "veiculos" :> Verb 'OPTIONS 200 '[JSON] ()
  :<|> "veiculo" :> ReqBody '[JSON] VehicleInput :> Post '[JSON] VehicleIdResponse
  :<|> "veiculo" :> Verb 'OPTIONS 200 '[JSON] ()
  :<|> "veiculo" :> Capture "id" Int :> Get '[JSON] Vehicle
  :<|> "veiculo" :> Capture "id" Int :> Verb 'OPTIONS 200 '[JSON] ()
  :<|> "veiculo" :> Capture "id" Int :> ReqBody '[JSON] VehicleInput :> Put '[JSON] NoContent
  :<|> "veiculo" :> Capture "id" Int :> ReqBody '[JSON] VehicleModelo :> Patch '[JSON] NoContent
  :<|> "veiculo" :> Capture "id" Int :> Delete '[JSON] NoContent

handlerListVehicles :: Connection -> Handler VehicleList
handlerListVehicles conn = do
  res <- liftIO $ query_ conn "SELECT id, placa, modelo, ano FROM Veiculo"
  let vehicles = map (\(id', placa', modelo', ano') -> Vehicle id' placa' modelo' ano') res
  pure (VehicleList vehicles)

handlerGetVehicle :: Connection -> Int -> Handler Vehicle
handlerGetVehicle conn vehicleId = do
  res <- liftIO $ query conn "SELECT id, placa, modelo, ano FROM Veiculo WHERE id = ?" (Only vehicleId)
  case res of
    [(id', placa', modelo', ano')] -> pure (Vehicle id' placa' modelo' ano')
    _ -> throwError err404

handlerPostVehicle :: Connection -> VehicleInput -> Handler VehicleIdResponse
handlerPostVehicle conn VehicleInput {placa = placaIn, modelo = modeloIn, ano = anoIn} = do
  res <- liftIO $ query conn "INSERT INTO Veiculo (placa, modelo, ano) VALUES (?, ?, ?) RETURNING id" (placaIn, modeloIn, anoIn)
  case res of
    [Only newId] -> pure (VehicleIdResponse newId)
    _ -> throwError err500

handlerPutVehicle :: Connection -> Int -> VehicleInput -> Handler NoContent
handlerPutVehicle conn vehicleId VehicleInput {placa = placaIn, modelo = modeloIn, ano = anoIn} = do
  res <- liftIO $ execute conn "UPDATE Veiculo SET placa = ?, modelo = ?, ano = ? WHERE id = ?" (placaIn, modeloIn, anoIn, vehicleId)
  if res == 1 then pure NoContent else throwError err404

handlerPatchModelo :: Connection -> Int -> VehicleModelo -> Handler NoContent
handlerPatchModelo conn vehicleId VehicleModelo {novoModelo} = do
  res <- liftIO $ execute conn "UPDATE Veiculo SET modelo = ? WHERE id = ?" (novoModelo, vehicleId)
  if res == 1 then pure NoContent else throwError err404

handlerDeleteVehicle :: Connection -> Int -> Handler NoContent
handlerDeleteVehicle conn vehicleId = do
  res <- liftIO $ execute conn "DELETE FROM Veiculo WHERE id = ?" (Only vehicleId)
  if res == 1 then pure NoContent else throwError err404

options :: Handler ()
options = pure ()

server :: Connection -> Server API
server conn =
     handlerListVehicles conn
  :<|> options
  :<|> handlerPostVehicle conn
  :<|> options
  :<|> handlerGetVehicle conn
  :<|> (\_ -> options)
  :<|> handlerPutVehicle conn
  :<|> handlerPatchModelo conn
  :<|> handlerDeleteVehicle conn

addCorsHeader :: Middleware
addCorsHeader app' req resp =
  app' req $ \res ->
    resp $
      mapResponseHeaders
        ( \hs ->
            [ ("Access-Control-Allow-Origin", "*")
            , ("Access-Control-Allow-Headers", "Content-Type, Authorization")
            , ("Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS")
            ]
              ++ hs
        )
        res

app :: Connection -> Application
app conn = addCorsHeader (serve (Proxy :: Proxy API) (server conn))
