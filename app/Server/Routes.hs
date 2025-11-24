{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.Routes where 

import Api.Model
import Data.Proxy
import Network.Wai
import Servant.API.Sub
import Servant.API
import Servant.Server
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Control.Monad.Except

type API = 
         "hello" :> Get '[PlainText] String 
    :<|> "soma" :> ReqBody '[JSON] Calculadora :> Post '[JSON] ResultadoResponse
    :<|> "soma"  :> Verb 'OPTIONS 200 '[JSON] ()
    :<|> "cliente" :> ReqBody '[JSON] Cliente :> Post '[JSON] ResultadoResponse 
    :<|> "cliente"  :> Verb 'OPTIONS 200 '[JSON] ()
    :<|> "clientes" :> Get '[JSON] ClienteResponse
    :<|> "cliente" :> Capture "id" Int :> Get '[JSON] Cliente
    :<|> "cliente" :> Capture "id" Int :> "nome" :> ReqBody '[JSON] ClienteNome :> Patch '[JSON] NoContent
    :<|> "cliente" :> Capture "id" Int :> ReqBody '[JSON] Cliente :> Put '[JSON] NoContent

handlerPutCliente :: Connection -> Int -> Cliente -> Handler NoContent
handlerPutCliente conn clienteId cli = do
    res <- liftIO $ execute conn "UPDATE Cliente SET nome = ?, cpf = ? WHERE id = ?" (nome cli, cpf cli, clienteId)

handlerPatchClienteNome :: Connection -> Int -> ClienteNome -> Handler NoContent
handlerPatchClienteNome conn clienteId (ClienteNome novoNome) = do
    res <- liftIO $ execute conn "UPDATE Cliente SET nome = ? WHERE id = ?" (novoNome, clienteId)
    if res == 1
        then pure NoContent
        else throwError err404

handlerClienteById :: Connection -> Int -> Handler Cliente
handlerClienteById conn clienteId = do
    res <- liftIO $ query conn "SELECT * FROM Cliente WHERE id = ?" (Only clienteId)
    case res of
        [(id', nome', cpf')] -> pure (Cliente id' nome' cpf')
        _ -> throwError err404

handlerClienteTodos :: Connection -> Handler ClienteResponse
handlerClienteTodos conn = do 
    res <- liftIO $ query_ conn "SELECT id, nome, cpf FROM Cliente" 
    let result = map (\(id', nome', cpf') -> Cliente id' nome' cpf') res
    pure (ClienteResponse result)

handlerCliente :: Connection -> Cliente -> Handler ResultadoResponse
handlerCliente conn cli = do 
    res <- liftIO $ query conn "INSERT INTO Cliente (nome,cpf) VALUES (?,?) RETURNING id" (nome cli, cpf cli)
    case res of 
        [Only novoId] -> pure (ResultadoResponse $ novoId)
        _ -> throwError err500

handlerSoma :: Calculadora -> Handler ResultadoResponse
handlerSoma (Calculadora x y) = pure (ResultadoResponse $ x + y)

options :: Handler ()
options = pure ()

-- Handler eh uma Monada que tem IO embutido
handlerHello :: Handler String 
handlerHello = pure "Ola, mundo!"

server :: Connection -> Server API 
server conn = handlerHello 
            :<|> handlerSoma 
            :<|> options 
            :<|> handlerCliente conn 
            :<|> options 
            :<|> handlerClienteTodos conn
            :<|> handlerClienteById conn
            :<|> handlerPatchClienteNome conn

addCorsHeader :: Middleware
addCorsHeader app' req resp =
  app' req $ \res ->
    resp $ mapResponseHeaders
      ( \hs ->
          [ ("Access-Control-Allow-Origin", "*")
          , ("Access-Control-Allow-Headers", "Content-Type, Authorization")
          , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
          ] ++ hs
      )
      res

app :: Connection -> Application 
app conn = addCorsHeader (serve (Proxy @API) (server conn))