{-# LANGUAGE DeriveGeneric #-}
module Api.Model where

import Data.Aeson
import GHC.Generics

data Calculadora = Calculadora {
    n1 :: Int,
    n2 :: Int 
} deriving (Show, Generic)

instance FromJSON Calculadora where 
instance ToJSON Calculadora where 

data ResultadoResponse = ResultadoResponse {
    resultado :: Int
} deriving (Show, Generic)

instance ToJSON ResultadoResponse where 

data Cliente = Cliente {
    id :: Int,
    nome :: String,
    cpf :: String
} deriving (Show, Generic)

instance FromJSON Cliente where 
instance ToJSON Cliente where 

data ClienteResponse = ClienteResponse {
    clientes :: [Cliente]
} deriving (Show, Generic)

instance ToJSON ClienteResponse where

data ClienteNome = ClienteNome {
    novoNome :: String
} deriving (Show, Generic)

instance FromJSON ClienteNome where