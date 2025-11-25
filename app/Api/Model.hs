{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api.Model where

import Data.Aeson
import GHC.Generics

data Vehicle = Vehicle
  { idVeiculo :: Int
  , placa :: String
  , modelo :: String
  , ano :: Int
  }
  deriving (Show, Generic)

instance FromJSON Vehicle
instance ToJSON Vehicle

data VehicleInput = VehicleInput
  { placa :: String
  , modelo :: String
  , ano :: Int
  }
  deriving (Show, Generic)

instance FromJSON VehicleInput
instance ToJSON VehicleInput

data VehicleList = VehicleList
  { veiculos :: [Vehicle]
  }
  deriving (Show, Generic)

instance ToJSON VehicleList

data VehicleIdResponse = VehicleIdResponse
  { resultado :: Int
  }
  deriving (Show, Generic)

instance ToJSON VehicleIdResponse

data VehicleModelo = VehicleModelo
  { novoModelo :: String
  }
  deriving (Show, Generic)

instance FromJSON VehicleModelo
