{-# LANGUAGE DeriveGeneric #-}

module Config (
  Config (..),
  readConfig,
  AnimationType (..),
) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON, eitherDecode')
import qualified Data.ByteString.Lazy.Char8 as BSL

data AnimationType = Smooth | Mixed | Digital deriving (Generic)

newtype Config = Config {animType :: AnimationType} deriving (Generic)

instance FromJSON AnimationType
instance ToJSON AnimationType

instance FromJSON Config
instance ToJSON Config

readConfig :: IO Config
readConfig = do
  json <- BSL.readFile "Config.json"
  let res = eitherDecode' json :: Either String Config
  case res of
    Right config -> return config
    Left msg -> fail msg