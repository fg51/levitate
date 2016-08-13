{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ConvertJson (toJsonString) where


import GHC.Generics (Generic)
-- import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Aeson (FromJSON, ToJSON, encode, decode)



type Position = (Double, Double, Double)


data PositionHash = PositionHash
         { time :: Double
         , velocity :: Double
         , height   :: Double
         } deriving (Generic, Show)

instance FromJSON PositionHash
instance ToJSON PositionHash


toJsonPosition (t, v, y) = PositionHash {
          time = t
        , velocity = v
        , height = y
        }

toJsonString :: [Position] -> String
toJsonString xs = LBS.unpack $ encode $ map toJsonPosition xs

-- innJson = do
--     innFile <- decode <$> LBS.readFile "test.json"
--     case (innFile :: Maybe Position) of
--       Just foo -> print foo
--       Nothing  -> putStrLn "fail to decode."


outJson = LBS.unpack $ encode ([
      PositionHash {
          time = 0
        , velocity = 10
        , height = 20
        }
    , PositionHash {
          time = 0
        , velocity = 10
        , height = 20
        }
    ])

