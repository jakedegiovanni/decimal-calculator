module OperatingTypes
    ( OperatingMode(..)
    , getOperatingModeValue
    ) where

import qualified Data.Map as Map

data OperatingMode = HEXADECIMAL | RGB | DECIMAL | BINARY | UNKNOWN deriving (Show, Eq, Ord)

operatingModeMap = Map.fromList
    [(1, HEXADECIMAL)
    ,(2, RGB)
    ,(3, DECIMAL)
    ,(4, BINARY)
    ,(5, UNKNOWN)
    ]

getOperatingModeValue :: Int -> OperatingMode
getOperatingModeValue x = extractValue $ Map.lookup x operatingModeMap
    
extractValue :: Maybe OperatingMode -> OperatingMode
extractValue (Just opMode) = opMode
extractValue Nothing = UNKNOWN