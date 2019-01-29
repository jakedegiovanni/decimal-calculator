module OperatingTypes
    ( OperatingMode(..)
    , getOperatingModeValue
    , hexList
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

hexList :: [Char]
hexList = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

getOperatingModeValue :: Int -> OperatingMode
getOperatingModeValue x = extractValue $ Map.lookup x operatingModeMap
    
extractValue :: Maybe OperatingMode -> OperatingMode
extractValue (Just opMode) = opMode
extractValue Nothing = UNKNOWN