module Binary 
    (
        fromBinary
    ) where

import OperatingTypes (OperatingMode(..))
import qualified Data.List as List

fromBinary :: OperatingMode -> OperatingMode -> String -> String
fromBinary DECIMAL BINARY inputValue = decimalToBinary inputValue
fromBinary _ _ _ = "todo"

decimalToBinary :: String -> String
decimalToBinary inputValue = do
    let number = read inputValue :: Int
    List.intercalate "" $ map (\x -> show x) $ reverse $ convertToReversedBinary number

convertToReversedBinary :: Int -> [Int]
convertToReversedBinary 0 = []
convertToReversedBinary x = do
    let val = x `div` 2
    let test = val :: Int
    let remainder = x `mod` 2
    remainder:(convertToReversedBinary test)
