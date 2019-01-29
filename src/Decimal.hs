module Decimal 
    (
        fromDecimal
    ) where

import OperatingTypes (OperatingMode(..))
import Binary (fromBinary)

-- TODO: make this use instance as it is identical
fromDecimal :: OperatingMode -> OperatingMode -> String -> String
fromDecimal DECIMAL BINARY inputValue = decimalToBinary inputValue
fromDecimal DECIMAL RGB _ = "doesn't make sense"
fromDecimal DECIMAL DECIMAL inputValue = inputValue
fromDecimal DECIMAL HEXADECIMAL inputValue = decimalToHexadecimal inputValue
fromDecimal _ _ _ = "what?"

decimalToHexadecimal :: String -> String
decimalToHexadecimal inputValue = fromBinary BINARY HEXADECIMAL $ decimalToBinary inputValue

decimalToBinary :: String -> String
decimalToBinary inputValue = do
    let number = read inputValue :: Int
    foldl (++) "" $ map (\x -> show x) $ reverse $ convertToReversedBinary number

convertToReversedBinary :: Int -> [Int]
convertToReversedBinary 0 = []
convertToReversedBinary x = do
    let val = x `div` 2
    let test = val :: Int
    let remainder = x `mod` 2
    remainder:(convertToReversedBinary test)
