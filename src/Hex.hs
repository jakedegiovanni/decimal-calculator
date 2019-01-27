module Hex
    (
        fromHex
    ) where

import OperatingTypes (OperatingMode(..))
import Data.Char (toUpper)
import qualified Data.List as List

hexList = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

fromHex :: OperatingMode -> OperatingMode -> String -> String
fromHex HEXADECIMAL DECIMAL inputValue = hexToDecimal inputValue
fromHex _ _ _ = "what?"

hexToDecimal :: String -> String
hexToDecimal inputValue = do
    let decimalValue = convertToDecimal (map toUpper inputValue) ((length inputValue) - 1)
    letsPrint decimalValue

convertToDecimal :: [Char] -> Int -> Maybe Int 
convertToDecimal (x:xs) 0 = do
    z <- List.elemIndex x hexList
    let e = z * (16 ^ 0)
    Just e
convertToDecimal (x:xs) a = do
    z <- List.elemIndex x hexList
    let e = z * (16 ^ a)
    fmap (e+) (convertToDecimal xs (a - 1))

letsPrint :: Maybe Int -> String
letsPrint (Just x) = show x
letsPrint Nothing = "couldn't do it"