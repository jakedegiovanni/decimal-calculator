module Hex
    (
        fromHex
    ) where

import OperatingTypes (OperatingMode(..))
import Data.Char (toUpper)
import qualified Data.List as List
import qualified Data.Text as Text

hexList = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

fromHex :: OperatingMode -> OperatingMode -> String -> String
fromHex HEXADECIMAL DECIMAL inputValue = hexToDecimal inputValue
fromHex HEXADECIMAL RGB inputValue = hexToRgb inputValue
fromHex _ _ _ = "what?"

hexToRgb :: String -> String
hexToRgb inputValue = do
    let validHex = (mod (length inputValue) 6) == 0
    if validHex
        then
            convertToRGB inputValue
        else
            "invalid rgb value"

convertToRGB :: String -> String
convertToRGB inputValue = do
    let groupOfHex = Text.chunksOf 2 $ Text.pack inputValue
    show $ to3Tuple $ map (\x -> hexToDecimal $ Text.unpack x) groupOfHex

to3Tuple :: [String] -> (String, String, String)
to3Tuple [a, b, c] = (a, b, c)
to3Tuple _ = ("no", "no", "no")

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