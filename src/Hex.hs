module Hex
    ( fromHex
    ) where

import OperatingTypes (OperatingMode(..), hexList)
import Decimal (fromDecimal)
import Data.Char (toUpper)
import qualified Data.List as List
import qualified Data.Text as Text

fromHex :: OperatingMode -> OperatingMode -> Text.Text -> Text.Text
fromHex HEXADECIMAL DECIMAL inputValue = hexToDecimal inputValue
fromHex HEXADECIMAL RGB inputValue = hexToRgb inputValue
fromHex HEXADECIMAL BINARY inputValue = hexToBinary inputValue
fromHex HEXADECIMAL HEXADECIMAL inputValue = inputValue
fromHex _ _ _ = Text.pack "what?"

hexToBinary :: Text.Text -> Text.Text
hexToBinary inputValue = fromDecimal DECIMAL BINARY $ hexToDecimal inputValue

hexToRgb :: Text.Text -> Text.Text
hexToRgb inputValue = do
    let validHex = (mod (Text.length inputValue) 6) == 0
    if validHex
        then
            convertToRGB inputValue
        else
            Text.pack "invalid rgb value"

convertToRGB :: Text.Text -> Text.Text
convertToRGB inputValue = do
    let groupOfHex = Text.chunksOf 2 inputValue
    Text.pack $ show $ to3Tuple $ map hexToDecimal groupOfHex

to3Tuple :: [Text.Text] -> (String, String, String)
to3Tuple [a, b, c] = (Text.unpack a, Text.unpack b, Text.unpack c)
to3Tuple _ = ("no", "no", "no")

hexToDecimal :: Text.Text -> Text.Text
hexToDecimal inputValue = do
    let decimalValue = convertToDecimal (Text.unpack (Text.toUpper inputValue)) ((Text.length inputValue) - 1)
    letsPrint decimalValue

convertToDecimal :: [Char] -> Int -> Maybe Int 
convertToDecimal (x:xs) 0 = do
    z <- List.elemIndex x hexList
    Just z
convertToDecimal (x:xs) a = do
    z <- List.elemIndex x hexList
    let e = z * (16 ^ a)
    fmap (e+) (convertToDecimal xs (a - 1))

letsPrint :: Maybe Int -> Text.Text
letsPrint (Just x) = Text.pack $ show x
letsPrint Nothing = Text.pack "couldn't do it"