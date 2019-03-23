module Decimal 
    (
        fromDecimal
    ) where

import OperatingTypes (OperatingMode(..))
import Binary (fromBinary)
import qualified Data.Text as Text

fromDecimal :: OperatingMode -> OperatingMode -> Text.Text -> Text.Text
fromDecimal DECIMAL BINARY inputValue = decimalToBinary inputValue
fromDecimal DECIMAL RGB _ = Text.pack "doesn't make sense"
fromDecimal DECIMAL DECIMAL inputValue = inputValue
fromDecimal DECIMAL HEXADECIMAL inputValue = decimalToHexadecimal inputValue
fromDecimal _ _ _ = Text.pack "what?"

decimalToHexadecimal :: Text.Text -> Text.Text
decimalToHexadecimal inputValue = fromBinary BINARY HEXADECIMAL $ decimalToBinary inputValue

decimalToBinary :: Text.Text -> Text.Text
decimalToBinary inputValue = do
    let number = read (Text.unpack inputValue) :: Int
    Text.pack $ foldl (++) "" $ map show $ reverse $ convertToReversedBinary number

convertToReversedBinary :: Int -> [Int]
convertToReversedBinary 0 = []
convertToReversedBinary x = do
    let val = x `div` 2
    let test = val :: Int
    let remainder = x `mod` 2
    remainder:(convertToReversedBinary test)
