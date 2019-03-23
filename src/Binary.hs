module Binary
    (
        fromBinary
    ) where

import OperatingTypes (OperatingMode(..), hexList)
import qualified Data.Text as Text
import qualified Data.List as List

fromBinary :: OperatingMode -> OperatingMode -> Text.Text -> Text.Text
fromBinary BINARY HEXADECIMAL inputValue = binaryToHex inputValue
fromBinary BINARY DECIMAL inputValue = Text.pack $ show $ binaryToDecimal inputValue
fromBinary BINARY BINARY inputValue = inputValue
fromBinary BINARY RGB _ = Text.pack "no"
fromBinary _ _ _ = Text.pack "what?"

binaryToHex :: Text.Text -> Text.Text
binaryToHex inputValue = Text.pack $ foldr (:) [] $ map (\x -> hexList!!x) $ map binaryToDecimal $ Text.chunksOf 4 $ padToMultiplesOf4 inputValue

padToMultiplesOf4 :: Text.Text -> Text.Text
padToMultiplesOf4 inputValue = do
    let len = Text.length inputValue
    let paddingAmmount = [x | x <- [len..(len * 4)], (mod x 4) == 0]
    let zeroPadding = replicate (paddingAmmount!!0 - len) '0'
    Text.append (Text.pack zeroPadding) inputValue

binaryToDecimal :: Text.Text -> Int
binaryToDecimal inputValue = addTwoPowers $ zip [0..] $ Text.unpack $ Text.reverse inputValue

addTwoPowers :: [(Int, Char)] -> Int
addTwoPowers [] = 0
addTwoPowers (x:xs) = if (snd x) == '0' then 0 + (addTwoPowers xs) else (2 ^ (fst x)) + (addTwoPowers xs)