module Binary
    (
        fromBinary
    ) where

import OperatingTypes (OperatingMode(..), hexList)
import qualified Data.Text as Text
import qualified Data.List as List

fromBinary :: OperatingMode -> OperatingMode -> String -> String
fromBinary BINARY HEXADECIMAL inputValue = binaryToHex inputValue
fromBinary BINARY DECIMAL inputValue = show $ binaryToDecimal inputValue
fromBinary BINARY BINARY inputValue = inputValue
fromBinary BINARY RGB _ = "no"
fromBinary _ _ _ = "what?"

binaryToHex :: String -> String
binaryToHex inputValue = foldr (:) [] $ map (\x -> hexList!!x) $ map binaryToDecimal $ map Text.unpack $ Text.chunksOf 4 $ Text.pack $ padToMultiplesOf4 inputValue

padToMultiplesOf4 :: String -> String
padToMultiplesOf4 inputValue = do
    let len = length inputValue
    let paddingAmmount = [x | x <- [len..(len * 4)], (mod x 4) == 0]
    let zeroPadding = replicate (paddingAmmount!!0 - len) '0'
    zeroPadding ++ inputValue

binaryToDecimal :: String -> Int
binaryToDecimal inputValue = addTwoPowers $ zip [0..] $ reverse inputValue

addTwoPowers :: [(Int, Char)] -> Int
addTwoPowers [] = 0
addTwoPowers (x:xs) = if (snd x) == '0' then 0 + (addTwoPowers xs) else (2 ^ (fst x)) + (addTwoPowers xs)