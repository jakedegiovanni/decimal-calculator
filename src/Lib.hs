module Lib
    ( 
        calculator
    ) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified System.Exit as SExit
import qualified Data.Text as Text

import OperatingTypes
import qualified Hex as Hex
import qualified Decimal as Decimal
import qualified Binary as Binary

calculator :: IO ()
calculator = do
    inputMode <- getOperatingMode "input"
    outputMode <- getOperatingMode "output"
    let inputType = opValue inputMode
    let outputType = opValue outputMode
    if inputType == UNKNOWN || outputType == UNKNOWN
        then
            SExit.die "Invalid conversion type inputted"
        else
            convertInput inputType outputType
    where
        opValue x = getOperatingModeValue $ (read x :: Int)

convertInput :: OperatingMode -> OperatingMode -> IO ()
convertInput inputType outputType = do
    putStrLn "Input Value: "
    inputValue <- getLine
    doConversion inputType outputType $ Text.pack inputValue

doConversion :: OperatingMode -> OperatingMode -> Text.Text -> IO ()
doConversion HEXADECIMAL x inputValue = display $ Hex.fromHex HEXADECIMAL x inputValue
doConversion DECIMAL x inputValue = display $ Decimal.fromDecimal DECIMAL x inputValue
doConversion BINARY x inputValue = display $ Binary.fromBinary BINARY x inputValue
doConversion _ _ _ = putStrLn "another"

getOperatingMode:: String -> IO String
getOperatingMode mode =  do
    putStrLn $ "What " ++ mode ++ " mode do you want?"
    putStrLn $ List.intercalate " " createFormattedString
    getLine

createFormattedString :: [String]
createFormattedString = map (\x -> show x ++ ") " ++ (show $ getOperatingModeValue x)) [1..4]

display :: Text.Text -> IO ()
display x = putStrLn $ Text.unpack x
