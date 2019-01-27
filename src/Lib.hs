module Lib
    ( 
        calculator
    ) where

import OperatingTypes
import qualified Data.Map as Map
import qualified Data.List as List
import qualified System.Exit as SExit
import qualified Hex as Hex
import qualified Decimal as Decimal

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
    doConversion inputType outputType inputValue

doConversion :: OperatingMode -> OperatingMode -> String -> IO ()
doConversion HEXADECIMAL x inputValue = putStrLn $ Hex.fromHex HEXADECIMAL x inputValue
doConversion DECIMAL x inputValue = putStrLn $ Decimal.fromDecimal DECIMAL x inputValue
doConversion _ _ _ = putStrLn "another"

getOperatingMode:: String -> IO String
getOperatingMode mode =  do
    putStrLn $ "What " ++ mode ++ " mode do you want?"
    putStrLn $ List.intercalate " " createFormattedString
    getLine

createFormattedString :: [String]
createFormattedString = map (\x -> show x ++ ") " ++ (show $ getOperatingModeValue x)) [1..4]
