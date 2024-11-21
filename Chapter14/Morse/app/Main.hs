module Main where
import Control.Monad (forever, when)
import System.IO (hIsEOF, stdin, hGetLine)
import System.Exit (exitSuccess, exitFailure)
import Morse (stringToMorse, morseToChar)
import Data.List (intercalate)
import System.Environment (getArgs)

convertToMorse :: IO ()
convertToMorse = forever $ do
    weAreDone <- hIsEOF stdin
    when weAreDone exitSuccess

    line <- hGetLine stdin
    convertLine line
    where
        convertLine line = do
            let morse = stringToMorse line
            case morse of
                (Just str) -> putStrLn $ intercalate " " str
                Nothing -> do
                    putStrLn $ "ERROR: " ++ line
                    exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
    weAreDone <- hIsEOF stdin
    when weAreDone exitSuccess

    line <- hGetLine stdin
    convertLine line
    where
        convertLine line = do
            let decoded :: Maybe String
                decoded = traverse morseToChar (words line)
            case decoded of
                (Just s) -> putStrLn s
                Nothing -> do
                    putStrLn $ "ERROR: " ++ line
                    exitFailure

main :: IO ()
main = do
    mode <- getArgs
    case mode of
        [arg] ->
            case arg of
                "from" -> convertFromMorse
                "to" -> convertToMorse
                _ -> argError
        _ -> argError
        where argError = do
                putStrLn "Please specify the first argument \\ as being 'from' or 'to' morse, \\ such as: morse to"
                exitFailure
