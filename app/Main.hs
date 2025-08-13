module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import MyLib

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> fileMode filePath
        _            -> interactiveMode

fileMode :: FilePath -> IO ()
fileMode filePath = do
    content <- readFile filePath
    let parsedNums = parseNumbers content
    if null parsedNums
      then hPutStrLn stderr "Error: The file contains no valid numbers. Please check the file content."
     else processNums parsedNums

interactiveMode :: IO ()
interactiveMode = do
    putStrLn "Welcome! Please input data set (separated by spaces or commas):"
    input <- getLine
    let numbers = parseNumbers input
    if null numbers
    then putStrLn "Program exiting due to invalid input."
    else processNums numbers

processNums :: [Integer] -> IO ()
processNums numbers = do
    putStrLn "Please input the number of classes:"
    classNumInp <- getLine
    case parseClassNum classNumInp of
        Nothing -> putStrLn "Error: Invalid number of classes provided. Program exiting."
        Just numOfC -> do
            let total     = length numbers
                range     = last numbers - head numbers
                cWidth    = getCW range numOfC
                classes   = getClasses cWidth numbers
                frequency = getF numbers classes
            
            putStrLn $ "Sample size:\n"      ++ show total
            putStrLn $ "Range:\n"            ++ show range
            putStrLn $ "Class intervals:\n"  ++ show classes
            putStrLn $ "Frequency:\n"        ++ show frequency
            putStrLn $ "Midpoints:\n"        ++ show (getMids classes)
            putStrLn $ "Cumulative f:\n"     ++ show (scanl1 (+) frequency)
            putStrLn $ "Relative f:\n"       ++ show (getRelativeF total frequency)
            putStrLn $ "Class boundaries:\n" ++ show (getClassB classes)
