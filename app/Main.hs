module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import MyLib

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:classNum:_) -> fileMode filePath classNum
        _                     -> interactiveMode

fileMode :: FilePath -> String -> IO ()
fileMode filePath classNumStr = do
    content <- readFile filePath
    
    let numbers  = parseDataSet content
        classNum = parseClassNum classNumStr

    case (numbers, classNum) of
        (Just [], _) -> hPutStrLn stderr "Error: The file contains no valid numbers."
        (Nothing, _) -> hPutStrLn stderr "Error: The file contains invalid characters. Numbers must be separated by spaces or commas."
        (_, Nothing) -> hPutStrLn stderr "Error: Invalid number of classes provided in arguments. Program exiting."
        (Just n, Just c) -> processNums n c

interactiveMode :: IO ()
interactiveMode = do
    putStrLn "Welcome! Please input data set (separated by spaces or commas):"
    numbersInput <- getLine
    putStrLn "Please input the number of classes:"
    classNumInput <- getLine
    
    let numbers  = parseDataSet numbersInput
        classNum = parseClassNum classNumInput

    case (numbers, classNum) of
        (Just [], _) -> putStrLn "Program exiting due to no valid numbers provided."
        (Nothing, _) -> putStrLn "Program exiting due to invalid data set input."
        (_, Nothing) -> putStrLn "Program exiting due to invalid number of classes provided."
        (Just n, Just c) -> processNums n c

processNums :: [Integer] -> Integer -> IO ()
processNums numbers classNum = do
    let sampleSize     = length numbers
        range          = last numbers - head numbers
        width          = classWidth range classNum
        classesRes     = classes width numbers
        frequenciesRes = frequencies numbers classesRes
    
    mapM_ putStrLn
      [ "\nSample size: "       ++ show sampleSize
      , "\nRange: "             ++ show range
      , "\nClass Width: "       ++ show width
      , "\nClass intervals:\n"  ++ show classesRes
      , "\nFrequencies:\n"      ++ show frequenciesRes
      , "\nMidpoints:\n"        ++ show (midpoints classesRes)
      , "\nCumulative f:\n"     ++ show (scanl1 (+) frequenciesRes)
      , "\nRelative f:\n"       ++ show (relativeFreqs sampleSize frequenciesRes)
      , "\nClass boundaries:\n" ++ show (classBounds classesRes)
      ]