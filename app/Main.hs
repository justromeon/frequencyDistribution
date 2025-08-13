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
    let parsedNums = parseDataSet content
    if null parsedNums
      then hPutStrLn stderr "Error: The file contains no valid numbers. Please check the file content."
      else processNums parsedNums

interactiveMode :: IO ()
interactiveMode = do
    putStrLn "Welcome! Please input data set (separated by spaces or commas):"
    input <- getLine
    let numbers = parseDataSet input
    if null numbers
      then putStrLn "Program exiting due to invalid input."
      else processNums numbers

processNums :: [Integer] -> IO ()
processNums numbers = do
    putStrLn "Please input the number of classes:"
    classNumInp <- getLine
    case parseClassNum classNumInp of
        Nothing -> putStrLn "Error: Invalid number of classes provided. Program exiting."
        Just clasNum -> do
            let sampleSize     = length numbers
                range          = last numbers - head numbers
                width          = classWidth range clasNum
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
