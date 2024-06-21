module Main where

import MyLib
import Data.List (sort)

--Input parsers
parseList :: String -> [Integer]
parseList = sort . map read . words . map (\x -> if x == ',' then ' ' else x)

parsetoInt :: String -> Integer
parsetoInt = read


main :: IO ()
main = do
    putStrLn "Welcome! Please input data set"
    dataSet <- parseList <$> getLine
    putStrLn "Please input the number of classes"
    numOfC  <- parsetoInt <$> getLine

    let total     = length dataSet
        range     = last dataSet - head dataSet
        cWidth    = getCW range numOfC
        classes   = getClasses cWidth dataSet
        frequency = getF dataSet classes

    putStrLn $ "Sample size:\n"     ++ show total
    putStrLn $ "Range:\n"           ++ show range
    putStrLn $ "Class intervals:\n" ++ show classes
    putStrLn $ "Frequency:\n"       ++ show frequency
    putStrLn $ "Midpoints:\n"       ++ show (getMids classes)
    putStrLn $ "Cumulative f:\n"    ++ show (scanl1 (+) frequency)
    putStrLn $ "Relative f:\n"      ++ show (getRelativeF total frequency)
    putStrLn $ "Class bounderies:\n"++ show (getClassB classes)