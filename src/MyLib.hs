module MyLib where

import Control.Arrow ((***))
import Control.Monad (ap)
import Data.List
import Data.Maybe
import Text.Read

type ClassWidth = Integer
type ClassInterval = (Integer, Integer)
type MidPoint = Double
type Frequency = Integer
type ClassBoundary = (Double, Double)

classWidth :: Integer -> Integer -> ClassWidth
classWidth range classNum
    | isInt result = round result
    | otherwise    = ceiling result
  where
    isInt  = ap (==) (fromInteger . round)
    result = fromIntegral range / fromIntegral classNum :: Double

classes :: ClassWidth -> [Integer] -> [ClassInterval]
classes cw dataset = zip lowerLimits upperLimits
  where
    lowerLimits = takeWhile (<= highest) $ iterate (+ cw) (head dataset)
    upperLimits = take (length lowerLimits) $ iterate (+ cw) (lowerLimits !! 1 - 1)
    highest     = last dataset

midpoints :: [ClassInterval] -> [MidPoint]
midpoints = map ((/2) . fromIntegral)
          . uncurry (zipWith (+))
          . unzip

frequencies :: [Integer] -> [ClassInterval] -> [Frequency]
frequencies dataset = map ((toInteger . length) . includes dataset)
  where
    includes list tup = [x | x <- list, x >= fst tup, x <= snd tup]

relativeFreqs :: Int -> [Frequency] -> [Float]
relativeFreqs sampleSize freq = map (/t') f'
  where
    t' = fromIntegral sampleSize
    f' = map fromIntegral freq

classBounds :: [ClassInterval] -> [ClassBoundary]
classBounds = map (lwrBound *** upprBound)
  where
    lwrBound  = subtract 0.5 . fromInteger
    upprBound = (+ 0.5) . fromInteger

parseDataSet :: String -> [Integer]
parseDataSet = sort . mapMaybe readMaybe . words . map (\x -> if x == ',' then ' ' else x)

parseClassNum :: String -> Maybe Integer
parseClassNum = readMaybe
