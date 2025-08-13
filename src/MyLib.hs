module MyLib where

import Control.Arrow ((***))
import Control.Monad (ap)
import Data.List
import Data.Maybe
import Text.Read

--Helper functions
getCW :: Integer -> Integer -> Integer
getCW r nc
    | isInt result = round result
    | otherwise    = ceiling result
  where
    isInt  = ap (==) (fromInteger . round)
    result = fromIntegral r / fromIntegral nc :: Double

getClasses :: Integer -> [Integer] -> [(Integer,Integer)]
getClasses cw ds = zip lowerLimits upperLimits
  where
    lowerLimits = takeWhile (<=highest) $ iterate (+cw) (head ds)
    upperLimits = take (length lowerLimits) $ iterate (+cw) $ (lowerLimits !! 1) - 1
    highest     = last ds

getMids :: [(Integer,Integer)] -> [Float]
getMids = map ((/2) . fromIntegral)
        . uncurry (zipWith (+))
        . unzip

getF :: [Integer] -> [(Integer,Integer)] -> [Integer]
getF ds = map ((toInteger . length) . includes ds)
         where
           includes list tup = [x | x <- list , x >= fst tup, x <= snd tup]

getRelativeF :: Int -> [Integer] -> [Float]
getRelativeF total freq = map (/t') f'
  where
    t' = fromIntegral total
    f' = map fromIntegral freq

getClassB :: [(Integer, Integer)] -> [(Double, Double)]
getClassB = map (lwrBound *** upprBound)
  where
    lwrBound  = subtract 0.5 . fromInteger
    upprBound = (+ 0.5) . fromInteger

parseNumbers :: String -> [Integer]
parseNumbers = sort . mapMaybe readMaybe . words . map (\x -> if x == ',' then ' ' else x)

parseClassNum :: String -> Maybe Integer
parseClassNum = readMaybe
