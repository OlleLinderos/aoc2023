module Day01 where

import Data.Char (isDigit)

digitizeWords :: String -> String
digitizeWords x
  | "one"   == take 3 x = '1' : tail x
  | "two"   == take 3 x = '2' : tail x
  | "three" == take 5 x = '3' : tail x
  | "four"  == take 4 x = '4' : tail x
  | "five"  == take 4 x = '5' : tail x
  | "six"   == take 3 x = '6' : tail x
  | "seven" == take 5 x = '7' : tail x
  | "eight" == take 5 x = '8' : tail x
  | "nine"  == take 4 x = '9' : tail x
  | otherwise = x

digitize :: String -> String -> String
digitize x "" = x
digitize x y = digitize (x ++ f) l
 where
  (f, l) = splitAt 1 $ digitizeWords y

partOne :: [String] -> Int
partOne x = result
 where
  result = sum $ map (\y -> read [head y, last y]) digits
  digits = map (filter isDigit) x

partTwo :: [String] -> Int
partTwo x = result
 where
  result = sum $ map (\y -> read [head y, last y]) digits
  digits = map (filter isDigit . digitize "") x

day01 :: IO ()
day01 = do
  inputLines <- lines <$> readFile "data/day01.txt"

  let p1 = partOne inputLines
  putStrLn $ "Part 1: " ++ show p1

  let p2 = partTwo inputLines
  putStrLn $ "Part 2: " ++ show p2
