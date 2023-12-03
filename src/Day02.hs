module Day02 where

import Data.List.Split (chunksOf, splitOn)

rgb :: [String]
rgb = ["red", "green", "blue"] 

colorInBounds :: String -> Int -> Maybe Bool
colorInBounds x y
  | x == "red"   = Just $ y <= 12
  | x == "green" = Just $ y <= 13
  | x == "blue"  = Just $ y <= 14
  | otherwise    = Nothing

cubesByColorPerSet :: String -> [[String]] -> [Int]
cubesByColorPerSet c cs =
  map read
    $ filter (not . null)
    $ map (\x -> filter (\_ -> last x == c) (head x)) cs

gameId :: String -> Int
gameId g = read $ last $ words $ head $ splitOn ":" g

setValid :: [[String]] -> Bool
setValid s = all ((== Just True) . (\y -> colorInBounds y (sum $ cubesByColorPerSet y s))) rgb

gameValid :: [[[String]]] -> Bool
gameValid = all setValid

parseGame :: String -> [[[String]]]
parseGame g = setPairs
 where
  sets = splitOn ";" $ filter (/= ':') $ filter (/= ',') g
  setPairs = map (chunksOf 2 . words) sets

minCubes :: [[[String]]] -> [[Int]]
minCubes g = map (\z -> maximum $ map (\x -> head $ map (\_ -> cubesByColorPerSet z x) z) g) rgb

powCubes :: [[Int]] -> [Int]
powCubes = foldr1 (zipWith (*))

partOne :: [String] -> Int
partOne gs = sum $ map (\x -> if gameValid (parseGame x) then gameId x else 0) gs

partTwo :: [String] -> Int
partTwo gs = head (foldr1 (zipWith (+)) (map (powCubes . minCubes . parseGame) gs))

day02 :: IO ()
day02 = do
  inputLines <- lines <$> readFile "data/day02.txt"

  let p1 = partOne inputLines
  putStrLn $ "Part 1: " ++ show p1

  let p2 = partTwo inputLines
  putStrLn $ "Part 2: " ++ show p2
