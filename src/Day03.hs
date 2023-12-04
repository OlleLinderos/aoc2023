module Day03 where

import Data.Char (isDigit)

data Value = Value
  { value' :: String
  , index' :: Int
  , length' :: Int
  , line' :: Int
  , number' :: Bool
  }
  deriving (Show)

extractValues :: Int -> Int -> String -> [Value]
extractValues _ _ [] = []
extractValues idx line (c : cs)
  | '.' == c = extractValues (idx + 1) line cs
  | isSymbol' c = Value [c] idx 1 line False : extractValues (idx + 1) line cs
  | isDigit c =
      let (num, rest) = span isDigit (c : cs)
       in Value num idx (length num) line True : extractValues (idx + length num) line rest
  | otherwise = extractValues (idx + 1) line cs

isSymbol' :: Char -> Bool
isSymbol' = (`elem` ['+', '-', '*', '/', '@', '$', '&', '%', '#', '='])

filterAdjacentNumbers :: Value -> [Value] -> Bool
filterAdjacentNumbers v vs = number' v && any (and . filters) vs
 where
  filters x =
    [ not $ number' x
    , index' x `elem` [(index' v - 1) .. (index' v + length' v)]
    , line' x `elem` [(line' v - 1) .. (line' v + 1)]
    ]

partOne :: [String] -> Int
partOne l = sum valueOfAdjacentNumbers
 where
  valueOfAdjacentNumbers = map (read . value') adjacentNumbers
  adjacentNumbers = filter (`filterAdjacentNumbers` allValues) allValues
  allValues = concatMap (uncurry (extractValues 1)) (zip [1 ..] l)

day03 :: IO ()
day03 = do
  inputLines <- lines <$> readFile "data/day03.txt"

  let p1 = partOne inputLines
  putStrLn $ "Part 1: " ++ show p1

  -- let p2 = partTwo inputLines
  -- putStrLn $ "Part 2: " ++ show p2