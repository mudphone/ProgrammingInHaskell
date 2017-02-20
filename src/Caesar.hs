module Caesar where

import Data.Char

-- The zip function
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- String comprehensions
lowercase :: String -> String
lowercase "" = ""
lowercase (x:xs) = toLower x : lowercase xs

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- The Caesar cipher
-- Encoing and decoding
let2int :: Char -> Int
let2int c | o > 96 = o - ord 'a'
          | otherwise = o - ord 'A'
  where o = ord c

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let go
          | isUpper c = int2let (go - 32)
          | otherwise = c
  where go = ((let2int c + n) `mod` 26)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Frequency tables
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.2, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x asLowers) n | x <- ['a'..'z']]
           where n = lowers asLowers
                 asLowers = lowercase xs

-- Cracking the cipher
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs
