module Ch4 where

-- 1
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
           where n = (length xs) `div` 2


-- 2
thirdVa1 :: [a] -> a
thirdVa1 xs = go xs 0
  where go ys 2 = head ys
        go ys n = go (tail ys) (n + 1)

thirdVa2 :: [a] -> a
thirdVa2 xs = head $ tail $ tail xs

thirdVb :: [a] -> a
thirdVb xs = xs !! 2

thirdVc :: [a] -> a
thirdVc (_ : _ : x : xs) = x


-- 3
safetailVa :: [a] -> [a]
safetailVa xs = if null xs then [] else tail xs

safetailVb :: [a] -> [a]
safetailVb xs | null xs = []
              | otherwise = tail xs

safetailVc :: [a] -> [a]
safetailVc [] = []
safetailVc xs = tail xs


-- 4
disj1 :: Bool -> Bool -> Bool
True `disj1` True = True
True `disj1` False = True
False `disj1` True = True
False `disj1` False = False

disj2 :: Bool -> Bool -> Bool
disj2 True _ = True
disj2 _ True = True
disj2 False _ = False

disj3 :: Bool -> Bool -> Bool
disj3 False b = b
disj3 a _ = a

disj4 :: Bool -> Bool -> Bool
disj4 False b = b
disj4 True _ = True


-- 5
condAnd :: Bool -> Bool -> Bool
condAnd x y = if x then (if y then True else False) else False


-- 6
condAnd2 :: Bool -> Bool -> Bool
condAnd2 x y = if x then y else False


-- 7
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

mult' :: Int -> Int -> Int -> Int
mult' = \x -> (\y -> (\z -> x*y*z))


-- 8
luhnDouble :: Int -> Int
luhnDouble x
  | x2 > 9 = x2 - 9
  | otherwise = x2
  where x2 = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod r 10 == 0
  where c2 = luhnDouble c
        a2 = luhnDouble a
        r = a2 + b + c2 + d
