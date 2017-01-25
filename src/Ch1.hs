module Ch1 where

-- 1
double x = x + x
-- alternate double
double' x = x * 2


-- 2
-- sum [x] == x
sumSingle [x] = x
sumEqual xs = sumSingle xs == sum xs


-- 3
product' [] = 1
product' (x : xs) = x * (product' xs)


-- 4
-- Reversed quicksort
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b >  x]


-- 5
-- Loss of equal comparisons
qsort' [] = []
qsort' (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                  smaller = [a | a <- xs, a < x]
                  larger  = [b | b <- xs, b > x]
