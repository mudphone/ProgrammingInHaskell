module Ch5 where

-- 1
squaresTo100 = [x^2 | x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]
           
-- 4
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : (replicate' (n - 1) x)

replicateComp :: Int -> a -> [a]
replicateComp n x = [x | _ <- [1..n]]

-- 5
isPyth :: Int -> Int -> Int -> Bool
isPyth x y z = x^2 + y^2 == z^2

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n],
                       y <- [1..n],
                       z <- [1..n],
                       isPyth x y z]

-- 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum . factors $ x) == 2 * x ]

-- 7
comp1 = [(x,y) | x <- [1,2], y <- [3,4]]

comp2 = concat [[(x,y) | x <- [1,2]] | y <- [3,4]]

-- 8
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

-- positions False [True, False, True, False]
-- => [1,3]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- find 'b' [('a',1),('b',2),('c',3),('b',4)]
-- => [2,4]

posFind :: Eq a => a -> [a] -> [Int]
posFind x xs = find x $ zip xs [0..]

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

-- 10
