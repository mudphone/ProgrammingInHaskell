module Ch2 where

-- 3
n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]


-- 4
lastV1 :: [a] -> a
lastV1 xs = head $ drop (n - 1) xs
            where n = length xs

lastV2 :: [a] -> a
lastV2 (x:[]) = x
lastV2 (x:xs) = lastV2 xs

lastV3 :: [a] -> a
lastV3 xs = head $ reverse xs

lastV4 :: [a] -> a
lastV4 xs = xs !! (n - 1)
            where n = length xs

lastV5 :: [a] -> a
lastV5 [x] = x
lastV5 xs = lastV5 $ tail xs


-- 5
initV1 :: [a] -> [a]
initV1 xs = reverse $ tail $ reverse xs

initV2 :: [a] -> [a]
initV2 xs = reverse $ drop 1 $ reverse xs

initV3 :: [a] -> [a]
initV3 xs = take (n - 1) xs
            where n = length xs
