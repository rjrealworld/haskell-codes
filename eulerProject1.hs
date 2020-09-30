euler1 :: Int -> Int
euler1 n = sum $ [x | x <- [1..(n-1)], (mod x 5) == 0 || (mod x 3) == 0]

main = do
    print $ euler1 100
