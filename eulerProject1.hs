euler1 :: Int -> Int
euler1 n = sum $ [x | x <- [1..(n-1)], (x mod 5) == 0 || (x mod 3) == 0]

main = do
    print $ euler1 100
