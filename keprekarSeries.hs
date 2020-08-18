import Data.Char
import Data.List

kaprekar :: Int -> Int
kaprekar n = big - small
  where big = read digits :: Int
        small = read (reverse digits) :: Int
        digits = reverse . sort . show $ n
                  
kaprekarList :: Int -> [Int]
kaprekarList x = if p == 6174 then [x, p] else [x] ++ kaprekarList p where p = kaprekar x

main = do
    print $ kaprekarList 2358
