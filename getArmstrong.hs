import Data.Char

num2Digits :: Int -> [Int]
num2Digits = map digitToInt . show

isArmstrong n = n == sum (map (^p) d)
                               where d = num2Digits n
                                     p = length d
getArmstrong :: Int -> Int -> [Int]
getArmstrong a b = filter isArmstrong [a..b]

main = do
    print $ getArmstrong 100 100000
