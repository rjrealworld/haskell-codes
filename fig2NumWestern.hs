import Data.Char
import Data.List


ones :: (Integral a, Ord a) => a -> String
ones n 
    | n > 0 && n < 10 = onsies !! fromIntegral(n - 1)
    | otherwise       = error $ "ones: not a one-digit value"
    where
        onsies = ["one", "two", "three", "four", "five", 
                    "six", "seven", "eight", "nine"]


tens:: (Integral a, Ord a) => a -> String
tens n
    | n > 0 && n < 10 = tensies !! fromIntegral(n - 1)
    | otherwise       = error $ "tens: not a tens place value"
    where
        tensies = ["ten", "twenty", "thirty", "forty", "fifty", 
                    "sixty", "seventy", "eighty", "ninety"]


teens:: (Integral a, Ord a) => a -> String
teens n
    | n > 9 && n < 20 = teensies !! fromIntegral(n - 10)
    | otherwise       = error $ "teens: not a teen value"
    where
        teensies = ["ten", "eleven", "twelve", "thirteen", "fourteen", 
                    "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]


groups :: [String]
groups = ["", " thousand", " million", " billion", " trillion"]


group2Word :: Integral a => a -> String
group2Word n
    | n == 0    = ""
    | n < 10    = ones n
    | n < 20    = teens n
    | n < 100   = tens (quot n 10) ++ ' ' : (group2Word $ rem n 10)
    | n < 1000  = ones (quot n 100) ++ " hundred " ++ (group2Word $ rem n 100)
    | otherwise = error ("Not a 3-digit group")


splitNum :: Integral a => a -> [a]
splitNum n
    | q == 0 = [n]
    | otherwise = r : splitNum q
    where
        (q,r) = quotRem n 1000


fig2Word :: (Integral a, Ord a) => a -> String
fig2Word n
    | n == 0     = "zero"
    | n >= 10^15 = error "fig2Word:Doesn't support number more than trillions"
    | otherwise  = concat $ intersperse " " $ reverse $ 
                    zipWith (++) wordGroups groups
    where
        wordGroups = map group2Word $ splitNum n


main = do
    print $ fig2Word 0
    print $ fig2Word 9 
    print $ fig2Word 16 
    print $ fig2Word 790
    print $ fig2Word 9056
    print $ fig2Word 79810
