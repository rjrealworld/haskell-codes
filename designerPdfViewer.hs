import Data.List
import Data.Char

position :: Char -> Int
position ch = (fromEnum ch) - (fromEnum 'a')

tallest :: [Char] -> [Int] -> Int
tallest word letterHeights = maximum $ [letterHeights !! (position ch) | ch <- word]

atoi :: String -> Int
atoi s = read s :: Int

main = do
    input <- getLine
    word <- getLine
    let answer = (length word) * (tallest word $ map atoi $ words input)
    print $ answer
