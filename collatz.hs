import Data.Char
import Data.List

nextCollatz :: Int -> Int
nextCollatz x = if (even x == 0) then (quot x 2) else (3 * x + 1)

collatz :: Int -> [Int]
collatz n = if n == 4 then [4, 2, 1] else ([n] ++ collatz p) where p = nextCollatz n

main = do
    print $ collatz 11
    print $ collatz 5
    print $ collatz 1
