fizzBuzz :: Int -> Int -> [String]
fizzBuzz a b = [
    if mod x 15 == 0 
        then "FizzBuzz" 
    else if mod x 3 == 0 
        then "Fizz" 
    else if mod x 5 == 0 
        then "Buzz" 
    else 
        show x 
    | x <- [a..b]]


main = do
    print $ fizzBuzz 1 100
