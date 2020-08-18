import Data.Text
import Data.Maybe
import qualified Data.Map.Strict as Map

upto20 = ["", "One ", "Two ", "Three ", "Four ", "Five ", "Six ", "Seven ", 
        "Eight ", "Nine ", "Ten ", "Eleven ", "Twelve ", "Thirteen ", "Fourteen ", 
        "Fiften ", "Sixteen ", "Seventeen ", "Eighteen ", "Nineteen "]

tens = ["", "", "Twenty ", "Thirty ", "Forty ", "Fifty ", "Sixty ", 
        "Seventy ", "Eighty ", "Ninety "]

hun = "Hundred "
han = "Hundred and "

--Western System
wValues = [10^12, 10^9, 10^6, 10^3, 1]
wNames = ["Trillion ", "Billion ", "Million ", "Thousand ", ""]
wdenomNames = Map.fromList $ Prelude.zip wValues wNames

-- Indian system
iValues = [10^7, 10^5, 1000, 1]
iNames = ["Crore ", "Lakh ", "Thousand ", ""]
idenomNames = Map.fromList $ Prelude.zip iValues iNames


convert2digits :: Int -> String
convert2digits n | n < 20    = upto20 !! n
convert2digits n | otherwise = (tens !! (div n 10)) ++ (upto20 !! (mod n 10))


convert3digits :: Int -> String
convert3digits n | n < 100        = convert2digits n
convert3digits n | mod n 100 == 0 = convert2digits (div n 100) ++ hun
convert3digits n | otherwise      = convert2digits h ++ han ++ convert2digits tu where
    h = div n 100
    tu = mod n 100


splitToDenoms :: Int -> [Int] -> [(Int, Int)]
splitToDenoms amount denoms | Prelude.length denoms == 1 = [(amount, Prelude.head denoms)]
splitToDenoms amount denoms | otherwise          = (quot amount d, d) : splitToDenoms (mod amount d) (Prelude.tail denoms) where d = Prelude.head denoms


converti :: (Int, Int) -> String
converti (0, b) = ""
converti (a, b) = convert3digits a ++  (Data.Maybe.fromMaybe "" ( Map.lookup b idenomNames))  


convertw :: (Int, Int) -> String
convertw (0, b) = ""
convertw (a, b) = convert3digits a ++  (Data.Maybe.fromMaybe "" ( Map.lookup b wdenomNames))


fig2Words :: Int -> [Int] -> String
fig2Words amount dValues
    | dValues == iValues = Prelude.concat (Prelude.map converti (splitToDenoms amount iValues))
    | dValues == wValues = Prelude.concat (Prelude.map convertw (splitToDenoms amount wValues)) 


main = do
    print $ fig2Words 123456789 wValues
    print $ fig2Words 123456789 iValues
