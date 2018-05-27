import Data.List
import System.IO.Unsafe

---- Data Paths
-- Remember to use double backslashes for windows paths


-- NOTE: QUESTION - What director do we need to save as, when we launch the assignment

data_path :: String
data_path = "data.csv"

sales_path :: String
sales_path = "sales.txt"

---- Part A
-- To build simple tools for manipulating price series using map and filter
-- NOTE: Leave out the errors

convert_single :: [Float] -> Float -> [Float]
convert_single series exchange = map (\ series -> series * exchange) series


convert_all :: [[Float]] -> Float -> [[Float]]
-- remember to use (get_data) to test
convert_all series exchange = map(map(\ series -> series * exchange)) series

-- days_above :: [Float] -> Float -> Int
days_above series amount = length(filter(>= amount) series)


days_between :: [Float] -> Float -> Float -> Int
days_between series lower upper = length(filter (\ series -> series >= lower && series <= upper) series)


----- Part B Implement a simple strategy usinbg foldl and scanl

{-

Task: NOTE: Buy low and sell high
Strategy 



-}

modify_position :: Float -> Float -> Float -> Float -> Float
modify_position buy_price sell_price position price =
    error "Not implemented"

final_position :: Float -> Float -> [Float] -> Float
final_position buy_price sell_price series =
    error "Not implemented"


daily_position :: Float -> Float -> [Float] -> [Float]
daily_position buy_price sell_price series =
    error "Not implemented"


daily_holding_values :: Float -> Float -> [Float] -> [Float]
daily_holding_values buy_price sell_price series =
    error "Not implemented"


---- Part C

sales_final_position :: [String] -> [Float]
sales_final_position sales =
    error "Not implemented"


sales_holding_value :: [String] -> [[Float]] -> [Float]
sales_holding_value sales series =
    error "Not implemented"


---- Code for loading the data -- do not modify!

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep [] = []
splitOn sep list =
    let
        ne_sep = \ x -> x /= sep
        first = takeWhile ne_sep list
        second = dropWhile ne_sep list
        rest = if null second then [] else tail second
    in
        first : splitOn sep rest

get_data :: [[Float]]
get_data =
    unsafePerformIO $
        do
            file <- readFile data_path
            let line_split = splitOn '\n' file
                remove_r = map (filter (/='\r')) line_split
                full_split = map (splitOn ',') remove_r
                converted = map (map (read :: String -> Float)) full_split
            return converted

get_series :: Int -> [Float]
get_series n =
    if n >= 0 && n < 10
    then (transpose get_data) !! n
    else error ("There is no series " ++ show n)

get_sales :: [String]
get_sales =
    unsafePerformIO $
        do
            file <- readFile sales_path
            return $ map (filter (/='\r')) $ splitOn '\n' file

short_data :: [[Float]]
short_data = take 10 get_data

get_short_series :: Int -> [Float]
get_short_series n = take 10 (get_series n)
