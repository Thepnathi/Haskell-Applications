import System.Environment
import System.IO

maze_path = "maze2.txt"

-- Useful code from Lecture 25
-- You may use this freely in your solutions

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char =
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze

---- Part A

-- Question 1

get_maze :: String -> IO [String]
get_maze input = do
            maze <- readFile input
            return (lines maze)

-- Question 2

print_maze :: [String] -> IO ()
print_maze input = do
              let maze = unlines input
              putStrLn maze

-- Question 3

is_wall :: [String] -> (Int, Int) -> Bool
is_wall maze (x,y)
                |(get maze x y) == '#' = True
                | otherwise = False

is_wall maze (x,y) = if (get maze x y == '#') then True else False



-- Question 4

place_player :: [String] -> (Int, Int) -> [String]
place_player maze (x,y) = set maze x y '@'
-- NOTE: can we place it on the wall?

---- Part B

-- Question 5

-- 'w' - up, 's' - down, 'a' - left, 'd' - right
move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) direc
                | direc == 'w' = (x, y - 1) -- move up
                | direc == 's' = (x, y + 1) -- move down
                | direc == 'a' = (x - 1, y) -- move left
                | direc == 'd' = (x + 1, y) -- move right
                | otherwise = (x,y) -- move restricted

-- Question 6

can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move maze (x, y) direc
                      | is_wall maze (move (x, y) direc) == True = False -- if there is a wall return false
                      | otherwise = True  -- otherwise, return true

-- Question 7

-- basically allow the player to move to the new position
game_loop :: [String] -> (Int, Int) -> IO ()
game_loop maze (x, y) = do
                let newCord = place_player maze (x, y)
                print_maze newCord
                movement <- getChar -- ask the user the direction
                putStrLn "\n"
                if can_move maze (x, y) movement == True
                  then game_loop maze (move (x, y) movement)
                else -- returns original position
                  game_loop maze (x, y)

---- Part C

-- Question 8

get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path maze (a, b) (x, y) = error "Not implemented"


{- Algorithms for the work-
List of functions requires:





-}

-- Question 9

main :: IO ()
main = error "Not implemented"
