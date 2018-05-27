{-
  Assignment 1 - COMP105 Assignment 1 - Recursion
  Created:    20/10/17, Author Thepnathi
  Deadline:   1/11/17
-}

-- NOTE: DO NOT USE LIBRARY FUNCTION, also
-- NOTE: NO LIST COMPREHENSION
-- NOTE: However, we can use head and tails for part A and B
-- NOTE: We can use mod, div, legnth, head and tail for part C


------------------------------------------------------------------------------------------------
-- Part A: Decoder
-- Question 1 : change a character to integer

char_to_int '1' = 1     -- change inputted char to integer
char_to_int '2' = 2
char_to_int '3' = 3
char_to_int '4' = 4
char_to_int '5' = 5
char_to_int '6' = 6
char_to_int '7' = 7
char_to_int '8' = 8
char_to_int '9' = 9
char_to_int '0' = 0

-- Question 2: Repeat a single letter character by n times. repeat_char c n

-- repeat_char :: (Eq x, Num x) => [a] -> x -> [a]
repeat_char _ 0 = []  -- set to stop the recursion
repeat_char c n = c : repeat_char c (n-1) -- increment the character n times

-- Question 3: Decode a string, eg.. "a2b2" should return "aabb"

-- pattern matching (x:xs)

decode [] = []
decode (x:y:xs) = repeat_char x (char_to_int y) ++ decode xs

------------------------------------------------------------------------------------------------
-- Part B: Build a encoder for simple repeat encoding.

-- Question 4: Write a function int_to_char iint - takes n 0 to 9 and return corresponding
-- character. 1 should return '1'

int_to_char 1 = '1'
int_to_char 2 = '2'
int_to_char 3 = '3'
int_to_char 4 = '4'
int_to_char 5 = '5'
int_to_char 6 = '6'
int_to_char 7 = '7'
int_to_char 8 = '8'
int_to_char 9 = '9'
int_to_char 0 = '0'


-- Question 5: length_char c. A function to find out how many times a single character
-- occurs in a list of character. eg.. 'length_char 'b' 'bbbcccc' = 3

length_char _ [] = 0
length_char c (x:xs)
                | x == c = 1 + length_char c xs
                | otherwise = 0

-- Question 6: drop_char c string, using one char, this function drop that character
-- from the string e.g. drop_char 'a' "aaabaa" returns "baa"

drop_char _ [] = []
drop_char c (x:xs)
              | x == c = drop_char c xs
              | otherwise = x:xs

-- Question 7: encode string, that encodes a string in simple repeat encoding. "aabb" will
-- be "a2b2", or ... "abcd" will return "a1b1c1d1"
-- putStrLn

encode [] = []
encode (x:xs) = x : [int_to_char (length_char x (x:xs))] ++ encode (drop_char x xs)


-- drop it before it gets pushed into

------------------------------------------------------------------------------------------------
-- Part C: (NOTE: we can use the following functions: mod, div, length, head, and tail without penalty)

-- Question 8: complex_encode string returns non-simple repeat encoding of string.
-- "hello" encodes to "hel2o"


convertion x y = remove_zero (int_to_char (div x y) : []) ++ (int_to_char (mod x y) : [])
-- length_char returns a 'char' value for x divisible by y
-- push the returned 'char' into a list - to form string
-- applies for right side. Then we concatinate them both

remove_zero (x:xs) = drop_char '0' (x:xs) -- call drop_char to drop '0' instance

integer_to_str n
              | n == 1 = [] -- if n is 1 then return nothing
              | n < 99 = convertion n 10
              | n < 999 = convertion n 100
              | otherwise = convertion n 1000 -- will only goes up to 1000s

complex_encode [] = []
complex_encode (x:xs) = x : integer_to_str (length_char x (x:xs)) ++ complex_encode (drop_char x xs)

-- Question 9: complex_encode string - now decode the string in a non-simple way QUESTION

complex_decode [] = []
complex_decode (x:y:xs)
            | if y <= 9 repeat_char x (char_to_int y) ++ complex_decode
            | otherwise = []

-- EXAMPLE "a11b2" --> "aaaaaaaaaaabb"











--- sjsjsj
