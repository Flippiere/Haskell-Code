module SOF3PaperOne2023 where
import Data.List (sort, sortBy)
import Data.Char (digitToInt, intToDigit)

{-
## Question 1

Define a function that has as input three `Integer`s and as output a
triple of the same integers, ordered with the lowest first and the
highest last.
-}
q1 :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
q1 x y z = (sort [x, y, z] !! 0,sort [x, y, z] !! 1,sort [x, y, z] !! 2)
test_q1 :: Bool
test_q1 = q1 1 2 3 == (1, 2, 3) && q1 3 2 1 == (1, 2, 3)

{-
## Question 2

A **Pythagorean triple** consists of three non-negative integers, such
that the sum of the squares of the two smallest values equals the
square of the largest value (that is, they can be the sides of a
right-angled triangle).

Define a function to test if three non-negative `Integer` values form
a Pythagorean triple.
-}
q2 :: Integer -> Integer -> Integer -> Bool
q2 x y z = q2' (q1 x y z)
  where
    q2' (x, y, z) = x^2 + y^2 == z^2
test_q2 :: Bool
test_q2 = q2 3 4 5 && not(q2 1 2 3)

{-
## Question 3

Define a function to check if a given value is present in every list
in a list of lists.  `False` should only be returned if there is some
list that does not contain the given value.
-}
q3 :: Eq a => a -> [[a]] -> Bool
q3 x = all (\y -> x `elem` y)
test_q3 :: Bool
test_q3 =    q3 'a' ["all", "lambs", "baa"]
          && not (q3 0 [[0], [1]])
          && q3 "Value" []
{-
## Question 4

Define a function that receives as input two lists.

The first list is a list of pairs, `conversionTable :: [(a, a)]`.  You
may assume that the first elements of each pair in the list are not
repeated as first elements; that is `map fst conversionTable` has no
repeated elements.  If the function is called with an argument that
breaks this assumption its behaviour is not specified.

The second list is a list of elements, with type `[a]`.

The output of the function is the second input list, but with any
element that occurs as the first element of a pair in the
conversion table replaced by the second element of the pair.
-}
q4 :: Eq a => [(a, a)] -> [a] -> [a]
q4 tupList list = q4' tupList list tupList
  where
    q4' _ [] tupList = []
    q4' [] (x:xs) tupList = x: q4' tupList xs tupList
    q4' ((a,b):ys) (x:xs) tupList | a == x = b: q4' tupList xs tupList
                                  | otherwise = q4' ys (x:xs) tupList
test_q4 :: Bool
test_q4 =    q4 [('a', 'A'), ('b', 'R'), ('c', 'C')] "mini cab" == "mini CAR"
          && q4 [(0,9), (2,9) , (4,9)] [0 .. 5] == [9, 1, 9, 3, 9, 5]

{-
## Question 5

Define a function that has as inputs:
1. a base, in the range `[2 .. 10]`, and
2. a `String` of digits (a digit is a `Char`acter in the range `['0' .. '9']`)

and which outputs the number represented by the digit string, in that base.
-}
q5 :: Int -> String -> Int
q5 base "" = 0
q5 base str = digitToInt (head str) * base^(length str - 1) + q5 base (tail str)
test_q5 :: Bool
test_q5 =    q5 2 "100" ==   4 -- 100 (base 2) is   4 (base 10)
          && q5 5 "401" == 101 -- 401 (base 5) is 101 (base 10)

{-
## Question 6

Define a function that converts an `Int` to a string of digits in a
given base, with the same assumptions as in Question 5.  The first
input is the base, and the second argument is the integer to convert
to a digit-string.
-}
q6 :: Int -> Int -> String
q6 base num = reverse (q6' base num)
  where
    q6' base 0 = []
    q6' base num = intToDigit (snd (num `divMod` base)) : q6' base (fst (num `divMod` base))
test_q6 = q6 10 123 == "123" && q6 2 6 == "110"


{-
## Question 7

Consider the following type of binary trees:
-}
data BinTree a = Leaf | Node (BinTree a) a (BinTree a)
  deriving Show
{-
Define a function to convert a `BinTree` to a list in **breadth-first**
order.
-}
q7 :: BinTree a -> [a]
q7 Leaf = []
q7 (Node x y z) = map fst (sortBy (\(_,a) (_,b) -> compare a b) (q7' (Node x y z) 0))
  where
    q7' Leaf _ = []
    q7' (Node x y z) depth = [(y,depth)] ++ q7' x (depth+1) ++ q7' z (depth+1)

test_q7 :: Bool
test_q7 =
  let
    bt = Node (Node (Node Leaf
                          3
                          Leaf)
                    1
                    (Node (Node Leaf
                                6
                                Leaf)
                          4
                          Leaf))
              0
              (Node Leaf
                    2
                    (Node Leaf
                          5
                          Leaf))
  in
       q7 bt == [0 .. 6] -- breadth-first order
    && q7 bt /= [0, 1, 3, 4, 6, 2, 5] -- depth-first order

{-
## Question 8

Define a function that generates permutations of lists.

The inputs are:

1. A permutation, represented as a bijection (that is, a total,
   one-to-one, onto function) from `[0 .. n - 1]` to itself, where `n`
   is the length of the list to be permuted.  The permutation maps
   **from new** indices **to old** indices.
2. the list to be permuted.

A permutation is a function that maps **from new** indices **to old** indices.
-}
q8 :: (Int -> Int) -> [a] -> [a]
q8 f list = map fst (sortBy (\(_,a) (_,b) -> compare a b) (map (\(a,b) -> (a,f(b))) (tagger list 0 f)))
  where
    tagger [] _ _ = []
    tagger (x:xs) num f = (x,f(num)) : tagger xs (num+1) f

test_q8 :: Bool
test_q8 =
  let -- example permutation
    p 0 = 3
    p 1 = 1
    p 2 = 0
    p 3 = 2
  in -- example use of a permutation
    q8 p "abcd" == "dbac"