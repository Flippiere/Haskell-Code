module ExercismSolutions where
import Data.Function(on)
import Data.List(nub, sort)
import Data.Map (Map, fromList)
import Data.Char (toUpper, toLower, digitToInt, intToDigit)
import Data.List (delete, transpose)
import TcRnMonad (WhereFrom)
type REPLACE_THIS_TYPE = ()


-- Solutions to some of the Exercises avaliable on exercism.org

data Tree = Leaf Int | Node Tree Int Tree deriving Show

insertX :: Int -> Tree -> Tree
insertX x (Leaf 0) = Leaf x
insertX x (Leaf z) | x >= z = Node (Leaf 0) z (Leaf x)
                   | otherwise = Node (Leaf x) z (Leaf 0)
insertX x (Node z y zz) | x >= y = Node z y (insertX x zz)
                        | otherwise = Node (insertX x z) y zz

listTree :: Tree -> [Int]
listTree (Leaf 0) = []
listTree (Leaf z) = [z]
listTree (Node z y zz) =  (listTree z) ++ [y] ++ (listTree zz)

collatz :: Integer -> Maybe Integer
collatz n = collatz' n 0
  where
    collatz' 1 x = Just x
    collatz' n x | n <= 0 = Nothing
                 | even n = collatz' (n `div` 2) (x+1)
                 | otherwise = collatz' ((n*3)+1) (x+1)

toRNA :: String -> Either Char String
toRNA x = toRNA' x ""
  where
    toRNA' [] y = Right (reverse y)
    toRNA' (x:xs) y | x == 'G' = toRNA' xs ('C':y)
                    | x == 'C' = toRNA' xs ('G':y)
                    | x == 'T' = toRNA' xs ('A':y)
                    | x == 'A' = toRNA' xs ('U':y)
                    | otherwise = Left x

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = nucleotideCounts' xs 0 0 0 0
  where
    nucleotideCounts' [] a c g t = Right (Data.Map.fromList [(A,a),(C,c),(G,g),(T,t)])
    nucleotideCounts' (x:xs) a c g t | x == 'A' = nucleotideCounts' xs (a+1) c g t
                                     | x == 'C' = nucleotideCounts' xs a (c+1) g t
                                     | x == 'G' = nucleotideCounts' xs a c (g+1) t
                                     | x == 'T' = nucleotideCounts' xs a c g (t+1)
                                     | otherwise = Left "Error"

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum (nub [y | x <- factors, x > 0, y <- map (x *) [1..(limit `div` x)], y /= limit])

square :: Integer -> Maybe Integer
square n | 1 < n = Nothing
         | n < 65 = Just (2^(n-1))
         | otherwise = Nothing

total :: Integer
total = sum (map (\n -> 2^(n-1)) [1..64])

abbreviate :: String -> String
abbreviate [] = []
abbreviate (x:xs) = toUpper x : abbreviate' xs
  where
    abbreviate' [] = []
    abbreviate' (x:xs) | x == ' ' = abbreviate xs
                      | x `elem` ['A'..'Z'] = x : abbreviate' xs
                      | otherwise = abbreviate' xs

discard :: (a -> Bool) -> [a] -> [a]
discard p [] = []
discard p (x:xs) | p x = discard p xs
                 | otherwise = x : discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep p [] = []
keep p (x:xs) | p x = x : keep p xs
              | otherwise = keep p xs

anagramsFor :: String -> [String] -> [String]
anagramsFor xs [] = []
anagramsFor xs (x:xss) | map toUpper xs == map toUpper x = anagramsFor xs xss
                       | anagramCheck xs x = x : anagramsFor xs xss
                       | otherwise = anagramsFor xs xss
  where
    anagramCheck [] [] = True
    anagramCheck a [] = False
    anagramCheck [] x = False
    anagramCheck (a:xs) x | toLower a `elem` x = anagramCheck xs (delete (toLower a) x)
                          | toUpper a `elem` x = anagramCheck xs (delete (toUpper a) x)
                          | otherwise = False

data Clock = Time Int Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Time ((hour + (min `div` 60)) `mod` 24) (min `mod` 60)

toString :: Clock -> String
toString (Time x y) = showT x ++ ":" ++ showT y
  where
    showT x | x < 10 = "0" ++ show x
            | otherwise = show x

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Time x y) = Time ((hour + x + ((min+y) `div` 60)) `mod` 24) ((min+y) `mod` 60)

encode :: String -> String
encode x | length x `div` length x /= length x = concat (transpose (rectangle (normalise x) (length x `div` length x) ((length x `div` length x) + 1)))
         | otherwise = concat (transpose (rectangle (normalise x) (length x `div` length x) (length x `div` length x)))
  where
    normalise [] = []
    normalise (x:xs) | x `elem` ['a'..'z'] = x : encode xs
                     | x `elem` ['A'..'Z'] = toLower x : encode xs
                     | otherwise = encode xs
    rectangle str 0 c = [str ++ replicate (c - length str) ' ']
    rectangle str r c = take c str : rectangle (reverse (take (length str - c) (reverse str))) (r-1) c

isValid :: String -> Bool
isValid n = (sum (map fUNction (odds (removeWs n))) + sum (map digitToInt (evens (removeWs n)))) `mod` 10 == 0 && length (removeWs n) > 1
  where
    evens (x:xs) = x:odds xs
    evens _ = []
    odds (_:xs) = evens xs
    odds _ = []
    removeWs xs = reverse (filter (\x -> x `elem` ['0'..'9']) xs)
    fUNction x = if (digitToInt x*2) > 9 then (digitToInt x*2)-9 else digitToInt x*2

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = nth' n [2] 3
  where
    nth' n lstOfPrimes x | length lstOfPrimes == n = Just (head lstOfPrimes)
                         | donkeyKong lstOfPrimes x = nth' n (x:lstOfPrimes) (x+1)
                         | otherwise = nth' n lstOfPrimes (x+1)
    donkeyKong [] x = True
    donkeyKong (y:ys) x | x `mod` y == 0 = False
                        | otherwise = donkeyKong ys x

annotate :: [String] -> [String]
annotate board = annotate' 0 0 board board
  where
    annotate' xI yI cleanBoard [] = cleanBoard
    annotate' xI yI cleanBoard (x:xs) = iterString xI yI cleanBoard x : annotate' 0 (yI+1) cleanBoard xs
    iterString xI yI cleanBoard [] = []
    iterString xI yI cleanBoard (x:xs) | x == '*' = iterString (xI+1) yI (anotate'' xI yI 0 0 cleanBoard) xs
                                       | otherwise = iterString (xI+1) yI cleanBoard xs
    anotate'' xI yI xI' yI' [] = []
    anotate'' xI yI xI' yI' (cx:cxs) = anotate''' xI yI xI' yI' cx : anotate'' xI yI 0 (yI'+1) cxs
    anotate''' xI yI xI' yI' [] = []
    anotate''' xI yI xI' yI' (x:xs) | xI + 1 >= xI' && xI - 1 <= xI' && yI + 1 >= yI' && yI - 1 <= yI' && x /= '*' = (if x == '.' then '1' else intToDigit (digitToInt x + 1)) : anotate''' xI yI (xI' + 1) yI' xs
                                    | otherwise = x : anotate''' xI yI (xI' + 1) yI' xs