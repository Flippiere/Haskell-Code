module SOF3PaperTwo2023 where

type REPLACE_THIS_TYPE = () -- DO NOT ALTER

{-
## Question 1

In this question you are asked to define types, as well as values.
Each type, *Name*, is given a default implementation: `type Name =
REPLACE_THIS_TYPE`.  You may change the keyword `type` to either
`newtype` or `data` if appropriate, and you should replace
`REPLACE_THIS_TYPE`, by a suitable type expression.  You may derive any
type classes, such as `Show`, that will be useful.

Marks will be mostly given for how well the type protects the
programmer from mistakes, for its utility to the programmer, and also
partly for the run-time efficiency of the type.

A timetabling application allows a college to construct a timetable
for each week.

A week consists of five working days, with the usual English names.
Each day consists of six slots, called *A* upto *F*.
-}
data Day = Mon | Tue | Wed | Thu | Fri deriving (Show, Enum)
data Period = A | B | C | D | E | F deriving (Show, Enum)
{-
### 1(i)

Define a type to represent *slots*, which are a period on a particular
day of the week (for example, "Tuesday, Period C").
-}
newtype Slot = Slot (Day, Period) deriving Show
{-
### 1(ii)

Define a list, `slots`, to represent all possible slots during the
week.  The list should be in the order in which the slots occur.
-}
slots :: [Slot]
slots = [Slot (day,per)|day <- [(Mon)..Fri],per <- [(A)..F]]
{-
The following definitions are for the convenience of this question: in
practice they would be more detailed.
-}
data Room = R1 | R2 | R3 | R4 deriving (Show, Enum, Eq)
newtype Person = Person Int deriving (Show, Eq, Ord)
{-
A timetable allocates a possibly-empty collection of people to each
room in each slot across a week.

### 1(iii)

Define a type to represent a timetable for a week.  The timetable
should associate a slot and a room with the collection of people who
should be in that room during that slot.
-}

newtype Timetable = Timetable [((Slot, Room), [Person])] deriving Show

{-
### 1(iv)

Define a value to represent an empty timetable for a block.
-}
emptyTimetable :: Timetable
emptyTimetable = Timetable []

{-
### 1(v)

A timetable should have the property that it allows for Wednesday
afternoons (Periods D, E, and F) for sports.  Define a function
that returns `True` if no activities are scheduled on a Wednesday
afternoon, and `False` otherwise.
-}
hasSports :: Timetable -> Bool
hasSports (Timetable x) = not (any (\((Slot (a,b),_),_) -> (fromEnum a, fromEnum b) `elem` [(fromEnum Wed,fromEnum slt)| slt <- [D,E,F]]) x)

{-
## Question 2

Consider a concert programme, listing pieces to be played during the
concert, plus their durations. Two possible datatypes that could be
used to hold this data are:

* A list of pieces in order, and a function to report the duration of
  each piece, in seconds. (You may assume that `lengthF` is defined
  for every string in `piecesF`.)
-}
data ProgF = ProgF {piecesF :: [String], lengthF :: String -> Int}
{-
* A list of pieces in order, where each piece is paired up with a
  timing, giving the duration in minutes and seconds.
-}
data ProgL = ProgL {getProgL :: [(String, (Int, Int))]}

{-
For each of the tasks 2(i) and 2(ii)  below, give **two*
solutions, one using each type.

### 2(i)

Report the number of pieces in the concert.
-}
sizeF :: ProgF -> Int
sizeL :: ProgL -> Int
sizeF prgF = length (piecesF prgF)
sizeL prgL = length (getProgL prgL)
{-
### 2(ii)

Report the total duration of the music to be played in the concert, in
seconds.
-}
durationF :: ProgF -> Int
durationL :: ProgL -> Int
durationF prgF = sum (map (lengthF prgF) (piecesF prgF))
durationL prgL = sum [(mins * 60)+sec |(_, (mins, sec)) <- getProgL prgL]

{-
### 2(iii)

Define two functions to convert between representations.
-}
f2l :: ProgF -> ProgL
l2f :: ProgL -> ProgF
f2l prgF = ProgL [(str,(lengthF prgF str `div` 60,lengthF prgF str `mod` 60))|str <- piecesF prgF]
l2f prgL = toProgF [] (\y -> 0) prgL
  where
    toProgF sngList f (ProgL []) = ProgF sngList f
    toProgF sngList f (ProgL (x:xs)) = toProgF (sngList ++ [fst x]) (\y -> if y == fst x then (fst (snd x)*60 + snd (snd x)) else f y) (ProgL xs)

{-
## Question 3

Recall the definition of the `ProofLayout` type constructor:
-}
infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
{-
Consider the `Prelude` function:
```haskell
const :: a -> b -> a
const u v = u -- const.0
```

### 3(i)
Prove the lemma:
```haskell
forall x :: a, y :: b, z :: c {const (const x) y z == x}
```
-}
const2Lemma :: a -> b -> c -> ProofLayout a
const2Lemma x y z = -- Base Case
  const (const x) y z
  :=: -- const.0
  const x z
  :=: -- const.0
  x
  :=: QED
{-
### 3(ii)
Consider the further `Prelude` functions:
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z                  -- foldr.0
foldr f z (x:xs) = f x (foldr f z xs) -- foldr.1

null :: [a] -> Bool
null []     = True  -- null.0
null (x:xs) = False -- null.1
```

Prove the theorem:
```haskell
forall xs :: [a] {foldr (const (const False)) True xs == null xs}
```

**Hint** Use structural induction over lists and the lemma `const2Lemma`.

You may assume the following strictness rules:
```haskell
foldr _ _ _|_ :=: _|_ -- foldr strict
null _|_ :=: _|_ -- null strict
```
-}
foldrNullTheorem :: [a] -> ProofLayout Bool
foldrNullTheorem (x:xs) = -- inductive case
  foldr (const (const False)) True (x:xs)
  :=: -- foldr.1
  (const (const False)) x (foldr (const (const False)) True xs)
  :=: -- Proof by Induction
  (const False) (foldr (const (const False)) True xs)
  :=: -- (const False).(*)
  False
  :=: -- null.1
  null (x:xs)
  :=: QED

foldrNullTheorem [] = -- null case
  foldr (const (const False)) True []
  :=: -- foldr.0
  True
  :=: -- null.0
  null []
  :=: QED