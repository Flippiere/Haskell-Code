module Snake where
import GHC.Base (IO(IO))

-- a WIP implementation of the game Snake in Haskell, started to better understand I/O.

type Pos = (Int,Int)
type Snake = [Pos]
type Fruit = Pos

right :: Pos -> Pos
right (x, y) = (x-1,y)

left :: Pos -> Pos
left (x, y) = (x+1,y)

up :: Pos -> Pos
up (x, y) = (x,y+1)

down :: Pos -> Pos
down (x, y) = (x,y-1)

sRight :: Snake -> Snake
sRight python = right (head python) : init python

sLeft :: Snake -> Snake
sLeft python = left (head python) : init python

sUp :: Snake -> Snake
sUp python = up (head python) : init python

sDown :: Snake -> Snake
sDown python = down (head python) : init python

grid = [
    "OOOOOOOOOOOOOO",
    "OOOOOOOOOOOOOO",
    "OOOOOOOOOOOOOO",
    "OOOOOOOOOOOOOO",
    "OOOOOOOOOOOOOO",
    "OOOOOOOOOOOOOO"
    ]

drawSnake :: [String] -> Pos -> [String]
drawSnake [] _ = []
drawSnake (x:xs) (xp,yp) | length (x:xs) - 1 == yp = insertX x xp : drawSnake xs (xp,yp)
                         | otherwise = x : drawSnake xs (xp,yp)
  where
    insertX [] xp = []
    insertX (x:xs) xp | xp == length (x:xs) - 1 = "X" ++ insertX xs xp
                      | otherwise = x : insertX xs xp

drawSnake' :: [String] -> Snake -> Fruit -> [String]
drawSnake' grid snakey fruit | last snakey == fruit = drawSnake' grid (snakey ++ [(9,9)]) (7,7)
                             | length snakey > 1 = drawSnake' (drawSnake grid (head snakey)) (tail snakey) fruit
                             | otherwise = drawSnake grid (head snakey)

gameStart :: Snake -> IO()
gameStart python =
    do
        putStrLn (unlines (drawSnake' grid python (4,4)))
        i <- getLine
        if i == "w" then do
            gameStart (sUp python)
        else if i == "a" then do
            gameStart (sLeft python)
        else if i == "s" then do
            gameStart (sDown python)
        else if i == "d" then do
            gameStart (sRight python)
        else do
        putStrLn "Error"