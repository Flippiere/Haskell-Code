module Pkmn where

-- Code that calculates the damage of an attack according to the pokemon damage formula

data Pkmntype = Grass | Fire | Water | Normal deriving Show
data Attack = Attack {atkName :: String, power :: Int, atkType :: Pkmntype, special :: Bool} deriving Show
data PkmnStats = Stats {hp :: Int, atk :: Int, def :: Int, spA :: Int, spD :: Int, spe :: Int} deriving Show
data Pokemon = Pokemon {iD :: Int, type1 :: Pkmntype, type2 :: Maybe Pkmntype, pkmnName :: String, stats :: PkmnStats, attacks :: [Attack], hpLive :: Int, level :: Int} deriving Show

instance Eq Pokemon where
    (==) pokemon1 pokemon2 = iD pokemon1 == iD pokemon2

instance Ord Pokemon where
    (>=) pokemon1 pokemon2 = iD pokemon1 >=  iD pokemon2
    (<=) pokemon1 pokemon2 = iD pokemon1 <=  iD pokemon2

tackle = Attack "Tackle" 40 Normal False
vineWhip = Attack "Vine Whip" 40 Grass False


effectiveness :: Pkmntype -> Pkmntype -> Float
effectiveness Fire Water = 0.5
effectiveness Water Fire = 1.5
effectiveness Fire Grass = 1.5
effectiveness Grass Water = 1.5
effectiveness Grass Fire = 0.5
effectiveness _ _ = 1

pkmnTable :: Int -> Pokemon
pkmnTable 1 = Pokemon 1 Grass Nothing "Bulbasaur" (Stats 45 49 49 65 65 45) [] 0 1
pkmnTable 4 = Pokemon 4 Fire Nothing "Charmander" (Stats 39 52 43 60 50 65) [] 0 1
pkmnTable _ = Pokemon 0 Normal Nothing "Misingno." (Stats 0 0 0 0 0 0) [] 0 1

fainted :: Pokemon -> Bool
fainted a = hpLive a <= 0

newPkmn :: Int -> [Attack] -> Int -> Int -> Pokemon
newPkmn x list hpL levelP | hpL >= 0 && hpL <= 100 = newPkmn' (pkmnTable x) list hpL levelP
                          | otherwise = error "invalid hp value"
  where
    newPkmn' (Pokemon x x' x'' x''' x'''' y y' y'') list hpL levelP = Pokemon x x' x'' x''' x'''' (take 4 list) ((((((2*hp x'''')*levelP) `div` 100)+levelP+10)*hpL) `div` 100) levelP

pkmnAttack :: Pokemon -> Int -> Pokemon -> Pokemon
pkmnAttack (Pokemon _ _ _ _ atkStats moves _ atkrLevel) move (Pokemon x defType x'' x''' defStats x'''' hp level) = Pokemon x defType x'' x''' defStats x'''' (hp - damage atkrLevel ((((2*atk atkStats)*atkrLevel) `div` 100) +5) ((((2*def defStats)*level) `div` 100) +5) (power (moves !! move)) (atkType (moves !! move)) defType) level
  where
    damage atkrLevel atk def bp atkType defType = floor (fromIntegral (((((2*atkrLevel) `div` 5) * bp * (atk `div` def)) `div` 50)+2) * effectiveness atkType defType)