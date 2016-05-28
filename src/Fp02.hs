module Fp02
  (

  )
where
import Control.Monad
-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

equalWeight :: Peg -> Peg -> Int
equalWeight p1 p2 = if p1 == p2 then 1 else 0

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c = sum . zipWith equalWeight c

-- Exercise 2 -----------------------------------------

count :: Code -> Peg -> Int
count [] _ = 0
count (x:xs) p = equalWeight x p + count xs p

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = map (count c) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum $ zipWith min (countColors c1) (countColors c2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g exact nonexact
  where exact = exactMatches s g
        nonexact = matches s g - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move g e n) s = e' == e && n == n'
  where (Move _ e' n') = getMove s g

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter <$> isConsistent

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (:[]) colors
allCodes n = foldMap concat $ allCodes (n-1)
  where concat code = map (:code) colors

-- Exercise 7 -----------------------------------------

solveDumb :: Code -> [Move]
solveDumb s = filter right (map (getMove s) all)
  where l = length s
        all = allCodes l
        right (Move g e n) = e == l

solve :: Code -> [Move]
solve s = choose . allCodes . length $ s
  where choose [] = []
        choose (x:xs) = let m = getMove s x in
          m : choose (filter (isConsistent m) xs)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
