module Intro01
  (
    luhn
  , hanoi
  , hanoi4
  )
where

import Control.Monad

-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

-- Generate the sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Compute the length of a list of Integers.
listLength :: [a] -> Integer
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

lastDigit :: Integer -> Integer
lastDigit = flip mod 10

dropLastDigit :: Integer -> Integer
dropLastDigit =  flip div 10

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0 = []
  | otherwise = liftM2 (:) lastDigit  (toRevDigits. dropLastDigit) n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:ys) = x:2*y:doubleEveryOther ys

sumDigits :: [Integer] -> Integer
sumDigits = foldl1 (+) . foldMap toRevDigits

checksum :: Integer -> Integer
checksum = sumDigits . doubleEveryOther . toRevDigits

luhn :: Integer -> Bool
luhn = (== 0) . (flip mod 10) . checksum

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | otherwise = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)


hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n <= 0 = []
  | n == 1 = [(a,b)]
  | otherwise =
      (hanoi4 k a c b d) ++
      (hanoi (n-k) a b d) ++
      (hanoi4 k c b a d)
    -- this is copyed from wiki [https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame.E2.80.93Stewart_algorithm]
    where k = n - (round . sqrt . fromInteger $ (2*n+1)) +1
