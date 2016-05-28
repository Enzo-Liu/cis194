module Intro01
    ( luhn
    ) where

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
