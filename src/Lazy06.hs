{-# OPTIONS_GHC -Wall #-}
module Lazy06 where

import Data.List
-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n | n < 0 = 0
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1:1:zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs)= x:streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) y = Cons x (sInterleave y xs)

sTake :: Int -> Stream a -> [a]
sTake n _ | n<=0 = []
sTake n (Cons x xs) = x:(sTake (n-1) xs)

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = Cons 0 (fmap (+1) nats)

zipStream :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipStream f (Cons x xs) (Cons y ys) = Cons (f x y) (zipStream f xs ys)

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) (zipStream (+) (sRepeat 1) ruler)

-- Exercise 7 -----------------------------------------
-- | Implementation of C rand
rand :: Int -> Stream Int
rand x = Cons x (rand next)
  where next = mod (1103515245 * x + 12345) 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 80 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 3 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = Just $ go (x,x) xs
  where go r [] = r
        go r@(mn,mx) (y:ys)
          | mn > y = go (y,mx) ys
          | mx < y = go (mn,y) ys
          | otherwise = go r ys

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = M Integer Integer Integer Integer deriving (Show)

instance Num Matrix where
  (M x1 x2 y1 y2) * (M x1' x2' y1' y2') =
    M (x1*x1'+x2*y1') (x1*x2'+x2*y2')
      (y1*x1'+y2*y1') (y1*x2'+y2*y2')
  fromInteger i = M (fromInteger i) 0 0 (fromInteger i)
  negate (M x1 x2 y1 y2)= M (-x1) (-x2) (-y1) (-y2)
  (+) = undefined
  abs = undefined
  signum = undefined

munit :: Matrix
munit = M 1 1 1 0

fastFib :: Int -> Integer
fastFib n | n <= 0 = 0
fastFib n = res
  where M _ res _ _ = go n
        go 1 = munit
        go a = case mod a 2 of
          0 -> (go $ div a 2)^(2::Integer)
          _ -> (*) munit $ (go $div (a-1) 2)^(2::Integer)
