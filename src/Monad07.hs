{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Monad07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Function
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= return . f

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV from to v = liftM2 swap (v !? from) (v !? to)
  where swap a b = v // [(from,b), (to,a)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f  = sequence . fmap f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts xs v = mapM (v !?) xs

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = liftM (v !?) $ getRandomR (0,V.length v)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = liftM V.fromList $ sequence (replicate n getRandom)

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n r = liftM V.fromList $ sequence (replicate n $ getRandomR r)

-- Exercise 5 -----------------------------------------
unsafeSwap :: Vector a -> [(Int, Int)] -> Vector a
unsafeSwap v []= v
unsafeSwap v ((from,to):xs) = unsafeSwap (v // [(from,v!to), (to, v!from)]) xs

shuffle :: Vector a -> Rnd (Vector a)
shuffle v | V.length v == 0 = return v
shuffle v = liftM (unsafeSwap v) $ mapM randomPair $ reverse [0 .. V.length v -1 ]
  where randomPair i = liftM (\r->(i,r)) $ getRandomR (0,i)

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (less, pivot, more)
  where pivot = v!i
        v' = V.take i v V.++ V.drop (i+1) v
        (less,more) = V.partition (< pivot) v'

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v | V.null v = v
qsort v = qsort [ y | y <- r, y < h ] V.++
          V.singleton h V.++
          qsort [ y | y <- r, y >= h ]
  where h = V.head v
        r = V.tail v

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v | V.null v = return v
qsortR v = do
  i <- getRandomR (0,length v -1)
  let (less,pivot,more) = partitionAt v i
  orderedLess <- qsortR less
  orderedMore <- qsortR more
  return $ orderedLess V.++ V.singleton pivot V.++ orderedMore

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select r v | r < 0 || r >= length v = return Nothing
select r v = do
  i <- getRandomR (0,length v -1)
  let (less,pivot,more) = partitionAt v i
      ll = length less
  case compare ll r of
    LT -> select (r-ll-1) more
    EQ -> return $ Just pivot
    GT -> select r less

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card label suit | suit <- suits, label <- labels]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d | V.null d = Nothing
nextCard d = Just (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n = go []
  where go cards deck
          | length cards >= n = return (cards, deck)
          | otherwise         = do
              (card, deck') <- nextCard deck
              go (cards ++ [card]) deck'

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }
repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
