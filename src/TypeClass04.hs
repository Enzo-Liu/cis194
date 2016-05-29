module TypeClass04
  (

  )
where
import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
  P [] == P [] = True
  P (x:xs) == P [] = x == 0 && P xs == P []
  P [] == P y@(_:[]) = P y == P []
  P (x:xs) == P (y:ys) = x == y && P xs == P ys

-- Exercise 3 -----------------------------------------
joinList :: String -> [String] -> String
joinList _ [] = []
joinList _ (y:[]) = y
joinList xs (y:ys) = y ++ xs ++ joinList xs ys

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P xs) = joinList " + " .
                  reverse .
                  filter (not.null) .
                  map display
                  $ zip [0..] xs
      where display (_,0) = ""
            display (0,c) = show c
            display (d,c) = showC c ++ showD d
            showC 1 = ""
            showC (-1) = "-"
            showC c = show c
            showD 1 = "x"
            showD d = "x^" ++ show d

-- Exercise 4 -----------------------------------------

plusList xs' [] = xs'
plusList [] ys' = ys'
plusList (x':xs') (y':ys') = (x'+y'):plusList xs' ys'

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P (plusList xs ys)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = P (foldl plusList ([]) $ timesList a b)
  where timesList a b = map (plusC a) $ zip [0..] b
        plusC l (d,c) = fill0 d $ map (*c) l
        fill0 0 l  = l
        fill0 n l  = fill0 (n-1) (0:l)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    -- since -1 is actual (negate 1) , if (* (-1)) will cause infinite loop
    -- negate  = (* P[-1])
    negate  (P a)    = P $ (map negate a)
    fromInteger i = P [fromInteger i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P l) a = sum $ map (apply a) (zip [0..] l)
  where apply a (d,c) = (a^d)*c

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 y = y
    nderiv n f = nderiv (n-1) (deriv f)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
  deriv (P []) = P []
  deriv (P l)  = P (zipWith (*) numList (tail l))
    where numList = 1 : map (+1) numList
