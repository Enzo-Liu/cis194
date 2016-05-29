module Adt03
  (

  )
where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st s i = \(s') -> if s' == s then i else st s'

empty :: State
empty = \_->0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE _ (Val i) = i
evalE st (Var a) = st a
evalE st (Op el bop er) = op bop (evalE st el) (evalE st er)
  where op Plus = (+)
        op Minus = (-)
        op Times = (*)
        op Divide = div
        op Gt = \a->boolToInt . (a >)
        op Ge = \a->boolToInt . (a >=)
        op Lt = \a->boolToInt . (a <)
        op Le = \a->boolToInt . (a <=)
        op Eql = \a->boolToInt . (a ==)
        boolToInt True = 1
        boolToInt False = 0


-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e) = DAssign s e
desugar (If e thenSm elseSm) = DIf e (desugar thenSm) (desugar elseSm)
desugar (While e sm) = DWhile e (desugar sm)
desugar (Sequence sm1 sm2) = DSequence (desugar sm1) (desugar sm2)
desugar Skip = DSkip
desugar (Incr var) = DAssign var (Op (Var var) Plus (Val 1))
desugar (For initSm predEx update body) = DSequence
                                          (desugar initSm)
                                          (DWhile predEx
                                           (DSequence
                                            (desugar body)
                                            (desugar update)))


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st DSkip = st
evalSimple st (DAssign s e) = extend st s (evalE st e)
evalSimple st (DIf e thenDs elseDs) =
  if evalE st e == 1 then evalSimple st thenDs else evalSimple st elseDs
evalSimple st (DWhile e ds) =
  if evalE st e == 1 then evalSimple (evalSimple st ds) (DWhile e ds) else st
evalSimple st (DSequence ds1 ds2) =
  evalSimple (evalSimple st ds1) ds2

run :: State -> Statement -> State
run st = evalSimple st . desugar

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
