{--
  CSCI 312 Homework #2

  Adpated from https://cs.pomona.edu/~michael/courses/csci131s18/hw/Hw02.html
--}

module Hw02 where

data ArithExp =
    Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp


-- Put your code here -------------------------
instance Show ArithExp where
  show (Num x) = "(" ++ "Num " ++ show x ++ ")"
  show (Plus x y)= "Plus " ++ show x ++ show y
  show (Neg x) = "Neg " ++ "("++ show x ++ ")"
  show (Times x y) =  "Times " ++ show x ++ show y

instance Eq ArithExp where
  -- e1 == e2 
  -- x == y = (eval x) == (eval y) || False
  -- _ == _ = False
  (Num x) == (Num y) = x == y || False
  (Plus x y) == (Plus x2 y2) = x == x2 && y == y2 || False
  (Neg x) == (Neg y) = x == y || False
  (Times x y) == (Times x2 y2) = x == x2 && y == y2 || False
  _ == _ = False

eval :: ArithExp -> Int
eval (Num x) = x 
eval (Neg x) = - eval x
eval (Plus x y) = eval x + eval y
eval (Times x y) = eval x * eval y

data ArithExp' =
    Num' Int
  | Plus' ArithExp' ArithExp'
  | Sub' ArithExp' ArithExp'
  | Times' ArithExp' ArithExp'
  | Neg' ArithExp'
  deriving Show

eval' :: ArithExp' -> Int
eval' = eval . translate

translate :: ArithExp' -> ArithExp
translate (Plus' x y) = Plus (translate x) (translate y)
translate (Times' x y) = Times (translate x) (translate y)
translate (Num' x) = Num x
translate (Neg' x) = Neg (translate x)
translate (Sub' x y) = Plus (translate x) (Neg(translate y))

instance Eq ArithExp' where
--   x == y = (eval' x) == (eval' y) || False
--   _ == _ = False
  -- (a x y) == (a x2 y2) = eval' (a x y) == eval' (a x2 y2) || False
  (Plus' x y) == (Num' x1) = eval' (Plus' x y) == x1 || False
  (Num' x) == (Num' y) = x == y || False
  (Plus' x y) == (Plus' x2 y2) = eval' (Plus' x y) == eval' (Plus' x2 y2) || False
  (Neg' x) == (Neg' y) = x == y || False
  (Times' x y) == (Times' x2 y2) = eval' (Times' x y) == eval' (Times' x2 y2) || False
  (Plus' x y) == (Num' x1) = eval' (Plus' x y) == x1 || False

instance Ord ArithExp' where
  compare (x) (y) 
    |  (eval' x) < (eval' y) = LT 
    |  (eval' x) > (eval' y) = GT
    |  (eval' x) == (eval' y) = EQ
    | otherwise = compare (eval' x) (eval' y)
    
--   (Neg' x) == (Neg' y) = x == y || False
--   (Times' x y) == (Times' x2 y2) = eval' (Times' x y) == eval' (Times' x2 y2) || False
--   (Plus' x y) == (Num' x1) = eval' (Plus' x y) == x1 || False
--   _ == _ = False

-- Tests: un-comment as you go ---------------

main = do

    putStrLn "Problem 1: arithmetic expressions -----------------------------------\n"

    putStr "\n(a) Should be Num 5: "
    print $ Num 5
    putStr "(a) Should be Neg (Plus (Num 1) (Num 1)): "
    print $ (Neg (Plus (Num 1) (Num 1)))

    putStr "\n(b) Should be True: " 
    print$ (Num 3) == (Num 3)
    putStr "(b) Should be False: " 
    print$ (Num 3) == (Num 4)
    putStr "(b) Should be True: " 
    print$ (Plus (Num 3) (Num 4)) == (Plus (Num 3) (Num 4))
    putStr "(b) Should be False: " 
    print $ (Plus (Num 3) (Num 4)) == (Num 7)
    putStr "\n(c) Should be 5: "
    print $ eval (Plus (Num 1) (Num 4))

    putStr "(c) Should be 0: "
    print $ eval (Plus (Num 42) (Neg (Num 42)))

    putStr "\n(d) Should be 2: "
    print $ eval' (Sub' (Num' 5) (Num' 3))

    putStr "(e) Should be False: " 
    print $ (Num' 2) == (Num' 3)
    putStr "(e) Should be True: " 
    print $ (Plus' (Num' 1) (Num' 2)) == (Num' 3)
    putStr "(e) Should be False: " 
    print $ (Num' 2) > (Num' 3)
    putStr "(e) Should be True: " 
    print $ (Plus' (Num' 1) (Num' 2)) < (Times' (Num' 2) (Num' 3))

    -- putStrLn "\nProblem 2: Functors ------------------------------------------------\n"
    -- putStr "\n(a) Should be ( 4 (6) 8 ): " 
    -- print $ fmap (\n -> 2 * n)(Node (Node Empty 2 Empty) 3 (Node Empty 4 Empty))
    -- putStr "\n(b) Should be Branch [Leaf 2,Leaf 3]: "
    -- print $ Branch [(Leaf 2), (Leaf 3)]
    -- putStr "\n(b) Should be: Branch [Leaf 1,Branch [Leaf 4,Leaf 9]]: "
    -- print $ fmap (\x -> x*x) (Branch [Leaf 1, (Branch [(Leaf 2), (Leaf 3)])])
    -- putStrLn ""
