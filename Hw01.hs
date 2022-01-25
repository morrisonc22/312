{--
  CSCI 312 Homework #1

  Adpated from https://cs.pomona.edu/~michael/courses/csci131s18/hw/Hw01.html
--}

module Hw01 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Distribution.Simple.Utils (xargs)

type Node = String
type DAG = Map.Map Node (Set.Set Node)

a = "a"
b = "b"
c = "c"
d = "d"
e = "e"

g = Map.fromList [(a, Set.fromList [b,c]),
                  (b, Set.fromList [d]),
                  (c, Set.fromList [d]),
                  (d, Set.fromList []),
                  (e, Set.fromList [a, c])]

-- Put your functions here --------------------
sumUp :: [Int] -> Int
sumUp[] = 0
sumUp (x:xs) = x + sumUp xs

evens :: [Int] -> [Int]
evens[] = []
evens (x:xs) = if mod x 2 == 0 then x: evens xs  else evens xs

incALL :: [Int] -> [Int]
incALL [] = []
incALL (x:xs) = x + 1 : incALL xs

incBy :: Int -> [Int] -> [Int]
incBy n [] = []
incBy n (x:xs) = x + n : incBy n xs

append :: [Int] -> [Int] -> [Int]
append (x:xs) [] = (x:xs)
append [] (xy) = xy
append (x:xs) (xy) = x : append xs xy

data IntTree = Empty | Node IntTree Int IntTree deriving (Eq,Show)

isLeaf :: IntTree -> Bool
isLeaf Empty = True
-- isLeaf (Empty x Empty) = True
isLeaf (Node l x r) = False

sumTree :: IntTree -> Int
sumTree Empty = 0
sumTree (Node l x r) = sumTree l + x + sumTree r

fringe :: IntTree -> [Int]
fringe Empty = []
fringe (Node l x r) = if ((isLeaf l) && (isLeaf r)) then x : [] else (fringe l ++ fringe r)

getValue :: IntTree -> [Int]
getValue Empty = []
getValue (Node Empty x Empty) = [x]
getValue (Node l x r) = (getValue l) ++ [x] ++ (getValue r)

isBST :: IntTree -> Bool
isBST Empty = True
isBST (Node l x r) = (all(\v-> x > v) (getValue l) && all(\v -> x < v) (getValue r) || isBST l && isBST r) || isBST(Node l x r)

sumUp' :: [Int] -> Int
sumUp'  = foldl (+) 0

evens' ::  [Int] -> [Int]
evens' (x:xs)  = filter p xs
  where p x = x `mod` 2 == 0

incAll' :: [Int] -> [Int]
incAll' (xs) = map (+1) xs

incBy' :: Int -> [Int] -> [Int]
incBy' n xs = map (+n) xs

map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 f (x:xs) = f x : map1 f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1  _[] = []
filter1 p (x:xs)
  | p x           = x : filter1 p xs
  | otherwise     = filter1 p xs


sqrt' :: Float -> Maybe Float

sqrt' (a) = if a >= 0 then Just (sqrt a) else Nothing

div' :: Float -> Float -> Either String Float
div' a 0 = Left "Error: Divide by zero"
div' a b = Right (a/b)

swap :: (a,b) -> (b, a)
swap (a,b) = (b,a)

pairUp :: [a] -> [b] -> [(a,b)]
pairUp _ [] = []
pairUp [] _ = []
pairUp (x:xs) (y:ys) = (x, y) : [] ++ pairUp xs ys

splitUp :: [(a,b)] -> ([a], [b])
splitUp [] = ([], [])
splitUp ((a, b):xs) = let split = (splitUp xs)
                      in (a: (fst split), b:(snd split))

sumAndLength :: [Int] -> (Int, Int)
-- sumAndLength [] = (0, 0)

sumAndLength xs = foldl(\(sum, lgth) x -> (sum + x, lgth + 1)) (0,0) xs

neighbors :: DAG -> Node -> Set.Set Node
neighbors dag node = (Map.!) dag node -- similar to Python dag[node]

any' :: Set.Set Bool -> Bool
any' a = listBool
  where listBool = Set.member True a

hasPath :: DAG -> Node -> Node -> Bool
hasPath dag node1 node2 = let nbrs = (Map.!) dag node1 in elem node2 nbrs || any (\node -> hasPath dag node node2) nbrs
-- Tests ----------------------------------------

main = do

    putStrLn "Problem 1: natural recursion -------------------- ---------------\n"

    putStr "Should be 6: "
    print $ sumUp [1,2,3]
    putStr "Should be [2,4,6,8]: "
    print $ evens [1,2,3,4,5,6,7,8,9]

    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
    print $ incALL [1,2,3,4,5,6,7,8,9]

    putStr "Should be 3,4,5,6,7,8,9,10,11]: "
    print $ incBy 2 [1,2,3,4,5,6,7,8,9]

    putStr "Should be 1,2,3]: "
    print $ append [] [1,2,3]

    putStr "Should be [1,2,3]: "
    print $ append [1,2,3] []

    putStr "Should be [1,2,3,4,5,6]: "
    print $ append [1,2,3] [4,5,6]

    -- putStrLn "\nProblem 2: data types -----------------------------------------\n"

    putStr "Should be True: "
    print $ isLeaf Empty

    putStr "Should be True: "
    print $ isLeaf (Node Empty 3 Empty)

    putStr "Should be False: "
    print $ isLeaf (Node (Node Empty 1 Empty) 2 Empty)

    putStr "Should be 10: "
    print $ sumTree (Node (Node Empty 1 Empty) 3 (Node Empty 2 (Node Empty 4 Empty)))

    putStr "Should be [2,7]: "
    print $ fringe (Node (Node Empty 1 (Node Empty 2 Empty))
                          5
                          (Node (Node Empty 7 Empty) 10 Empty))

    -- putStrLn "\nProblem 3: binary search trees --------------------------------\n"

    putStr "Should be True: "
    print $ isBST (Node (Node Empty 2 Empty)  4 (Node Empty 5 Empty))

    putStr "Should be False: "
    print $ isBST (Node (Node Empty 5 Empty)  4 (Node Empty 2 Empty))

    -- putStrLn "\nProblem 4: map and filter -------------------------------------\n"

    putStr "Should be 6: "
    print $ sumUp' [1,2,3]

    putStr "Should be [2,4,6,8]: "
    print $ evens' [1,2,3,4,5,6,7,8,9]

    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
    print $ incAll' [1,2,3,4,5,6,7,8,9]

    putStr "Should be [3,4,5,6,7,8,9,10,11]: "
    print $ incBy' 2 [1,2,3,4,5,6,7,8,9]

    -- putStrLn "\nProblem 5: defining higher-order functions --------------------\n"

    putStr "Should be [1,4,9,16,25]: "
    print $ map1 (\x -> x * x) [1,2,3,4,5]

    putStr "Should be [1,3,5,7,9]: "
    print $ filter1 odd [0,1,2,3,4,5,6,7,8,9]

    -- putStrLn "\nProblem 6: Maybe and Either ------------------------------------\n"

    putStr "Should be [0.0,1.0,2.0,3.0]: "
    print $ mapMaybe sqrt' [0,-1,1,-4,4,9,-9]

    -- putStrLn "\nProblem 7: Creating polymorphic data types ---------------------\n"

    putStr "Should be (\"hello\", 3): "
    print $ swap (3, "hello")

    putStr "Should be [(0,1),(2,3),(4,5),(6,7),(8,9)]: "
    print $ pairUp [0,2,4,6,8] [1,3,5,7,9]

    putStr "Should be ([0,2,4,6,8],[1,3,5,7,9]): "
    print $ splitUp [(0,1),(2,3),(4,5),(6,7),(8,9)]

    putStr "Should be (15, 5): "
    print $ sumAndLength [1,2,3,4,5]

    case div' 1 0 of
      Right val -> print $ val
      Left  msg -> putStrLn msg

    case div' 1 2 of
      Right val -> print $ val
      Left  msg -> putStrLn msg

    -- putStrLn "\nProblem 8: maps and sets --------------------------------------\n"

    putStr "Should be True: "
    print $ hasPath g a d

    putStr "Should be False: "
    print $ hasPath g a e

