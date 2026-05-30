{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AVL
import Rainbow ( fore, green, red, putChunk )
import Data.Function ( (&) )

ins :: [Int] -> AVL Int
ins = foldl (flip insertAVL) nilAVL

check :: String -> AVL Int -> IO ()
check label t = 
    do putChunk $ if isBalanced t then "[OK] " & fore green else "[FAIL] " & fore red
       putStrLn $ label ++ ": " ++ debug t

debug :: AVL Int -> String
debug = ("\ndebug: " ++) . show

test01_whenInserting_CheckSingleRotations :: IO ()
test01_whenInserting_CheckSingleRotations =
    do check "INS: LL (3,2,1)" (ins [3,2,1])
       check "INS: RR (1,2,3)" (ins [1,2,3])

test02_whenInserting_CheckDoubleRotations :: IO ()
test02_whenInserting_CheckDoubleRotations =
    do check "INS: LR (3,1,2)" (ins [3,1,2])
       check "INS: RL (1,3,2)" (ins [1,3,2])

test03_whenInsertingExtras :: IO ()
test03_whenInsertingExtras =
    do check "INS: 1..5"     (ins [1..5])
       check "INS: 5..1"     (ins [5,4..1])
       check "INS: random"   (ins [5,2,4,1,3])
       check "INS: random 2" (ins [10,15,5,33,7,12,20,11,4])

test04_whenDeleting_CheckSingleRotations :: IO ()
test04_whenDeleting_CheckSingleRotations =
    do check "DEL 4: LL (3,4,2,1)" ((deleteAVL 4 . ins) [3,4,2,1])
       check "DEL 1: RR (2,1,3,4)" ((deleteAVL 1 . ins) [2,1,3,4])

test05_whenDeleting_CheckDoubleRotations :: IO ()
test05_whenDeleting_CheckDoubleRotations =
    do check "DEL 4: LR (3,4,1,2)" (ins [3,4,1,2])
       check "DEL 1: RL (1,3,2)" (ins [2,1,4,3])

test06_whenDeletingExtras :: IO ()
test06_whenDeletingExtras =
    do check "DEL 1: 1..5"     (ins [1..5])
       check "DEL 4: 5..1"     (ins [5,4..1])
       check "DEL 3: random"   (ins [5,2,4,1,3])
       check "DEL 33: random 2" (ins [10,15,5,33,7,12,20,11,4])

test07_whenTryingToDoFold_ItWorks :: IO ()
test07_whenTryingToDoFold_ItWorks = 
    do putChunk $ if sum (ins [1..5]) == 15 then "[OK] " & fore green else "[FAIL] " & fore red
       putStrLn "fold test!"

main :: IO ()
main =
    do -- Inserting Tests
       test01_whenInserting_CheckSingleRotations
       test02_whenInserting_CheckDoubleRotations
       test03_whenInsertingExtras
       -- Deleting Tests
       test04_whenDeleting_CheckSingleRotations
       test05_whenDeleting_CheckDoubleRotations
       test06_whenDeletingExtras
       -- Test access to fold!
       test07_whenTryingToDoFold_ItWorks
