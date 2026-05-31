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
       putStrLn label 
       -- putStrLn $ (("debug: " ++) . show) t

test01_whenInserting_CheckSingleRotations :: IO ()
test01_whenInserting_CheckSingleRotations =
    do check "INS [3,2,1] (LL)" (ins [3,2,1])
       check "INS [1,2,3] (RR)" (ins [1,2,3])

test02_whenInserting_CheckDoubleRotations :: IO ()
test02_whenInserting_CheckDoubleRotations =
    do check "INS [3,1,2] (LR)" (ins [3,1,2])
       check "INS [1,3,2] (RL)" (ins [1,3,2])

test03_whenInsertingExtras :: IO ()
test03_whenInsertingExtras =
    do check "INS [1..5]"                    (ins [1..5])
       check "INS [5,4..1]"                  (ins [5,4..1])
       check "INS [5,2,4,1,3]"               (ins [5,2,4,1,3])
       check "INS [10,15,5,33,7,12,20,11,4]" (ins [10,15,5,33,7,12,20,11,4])

test04_whenDeleting_CheckSingleRotations :: IO ()
test04_whenDeleting_CheckSingleRotations =
    do check "DEL 4 (INS [3,4,2,1]) (LL)" (deleteAVL 4 $ ins [3,4,2,1])
       check "DEL 1 (INS [2,1,3,4]) (RR)" (deleteAVL 1 $ ins [2,1,3,4])

test05_whenDeleting_CheckDoubleRotations :: IO ()
test05_whenDeleting_CheckDoubleRotations =
    do check "DEL 4 (INS [3,4,1,2]) (LR)" (deleteAVL 4 $ ins [3,4,1,2])
       check "DEL 1 (INS [3,4,1,2]) (RL)" (deleteAVL 1 $ ins [3,4,1,2])

test06_whenDeletingExtras :: IO ()
test06_whenDeletingExtras =
    do check "DEL 1  (INS [1..5])"                    (deleteAVL 1  $ ins [1..5])
       check "DEL 4  (INS [5,4..1])"                  (deleteAVL 4  $ ins [5,4..1])
       check "DEL 3  (INS [5,2,4,1,3])"               (deleteAVL 3  $ ins [5,2,4,1,3])
       check "DEL 33 (INS [10,15,5,33,7,12,20,11,4])" (deleteAVL 33 $ ins [10,15,5,33,7,12,20,11,4])

test07_whenTryingToDoFold_ItWorks :: IO ()
test07_whenTryingToDoFold_ItWorks = 
    do putChunk $ if sum (ins [1..5]) == 15 then "[OK] " & fore green else "[FAIL] " & fore red
       putStrLn "fold test"

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
