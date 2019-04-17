module TestBonus2 where

import Bonus2

-- 1
testCardColor :: IO ()
testCardColor = do
    let cards = [Card {suit = Hearts, rank = Jack},Card {suit = Diamonds, rank = Jack},Card {suit = Spades, rank = Jack},Card {suit = Clubs, rank = Jack}]
    let results = [Red,Red,Black,Black]
    putStrLn (if map cardColor cards == results then "cardColor test passed" else "cardColor test failed")

-- 2
testCardValue :: IO ()
testCardValue = do
    let cards = [Card {suit = Hearts, rank = Num 1},Card {suit = Hearts, rank = Num 2},Card {suit = Hearts, rank = Num 3},Card {suit = Hearts, rank = Num 4},Card {suit = Hearts, rank = Num 5},Card {suit = Hearts, rank = Num 6},Card {suit = Hearts, rank = Num 7},Card {suit = Hearts, rank = Num 8},Card {suit = Hearts, rank = Num 9},Card {suit = Hearts, rank = Num 10},Card {suit = Hearts, rank = Jack},Card {suit = Hearts, rank = Queen},Card {suit = Hearts, rank = King},Card {suit = Hearts, rank = Ace}]
    let results = [1,2..10] ++ [10,10,10,11]
    putStrLn (if map cardValue cards == results then "cardValue test passed" else "cardValue test failed")

-- 3
testRemoveCard :: IO ()
testRemoveCard = do
    -- list of ([Card], Card:toBeDeleted)
    -- these are not enough and not readable
    let cards = [([Card {suit = Hearts, rank = Jack},Card {suit = Hearts, rank = Jack},Card {suit = Hearts, rank = Jack},Card {suit = Hearts, rank = Jack}], Card {suit = Hearts, rank = Jack}), ([Card {suit = Hearts, rank = Num 1},Card {suit = Diamonds, rank = Num 2},Card {suit = Clubs, rank = Num 3},Card {suit = Spades, rank = Num 3}], Card {suit = Clubs, rank = Num 3}), ([Card {suit = Clubs, rank = Ace}], Card {suit = Clubs, rank = Ace})]
    let results = [[Card {suit = Hearts, rank = Jack},Card {suit = Hearts, rank = Jack},Card {suit = Hearts, rank = Jack}], [Card {suit = Hearts, rank = Num 1},Card {suit = Diamonds, rank = Num 2},Card {suit = Spades, rank = Num 3}], []]
    putStrLn (if map (\t -> removeCard (fst t) (snd t)) cards == results then "removeCard test passed" else "removeCard test failed")

-- 4
testAllSameColor :: IO ()
testAllSameColor = do
    let cardsFalse  = [Card {suit = Hearts, rank = Jack},Card {suit = Diamonds, rank = Jack},Card {suit = Diamonds, rank = Jack},Card {suit = Clubs, rank = Jack}]
    let cardsTrue   = [Card {suit = Hearts, rank = Jack},Card {suit = Diamonds, rank = Jack},Card {suit = Diamonds, rank = Jack},Card {suit = Hearts, rank = Jack}]
    let results     = [False, True]
    putStrLn (if [allSameColor cardsFalse, allSameColor cardsTrue] == results then "allSameColor test passed" else "allSameColor test failed")

-- 5
testSumCards :: IO ()
testSumCards = do
    let cards = [[Card {suit = Hearts, rank = Jack},Card {suit = Diamonds, rank = Jack},Card {suit = Diamonds, rank = Jack},Card {suit = Clubs, rank = Jack}],[Card {suit = Hearts, rank = Num 3},Card {suit = Diamonds, rank = Num 3},Card {suit = Diamonds, rank = Num 3},Card {suit = Clubs, rank = Num 3}], [Card {suit = Hearts, rank = Num 7},Card {suit = Diamonds, rank = Num 7},Card {suit = Diamonds, rank = Ace},Card {suit = Clubs, rank = Queen}], []]
    let results = [40, 12, 35, 0]
    putStrLn (if map sumCards cards == results then "sumCards test passed" else "sumCards test failed")

testScore :: IO ()
testScore = do
    let cards = [([Card {suit = Hearts, rank = Jack},Card {suit = Diamonds, rank = Jack},Card {suit = Diamonds, rank = Jack},Card {suit = Clubs, rank = Jack}], 25),([Card {suit = Hearts, rank = Num 3},Card {suit = Diamonds, rank = Num 3},Card {suit = Diamonds, rank = Num 3},Card {suit = Clubs, rank = Num 3}], 20), ([Card {suit = Hearts, rank = Num 7},Card {suit = Diamonds, rank = Num 7},Card {suit = Diamonds, rank = Ace},Card {suit = Clubs, rank = Queen}], 35), ([], 5), ([Card {suit = Hearts, rank = Num 7},Card {suit = Diamonds, rank = Num 7},Card {suit = Diamonds, rank = Ace},Card {suit = Hearts, rank = Queen}], 25), ([Card {suit = Hearts, rank = Num 7},Card {suit = Diamonds, rank = Num 7},Card {suit = Diamonds, rank = Ace},Card {suit = Hearts, rank = Queen}], 20), ([Card {suit = Hearts, rank = Num 7},Card {suit = Diamonds, rank = Num 7},Card {suit = Diamonds, rank = Ace},Card {suit = Hearts, rank = Queen}], 45), ([Card {suit = Hearts, rank = Num 7},Card {suit = Diamonds, rank = Num 7},Card {suit = Diamonds, rank = Ace},Card {suit = Hearts, rank = Queen}], 40)]
    let results = [45, 8, 0, 2, 15, 22, 5, 2]
    putStrLn (if map (\t -> score (fst t) (snd t)) cards == results then "score test passed" else "score test failed")

main :: IO ()
main = do
    testCardColor