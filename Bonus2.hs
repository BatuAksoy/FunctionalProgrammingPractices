module Bonus2 where

import Data.Char

data Color = Red | Black deriving (Eq, Show)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show)

data Rank = Num Int | Jack | Queen | King | Ace deriving (Eq, Show)

data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Show)

data Move = Draw | Discard Card deriving (Eq, Show)

data State = WaitingInput | Playing



-- 1 cardColor
cardColor :: Card -> Color
cardColor c
    | suit c == Diamonds || suit c == Hearts        = Red
    | otherwise                                     = Black

-- 2 cardValue
cardValue :: Card -> Int
cardValue c
    | rank c == Jack || rank c == Queen || rank c == King = 10
    | rank c == Ace                                       = 11
    | otherwise                                           = case rank c of Num k -> if k > 0 && k < 11 then k else error "This card is not real"

-- 3 removeCard
removeCard :: [Card] -> Card -> [Card]
removeCard [] _           = error "Card is not in the list"
removeCard [c1]    c2     = if c1 == c2 then [] else error "Card is not in the list"
removeCard (c':cs) c      = if c' == c then cs else c' : (removeCard cs c)

-- 4 allSameColor
allSameColor :: [Card] -> Bool
allSameColor []     = True
allSameColor [c]    = True
allSameColor (c1:cs'@(c2:cs)) = if cardColor c1 /= cardColor c2 then False else allSameColor cs'

-- 5 sumCards
sumCards :: [Card] -> Int
sumCards cs = sumCards' 0 cs
    where
        sumCards' :: Int -> [Card] -> Int
        sumCards' acc []      = acc
        sumCards' acc (c:cs)  = sumCards' ((cardValue c) + acc) cs

-- 6 score
score :: [Card] -> Int -> Int
score cs goal
    | sumCards cs > goal = pre (3 * (sumCards cs - goal))
    | otherwise          = pre (goal - sumCards cs)
    where
        pre :: Int -> Int
        pre sum = if (allSameColor cs) then (div sum 2) else sum

-- 8 runGame
runGame :: [Card] -> [Move] -> Int -> Int
runGame cs mvs goal = runner Playing cs [] mvs goal
    where
        runner :: State -> [Card] -> [Card] -> [Move] -> Int -> Int
        runner _  _ heldcs [] goal  = score heldcs goal
        runner _ [] heldcs  _ goal  = score heldcs goal
        runner _ (cs'@(c:cs)) heldcs (mv:mvs) goal = case mv of
            Draw        -> if (sumCards (c:heldcs)) > goal then score (c:heldcs) goal else runner Playing cs (c:heldcs) mvs goal
            Discard dc  -> runner Playing cs' (removeCard heldcs dc) mvs goal

-- 9 convertSuit
convertSuit :: Char -> Suit
convertSuit c
    | elem c "cC" = Clubs
    | elem c "dD" = Diamonds
    | elem c "hH" = Hearts
    | elem c "sS" = Spades
    | otherwise   = error "Suit is unknown!"

-- 10 convertRank
convertRank :: Char -> Rank
convertRank c
    | elem c "tT" = Num 10
    | elem c "jJ" = Jack
    | elem c "qQ" = Queen
    | elem c "kK" = King
    | isDigit c && (digitToInt c) >= 1 &&  (digitToInt c) <= 9 = if digitToInt c == 1 then Ace else Num (digitToInt c)
    | otherwise   = error "Rank is unknown!"

-- 11 convertCard
convertCard :: Char -> Char -> Card
convertCard suit rank = Card {suit = convertSuit suit, rank = convertRank rank}

-- 12 readCards
readCards :: [Card] -> IO [Card]
readCards cs = do
    line <- getLine
    if length line == 2 then readCards ((convertCard (line!!0) (line!!1)):cs) else if line == "." then return(cs) else error "Input card is unknown!"

-- 13 convertMove
convertMove :: Char -> Char -> Char -> Move
convertMove move suit rank
    | elem move "dD" = Draw
    | elem move "rR" = Discard Card{suit=convertSuit suit, rank=convertRank rank}

-- 14 readMoves
readMoves :: [Move] -> IO [Move]
readMoves mvs = do
    line <- getLine
    if length line == 1 && line == "." then return(mvs) else if length line == 1 then readMoves ((convertMove (line!!0) 'a' 'a'):mvs)
        else if length line == 3 then readMoves ((convertMove (line!!0) (line!!1) (line!!2)):mvs) else error "Input move is unknown!"


main = do
        putStrLn "Enter cards:"
        cards <- readCards []
        putStrLn (show cards)
        putStrLn "Enter moves:"
        moves <- readMoves []
        putStrLn (show moves)
        putStrLn "Enter goal:"
        line <- getLine
        let goal = read line :: Int
        let score = runGame cards moves goal
        putStrLn ("Score: " ++ show score)