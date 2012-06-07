import Data.List
import Data.Function
import Data.Maybe
import System.IO

data CardRank = High | OnePair | TwoPairs | ThreeKind
    | Straight | Flush | FullHouse | FourKind
    | StraightFlush | RoyalFlush
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data CardValue = Two | Three | Four | Five | Six
    | Seven | Eight | Nine | Ten | Jack
    | Queen | King | Ace
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

conseq :: [(CardValue, Char)] ->  Bool
conseq cards = helper (tail cards) (fst (head cards))
    where helper cards current
            | null cards = True
            | next /= (maxBound :: CardValue) && current /= succ next = False
            | otherwise = helper (tail cards) next
            where next = fst $ head cards

same_suit :: [(CardValue, Char)] -> Bool
same_suit (x:xs) = helper xs
    where init = snd x
          helper list
            | null list = True
            | init /= current = False
            | otherwise = helper $ tail list
            where current = snd $ head list

rank :: [(CardValue, Char)] -> (CardRank, Int)
rank cards@(_:_:_:_:_:[])
    | conseq cards = rank_conseq cards
    | same_suit cards = (Flush, value)
    | otherwise = rank_group $ sortBy (compare `on` ((0-) . length)) $ groupBy ((==) `on` fst) cards
    where value = fst $ head cards
          card_length = length cards
          rank_conseq cards
            | same_suit cards = if value == Ace
                                then (RoyalFlush, value)
                                else (StraightFlush, value)
            | otherwise = (Straight, value)
          rank_group groups = case length groups of
            5 -> (High, value)
            4 -> (OnePair, value2)
            3 -> if length group_head == 3
                    then (ThreeKind, value2)
                    else (TwoPairs, value2)
            2 -> if length group_head == 3
                    then (FullHouse, value2)
                    else (FourKind, value2)
            where group_head = head groups
                  value2 = fst $ head group_head
