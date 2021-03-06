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

beat :: [(CardValue, Char)] -> [(CardValue, Char)] -> Bool
beat play_one play_two = case compare (rank play_one) (rank play_two) of
    GT -> True
    LT -> False
    EQ -> cmp play_one play_two
        where cmp c1 c2
                | b1 == b2 = cmp (tail c1) (tail c2)
                | otherwise = b1 > b2
                where b1 = fst $ head c1
                      b2 = fst $ head c2

parse :: [String] -> [(CardValue, Char)]
parse cards
    | null cards = []
    | otherwise = ((get_card value),kind):(parse $ tail cards)
    where c = head cards
          value = head c
          kind = head $ tail c
          get_card value = [Two .. Ace] !! (fromJust $ elemIndex value "23456789TJQKA")

main :: IO ()
main = do
       inh <- openFile "poker.txt" ReadMode
       mainLoop inh 0
       hClose inh

mainLoop :: Handle -> Int -> IO ()
mainLoop inh n =
    do eof <- hIsEOF inh
       if eof
           then
               putStrLn $ show n
           else do
                line <- hGetLine inh
                let (x, y) = splitAt 5 $ parse $ words line
                if (beat `on` reverse) (sort x) (sort y)
                    then mainLoop inh (n + 1)
                    else mainLoop inh n
