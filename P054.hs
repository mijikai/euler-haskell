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
            | current /= succ next = False
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
