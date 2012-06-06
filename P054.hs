data CardRank = High | OnePair | TwoPairs | ThreeKind
    | Straight | Flush | FullHouse | FourKind
    | StraightFlush | RoyalFlush
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

conseq :: [(Int, Char)] ->  Bool
conseq cards = helper (tail cards) (fst (head cards))
    where helper cards current
            | null cards = True
            | current /= next - 1 = False
            | otherwise = helper (tail cards) next
            where next = fst $ head cards

group_by :: (Eq b) => [a] -> (a -> b) -> [[a]]
group_by (x:xs) key = helper (key x) xs [x] []
    where helper curr list group all
            | null list = (reverse ((reverse group):all))
            | f == curr = helper curr rest (first:group) all
            | otherwise = helper f rest [first] ((reverse group):all)
            where first = head list 
                  f = key first
                  rest = tail list
