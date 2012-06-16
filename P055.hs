#!/usr/bin/env runhaskell

reverse_num :: Integer -> Integer
reverse_num 0 = 0
reverse_num num = helper num 0
    where helper num rev
            | num == 0 = rev
            | otherwise = helper (num `div` 10) (10 * rev + (num `mod` 10))

is_pallindrome :: Integer -> Bool
is_pallindrome num = num == reverse_num num

reverse_and_add :: Integer -> Integer
reverse_and_add num = num + (reverse_num num)

iter_limit = 50

is_lychrel :: Integer -> Bool
is_lychrel num = (length $ take iter_limit $ takeWhile (not . is_pallindrome)
                    $ tail $ iterate reverse_and_add num) == iter_limit

lst = [1..10000 - 1]

main :: IO()
main = putStrLn $ show $ length $ filter is_lychrel lst
