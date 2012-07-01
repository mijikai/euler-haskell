#!/usr/bin/env runhaskell
import Data.Bits

is_prime :: Integer -> Bool
is_prime n
    | n < 0 = is_prime (-n)
    | n == 2 = True
    | n == 1 || n `mod` 2 == 0 = False
    | otherwise = all (miller_rabbin_prime_test n)
                    $ filter (<n) [2, 7, 61]

find_pow_2 :: Integer -> (Integer, Integer)
find_pow_2 n = find 0 (n - 1)
    where find expo rem
            | rem `mod` 2 == 0 = find (expo + 1) (rem `div` 2)
            | otherwise = (expo, rem)

miller_rabbin_prime_test :: Integer -> Integer -> Bool
miller_rabbin_prime_test n a
    | x == 1 = True
    | otherwise = test x r
    where (r, d) = find_pow_2 n
          x = a ^ d `mod` n
          test x r
                | x == n - 1 = True
                | r < 0 = False
                | otherwise = test ((x * x) `mod` n) (r - 1)
