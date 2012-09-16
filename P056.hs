#!/usr/bin/env runhaskell
{-
 - Find the maximum digit sum of a ^ b where a, b < 100
 -}
import Char

main :: IO()
main = maximum $ map (sum . (map digitToInt)) [ show $ i ^ j | i <- [1..100], j <- [1..100]]
