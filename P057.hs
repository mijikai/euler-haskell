#!/usr/bin/env runhaskell
import Data.Ratio
import Data.Function

f :: Rational -> Rational
f x = 2 + 1 / x

greater_length :: Integer -> Integer -> Bool
greater_length = ((>) `on` (length . show))

no_of_iteration = 1000
main :: IO()
main = putStrLn $ show $ length $ filter
            (\frac -> (numerator frac) `greater_length` (denominator frac))
            $ take no_of_iteration $ tail $ map (subtract 1) $ iterate f 2
