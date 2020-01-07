module Puzzle where

import Data.Char( toLower )

-- creates a sentence from 26 natural numbers
-- comment in/out different strings to get variations

sentence :: [Int] -> String
sentence ks =
  "This sentence contains "                        -- has a solution
  -- "This sentence has "                             -- has a solution
  -- "Would you believe that this sentence contains " -- has a solution
  -- "At the WM, we counted "                         -- has a solution
  -- "No sentence contains exactly "                  -- has a solution
  -- "This Haskell-generated sentence has "           -- has a solution
  -- "Are there sentences that have "         -- I have not found a solution :-o
  ++ andIt [ number k ++ " "
                      ++ [c]
                      ++ if k > 1 then "'s" else ""
           | (k,c) <- ks `zip` ['a'..'z']
           , k > 0
           ]
  ++ "."
 where
  andIt [] = "- and -"
  andIt xs = concat [ x ++ ", " | x <- init xs ] ++ "and " ++ last xs

-- linearizes a natural number 1..59 in English

number :: Int -> String
number  1 = "one"
number  2 = "two"
number  3 = "three"
number  4 = "four"
number  5 = "five"
number  6 = "six"
number  7 = "seven"
number  8 = "eight"
number  9 = "nine"
number 10 = "ten"
number 11 = "eleven"
number 12 = "twelve"
number 13 = "thirteen"
number 15 = "fifteen"
number 18 = "eighteen"
number  k | k > 12 && k < 20 = number (k-10) ++ "teen"
number  k = decs !! (k `div` 10 - 2)
            ++ if k `mod` 10 == 0 then "" else "-" ++ number (k `mod` 10)
 where
  decs = ["twenty", "thirty", "forty", "fifty"] ++ error ("number " ++ show k)

-- counts all the letters in a given string as a list of 26 results

count :: String -> [Int]
count s = [ length (filter ((c==) . toLower) s) | c <- ['a'..'z'] ]

-- checks if a list of 26 numbers is a solution to the puzzle

check :: [Int] -> Bool
check ks = count (sentence ks) == ks

