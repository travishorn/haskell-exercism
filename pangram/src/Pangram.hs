--Determine if a sentence is a pangram. A pangram (Greek: παν γράμμα, pan
-- gramma, "every letter") is a sentence using every letter of the alphabet at
-- least once. The best known English pangram is:

--The quick brown fox jumps over the lazy dog.

--The alphabet used consists of ASCII letters a to z, inclusive, and is case
-- insensitive. Input will not contain non-ASCII symbols.

module Pangram (isPangram) where

-- Import toUpper from Data.Char. This function turns a lowercase Char into its
-- uppercase counterpart
import Data.Char (toUpper)

-- Map over all Chars in a String and make them uppercase. This will be useful
-- to make the input string all uppercase. Then we can compare it to the full
-- uppercase alphabet. This effectively makes our end function case-insensitive.
-- Shorthand version of `allUpper s = map toUpper s`
allUpper :: [Char] -> [Char]
allUpper = map toUpper

-- Get the whole alphabet, uppercased
alphabet :: [Char]
alphabet = ['A'..'Z']

-- The following commented out function gets us close. It maps over the alphabet
-- and calls `elem` on each letter, checking it against the input text. Returns
-- a list of 26 Bools, one for each letter. If it exists in the input text, the
-- Bool is True, otherwise False.

-- isPangram text = map (`elem` allUpper text) alphabet

-- Replace `map` with `all` to get the solution. The `all` function is similar
-- to map, but it reduces a list to a single Bool. If it gets to the end of the
-- list without returning False on a single item, it returns True. Otherwise, if
-- any of the iterations returns False, it returns False.

isPangram :: String -> Bool
isPangram text = all (`elem` allUpper text) alphabet
