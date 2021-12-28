-- Each of us inherits from our biological parents a set of chemical
-- instructions known as DNA that influence how our bodies are constructed. All
-- known life depends on DNA!

-- Note: You do not need to understand anything about nucleotides or DNA to
-- complete this exercise.

-- DNA is a long chain of other chemicals and the most important are the four
-- nucleotides, adenine, cytosine, guanine and thymine. A single DNA chain can
-- contain billions of these four nucleotides and the order in which they occur
-- is important! We call the order of these nucleotides in a bit of DNA a "DNA
-- sequence".

-- We represent a DNA sequence as an ordered collection of these four
-- nucleotides and a common way to do that is with a string of characters such
-- as "ATTACG" for a DNA sequence of 6 nucleotides. 'A' for adenine, 'C' for
-- cytosine, 'G' for guanine, and 'T' for thymine.

-- Given a string representing a DNA sequence, count how many of each nucleotide
-- is present. If the string contains characters that aren't A, C, G, or T then
-- it is invalid and you should signal an error.

-- For example:

-- "GATTACA" -> [ (A, 3), (C, 1), (G, 1), (T, 2) ]
-- "INVALID" -> error

module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (fromListWith, Map)

-- Custom enumerated data type. Nucleotides can be one of A, C, G, or T.
data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- If x is not one of "ACGT" then False. Otherwise, True.
isInvalidNucleotide :: Char -> Bool
isInvalidNucleotide x = x `notElem` "ACGT"

-- Convert a single character nucleotide into its corresponding enumeration from
-- the Nucleotide data type.
-- Problem with this function: it doesn't handle invalid input. Instead, you
-- must check for validity before calling it. This is not good practice.
toNucleotide :: Char -> Nucleotide
toNucleotide x
  | x == 'A' = A
  | x == 'C' = C
  | x == 'G' = G
  | x == 'T' = T

-- Pairs each character into a tuple with the number 1.
-- pairWithOne "GATT" = [ (G, 1), (A, 1), (T, 1), (T, 1) ]
pairWithOne :: String -> [(Nucleotide, Int)]
pairWithOne = map (\x -> (toNucleotide x, 1))

-- Merge (Nucleotide, 1) pairs so that each nucleotide appears once and is
-- paired with the total number of occurences.
sumPairs :: [(Nucleotide, Int)] -> Map Nucleotide Int
sumPairs = fromListWith (+)

-- Combine the two functions above so that pairs are created and then merged.
-- This is the main solution, but doesn't handle invalid input.
sumNucleotides :: String -> Map Nucleotide Int
sumNucleotides xs = sumPairs $ pairWithOne xs

-- First, check if any passed in nucleotides are invalid. If so, error out.
-- Otherwise, sum the nucleotides.
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = if any isInvalidNucleotide xs
  then Left "Error: Invalid nucleotide in strand"
  else Right $ sumNucleotides xs
