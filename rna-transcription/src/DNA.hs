-- Given a DNA strand, return its RNA complement (per RNA transcription).

-- Both DNA and RNA strands are a sequence of nucleotides.

-- The four nucleotides found in DNA are adenine (A), cytosine (C), guanine (G)
-- and thymine (T).

-- The four nucleotides found in RNA are adenine (A), cytosine (C), guanine (G)
-- and uracil (U).

-- Given a DNA strand, its transcribed RNA strand is formed by replacing each
-- nucleotide with its complement:

-- G -> C
-- C -> G
-- T -> A
-- A -> U

-- Given invalid output, your program should return the first invalid character.

module DNA (toRNA) where

-- Match a specific DNA nucleotide with an RNA nucleotide.
toRNANucleotide :: Char -> Char
toRNANucleotide x
  | x == 'G' = 'C'
  | x == 'C' = 'G'
  | x == 'T' = 'A'
  | x == 'A' = 'U'
  | otherwise = error "Not a valid DNA nucleotide."

toRNA :: String -> Either Char String

-- If an empty string is passed in, return an empty string back.
-- Right indicates we are returning a String (from the `Either Char String`
-- definition).

toRNA [] = Right []

toRNA (x:xs)
  -- x is the current nucleotide, xs are the rest
  -- If the current nucleotide is one of "GCTA", push the matching RNA
  -- nucleotide into the stack and map over the rest of the sequence with toRNA
  -- again (recursion).
  | x `elem` "GCTA" = (toRNANucleotide x :) <$> toRNA xs

  -- If the current nucleotide is not a valid DNA nucleotide, return it alone.
  -- Left indicates we're returning a Char (from the `Either Char String`
  -- definition).
  | otherwise = Left x
