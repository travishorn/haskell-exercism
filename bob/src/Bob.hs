module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, toUpper)
import Data.List (dropWhile, dropWhileEnd)

-- Use `any` to map over entire string with the `isAlpha` function from
-- Data.Char. If any character is alphabetic, return True
hasAlpha :: String -> Bool
hasAlpha = any isAlpha

-- Map over the string with `toUpper` to capitalize every character.
allUpper :: String -> String
allUpper = map toUpper

-- `dropWhileEnd isSpace` will drop whitespace characters from the end of the
-- string. We pass the result of that to `dropWhile isSpace` to drop whitespace
-- from the front of the string. Effectively ignoring any whitespace at the
-- beginning and/or end of the string
trim :: String -> String
trim s = dropWhile isSpace (dropWhileEnd isSpace s)

-- If the string is completely empty after trimming, it's silence.
isSilence :: String -> Bool
isSilence s = trim s == ""

-- If the string has alphabetic characters and the string is identical to itself
-- after capitalizing every character, then it's all caps. Note: we need to make
-- sure that it contains at least one alphabetic character because "123" would
-- pass would return True otherwise.
isShouting :: String -> Bool
isShouting s = hasAlpha s && s == allUpper s

-- If the last character after trimming is a question mark, it's a question.
isQuestion :: String -> Bool
isQuestion s = last (trim s) == '?'

-- Return a value depending on what tests the string passes.
responseFor :: String -> String
responseFor s
  | isSilence s = "Fine. Be that way!"
  | isShouting s && isQuestion s = "Calm down, I know what I'm doing!"
  | isQuestion s = "Sure."
  | isShouting s = "Whoa, chill out!"
  | otherwise = "Whatever."
