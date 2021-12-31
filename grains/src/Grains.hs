-- Calculate the number of grains of wheat on a chessboard given that the number
-- on each square doubles.

-- There once was a wise servant who saved the life of a prince. The king
-- promised to pay whatever the servant could dream up. Knowing that the king
-- loved chess, the servant told the king he would like to have grains of wheat.
-- One grain on the first square of a chess board, with the number of grains
-- doubling on each successive square.

-- There are 64 squares on a chessboard (where square 1 has one grain, square 2
-- has two grains, and so on).

-- Write code that shows:
-- - how many grains were on a given square, and
-- - the total number of grains on the chessboard

module Grains (square, total) where

-- An infinite list that starts with 0, 1, then doubles forever
-- : is the "cons" operator. It constructs lists. The list is constructed of 0,
-- 1, and then a "list comprehension." The list comprehension takes the tail of
-- the list, assigns it to x, then doubles x. This is the next item in the list.
squares :: [Integer]
squares = 0 : 1 : [x * 2 | x <- tail squares]

-- Get the number of grains on a given square
square :: Int -> Maybe Integer
square n
  -- The problem states a chess board. It has squares 1 to 64 inclusive.
  -- Anything outside that range should return Nothing
  | n < 1 || n > 64 = Nothing

  -- If the requested square is within the range, give the square at index n
  -- !! is the list index operator
  | otherwise = Just (squares !! n)

-- Given a number of squares n, return the sum of the grains on those squares
-- from 1 to n.
sumSquares :: Int -> Integer
sumSquares n =
  -- The sum of...
  sum $

  -- Taking n+1 squares. 1 is added because the tests expect the list to start
  -- chess square 1, while the squares list actually starts at 0.
  take (n + 1) squares

-- Sum squares from 1 to 64
total :: Integer
total = sumSquares 64

-- BONUS

-- The value of any square is equivalent to "2 to the power of that square's
-- index minus 1". The square function be optimized to this:
-- square n
--   | n < 1 || n > 64 = Nothing
--   | otherwise = Just $ 2 ^ (n - 1)

-- Also, total never changes. The most optimized way to write it is like this:
-- total = 18446744073709551615
