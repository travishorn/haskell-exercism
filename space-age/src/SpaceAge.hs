-- Given an age in seconds, calculate how old someone would be on:

-- Mercury: orbital period 0.2408467 Earth years
-- Venus: orbital period 0.61519726 Earth years
-- Earth: orbital period 1.0 Earth years, 365.25 Earth days, or 31557600 seconds
-- Mars: orbital period 1.8808158 Earth years
-- Jupiter: orbital period 11.862615 Earth years
-- Saturn: orbital period 29.447498 Earth years
-- Uranus: orbital period 84.016846 Earth years
-- Neptune: orbital period 164.79132 Earth years

-- So if you were told someone were 1,000,000,000 seconds old, you should be
-- able to say that they're 31.69 Earth-years old.

module SpaceAge (Planet(..), ageOn) where

-- Planet is an enumerated type. A planet can be any of these possible types.
data Planet = Mercury
  | Venus
  | Earth
  | Mars 
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

-- Convert seconds to years. One year is 31557600 seconds. Just divide the
-- number of seconds passed in by that number to get years.
secondsToYears :: Float -> Float
secondsToYears seconds = seconds / 31557600

-- ageOn takes a Planet (can be any of the values defined in the enumerated type
-- declaration above) and a number of seconds. It first converts the seconds to
-- years using the function above. Then it divides that number by a specific
-- value depending on the Planet.
-- 1 year on Mercury is 0.2408467 years on Earth. So it gets divided by that
-- number, and so on.
ageOn :: Planet -> Float -> Float
ageOn planet seconds = secondsToYears seconds / case planet of
  Mercury ->   0.2408467
  Venus   ->   0.61519726
  Earth   ->   1.0
  Mars    ->   1.8808158 
  Jupiter ->  11.862615
  Saturn  ->  29.447498
  Uranus  ->  84.016846
  Neptune -> 164.79132
