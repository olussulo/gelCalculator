--gelcalc.hs calculates how many gels are needed
--for the given amount of samples
--Considers controls as samples
--assumes 2 ladders per big gel
--1 ladder in smallGel.
module GelCalculator where
  data Gel = Big | Small deriving Show
  gelSize :: Gel -> Integer
  gelSize Small = 7
  gelSize Big = 15


  myGel :: Integer -> [(Gel, Integer)]
  myGel samples = [(Big, (gelResult - lastGel)), (Small, lastGel)]
    where
      gelResult = ceiling (fromInteger (samples) / 15)
      lastGel = if (samples - (gelSize Big)) `mod` (gelSize Big) < 8 &&
                   (samples - (gelSize Big)) `mod` (gelSize Big) > 0
                then 1
                else 0


  -- Sergio's: sulo x = ((x+10) `div` 15, (x+2) `mod` 15 < 7)

-- It should work
