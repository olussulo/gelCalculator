--gelcalc.hs calculates how many gels is needed for the given amount of samples
--Assumes 3 controls and 2 ladders per gel
module GelCalculator where
  firstGel = 12
  otherGel = 15
  myGel x =
          if (x > 12)
            then 1 + (ceiling ((x - firstGel) / otherGel))
          else
            ceiling (x / firstGel)
         
