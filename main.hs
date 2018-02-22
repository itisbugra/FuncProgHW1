{- |
Module      :  Main
Description :  Application start point.
Copyright   :  (c) 2018 Buğra Ekuklu (chatatata)
License     :  See LICENSE.md

Maintainer  :  ekuklu[at]icloud.com
Stability   :  experimental
Portability :  portable

Implements *Zeller's congruence* in order to calculated the day of the week on given day, month
and year in Gregorian calendar.
-}
module Main where
  import Prelude

  -- | Size of a century.
  centurySize :: Integer
  centurySize = 100


  data DayOfWeek = 
    Saturday | Sunday | Monday | Tuesday | Wednesday | Thursday | Friday  deriving (Enum)

  -- | Calculates day of a week on a Gregorian calendar by using *Zeller's congruence* method.
  dayOfWeek :: Integer -- ^ Day of the month, e.g. 21.
            -> Integer -- ^ Month of the year.
            -> Integer -- ^ Year.
            -> Integer -- ^ Calculated day of the week.
  dayOfWeek day month year = 
      round(calculatedDay) `mod` 7 where
    yearOfCentury :: Fractional a => a
    yearOfCentury = realToFrac(fromInteger(year `mod` centurySize))

    flooredQuarterOfYearOfCentury :: Fractional a => a
    flooredQuarterOfYearOfCentury = floor(yearOfCentury / 4.00)

    zeroBasedCentury :: Fractional a => a
    zeroBasedCentury = realToFrac(fromInteger(year) / 100.00)

    flooredQuarterOfZeroBasedCentury :: Fractional a => a
    flooredQuarterOfZeroBasedCentury = floor(zeroBasedCentury / 4.00)

    flooredZeroBasedCentury :: Fractional a => a
    flooredZeroBasedCentury = floor(zeroBasedCentury)

    calculatedDay :: Fractional a => a
    calculatedDay = fromInteger(day) + (13.00 * (fromInteger(month) + 1.00)) / 5.00 + yearOfCentury + flooredQuarterOfYearOfCentury + flooredQuarterOfZeroBasedCentury - 2.00 * flooredZeroBasedCentury
