{- |
Module      :  ZellersCongruence
Description :  Application start point.
Copyright   :  (c) 2018 Buğra Ekuklu (chatatata)
License     :  See LICENSE.md

Maintainer  :  ekuklu[at]icloud.com
Stability   :  experimental
Portability :  portable

Implements *Zeller's congruence* in order to calculated the day of the week on given day, month
and year in Gregorian calendar.
-}
module ZellersCongruence where
  import Prelude

  -- | Size of a century.
  centurySize :: Integer
  centurySize = 100

  -- | Data type representing day of a week.
  data DayOfWeek = 
    Saturday | Sunday | Monday | Tuesday | Wednesday | Thursday | Friday  deriving (Enum)

  -- | Calculates day of a week on a Gregorian calendar by using *Zeller's congruence* method.
  dayOfWeek :: Integer -- ^ Day of the month, e.g. 21.
            -> Integer -- ^ Month of the year.
            -> Integer -- ^ Year.
            -> Integer -- ^ Calculated day of the week.
  dayOfWeek day month year = 
      calculatedDay `mod` 7 where
    yearOfCentury :: Integer
    yearOfCentury = year `mod` centurySize

    zeroBasedCentury :: Integer
    zeroBasedCentury = floor(fromInteger(year) / fromInteger(centurySize))

    calculatedDay :: Integer
    calculatedDay = month + yearOfCentury + (realToFrac(zeroBasedCentury) / realToFrac(4.00))
