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

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- | Size of a century.
centurySize :: Integer
centurySize = 100

-- | Width of a leap.
leapWidth :: Integer
leapWidth = 4

-- | Size of a week in calendar.
weekSize :: Integer
weekSize = 7

-- | Calculates day of a week on a Gregorian calendar by using *Zeller's congruence* method.
dayOfWeek :: Integer -- ^ Year.
          -> Integer -- ^ Month of the year.
          -> Integer -- ^ Day of the month, e.g. 21.
          -> Integer -- ^ Calculated day of the week.
dayOfWeek year realMonth day = calculatedDay `mod` weekSize
  where
    month :: Integer
    month = realMonth - 1
    yearOfCentury :: Integer
    yearOfCentury = year `mod` centurySize
    zeroBasedCentury :: Integer
    zeroBasedCentury = floor(fromIntegral(year) / fromIntegral(centurySize))
    mappedMonth :: Integer
    mappedMonth = [13, 14, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]!!(fromIntegral(month))
    calculatedDay :: Integer
    calculatedDay =
      day +
      floor(fromIntegral(13 * (mappedMonth + 1)) / 5.00) +
      yearOfCentury +
      floor(fromIntegral(yearOfCentury) / 4.00) +
      floor(fromIntegral(zeroBasedCentury) / 4.00) +
      (5 * zeroBasedCentury)

  zeroBasedCentury :: Integer
  zeroBasedCentury = floor(fromInteger(year) / fromInteger(centurySize))

  calculatedDay :: Integer
  calculatedDay = month + yearOfCentury + (realToFrac(zeroBasedCentury) / realToFrac(4.00))

