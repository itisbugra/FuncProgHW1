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

-- | Application start point.
main :: IO ()
main = do putStrLn (show (sundays1 1901 2000))
          putStrLn (show (sundays2 1901 2000))
          putStrLn (show (sundays1tr 1901 2000))

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
    zeroBasedCentury = floor ((fromIntegral year) / (fromIntegral centurySize))
    mappedMonth :: Integer
    mappedMonth = [13, 14, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]!!(fromIntegral month)
    calculatedDay :: Integer
    calculatedDay =
      day +
      floor (fromIntegral (13 * (mappedMonth + 1)) / 5.00) +
      yearOfCentury +
      floor ((fromIntegral yearOfCentury) / 4.00) +
      floor ((fromIntegral zeroBasedCentury) / 4.00) +
      (5 * zeroBasedCentury)

-- | Calculates the number of months starting with sundays in given time interval of years.
sundays1 :: Integer -- ^ Starting year of the interval.
         -> Integer -- ^ Ending year of the interval.
         -> Integer -- ^ Number of sundays between given years.
sundays1 start end = sundays' start 1
  where
    sundays' :: Integer -> Integer -> Integer
    sundays' y m
      | y > end   = 0
      | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
      where
        nextY = sundays' (y + 1) 1
        nextM = sundays' y (m + 1)
        rest  = if m < 12 then nextM else nextY

-- | Tail recursive variant of `sundays1 (Integer, Integer) -> Integer`.
sundays1tr :: Integer -- ^ Starting year of the interval.
           -> Integer -- ^ Ending year of the interval.
           -> Integer -- ^ Number of sundays between given years.
sundays1tr start end = sundays' start 1 0
  where
    sundays' :: Integer -> Integer -> Integer -> Integer
    sundays' y m acc
      | y > end   = acc
      | otherwise = if dayOfWeek y m 1 == 1 then inc else pass
      where
      nextY acc = sundays' (y + 1) 1 acc
      nextM acc = sundays' y (m + 1) acc
      ftor newAcc = if m < 12 then nextM newAcc else nextY newAcc
      inc = if m < 12 then nextM (acc + 1) else nextY (acc + 1)
      pass = if m < 12 then nextM acc else nextY acc

-- | Finds out if the given year has a leap day.
leap :: Integer -- ^ Year.
     -> Bool -- ^ Truthy value if year has a leap day, otherwise false.
leap year = isForth && not(isCenturyStart) || isForthCenturyStart
  where
    isForth :: Bool
    isForth = (year `mod` leapWidth) == 0
    isCenturyStart :: Bool
    isCenturyStart = (year `mod` centurySize) == 0
    isForthCenturyStart :: Bool
    isForthCenturyStart = (year `mod` (centurySize * leapWidth)) == 0

-- | Calculates days in given month-year pair.
daysInMonth :: Integer -- ^ Month of the year.
            -> Integer -- ^ Year.
            -> Integer -- ^ Number of days.
daysInMonth month year
  | month == 2  = if leap(year) then 29 else 28
  | month == 4 || 
    month == 6 || 
    month == 9 || 
    month == 11 = 30
  | otherwise   = 31

-- | Second variant of function `sundays1 (Integer, Integer) -> Integer`, using week days in order
-- calculate number of sundays between given years.
sundays2 :: Integer -- ^ Starting year of the interval.
         -> Integer -- ^ Ending year of the interval.
         -> Integer -- ^ Number of sundays between given years.
sundays2 start end = sundays' start 1 2
  where
    sundays' :: Integer -> Integer -> Integer -> Integer
    sundays' year month weekDay
      | year > end   = 0
      | otherwise = if nextWeekday `mod` 7 == 0 then rest + 1 else rest
      where
        nextY :: Integer
        nextY = sundays' (year + 1) 1 nextWeekday
        nextM :: Integer
        nextM = sundays' year (month + 1) nextWeekday
        nextWeekday :: Integer
        nextWeekday = weekDay + ((daysInMonth month year) `mod` weekSize)
        rest :: Integer
        rest  = if month < 12 then nextM else nextY
