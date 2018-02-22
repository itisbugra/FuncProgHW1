module ZellerImplementation where
  import Prelude

  centurySize :: Integer
  centurySize = 100

  dayOfWeek :: Integer -> Integer -> Integer -> Integer
  dayOfWeek day month year = round(calculatedDay) `mod` 7 where
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
