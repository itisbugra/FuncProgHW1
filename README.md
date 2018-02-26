# FuncProgHW1

Functional Programming, Assignment 1.

Implements *Zeller's congruence* to solve the *Counting Sundays* problem.

#### Equivalent ES7 implementation

```javascript
function dayOfWeek(day, month, year) {
  const yearOfCentury = year % 100;
  const zeroBasedCentury = Math.floor(year / 100);
  const mappedMonth = [13, 14, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12][month];

  return (
    day + 
    Math.floor((13 * (mappedMonth + 1)) / 5) + 
    yearOfCentury + 
    Math.floor(yearOfCentury / 4) + 
    Math.floor(zeroBasedCentury / 4) + 
    (5 * zeroBasedCentury)
  ) % 7;
}
```

## Installation and usage

1. Run *cabal* build engine: `$ cabal install -j`.
2. Run `$ .cabal-sandbox/bin/ZellersCongruence` to start the application.

### Answer for the math question

Quoting from [Wikipedia.org](https://en.wikipedia.org/wiki/Leap_year):

> ... in the Gregorian calendar, each leap year has 366 days instead of the usual 365, by extending February to 29 days rather than the common 28. These extra days occur in years which are multiples of four (with the exception of years divisible by 100 but not by 400).

Each week contains 7 days. Without leap days, 400 years would contain (365 * 400) days, which would
be equal to **146.000** days.

There should be 24 leap days in a century, if the destination year is not divisible by 400, which adds (24 * 3) days, which would be equal to **72**.

One of the centuries found in 400 years interval has **25** leap days since it is divisible by 400.

That makes **146.097** days, number of days in 400 years is a multiple of 7.

Pick one day would be bounded to the same probability, all days are equally possible since this is a uniform distribution.

## License

MIT with additional statements. See license file for more information.
