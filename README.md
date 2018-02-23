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

## License

MIT with additional statements. See license file for more information.
