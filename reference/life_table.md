# life table

this is the function called in the
[`make_life_table()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/make_life_table.md).
It calculates the vectors required for creating life expectancy methods
outlined in the Public Health England calculator that lives
[here](https://fingertips.phe.org.uk/documents/phe%20life%20expectancy%20calculator.xlsm).

## Usage

``` r
life_table(data)
```

## Arguments

- data:

  a data frame

## Value

a data frame

## Details

Some things have been added, including the option for 0-4 ages (instead
of 0 and 1-4) and 85+ (instead of 85-89 and 90+)

## Examples

``` r
# don't run this on its own - run make_life_table()
```
