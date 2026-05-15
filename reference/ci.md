# Confidence interval for a proportion

Use this function to calculate the lower and upper 95% confidence
intervals for a proportion (e.g. rate) using the normal approximation
method. The confidence interval estimates a range of values that capture
the true population value for a variable of interest. For a 95%
confidence interval, if a rate of 5.5 falls within a 95% confidence
interval of 4.5, 6.5, it means that one is 95% confidence that the true
value from the population falls within that range.

## Usage

``` r
ci(num, denom)
```

## Arguments

- num:

  This is the numerator of the proportion.

- denom:

  This is the denominator of the proportion.

## Value

The lower and upper bounds of the 95% confidence interval for a given
proportion.

## Examples

``` r

ci(5, 25)
#> Error in ci(5, 25): object 'num' not found

```
