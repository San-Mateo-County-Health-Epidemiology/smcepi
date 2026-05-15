# Relative standard error calculation for sample and survey data

Use this function to calculate the relative standard error (RSE) for a
sample and survey data. The RSE measures the reliability of an estimate
by dividing the standard error of the estimate by the estimate. The RSE
is displayed as a percentage of the estimate.

## Usage

``` r
prop_rse(prop_sd, n, method)
```

## Arguments

- prop_sd:

  This is the standard deviation of the proportion.

- n:

  This is the sample size of the data.

- method:

  This is the method by which the RSE is calculated.

  - `"sample"`: used to calculate the RSE of surveillance data sets

  - `"survey"`: utilized to calculate the RSE of data from a survey or a
    limited data set from a larger population.

## Value

The relative standard error of sample or survey data.

## Examples

``` r
if (FALSE) { # \dontrun{

prop_rse(25, 1050, method = "survey")
} # }
```
