# Classify zip codes by region in San Mateo County

This function sorts 5-digit San Mateo zip codes into county regions:
north, south, mid, coastside and "not a residential zip". Non-San Mateo
zipcodes will return an `NA` in the `city_clean` column.

## Usage

``` r
smc_zip_region_sort(data,
                           zip_col = "zip",
                           region_col = "zip_region")
```

## Arguments

- data:

  This is the name of the dataframe with the zip code variable in it
  that you'd like to sort

- zip_col:

  (optional): This is a string that specifies the name of the zip code
  variable you want to sort. By default the function assumes the
  zipcodes are stored in a variable called `zip`

- region_col:

  (optional): This is a string to specify the name of the variable with
  the zip regions. By default the sorted zipcodes will be saved in a
  variable called `region_col`

## Value

a dataset with a new variable for the sorted zip codes

## Examples

``` r
data <- data.frame(
  cty_zip = c("94015", "94403", "94303", "94019", "94128", "94110")
  )

data %>%
  smc_zip_region_sort(zip_col = "cty_zip",
                      region_col = "cty_zip_region")
#>   cty_zip        cty_zip_region
#> 1   94015                 North
#> 2   94403                   Mid
#> 3   94303                 South
#> 4   94019                 Coast
#> 5   94128 Not a residential zip
#> 6   94110                  <NA>

```
