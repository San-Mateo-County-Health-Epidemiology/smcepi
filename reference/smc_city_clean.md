# cleaning up San Mateo Cities in a data frame

This function is meant to be used to clean up San Mateo cities in a
dataframe. It will only look for San Mateo County cities - it doesn't
look for other cities. Non-San Mateo cities will return an `NA` in the
`city_clean` column.

## Usage

``` r
smc_city_clean(data,
      city_col = "city",
      new_col = "city_clean")
```

## Arguments

- data:

  This is the name of the dataframe with the city variable in it that
  you'd like to clean

- city_col:

  (optional): This is a string that specifies the name of the city
  variable you want to clean. By default, the function assumes the
  variable is called `city`.

- new_col:

  (optional): This is a string to specify the name of the variable with
  the cleaned city names. By default the cleaned cities will be saved in
  a variable called `city_clean`

## Value

a dataset with a new variable for the cleaned city values

## Examples

``` r
data <- data.frame(
 city_dirty = c("Burligame", "Fost City", "San Mato", "Daily Cit", "S S Francisco",
                "South San Fransico", "SoSan Franc", "San Francisco")
 )

data %>%
    smc_city_clean(city_col = "city_dirty",
                   new_col = "city_smc")
#>           city_dirty            city_smc
#> 1          Burligame          Burlingame
#> 2          Fost City         Foster City
#> 3           San Mato           San Mateo
#> 4          Daily Cit           Daly City
#> 5      S S Francisco South San Francisco
#> 6 South San Fransico South San Francisco
#> 7        SoSan Franc South San Francisco
#> 8      San Francisco                <NA>

```
