# Data Management

These functions are for small data processing tasks.

## remove_right_empty_cols()

This function removes all empty columns on the right of a data set. This
function is useful when you import a file and R adds empty columns to
the right of the actual data. This function won’t remove empty columns
on the left or in the middle of the data set.

### it will remove empty columns on the right…

This data has two empty columns on the right:

``` r

data <- data.frame(
  col1 = sample(1:100, 100, replace = T),
  col2 = NA,
  col3 = runif(100),
  col4 = sample(c("blue", "green", "yellow", "orange"), 100, replace = T),
  col5 = NA,
  col6 = NA
)
head(data)
#>   col1 col2      col3   col4 col5 col6
#> 1   45   NA 0.2485387   blue   NA   NA
#> 2   23   NA 0.4028812 yellow   NA   NA
#> 3   76   NA 0.7696302 yellow   NA   NA
#> 4   63   NA 0.1194854 yellow   NA   NA
#> 5   47   NA 0.1946950   blue   NA   NA
#> 6   31   NA 0.1645692   blue   NA   NA
```

After passing it through
[`remove_right_empty_cols()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/remove_right_empty_cols.md),
the two empty columns on the right (`col5` and `col6` have been
dropped):

``` r

head(remove_right_empty_cols(data))
#>   col1 col2      col3   col4
#> 1   45   NA 0.2485387   blue
#> 2   23   NA 0.4028812 yellow
#> 3   76   NA 0.7696302 yellow
#> 4   63   NA 0.1194854 yellow
#> 5   47   NA 0.1946950   blue
#> 6   31   NA 0.1645692   blue
```

### … and only on the right.

If you accidentally run this function on a data set that doesn’t have
any empty columns on the right, the data set will not change. The
rightmost variable in this data set has information in it:

``` r

data <- data.frame(
  col1 = sample(1:100, 100, replace = T),
  col2 = NA,
  col3 = runif(100),
  col4 = NA,
  col5 = NA,
  col6 = sample(c("blue", "green", "yellow", "orange"), 100, replace = T)
)

head(data)
#>   col1 col2      col3 col4 col5   col6
#> 1   37   NA 0.8912849   NA   NA   blue
#> 2   16   NA 0.5880786   NA   NA   blue
#> 3   51   NA 0.6332316   NA   NA   blue
#> 4  100   NA 0.2595095   NA   NA   blue
#> 5   33   NA 0.3182127   NA   NA   blue
#> 6   29   NA 0.3392539   NA   NA orange
```

In this example `col6` has data in it, so no variables are dropped from
the data set.

``` r

head(remove_right_empty_cols(data))
#>   col1 col2      col3 col4 col5   col6
#> 1   37   NA 0.8912849   NA   NA   blue
#> 2   16   NA 0.5880786   NA   NA   blue
#> 3   51   NA 0.6332316   NA   NA   blue
#> 4  100   NA 0.2595095   NA   NA   blue
#> 5   33   NA 0.3182127   NA   NA   blue
#> 6   29   NA 0.3392539   NA   NA orange
```
