# Remove all empty columns on the right end of a data set.

This function will drop empty columns at the far right of a data set.
This is useful when you import data and get empty columns on the right
that shouldn't be there. This function only removes empty columns on the
right - it doesn't remove empty columns anywhere else.

## Usage

``` r
remove_right_empty_cols(data)
```

## Arguments

- data:

  a data frame

## Value

the inputted data frame without empty columns on the right

## Examples

``` r

data <- data.frame(
  col1 = sample(1:100, 100, replace = TRUE),
  col2 = NA,
  col3 = runif(100),
  col4 = sample(c("blue", "green", "yellow", "orange"), 100, replace = TRUE),
  col5 = NA,
  col6 = NA)

remove_right_empty_cols(data)
#>     col1 col2        col3   col4
#> 1     68   NA 0.663206581 orange
#> 2     73   NA 0.856575005 yellow
#> 3     69   NA 0.926546448 yellow
#> 4      5   NA 0.552377595 yellow
#> 5     24   NA 0.577065694 yellow
#> 6     79   NA 0.687447746  green
#> 7     77   NA 0.244718230 orange
#> 8      2   NA 0.044617158 orange
#> 9     62   NA 0.909854557 yellow
#> 10    55   NA 0.070681219 yellow
#> 11    43   NA 0.996891473 orange
#> 12    62   NA 0.611852417 orange
#> 13    43   NA 0.172558846 orange
#> 14     5   NA 0.909440965 yellow
#> 15    85   NA 0.037451167 orange
#> 16    44   NA 0.593553790 orange
#> 17    61   NA 0.236977555 orange
#> 18    34   NA 0.906297267 orange
#> 19    70   NA 0.818872984  green
#> 20    63   NA 0.699829357 yellow
#> 21     4   NA 0.220000329  green
#> 22    34   NA 0.727990938  green
#> 23    35   NA 0.217084462 orange
#> 24    89   NA 0.456230198 orange
#> 25    86   NA 0.332799758   blue
#> 26    43   NA 0.568352669   blue
#> 27     7   NA 0.252205725  green
#> 28    32   NA 0.464013567  green
#> 29    53   NA 0.917660507   blue
#> 30    70   NA 0.972844218   blue
#> 31     9   NA 0.819082447 yellow
#> 32    22   NA 0.902923798   blue
#> 33    10   NA 0.581366044 orange
#> 34    32   NA 0.773008481 yellow
#> 35    34   NA 0.995123026 orange
#> 36    17   NA 0.710971250 yellow
#> 37    52   NA 0.214942596 yellow
#> 38    56   NA 0.291757630 orange
#> 39    22   NA 0.721759729 orange
#> 40    95   NA 0.866615703 yellow
#> 41     1   NA 0.238453106   blue
#> 42    70   NA 0.004496308 orange
#> 43    34   NA 0.943516464   blue
#> 44    51   NA 0.438137200   blue
#> 45    46   NA 0.750603328   blue
#> 46    65   NA 0.667815764  green
#> 47    25   NA 0.407973201 yellow
#> 48    37   NA 0.351248815 yellow
#> 49     8   NA 0.738091561 orange
#> 50    33   NA 0.664285493  green
#> 51    96   NA 0.085224700 yellow
#> 52    83   NA 0.856132157 orange
#> 53    92   NA 0.076983322  green
#> 54    28   NA 0.852844803 yellow
#> 55    48   NA 0.106346961  green
#> 56    75   NA 0.484802824 yellow
#> 57    90   NA 0.247219110  green
#> 58    35   NA 0.686569211  green
#> 59     2   NA 0.163623198  green
#> 60    87   NA 0.952824800  green
#> 61     3   NA 0.321854551  green
#> 62    38   NA 0.361534117   blue
#> 63    68   NA 0.887723417 yellow
#> 64    65   NA 0.828014418 orange
#> 65     6   NA 0.100656458  green
#> 66    22   NA 0.906051578  green
#> 67    19   NA 0.772730364  green
#> 68     2   NA 0.383370670   blue
#> 69    64   NA 0.999652457   blue
#> 70    40   NA 0.349299049  green
#> 71    65   NA 0.947318266 yellow
#> 72    28   NA 0.216099976   blue
#> 73    97   NA 0.032092709 orange
#> 74    71   NA 0.145315843 orange
#> 75    12   NA 0.854383888  green
#> 76    54   NA 0.213149311  green
#> 77    41   NA 0.210310738 orange
#> 78    42   NA 0.039520690  green
#> 79    66   NA 0.944774803 orange
#> 80    26   NA 0.244927987 yellow
#> 81    60   NA 0.781122568  green
#> 82    14   NA 0.288237168 yellow
#> 83     5   NA 0.875357910 yellow
#> 84    76   NA 0.295750094 orange
#> 85    17   NA 0.983525408 orange
#> 86    13   NA 0.589837556 orange
#> 87    97   NA 0.759158380 yellow
#> 88    33   NA 0.836075306 orange
#> 89    77   NA 0.762819470 yellow
#> 90    82   NA 0.417269926  green
#> 91    32   NA 0.138074841   blue
#> 92    69   NA 0.080844962 orange
#> 93    55   NA 0.655982626  green
#> 94    11   NA 0.602003859 yellow
#> 95    33   NA 0.656995830   blue
#> 96    36   NA 0.329317164 orange
#> 97     7   NA 0.979474220 yellow
#> 98    23   NA 0.715186134 orange
#> 99    88   NA 0.872630305   blue
#> 100   34   NA 0.983283747   blue
 # the function drops `col5` and `col6` but keeps `col3`:
head(data)
#>   col1 col2      col3   col4 col5 col6
#> 1   68   NA 0.6632066 orange   NA   NA
#> 2   73   NA 0.8565750 yellow   NA   NA
#> 3   69   NA 0.9265464 yellow   NA   NA
#> 4    5   NA 0.5523776 yellow   NA   NA
#> 5   24   NA 0.5770657 yellow   NA   NA
#> 6   79   NA 0.6874477  green   NA   NA

```
