# Life Expectancy

Public Health England (PHE) has an excel file that will calculate life
expectancy using an abridged life table. The
[`make_life_table()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/make_life_table.md)
function in this package was written to exactly replicate the formulas
and methods used in that excel file. The PHE excel file can be found
[here](https://fingertips.phe.org.uk/documents/phe%20life%20expectancy%20calculator.xlsm).

The
[`make_life_table()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/make_life_table.md)
function is designed to work with grouped data so that you can calculate
life tables for multiple groups at once. It also allows for the use of
ACS age groups in addition to the age groups included in the PHE excel
calculator.

- PHE age groups: 0, 1-4, 5-9, 10-14 … 80-84, 85-89, 90+  
- ACS age groups: 0-4, 5-9, 10-14 … 80-84, 85+

## make_life_table()

### Basic usage

The output from the
[`make_life_table()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/make_life_table.md)
function has every column that is included in the PHE excel calculator.
This example includes the same death and population numbers that are in
the PHE excel calculator:

``` r

data1 <- data.frame(
  age_cat = c("0", "1-4", "5-9", "10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
  deaths = c(206, 37, 23, 23, 105, 162, 268, 314, 413, 584, 954,
            1359, 1912, 2824, 4507, 5851, 7117, 8192, 7745, 6442),
  population = c(50698, 215400, 280458, 258105, 282062, 329060, 306097, 274544, 260415,
           267450, 311314, 324311, 296825, 271339, 284608, 228062, 162785, 111263, 58987, 26016))

life_table1 <- make_life_table(data1)
str(life_table1)
#> 'data.frame':    20 obs. of  20 variables:
#>  $ age_cat            : chr  "0" "1-4" "5-9" "10-14" ...
#>  $ deaths             : num  206 37 23 23 105 162 268 314 413 584 ...
#>  $ population         : num  50698 215400 280458 258105 282062 ...
#>  $ start_age          : num  0 1 5 10 15 20 25 30 35 40 ...
#>  $ int_width          : num  1 4 5 5 5 5 5 5 5 5 ...
#>  $ fract_surv         : num  0.1 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
#>  $ death_rate         : num  4.06e-03 1.72e-04 8.20e-05 8.91e-05 3.72e-04 ...
#>  $ prob_dying         : num  0.004048 0.000687 0.00041 0.000445 0.00186 ...
#>  $ prob_surv          : num  0.996 0.999 1 1 0.998 ...
#>  $ num_alive_int      : num  100000 99595 99527 99486 99442 ...
#>  $ num_dying_int      : num  404.8 68.4 40.8 44.3 184.9 ...
#>  $ pers_yrs_lived_int : num  99636 398244 497532 497319 496746 ...
#>  $ pers_yrs_lived_past: num  7832451 7732815 7334572 6837040 6339721 ...
#>  $ obs_le_int         : num  78.3 77.6 73.7 68.7 63.8 ...
#>  $ sample_var         : num  7.92e-08 1.27e-08 7.30e-09 8.62e-09 3.29e-08 ...
#>  $ weighted_var       : num  4888352 724166 367032 374649 1224149 ...
#>  $ sample_var_pers_yrs: num  39355415 34467062 33742896 33375864 33001214 ...
#>  $ sample_var_obs_le  : num  0.00394 0.00347 0.00341 0.00337 0.00334 ...
#>  $ ci_low_95          : num  78.2 77.5 73.6 68.6 63.6 58.8 53.9 49.1 44.4 39.7 ...
#>  $ ci_high_95         : num  78.4 77.8 73.8 68.8 63.9 59 54.1 49.3 44.6 39.9 ...
```

### Additional options

#### Multiple groups

One benefit to using the
[`make_life_table()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/make_life_table.md)
function instead of the excel file is that you can create full life
tables for multiple groups at once. You just specify the grouping
variables with the `grouping_vars` argument.

Here we have a version of `data1` but with an additional set of data
that were simulated based on the data entered into the PHE excel file.

``` r

data2 <- data.frame(
  group = c(rep("phe", 20), rep("simulated", 20)),
  age_cat = rep(c("0", "1-4", "5-9", "10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44",
                 "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 2),
  deaths = c(206, 37, 23, 23, 105, 162, 268, 314, 413, 584, 954,
               1359, 1912, 2824, 4507, 5851, 7117, 8192, 7745, 6442,
               232, 30, 41, 22, 194, 168, 315, 313, 406, 643, 963,
               1446, 1979, 2814, 4587, 5874, 7111, 8221, 7825, 6540),
  population = c(50698, 215400, 280458, 258105, 282062, 329060, 306097, 274544, 260415,
                       267450, 311314, 324311, 296825, 271339, 284608, 228062, 162785, 111263, 58987, 26016,
                       51578, 215512, 279462, 257256, 282348, 329111, 306514, 274397, 259847, 267045,
                       311791, 323739, 297453, 271344, 285047, 227655, 162922, 110554, 58886, 26243))
```

Then you can create life tables for each group by specifying that
`grouping_vars = "group"`. The `grouping_vars` argument can take a list
of variables if you want to group by year and geography (for example).

``` r

life_table2 <- make_life_table(data2,
                               grouping_vars = "group")
str(life_table2)
#> 'data.frame':    40 obs. of  21 variables:
#>  $ group              : chr  "phe" "phe" "phe" "phe" ...
#>  $ age_cat            : chr  "0" "1-4" "5-9" "10-14" ...
#>  $ deaths             : num  206 37 23 23 105 162 268 314 413 584 ...
#>  $ population         : num  50698 215400 280458 258105 282062 ...
#>  $ start_age          : num  0 1 5 10 15 20 25 30 35 40 ...
#>  $ int_width          : num  1 4 5 5 5 5 5 5 5 5 ...
#>  $ fract_surv         : num  0.1 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
#>  $ death_rate         : num  4.06e-03 1.72e-04 8.20e-05 8.91e-05 3.72e-04 ...
#>  $ prob_dying         : num  0.004048 0.000687 0.00041 0.000445 0.00186 ...
#>  $ prob_surv          : num  0.996 0.999 1 1 0.998 ...
#>  $ num_alive_int      : num  100000 99595 99527 99486 99442 ...
#>  $ num_dying_int      : num  404.8 68.4 40.8 44.3 184.9 ...
#>  $ pers_yrs_lived_int : num  99636 398244 497532 497319 496746 ...
#>  $ pers_yrs_lived_past: num  7832451 7732815 7334572 6837040 6339721 ...
#>  $ obs_le_int         : num  78.3 77.6 73.7 68.7 63.8 ...
#>  $ sample_var         : num  7.92e-08 1.27e-08 7.30e-09 8.62e-09 3.29e-08 ...
#>  $ weighted_var       : num  4888352 724166 367032 374649 1224149 ...
#>  $ sample_var_pers_yrs: num  39355415 34467062 33742896 33375864 33001214 ...
#>  $ sample_var_obs_le  : num  0.00394 0.00347 0.00341 0.00337 0.00334 ...
#>  $ ci_low_95          : num  78.2 77.5 73.6 68.6 63.6 58.8 53.9 49.1 44.4 39.7 ...
#>  $ ci_high_95         : num  78.4 77.8 73.8 68.8 63.9 59 54.1 49.3 44.6 39.9 ...
```

#### Renaming columns

The function expects a `data.frame` with a minimum of three columns:

- age categories (`age_cat`): this should be a character variable with
  the start age and end age separated by a `-`. Ex:
  `c("0", "1-4", "5-9", "10-14", ..."90+")`.
- death counts (`deaths`): this should be a numeric variable with the
  number of deaths that occurred in each age group for each interval
- population years (`population`): this should be a numeric variable
  with the number of population years in each group for each interval

If your variables aren’t named `"age_cat"`, `"deaths"` and
`"population"`, you can pass your variable names to the function using
the `age_cat_var`, `deaths_var` and `population_var` arguments. For
example:

``` r

data2 <- data.frame(
  ages = c("0", "1-4", "5-9", "10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
  death_count = c(206, 37, 23, 23, 105, 162, 268, 314, 413, 584, 954,
            1359, 1912, 2824, 4507, 5851, 7117, 8192, 7745, 6442),
  population_years = c(50698, 215400, 280458, 258105, 282062, 329060, 306097, 274544, 260415,
           267450, 311314, 324311, 296825, 271339, 284608, 228062, 162785, 111263, 58987, 26016))

life_table1 <- make_life_table(data2,
                               age_cat_var = "ages",
                               deaths_var = "death_count",
                               population_var = "population_years")
str(life_table1)
#> 'data.frame':    20 obs. of  20 variables:
#>  $ age_cat            : chr  "0" "1-4" "5-9" "10-14" ...
#>  $ deaths             : num  206 37 23 23 105 162 268 314 413 584 ...
#>  $ population         : num  50698 215400 280458 258105 282062 ...
#>  $ start_age          : num  0 1 5 10 15 20 25 30 35 40 ...
#>  $ int_width          : num  1 4 5 5 5 5 5 5 5 5 ...
#>  $ fract_surv         : num  0.1 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
#>  $ death_rate         : num  4.06e-03 1.72e-04 8.20e-05 8.91e-05 3.72e-04 ...
#>  $ prob_dying         : num  0.004048 0.000687 0.00041 0.000445 0.00186 ...
#>  $ prob_surv          : num  0.996 0.999 1 1 0.998 ...
#>  $ num_alive_int      : num  100000 99595 99527 99486 99442 ...
#>  $ num_dying_int      : num  404.8 68.4 40.8 44.3 184.9 ...
#>  $ pers_yrs_lived_int : num  99636 398244 497532 497319 496746 ...
#>  $ pers_yrs_lived_past: num  7832451 7732815 7334572 6837040 6339721 ...
#>  $ obs_le_int         : num  78.3 77.6 73.7 68.7 63.8 ...
#>  $ sample_var         : num  7.92e-08 1.27e-08 7.30e-09 8.62e-09 3.29e-08 ...
#>  $ weighted_var       : num  4888352 724166 367032 374649 1224149 ...
#>  $ sample_var_pers_yrs: num  39355415 34467062 33742896 33375864 33001214 ...
#>  $ sample_var_obs_le  : num  0.00394 0.00347 0.00341 0.00337 0.00334 ...
#>  $ ci_low_95          : num  78.2 77.5 73.6 68.6 63.6 58.8 53.9 49.1 44.4 39.7 ...
#>  $ ci_high_95         : num  78.4 77.8 73.8 68.8 63.9 59 54.1 49.3 44.6 39.9 ...
```

## get_le()

### Basic usage

Typically you just want the estimated life expectancy at age 0 and you
don’t need the entire the life table. You can use the
[`get_le()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/get_le.md)
function on the output of the
[`make_life_table()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/make_life_table.md)
function for this. Note the
[`get_le()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/get_le.md)
function was written explicitly to work with the output of the
[`make_life_table()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/make_life_table.md)
function.

``` r

get_le(life_table1)
#>   obs_le_int ci_low_95 ci_high_95
#> 1   78.32451      78.2       78.4
```

### Additional options

#### Multiple groups

[`get_le()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/get_le.md)
also works for grouped data. Like with the
[`make_life_table()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/make_life_table.md)
function, you should use the `grouping_vars` argument to specify the
groups:

``` r

get_le(life_table2, grouping_vars = "group")
#>        group obs_le_int ci_low_95 ci_high_95
#> 1        phe   78.32451      78.2       78.4
#> 21 simulated   77.99384      77.9       78.1
```

#### Excluding confidence intervals

95% confidence intervals are included in the output of
[`get_le()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/get_le.md)
by default. You can elect to just get the estimate by setting
`include_ci` to `FALSE`.

``` r

get_le(life_table1, include_ci = F)
#> [1] 78.32451
```

#### Ages other than 0

If you’re interested in the life expectancy at an age group other than
0, you can specify the age with the `start_age` argument. The start age
you specify should be numeric and must be the start of an age group in
the data passed through
[`make_life_table()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/make_life_table.md).

``` r

get_le(life_table1, start_age = 30)
#>   obs_le_int ci_low_95 ci_high_95
#> 8    49.2322      49.1       49.3
```
