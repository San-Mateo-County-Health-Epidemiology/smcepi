# Creating life tables

This function creates a life table based on the life table from Public
Health England that lives
[here](https://fingertips.phe.org.uk/documents/phe%20life%20expectancy%20calculator.xlsm).
It is designed to work with grouped data so that you can calculate life
tables for multiple groups at once.

## Usage

``` r
make_life_table(data,
                grouping_vars = NULL,
                age_cat_var = "age_cat",
                deaths_var = "deaths",
                population_var = "population")
```

## Arguments

- data:

  a 2x2 data frame with variables for age categories, population years
  and death count.

- grouping_vars:

  a list of variables used to group the output.

- age_cat_var:

  the name of the variable with age categories if the variable has a
  name other than "age_cat". Age categories should be in the format
  `start_age-end_age`, ex: `0-4`, `5-9`, etc

- deaths_var:

  the name of the variable with death counts if the variable has a name
  other than "deaths"

- population_var:

  the name of the variable with the population years if the variable has
  a name other than "population"

## Value

a data frame with a variable for each of the columns in the PHE life
table

## Examples

``` r
# step 1: generate data from which to calculate life expectancy
test_data <- data.frame(
  group = c(rep("phe", 20), rep("simulated", 20)),
  ages = rep(c("0", "1-4", "5-9", "10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 2),
  deaths = c(206, 37, 23, 23, 105, 162, 268, 314, 413, 584, 954,
            1359, 1912, 2824, 4507, 5851, 7117, 8192, 7745, 6442,
            232, 30, 41, 22, 194, 168, 315, 313, 406, 643, 963,
            1446, 1979, 2814, 4587, 5874, 7111, 8221, 7825, 6540),
  population_est = c(50698, 215400, 280458, 258105, 282062, 329060, 306097, 274544, 260415,
           267450, 311314, 324311, 296825, 271339, 284608, 228062, 162785, 111263, 58987, 26016,
           51578, 215512, 279462, 257256, 282348, 329111, 306514, 274397, 259847, 267045,
           311791, 323739, 297453, 271344, 285047, 227655, 162922, 110554, 58886, 26243))

# step 2: create a life table using the methods from Public Health England .xlsm file
le_table <- make_life_table(data = test_data,
                            grouping_vars = c("group"),
                            age_cat_var = "ages",
                            population_var = "population_est")

# step 3: pull out the life expectancy for each group
## 3a: get le for age 0 (typical estimation used) with confidence intervals
get_le(le_table, grouping_vars = c("group"))
#>        group obs_le_int ci_low_95 ci_high_95
#> 1        phe   78.32451      78.2       78.4
#> 21 simulated   77.99384      77.9       78.1

## 3b. get le for the age group starting at 30 without confidence intervals
get_le(le_table, start_age = 30, grouping_vars = c("group"), include_ci = FALSE)
#>        group obs_le_int
#> 8        phe   49.23220
#> 28 simulated   49.08431
```
