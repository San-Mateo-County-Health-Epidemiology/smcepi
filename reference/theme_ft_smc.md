# Using `theme_ft_smc`

This function will format a flextable object according to the San Mateo
County Office of Epidemiology and Evaluation's style guide.

## Usage

``` r
theme_ft_smc(ft)
```

## Arguments

- ft:

  This should be a `flextable` object

## Value

a `flextable` object with custom formatting

## Examples

``` r
if (FALSE) { # \dontrun{

library(flextable)
library(dplyr)

iris %>%
   slice(1:5) %>%
   select(Species, everything()) %>%
   flextable() %>%
   footnote(i = 1, j = 1, value = as_paragraph("footer test")) %>%
   theme_ft_smc()
} # }
```
