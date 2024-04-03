
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smcepi

<!-- badges: start -->
<!-- badges: end -->

The goal of smcepi is to provide functions that make data analysis
easier for San Mateo County epidemiologists.

## Installation

You can install the development version of smcepi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("San-Mateo-County-Health-Epidemiology/smcepi")
```

## Using `theme_gg_smc()`

`theme_gg_smc()` will format your `ggplot2` charts according to OEE
style guidelines. Embedded within the `theme_gg_smc()` is the
`load_smc_fonts()` function that will download and/or enable the fonts
from the
[extrafont](https://cran.r-project.org/web/packages/extrafont/extrafont.pdf)
package. If you haven’t already installed the extrafont fonts on your
computer, the `theme_gg_smc()` and `load_smc_fonts()` functions will
download them the first time you run either of those functions.

``` r
library(smcepi)
#> Loading required package: ggplot2

iris %>% 
  ggplot(aes(x = Species, 
             y = Sepal.Length)) +
  geom_point(
    position = position_jitter(w = 1/6, h = 0),
    size = 4, 
    color = "#006cb6", 
    alpha = 2/3
  ) +
  scale_y_continuous(
    limits = c(4, 8),
    breaks = seq(4, 8, 2)
  ) + 
  labs(title = "Iris Jitter Plot") + 
  theme_gg_smc(plot_lines = "vertical")
```

<img src="man/figures/README-example-1.png" width="60%" style="display: block; margin: auto;" />
