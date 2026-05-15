# Style Guide Templates

## Using formatting themes

### `theme_gg_smc()`

[`theme_gg_smc()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/theme_gg_smc.md)
will format your `ggplot2` charts according to OEE style guidelines.
Embedded within the
[`theme_gg_smc()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/theme_gg_smc.md)
is the
[`load_smc_fonts()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/load_smc_fonts.md)
function that will download and/or enable the fonts from the
[extrafont](https://cran.r-project.org/web/packages/extrafont/extrafont.pdf)
package. If you haven’t already installed the extrafont fonts on your
computer, the
[`theme_gg_smc()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/theme_gg_smc.md)
and
[`load_smc_fonts()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/load_smc_fonts.md)
functions will download them the first time you run either of those
functions.

``` r

library(smcepi)
library(ggplot2)

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

### `theme_ft_smc()`

[`theme_ft_smc()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/theme_ft_smc.md)
will format your `flextable` charts according to OEE style guidelines.
Like in the `smc_gg_theme()` function, the
[`load_smc_fonts()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/load_smc_fonts.md)
is embedded within the
[`theme_ft_smc()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/theme_ft_smc.md)
function.

``` r

library(smcepi)
library(flextable)

iris %>%
  dplyr::slice(1:5) %>%
  dplyr::select(Species, everything()) %>%
  flextable() %>%
  footnote(i = 1, j = 1, value = as_paragraph("footer test")) %>%
  theme_ft_smc()
```

## Selectively coloring text

### `gg_color_title()`

[`gg_color_title()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/gg_color_title.md)
allows you to specify the color for specific words or phrases in your
`ggplot2` title, subtitle or caption.

In order for the
[`gg_color_title()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/gg_color_title.md)
to render correctly in your chart, you should use the
[`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html)
in your
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
instead of using the standard
[`element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html).
The
[`theme_gg_smc()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/theme_gg_smc.md)
function uses
[`element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html).

``` r

library(ggtext)

brown_color <- "#873600"
blue_color <- "#006cb6"

title <- gg_color_title("Brown and blue eyes <br> are most common",
                       c("Brown", "blue"), c(brown_color, blue_color))
HairEyeColor %>%
  data.frame() %>%
  dplyr::group_by(Eye) %>%
  dplyr:: summarize(freq = sum(Freq),
                    .groups = "keep") %>%
  dplyr:: ungroup() %>%
  ggplot2::ggplot(aes(x = Eye,
                      y = freq,
                      fill = Eye)) +
  scale_fill_manual(values = c("Brown" = brown_color,
                               "Blue" = blue_color,
                               "Hazel" = "#D5D8DC",
                               "Green" = "#D5D8DC")) + 
  geom_bar(position = "dodge",
           stat = "identity") +
  labs(title = title) +
  theme_gg_smc(legend_loc = "none") +
  ggplot2::theme(plot.title = ggtext::element_markdown(size = 16, hjust = 0, face = "bold"))
```
