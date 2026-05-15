# Using `theme_gg_smc`

This function is meant to be used with `ggplot2` charts to quickly
format them according to the San Mateo County Office of Epidemiology and
Evaluation's style guide.

## Usage

``` r
theme_gg_smc(plot,
  plot_lines = "horizontal",
  legend_loc = "top",
  title_font_size = 18,
  subtitle_font_size = 12,
  caption_font_size = 8,
  axis_font_size = 10)
```

## Arguments

- plot:

  This should be a `ggplot2` object

- plot_lines:

  Specify which lines should appear on your chart. The default is
  horizontal lines, but you can also choose:

  - `"horizontal"` (the default): only horizontal lines

  - `"vertical"`: only vertical lines

  - `"both"`: horizontal and vertical lines

  - `"none"`: no lines on the plot

- legend_loc:

  Specify the legend location. The default is for the legend to appear
  at the top, but you can override this. The available options are:
  “left”,“top”, “right”, “bottom” and "none"

- title_font_size:

  A numeric value to specify the font size of the title. The default is
  18, the minimum is 6 and the maximum is 60.

- subtitle_font_size:

  A numeric value to specify the font size of the subtitle. The default
  is 12, the minimum is 6 and the maximum is 60.

- caption_font_size:

  A numeric value to specify the font size of the caption. The default
  is 8, the minimum is 6 and the maximum is 60.

- axis_font_size:

  A numeric value to specify the font size of the axis labels. The
  default is 10, the minimum is 6 and the maximum is 60.

## Value

a ggplot2 object with custom formatting

## Details

**\[experimental\]**

## Examples

``` r
# creating a jitter plot with `theme_gg_smc`
if (FALSE) { # \dontrun{
library(ggplot2)

iris %>%
  ggplot(aes(x = Species, y = Sepal.Length)) +
  geom_point(position = position_jitter(w = 1/6, h = 0),
             size = 4,
             color = "#006cb6",
             alpha = 2/3) +
  scale_y_continuous(limits = c(4, 8),
                     breaks = seq(4, 8, 2)) +
  labs(title = "Iris Jitter Plot") +
  theme_gg_smc()
} # }
```
