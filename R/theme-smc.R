#' Using `theme_gg_smc`
#'
#' @description
#' This function is meant to be used with `ggplot2` charts to quickly format them according to the San Mateo County Office of Epidemiology and Evaluation's style guide.
#'
#' @usage theme_gg_smc(plot,
#'   plot_lines = "horizontal",
#'   legend_loc = "top")
#'
#' @param plot This should be a `ggplot2` object
#' @param plot_lines Specify which lines should appear on your chart.
#'   The default is horizontal lines, but you can also choose:
#'
#'   * `"vertical"`: only vertical lines
#'   * `"both"`: horizontal and vertical lines
#'   * `"none"`: no lines on the plot
#' @param legend_loc Specify the legend location. The default is for the legend to appear at the top, but you can override this using the arguments from `ggplot2::theme(legend.position)` argument. The available options are: “left”,“top”, “right”, “bottom” and "none"
#' @return a ggplot2 object with custom formatting
#'
#' @examples
#' # creating a jitter plot with `theme_gg_smc`
#' iris %>%
#'   ggplot(aes(x = Species, y = Sepal.Length)) +
#'   geom_point(position = position_jitter(w = 1/6, h = 0),
#'              size = 4,
#'              color = "#006cb6",
#'              alpha = 2/3) +
#'   scale_y_continuous(limits = c(4, 8),
#'                      breaks = seq(4, 8, 2)) +
#'   labs(title = "Iris Jitter Plot") +
#'   theme_gg_smc()
#'
#' @md
#'
#' @export
#' @importFrom ggplot2 theme
theme_gg_smc <- function(plot, plot_lines = "horizontal", legend_loc = "top") {

  load_smc_fonts()

  if(sum(grepl("^Arial$", names(grDevices::windowsFonts()))) == 0) {
    font <- "serif"
  } else {
    font <- "Arial"
  }

  if(sum(grepl("^Georgia$", names(grDevices::windowsFonts()))) == 0) {
    title_font <- "sans"
  } else {
    title_font <- "Georgia"
  }

  title_color <- "#17202A" # black
  caption_color <- "#566573" # dark grey
  axis_color <- "#17202A" # black
  grid_color <- "#D5D8DC" # light grey

  if(missing(legend_loc)) {
    legend_loc <- "top"
  }

  if(missing(plot_lines)) {

    panel.grid.major.x <- ggplot2::element_blank()
    panel.grid.major.y <- ggplot2::element_line(color = grid_color)

  } else if(plot_lines == "horizontal") {

    panel.grid.major.x <- ggplot2::element_blank()
    panel.grid.major.y <- ggplot2::element_line(color = grid_color)

  } else if(plot_lines == "vertical") {

    panel.grid.major.x <- ggplot2::element_line(color = grid_color)
    panel.grid.major.y <- ggplot2::element_blank()

  } else if(plot_lines == "none") {

    panel.grid.major.x <- ggplot2::element_blank()
    panel.grid.major.y <- ggplot2::element_blank()

  } else if(plot_lines == "both") {

    panel.grid.major.x <- ggplot2::element_line(color = grid_color)
    panel.grid.major.y <- ggplot2::element_line(color = grid_color)

  }

  ggplot2::theme(
    plot.title = ggplot2::element_text(family = title_font, size = 16, hjust = 0, face = "bold", color = title_color),
    plot.subtitle = ggplot2::element_text(family = title_font, size = 12, hjust = 0, face = "bold", color = title_color),
    plot.caption = ggplot2::element_text(family = font, size = 8, color = caption_color, hjust = 0),

    legend.position = legend_loc,
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font, size = 10, color = axis_color),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),

    axis.title = ggplot2::element_text(family = font, size = 10, color = axis_color),
    axis.text = ggplot2::element_text(family = font, size = 10, color = axis_color),
    # top, right, bottom, left
    axis.text.x = ggplot2::element_text(family = font, margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0), color = axis_color),
    axis.text.y = ggplot2::element_text(family = font, margin = ggplot2::margin(0, 5, 0, 10), color = axis_color),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = panel.grid.major.x,
    panel.grid.major.y = panel.grid.major.y,
    panel.background = ggplot2::element_blank(),

    strip.text = ggplot2::element_text(family = title_font, size = 12, hjust = 0),
    strip.background = ggplot2::element_rect(fill = "#FFFFFF")
  )

}

#' Using `theme_ft_smc`
#'
#' @description
#' This function will format a flextable object according to the San Mateo County Office of Epidemiology and Evaluation's style guide.
#'
#' @usage theme_ft_smc(ft)
#'
#' @param ft This should be a `flextable` object
#' @return a `flextable` object with custom formatting
#'
#' @examples
#' iris %>%
#'    dplyr::slice(1:5) %>%
#'    dplyr::select(Species, everything()) %>%
#'    flextable() %>%
#'    footnote(i = 1, j = 1, value = as_paragraph("footer test")) %>%
#'    theme_ft_smc()
#'
#' @md
#' @import flextable
#' @export
theme_ft_smc <- function(ft) {

  load_smc_fonts()

  ft1 <- ft %>%
    # header
    flextable::font(fontname = "Georgia", part = "header") %>%
    flextable::bold(bold = T, part = "header") %>%

    # body
    flextable::font(fontname = "Arial", part = "body") %>%
    flextable::hline(part = "body", border = officer::fp_border(color = "#D5D8DC")) %>%

    # footer
    flextable::font(fontname = "Arial", part = "footer") %>%
    flextable::fontsize(size = 10, part = "footer") %>%
    flextable::color(color = "#566573", part = "footer")
  ft1
}
