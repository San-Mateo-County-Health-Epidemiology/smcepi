#' Using `theme_smc`
#'
#' @param plot this should be a ggplot2 object
#' @param plot_lines specify if you'd like horizontal, vertical, both (horizontal and vertical) or no (none) lines on the plot. the default is horizontal
#' @param legend_loc specify if you'd like the legend to appear at the top, bottom or to not appear at all (none)
#'
#' @return
#' @export
#'
#' @examples
theme_smc <- function(plot, plot_lines = "horizontal", legend_loc = "top") {


  # plot_lines = c("horizontal", "vertical", "none", "both")
  #legend_loc = c("top", "bottom", "none")

  title_font <- "Georgia"
  font <- "Arial"

  title_color <- "#17202A" # black
  caption_color <- "#566573" # dark grey
  axis_color <- "#17202A" # black
  grid_color <- "#EAECEE" # grid color

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

    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font, size = 10, color = axis_color),
    legend.position = legend_loc,
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

    strip.text = ggplot2::element_text(family = font, size = 22, hjust = 0),
    strip.background = ggplot2::element_rect(fill = "#FFFFFF")
  )

}
