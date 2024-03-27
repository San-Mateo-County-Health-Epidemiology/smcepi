theme_smc <- function(plot, plot_lines = c("horizontal", "vertical", "none", "both"), legend_loc = c("top", "bottom", "none")) {

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

    panel.grid.major.x <- element_blank()
    panel.grid.major.y <- element_line(color = grid_color)

  } else if(plot_lines == "horizontal") {

    panel.grid.major.x <- element_blank()
    panel.grid.major.y <- element_line(color = grid_color)

  } else if(plot_lines == "vertical") {

    panel.grid.major.x <- element_line(color = grid_color)
    panel.grid.major.y <- element_blank()

  } else if(plot_lines == "none") {

    panel.grid.major.x <- element_blank()
    panel.grid.major.y <- element_blank()

  } else if(plot_lines == "both") {

    panel.grid.major.x <- element_line(color = grid_color)
    panel.grid.major.y <- element_line(color = grid_color)

  }

  ggplot2::theme(
    plot.title = element_text(family = title_font, size = 16, hjust = 0, face = "bold", color = title_color),
    plot.subtitle = element_text(family = title_font, size = 12, hjust = 0, face = "bold", color = title_color),
    plot.caption = element_text(family = font, size = 8, color = caption_color, hjust = 0),

    legend.title = element_blank(),
    legend.text = element_text(family = font, size = 10, color = axis_color),
    legend.position = legend_loc,
    legend.background = element_blank(),
    legend.key = element_blank(),

    axis.title = element_text(family = font, size = 10, color = axis_color),
    axis.text = element_text(family = font, size = 10, color = axis_color),
    # top, right, bottom, left
    axis.text.x = element_text(family = font, margin = margin(t = 5, r = 0, b = 0, l = 0), color = axis_color),
    axis.text.y = element_text(family = font, margin = margin(0, 5, 0, 10), color = axis_color),
    axis.ticks = element_blank(),
    axis.line = element_blank(),

    panel.grid.minor = element_blank(),
    panel.grid.major.x = panel.grid.major.x,
    panel.grid.major.y = panel.grid.major.y,
    panel.background = element_blank(),

    strip.text = element_text(family = font, size = 22, hjust = 0),
    strip.background = element_rect(fill = "#FFFFFF")
  )

}
