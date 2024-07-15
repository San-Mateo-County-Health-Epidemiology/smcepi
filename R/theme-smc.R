#' Using `theme_gg_smc`
#'
#' `r lifecycle::badge('experimental')`
#'
#' @description
#' This function is meant to be used with `ggplot2` charts to quickly format them according to the San Mateo County Office of Epidemiology and Evaluation's style guide.
#'
#' @usage theme_gg_smc(plot,
#'   plot_lines = "horizontal",
#'   legend_loc = "top",
#'   title_font_size = 18,
#'   subtitle_font_size = 12,
#'   caption_font_size = 8,
#'   axis_font_size = 10)
#'
#' @param plot This should be a `ggplot2` object
#' @param plot_lines Specify which lines should appear on your chart. The default is horizontal lines, but you can also choose:
#'   * `"horizontal"` (the default): only horizontal lines
#'   * `"vertical"`: only vertical lines
#'   * `"both"`: horizontal and vertical lines
#'   * `"none"`: no lines on the plot
#' @param legend_loc Specify the legend location. The default is for the legend to appear at the top, but you can override this. The available options are: “left”,“top”, “right”, “bottom” and "none"
#' @param title_font_size A numeric value to specify the font size of the title. The default is 18, the minimum is 6 and the maximum is 60.
#' @param subtitle_font_size A numeric value to specify the font size of the subtitle. The default is 12, the minimum is 6 and the maximum is 60.
#' @param caption_font_size A numeric value to specify the font size of the caption. The default is 8, the minimum is 6 and the maximum is 60.
#' @param axis_font_size A numeric value to specify the font size of the axis labels. The default is 10, the minimum is 6 and the maximum is 60.
#' @return a ggplot2 object with custom formatting
#'
#' @examples
#' # creating a jitter plot with `theme_gg_smc`
#' \dontrun{
#' library(ggplot2)
#'
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
#'}
#' @md
#'
#' @export
#' @importFrom ggplot2 theme
#' @importFrom ggtext element_markdown
theme_gg_smc <- function(plot, plot_lines = "horizontal", legend_loc = "top", title_font_size = 18, subtitle_font_size = 12, caption_font_size = 8, axis_font_size = 10) {

  stopifnot(is.numeric(title_font_size) & is.numeric(subtitle_font_size) & is.numeric(caption_font_size) & is.numeric(axis_font_size))

  # set colors -----------------------------------------
  title_color <- "#17202A" # black
  caption_color <- "#566573" # dark grey
  axis_color <- "#17202A" # black
  grid_color <- "#D5D8DC" # light grey

  # get the fonts we need ------------------------------
  load_smc_fonts()

  if(sum(grepl("^Arial$", names(grDevices::windowsFonts()))) == 0) {
    font <- "sans"
  } else {
    font <- "Arial"
  }

  if(sum(grepl("^Trade Gothic Next Rounded$", names(grDevices::windowsFonts()))) == 0) {
    title_font <- "sans"
  } else {
    title_font <- "Trade Gothic Next Rounded"
  }

  # font sizes -----------------------------------------
  title_font_size <- ifelse(title_font_size < 6, 6, ifelse(title_font_size > 60, 60, title_font_size))
  subtitle_font_size <- ifelse(subtitle_font_size < 6, 6, ifelse(subtitle_font_size > 60, 60, subtitle_font_size))
  caption_font_size <- ifelse(caption_font_size < 6, 6, ifelse(caption_font_size > 60, 60, caption_font_size))
  axis_font_size <- ifelse(axis_font_size < 6, 6, ifelse(axis_font_size > 60, 60, axis_font_size))

  # legend ---------------------------------------------
  legend_loc <- rlang::arg_match(legend_loc, c("top","left", "right", "bottom", "none"))

  # plot lines -----------------------------------------
  plot_lines <- rlang::arg_match(plot_lines, c("horizontal","vertical", "both", "none"))

  ## x lines ----
  switch(plot_lines,
         horizontal = {panel.grid.major.x <- ggplot2::element_blank()},
         vertical = {panel.grid.major.x <- ggplot2::element_line(color = grid_color)},
         both = {panel.grid.major.x <- ggplot2::element_line(color = grid_color)},
         none = {panel.grid.major.x <- ggplot2::element_blank()})

  ## y lines ----
  switch(plot_lines,
         horizontal = {panel.grid.major.y <- ggplot2::element_line(color = grid_color)},
         vertical = {panel.grid.major.y <-  ggplot2::element_blank()},
         both = {panel.grid.major.y <- ggplot2::element_line(color = grid_color)},
         none = {panel.grid.major.y <- ggplot2::element_blank()})

  ggplot2::theme(
    plot.title = ggtext::element_markdown(family = title_font, size = title_font_size, hjust = 0, face = "bold", color = title_color),
    plot.subtitle = ggtext::element_markdown(family = title_font, size = subtitle_font_size, hjust = 0, color = title_color),
    plot.caption = ggtext::element_markdown(family = font, size = caption_font_size, color = caption_color, hjust = 0),

    legend.position = legend_loc,
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font, size = axis_font_size, color = axis_color),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),

    axis.title = ggplot2::element_text(family = font, size = axis_font_size, color = axis_color),
    axis.text = ggplot2::element_text(family = font, size = axis_font_size, color = axis_color),
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


#'  Using `theme_pl_smc`
#'
#' @description
#' This function is meant to be used with `plotly` charts to quickly format them according to the San Mateo County Office of Epidemiology and Evaluation's style guide
#'
#' @usage theme_pl_smc(plot,
#'    plot_lines = "horizontal",
#'    legend_loc = "top",
#'    ystart = "tozero",
#'    title = NULL,
#'    x_lab = NULL,
#'    y_lab = NULL)
#'
#' @param plot A `plotly` plot object
#' @param plot_lines Specify which lines should appear on your chart. The default is horizontal lines, but you can also choose:
#'
#'   * `"horizontal"` (the default): only horizontal lines
#'   * `"vertical"`: only vertical lines
#'   * `"both"`: horizontal and vertical lines
#'   * `"none"`: no lines on the plot
#' @param legend_loc Specify the legend location. The default is for the legend to appear at the top, but you can override this. The available options are: “left”,“top”, “right”, “bottom” and "none"
#' @param ystart Specify whether or not the y-axis should start at zero. The default is to start at zero. The options are:
#'
#'   * `"tozero"` (the default) : axis starts at 0
#'   * `"none"`: axis starts wherever plotly decides
#' @param title a string of text for the plot title
#' @param x_lab a string of text for the x-axis label
#' @param y_lab a string of tect for the y-axis label
#' @return a plotly object with custom formatting
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(plotly)
#' library(dplyr)
#'
#' title <- "Sepal Lengths of Iris Species"
#' x <- "Species"
#' y <- "Sepal Length"
#'
#' iris_vals <- iris %>%
#'   distinct(Species) %>%
#'   mutate(number = as.numeric(Species))
#'
#' iris %>%
#'   plot_ly() %>%
#'   add_markers(x = ~jitter(as.numeric(Species)), y = ~Sepal.Length,
#'               color = ~Species,
#'               marker = list(size = 6),
#'               hoverinfo = "text",
#'               text = ~paste0("Group: ", Species,
#'                              "<br>xval: ", Sepal.Length),
#'               showlegend = FALSE) %>%
#'               layout(xaxis = list(tickvals = iris_vals$number,
#'                                   ticktext = iris_vals$Species)) %>%
#'   theme_pl_smc(plot_lines = "vertical",
#'                title = title,
#'                x_lab = x,
#'                y_lab = y)
#'}

theme_pl_smc <- function(plot, plot_lines = "horizontal", legend_loc = "top", ystart = "tozero", title = NULL, x_lab = NULL, y_lab = NULL) {

  title_color <- "#17202A" # black
  caption_color <- "#566573" # dark grey
  axis_color <- "#17202A" # black
  grid_color <- "#D5D8DC" # light grey
  grid_white <- "#FDFEFE" # white

  load_smc_fonts()

  if(sum(grepl("^Arial$", names(grDevices::windowsFonts()))) == 0) {

    font <- list(family = "sans",
                 size = 12)
  } else {

    font <- list(family = "Arial",
                 size = 12)
  }

  if(sum(grepl("^Trade Gothic Next Rounded$", names(grDevices::windowsFonts()))) == 0) {

    title_font <- list(family = "sans",
                       size = 16)
  } else {

    title_font <- list(family = "Trade Gothic Next Rounded",
                       size = 22)
  }

  # legend ---------------------------------------------
  legend_loc <- rlang::arg_match(legend_loc, c("top","left", "right", "bottom", "none"))

  legend <- if(legend_loc == "right") {

    legend_position <- list(orientation = "v",
                            x = 1.1, xanchor = "center",
                            y = 0.5, yanchor = "center")
    show_legend <- T

  } else if (legend_loc == "top") {

    legend_position <- list(orientation = "h",
                            x = 0.5, xanchor = "center",
                            y = 1.1, yanchor = "center")
    show_legend <- T

  } else if (legend_loc == "bottom") {

    legend_position <- list(orientation = "h",
                            x = 0.5, xanchor = "center",
                            y = -0.1, yanchor = "center")
    show_legend <- T

  } else if (legend_loc == "left") {

    legend_position <- list(orientation = "v",
                            x = -0.2, xanchor = "center",
                            y = 0.5, yanchor = "center")
    show_legend <- T

  } else if (legend_loc == "none") {

    legend_position <- list()
    show_legend <- F

  }

  # plot lines -----------------------------------------
  plot_lines <- rlang::arg_match(plot_lines, c("horizontal","vertical", "both", "none"))
  ystart <- rlang::arg_match(ystart, c("tozero", "none"))

  switch(ystart,
         tozero = {ystart <- "tozero"},
         none = {ystart <- ""})


  ## x lines ----
  switch(plot_lines,
         horizontal = {x_color <- grid_white},
         vertical = {x_color <- grid_color},
         both = {x_color <- grid_color},
         none = {x_color <- grid_white})

  ## y lines ----
  switch(plot_lines,
         horizontal = {y_color <- grid_color},
         vertical = {y_color <- grid_white},
         both = {y_color <- grid_color},
         none = {y_color <- grid_white})

  # plotly layout --------------------------------------

  plotly::layout(plot,
                 title = list(text = title, font = title_font),

                 font = list(family = "Arial", size = 14),

                 # legend ----
                 legend = legend_position,
                 showlegend = show_legend,

                 # axis labels + grid lines ----
                 xaxis = list(title = x_lab,
                              zerolinecolor = x_color,
                              zerolinewidth = 1,
                              gridcolor = x_color),
                 yaxis = list(title = y_lab,
                              zerolinecolor = y_color,
                              zerolinewidth = 1,
                              gridcolor = y_color,
                              rangemode = ystart),

                 margin = list(t = 50)

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
#' \dontrun{
#'
#' library(flextable)
#' library(dplyr)
#'
#' iris %>%
#'    slice(1:5) %>%
#'    select(Species, everything()) %>%
#'    flextable() %>%
#'    footnote(i = 1, j = 1, value = as_paragraph("footer test")) %>%
#'    theme_ft_smc()
#'}
#'
#' @md
#' @export
theme_ft_smc <- function(ft) {

  load_smc_fonts()

  if(sum(grepl("^Arial$", names(grDevices::windowsFonts()))) == 0) {
    font <- "sans"
  } else {
    font <- "Arial"
  }

  if(sum(grepl("^Trade Gothic Next Rounded$", names(grDevices::windowsFonts()))) == 0) {
    title_font <- "sans"
  } else {
    title_font <- "Trade Gothic Next Rounded"
  }

  ft1 <- ft %>%
    # header
    flextable::font(fontname = title_font, part = "header") %>%
    flextable::bold(bold = T, part = "header") %>%

    # body
    flextable::font(fontname = font, part = "body") %>%
    flextable::hline(part = "body", border = officer::fp_border(color = "#D5D8DC")) %>%

    # footer
    flextable::font(fontname = title_font, part = "footer") %>%
    flextable::fontsize(size = 10, part = "footer") %>%
    flextable::color(color = "#566573", part = "footer")
  ft1
}
