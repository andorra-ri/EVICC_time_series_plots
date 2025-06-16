#' Apply layout styling to a ggplot time series plot
#'
#' This function standardizes layout elements for ggplot objects including title, axis labels,
#' font sizes, colors, and grid line styling. It ensures consistent formatting for use in reports,
#' dashboards, and faceted visualizations.
#'
#' @param p A ggplot object.
#' @param title Plot title as a string.
#' @param xlab X-axis label as a string.
#' @param ylab Y-axis label as a string.
#' @param dict_fontsize Named list of font sizes and font family with keys: title, x_label, y_label, x_ticks, y_ticks, font.
#' @param dict_color Named list of colors, including at least `font_color` and `grid_lines_color`.
#' @param gridwidth Numeric; thickness of horizontal grid lines.
#'
#' @return A ggplot object with updated layout and styling.
#' @export
#'
#' @examples
#' dict_fontsize <- list(
#'   title = 14, x_label = 12, y_label = 12,
#'   x_ticks = 10, y_ticks = 10, font = "Arial"
#' )
#' dict_color <- list(
#'   font_color = "black",
#'   grid_lines_color = "grey80"
#' )
#' p <- ggplot(data, aes(x = time, y = value)) + geom_line()
#' p <- set_layout_ggplot(p, "Plot Title", "Time", "Value", dict_fontsize, dict_color, gridwidth = 0.5)


set_layout_ggplot <- function(p, title, xlab, ylab,
                              dict_fontsize = dict_fontsize,
                              dict_color = dict_color,
                              gridwidth = gridwidth) {
  # Validate dict_fontsize
  required_keys <- c("title", "x_label", "y_label", "x_ticks", "y_ticks", "font")
  missing_keys <- setdiff(required_keys, names(dict_fontsize))
  if (length(missing_keys) > 0) {
    stop(paste("Missing keys in dict_fontsize:", paste(missing_keys, collapse = ", ")))
  }

  # Apply labels and theme customization
  p <- p +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_family = dict_fontsize[["font"]]) +
    theme(
      plot.title = element_text(
        size = dict_fontsize[["title"]],
        color = dict_color[["font_color"]],
        hjust = 0.5,
        face = "bold"
      ),
      axis.title.x = element_text(
        size = dict_fontsize[["x_label"]],
        color = dict_color[["font_color"]],
        margin = margin(t = 10)
      ),
      axis.title.y = element_text(
        size = dict_fontsize[["y_label"]],
        color = dict_color[["font_color"]],
        margin = margin(r = 10)
      ),
      axis.text.x = element_text(
        size = dict_fontsize[["x_ticks"]],
        color = dict_color[["font_color"]]
      ),
      axis.text.y = element_text(
        size = dict_fontsize[["y_ticks"]],
        color = dict_color[["font_color"]]
      ),
      panel.grid.major.y = element_line(
        color = dict_color[["grid_lines_color"]],
        linewidth = gridwidth
      ),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(
        color = dict_color[["grid_lines_color"]],
        linewidth = gridwidth
      )
    )

  return(p)
}
