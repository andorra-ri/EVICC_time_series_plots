#' Create a Base Plotly Time Series Plot (Line or Bar)
#'
#' This function generates a Plotly time series chart based on a given data frame,
#' using either line or bar chart styles. It optionally supports conditional coloring
#' based on the sign of the values.
#'
#' @param df A data frame containing the input time series data.
#' @param time_var Character; name of the time variable column (must exist in `df`).
#' @param value_var Character; name of the numeric variable to be plotted.
#' @param plot_type Character; either `"line"` or `"bar"` to define plot style.
#' @param color_per_value_var Logical; if `TRUE`, use positive/negative colors. Defaults to `FALSE`.
#' @param dict_color Named list of color values. Must include:
#' \describe{
#'   \item{main_color}{Used if `color_per_value_var = FALSE`}
#'   \item{positive_value_color}{Color for positive values (if conditional coloring is used)}
#'   \item{negative_value_color}{Color for negative values (if conditional coloring is used)}
#' }
#'
#' @return A `plotly` object representing the time series plot.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly add_trace add_bars
#'
#' @examples
#' dict_colors <- list(
#'   main_color = "steelblue",
#'   positive_value_color = "green",
#'   negative_value_color = "red"
#' )
#' df <- data.frame(
#'   time = Sys.Date() + 1:10,
#'   value = rnorm(10)
#' )
#' get_base_plot(df, "time", "value", plot_type = "bar", color_per_value_var = TRUE, dict_color = dict_colors)

get_base_plot <- function(df,
                          time_var,
                          value_var,
                          plot_type = c("line", "bar"),
                          color_per_value_var = FALSE,
                          dict_color = dict_color) {
  tryCatch({

    plot_type <- match.arg(plot_type)

    # Ensure required columns exist
    if (!all(c(time_var, value_var) %in% names(df))) {
      stop("Both 'time_var' and 'value_var' must exist in the dataset.")
    }

    # Handle conditional coloring logic
    if (color_per_value_var) {

      # Validate color keys
      if (!all(c("positive_value_color", "negative_value_color") %in% names(dict_color))) {
        stop("Missing keys in 'dict_color'. Required: 'positive_value_color' and 'negative_value_color'.")
      }

      # Apply color logic per observation
      plot_color <- rep(NA_character_, nrow(df))
      plot_color[!is.na(df[[value_var]]) & df[[value_var]] >= 0] <- dict_color[["positive_value_color"]]
      plot_color[!is.na(df[[value_var]]) & df[[value_var]] <  0] <- dict_color[["negative_value_color"]]

      # Filter out NA rows (optional but helps with clarity and stability)
      valid_rows <- !is.na(df[[value_var]])
      df <- df[valid_rows, ]
      plot_color <- plot_color[valid_rows]

    } else {
      # Single static color
      plot_color <- dict_color[["main_color"]]
    }

    # Initialize base plot with time variable on x-axis
    p <- plot_ly(data = df, x = ~.data[[time_var]])

    # Add the appropriate trace type
    if (plot_type == "line") {
      p <- p %>%
        add_trace(
          y = df[[value_var]],
          name = "Value",
          type = "scatter", mode = "lines+markers",
          line = list(color = plot_color),
          marker = list(size = 4, color = plot_color),
          showlegend = FALSE
        )
    } else if (plot_type == "bar") {
      p <- p %>%
        add_bars(
          y = df[[value_var]],
          name = "Value",
          marker = list(color = plot_color),
          showlegend = FALSE
        )
    }

    return(p)

  }, error = function(e) {
    message("Error in get_base_plot(): ", e$message)
    return(NULL)
  })
}
