#' Add Vertical Year Markers to a Time Series Plot
#'
#' This function adds vertical lines to a Plotly time series chart at the start of each year.
#' It helps visually distinguish yearly boundaries on the x-axis.
#'
#' @param p A plotly object representing the base plot.
#' @param vertical_lines_year Logical; if TRUE, year markers are added. If FALSE, returns the plot unchanged.
#' @param df Data frame containing at least time and value columns.
#' @param time_var Character; name of the column with time data (must be of class Date or POSIXt).
#' @param value_var Character; name of the numeric column for value axis.
#' @param color Color to use for vertical lines. Defaults to `dict_color[["grid_lines_color"]]`.
#' @param yrange Optional numeric vector of length 2 for the y-axis range. Used to define line height.
#'
#' @return A modified `plotly` object with vertical year lines added (if enabled).
#' @export
#'
#' @importFrom plotly add_segments
#' @importFrom lubridate year
#' @importFrom magrittr %>%

add_vertical_year_lines <- function(p,
                                    vertical_lines_year,
                                    df,
                                    time_var,
                                    value_var,
                                    color = dict_color[["grid_lines_color"]],
                                    yrange) {
  tryCatch({

    if (!vertical_lines_year) return(p)  # Skip if not requested

    # Validate input columns
    if (!all(c(time_var, value_var) %in% names(df))) {
      stop("Missing required columns in data frame.")
    }

    # Extract time and value data
    time_values <- df[[time_var]]
    value_values <- df[[value_var]]

    # Ensure time column is Date or POSIXt
    if (!inherits(time_values, c("Date", "POSIXt"))) {
      time_values <- as.Date(time_values)
    }

    # Generate one timestamp per year (January 1st of each year)
    start_year <- lubridate::year(min(time_values, na.rm = TRUE))
    end_year   <- lubridate::year(max(time_values, na.rm = TRUE))
    x_values   <- as.Date(paste0(seq(start_year, end_year), "-01-01"))

    # Determine y-range for vertical lines
    if (is.numeric(yrange) && length(yrange) >= 2 && !all(is.na(yrange))) {
      y_min <- min(yrange)
      y_max <- max(yrange)
    } else {
      y_min <- if (min(value_values, na.rm = TRUE) < 0) min(value_values, na.rm = TRUE) else 0
      y_max <- max(value_values, na.rm = TRUE)
    }

    # Add vertical lines in a single plotly trace
    for (x in x_values){
      p <- p %>%
        add_segments(
          x = x_values,
          xend = x_values,
          y = rep(y_min, length(x_values)),
          yend = rep(y_max, length(x_values)),
          line = list(
            color = color,
            dash = "lines",
            width = gridwidth + 0.1
          ),
          showlegend = FALSE
        )
    }

    return(p)

  }, error = function(e) {
    message("Error in add_vertical_year_lines(): ", e$message)
    return(p)
  })
}
