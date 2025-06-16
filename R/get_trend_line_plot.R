#' Add a Linear Trend Line to a Plotly Time Series Plot
#'
#' Fits a simple linear regression model to a time series and overlays
#' the predicted trend line on an existing plotly plot.
#'
#' Automatically detects long-format data (e.g., from `pivot_longer`) and filters accordingly.
#'
#' @param p A plotly plot object to which the trend line will be added.
#' @param trend Logical; if TRUE, the trend line is added.
#' @param df A data frame containing the time and value variables.
#' @param time_var Character; name of the time variable column (must be coercible to numeric).
#' @param value_var Character; name of the numeric value variable (or variable name if long format).
#' @param color Character; color for the trend line. Defaults to \code{dict_color[["trend_line_color"]]}.
#'
#' @return A plotly object with the trend line added (if \code{trend = TRUE}).
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom stats lm predict
#' @importFrom plotly add_trace
#'
#' @examples
#' df <- data.frame(time = as.Date("2020-01-01") + 0:9, value = 1:10 + rnorm(10))
#' p <- plotly::plot_ly(df, x = ~time, y = ~value, type = "scatter", mode = "lines")
#' add_trend_line(p, TRUE, df, "time", "value")

add_trend_line <- function(p, trend, df, time_var, value_var,
                           color = dict_color[["trend_line_color"]]) {
  tryCatch({
    if (!trend) return(p)

    # Detect long-format data
    is_long_format <- "variable" %in% names(df) && value_var %in% unique(df$variable)
    y_column <- if (is_long_format) "value" else value_var

    # Subset to the relevant variable in long format
    if (is_long_format) {
      df <- df[df$variable == value_var, ]
    }

    # Drop NA rows
    model_data <- df[!is.na(df[[time_var]]) & !is.na(df[[y_column]]), ]

    if (nrow(model_data) < 2) {
      message("Insufficient data for trend line on variable: ", value_var)
      return(p)
    }

    # Coerce time to numeric for regression
    time_vals <- as.numeric(model_data[[time_var]])
    value_vals <- model_data[[y_column]]

    if (!is.numeric(value_vals)) {
      stop(paste("The variable", y_column, "must be numeric."))
    }

    # Fit and predict linear model
    model <- lm(value_vals ~ time_vals)
    pred <- predict(model, newdata = data.frame(time_vals = time_vals))

    # Add trend line to plot
    p <- p %>%
      add_trace(
        x = model_data[[time_var]],
        y = pred,
        name = "Trend",
        mode = "lines",
        line = list(color = color, dash = "dot"),
        showlegend = FALSE
      )

    return(p)
  }, error = function(e) {
    message("Error in add_trend_line(): ", e$message)
    return(p)
  })
}

