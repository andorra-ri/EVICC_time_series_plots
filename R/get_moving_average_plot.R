#' Compute Moving Average for a Time Series Variable
#'
#' This function computes a right-aligned rolling mean (moving average) on a specified numeric column.
#' The result is a new column `moving_avg` added to the input data frame.
#'
#' @param df A data frame containing the time and value columns.
#' @param time_var Character; name of the time variable column.
#' @param value_var Character; name of the numeric value column to smooth.
#' @param k_ma Integer; window size for the moving average.
#'
#' @return A data frame with a new column `moving_avg` containing the smoothed values.
#' @export
#'
#' @importFrom dplyr arrange mutate
#' @importFrom zoo rollmean
#' @importFrom rlang sym
#'
#' @examples
#' df <- data.frame(time = Sys.Date() + 1:10, value = 1:10)
#' compute_moving_average(df, "time", "value", 3)

compute_moving_average <- function(df, time_var, value_var, k_ma) {
  tryCatch({

    # Ensure required columns exist
    if (!all(c(time_var, value_var) %in% names(df))) {
      stop("Missing required columns: ", paste(setdiff(c(time_var, value_var), names(df)), collapse = ", "))
    }

    if (!is.numeric(df[[value_var]])) {
      stop("The value column must be numeric.")
    }

    # Compute rolling mean
    df <- df %>%
      dplyr::arrange(!!rlang::sym(time_var)) %>%
      dplyr::mutate(moving_avg = zoo::rollmean(.data[[value_var]], k = k_ma, fill = NA, align = "right"))

    return(df)

  }, error = function(e) {
    message("Error in compute_moving_average(): ", e$message)
    return(df)
  })
}

#' Add a Moving Average Line to a Plotly Time Series Plot
#'
#' Computes and overlays a moving average line on an existing Plotly time series plot.
#' This helps visualize longer-term trends by smoothing short-term fluctuations.
#'
#' @param p A plotly object to which the moving average line will be added.
#' @param moving_avg Logical; whether to add the moving average line.
#' @param df A data frame containing the input time series data.
#' @param time_var Character; name of the time variable column.
#' @param value_var Character; name of the numeric value column to smooth.
#' @param k_ma Integer; number of periods for the moving average.
#' @param color Character; color for the moving average line.
#'
#' @return A plotly object with the moving average line added (if enabled).
#' @export
#'
#' @importFrom plotly add_trace
#' @importFrom dplyr %>%
#'
#' @examples
#' df <- data.frame(time = Sys.Date() + 1:10, value = rnorm(10))
#' p <- plotly::plot_ly(df, x = ~time, y = ~value, type = "scatter", mode = "lines")
#' add_moving_average_plot(p, TRUE, df, "time", "value", 3)

add_moving_average_plot <- function(p, moving_avg, df, time_var, value_var, k_ma,
                                    color = dict_color[["moving_avg_color"]], ylab) {
  tryCatch({

    if (!moving_avg) return(p)

    # Validate columns
    if (!all(c(time_var, value_var) %in% names(df))) {
      stop("Missing required columns: ", paste(setdiff(c(time_var, value_var), names(df)), collapse = ", "))
    }

    if (!is.numeric(df[[value_var]])) {
      stop(paste("The variable", value_var, "must be numeric for moving average."))
    }

    # Compute smoothed values
    df <- compute_moving_average(df, time_var, value_var, k_ma)

    # Add the moving average trace
    p <- p %>%
      add_trace(
        x = df[[time_var]],
        y = df$moving_avg,
        name = "Moving average",
        type = "scatter",
        mode = "lines",
        line = list(color = color, dash = "solid", width = 0.8),
        hovertemplate = paste0(
          "%{x}<br>",
          "Moving average: %{y:.2f}", ylab,
          "<extra></extra>"
        ),
        showlegend = FALSE
      )

    return(p)

  }, error = function(e) {
    message("Error in add_moving_average_plot(): ", e$message)
    return(p)
  })
}
