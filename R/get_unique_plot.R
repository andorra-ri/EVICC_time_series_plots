#' Generate a Modular Time Series Plot with Enhancements
#'
#' Constructs a plotly time series plot with optional enhancements including a moving average,
#' trend line, and vertical lines at year boundaries. This function is designed to be modular
#' and works with the ARIPlot pipeline system.
#'
#' @param df A data frame containing time series data.
#' @param time_var Character; name of the time variable (must be coercible to Date/POSIXt).
#' @param value_var Character; name of the numeric variable to be plotted.
#' @param plot_type Character; type of base plot (e.g., "line", "bar").
#' @param color_per_value_var Logical; whether to color based on value_var.
#' @param moving_avg Logical; whether to include a moving average line.
#' @param trend Logical; whether to add a trend line.
#' @param k_ma Integer; window size for the moving average (default is 6).
#' @param title Character; plot title.
#' @param xlab Character; label for the x-axis.
#' @param ylab Character; label for the y-axis.
#' @param vertical_lines_year Logical; whether to include vertical lines at year breaks.
#' @param yrange Numeric vector of length 2 specifying the y-axis range.
#' @param row Integer; layout row for subplot positioning (used in multi-panel contexts).
#'
#' @return A `plotly` object with applied styling and enhancements.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   time = seq.Date(Sys.Date(), by = "month", length.out = 24),
#'   value = cumsum(rnorm(24))
#' )
#' get_unique_plot(df, "time", "value", "line", TRUE, TRUE, TRUE, 6, "My Plot", "Date", "Value", TRUE, c(0, 100), 1)

get_unique_plot <- function(df,
                            time_var,
                            value_var,
                            plot_type,
                            color_per_value_var,
                            moving_avg = TRUE,
                            trend = TRUE,
                            k_ma = 6,
                            general_title,
                            title,
                            xlab,
                            ylab,
                            vertical_lines_year = TRUE,
                            yrange,
                            row) {

  tryCatch({

    # Step 1: Build base plot
    p <- get_base_plot(
      df,
      time_var,
      value_var,
      plot_type,
      color_per_value_var,
      dict_color = dict_color,
      ylabel = ylab,
      title = title
    )

    # Step 2: Create list of transformation functions (pipeline)
    pipeline <- list(
      function(p) add_moving_average_plot(
        p, moving_avg, df, time_var, value_var, k_ma,
        color = dict_color[["moving_avg_color"]], ylab = ylab
      ),
      function(p) add_trend_line(
        p, trend, df, time_var, value_var,
        color = dict_color[["trend_line_color"]], ylab = ylab
      ),
      function(p) add_vertical_year_lines(
        p, vertical_lines_year, df, time_var, value_var,
        color = dict_color[["grid_lines_color"]], yrange = yrange
      )
    )

    # Step 3: Configure layout using helper
    layout_plotly_obj <- set_layout(
      title = title,
      xlab = xlab,
      ylab = ylab,
      xrange = df[[time_var]],
      yrange = yrange,
      row = row
    )


    # Step 4: Apply layout (annotations and axes)
    annotations <- layout_plotly_obj[[1]]
    xaxis <- layout_plotly_obj[[2]]
    yaxis <- layout_plotly_obj[[3]]
    p <- plotly::layout(p, annotations = annotations, xaxis = xaxis, yaxis = yaxis)

    # Step 5: Apply all pipeline transformations
    final_plot <- apply_plot_pipeline(p, pipeline)


    #final_plot <- set_last_window_view(final_plot, df, time_var)


    return(final_plot)

  }, error = function(e) {
    message("Error in get_unique_plot(): ", e$message)
    return(NULL)
  })
}

