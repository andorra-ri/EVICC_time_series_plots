#' Add a Custom "Restore 10-Year View" Button to Plotly Modebar
#'
#' This function adds a custom button to the Plotly modebar that resets the x-axis
#' to display the last 10 years of data. It dynamically detects the maximum date
#' in the given data frame and sets the view range accordingly.
#'
#' @param p A Plotly object (e.g., output from plot_ly()).
#' @param df A data.frame containing the data used in the plot.
#' @param time_var A string representing the column name in `df` with time data (must be POSIXct or Date).
#'
#' @return A Plotly object with an embedded JavaScript modebar button to reset the x-axis range.
#' @export
#' @examples
#' library(plotly)
#' library(lubridate)
#' df <- data.frame(date = seq(ymd("2000-01-01"), ymd("2040-01-01"), by = "months"),
#'                  value = rnorm(481))
#' p <- plot_ly(df, x = ~date, y = ~value, type = "scatter", mode = "lines")
#' p <- set_last_window_view(p, df, "date")
#' p
#'
set_last_window_view <- function(p, df, time_var) {
  stopifnot(is.data.frame(df))
  stopifnot(time_var %in% names(df))
  stopifnot(inherits(df[[time_var]], c("Date", "POSIXct", "POSIXt")))

  max_time <- max(df[[time_var]], na.rm = TRUE)
  min_view <- max_time - lubridate::years(10)

  p <- htmlwidgets::onRender(
    p,
    sprintf(
      "
      function(el, x) {
        var Plotly = window.Plotly;

        var restoreButton = {
          name: 'Restore10Year',
          icon: Plotly.Icons.autoscale,  // Use any icon you prefer
          title: 'Reset axes',
          click: function(gd) {
            Plotly.relayout(gd, {
              'xaxis.range': [%s, %s]
            });
          }
        };

        Plotly.newPlot(el.id, x.data, x.layout, {
          modeBarButtonsToAdd: [restoreButton],
          modeBarButtonsToRemove: ['resetScale2d', 'autoscale'],  // Remove both unwanted icons
          displaylogo: false
        });
      }
      ",
      as.numeric(as.POSIXct(min_view)) * 1000,
      as.numeric(as.POSIXct(max_time)) * 1000
    )
  )

  return(p)
}

