#' Apply layout styling to a Plotly time series plot
#'
#' This function generates layout metadata for a Plotly plot, including title annotations, axis formatting,
#' tick formatting based on inferred time granularity, and optional y-axis range control.
#'
#' It returns layout components (annotations, xaxis, yaxis) to be applied later via `plotly::layout()`.
#'
#' @param title A character string for the plot title.
#' @param xlab A character string for the x-axis label.
#' @param ylab A character string for the y-axis label.
#' @param xrange A Date or POSIXct vector used to determine time granularity and x-axis ticks.
#' @param yrange A numeric vector of length 2 specifying y-axis limits.
#' @param row Integer; subplot row index (optional, used for annotation adjustment).
#'
#' @return A list containing: `annotations`, `xaxis`, and `yaxis` layout components.
#' @export
#'
#' @examples
#' # Used internally in plotting functions:
#' layout_components <- set_layout(
#'   title = "My Plot", xlab = "Time", ylab = "Value",
#'   xrange = as.Date(c("2020-01-01", "2023-01-01")),
#'   yrange = c(0, 100), row = 1
#' )
set_layout <- function(title, xlab, ylab, xrange, yrange, row) {

  tryCatch({

    # --- Validate required font keys ---
    required_keys <- c("title", "x_label", "y_label", "x_ticks", "y_ticks", "font")
    missing_keys <- setdiff(required_keys, names(dict_fontsize))
    if (length(missing_keys) > 0) {
      stop(paste("Missing keys in dict_fontsize:", paste(missing_keys, collapse = ", ")))
    }

    # --- Create title annotation object ---
    annotations <- list(
      list(
        text = title,
        x = 0.5,
        y = 1.02,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(
          size = dict_fontsize[["title"]],
          family = dict_fontsize[["font"]],
          color = dict_color[["font_color"]]
        ),
        xanchor = "center",
        yanchor = "bottom"
      )
    )

    # --- Determine time granularity ---
    time_granularity <- detect_time_granularity(xrange)

    tickformatstops <- NULL
    tickvals <- NULL
    tickformat <- NULL

    if (time_granularity == "yearly") {
      # Format: %Y
      tickformatstops <- list(
        list(dtickrange = list(NULL, NULL), value = "%Y")
      )

      if (!is.null(min(xrange)) && !is.null(max(xrange))) {
        years <- seq(from = as.numeric(format(min(xrange), "%Y")),
                     to = as.numeric(format(max(xrange), "%Y")),
                     by = 1)
        tickvals <- as.character(as.Date(paste0(years, "-01-01")))
        tickformat <- "%Y"
      }

    } else if (time_granularity == "monthly") {
      tickformatstops <- list(
        list(dtickrange = list(NULL, "M1"), value = "%m-%Y"),
        list(dtickrange = list("M1", NULL), value = "%Y")
      )

    } else {
      tickformatstops <- list(
        list(dtickrange = list(NULL, 24 * 60 * 60 * 1000), value = "%H:%M\n%d %m"),
        list(dtickrange = list(24 * 60 * 60 * 1000, 7 * 24 * 60 * 60 * 1000), value = "%d %b"),
        list(dtickrange = list(7 * 24 * 60 * 60 * 1000, "M1"), value = "%b-%Y"),
        list(dtickrange = list("M1", NULL), value = "%b %Y")
      )
    }

    # --- X-axis configuration ---
    xaxis <- list(
      showgrid = FALSE,
      title = list(
        text = xlab,
        font = list(
          size = dict_fontsize[["x_label"]],
          family = dict_fontsize[["font"]],
          color = dict_color[["font_color"]]
        )
      ),
      type = "date",
      ticklabelmode = "instant",
      tickfont = list(
        size = dict_fontsize[["x_ticks"]],
        family = dict_fontsize[["font"]],
        color = dict_color[["font_color"]]
      ),
      ticklen = 0,
      tickpad = 0,
      automargin = TRUE,
      constrain = "range",
      constraintoward = "center"
    )

    if (!is.null(tickformat)) xaxis$tickformat <- tickformat
    if (!is.null(tickformatstops)) xaxis$tickformatstops <- tickformatstops
    if (!is.null(tickvals)) xaxis$tickvals <- tickvals

    # --- Y-axis configuration ---
    yaxis <- list(
      showgrid = TRUE,
      gridcolor = dict_color[['grid_lines_color']],
      gridwidth = gridwidth,
      zeroline = FALSE,
      zerolinewidth = 0,
      zerolinecolor = "transparent",
      layer = "below traces",
      title = list(
        text = ylab,
        font = list(
          size = dict_fontsize[["y_label"]],
          family = dict_fontsize[["font"]],
          color = dict_color[["font_color"]]
        ),
        standoff = 2
      ),
      tickfont = list(
        size = dict_fontsize[["y_ticks"]],
        family = dict_fontsize[["font"]],
        color = dict_color[["font_color"]]
      ),
      ticklen = 0,
      tickpad = 0,
      automargin = TRUE
    )

    # Optional: y-axis range and ticks
    if (is.numeric(yrange) && length(yrange) >= 2 && !all(is.na(yrange))) {
      yaxis$range <- range(yrange)
      tick_step <- yrange[2] - yrange[1]
      if (is.finite(tick_step) && tick_step != 0) {
        yaxis$tickmode <- "array"
        yaxis$tickvals <- yrange
      }
    }

    return(list(annotations, xaxis, yaxis))

  }, error = function(e) {
    message("Error in set_layout(): ", e$message)
    return(list(NULL, NULL, NULL))
  })
}
