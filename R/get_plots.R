#' Generate Interactive Time Series Plot(s) from Input Parameters
#'
#' This is the main entry point for generating one or more time series plots.
#' It dynamically handles single-variable, multi-variable, and grouped faceted plots
#' using schema-driven configuration and outputs interactive Plotly visualizations.
#'
#' @name plot_ts_data
#' @param input_parameters A named list of configuration parameters.
#'   Must include items like csv_pathfile, time_var, and a list of plots.
#' @return A plotly object or a named list of plotly objects, depending on the plot structure.
#' @importFrom plotly plot_ly
#' @export
#'
#' @examples
#' \dontrun{
#' input <- list(
#'   csv_pathfile = "data/example.csv",
#'   csv_sep = ",",
#'   time_var = "date",
#'   plots = list(
#'     list(value_var = "sales", plot_type = "line", title = "Sales Over Time")
#'   )
#' )
#' plot_ts_data(input)
#' }
plot_ts_data <- function(input_parameters) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Please install the 'plotly' package to use interactive plots.")
  }

  result <- tryCatch({

    # Generate schema from input parameters
    schema <- generate_plot_schema(input_parameters)

    # Load and preprocess data based on schema
    df <- prepare_data_from_schema(schema)

    global <- schema$global
    plots_cfg <- schema$plots
    annual_values_by_month <- global$annual_values_by_month

    if (!annual_values_by_month) {
      # Handle single or multi-variable non-grouped plots
      list_args <- build_all_plot_inputs(df, schema)

      if (length(plots_cfg) > 1) {
        p <- get_combined_plots(list_args)  # Combine multiple plots
      } else {
        p <- do.call(get_unique_plot, list_args[[1]])  # Single plot
      }

      p <- set_last_window_view(p, df, global$time_var)

    } else {
      # Handle grouped plots by month_name
      p <- lapply(names(plots_cfg), function(var_name) {
        var_cfg <- plots_cfg[[var_name]]

        args <- list(
          df = df,
          time_var = global$time_var,
          value_var = var_cfg$value_var,
          x_var = "year",
          group_var = "month_name",
          trend_line = global$trend,
          xlab = var_cfg$xlab,
          ylab = var_cfg$ylab,
          title = var_cfg$title,
          general_title = global$general_title,
          yrange = var_cfg$yrange
        )
        do.call(get_grouped_facet_plot, args)
      })
      names(p) <- names(plots_cfg)
    }

    # Limit initial zoom based on detected time granularity
    granularity <- detect_time_granularity(df[[global$time_var]])
    window_bar <- if (granularity == "daily") 1 else 7
    p <- limit_zoom_js(p, window_bar)

    return(p)

  }, error = function(e) {
    message("An error occurred in plot_ts_data(): ", e$message)
    return(NULL)
  })

  return(result)
}
