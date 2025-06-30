#' Generate Combined Subplots for Multiple Time Series
#'
#' Creates multiple time series plots and combines them into a shared interactive
#' subplot layout using Plotly. It automatically determines layout (rows/columns)
#' and synchronizes axes based on common y-ranges.
#'
#' @param list_args_variables A named list of argument lists, where each item contains
#'   the arguments for `get_unique_plot()` plus optional layout info like `row`.
#'
#' @return A single plotly subplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' schema <- generate_plot_schema(input_parameters)
#' df <- prepare_data_from_schema(schema)
#' list_args <- build_all_plot_inputs(df, schema)
#' get_combined_plots(list_args)
#' }
get_combined_plots <- function(list_args_variables) {
  tryCatch({

    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("Please install the 'plotly' package to use interactive subplots.")
    }

    plots <- list()
    n_layout <- list()

    # Generate individual plotly plots from argument sets
    for (var_name in names(list_args_variables)) {
      plots[[var_name]] <- do.call(get_unique_plot, list_args_variables[[var_name]])
      n_layout[[var_name]] <- list(row = list_args_variables[[var_name]]$row)
    }

    # Determine layout grid: number of rows and columns
    n <- length(plots)
    n_rows <- if (any(sapply(n_layout, function(x) is.null(x$row)))) {
      ceiling(sqrt(n))  # automatic square-like layout
    } else {
      max(sapply(n_layout, `[[`, "row"))  # user-specified layout
    }
    n_cols <- ceiling(n / n_rows)

    # Fill empty spaces in layout if specific row indices provided
    if (!any(sapply(n_layout, function(x) is.null(x$row)))) {
      filled <- fill_layout_with_empty_plots(plots, n_layout)
      plots <- filled$plots
      n_layout <- filled$n_layout
    }

    # Combine all plots into subplot
    combined_plot <- plotly::subplot(
      plots,
      nrows = n_rows,
      margin = 0.05,
      shareX = FALSE,
      shareY = FALSE,
      titleX = TRUE,
      titleY = TRUE
    )

    # Set up shared y-axes for groups of plots with matching yrange
    shared_yrange_groups <- get_vars_same_yrange(list_args_variables, return_largest = FALSE)
    name_to_index <- setNames(seq_along(plots), names(plots))
    layout_axes <- list()

    for (row in seq_len(n_rows)) {
      row_indices <- ((row - 1) * n_cols + 1):min(row * n_cols, length(plots))
      row_vars <- names(plots)[row_indices]

      for (group in shared_yrange_groups) {
        matched_vars <- intersect(row_vars, group)
        if (length(matched_vars) > 1) {
          for (var in matched_vars) {
            i <- name_to_index[[var]]
            axis_name <- paste0("yaxis", ifelse(i == 1, "", i))
            layout_axes[[axis_name]] <- list(matches = "y")
          }
        }
      }
    }

    # Match x-axes for all plots
    for (i in seq_along(plots)) {
      axis_name <- paste0("xaxis", ifelse(i == 1, "", i))
      layout_axes[[axis_name]] <- list(matches = "x")
    }

    # Apply matched axes layout
    combined_plot <- do.call(plotly::layout, c(list(p = combined_plot), layout_axes))

    # Add title and styling to the full subplot
    combined_plot <- plotly::layout(
      combined_plot,
      title = list(
        text = list_args_variables[[names(list_args_variables)[[1]]]]$general_title,
        x = 0.5,
        y = 1.10,
        font = list(
          size = dict_fontsize[["title"]],
          color = dict_color[["font_color"]],
          family = dict_fontsize[["font"]]
        )
      ),
      dragmode = "zoom"
    )

    return(combined_plot)

  }, error = function(e) {
    message("Error in get_combined_plots(): ", e$message)
    return(NULL)
  })
}
