#' Generate a structured plot schema from input parameters
#'
#' This function transforms a user-defined list of plotting parameters into a structured schema
#' that includes global settings and plot-specific configurations. This schema is used for
#' subsequent time series plotting functions.
#'
#' @param input_parameters A named list. Must include global keys like `time_var`, `csv_pathfile`,
#'        and a `plots` sublist where each element defines plot-specific parameters such as
#'        `value_var`, `plot_type`, `title`, etc.
#'
#' @return A structured list with two components:
#'   \describe{
#'     \item{global}{A list of global settings (e.g., time_var, csv_pathfile).}
#'     \item{plots}{A named list where each item corresponds to a variable to be plotted.}
#'   }
#' @export
generate_plot_schema <- function(input_parameters) {
  tryCatch({

    # Sanity check: prevent using reserved word as time variable
    if (!is.null(input_parameters$time_var) && input_parameters$time_var == "data") {
      stop("Invalid 'time_var': 'data' is not a suitable name for a variable. Please choose a different name.")
    }

    # Extract user-defined plot list
    plot_config_list <- input_parameters$plots
    value_vars <- names(plot_config_list)

    # Create individual configuration per plot variable
    per_var_list <- list()
    for (var_name in value_vars) {
      cfg <- plot_config_list[[var_name]]

      per_var_list[[var_name]] <- list(
        value_var = cfg$value_var,
        plot_type = cfg$plot_type,
        xlab = if (is.null(cfg$xlab) || trimws(cfg$xlab) == "") NULL else cfg$xlab,
        ylab = if (is.null(cfg$ylab) || trimws(cfg$ylab) == "") NULL else cfg$ylab,
        title = if (is.null(cfg$title) || trimws(cfg$title) == "") var_name else cfg$title,
        color_per_value_var = if (is.null(cfg$color_per_value_var)) TRUE else cfg$color_per_value_var,
        yrange = if (is.null(cfg$yrange) || (is.character(cfg$yrange) && all(trimws(cfg$yrange) == ""))) {
          rep(NA, 2)
        } else {
          cfg$yrange
        },
        row = cfg$row
      )
    }

    # Define global plotting settings
    schema <- list(
      global = list(
        csv_pathfile = input_parameters$csv_pathfile,
        csv_sep = input_parameters$csv_sep,
        time_var = input_parameters$time_var,
        general_title = if (is.null(input_parameters$general_title) || trimws(input_parameters$general_title) == "") {
          NULL
        } else {
          input_parameters$general_title
        },
        grouped_plot_by_var = input_parameters$grouped_plot_by_var,
        grouped_plot_x_var = input_parameters$grouped_plot_x_var,
        dict_color = input_parameters$dict_color,
        k_ma = input_parameters$k_ma,
        vertical_lines_year = input_parameters$vertical_lines_year,
        moving_avg = input_parameters$moving_avg,
        trend = input_parameters$trend
      ),
      plots = per_var_list
    )

    return(schema)

  }, error = function(e) {
    message("Error in generate_plot_schema(): ", e$message)
    return(NULL)
  })
}


#' Parse and clean a time variable column
#'
#' Attempts to parse a specified column in a data frame into a recognized date format
#' (Date or POSIXt). Automatically detects common date separators and formats.
#' Adds derived columns such as year, month, day, and month name.
#'
#' @param data A data frame containing a column to be parsed as a date.
#' @param time_var A character string representing the column name to be parsed.
#'
#' @return A data frame with the parsed date column and additional columns:
#'   \itemize{
#'     \item `year`: extracted year (if parseable)
#'     \item `month`: extracted month (numeric)
#'     \item `day`: extracted day
#'     \item `month_name`: month name as factor (Jan–Dec)
#'   }
#'   If parsing fails, returns NULL.
#'
#' @keywords internal
#' @importFrom lubridate parse_date_time year month day
#'
#' @examples
#' df <- data.frame(date = c("2023-01-01", "2023-02", "01/2023", "March"))
#' parse_time_data(df, "date")
parse_time_data <- function(data, time_var) {
  tryCatch({

    # Ensure the specified time_var exists in the data frame
    if (!time_var %in% names(data)) {
      stop(sprintf("The variable '%s' does not exist in the dataset.", time_var))
    }

    # Ensure the column is in an acceptable format
    if (!(
      is.character(data[[time_var]]) ||
      is.numeric(data[[time_var]]) ||
      is.factor(data[[time_var]]) ||
      inherits(data[[time_var]], "Date") ||
      inherits(data[[time_var]], "POSIXt")
    )) {
      stop(sprintf("The variable '%s' must be character, numeric, factor, Date, or POSIXt for parsing.", time_var))
    }

    # Convert to character to simplify parsing
    raw_time <- as.character(data[[time_var]])

    # Infer separator or format pattern
    sep_char <- if (all(grepl("-", raw_time))) {
      "-"
    } else if (all(grepl("/", raw_time))) {
      "/"
    } else if (all(grepl("\\.", raw_time))) {
      "."
    } else if (all(grepl("^\\d{4}$", raw_time))) {
      " "
    } else {
      stop("Unable to determine the date format. Input must use consistent separators like '-', '/', '.', or be a 4-digit year.")
    }

    # Choose appropriate parsing formats based on separator
    orders <- switch(
      sep_char,
      "-" = c("y-m-d", "y-b-d", "y-B-d", "d-m-y", "d-b-y", "d-B-y", "y-m", "m-y"),
      "/" = c("y/m/d", "d/m/y", "y/m", "m/y"),
      "." = c("y.m.d", "d.m.y", "y.m", "m.y"),
      " " = c("y")
    )

    # Parse the time values
    parsed <- lubridate::parse_date_time(raw_time, orders = orders, exact = FALSE, quiet = TRUE)

    # Replace original column with parsed values
    data[[time_var]] <- parsed

    # Extract and append additional time components
    data$year <- suppressWarnings(ifelse(is.na(parsed), NA, lubridate::year(parsed)))
    data$month <- suppressWarnings(ifelse(is.na(parsed), NA, lubridate::month(parsed)))
    data$day <- suppressWarnings(ifelse(is.na(parsed), NA, lubridate::day(parsed)))

    # Add month name (ordered factor Jan to Dec)
    Sys.setlocale("LC_TIME", "C")
    data$month_name <- suppressWarnings(ifelse(is.na(parsed), NA,
                                               as.character(lubridate::month(parsed, label = TRUE))))
    data$month_name <- factor(data$month_name, levels = month.abb)

    # Clean up uninformative columns
    if (all(is.na(data$year))) data$year <- NULL
    if (all(is.na(data$month))) data$month <- NULL

    return(data)

  }, error = function(e) {
    message("Error parsing time variable: ", e$message)
    return(NULL)
  })
}


#' Validate and Prepare Data from Schema for Time Series Plotting
#'
#' This function loads a CSV file specified in a schema object,
#' validates that the required time and value variables are present and properly typed,
#' parses the time column into Date/POSIX format, and returns a cleaned dataset.
#'
#' @param schema A list object produced by `generate_plot_schema()`, including `global` and `plots` sections.
#' @param verbose Logical; if TRUE, displays progress messages during processing.
#'
#' @return The cleaned and validated data frame (returned invisibly), or NULL if an error occurs.
#'
#' @keywords internal
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' \dontrun{
#' schema <- generate_plot_schema(input_parameters)
#' df <- prepare_data_from_schema(schema, verbose = TRUE)
#' }
prepare_data_from_schema <- function(schema, verbose = FALSE) {

  # Logging utility
  log_message <- function(msg) {
    if (verbose) message("[prepare_data_from_schema] ", msg)
  }

  # Helper function to validate numeric columns
  validate_variable <- function(data, var_name) {
    if (!(var_name %in% names(data))) {
      stop(sprintf("Variable '%s' is missing from the dataset."), call. = FALSE)
    }
    if (!is.numeric(data[[var_name]])) {
      stop(sprintf("Variable '%s' must be numeric. Current type: %s",
                   var_name, class(data[[var_name]])), call. = FALSE)
    }
  }

  tryCatch({

    # Retrieve file path and separator
    csv_pathfile <- schema$global$csv_pathfile
    csv_sep <- schema$global$csv_sep

    log_message(sprintf("Checking file existence: %s", csv_pathfile))
    if (!file.exists(csv_pathfile)) {
      stop(sprintf("CSV file not found at path: %s", csv_pathfile), call. = FALSE)
    }

    log_message("Reading CSV file...")
    data <- tryCatch({
      read.csv(csv_pathfile, sep = csv_sep)
    }, error = function(e) {
      stop("Failed to read CSV file: ", e$message, call. = FALSE)
    })

    if (is.null(data) || nrow(data) == 0) {
      stop("Loaded data is NULL or contains no rows.", call. = FALSE)
    }

    # Extract and validate time variable
    time_var <- schema$global$time_var
    log_message(sprintf("Validating time variable: '%s'", time_var))
    if (!(time_var %in% names(data))) {
      stop(sprintf("Time variable '%s' is not present in the dataset.", time_var), call. = FALSE)
    }

    log_message("Parsing time column...")
    data <- tryCatch({
      parse_time_data(data, time_var)
    }, error = function(e) {
      stop("Error parsing time variable: ", e$message, call. = FALSE)
    })

    if (!inherits(data[[time_var]], c("Date", "POSIXt"))) {
      stop(sprintf("Parsed time variable '%s' must be of class Date or POSIXt. Got: %s",
                   time_var, class(data[[time_var]])), call. = FALSE)
    }

    # Validate each value variable
    list_value_vars <- names(schema$plots)
    log_message(sprintf("Validating plot variables: %s", paste(list_value_vars, collapse = ", ")))

    for (var in list_value_vars) {
      validate_variable(data, var)
    }

    log_message("All validations passed. Returning cleaned dataset.")
    return(invisible(data))

  }, error = function(e) {
    message("[ERROR] prepare_data_from_schema: ", e$message)
    return(NULL)
  })
}


#' Build Plotting Arguments from Schema
#'
#' Constructs a complete argument list for time series plotting functions
#' such as `get_unique_plot()` or `get_grouped_facet_plot()` based on
#' global settings and per-variable configurations provided in the schema.
#'
#' @param df A data frame containing time series data.
#' @param global_parameters A list of global plotting parameters (e.g., time_var, moving_avg).
#' @param var_parameters A list of plot-specific configuration for a single variable.
#' @param extra Optional list of additional arguments to override or extend defaults.
#'
#' @return A named list of arguments ready for use in `do.call()`.
#' @export
#'
#' @examples
#' \dontrun{
#' args <- build_plot_args(df, schema$global, schema$plots[["sales"]])
#' p <- do.call(get_unique_plot, args)
#' }
build_plot_args <- function(df, global_parameters, var_parameters, extra = list()) {
  tryCatch({

    # Extract key variable names
    time_var <- global_parameters$time_var
    value_var <- var_parameters$value_var

    # Construct base argument list
    args <- list(
      time_var = time_var,
      value_var = value_var,
      df = df[, c(time_var, value_var), drop = FALSE],
      plot_type = var_parameters$plot_type,
      color_per_value_var = var_parameters$color_per_value_var,
      moving_avg = global_parameters$moving_avg,
      trend = global_parameters$trend,
      k_ma = global_parameters$k_ma,
      title = var_parameters$title,
      xlab = var_parameters$xlab,
      ylab = var_parameters$ylab,
      vertical_lines_year = global_parameters$vertical_lines_year,
      yrange = var_parameters$yrange,
      row = var_parameters$row
    )

    # Merge with additional overrides if provided
    if (length(extra) > 0) {
      args <- modifyList(args, extra)
    }

    return(args)

  }, error = function(e) {
    message("Error in build_plot_args(): ", e$message)
    return(NULL)
  })
}

#' Build list of plot input parameters for all schema-defined variables
#'
#' Iterates over all variables defined in the `schema$plots` section and constructs
#' individual argument lists for each using `build_plot_args()`. This is a helper function
#' that prepares inputs for batch plotting functions like `get_combined_plots()`.
#'
#' @param df A data frame containing the time series data to be plotted.
#' @param schema A schema list created by `generate_plot_schema()`, containing global and per-variable configs.
#'
#' @return A named list of argument lists (one per plot variable) for use with `do.call()`.
#' @export
#'
#' @examples
#' \dontrun{
#' schema <- generate_plot_schema(input)
#' df <- prepare_data_from_schema(schema)
#' input_list <- build_all_plot_inputs(df, schema)
#' }
build_all_plot_inputs <- function(df, schema) {
  tryCatch({

    # Extract global settings from schema
    global_parameters <- schema$global
    input_parameters_list <- list()

    # Loop through each plot configuration
    for (var_name in names(schema$plots)) {
      input_parameters_list[[var_name]] <- build_plot_args(
        df = df,
        global_parameters = global_parameters,
        var_parameters = schema$plots[[var_name]]
      )
    }

    return(input_parameters_list)

  }, error = function(e) {
    message("Error in build_all_plot_inputs(): ", e$message)
    return(NULL)
  })
}


#' Check if a date vector contains valid year information
#'
#' This helper function checks whether the input date vector contains any non-missing
#' year values that appear to be realistic (i.e., year >= 1000). Useful for validating
#' whether a date-like object includes full date information.
#'
#' @param dates A vector of Date, POSIXt, or character values convertible to dates.
#'
#' @return Logical; TRUE if year information is detected in at least one entry, FALSE otherwise.
#' @keywords internal
#'
#' @examples
#' has_year(as.Date("2023-01-01"))              # TRUE
#' has_year(as.Date("01-01", format = "%m-%d")) # FALSE
has_year <- function(dates) {
  tryCatch({

    # Extract year component from each date
    years <- lubridate::year(dates)

    # Return TRUE if at least one year is valid
    any(!is.na(years) & years >= 1000)

  }, error = function(e) {
    # If anything goes wrong, safely return FALSE
    FALSE
  })
}

#' Apply a Pipeline of Plotly Transformations
#'
#' This function applies a sequence of transformation functions to a plotly plot object.
#' Each function in the pipeline must take a plotly object as input and return a modified one.
#' This enables composable, modular manipulation of plots (e.g., adding trend lines, overlays).
#'
#' @param p A plotly object (e.g., returned from `plot_ly()` or `ggplotly()`).
#' @param pipeline A list of functions, each taking and returning a plotly object.
#'
#' @return A plotly object resulting from applying all transformations in sequence.
#' @keywords internal
#' @importFrom purrr reduce
#'
#' @examples
#' \dontrun{
#' pipeline <- list(
#'   function(p) add_trend_line(p, trend = TRUE, data = df, time_var = time, value_var = value),
#'   function(p) add_moving_average_plot(p, TRUE, df, value_var = value, k_ma = 3)
#' )
#' base_plot <- plot_ly(df, x = ~time) %>% add_bars(y = ~value)
#' result_plot <- apply_plot_pipeline(base_plot, pipeline)
#' }
apply_plot_pipeline <- function(p, pipeline) {
  tryCatch({

    # Apply each transformation function in order using purrr::reduce
    purrr::reduce(pipeline, function(p_acc, f) f(p_acc), .init = p)

  }, error = function(e) {
    message("Error in apply_plot_pipeline(): ", e$message)
    return(NULL)
  })
}


#' Group Variables by Shared Y-Axis Range
#'
#' This function examines the `yrange` parameter for each variable and identifies
#' which variables share the same y-axis range configuration. It can optionally return
#' only the largest such group.
#'
#' @param input_parameters A named list of plot configurations (typically schema$plots).
#' @param return_largest Logical; if TRUE, returns only the largest group sharing the same `yrange`.
#'                       If FALSE, returns all such groups.
#'
#' @return A character vector (if `return_largest = TRUE`) or a list of vectors grouped by shared `yrange`.
#'         Returns NULL if no valid `yrange` is defined.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_vars_same_yrange(schema$plots)
#' get_vars_same_yrange(schema$plots, return_largest = FALSE)
#' }
get_vars_same_yrange <- function(input_parameters, return_largest = TRUE) {
  tryCatch({

    # Convert each yrange to a unique string key; ignore NULL or all-NA
    clean_yranges <- lapply(input_parameters, function(p) {
      yr <- p$yrange
      if (is.null(yr) || all(is.na(yr))) return(NULL)
      paste(sort(unique(yr)), collapse = "-")
    })

    # Remove any NULL values
    non_null_keys <- Filter(Negate(is.null), clean_yranges)

    if (length(non_null_keys) == 0) return(NULL)

    # Group variable names by their `yrange` signature
    grouped <- split(names(non_null_keys), unlist(non_null_keys))

    if (return_largest) {
      # Identify the group with the most members
      largest_key <- names(which.max(sapply(grouped, length)))
      return(grouped[[largest_key]])
    } else {
      # Return full grouping structure
      return(grouped)
    }

  }, error = function(e) {
    message("Error in get_vars_same_yrange(): ", e$message)
    return(NULL)
  })
}

#' Fill Layout with Empty Plotly Placeholders
#'
#' Ensures all grid rows in a faceted Plotly layout have the same number of columns.
#' If some rows have fewer plots than others, this function inserts empty placeholder
#' plots (`plotly::plotly_empty()`) to balance the layout.
#'
#' @param plots A named list of Plotly plot objects.
#' @param n_layout A named list of layout info (row/col) for each plot, with names matching `plots`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{plots}{An updated list of Plotly objects including empty placeholders where needed.}
#'   \item{n_layout}{An updated layout list, including placeholders.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' filled <- fill_layout_with_empty_plots(my_plots, my_layout)
#' subplot(filled$plots, nrows = 2)
#' }
fill_layout_with_empty_plots <- function(plots, n_layout) {
  tryCatch({

    # Group plot names by their assigned row
    row_groups <- split(names(plots), sapply(n_layout, function(x) x$row))

    # Find the maximum number of columns in any row
    max_cols <- max(sapply(row_groups, length))

    # Prepare output containers
    ordered_plots <- list()
    ordered_layout <- list()
    empty_counter <- 1

    for (row in sort(as.numeric(names(row_groups)))) {
      group <- row_groups[[as.character(row)]]
      col_idx <- 1

      # Add actual plots with inferred or existing column index
      for (var_name in group) {
        if (is.null(n_layout[[var_name]]$col) || is.na(n_layout[[var_name]]$col)) {
          n_layout[[var_name]]$col <- col_idx
        }
        ordered_plots[[var_name]] <- plots[[var_name]]
        ordered_layout[[var_name]] <- n_layout[[var_name]]
        col_idx <- col_idx + 1
      }

      # Add empty plot placeholders if this row is incomplete
      while (col_idx <= max_cols) {
        empty_name <- paste0("__empty_", empty_counter)
        ordered_plots[[empty_name]] <- plotly::plotly_empty()
        ordered_layout[[empty_name]] <- list(row = as.integer(row), col = col_idx)
        col_idx <- col_idx + 1
        empty_counter <- empty_counter + 1
      }
    }

    return(list(plots = ordered_plots, n_layout = ordered_layout))

  }, error = function(e) {
    message("Error in fill_layout_with_empty_plots(): ", e$message)
    return(NULL)
  })
}


#' Detect Time Granularity
#'
#' Attempts to classify the temporal granularity of a time vector by analyzing
#' the interval between consecutive dates. Automatically converts character input
#' to `Date` class where needed.
#'
#' This function returns a classification as one of: `"daily"`, `"weekly"`,
#' `"monthly"`, or `"yearly"`, based on typical interval thresholds.
#'
#' @param time_vector A vector of `Date`, `POSIXt`, or coercible character timestamps.
#'        If not already in date format, it will be coerced via `as.Date()`.
#'
#' @return A character string indicating the granularity: one of `"daily"`, `"weekly"`,
#'         `"monthly"`, `"yearly"`; or `NULL` if detection fails or input is invalid.
#'
#' @examples
#' detect_time_granularity(as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")))
#' # Returns: "daily"
#'
#' detect_time_granularity(as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")))
#' # Returns: "monthly"
#'
#' detect_time_granularity(as.Date(c("2010-01-01", "2011-01-01", "2012-01-01")))
#' # Returns: "yearly"
#'
#' detect_time_granularity(c("2020-01-01", "2020-01-08", "2020-01-15"))
#' # Returns: "weekly"
#'
#' detect_time_granularity("not a date")
#' # Returns: NULL (with message)
#'
#' @export

detect_time_granularity <- function(time_vector) {
  tryCatch({
    # Attempt to convert input to Date if not already
    if (!inherits(time_vector, c("Date", "POSIXt"))) {
      time_vector <- tryCatch(as.Date(time_vector), error = function(e) NA)
    }

    # Remove NAs and check length
    time_vector <- na.omit(time_vector)
    if (length(time_vector) < 2) {
      stop("At least two valid time points are required to detect granularity.")
    }

    # Compute time differences in days
    diffs <- diff(sort(time_vector))
    time_interval <- unique(as.numeric(diffs))

    # Classify granularity
    if (all(time_interval > 360)) {
      return("yearly")
    } else if (all(time_interval < 7)) {
      return("daily")
    } else if (all(time_interval < 28)) {
      return("weekly")
    } else {
      return("monthly")
    }

  }, error = function(e) {
    message("Error in detect_time_granularity(): ", e$message)
    return(NULL)
  })
}


#' Enforce Zoom Limits in Plotly Using Custom JavaScript
#'
#' Injects client-side JavaScript into a Plotly plot to prevent zooming below a
#' certain time window threshold. This is useful for avoiding overly granular or
#' misleading time views in interactive plots.
#'
#' @param plot A `plotly` plot object.
#' @param min_zoom_bars Minimum zoom width in "bar units" (e.g., 1 = 1 month).
#'
#' @return A modified `plotly` plot object with zoom constraints applied.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- plot_ly(data, x = ~date, y = ~value, type = 'scatter', mode = 'lines')
#' limit_zoom_js(p, min_zoom_bars = 1)
#' }
limit_zoom_js <- function(plot, min_zoom_bars) {
  tryCatch({
    # Inject JavaScript using htmlwidgets::onRender
    plot <- htmlwidgets::onRender(plot, sprintf("
      function(el, x) {
        let alertShown = false;

        el.on('plotly_relayout', function(eventdata) {
          let layout = el._fullLayout;
          let xaxis = layout.xaxis;

          let xStart = xaxis.range[0];
          let xEnd = xaxis.range[1];

          let start = new Date(xStart).getTime();
          let end = new Date(xEnd).getTime();

          let range = end - start;

          // Define minimum allowed zoom window in milliseconds
          let minRange = %d * 30 * 24 * 60 * 60 * 1000;

          // Only apply restriction during zoom events (not pan)
          if (eventdata['xaxis.range[0]'] !== undefined && eventdata['xaxis.range[1]'] !== undefined) {
            if (range < minRange) {
              let center = (start + end) / 2;
              let newStart = new Date(center - minRange / 2);
              let newEnd = new Date(center + minRange / 2);

              Plotly.relayout(el, {
                'xaxis.range': [newStart.toISOString(), newEnd.toISOString()]
              });

              if (!alertShown) {
                alert('Maximum zoom level reached. Zooming in further is not allowed.');
                alertShown = true;
              }
            }
          }
        });
      }
    ", min_zoom_bars))

    return(plot)

  }, error = function(e) {
    message("Error applying zoom limit to plot: ", e$message)
    return(plot)  # Return original plot if JS injection fails
  })
}



#' Compute Mean Values Grouped by One or More Variables
#'
#' Groups a dataset by one or more specified variables and calculates the mean of a
#' specified numeric variable, ignoring missing values (`NA`). This is useful for
#' aggregation and summary statistics.
#'
#' @param data A data frame containing the data to be grouped and summarized.
#' @param list_group_var A character vector of column names to group by.
#' @param value_var A character string indicating the column to summarize (must be numeric).
#'
#' @return A data frame with grouping variables and the corresponding average value.
#' Returns `NULL` on error.
#'
#' @examples
#' df <- data.frame(region = c("A", "A", "B"), year = c(2020, 2020, 2021), value = c(1, 2, 3))
#' get_mean_grouped_data(df, c("region", "year"), "value")
#'
#' @export
get_mean_grouped_data <- function(data, list_group_var, value_var) {
  tryCatch({
    # Validate required columns exist
    missing_vars <- setdiff(c(list_group_var, value_var), names(data))
    if (length(missing_vars) > 0) {
      stop(paste("Missing variables in data:", paste(missing_vars, collapse = ", ")))
    }

    # Convert target variable to a symbol for tidy evaluation
    value_sym <- rlang::sym(value_var)

    # Group by the provided grouping variables and calculate mean
    output <- data %>%
      dplyr::group_by_at(list_group_var) %>%
      dplyr::summarise(avg_val = mean(!!value_sym, na.rm = TRUE), .groups = "drop")

    return(output)

  }, error = function(e) {
    message("Error in get_mean_grouped_data(): ", e$message)
    return(NULL)
  })
}








