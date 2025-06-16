devtools::load_all()  # Loads all functions from R/ like a package

library(plotly)
library(dplyr)
library(zoo)
library(lubridate)
library(trend)
library(patchwork)

if (interactive()){

  input_parameters <- list(
    # Path to the CSV file containing the input time series data.
    # Use forward slashes or file.path() to ensure cross-platform compatibility.
    csv_pathfile = "./data/indicadors_hidric.csv",

    csv_sep = ";",

    # Name of the column representing time (e.g., "mes", "date", "year_month").
    time_var = "date2",

    # Title to display at the top of the combined plot (can be left empty).
    general_title = "",

    # Optional variable name used to group multiple plots (e.g., by region or category).
    # Set to NULL if plots are not grouped.
    grouped_plot_by_var = "month_name",

    grouped_plot_x_var = "year",

    # Logical: Add vertical lines at each new year on the x-axis.
    vertical_lines_year = TRUE,

    # Logical: Apply a moving average smoothing to the data.
    moving_avg = TRUE,

    # Integer: Number of periods used for the moving average (e.g., 6 = half-year).
    k_ma = 6,

    # Logical: Add a trend line to each plot (e.g., linear regression).
    trend = TRUE,

    # List of individual plots to generate, keyed by their internal plot name.
    plots = list(
      # Plot 1: Anomalies
      anomalies = list(
        # Name of the column in the data containing values to plot.
        value_var = "anomalies",
        # Title of the plot (leave empty to auto-generate or omit).
        title = "anomalies",
        # Type of plot: "bar", "line", etc.
        plot_type = "bar",
        # Logical: Use different colors for positive vs negative values.
        color_per_value_var = TRUE,
        # X-axis label.
        xlab = "",
        # Y-axis label.
        ylab = "%",
        # Y-axis limits (e.g., seq(0, 600, 100)), or NULL for auto-scaling.
        yrange = NULL,
        # Row in the subplot grid (e.g., 1 for top row). NULL lets the system auto-place it.
        row = 1
      ),
      iqr = list(
        # Name of the column in the data containing values to plot.
        value_var = "iqr",
        # Title of the plot (leave empty to auto-generate or omit).
        title = "iqrr",
        # Type of plot: "bar", "line", etc.
        plot_type = "bar",
        # Logical: Use different colors for positive vs negative values.
        color_per_value_var = TRUE,
        # X-axis label.
        xlab = "",
        # Y-axis label.
        ylab = "%",
        # Y-axis limits (e.g., seq(0, 600, 100)), or NULL for auto-scaling.
        yrange = NULL,
        # Row in the subplot grid (e.g., 1 for top row). NULL lets the system auto-place it.
        row = 2
      )

  )
  )

  #get_grouped_facet_plot(data = data, time_var = input_parameters$time_var, value_var =  input_parameters$value_var, group_var = input_parameters$grouped_plot_by_var, vertical_lines_year = input_parameters$vertical_lines_year, dict_color = dict_color)
  plot_ts_data(input_parameters)
}




