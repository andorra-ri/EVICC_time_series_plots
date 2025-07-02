devtools::load_all()  # Loads all functions from R/ like a package

if (interactive()){

  input_parameters <- list(
    # File settings
    csv_pathfile            = file.path("data", "indicadors_hidric.csv"),   # (string) Path to the input CSV file
    csv_sep                 = ";",                                          # (string) Field delimiter: ";" / "," / "\t" / etc.

    # Time variable
    time_var                = "date2",                                      # (string) Column name for the time variable (avoid names like "data")

    # Data grouping
    annual_values_by_month  = TRUE,                                        # (logical) TRUE = group annually by month, FALSE = don't group

    # General settings
    general_title           = "",                                           # (string) Title for the entire plot layout; "" for no title
    vertical_lines_year     = TRUE,                                         # (logical) TRUE = add vertical year lines, FALSE = don't
    moving_avg              = TRUE,                                         # (logical) TRUE = apply moving average, FALSE = raw data
    k_ma                    = 6,                                            # (integer) Number of periods for moving average (e.g., 6, 12)
    trend                   = TRUE,                                         # (logical) TRUE = add trend line, FALSE = omit

    # Plot configurations
    # Use the variable name you wish to plot as the key in the 'plots' list.
    # Examples: anomalies, iqr, etc.
    plots = list(
      anomalies = list(
        title               = "",                                  # (string) Plot title
        plot_type           = "line",                                       # (string) Plot type: "line" / "bar"
        color_per_value_var = TRUE,                                         # (logical) TRUE = color based on value sign, FALSE = single color
        xlab                = "",                                           # (string) X-axis label; "" to omit
        ylab                = "%",                                          # (string) Y-axis label
        yrange              = NULL,                                         # (NULL or numeric vector) Y-axis range: NULL = auto / e.g., c(0, 100)
        row                 = 1                                             # (integer or NULL) Layout row position; NULL = auto
      ),
      iqr = list(
        title               = " ",                                  # (string) Plot title
        plot_type           = "line",                                       # (string) Plot type: "line" / "bar"
        color_per_value_var = FALSE,                                         # (logical) TRUE = color based on value sign, FALSE = single color
        xlab                = "",                                           # (string) X-axis label; "" to omit
        ylab                = "%",                                          # (string) Y-axis label
        yrange              = "     ",                                         # (NULL or numeric vector) Y-axis range: NULL = auto / e.g., c(0, 100)
        row                 = 1                                           # (integer or NULL) Layout row position; NULL = auto
      ),
      disponibilitat = list(
        title               = "         ",                                  # (string) Plot title
        plot_type           = "line",                                       # (string) Plot type: "line" / "bar"
        color_per_value_var = FALSE,                                         # (logical) TRUE = color based on value sign, FALSE = single color
        xlab                = "",                                           # (string) X-axis label; "" to omit
        ylab                = "%",                                          # (string) Y-axis label
        yrange              = "",                                         # (NULL or numeric vector) Y-axis range: NULL = auto / e.g., c(0, 100)
        row                 = 2                                            # (integer or NULL) Layout row position; NULL = auto
      ),
      disponibilitat_mean = list(
        title               = "disponibilitat_VSDV_mean",                                  # (string) Plot title
        plot_type           = "bar",                                       # (string) Plot type: "line" / "bar"
        color_per_value_var = FALSE,                                         # (logical) TRUE = color based on value sign, FALSE = single color
        xlab                = "",                                           # (string) X-axis label; "" to omit
        ylab                = "%",                                          # (string) Y-axis label
        yrange              = seq(0, 1000, 200),                                         # (NULL or numeric vector) Y-axis range: NULL = auto / e.g., c(0, 100)
        row                 = 2                                           # (integer or NULL) Layout row position; NULL = auto
      )
  )
)
  plot_ts_data(input_parameters)

}




