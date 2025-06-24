# Time Series Plotting Script

This project provides a flexible way to generate time series plots from a CSV file using customizable settings. It supports multiple plot types, moving averages, trends, and grouped annual data.

## 🔧 Usage

Install package:

```r
devtools::install_github("andorra-ri/EVICC_time_series_plots", auth_token = "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", force = TRUE)
```

The main script to run is:

```r

library(EVICC)

input_parameters <- list(
  # File settings
  csv_pathfile            = file.path("data", "indicadors_hidric.csv"),   # (string) Path to the input CSV file
  csv_sep                 = ";",                                          # (string) Field delimiter: ";" / "," / "\t" / etc.

  # Time variable
  time_var                = "date2",                                      # (string) Column name for the time variable (avoid names like "data")

  # Data grouping
  annual_values_by_month  = FALSE,                                        # (logical) TRUE = group annually by month, FALSE = don't group

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
      title               = "Anomalies",                                  # (string) Plot title
      plot_type           = "line",                                       # (string) Plot type: "line" / "bar"
      color_per_value_var = TRUE,                                         # (logical) TRUE = color based on value sign, FALSE = single color
      xlab                = "",                                           # (string) X-axis label; "" to omit
      ylab                = "%",                                          # (string) Y-axis label
      yrange              = NULL,                                         # (NULL or numeric vector) Y-axis range: NULL = auto / e.g., c(0, 100)
      row                 = 1                                             # (integer or NULL) Layout row position; NULL = auto
    ),

    iqr = list(
      title               = "iqr",                                        # (string) Plot title
      plot_type           = "line",                                       # (string) Plot type: "line" / "bar"
      color_per_value_var = TRUE,                                         # (logical) TRUE = color based on value sign, FALSE = single color
      xlab                = "",                                           # (string) X-axis label; "" to omit
      ylab                = "%",                                          # (string) Y-axis label
      yrange              = NULL,                                         # (NULL or numeric vector) Y-axis range: NULL = auto / e.g., c(0, 100)
      row                 = 2                                             # (integer or NULL) Layout row position; NULL = auto
    )
  )
)

plot_ts_data(input_parameters)

```

Where input_parameters is a list defining all necessary configurations for data input and visualization.

📥 Input Parameters
Below is a breakdown of the fields used in the input_parameters list:

📁 File Settings
Parameter	Type	Description
csv_pathfile	string	Path to the input CSV file (e.g., "data/indicadors_hidric.csv")
csv_sep	string	Field delimiter: ";", ",", "\t", etc.

🕒 Time Variable
Parameter	Type	Description
time_var	string	Name of the column representing time (avoid "data")

📊 Data Grouping and Transformation
Parameter	Type	Description
annual_values_by_month	logical	TRUE = group annual data by month, FALSE = no grouping
moving_avg	logical	Apply moving average to smooth data
k_ma	integer	Number of periods used in the moving average (e.g., 6, 12)
trend	logical	Add a trend line to the plot (TRUE / FALSE)
vertical_lines_year	logical	Show vertical lines at the start of each year

🖼️ General Plot Title
Parameter	Type	Description
general_title	string	Title displayed above the full plot layout (can be empty)

📈 Plot Configuration
The plots list defines each individual plot. Each element is keyed by the variable name you want to visualize (e.g., "anomalies", "iqr").


Example

```r
plots = list(
  anomalies = list(
    title = "Anomalies",
    plot_type = "line",
    color_per_value_var = TRUE,
    xlab = "",
    ylab = "%",
    yrange = NULL,
    row = 1
  ),
  iqr = list(
    title = "iqr",
    plot_type = "line",
    color_per_value_var = TRUE,
    xlab = "",
    ylab = "%",
    yrange = NULL,
    row = 2
  )
)
```
▶️ Running the Code
Once input_parameters is set up, call:

```r
plot_ts_data(input_parameters)
```

This will generate one or more plots based on the configuration.

📝 Notes
Ensure that your CSV file uses consistent date formatting.

Variable names in plots must match columns in your dataset.

This setup supports extensibility by adding more plots to the plots list.

<pre> ```text 📂 Project Structure ├── data/ │ └── indicadors_hidric.csv ├── scripts/ │ └── plot_ts_data.R ├── README.md └── your_project.Rproj ``` </pre>

📄 License

MIT License