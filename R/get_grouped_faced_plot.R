#' Create a faceted time series plot grouped by a categorical variable
#'
#' This function generates a faceted Plotly time series plot using ggplot2,
#' where each facet corresponds to a level of a grouping variable. It includes
#' a dashed horizontal line indicating the group-level mean and allows custom
#' axis labels, titles, and axis ranges.
#'
#' @param df A data frame containing the input variables.
#' @param time_var Character; name of the time variable (e.g., "year").
#' @param value_var Character; name of the numeric value variable.
#' @param x_var Character; variable for x-axis (usually time_var).
#' @param group_var Character; name of the categorical grouping variable (e.g., "month").
#' @param trend_line Logical; if TRUE, include trend lines per group.
#' @param xlab Character; label for the x-axis.
#' @param ylab Character; label for the y-axis.
#' @param title Character; main title of the plot.
#' @param yrange Numeric vector of length 2; y-axis range.
#'
#' @return A `plotly` object showing faceted time series plots.
#' @export
#'
#' @importFrom dplyr group_by summarise %>%
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline facet_wrap
#'   scale_x_continuous labs theme theme_minimal element_blank element_line
#' @importFrom plotly ggplotly layout
#' @importFrom rlang sym
#'
#' @examples
#' get_grouped_facet_plot(df, time_var = "year", value_var = "sales",
#'                        x_var = "year", group_var = "month",
#'                        xlab = "Year", ylab = "Sales", title = "Monthly Sales",
#'                        yrange = c(0, 100))


get_grouped_facet_plot <- function(df, time_var, value_var, x_var, group_var,
                                   trend_line, xlab, ylab, title, general_title, yrange) {
  tryCatch({

    # Convert variable names to symbols
    time_sym  <- rlang::sym(time_var)
    value_sym <- rlang::sym(value_var)
    x_sym     <- rlang::sym(x_var)
    group_sym <- rlang::sym(group_var)

    # Check for required columns
    required_cols <- c(time_var, value_var, x_var, group_var)
    if (!all(required_cols %in% names(df))) {
      stop("Missing required columns: ", paste(setdiff(required_cols, names(df)), collapse = ", "))
    }

    # Create 'month_year' label for tooltip
    df$tooltip_text <- paste0(df[[group_var]], " ", year(df[[time_var]]), "<br>", title, ": ", sprintf("%.2f", df[[value_var]]), ylab)

    # Compute group means
    group_avg <- df %>%
      dplyr::group_by(!!group_sym) %>%
      dplyr::summarise(avg_val = mean(!!value_sym, na.rm = TRUE), .groups = "drop")

    # Force month order (optional)
    month_levels <- month.abb
    df[[group_var]] <- factor(df[[group_var]], levels = month_levels)
    group_avg[[group_var]] <- factor(group_avg[[group_var]], levels = month_levels)

    # x-axis ticks
    seq_vector <- seq(min(df[[x_var]], na.rm = TRUE), max(df[[x_var]], na.rm = TRUE), by = (max(df[[x_var]], na.rm = TRUE)-min(df[[x_var]], na.rm = TRUE)))

    # Base ggplot with tooltip
    p <- ggplot2::ggplot(df, ggplot2::aes(x = !!x_sym, y = !!value_sym, group = !!group_sym, text = tooltip_text)) +
      ggplot2::geom_line(size = 0.3, show.legend = FALSE, color = dict_color[["main_color"]]) +
      ggplot2::geom_point(size = 1, show.legend = FALSE, color = dict_color[["main_color"]]) +
      ggplot2::geom_hline(
        data = group_avg,
        ggplot2::aes(yintercept = .data[["avg_val"]]),
        color = dict_color[["hline"]],
        linetype = "dashed",
        size = 0.3,
        inherit.aes = FALSE
      ) +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", group_var)),
        nrow = 1, scales = "fixed"
      )

    if (!(length(yrange) == 2 && all(is.na(yrange))))  {
      p <- p + ggplot2::scale_y_continuous(
        limits = range(yrange, na.rm = TRUE),
        breaks = yrange
      )
    }

      p <- p + ggplot2::theme(
        panel.spacing = ggplot2::unit(0.5, "lines"),  # spacing between panels
        panel.border = ggplot2::element_rect(color = dict_color[["grid_lines_color"]], fill = NA, size = gridwidth + 0.3),  # this adds borders
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        strip.background = ggplot2::element_rect(fill = "white", color = NA)
      )

    if (trend_line) {
      p <- p + ggplot2::stat_smooth(
        method = "lm",
        se = FALSE,
        linetype = "dotted",
        size = 0.2,
        color = dict_color[["trend_line_color"]],
        show.legend = FALSE
      )
    }

    layout_plotly_obj <- set_layout(
      title = title,
      xlab = xlab,
      ylab = ylab,
      xrange = seq_vector,
      yrange = yrange,
      row = 1
    )

    annotations <- layout_plotly_obj[[1]]
    annotations[[1]]$y <- 1.03
    yaxis <- layout_plotly_obj[[3]]

    # 🎯 ggplotly with custom tooltip
    p <- plotly::ggplotly(p, tooltip = "text")
    p <- plotly::layout(p, annotations = annotations, yaxis = yaxis)

    return(p)

  }, error = function(e) {
    message("Error in get_grouped_facet_plot(): ", e$message)
    return(NULL)
  })
}
