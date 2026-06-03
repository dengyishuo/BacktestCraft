#' Generate signal based on rolling window statistics
#'
#' Create a 0/1 signal when the indicator value exceeds a rolling mean plus/minus
#' a multiple of rolling standard deviation (e.g., Bollinger Bands breakout) or
#' crosses a moving average. The rolling calculation is performed per asset.
#'
#' @param df Data frame in long format, must contain 'date', 'code', and the
#'   indicator column.
#' @param indicator_col Character string of column name to analyze (e.g., "close").
#' @param window Integer, rolling window length (e.g., 20 for 20-day).
#' @param n_sd Numeric, number of standard deviations for the band. Use 0 for
#'   simple moving average only.
#' @param direction Character: "above" (signal when indicator > center + n_sd * sd),
#'   "below" (indicator < center - n_sd * sd), or "cross_above" (crossover above
#'   the upper band, requires previous day condition).
#' @param center_type Character: "mean" (default) or "median" for the central tendency.
#' @param signal_name Character, output signal column name. Auto-generated if NULL.
#' @param output_type Output format: "tibble" (default) or "data.frame"
#'
#' @return Original data frame with an appended signal column.
#'
#' @importFrom dplyr group_by mutate lag ungroup
#' @importFrom zoo rollapply
#' @importFrom rlang .data !! sym :=
#' @export
#'
#' @examples
#' \dontrun{
#' # Bollinger Band breakout: close > 20-day mean + 2 * 20-day sd
#' df <- add_rolling_signal(df,
#'   indicator_col = "close", window = 20,
#'   n_sd = 2, direction = "above"
#' )
#'
#' # Price above 50-day SMA (crossover detection)
#' df <- add_rolling_signal(df,
#'   indicator_col = "close", window = 50,
#'   n_sd = 0, direction = "cross_above"
#' )
#' }
add_rolling_signal <- function(
  df,
  indicator_col,
  window = 20,
  n_sd = 2,
  direction = c("above", "below", "cross_above", "cross_below"),
  center_type = c("mean", "median"),
  signal_name = NULL,
  output_type = c("tibble", "data.frame")
) {
  # Input validation
  if (!all(c("date", "code") %in% colnames(df))) {
    stop("Data must contain 'date' and 'code' columns!")
  }
  if (!indicator_col %in% colnames(df)) {
    stop("Indicator column not found: ", indicator_col)
  }
  if (window < 2) stop("window must be at least 2")
  if (n_sd < 0) stop("n_sd must be non-negative")

  direction <- match.arg(direction)
  center_type <- match.arg(center_type)
  output_type <- match.arg(output_type)

  # Helper for rolling stats
  roll_func <- function(x) {
    if (center_type == "mean") {
      mu <- zoo::rollapply(x, window, mean, fill = NA, align = "right")
      sigma <- zoo::rollapply(x, window, sd, fill = NA, align = "right")
    } else {
      mu <- zoo::rollapply(x, window, median, fill = NA, align = "right")
      sigma <- zoo::rollapply(x, window, sd, fill = NA, align = "right") # still use sd for bands
    }
    list(mu = mu, sigma = sigma)
  }

  result <- df %>%
    dplyr::group_by(.data$code) %>%
    dplyr::arrange(.data$date, .by_group = TRUE) %>%
    dplyr::mutate(
      value = !!sym(indicator_col),
      roll = list(roll_func(.data$value)),
      center = .data$roll[[1]]$mu,
      sd_roll = .data$roll[[1]]$sigma,
      upper_band = .data$center + n_sd * .data$sd_roll,
      lower_band = .data$center - n_sd * .data$sd_roll
    ) %>%
    dplyr::select(-.data$roll) %>%
    dplyr::mutate(
      signal_int = dplyr::case_when(
        direction == "above" ~ as.integer(.data$value > .data$upper_band),
        direction == "below" ~ as.integer(.data$value < .data$lower_band),
        direction == "cross_above" ~ as.integer(
          .data$value > .data$upper_band &
            dplyr::lag(.data$value, 1) <= dplyr::lag(.data$upper_band, 1)
        ),
        direction == "cross_below" ~ as.integer(
          .data$value < .data$lower_band &
            dplyr::lag(.data$value, 1) >= dplyr::lag(.data$lower_band, 1)
        )
      )
    ) %>%
    dplyr::ungroup()

  # Replace NA in signal_int with 0
  result$signal_int[is.na(result$signal_int)] <- 0

  # Auto-generate name
  if (is.null(signal_name)) {
    dir_short <- switch(direction,
      above = "above_upper",
      below = "below_lower",
      cross_above = "cross_above",
      cross_below = "cross_below"
    )
    center_short <- ifelse(center_type == "mean", "SMA", "MED")
    signal_name <- paste0(
      "signal_", indicator_col, "_", center_short, window,
      if (n_sd > 0) paste0("_sd", n_sd), "_", dir_short
    )
  }

  result[[signal_name]] <- result$signal_int
  result <- result %>% dplyr::select(
    -.data$value, -.data$center, -.data$sd_roll,
    -.data$upper_band, -.data$lower_band, -.data$signal_int
  )

  message(
    " Generated rolling signal column: ", signal_name,
    " (window=", window, ", n_sd=", n_sd, ", direction=", direction, ")"
  )

  if (output_type == "tibble") {
    result <- tibble::as_tibble(result)
  }
  return(result)
}
